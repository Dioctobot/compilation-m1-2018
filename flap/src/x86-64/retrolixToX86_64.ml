(** This module implements a compiler from Retrolix to X86-64 *)

(** In more details, this module performs the following tasks:
   - turning accesses to local variables and function parameters into stack
     loads and stores ;
   - generating initialization code and reserving space in the .data section for
     global variables ;
   - reserving space in the .data section for literal strings.
 *)

(* TODO tail recursion *)

let error ?(pos = Position.dummy) msg =
  Error.error "compilation" pos msg

(** As in any module that implements {!Compilers.Compiler}, the source
    language and the target language must be specified. *)
module Source = Retrolix
module Target = X86_64
module S = Source.AST
module T = Target.AST

module Str = struct type t = string let compare = Pervasives.compare end
module StrMap = Map.Make(Str)
module StrSet = Set.Make(Str)

(** {2 Low-level helpers} *)

let scratchr = X86_64_Architecture.scratch_register

let scratch = `Reg scratchr
let rsp = `Reg X86_64_Architecture.RSP
let rbp = `Reg X86_64_Architecture.RBP
let rdi = `Reg X86_64_Architecture.RDI


let string_src ~src = X86_64_PrettyPrinter.(match src with
  | dst -> match dst with
    | `Addr addr -> (to_string address addr)
    | `Reg r -> (to_string reg r)
  | `Imm i -> (to_string imm i))

let display ~src = Printf.printf "%s\n" (string_src src)

let display' lsrc = List.iter (fun src -> display src) lsrc

let position_argument offset = 
  let n = Mint.to_int offset in
  Printf.printf "%d\n" (n / 8)


(** [align n b] returns the smallest multiple of [b] larger than [n]. *)
let align n b =
  let m = n mod b in
  if m = 0 then n else n + b - m

(** {2 Label mangling and generation} *)

let hash x = string_of_int (Hashtbl.hash x)

let label_for_string_id id =
  ".S_" ^ string_of_int id

let label_of_retrolix_label (s : string) =
  s

let label_of_function_identifier (S.FId s) =
  label_of_retrolix_label s

let data_label_of_global (S.Id s) =
  label_of_retrolix_label s

let init_label_of_global (xs : S.identifier list) =
  ".I_" ^ hash xs

let label_of_internal_label_id (id : T.label) =
  ".X_" ^ id

let fresh_label : unit -> T.label =
  let r = ref 0 in
  fun () -> incr r; label_of_internal_label_id (string_of_int !r)

let fresh_string_label : unit -> string =
  let r = ref 0 in
  fun () -> let n = !r in incr r; label_for_string_id n

(** {2 Environments} *)

type environment =
  {
    externals : S.FIdSet.t;
    (** All the external functions declared in the retrolix program. *)
    globals : S.IdSet.t;
    (** All the global variables found in the Retrolix program, each with a
        unique integer. *)
    data_lines : T.line list;
    (** All the lines to be added to the .data section of the complete file. *)
  }

let make_environment ~externals ~globals () =
  let open T in

  let data_lines =
    S.IdSet.fold
      (fun ((S.Id id_s) as id) lines ->
        Label (data_label_of_global id)
        :: Instruction (Comment id_s)
        :: Directive (Quad [Lit Mint.zero])
        :: lines
      )
      globals
      []
  in

  let data_lines =
    S.FIdSet.fold
      (fun (S.FId f) lines -> Directive (Extern f) :: lines)
      externals
      data_lines
  in

  {
    externals;
    globals;
    data_lines;
  }

let is_external env (f : S.rvalue) =
  match f with
  | `Immediate (LFun f) ->
     S.FIdSet.mem f env.externals
  | _ ->
     false

let is_global env f =
  S.IdSet.mem f env.globals

let register_string s env =
  let open T in
  let l = fresh_string_label () in
  l,
  { env with data_lines = Label l :: Directive (String s) :: env.data_lines; }

(* The following function is here to please Flap's architecture. *)
let initial_environment () =
  make_environment ~externals:S.FIdSet.empty ~globals:S.IdSet.empty ()

let register_globals global_set env =
  let open T in
  let globals, data_lines =
    S.IdSet.fold
      (fun ((S.Id id_s) as id) (globals, lines) ->
        S.IdSet.add id globals,
        Label (data_label_of_global id)
        :: Instruction (Comment id_s)
        :: Directive (Quad [Lit Mint.zero])
        :: lines
      )
      global_set
      (S.IdSet.empty, env.data_lines)
  in
  { env with globals; data_lines; }

(** {2 Abstract instruction selectors and calling conventions} *)

module type InstructionSelector =
  sig
    (** [mov ~dst ~srcl ~srcr] generates the x86-64 assembly listing to copy
        [src] into [dst]. *)
    val mov : dst:T.dst -> src:T.src -> T.line list

    (** [add ~dst ~srcl ~srcr] generates the x86-64 assembly listing to store
        [srcl + srcr] into [dst]. *)
    val add : dst:T.dst -> srcl:T.src -> srcr:T.src -> T.line list

    (** [sub ~dst ~srcl ~srcr] generates the x86-64 assembly listing to store
        [srcl - srcr] into [dst]. *)
    val sub : dst:T.dst -> srcl:T.src -> srcr:T.src -> T.line list

    (** [mul ~dst ~srcl ~srcr] generates the x86-64 assembly listing to store
        [srcl * srcr] into [dst]. *)
    val mul : dst:T.dst -> srcl:T.src -> srcr:T.src -> T.line list

    (** [div ~dst ~srcl ~srcr] generates the x86-64 assembly listing to store
        [srcl / srcr] into [dst]. *)
    val div : dst:T.dst -> srcl:T.src -> srcr:T.src -> T.line list

    (** [andl ~dst ~srcl ~srcr] generates the x86-64 assembly listing to store
        [srcl & srcr] into [dst]. *)
    val andl : dst:T.dst -> srcl:T.src -> srcr:T.src -> T.line list

    (** [orl ~dst ~srcl ~srcr] generates the x86-64 assembly listing to store
        [srcl | srcr] into [dst]. *)
    val orl : dst:T.dst -> srcl:T.src -> srcr:T.src -> T.line list

    (** [conditional_jump ~cc ~srcl ~srcr ~ll ~lr] generates the x86-64 assembly
        listing to test whether [srcl, srcr] satisfies the relation described by
        [cc] and jump to [ll] if they do or to [lr] when they do not. *)
    val conditional_jump :
      cc:T.condcode ->
      srcl:T.src -> srcr:T.src ->
      ll:T.label -> lr:T.label ->
      T.line list

    (** [switch ~default ~discriminant ~cases] generates the x86-64 assembly
       listing to jump to [cases.(discriminant)], or to the (optional) [default]
       label when discriminant is larger than [Array.length cases].

       The behavior of the program is undefined if [discriminant < 0], or if
       [discriminant >= Array.length cases] and no [default] has been given. *)
    val switch :
      ?default:T.label ->
      discriminant:T.src ->
      cases:T.label array ->
      T.line list
  end

module type FrameManager =
  sig
    (** The abstract data structure holding the information necessary to
        implement the calling convention. *)
    type frame_descriptor

    (** Generate a frame descriptor for the function with parameter [params] and
        locals [locals]. *)
    val frame_descriptor :
      params:S.identifier list ->
      locals:S.identifier list ->
      frame_descriptor

    (** [location_of fd v] computes the address of [v] according to the frame
        descriptor [fd]. Note that [v] might be a local variable, a function
        parameter, or a global variable. *)
    val location_of : frame_descriptor -> S.identifier -> T.address

    (** [function_prologue fd] generates the x86-64 assembly listing to setup
        a stack frame according to the frame descriptor [fd]. *)
    val function_prologue : frame_descriptor -> T.line list

    (** [function_epilogue fd] generates the x86-64 assembly listing to setup a
        stack frame according to the frame descriptor [fd]. *)
    val function_epilogue : frame_descriptor -> T.line list

    (** [call fd ~kind ~f ~args] generates the x86-64 assembly listing to setup
        a call to the function at [f], with arguments [args], with [kind]
        specifying whether this should be a normal or tail call.  *)
    val call :
      frame_descriptor ->
      kind:[ `Normal | `Tail ] ->
      f:T.src ->
      args:T.src list ->
      T.line list
  end

(** {2 Code generator} *)

(** This module implements an x86-64 code generator for Retrolix using the
    provided [InstructionSelector] and [FrameManager].  *)
module Codegen(IS : InstructionSelector)(FM : FrameManager) =
  struct
    let translate_label (S.Label l) =
      label_of_retrolix_label l

    let translate_variable fd v =
      `Addr (FM.location_of fd v)

    let translate_literal lit env =
      match lit with
      | S.LInt i ->
         T.Lit i, env

      | S.LFun f ->
         T.Lab (label_of_function_identifier f), env

      | S.LChar c ->
         T.Lit (Mint.of_int @@ Char.code c), env

      | S.LString s ->
         let l, env = register_string s env in
         T.Lab l, env

    let translate_register (S.RId s) =
      X86_64_Architecture.register_of_string s

    let translate_lvalue fi lv =
      match lv with
      | `Variable v ->
         translate_variable fi v
      | `Register reg ->
         `Reg (translate_register reg)

    let translate_rvalue fi rv env =
      match rv with
      | `Immediate lit ->
         let lit, env = translate_literal lit env in
         `Imm lit, env
      | (`Variable _ | `Register _) as lv ->
         translate_lvalue fi lv, env

    let translate_rvalues fi rvs env =
      List.fold_right
        (fun rv (rvs, env) ->
          let rv, env = translate_rvalue fi rv env in
          rv :: rvs, env)
        rvs
        ([], env)

    let translate_label_to_operand (S.Label l) =
      `Imm (T.Lab l)

    let translate_cond cond =
      match cond with
      | S.GT -> T.G
      | S.LT -> T.L
      | S.GTE -> T.GE
      | S.LTE -> T.LE
      | S.EQ -> T.E

    let translate_instruction fd ins env : T.line list * environment  =
      let open T in
      let open X86_64_Architecture in
      begin match ins with
      | S.Call (f, args, is_tail) ->
         let kind = if is_tail then `Tail else `Normal in
         let f, env = translate_rvalue fd f env in
         let args, env = translate_rvalues fd args env in
         FM.call fd ~kind ~f ~args,
         env

      | S.Assign (dst, op, args) ->
         let dst = translate_lvalue fd dst in
         let args, env = translate_rvalues fd args env in
         let inss =
           match op, args with
           | S.Add, [ srcl; srcr; ] ->
              IS.add ~dst ~srcl ~srcr
           | S.Sub, [ srcl; srcr; ] ->
              IS.sub ~dst ~srcl ~srcr
           | S.Mul, [ srcl; srcr; ] ->
              IS.mul ~dst ~srcl ~srcr
           | S.Div, [ srcl; srcr; ] ->
              IS.div ~dst ~srcl ~srcr
           | S.And, [ srcl; srcr; ] ->
              IS.andl ~dst ~srcl ~srcr
           | S.Or, [ srcl; srcr; ] ->
              IS.orl ~dst ~srcl ~srcr
           | S.Copy, [ src; ] ->
              IS.mov ~dst ~src
           | _ ->
              error "Unknown operator or bad arity"
         in
         inss, env

      | S.Ret ->
         FM.function_epilogue fd @ insns [T.Ret],
         env

      | S.Jump l ->
         insns
           [
             T.jmpl (translate_label l);
           ],
         env

      | S.ConditionalJump (cond, args, ll, lr) ->
         let cc = translate_cond cond in
         let srcl, srcr, env =
           match args with
           | [ src1; src2; ] ->
              let src1, env = translate_rvalue fd src1 env in
              let src2, env = translate_rvalue fd src2 env in
              src1, src2, env
           | _ ->
              failwith "translate_exp: conditional jump with invalid arity"
         in
         IS.conditional_jump
           ~cc
           ~srcl ~srcr
           ~ll:(translate_label ll) ~lr:(translate_label lr),
         env

      | Switch (discriminant, cases, default) ->
         let discriminant, env = translate_rvalue fd discriminant env in
         let cases = Array.map translate_label cases in
         let default = ExtStd.Option.map translate_label default in
         IS.switch ?default ~discriminant ~cases,
         env

      | S.Comment s ->
         insns
           [
             Comment s;
           ],
         env

      | Exit ->
         IS.mov ~src:(liti 0) ~dst:rdi
         @ FM.call fd ~kind:`Normal ~f:(`Imm (Lab "exit")) ~args:[],
         env
      end


    let translate_labelled_instruction fi (body, env) (l, ins) =
      let ins, env = translate_instruction fi ins env in
      List.rev ins @ T.Label (translate_label l) :: body,
      env

    let translate_labelled_instructions fi env inss =
      let inss, env =
        List.fold_left (translate_labelled_instruction fi) ([], env) inss
      in
      List.rev inss, env

    let translate_fun_def ~name ?(desc = "") ~params ~locals gen_body =
      let open T in

      let fd = FM.frame_descriptor ~params ~locals in

      let prologue = FM.function_prologue fd in

      let body, env = gen_body fd in

      Directive (PadToAlign { pow = 3; fill = 0x90; }) :: Label name
      :: (if desc = ""
          then prologue
          else Instruction (Comment desc) :: prologue)
      @ body,
      env

    let translate_block ~name ?(desc = "") ~params (locals, body) env =
      translate_fun_def
        ~name
        ~desc
        ~params
        ~locals
        (fun fi -> translate_labelled_instructions fi env body)

    let translate_definition def (body, env) =
      match def with
      | S.DValues (xs, block) ->
         let ids = RetrolixPrettyPrinter.(to_string identifiers xs) in
         let name = init_label_of_global xs in
         let def, env =
           translate_block
             ~name
             ~desc:("Initializer for " ^ ids ^ ".")
             ~params:[]
             block
             env
         in
         def @ body, env

      | S.DFunction ((S.FId id) as f, params, block) ->
         let def, env =
           translate_block
             ~desc:("Retrolix function " ^ id ^ ".")
             ~name:(label_of_function_identifier f)
             ~params
             block
             env
         in
         def @ body, env

      | S.DExternalFunction (FId id) ->
         T.(Directive (Extern id)) :: body,
         env

    let generate_main env p =
      let open X86_64_Architecture in
      let open T in

      let body =
        List.rev
          [
            Directive (PadToAlign { pow = 3; fill = 0x90; });
            Label "main";
            Instruction (Comment "Program entry point.");
            Instruction (T.subq ~src:(`Imm (Lit 8L)) ~dst:rsp);
          ]
      in

      (* Call all initialization stubs *)
      let body =
        let call body def =
          match def with
          | S.DValues (ids, _) ->
             let l = init_label_of_global ids in
             Instruction (T.calld (Lab l)) :: body
          | S.DFunction _ | S.DExternalFunction _ ->
             body
        in
        List.fold_left call body p
      in

      let body =
        T.insns
          [
            T.calld (Lab "exit");
            T.movq ~src:(liti 0) ~dst:rdi;
          ]
        @ body
      in

      Directive (Global "main") :: List.rev body

    (** [translate p env] turns a Retrolix program into a X86-64 program. *)
    let rec translate (p : S.t) (env : environment) : T.t * environment =
      let env = register_globals (S.globals p) env in
      let pt, env = List.fold_right translate_definition p ([], env) in
      let main = generate_main env p in
      let p = T.data_section :: env.data_lines @ T.text_section :: main @ pt in
      T.remove_unused_labels p, env
  end

(** {2 Concrete instructions selectors and calling conventions} *)

module InstructionSelector : InstructionSelector =
  struct
    open T

    let mov ~(dst : dst) ~(src : src) =
      [T.(Instruction (movq src dst))]

    let bin ins ~dst ~srcl ~srcr =
      []

    let add ~dst ~srcl ~srcr =
      [
        T.(Instruction (movq srcl scratch));
        T.(Instruction (addq srcr scratch));
        T.(Instruction (movq scratch dst));
      ]
    let sub ~dst ~srcl ~srcr =
      [
        T.(Instruction (movq srcl scratch));
        T.(Instruction (subq srcr scratch));
        T.(Instruction (movq scratch dst));
      ]

    let mul ~dst ~srcl ~srcr =
      [
        T.(Instruction (movq srcl scratch));
        T.(Instruction (imulq srcr scratch));
        T.(Instruction (movq scratch dst));
      ]

    let div ~dst ~srcl ~srcr =
      []

    let andl ~dst ~srcl ~srcr =
      []

    let orl ~dst ~srcl ~srcr =
      []

    let conditional_jump ~cc ~srcl ~srcr ~ll ~lr =
      [
        T.(Instruction (cmpq srcl srcr));
        T.(Instruction (jccl cc ll));
        T.(Instruction (jmpl lr));
      ]

    let switch ?default ~discriminant ~cases =
      []

  end

module FrameManager(IS : InstructionSelector) : FrameManager =
  struct
    type frame_descriptor =
      {
        param_count : int;
        (** Number of parameters. *)
        locals_space : int;
        (** Amount of space dedicated to local variables in the stack frame. *)
        stack_map : Mint.t S.IdMap.t;
        (** Maps stack-allocated variable names to stack slots expressed as
            frame-pointer relative offsets. *)
      }

    (** [empty_frame fd] returns [true] if and only if the stack frame described
        by [fd] is empty. *)
    let empty_frame fd =
      fd.param_count = 0 && fd.locals_space = 0

    (** [stack_usage_after_prologue fd] returns the size, in bytes, of the stack
        space after the function prologue. *)
    let stack_usage_after_prologue fd =
      Mint.size_in_bytes
      + (if empty_frame fd then 0 else 1) * Mint.size_in_bytes
      + fd.locals_space

    let frame_descriptor ~params ~locals =
      (* Student! Implement me! *)
      let calc_offset_param index = Mint.of_int  ((-index + -1) * Mint.size_in_bytes) in
      let calc_offset_local index = Mint.of_int  ((index + 1) * Mint.size_in_bytes) in

      let stack_params = 
        let par = List.mapi (fun index param ->
          (param, calc_offset_param index)
        ) params
        in
        List.fold_left (fun map -> function
            | id, offset -> S.IdMap.add id offset map
          ) (S.IdMap.empty) par
      in
      let base = S.IdMap.add (S.Id "rbp") (Mint.of_int 0) S.IdMap.empty in
      let stack_locals = 
        let loc = List.mapi (fun index local ->
          (local, calc_offset_local index)
        ) locals
        in
        List.fold_left (fun map -> function
            | id, offset -> S.IdMap.add id offset map
          ) (S.IdMap.empty) loc
      in

      let stack =
        (S.IdMap.bindings stack_params) @ 
        (S.IdMap.bindings base) @ 
        (S.IdMap.bindings stack_locals)
      in

      { 
        param_count = List.length params;
        locals_space = List.length locals * Mint.size_in_bytes;
        stack_map = List.fold_left (fun map -> function
            | id, offset -> S.IdMap.add id offset map
          ) (S.IdMap.empty) stack;
      }

    let location_of fd id =
      let open X86_64_Architecture in
      let base = register_of_string "rip" in
      let idx = register_of_string "rsp" in
      (*

      Printf.printf "id = %s\n" (data_label_of_global id);
      S.IdMap.iter (fun local_id offset ->
        Printf.printf "%s %d\n" (data_label_of_global local_id) (Mint.to_int offset)) fd.stack_map;
      try
        T.(addr ~offset:(Lit (S.IdMap.find id fd.stack_map)) 
        ~base:base
        ~idx:idx ())
      with _ -> 
        (T.addr ~offset:(Lab(data_label_of_global id)) ~base:base ()*)
      try
        T.addr ~offset:(Lit (S.IdMap.find id fd.stack_map)) ~base:base ~idx:idx ()
      with _ -> 
        T.addr ~offset:(Lab (data_label_of_global id)) ~base:base ()

      

    let function_prologue fd =
      (* Student! Implement me!
      [
        T.(Instruction (pushq rbp));
      ] 
      @ (IS.mov rbp rsp) @
      [
        T.(Instruction (subq rdi rsp));
      ]*)
      let check_frame = 
        if empty_frame fd then 
          []
        else
          if fd.locals_space = 0 then
            []
          else
            [T.(Instruction (subq (liti fd.locals_space) rsp))]
      in
      [
        T.(Instruction (pushq rbp));
        T.(Instruction (movq rsp rbp));
      ] @ check_frame

    let function_epilogue fd =
      (* Student! Implement me! *)
      let check_frame = 
        if empty_frame fd then 
          []
        else
          if fd.locals_space = 0 then
            []
          else
            [T.(Instruction (addq (liti fd.locals_space) rsp))]
      in
      Printf.printf "%d\n" fd.locals_space;
      check_frame @
      [
        T.(Instruction (popq rbp));
      ]

    let call fd ~kind ~f ~args =
      let open X86_64_Architecture in
      (*
      let check_args = List.mapi (fun index arg -> 
        if index < 5 then
          T.(Instruction (movq arg (`Reg (List.nth (List.tl argument_passing_registers) index))))
        else
          T.(Instruction (pushq arg)) 
      ) (List.tl args)
      in
      (List.rev check_args) @*)
   
      let n = List.length args in
      let check_args = 
        if n = 0 then 
          [], []
        else
          List.rev (List.mapi (fun index arg ->
            (*if index < 6 then
              T.(Instruction (movq arg (`Reg (List.nth (argument_passing_registers) index))))
            else*)
            T.(Instruction (pushq arg))
          ) args), 
          [T.(Instruction (addq (liti fd.locals_space) rsp))]
      in
      (fst check_args)
      @
      [
        T.(Instruction (calldi f));
      ]
      @ 
      (snd check_args)
  end

module CG =
  Codegen(InstructionSelector)(FrameManager(InstructionSelector))

let translate = CG.translate
