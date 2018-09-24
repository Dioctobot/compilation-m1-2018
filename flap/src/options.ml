(** Options *)

open ExtStd
open ExtStd.Pervasives

let make_string_option what kind =
  let language = ref "" in
  let get () =
    if !language = "" then
      Error.global_error
        "during analysis of options"
        (Printf.sprintf "You should specify the %s %s using '--%s'."
           kind what kind);
    !language
  in
  let set = ( := ) language in
  let is_set () = !language <> "" in
  get, set, is_set

let (get_source_language, set_source_language, is_source_language_set) =
  make_string_option "language" "source"

let (get_target_language, set_target_language, is_target_language_set) =
  make_string_option "language" "target"

type mode = Interactive | Batch

let mode = ref Batch

let set_mode = ( := ) mode

let get_mode () = !mode

let (get_input_filename, set_input_filename, is_input_filename_set) =
  make_string_option "filename" "input"

let using : string list ref = ref []
let insert_using x = using := x :: !using
let get_using () = !using

let set_interactive_mode = function
  | true -> set_mode Interactive
  | false -> set_mode Batch

let set_running_mode, get_running_mode = Ref.as_functions false
let set_verbose_mode, get_verbose_mode = Ref.as_functions false
let set_dry_mode, get_dry_mode         = Ref.as_functions false
let set_benchmark, get_benchmark       = Ref.as_functions false
let set_unsafe, get_unsafe             = Ref.as_functions false
let set_gcc, get_gcc                   = Ref.as_functions false
let set_show_types, get_show_types     = Ref.as_functions false
let set_infer_types, get_infer_types   = Ref.as_functions false
let set_check_types, get_check_types   = Ref.as_functions true
let set_verbose_eval, get_verbose_eval = Ref.as_functions false
let set_retromips, get_retromips       = Ref.as_functions false
let set_use_sexp_in, get_use_sexp_in   = Ref.as_functions false
let set_use_sexp_out, get_use_sexp_out = Ref.as_functions false
let set_scripts_dir, get_scripts_dir   = Ref.as_functions "/bin"
let set_include_dir, get_include_dir   = Ref.as_functions "/usr/include"

let set_mips_host, get_mips_host =
  Ref.as_functions (("localhost" |< Sys.getenv) "MIPSHOST")

let set_mips_port, get_mips_port =
  Ref.as_functions (("10022" |< Sys.getenv) "MIPSPORT")

let set_output_file, get_output_file = Ref.as_functions ""

let set_fast_match, get_fast_match = Ref.as_functions false
