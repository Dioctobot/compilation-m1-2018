(** Register some compilers that have X86_64 as a target or source language. *)
let initialize () =
  Languages.register (module X86_64);
  Compilers.register (module RetrolixToX86_64)
