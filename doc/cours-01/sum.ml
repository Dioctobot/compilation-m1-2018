let sum start stop body =
  let r = ref 0 in
  for i = start to stop do
    r := !r + body i
  done;
  !r

let main =
  let n = int_of_string Sys.argv.(1) in
  let r = sum 1 n (fun x -> sum 1 n (fun y -> x + y)) in
  Printf.printf "%d\n%!" r
