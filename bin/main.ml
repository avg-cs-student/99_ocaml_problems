(* open Printf *)

let rec map lst fn =
    match lst with
    | [] -> fn None
    | [ h ] -> fn (Some h)
    | h :: t -> let _ = fn (Some h) in map t fn;;

(* let rec print_all lst = *)
(*     match lst with *)
(*     | [] -> () *)
(*     | [ h ] -> printf "%s\n" h *)
(*     | h :: t -> let () = printf "%s\n" h in print_all t;; *)

let opt_print_string = function
    | None -> ()
    | Some a -> print_string (String.concat "" [a; "\n"]);;

(* let _ = print_all (Array.to_list Sys.argv); *)
let _ = map (Array.to_list Sys.argv) opt_print_string;
