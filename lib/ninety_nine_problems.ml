(** [last lst] Tail of a list. *)
let rec last lst = match lst with
| [] -> None
| [hd] -> Some hd
| _::tl -> last tl

let%test _ = last [] = None
let%test _ = last ["a"] = Some "a"
let%test _ = last ["a";"b";"c"] = Some "c"

(** [last_two lst] Last two of a list. *)
let rec last_two lst = match lst with
| [] | [_] -> None
| hd::[tl] -> Some(hd,tl)  (* or [hd;tl] *)
| _::tl -> last_two tl

let%test _ = last_two [] = None
let%test _ = last_two ["a"] = None
let%test _ = last_two ["a";"b"] = Some ("a","b")
let%test _ = last_two ["a";"b";"c"] = Some ("b","c")

(** [nth n lst] Nth element of a list. *)
let rec nth n = function
| [] -> None
| hd::tl -> if n = 0 then Some hd else nth (n-1) tl

let%test _ = nth 0 [] = None
let%test _ = nth 0 ["a"] = Some "a"
let%test _ = nth 2 ["a";"b";"c"] = Some "c"
let%test _ = nth 3 ["a";"b";"c"] = None

(** [len lst] Length of a list. *)
let rec len lst = match lst with
| [] -> 0
| _::tl -> 1 + len tl

let%test _ = len [] = 0
let%test _ = len ["a"] = 1
let%test _ = len ["a";"b";"c"] = 3


(** [rev lst] Reverse a list. *)
let rev lst =
    let rec build acc = function
        | [] -> acc
        | hd::tl -> build (hd::acc) tl
    in
    build [] lst

let%test _ = rev [] = []
let%test _ = rev ["a"] = ["a"]
let%test _ = rev ["a";"b"] = ["b";"a"]
let%test _ = rev ["a";"b";"c"] = ["c";"b";"a"]



