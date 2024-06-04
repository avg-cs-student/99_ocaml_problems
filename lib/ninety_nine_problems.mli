open Base

(** Problem 1: [last lst] Tail of a list. *)
val last: 'a list -> 'a option

(** Problem 2: [last_two lst] Last two of a list. *)
val last_two: 'a list -> ('a * 'a) option

(** Problem 3: [nth n lst] Nth element of a list. *)
val nth: int -> 'a list -> 'a option

(** Problem 4: [len lst] Length of a list. *)
val len: 'a list -> int

(** Problem 5: [rev lst] Reverse a list. *)
val rev: 'a list -> 'a list

(** Problem 6: [is_palindrome lst] Check if list is a palindrome. *)
val is_palindrome: 'a list -> bool

(** Problem 7: [rl_encode lst] Run-length encode a list. *)
val rl_encode: string list -> (int * string) list
