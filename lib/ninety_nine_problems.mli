val last : 'a list -> 'a option
(** Problem 1: [last lst] Tail of a list. *)

val last_two : 'a list -> ('a * 'a) option
(** Problem 2: [last_two lst] Last two of a list. *)

val nth : int -> 'a list -> 'a option
(** Problem 3: [nth n lst] Nth element of a list. *)

val len : 'a list -> int
(** Problem 4: [len lst] Length of a list. *)

val rev : 'a list -> 'a list
(** Problem 5: [rev lst] Reverse a list. *)

val is_palindrome : 'a list -> bool
(** Problem 6: [is_palindrome lst] Check if list is a palindrome. *)

val rl_encode : string list -> (int * string) list
(** Problem 7: [rl_encode lst] Run-length encode a list. *)

type 'a rle = One of 'a | Many of int * 'a

val modified_rl_encode : 'a list -> 'a rle list
(** Problem 8: [mrl_encode lst] Modified run-length encode a list. *)

val duplicate : 'a list -> 'a list
(** Problem 9: [duplicate: lst] Duplicate the items in a list. *)

val remove_at : int -> 'a list -> 'a list
(** Problem 10: [remove_at: k lst] Remove an element from a list at position k.
*)

val insert_at : 'a -> int -> 'a list -> 'a list
(** Problem 11: [insert_at elem pos lst] Insert into a list at position k. *)

val range : int -> int -> int list
(** Problem 12: [range: start stop] Iterate over a range in either direction. *)

val replicate : 'a list -> int -> 'a list
(** Problem 13: [replicate: list count] Replicate the elements of a list a given
    number of times. *)

val drop : 'a list -> int -> 'a list
(** Problem 14: [drop: list n] Drop every n'th element from a list. *)

val extract : 'a list -> int -> int -> 'a list
(** Problem 15: [extract: list start stop] Extract a slice from a list. *)
