let rec last lst =
  match lst with [] -> None | [ hd ] -> Some hd | _ :: tl -> last tl

let%test_unit "last-empty" =
  [%test_result: Base.string Base.option] (last []) ~expect:None

let%test_unit "last-only-one" =
  [%test_result: Base.string Base.option] (last [ "a" ]) ~expect:(Some "a")

let%test_unit "last-of-three" =
  [%test_result: Base.string Base.option]
    (last [ "a"; "b"; "c" ])
    ~expect:(Some "c")

let rec last_two lst =
  match lst with
  | [] | [ _ ] -> None
  | hd :: [ tl ] -> Some (hd, tl) (* or [hd;tl] *)
  | _ :: tl -> last_two tl

let%test_unit "last_two-empty" =
  [%test_result: (Base.string * Base.string) Base.option] (last_two [])
    ~expect:None

let%test_unit "last_two-only-one" =
  [%test_result: (Base.string * Base.string) Base.option] (last_two [ "a" ])
    ~expect:None

let%test_unit "last_two-only-two" =
  [%test_result: (Base.string * Base.string) Base.option]
    (last_two [ "a"; "b" ])
    ~expect:(Some ("a", "b"))

let%test_unit "last_two-of-three" =
  [%test_result: (Base.string * Base.string) Base.option]
    (last_two [ "a"; "b"; "c" ])
    ~expect:(Some ("b", "c"))

let rec nth n = function
  | [] -> None
  | hd :: tl -> if n = 0 then Some hd else nth (n - 1) tl

let%test_unit "nth-zero-of-none" =
  [%test_result: Base.string Base.option] (nth 0 []) ~expect:None

let%test_unit "nth-zero-of-one" =
  [%test_result: Base.string Base.option] (nth 0 [ "a" ]) ~expect:(Some "a")

let%test_unit "nth-two-of-three" =
  [%test_result: Base.string Base.option]
    (nth 2 [ "a"; "b"; "c" ])
    ~expect:(Some "c")

let%test_unit "nth-three-of-three" =
  [%test_result: Base.string Base.option] (nth 3 [ "a"; "b"; "c" ]) ~expect:None

let rec len lst = match lst with [] -> 0 | _ :: tl -> 1 + len tl
let%test_unit "len-of-none" = [%test_result: Base.int] (len []) ~expect:0
let%test_unit "len-of-one" = [%test_result: Base.int] (len [ "a" ]) ~expect:1

let%test_unit "len-of-three" =
  [%test_result: Base.int] (len [ "a"; "b"; "c" ]) ~expect:3

let rev lst =
  let rec build acc = function [] -> acc | hd :: tl -> build (hd :: acc) tl in
  build [] lst

let%test_unit "rev-none" =
  [%test_result: Base.string Base.list] (rev []) ~expect:[]

let%test_unit "rev-one" =
  [%test_result: Base.string Base.list] (rev [ "a" ]) ~expect:[ "a" ]

let%test_unit "rev-two" =
  [%test_result: Base.string Base.list] (rev [ "a"; "b" ]) ~expect:[ "b"; "a" ]

let%test_unit "rev-three" =
  [%test_result: Base.string Base.list]
    (rev [ "a"; "b"; "c" ])
    ~expect:[ "c"; "b"; "a" ]

let is_palindrome (lst : 'a list) : bool = lst = rev lst

let%test_unit "palindrome-one-yes" =
  [%test_result: Base.bool] (is_palindrome [ "a" ]) ~expect:true

let%test_unit "palindrome-two-no" =
  [%test_result: Base.bool] (is_palindrome [ "a"; "b" ]) ~expect:false

let%test_unit "palindrome-three-yes" =
  [%test_result: Base.bool] (is_palindrome [ "a"; "b"; "a" ]) ~expect:true

let%test_unit "palindrome-four-yes" =
  [%test_result: Base.bool] (is_palindrome [ "a"; "b"; "b"; "a" ]) ~expect:true

let%test_unit "palindrome-four-no" =
  [%test_result: Base.bool] (is_palindrome [ "a"; "b"; "c"; "a" ]) ~expect:false

let rl_encode lst =
  let rec encode count last acc = function
    | [] -> (count, last) :: acc
    | hd :: tl ->
        if hd = last then encode (count + 1) hd acc tl
        else if count != 0 then encode 1 hd ((count, last) :: acc) tl
        else encode 1 hd acc tl
  in
  rev (encode 0 "" [] lst)

let%test_unit "rl_encode-one" =
  [%test_result: (Base.int * Base.string) Base.list] (rl_encode [ "a" ])
    ~expect:[ (1, "a") ]

let%test_unit "rl_encode-two" =
  [%test_result: (Base.int * Base.string) Base.list]
    (rl_encode [ "a"; "b" ])
    ~expect:[ (1, "a"); (1, "b") ]

let%test_unit "rl_encode-two-same" =
  [%test_result: (Base.int * Base.string) Base.list]
    (rl_encode [ "a"; "a" ])
    ~expect:[ (2, "a") ]

let%test_unit "rl_encode-six" =
  [%test_result: (Base.int * Base.string) Base.list]
    (rl_encode [ "a"; "a"; "b"; "c"; "c"; "c" ])
    ~expect:[ (2, "a"); (1, "b"); (3, "c") ]
