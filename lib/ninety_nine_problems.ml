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
  let rec encode count acc = function
    | [] -> acc
    | [ hd ] -> (count + 1, hd) :: acc
    | first :: (second :: _ as tl) ->
        if first = second then encode (count + 1) acc tl
        else encode 0 ((count + 1, first) :: acc) tl
  in
  encode 0 [] (rev lst)

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

type 'a rle = One of 'a | Many of Base.int * 'a [@@deriving sexp_of]

let modified_rl_encode lst =
  let rec encode count acc = function
    | [] -> acc
    | [ hd ] -> if count = 0 then One hd :: acc else Many (count + 1, hd) :: acc
    | h1 :: (h2 :: _ as tl) ->
        if h1 != h2 && count = 0 then encode 0 (One h1 :: acc) tl
        else if h1 = h2 then encode (count + 1) acc tl
        else encode 0 (Many (count + 1, h1) :: acc) tl
  in
  rev (encode 0 [] lst)

let%test "mrl_encode-two-same" =
  modified_rl_encode [ "a"; "a" ] = [ Many (2, "a") ]

let%test "mrl_encode-two-same-one-diff" =
  modified_rl_encode [ "a"; "a"; "b" ] = [ Many (2, "a"); One "b" ]

let%test "mrl-encode-complex" =
  modified_rl_encode [ 1; 0; 32; 32; 32; 0; 1; 1; 1 ]
  = [ One 1; One 0; Many (3, 32); One 0; Many (3, 1) ]

let duplicate lst =
  let rec clone acc = function
    | [] -> acc
    | hd :: tl -> clone (hd :: hd :: acc) tl
  in
  rev (clone [] lst)

let%test_unit "duplicate-none" =
  [%test_result: Base.string Base.list] (duplicate []) ~expect:[]

let%test_unit "duplicate-one" =
  [%test_result: Base.string Base.list] (duplicate [ "a" ]) ~expect:[ "a"; "a" ]

let%test_unit "duplicate-two" =
  [%test_result: Base.string Base.list]
    (duplicate [ "a"; "b" ])
    ~expect:[ "a"; "a"; "b"; "b" ]

let split lst num =
  let mov acc = function
    | [] -> (acc, [])
    | [ h ] -> (h :: acc, [])
    | h :: t -> (h :: acc, t)
  in
  let rec swap (l1, l2) i =
    if i <= 0 then (rev l1, l2) else swap (mov l1 l2) (i - 1)
  in
  swap ([], lst) num

let%test_unit "split-none" =
  [%test_result: Base.string Base.list * Base.string Base.list]
    (split [ "a"; "b"; "c"; "d"; "e" ] 0)
    ~expect:([], [ "a"; "b"; "c"; "d"; "e" ])

let%test_unit "split-one" =
  [%test_result: Base.string Base.list * Base.string Base.list]
    (split [ "a"; "b" ] 1)
    ~expect:([ "a" ], [ "b" ])

let%test_unit "split-two" =
  [%test_result: Base.string Base.list * Base.string Base.list]
    (split [ "a"; "b"; "c"; "d" ] 2)
    ~expect:([ "a"; "b" ], [ "c"; "d" ])

let%test_unit "split-five" =
  [%test_result: Base.string Base.list * Base.string Base.list]
    (split [ "a"; "b"; "c"; "d"; "e" ] 5)
    ~expect:([ "a"; "b"; "c"; "d"; "e" ], [])

let%test_unit "split-too-many" =
  [%test_result: Base.string Base.list * Base.string Base.list]
    (split [ "a"; "b"; "c"; "d"; "e" ] 10)
    ~expect:([ "a"; "b"; "c"; "d"; "e" ], [])

let remove_at k lst =
  let rec aux acc k = function
    | [] -> acc
    | [ h ] -> if k = 0 then acc else h :: acc
    | h :: t -> if k > 0 then aux (h :: acc) (k - 1) t else t @ acc
  in
  rev (aux [] k lst)

let%test_unit "remove_at-zero" =
  [%test_result: Base.string Base.list]
    (remove_at 0 [ "a"; "b" ])
    ~expect:[ "b" ]

let%test_unit "remove_at-one" =
  [%test_result: Base.string Base.list]
    (remove_at 1 [ "a"; "b"; "c" ])
    ~expect:[ "a"; "c" ]

let%test_unit "remove_at-two" =
  [%test_result: Base.string Base.list]
    (remove_at 2 [ "a"; "b"; "c" ])
    ~expect:[ "a"; "b" ]

let%test_unit "remove_at-three" =
  [%test_result: Base.string Base.list]
    (remove_at 3 [ "a"; "b"; "c" ])
    ~expect:[ "a"; "b"; "c" ]

let insert_at elem pos lst =
  let rec aux acc elem pos = function
    | [] -> if pos = 0 then elem :: acc else []
    | [ h ] ->
        if pos = 0 then acc @ [ elem ] @ [ h ] else acc @ [ h ] @ [ elem ]
    | h :: t as lst ->
        if pos = 0 then elem :: lst else aux (acc @ [ h ]) elem (pos - 1) t
  in
  aux [] elem pos lst

let%test_unit "insert_at-zero" =
  [%test_result: Base.string Base.list]
    (insert_at "wat" 0 [ "a"; "b"; "c" ])
    ~expect:[ "wat"; "a"; "b"; "c" ]

let%test_unit "insert_at-two" =
  [%test_result: Base.string Base.list]
    (insert_at "wat" 2 [ "a"; "b"; "c" ])
    ~expect:[ "a"; "b"; "wat"; "c" ]

let%test_unit "insert_at-empty-nok" =
  [%test_result: Base.string Base.list] (insert_at "wat" 2 []) ~expect:[]

let%test_unit "insert_at-empty-ok" =
  [%test_result: Base.string Base.list] (insert_at "wat" 0 []) ~expect:[ "wat" ]

let range start stop =
  let rec ascend acc start stop =
    if start = stop then acc @ [ start ]
    else ascend (acc @ [ start ]) (start + 1) stop
  in
  if start < stop then ascend [] start stop else rev (ascend [] stop start)

let%test_unit "range-0-0" =
  [%test_result: Base.int Base.list] (range 0 0) ~expect:[ 0 ]

let%test_unit "range-0-3" =
  [%test_result: Base.int Base.list] (range 0 3) ~expect:[ 0; 1; 2; 3 ]

let%test_unit "range-3-0" =
  [%test_result: Base.int Base.list] (range 3 0) ~expect:[ 3; 2; 1; 0 ]

let%test_unit "range-neg3-pos3" =
  [%test_result: Base.int Base.list] (range (-3) 3)
    ~expect:[ -3; -2; -1; 0; 1; 2; 3 ]

let compress lst =
  let same_as_head elem = function
    | [] -> false
    | h :: _ -> if elem = h then true else false
  in
  let rec aux acc = function
    | [] -> rev acc
    | [ h ] -> if same_as_head h acc then aux acc [] else aux (h :: acc) []
    | h :: t -> if same_as_head h acc then aux acc t else aux (h :: acc) t
  in
  aux [] lst

let%test_unit "compress-single" =
  [%test_result: Base.int Base.list] (compress [ 1 ]) ~expect:[ 1 ]

let%test_unit "compress-triple" =
  [%test_result: Base.int Base.list] (compress [ 1; 1; 1 ]) ~expect:[ 1 ]

let%test_unit "compress-multiple" =
  [%test_result: Base.int Base.list]
    (compress [ 0; 0; 1; 1; 1 ])
    ~expect:[ 0; 1 ]

let%test_unit "compress-complex" =
  [%test_result: Base.int Base.list]
    (compress [ 0; 0; 1; 0; 0; 1; 1; 1; 1 ])
    ~expect:[ 0; 1; 0; 1 ]

let replicate lst n =
  let rec replicate_single acc item = function
    | 0 -> acc
    | n -> replicate_single (item :: acc) item (n - 1)
  in
  let rec map acc count = function
    | [] -> acc
    | [ h ] -> map (acc @ replicate_single [] h count) count []
    | h :: t -> map (acc @ replicate_single [] h count) count t
  in
  map [] n lst

let%test_unit "replicate-one" =
  [%test_result: Base.int Base.list] (replicate [ 0 ] 1) ~expect:[ 0 ]

let%test_unit "replicate-two" =
  [%test_result: Base.int Base.list] (replicate [ 0 ] 2) ~expect:[ 0; 0 ]

let%test_unit "replicate-ten" =
  [%test_result: Base.int Base.list] (replicate [ 0 ] 10)
    ~expect:[ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ]

let%test_unit "replicate-none" =
  [%test_result: Base.int Base.list] (replicate [ 0 ] 0) ~expect:[]

let%test_unit "replicate-many" =
  [%test_result: Base.int Base.list]
    (replicate [ 0; 1; 2 ] 3)
    ~expect:[ 0; 0; 0; 1; 1; 1; 2; 2; 2 ]

let drop lst n =
  let rec aux acc count = function
    | [] -> acc
    | [ h ] -> if count == n then acc else h :: acc
    | h :: t -> if count == n then aux acc 1 t else aux (h :: acc) (count + 1) t
  in
  List.rev (aux [] 1 lst)

let%test_unit "drop-every-other" =
  [%test_result: Base.int Base.list] (drop [ 0; 1; 2; 3 ] 2) ~expect:[ 0; 2 ]

let%test_unit "drop-every-third" =
  [%test_result: Base.int Base.list]
    (drop [ 0; 1; 2; 3; 4; 5; 6; 7; 8 ] 3)
    ~expect:[ 0; 1; 3; 4; 6; 7 ]

let%test_unit "drop-every-item" =
  [%test_result: Base.int Base.list] (drop [ 0; 1; 2; 3 ] 1) ~expect:[]

let extract lst first last =
  let rec discard_rear acc count = function
    | [] -> acc
    | [ h ] -> if count <= 0 then acc else h :: acc
    | h :: t ->
        if count <= 0 then acc else discard_rear (h :: acc) (count - 1) t
  in
  let rec discard_front count = function
    | [] -> []
    | [ h ] -> if count <= 0 then [ h ] else []
    | h :: t -> if count <= 0 then h :: t else discard_front (count - 1) t
  in
  discard_front first (List.rev (discard_rear [] (last + 1) lst))

let%test_unit "slice-middle-1" =
  [%test_result: Base.int Base.list]
    (extract [ 0; 1; 2; 3; 4; 5 ] 1 1)
    ~expect:[ 1 ]

let%test_unit "slice-middle-2" =
  [%test_result: Base.int Base.list]
    (extract [ 0; 1; 2; 3; 4; 5 ] 1 2)
    ~expect:[ 1; 2 ]

let%test_unit "slice-middle-3" =
  [%test_result: Base.int Base.list]
    (extract [ 0; 1; 2; 3; 4; 5 ] 1 3)
    ~expect:[ 1; 2; 3 ]

let%test_unit "slice-beginning-1" =
  [%test_result: Base.int Base.list]
    (extract [ 0; 1; 2; 3; 4; 5 ] 0 0)
    ~expect:[ 0 ]

let%test_unit "slice-beginning-2" =
  [%test_result: Base.int Base.list]
    (extract [ 0; 1; 2; 3; 4; 5 ] 0 1)
    ~expect:[ 0; 1 ]

let%test_unit "slice-end-1" =
  [%test_result: Base.int Base.list]
    (extract [ 0; 1; 2; 3; 4; 5 ] 5 5)
    ~expect:[ 5 ]
    
let%test_unit "slice-end-2" =
  [%test_result: Base.int Base.list]
    (extract [ 0; 1; 2; 3; 4; 5 ] 4 5)
    ~expect:[ 4; 5 ]

let%test_unit "slice-all-valid" =
  [%test_result: Base.int Base.list]
    (extract [ 0; 1; 2; 3; 4; 5 ] 0 5)
    ~expect:[ 0; 1; 2; 3; 4; 5 ]

let%test_unit "slice-all-invalid-1" =
  [%test_result: Base.int Base.list]
    (extract [ 0; 1; 2; 3; 4; 5 ] 0 8)
    ~expect:[ 0; 1; 2; 3; 4; 5 ]

let%test_unit "slice-all-invalid-2" =
  [%test_result: Base.int Base.list]
    (extract [ 0; 1; 2; 3; 4; 5 ] (-1) 8)
    ~expect:[ 0; 1; 2; 3; 4; 5 ]
