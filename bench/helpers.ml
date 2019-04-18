let key_from_int n =
  let rec inner n = match n with
    | 0 -> []
    | n -> n mod 26
           |> (+) (int_of_char 'a' - 1)
           |> char_of_int
           |> String.make 1
           |> fun s -> s::(inner (n / 26))

  in String.concat "" (inner n)

let sequence_list lower upper =
  let rec helper n acc = match n with
    | n when n = lower -> lower::acc
    | n                -> helper (n-1) (n::acc)
  in helper upper []

(* Convert two lists to a list of pairs *)
let zip list1 list2 =
  let rec inner l1 l2 acc = match l1, l2 with
    | [], [] -> acc
    | x::xs, y::ys -> inner xs ys ((x, y)::acc)
    | _ -> invalid_arg "Lists do not have the same length"
  in inner list1 list2 []
