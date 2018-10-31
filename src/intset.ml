module Int = struct
  type t = int
  let (=) = Pervasives.(=)
  let of_string = int_of_string
  let to_string = string_of_int
end

module IntSet = Set.Make(Int)
