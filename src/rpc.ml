
module Version = struct
  type t = int
  let compare a b = compare a b
end

type response = {contents: string}
type call = {name: string; params: string list}
let call name params = {name; params}
