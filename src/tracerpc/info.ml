module type S = sig
  val make: ?author:string ->
    ('a, Format.formatter, unit, Irmin.Info.f) format4 -> 'a
end
