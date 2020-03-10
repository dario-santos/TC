let r = Str.regexp "a b"
let v = (Str.string_match r "a" 0)
let _ = Printf.printf "%b\n" v
