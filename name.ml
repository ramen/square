module Name : sig
  type t
  val compare : t -> t -> int
  val of_string : string -> t
  val to_string : t -> string
end = struct
  type t = int
  let compare = compare

  let count = ref (-1)
  let string_to_int = Hashtbl.create 64
  let int_to_string = Hashtbl.create 64

  let of_string s =
    try Hashtbl.find string_to_int s
    with Not_found ->
      (incr count;
       Hashtbl.replace string_to_int s !count;
       Hashtbl.replace int_to_string !count s;
       !count)

  let to_string n = Hashtbl.find int_to_string n
end

module NameMap = struct
  include Map.Make(Name)
  let pairs m = fold (fun k v a -> (k, v) :: a) m []
end
