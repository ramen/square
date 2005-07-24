open Name
open Names

module Value = struct
  type t =
    | None
    | Symbol of Name.t
    | String of string
    | Int of int
    | Float of float
    | Char of char
    | List of t list
    | Record of (Name.t * t NameMap.t)
    | Function of (t -> t)

  let compare = compare

  let record tag items = 
    Record
      (tag,
       List.fold_left
         (fun a (k, v) -> NameMap.add k v a)
         NameMap.empty items)

  exception Error of t

  let fail_value code data = raise (Error (record Names.record [code, data]))
  let fail code msg = fail_value code (String msg)

  let generics = ref None

  let generic func tag =
    (match !generics with
       | Record (_, r) ->
           (match NameMap.find func r with
              | Record (_, r') ->
                  (match NameMap.find tag r' with
                     | Function g -> g
                     | _ -> fail Names.e_type "non-function in generics table")
              | _ -> failwith "non-record in generics table")
       | _ -> failwith "non-record in generics table")

  let def_generic func tag f =
    generics :=
      (match !generics with
         | Record (_, r) ->
             Record
               (Names.record,
                (NameMap.add
                   func
                   (Record
                      (Names.record,
                       (try
                          (match NameMap.find func r with
                             | Record (_, r') ->
                                 (NameMap.add tag (Function f) r')
                             | _ ->
                                 failwith "non-record in generics table")
                        with
                          | Not_found ->
                              (NameMap.add
                                 tag (Function f) NameMap.empty))))
                   r))
         | _ -> failwith "non-record in generics table")

  let undef_generic func tag =
    generics :=
      (match !generics with
         | Record (_, r) ->
             Record
               (Names.record,
                (NameMap.add
                   func
                   (Record
                      (Names.record,
                       (try
                          (match NameMap.find func r with
                             | Record (_, r') ->
                                 (NameMap.remove tag r')
                             | _ ->
                                 failwith "non-record in generics table")
                        with
                          | Not_found ->
                              (NameMap.empty))))
                   r))
         | _ -> failwith "non-record in generics table")

  let rec to_string = function
    | None -> "[]"
    | Symbol n -> "." ^ (Name.to_string n)
    | String s -> s
    | Int i -> string_of_int i
    | Float f -> string_of_float f
    | Char c -> String.make 1 c
    | List l -> "(" ^ (String.concat ", " (List.map to_string l)) ^ ")"
    | Record (tag, _) as r ->
        (try
           (let f = generic Names.to_string tag in
            match f r with
              | String s -> s
              | _ -> fail Names.e_type "to_string must return a string")
         with Not_found -> ("<" ^ (Name.to_string tag) ^ ">"))
    | Function _ -> "<function>"

  let _ =
    generics :=
      let record_to_string =
        (Function
           (function
              | Record (_, r) ->
                  String
                    ("{" ^
                       (String.concat ", "
                          (NameMap.fold
                             (fun k v a ->
                                ((Name.to_string k) ^ ": " ^ (to_string v))
                                :: a)
                             r [])) ^ "}")
              | _ -> fail Names.e_value "argument must be a record")) in
      (record Names.record [Names.to_string,
                            record Names.record [Names.record,
                                                 record_to_string]])

  let true_ = Symbol (Names.true_)
  let false_ = Symbol (Names.false_)

  let of_bool = function
    | true -> true_
    | false -> false_

  let to_bool = function
    | None
    | String ""
    | Int 0
    | Float 0.0
    | List []
        -> false
    | Symbol n when n = Names.false_
        -> false
    | Record (_, r) when NameMap.is_empty r
        -> false
    | _ -> true
end
