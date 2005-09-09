open Name

module Names = struct
  let true_ = Name.of_string "true"
  let false_ = Name.of_string "false"
  let none = Name.of_string "none"
  let symbol = Name.of_string "symbol"
  let string = Name.of_string "string"
  let int = Name.of_string "int"
  let float = Name.of_string "float"
  let char = Name.of_string "char"
  let list = Name.of_string "list"
  let record = Name.of_string "record"
  let function_ = Name.of_string "function"
  let module_ = Name.of_string "module"
  let to_string = Name.of_string "to_string"

  let lt = Name.of_string "<"
  let eq = Name.of_string "="
  let gt = Name.of_string ">"

  let e_type = Name.of_string "TypeError"
  let e_value = Name.of_string "ValueError"
  let e_name = Name.of_string "NameError"
  let e_field_nf = Name.of_string "FieldNotFound"
  let e_index_nf = Name.of_string "IndexNotFound"
  let e_value_nf = Name.of_string "ValueNotFound"
  let e_eval = Name.of_string "EvalError"
end
