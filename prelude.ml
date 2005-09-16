(*pp camlp4o -I . pa_sqfun.cmo *)

open Name
open Names
open Value
open Eval

let arith_op_apply int_op float_op a b =
  match (a, b) with
    | (Value.Int a, Value.Int b)
      -> Value.Int (int_op a b)
    | (Value.Int a, Value.Float b)
      -> Value.Float (float_op (float a) b)
    | (Value.Float a, Value.Int b)
      -> Value.Float (float_op a (float b))
    | (Value.Float a, Value.Float b)
      -> Value.Float (float_op a b)
    | _ -> Value.fail Names.e_type "arguments must be numbers"

let arith_op int_op float_op =
  (sqfun
     | Value.Int _
     | Value.Float _ as a
       -> (sqfun
             | Value.Int _
             | Value.Float _ as b -> arith_op_apply int_op float_op a b
             | _ -> Value.fail Names.e_type "argument must be a number")
     | _ -> Value.fail Names.e_type "argument must be a number")

let arith_unary float_op =
  (sqfun
     | Value.Int i -> Value.Float (float_op (float i))
     | Value.Float f -> Value.Float (float_op f)
     | _ -> Value.fail Names.e_type "arguments must be numbers")

let arith_binary float_op =
  (sqfun x ->
     (sqfun y ->
        let (x, y) =
          match (x, y) with
            | (Value.Int x, Value.Int y) -> (float x, float y)
            | (Value.Float x, Value.Int y) -> (x, float y)
            | (Value.Int x, Value.Float y) -> (float x, y)
            | (Value.Float x, Value.Float y) -> (x, y)
            | _ -> Value.fail Names.e_type "arguments must be numbers" in
        Value.Float (float_op x y)))

(* thanks to extlib *)
let string_find str sub =
  let sublen = String.length sub in
    if sublen = 0 then
      0
    else
      let found = ref 0 in
      let len = String.length str in
        try
          for i = 0 to len - sublen do
            let j = ref 0 in
              while String.get str (i + !j) = String.get sub !j do
                incr j;
                if !j = sublen then begin found := i; raise Exit; end;
              done;
          done;
          raise Not_found
        with
            Exit -> !found

(* thanks to extlib *)
let buf_len = 8192
let input_all ic =
  let rec loop acc total buf ofs =
    let n = input ic buf ofs (buf_len - ofs) in
      if n = 0 then
        let res = String.create total in
        let pos = total - ofs in
        let _ = String.blit buf 0 res pos ofs in
        let coll pos buf =
          let new_pos = pos - buf_len in
            String.blit buf 0 res new_pos buf_len;
            new_pos in
        let _ = List.fold_left coll pos acc in
          res
      else
        let new_ofs = ofs + n in
        let new_total = total + n in
          if new_ofs = buf_len then
            loop (buf :: acc) new_total (String.create buf_len) 0
          else loop acc new_total buf new_ofs in
    loop [] 0 (String.create buf_len) 0

let rec make_array self =
  Value.record (Name.of_string "array") [
    Name.of_string "copy",
      (sqfun
         | Value.None -> make_array (Array.copy self)
         | _ -> Value.fail Names.e_type "function takes no arguments");
    Name.of_string "get",
      (sqfun
         | Value.Int i ->
             (try Array.get self i
              with Invalid_argument "index out of bounds" ->
                Value.fail_value
                  Names.e_index_nf (Value.Int i))
         | _ ->
             Value.fail Names.e_type "argument must be an integer");
    Name.of_string "put",
      (sqfun
         | Value.Int i ->
             (sqfun x ->
                (try
                   Array.set self i x;
                   Value.None
                 with
                   | Invalid_argument
                       "index out of bounds" ->
                       Value.fail_value
                         Names.e_index_nf
                         (Value.Int i)))
         | _ ->
             Value.fail
               Names.e_type
               "argument must be an integer");
    Name.of_string "size",
      (sqfun
         | Value.None -> Value.Int (Array.length self)
         | _ ->
             Value.fail
               Names.e_type
               "function takes no arguments");
    Name.of_string "sort_with",
      (sqfun
         | Value.Function f ->
             Array.sort
               (fun a b ->
                  let c = f (Value.List [a; b]) in
                  match c with
                    | Value.Symbol n when n = Names.lt -> -1
                    | Value.Symbol n when n = Names.eq -> 0
                    | Value.Symbol n when n = Names.gt -> 1
                    | _ ->
                        Value.fail
                          Names.e_type
                          "comparator must return .<, .=, or .>")
               self;
             Value.None
         | _ ->
             Value.fail
               Names.e_type
               "argument must be a function");
  ]

let global_env = ref NameMap.empty

let add_binding env name value =
  env := NameMap.add (Name.of_string name) (ref value) !env

let def = add_binding global_env in

def "throw" (sqfun x -> raise (Value.Error x));

def "hash"
  (sqfun
     | Value.Record (tag, r) as r' ->
         if tag = Names.record
         then
           Value.Int
             (Hashtbl.hash (NameMap.fold (fun k v a -> (k, v) :: a) r []))
         else
           (try
              Value.Int
                (Hashtbl.hash (Value.generic (Name.of_string "hash") tag r'))
            with
              | Not_found ->
                  Value.fail
                    Names.e_type
                    ("hash is not defined for " ^ (Name.to_string tag)))
     | _ as x ->
         Value.Int (Hashtbl.hash x));

def "is"
  (sqfun x ->
     (sqfun y ->
        Value.of_bool
          (match (x, y) with
             | (Value.Symbol x', Value.Symbol y') -> (x' == y')
             | _ -> (x == y))));

def "not" (sqfun x -> Value.of_bool (not (Value.to_bool x)));

def "typeof"
  (sqfun
     | Value.None -> Value.Symbol Names.none
     | Value.Symbol _ -> Value.Symbol Names.symbol
     | Value.String _ -> Value.Symbol Names.string
     | Value.Int _ -> Value.Symbol Names.int
     | Value.Float _ -> Value.Symbol Names.float
     | Value.Char _ -> Value.Symbol Names.char
     | Value.List _ -> Value.Symbol Names.list
     | Value.Record (tag, _) -> Value.Symbol tag
     | Value.Function _ -> Value.Symbol Names.function_);

def "bool" (sqfun x -> Value.of_bool (Value.to_bool x));

def "string" (sqfun x -> Value.String (Value.to_string x));

def "symbol"
  (sqfun
     | Value.Symbol _ as s -> s
     | Value.String s -> Value.Symbol (Name.of_string s)
     | _ ->
         Value.fail Names.e_type "argument must be a string or symbol");

def "symbol_name"
  (sqfun
     | Value.Symbol n -> Value.String (Name.to_string n)
     | _ -> Value.fail Names.e_type "argument must be a symbol");

def "char"
  (sqfun
     | Value.Char _ as c -> c
     | Value.String s ->
         (if String.length s = 1
          then Value.Char (s.[0])
          else Value.fail Names.e_value "string must have exactly 1 char")
     | Value.Int i ->
         (try Value.Char (char_of_int i)
          with Invalid_argument "char_of_int" ->
            Value.fail Names.e_value "invalid literal for 'char'")
     | _ -> Value.fail Names.e_type "invalid type for 'char'");

def "int"
  (sqfun
     | Value.Int _ as i -> i
     | Value.Float f -> Value.Int (int_of_float f)
     | Value.Char c -> Value.Int (int_of_char c)
     | Value.String s ->
         (try Value.Int (int_of_string s)
          with Failure "int_of_string" ->
            Value.fail Names.e_value "invalid literal for 'int'")
     | _ -> Value.fail Names.e_type "invalid type for 'int'");

def "float"
  (sqfun
     | Value.Float _ as f -> f
     | Value.Int i -> Value.Float (float i)
     | Value.String s ->
         (try Value.Float (float_of_string s)
          with Failure "float_of_string" ->
            Value.fail Names.e_value "invalid literal for 'float'")
     | _ -> Value.fail Names.e_type "invalid type for 'float'");

def "print"
  (sqfun
     | Value.List list ->
         (List.iter (fun x -> print_string (Value.to_string x)) list;
          Value.None)
     | x ->
         (print_string (Value.to_string x));
         Value.None);

def "cons"
  (sqfun x ->
     (sqfun
        | Value.List list -> Value.List (x :: list)
        | _ ->
            Value.fail
              Names.e_type
              "second argument must be a list"));

def "head"
  (sqfun
     | Value.List (h :: t) -> h
     | Value.List [] -> Value.fail Names.e_type "list must not be empty"
     | _ -> Value.fail Names.e_type "argument must be a list");

def "tail"
  (sqfun
     | Value.List (h :: t) -> Value.List t
     | Value.List [] -> Value.fail Names.e_type "list must not be empty"
     | _ -> Value.fail Names.e_type "argument must be a list");

def "size"
  (sqfun
     | Value.String s -> Value.Int (String.length s)
     | Value.List l -> Value.Int (List.length l)
     | Value.Record (tag, r) as r' ->
         (try Value.generic (Name.of_string "size") tag r'
          with Not_found -> Value.Int (NameMap.fold
                                         (fun _ _ a -> a + 1) r 0))
     | _ ->
         Value.fail
           Names.e_type
           "argument must be a string, list, or record");

def "fold"
  (sqfun
     | Value.String s ->
         (sqfun
            | Value.Function f ->
                (sqfun a ->
                   let result = ref a in
                   let count = ref 0 in
                   String.iter
                     (fun c ->
                        result := f (Value.List [Value.Int !count;
                                                 Value.Char c;
                                                 !result]);
                        incr count) s;
                   !result)
            | _ ->
                Value.fail
                  Names.e_type
                  "second argument must be a function")
     | Value.List l ->
         (sqfun
            | Value.Function f ->
                (sqfun a ->
                   (let rec loop i xs a =
                      match xs with
                        | h :: t ->
                            (loop
                               (i + 1) t
                               (f (Value.List
                                     [Value.Int i; h; a])))
                        | _ -> a
                    in loop 0 l a))
            | _ ->
                Value.fail
                  Names.e_type
                  "second argument must be a function")
     | Value.Record (tag, r) ->
         (sqfun
            | Value.Function f ->
                (sqfun a ->
                   (try
                      let g = Value.generic (Name.of_string "fold") tag in
                      g (Value.List [Value.Record (tag, r);
                                     Value.Function f;
                                     a])
                    with
                      | Not_found ->
                          NameMap.fold
                            (fun n v a' ->
                               f (Value.List [Value.Symbol n; v; a'])) r a))
            | _ ->
                Value.fail
                  Names.e_type
                  "second argument must be a function")
     | _ ->
         Value.fail
           Names.e_type
           "first argument must be a string, list, or record");

def "in"
  (sqfun
    | Value.String s ->
        (sqfun
           | Value.Char c ->
               (Value.of_bool (String.contains s c))
           | Value.String s' ->
               (try ignore (string_find s s'); Value.true_
                with Not_found -> Value.false_)
           | _ ->
               Value.fail
                 Names.e_type
                 "second argument must be a char or string")
    | Value.List l ->
        (sqfun x ->
           (Value.of_bool
              (List.mem x l)))
    | Value.Record (_, r) ->
        (sqfun
           | Value.Symbol n ->
               if NameMap.mem n r then Value.true_ else Value.false_
           | _ ->
               Value.fail
                 Names.e_type
                 "second argument must be a symbol")
    | _ ->
        Value.fail
          Names.e_type
          "first argument must be a string, list, or record");

def "index"
  (sqfun
     | Value.String s ->
         (sqfun
            | Value.Char c ->
                (try Value.Int (String.index s c)
                 with Not_found ->
                   Value.fail_value Names.e_value_nf (Value.Char c))
            | Value.String s' ->
                (try Value.Int (string_find s s')
                 with Not_found ->
                   Value.fail_value Names.e_value_nf (Value.String s'))
            | _ ->
                Value.fail
                  Names.e_type
                  "second argument must be a char or string")
     | Value.List l ->
         (sqfun x ->
            let i = ref (-1) in
            (try
               let _ =
                 List.fold_left
                   (fun a v ->
                      if x = v
                      then (i := a; raise Exit)
                      else (a + 1))
                   0 l
               in Value.fail_value Names.e_value_nf x
             with
               | Exit -> Value.Int !i))
     | Value.Record (_, r) ->
         (sqfun x ->
            let k = ref (Name.of_string "false") in
            (try
               let _ =
                 NameMap.iter
                   (fun k' v ->
                      if x = v
                      then (k := k'; raise Exit)) r
               in Value.fail_value Names.e_value_nf x
             with
               | Exit -> Value.Symbol (!k)))
     | _ ->
         Value.fail
           Names.e_type
           "first argument must be a string, list, or record");

def "sort_with"
  (sqfun
     | Value.Function f ->
         (sqfun
            | Value.List l ->
                Value.List
                  (List.sort
                     (fun a b ->
                        let c = f (Value.List [a; b]) in
                        match c with
                          | Value.Symbol n
                              when n = Names.lt -> -1
                          | Value.Symbol n
                              when n = Names.eq -> 0
                          | Value.Symbol n
                              when n = Names.gt -> 1
                          | _ ->
                              Value.fail
                                Names.e_type
                                "comparator must return .<, .=, or .>") l)
            | Value.Record (tag, r) as r' ->
                (try
                   Value.generic
                     (Name.of_string "sort_with")
                     tag
                     (Value.List [Value.Function f; r'])
                 with Not_found ->
                   Value.fail
                     Names.e_type
                     ("sort_with is not defined for " ^ (Name.to_string tag)))
            | _ ->
                Value.fail
                  Names.e_type
                  "second argument must be a list or record")
     | _ ->
         Value.fail Names.e_type "first argument must be a function");

def "globals"
  (sqfun
     | Value.None -> Value.Record (Names.record,
                                   NameMap.map (!) !global_env)
     | _ -> Value.fail Names.e_type "function takes no arguments");

def "generics"
  (sqfun
     | Value.None -> !Value.generics
     | _ -> Value.fail Names.e_type "function takes no arguments");

def "def_generic"
  (sqfun
     | Value.Symbol func ->
         (sqfun
            | Value.Symbol tag ->
                (sqfun
                   | Value.Function f ->
                       Value.def_generic func tag f;
                       Value.None
                   | _ ->
                       Value.fail
                         Names.e_type
                         "third argument must be a function")
            | _ ->
                Value.fail
                  Names.e_type
                  "second argument must be a symbol")
     | _ ->
         Value.fail
           Names.e_type
           "first argument must be a symbol");

def "undef_generic"
  (sqfun
     | Value.Symbol func ->
         (sqfun
            | Value.Symbol tag ->
                Value.undef_generic func tag;
                Value.None
            | _ ->
                Value.fail
                  Names.e_type
                  "second argument must be a symbol")
     | _ ->
         Value.fail
           Names.e_type
           "first argument must be a symbol");

def "tag"
  (sqfun
     | Value.Symbol n ->
         (sqfun
            | Value.Record (_, r) -> Value.Record (n, r)
            | _ ->
                Value.fail
                  Names.e_type
                  "second argument must be a record")
     | _ -> Value.fail Names.e_type "first argument must be a symbol");

def "compare"
  (let rec cmp a b =
     try
       match (a, b) with
         | (Value.Record (t1, r1), Value.Record (t2, r2))
             when t1 = Names.record && t2 = Names.record ->
             NameMap.compare cmp r1 r2
         | (Value.Record (t1, r1), Value.Record (t2, r2)) ->
             (try
                (let f = Value.generic (Name.of_string "compare") t1 in
                 let c = f (Value.List [a; b]) in
                 match c with
                   | Value.Symbol n when n = Names.lt -> -1
                   | Value.Symbol n when n = Names.eq -> 0
                   | Value.Symbol n when n = Names.gt -> 1
                   | _ ->
                       Value.fail
                         Names.e_type
                         "comparator must return .<, .=, or .>")
              with Not_found -> compare r1 r2)
         | _ ->
             compare a b
     with
       | Invalid_argument "equal: functional value" ->
           Value.fail
             Names.e_type
             "functions cannot be compared"
   in
   (sqfun
      | Value.List [a; b] ->
          let c = cmp a b in
          if c < 0 then Value.Symbol Names.lt
          else if c > 0 then Value.Symbol Names.gt
          else Value.Symbol Names.eq
      | Value.List _ ->
          Value.fail Names.e_type "function requires a list of size 2"
      | _ ->
          Value.fail Names.e_type "argument must be a list"));

def "+" (arith_op ( + ) ( +. ));
def "-" (arith_op ( - ) ( -. ));
def "*" (arith_op ( * ) ( *. ));
def "/" (arith_op ( / ) ( /. ));
def "%" (arith_op (mod) (mod_float));
def "**" (arith_op (fun a b -> int_of_float (float a ** float b)) ( ** ));

def "sum"
  (sqfun
     | Value.List l ->
         List.fold_left (arith_op_apply ( + ) ( +. )) (Value.Int 0) l
     | _ ->
         Value.fail Names.e_type "argument must be a list");

def "product"
  (sqfun
     | Value.List l ->
         List.fold_left (arith_op_apply ( * ) ( *. )) (Value.Int 1) l
     | _ ->
         Value.fail Names.e_type "argument must be a list");

def "add"
  (sqfun
     | Value.List l ->
         (sqfun x -> Value.List (l @ [x]))
     | Value.Record (tag, oldvals) ->
         (sqfun
            | Value.Symbol n ->
                (sqfun x ->
                   Value.Record (tag, NameMap.add n x oldvals))
            | _ ->
                Value.fail
                  Names.e_type
                  "second argument must be a symbol")
     | _ ->
         Value.fail
           Names.e_type
           "first argument must be a list or record");

def "append"
  (sqfun
     | Value.List l ->
         Value.List
           (List.flatten
              (List.rev
                 (List.fold_left
                    (fun a x ->
                       match x with
                         | Value.List l' -> l' :: a
                         | _ ->
                             Value.fail
                               Names.e_type
                               "contents must be lists")
                    [] l)))
     | _ -> Value.fail Names.e_type "argument must be a list");

def "join"
  (sqfun
     | Value.String s ->
         (sqfun
            | Value.List l ->
                Value.String
                  (String.concat s (List.map Value.to_string l))
            | _ ->
                Value.fail
                  Names.e_type
                  "second argument must be a list")
     | Value.Char c ->
         (sqfun
            | Value.List l ->
                let s = String.make 1 c in
                Value.String
                  (String.concat s (List.map Value.to_string l))
            | _ ->
                Value.fail
                  Names.e_type
                  "second argument must be a list")
     | _ ->
         Value.fail
           Names.e_type "first argument must be a string or character");

def "split"
  (sqfun
     | Value.String d ->
         let regexp = Str.regexp d in
         (sqfun
            | Value.String s ->
                Value.List
                  (List.map
                     (fun x -> Value.String x) (Str.split_delim regexp s))
            | _ ->
                Value.fail
                  Names.e_type
                  "second argument must be a list")
     | Value.Char d ->
         let regexp = Str.regexp_string (String.make 1 d) in
         (sqfun
            | Value.String s ->
                Value.List
                  (List.map
                     (fun x -> Value.String x) (Str.split_delim regexp s))
            | _ ->
                Value.fail
                  Names.e_type
                  "second argument must be a list")
     | _ ->
         Value.fail
           Names.e_type "first argument must be a string or character");

def "slice"
  (sqfun
     | Value.List args ->
         let (start, stop) =
           match args with
             | [Value.Int start] -> (start, None)
             | [Value.Int start; Value.Int stop] -> (start, Some stop)
             | _ ->
                 Value.fail
                   Names.e_type
                   "function requires a list of size 1-2 integers"
         in
           (sqfun
              | Value.String s ->
                  let start =
                    if start >= 0 then start else max 0 (String.length s + start) in
                  let len =
                    match stop with
                      | None -> String.length s - start
                      | Some stop when stop >= 0 -> stop - start
                      | Some stop -> String.length s + stop - start
                  in
                    (try Value.String (String.sub s start len)
                     with Invalid_argument "String.sub" -> Value.String "")
              | Value.List l ->
                  let list_length = List.length l in
                  let start =
                    if start >= 0 then start else list_length + start in
                  let stop =
                    match stop with
                      | None -> list_length
                      | Some stop when stop >= 0 -> stop
                      | Some stop -> list_length + stop
                  in
                    Value.List
                      (List.rev
                         (snd
                            (List.fold_left
                               (fun (i, a) v ->
                                  (i + 1,
                                   if i >= start && i < stop
                                   then v :: a
                                   else a))
                               (0, []) l)))
              | _ ->
                  Value.fail
                    Names.e_type
                    "second argument must be a string or list")
     | _ -> Value.fail Names.e_type "first argument must be a list");

def "lowercase"
  (sqfun
     | Value.String s -> Value.String (String.lowercase s)
     | Value.Char c -> Value.Char (Char.lowercase c)
     | _ -> Value.fail Names.e_type "argument must be a string or character");

def "uppercase"
  (sqfun
     | Value.String s -> Value.String (String.uppercase s)
     | Value.Char c -> Value.Char (Char.uppercase c)
     | _ -> Value.fail Names.e_type "argument must be a string or character");

def "starts_with"
  (sqfun
     | Value.String p ->
         (sqfun
            | Value.String s ->
                (Value.of_bool
                   (let len = String.length p in
                    if String.length s < len then
                      false
                    else
                      String.sub s 0 len = p))
            | _ ->
                Value.fail
                  Names.e_type "second argument must be a string")
     | _ ->
         Value.fail
           Names.e_type "first argument must be a string");

def "ends_with"
  (sqfun
     | Value.String e ->
         (sqfun
            | Value.String s ->
                (Value.of_bool
                   (let el = String.length e in
                    let sl = String.length s in
                    if sl < el then
                      false
                    else
                      String.sub s (sl - el) el = e))
            | _ ->
                Value.fail
                  Names.e_type "second argument must be a string")
     | _ ->
         Value.fail
           Names.e_type "first argument must be a string");

def "update"
  (sqfun
     | Value.Record (tag, oldvals) ->
         (sqfun
            | Value.Record (_, newvals) ->
                Value.Record (tag,
                              NameMap.fold NameMap.add newvals oldvals)
            | _ ->
                Value.fail
                  Names.e_type
                  "second argument must be a record")
     | _ ->
         Value.fail
           Names.e_type
           "first argument must be a record");

def "remove"
  (sqfun
     | Value.Record (tag, r) ->
         (sqfun
            | Value.Symbol n ->
                Value.Record (tag, NameMap.remove n r)
            | _ ->
                Value.fail
                  Names.e_type
                  "second argument must be a symbol")
     | _ ->
         Value.fail
           Names.e_type
           "first argument must be a record");

def "Array"
  (Value.record Names.module_ [
     Name.of_string "make",
     (sqfun
        | Value.Int i ->
            (sqfun x -> make_array (Array.make i x))
        | _ ->
            Value.fail Names.e_type "first argument must be an integer")
   ]);

def "File"
  (let make_out_file out_channel =
     Value.record (Name.of_string "file") [
       Name.of_string "close",
       (sqfun
          | Value.None ->
              (try close_out out_channel; Value.None
               with
                 | Sys_error e ->
                     Value.fail (Name.of_string "IOError") e)
          | _ ->
              Value.fail Names.e_type "function takes no arguments");
     ]
   in
   let make_in_file in_channel =
     Value.record (Name.of_string "file") [
       Name.of_string "read",
       (sqfun
          | Value.None ->
              (try
                 Value.String (input_all in_channel)
               with Sys_error e ->
                 Value.fail (Name.of_string "IOError") e)
          | _ ->
              Value.fail Names.e_type "function takes no arguments");

       Name.of_string "read_byte",
       (sqfun
          | Value.None ->
              (try Value.Int (input_byte in_channel)
               with
                 | End_of_file ->
                     Value.fail_value (Name.of_string "EndOfFile") Value.None
                 | Sys_error e ->
                     Value.fail (Name.of_string "IOError") e)
          | _ ->
              Value.fail Names.e_type "function takes no arguments");

       Name.of_string "read_char",
       (sqfun
          | Value.None ->
              (try Value.Char (input_char in_channel)
               with
                 | End_of_file ->
                     Value.fail_value (Name.of_string "EndOfFile") Value.None
                 | Sys_error e ->
                     Value.fail (Name.of_string "IOError") e)
          | _ ->
              Value.fail Names.e_type "function takes no arguments");

       Name.of_string "read_line",
       (sqfun
          | Value.None ->
              (try Value.String (input_line in_channel)
               with
                 | End_of_file ->
                     Value.fail_value (Name.of_string "EndOfFile") Value.None
                 | Sys_error e ->
                     Value.fail (Name.of_string "IOError") e)
          | _ ->
              Value.fail Names.e_type "function takes no arguments");

       Name.of_string "close",
       (sqfun
          | Value.None ->
              (try close_in in_channel; Value.None
               with
                 | Sys_error e ->
                     Value.fail (Name.of_string "IOError") e)
          | _ ->
              Value.fail Names.e_type "function takes no arguments");
     ]
   in
     (Value.record Names.module_ [
        Name.of_string "open_in",
        (sqfun
           | Value.String filename ->
               (try make_in_file (open_in filename)
                with
                  | Sys_error e ->
                      Value.fail (Name.of_string "IOError") e)
           | _ ->
               Value.fail Names.e_type "argument must be a string");
        Name.of_string "open_in_bin",
        (sqfun
           | Value.String filename ->
               (try make_in_file (open_in_bin filename)
                with
                  | Sys_error e ->
                      Value.fail (Name.of_string "IOError") e)
           | _ ->
               Value.fail Names.e_type "argument must be a string");
        Name.of_string "open_out",
        (sqfun
           | Value.String filename ->
               (try make_out_file (open_out filename)
                with
                  | Sys_error e ->
                      Value.fail (Name.of_string "IOError") e)
           | _ ->
               Value.fail Names.e_type "argument must be a string");
        Name.of_string "open_out_bin",
        (sqfun
           | Value.String filename ->
               (try make_out_file (open_out_bin filename)
                with
                  | Sys_error e ->
                      Value.fail (Name.of_string "IOError") e)
           | _ ->
               Value.fail Names.e_type "argument must be a string");
     ]));

def "MD5"
  (Value.record Names.module_ [
     Name.of_string "hex",
     (sqfun
        | Value.String s -> Value.String (Digest.to_hex (Digest.string s))
        | _ -> Value.fail Names.e_type "argument must be a string");

     Name.of_string "hex_file",
     (sqfun
        | Value.String s ->
            Value.String
              (try (Digest.to_hex (Digest.file s))
               with Sys_error e -> Value.fail (Name.of_string "OSError") e)
        | _ -> Value.fail Names.e_type "argument must be a string");

     Name.of_string "raw",
     (sqfun
        | Value.String s -> Value.String (Digest.string s)
        | _ -> Value.fail Names.e_type "argument must be a string");

     Name.of_string "raw_file",
     (sqfun
        | Value.String s ->
            Value.String
              (try (Digest.file s)
               with Sys_error e -> Value.fail (Name.of_string "OSError") e)
        | _ -> Value.fail Names.e_type "argument must be a string");
   ]);

def "Math"
  (Value.record Names.module_ [
     Name.of_string "acos", arith_unary acos;
     Name.of_string "asin", arith_unary asin;
     Name.of_string "atan", arith_unary atan;
     Name.of_string "atan2", arith_binary atan2;
     Name.of_string "ceil", arith_unary ceil;
     Name.of_string "cos", arith_unary cos;
     Name.of_string "cosh", arith_unary cosh;
     Name.of_string "e", Value.Float (exp 1.0);
     Name.of_string "exp", arith_unary exp;
     Name.of_string "floor", arith_unary floor;
     Name.of_string "log", arith_unary log;
     Name.of_string "log10", arith_unary log10;
     Name.of_string "Phi", Value.Float ((sqrt 5.0 +. 1.0) /. 2.0);
     Name.of_string "phi", Value.Float ((sqrt 5.0 -. 1.0) /. 2.0);
     Name.of_string "pi", Value.Float (4.0 *. atan 1.0);
     Name.of_string "sin", arith_unary sin;
     Name.of_string "sinh", arith_unary sinh;
     Name.of_string "sqrt", arith_unary sqrt;
     Name.of_string "tan", arith_unary tan;
     Name.of_string "tanh", arith_unary tanh;
   ]);

def "OS"
  (Value.record Names.module_ [
     Name.of_string "args",
     make_array (Array.map (fun x -> Value.String x) Sys.argv);

     Name.of_string "environ",
     make_array (Array.map (fun x -> Value.String x) (Unix.environment ()));

     Name.of_string "chdir",
       (sqfun
          | Value.String s ->
              (try (Sys.chdir s; Value.None)
               with Sys_error e -> Value.fail (Name.of_string "OSError") e)
          | _ -> Value.fail Names.e_type "argument must be a string");

     Name.of_string "chmod",
       (sqfun
          | Value.String path ->
              (sqfun
                 | Value.Int mode ->
                     (try (Unix.chmod path mode; Value.None)
                      with
                        | Unix.Unix_error (e, _, n) ->
                            Value.fail
                              (Name.of_string "OSError")
                              ((Unix.error_message e) ^ ": " ^ n))
                 | _ -> Value.fail Names.e_type "second argument must be an integer")
          | _ ->
              Value.fail Names.e_type "first argument must be a string");

     Name.of_string "chroot",
       (sqfun
          | Value.String s ->
              (try (Unix.chroot s; Value.None)
               with
                 | Unix.Unix_error (e, _, n) ->
                     Value.fail
                       (Name.of_string "OSError")
                       ((Unix.error_message e) ^ ": " ^ n))
          | _ -> Value.fail Names.e_type "argument must be a string");

     Name.of_string "getcwd",
       (sqfun
          | Value.None ->
              Value.String (Sys.getcwd ())
          | _ ->
              Value.fail Names.e_type "function takes no arguments");

     Name.of_string "link",
       (sqfun
          | Value.String s1 ->
             (sqfun
                | Value.String s2 ->
                    (try (Unix.link s1 s2; Value.None)
                       with
                         | Unix.Unix_error (e, _, n) ->
                             Value.fail
                               (Name.of_string "OSError")
                               ((Unix.error_message e) ^ ": " ^ n))
                | _ ->
                    Value.fail Names.e_type "second argument must be a string")
          | _ ->
              Value.fail Names.e_type "first argument must be a string");

     Name.of_string "listdir",
       (sqfun
          | Value.String s ->
              (try (Value.List (Array.to_list (Array.map
                                                 (fun x -> Value.String x)
                                                 (Sys.readdir s))))
               with Sys_error e -> Value.fail (Name.of_string "OSError") e)
          | _ -> Value.fail Names.e_type "argument must be a string");

     Name.of_string "rename",
       (sqfun
          | Value.String s1 ->
             (sqfun
                | Value.String s2 ->
                    (try (Unix.rename s1 s2; Value.None)
                       with
                         | Unix.Unix_error (e, _, n) ->
                             Value.fail
                               (Name.of_string "OSError")
                               ((Unix.error_message e) ^ ": " ^ n))
                | _ ->
                    Value.fail Names.e_type "second argument must be a string")
          | _ ->
              Value.fail Names.e_type "first argument must be a string");

     Name.of_string "stat",
       (sqfun
          | Value.String s ->
              (try
                 let {Unix.st_dev=dev; st_ino=ino;
                      st_kind=kind; st_perm=perm; st_nlink=nlink;
                      st_uid=uid; st_gid=gid; st_rdev=rdev; st_size=size;
                      st_atime=atime; st_mtime=mtime; st_ctime=ctime} =
                   Unix.stat s in
                 Value.record Names.record [
                   Name.of_string "dev", Value.Int dev;
                   Name.of_string "ino", Value.Int ino;
                   Name.of_string "kind", Value.Symbol
                     (Name.of_string (match kind with
                                        | Unix.S_REG -> "reg"
                                        | Unix.S_DIR -> "dir"
                                        | Unix.S_CHR -> "chr"
                                        | Unix.S_BLK -> "blk"
                                        | Unix.S_LNK -> "lnk"
                                        | Unix.S_FIFO -> "fifo"
                                        | Unix.S_SOCK -> "sock"));
                   Name.of_string "perm", Value.Int perm;
                   Name.of_string "nlink", Value.Int nlink;
                   Name.of_string "uid", Value.Int uid;
                   Name.of_string "gid", Value.Int gid;
                   Name.of_string "rdev", Value.Int rdev;
                   Name.of_string "size", Value.Int size;
                   Name.of_string "atime", Value.Float atime;
                   Name.of_string "mtime", Value.Float mtime;
                   Name.of_string "ctime", Value.Float ctime;
                 ]
               with
                 | Unix.Unix_error (e, _, n) ->
                     Value.fail
                       (Name.of_string "OSError")
                       ((Unix.error_message e) ^ ": " ^ n))
          | _ -> Value.fail Names.e_type "argument must be a string");

     Name.of_string "system",
       (sqfun
          | Value.String cmd ->
              Value.Int (Sys.command cmd)
          | _ ->
              Value.fail Names.e_type "argument must be a string");

     Name.of_string "unlink",
       (sqfun
          | Value.String s ->
              (try (Unix.unlink s; Value.None)
               with
                 | Unix.Unix_error (e, _, n) ->
                     Value.fail
                       (Name.of_string "OSError")
                       ((Unix.error_message e) ^ ": " ^ n))
          | _ -> Value.fail Names.e_type "argument must be a string");
   ]);

def "RE"
  (Value.record Names.module_ [
     Name.of_string "quote",
       (sqfun s ->
          Value.String (Str.quote (Value.to_string s)));

     Name.of_string "compile",
       (sqfun
          | Value.String re ->
              let regexp = Str.regexp re in
              (Value.record (Name.of_string "RE") [
                 Name.of_string "search",
                   (sqfun
                      | Value.String s ->
                          (try
                             let start = Str.search_forward regexp s 0 in
                             let end_ = Str.match_end () in
                             let groups =
                               List.rev
                                 (let rec loop i a =
                                    (try
                                       loop
                                         (i + 1)
                                         (Value.String (Str.matched_group i s)
                                          :: a)
                                     with
                                       | Invalid_argument "Str.matched_group"
                                         -> a) in
                                  loop 0 []) in
                             Value.record Names.record [
                               Name.of_string "start", Value.Int start;
                               Name.of_string "end", Value.Int end_;
                               Name.of_string "groups", Value.List groups;
                             ]
                           with
                             | Not_found -> Value.None)
                      | _ ->
                          Value.fail
                            Names.e_type "first argument must be a string");

                 Name.of_string "replace",
                   (sqfun
                      | Value.String repl ->
                          (sqfun
                            | Value.String s ->
                                (Value.String
                                   (Str.global_replace regexp repl s))
                            | _ ->
                                Value.fail
                                  Names.e_type
                                  "second argument must be a string")
                      | _ ->
                          Value.fail
                            Names.e_type
                            "first argument must be a string");
               ])
          | _ ->
              Value.fail
                Names.e_type "first argument must be a string")
   ]);

def "Time"
  (Value.record Names.module_ [
     Name.of_string "time",
       (sqfun
          | Value.None ->
              Value.Float (Unix.gettimeofday ())
          | _ ->
              Value.fail
                Names.e_type
                "function takes no arguments")
   ]);
()

let _ =
  try ignore (eval_string global_env "

defun generic sym ->
  fun x ->
    let {g: try generics [] sym [typeof x]
            catch {FieldNotFound} ->
              throw {ValueError: join \"\" ([symbol_name sym],
                                            \" is not defined for \",
                                            [symbol_name [typeof x]])}}
    g x;

defun  = a -> fun b -> is .= [compare (a, b)];
defun <> a -> fun b -> not [is .= [compare (a, b)]];
defun <  a -> fun b -> is .< [compare (a, b)];
defun >  a -> fun b -> is .> [compare (a, b)];
defun <= a -> fun b -> not [is .> [compare (a, b)]];
defun >= a -> fun b -> not [is .< [compare (a, b)]];

defun @ x -> x.value[];
defun @= x -> fun y -> x.set_value y;

defun abs x -> if or (< x 0, = x -0.0) then neg x else x;

defun assoc key -> fun alist ->
  letrec {
    loop:  fun list -> if list then match [head list] [tail list] else [],
    match: fun (k, v) -> fun rest -> if = key k then v else loop rest,
  }
  loop alist;

defun assq key -> fun alist ->
  letrec {
    loop:  fun list -> if list then match [head list] [tail list] else [],
    match: fun (k, v) -> fun rest -> if is key k then v else loop rest,
  }
  loop alist;

defun compose fs ->
  letrec {
    loop: fun (f, fs) ->
      if fs
      then [let {g: head fs, fs: tail fs}
            loop (fun x -> f [g x], fs)]
      else f
  }
  loop (head fs, tail fs);

def copy generic .copy;

defun default d -> fun v -> fun x ->
  try v x
  catch {IndexNotFound} -> d
  catch {FieldNotFound} -> d;

defun each xs -> fun f ->
  fold xs [fun (_, v, _) -> [f v; []]] [];

defun eachi xs -> fun f ->
  fold xs [fun (i, v, _) -> [f (i, v); []]] [];

defun filter xs -> fun f ->
  reverse [fold xs [fun (_, v, a) -> if f v then cons v a else a] ()];

defun filteri xs -> fun f ->
  reverse [fold xs [fun (i, v, a) -> if f (i, v) then cons (i, v) a else a] ()];

defun flip f -> fun x -> fun y -> f y x;

def hex
  let {
    int_0:  int '0',
    int_9:  int '9',
    lookup: {a: 10, b: 11, c: 12, d: 13, e: 14, f: 15},
  }
  let {
    hex_to_dec: fun h ->
      let {int_h: int h}
      if and (>= int_h int_0, <= int_h int_9)
      then int [string h]
      else [
        try lookup [symbol [string [lowercase h]]]
        catch {FieldNotFound} -> throw {ValueError: h}
      ]
  }
  fun str ->
    letrec {
      loop: fun (idx, acc) ->
        if >= idx 0
        then [+ [hex_to_dec [str idx]] [* 16 [loop (- idx 1, acc)]]]
        else acc
    }
    loop (- [size str] 1, 0);

defun id x -> x;

defun indexes xs -> mapi xs [fun (i, _) -> i];
def fields indexes;

defun isa type -> fun value -> is type [typeof value];

defun map xs -> fun f ->
  reverse [fold xs [fun (_, v, a) -> cons [f v] a] ()];

defun mapi xs -> fun f ->
  reverse [fold xs [fun (i, v, a) -> cons [f (i, v)] a] ()];

def min [];
def max [];
let {
  optimum: fun f -> fun xs ->
    letrec {
      loop: fun (m, xs) ->
        if xs
        then [let {x: head xs, xs: tail xs}
              if f x m then loop (x, xs) else loop (m, xs)]
        else m
    }
    loop (head xs, xs)
}
do [
  set min optimum <;
  set max optimum >;
];

defun module r -> tag .module r;

defun neg x -> - 0 x;

defun octal num ->
  letrec {
    loop: fun (rem, acc) ->
      if = rem 0 then acc else [+ [% rem 10] [* 8 [loop (/ rem 10, acc)]]]
  }
  loop (num, 0);

defun pairs x -> mapi x [fun p -> p];

defun println x -> [print x; print \"\n\"];

defun range args ->
  let {
    _: if not [isa .record args]
       then throw {TypeError: \"function requires a record argument\"},
    _: if not [in args .start]
       then [
         if not [in args .stop]
         then throw {ValueError: \"either start or stop must be provided\"}
       ],
  }
  let {
    start: if in args .start then args.start else 0,
    stop:  if in args .stop  then args.stop  else 0,
    step:  if in args .step  then args.step  else 1,
  }
  let {
    _: if = step 0 then throw {ValueError: \"step must not be zero\"},
    in_range: if < step 0 then > else <,
    current: start,
  }
  stream fun [] ->
    if in_range current stop
    then let {result: current} [set current [+ current step]; result]
    else throw {EndOfStream: []};

defun ref value ->
  tag .ref {value: fun [] -> value,
            set_value: fun x -> set value x};

defun replace from -> fun to -> fun s ->
  join [string to] [split [RE.quote from] s];

def reverse generic .reverse;

def sort sort_with compare;

defun stream seq_or_func ->
  if is .stream [typeof seq_or_func]
  then seq_or_func
  else [
    tag .stream
    if is .function [typeof seq_or_func]
    then {next: seq_or_func}
    else [
      let {buffer: values seq_or_func}
      {next: fun [] ->
         if buffer
         then let {result: head buffer} [!tail buffer; result]
         else throw {EndOfStream: []}}
    ]
  ];

defun take n -> fun stream ->
  if > n 0
  then append (take [- n 1] stream,
               try (stream.next []) catch {EndOfStream} -> ())
  else ();

defun drop n -> fun stream ->
  do [
    if > n 0 then [
      try stream.next [] catch {EndOfStream} -> [];
      drop [- n 1] stream;
    ];
    stream
  ];

defun count_from n ->
  stream fun [] ->
    let {result: n}
    do [
      set n [+ n 1];
      result
    ];

defun chain streams ->
  let {streams: stream streams}
  let {current: stream [streams.next []]}
  letrec {
    find_next: fun [] ->
      try current.next []
      catch {EndOfStream} -> [
        set current stream [streams.next []];
        find_next []
      ]
  }
  stream find_next;

defun map_stream s -> fun f ->
  stream fun [] ->
    f [s.next []];

defun filter_stream s -> fun f ->
  letrec {
    find_next: fun [] ->
      let {result: s.next []}
      if f result then result else find_next []
  }
  stream find_next;

defun values xs ->
  if is .list [typeof xs]
  then xs
  else map xs id;

!update Array {
  get: fun a -> a.get,
  put: fun a -> a.put,

  fold: fun (arr, f, init) ->
    let {size: size arr}
    letrec {
      loop: fun (i, a) -> [
        if < i size
        then loop (+ i 1, f (i, arr.get i, a))
        else a
      ],
    }
    loop (0, init),

  from_list: fun list ->
    let {result: Array.make [size list] []} [
      eachi list fun (i, v) -> result.put i v;
      result
    ],
};

!update File {
  bytes: fun file ->
    stream fun [] ->
      try file.read_byte [] catch {EndOfFile} -> throw {EndOfStream: []},

  chars: fun file ->
    stream fun [] ->
      try file.read_char [] catch {EndOfFile} -> throw {EndOfStream: []},

  lines: fun file ->
    stream fun [] ->
      try file.read_line [] catch {EndOfFile} -> throw {EndOfStream: []},

  close: fun {close} -> close [],
};

def Hash module {
  make: fun size ->
    tag .hash
    let {s: min (max (1, size), 4194303)}
    {size: ref 0, data: ref [Array.make s .empty]},

  from_list: fun lst ->
    let {h: Hash.make [size lst]}
    do [
      each lst fun (k, v) ->
        Hash.put h k v;
      h;
    ],

  copy: fun {size, data} ->
    tag .hash
    {size: ref [@size], data: ref [copy [@data]]},

  clear: fun {size, data} ->
    letrec {
      loop: fun i ->
        if < i [size [@data]]
        then [Array.put [@data] i .empty; loop [+ 1 i]],
    }
    do [
      loop 0;
      @= size 0;
    ],

  _resize: fun hashfun -> fun {size, data} ->
    letrec {
      odata: @data,
      osize: globals[].size odata,
      nsize: min (+ 1 [* 2 osize], 4194303),
    }
    if <> nsize osize then
      let {ndata: Array.make nsize .empty}
      letrec {
        insert_bucket: fun x ->
          if is .empty x
          then []
          else [
            insert_bucket [x.rest];
            let {nidx: % [hashfun [x.key]] nsize}
            Array.put ndata nidx {key: x.key, data: x.data, rest: ndata nidx}
          ],

        loop: fun i ->
          if < i osize then [insert_bucket [odata i]; loop [+ 1 i]],
      }
      do [
        loop 0;
        @= data ndata;
      ],

  get: fun {size, data} -> fun key ->
    letrec {
      get_rec: fun key -> fun x ->
        if is .empty x
        then throw {IndexNotFound: key}
        elif is .= [compare (x.key, key)]
        then x.data
        else get_rec key [x.rest],
    }
    let {x: @data [% [hash key] [globals[].size [@data]]]}
    if is .empty x
    then throw {IndexNotFound: key}
    elif is .= [compare (x.key, key)]
    then x.data
    else get_rec key [x.rest],

  put: fun {size, data} -> fun key -> fun info ->
    letrec {
      replace_bucket: fun x ->
        if is .empty x
        then throw {IndexNotFound: 0}
        elif is .= [compare (x.key, key)]
        then {key: x.key, data: info, rest: x.rest}
        else {key: x.key, data: x.data, rest: replace_bucket [x.rest]},
    }
    let {i: % [hash key] [globals[].size [@data]]}
    let {l: @data i}
    try
      Array.put [@data] i [replace_bucket l]
    catch {IndexNotFound} -> [
      Array.put [@data] i {key: key, data: info, rest: l};
      @= size [+ 1 [@size]];
      if > [@size] [* [globals[].size [@data]] 2] then Hash._resize hash h;
    ],

  remove: fun {size, data} -> fun key ->
    letrec {
      remove_bucket: fun x ->
        if is .empty x
        then .empty
        else if is .= [compare (x.key, key)]
        then [@= size [- [@size] 1]; x.rest]
        else {key: x.key, data: x.data, rest: remove_bucket [x.rest]},
    }
    let {i: % [hash key] [globals[].size [@data]]}
    Array.put [@data] i [remove_bucket [@data i]],

  fold: fun (h, f, init) ->
    letrec {
      do_bucket: fun b -> fun accu ->
        if is .empty b
        then accu
        else do_bucket [b.rest] [f (b.key, b.data, accu)],
    }
    let {
      d: @[h.data],
      accu: ref init,
    }
    do [
      each d [fun bucket -> @= accu [do_bucket bucket [@accu]]];
      @accu;
    ],
};

!update Math {
  degrees: fun x -> / [* x 180.0] [Math.pi],
  radians: fun x -> / [* x [Math.pi]] 180.0,
};

!update RE {
  search:  fun re -> [RE.compile re].search,
  replace: fun re -> [RE.compile re].replace,
};

def_generic .call .array [Array.get];
def_generic .call .hash [Hash.get];

def_generic .compare .array fun (a, b) -> compare (values a, values b);
def_generic .compare .ref fun (a, b) -> compare (a.value [], b.value []);

def_generic .copy .array fun x -> x.copy [];
def_generic .copy .hash [Hash.copy];

def_generic .fold .array [Array.fold];
def_generic .fold .hash [Hash.fold];

def_generic .fold .stream fun (stream, func, init) ->
  letrec {
    loop: fun (i, acc) ->
      let {next: [], done: .false}
      do [
        try set next stream.next []
        catch {EndOfStream} -> set done .true;
        if done then acc else loop (+ i 1, func (i, next, acc));
      ]
  }
  loop (0, init);

def_generic .reverse .string fun s ->
  join \"\" [fold s [fun (_, v, a) -> cons v a] ()];
def_generic .reverse .list fun l ->
  fold l [fun (_, v, a) -> cons v a] ();
def_generic .reverse .array fun a ->
  Array.from_list [reverse [values a]];

def_generic .size .array fun x -> x.size [];
def_generic .size .hash fun {size} -> @size;

def_generic .sort_with .array fun (cmp, arr) ->
  Array.from_list [sort_with cmp [values arr]];

def_generic .to_string .array fun x -> join \"\" (\"array\", values x);
def_generic .to_string .hash fun x -> join \"\" (\"hash\", pairs x);
def_generic .to_string .ref fun ref ->
  join \"\" (\"[ref \", string [ref.value []], \"]\");

!add OS.environ [
  fold [OS.environ]
       [fun (_, v, a) ->
          let {vals: split '=' v}
          add a [symbol [head vals]] [join '=' [tail vals]]]
       {}
];

") with Value.Error e ->
    print_endline ("Prelude Error: " ^ (Value.to_string e))
