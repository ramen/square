open Printf

open Ast
open Name
open Names
open Value
open Parse

let rec eval env = function
  | AST.Name n -> !(try NameMap.find n !env
                    with Not_found ->
                      Value.fail Names.e_name (Name.to_string n))
  | AST.Symbol n -> Value.Symbol n
  | AST.String s -> Value.String s
  | AST.Int i -> Value.Int i
  | AST.Float f -> Value.Float f
  | AST.Char c -> Value.Char c
  | AST.List l -> Value.List (List.map (eval env) l)
  | AST.Record r -> Value.Record (Names.record, NameMap.map (eval env) r)
  | AST.Do [] -> Value.None
  | AST.Do [expr] -> eval env expr
  | AST.Do (expr :: exprs) -> let _ = eval env expr in eval env (AST.Do exprs)
  | AST.Def (n, v) ->
      env := NameMap.add n (ref (eval env v)) !env;
      Value.None
  | AST.Set (n, v) ->
      (try (NameMap.find n !env) := (eval env v)
       with Not_found ->
         Value.fail
           Names.e_name ("name " ^ (Name.to_string n) ^ " not found"));
      Value.None
  | AST.Undef n ->
      env := NameMap.remove n !env;
      Value.None
  | AST.Let (bindings, a) ->
      let new_env =
        List.fold_left
          (fun a (n, v) ->
             NameMap.add n (ref (eval env v)) a) !env bindings in
      eval (ref new_env) a
  | AST.If [] -> Value.None
  | AST.If (elseexpr :: []) -> eval env elseexpr
  | AST.If (ifcond :: ifexpr :: rest) ->
    (if Value.to_bool (eval env ifcond)
     then eval env ifexpr
     else eval env (AST.If rest))
  | AST.App (f, a) ->
      (match (eval env f) with
         | Value.Function f' -> f' (eval env a)
         | Value.String s ->
             (match (eval env a) with
                | Value.Int i ->
                    (try Value.Char s.[i]
                     with Invalid_argument "index out of bounds" ->
                       Value.fail_value Names.e_index_nf (Value.Int i))
                | _ -> Value.fail Names.e_type "can't apply non-integer to string")
         | Value.List l ->
             (match (eval env a) with
                | Value.Int i -> (try List.nth l i
                                  with Failure "nth" ->
                                    Value.fail_value
                                      Names.e_index_nf (Value.Int i))
                | _ -> Value.fail Names.e_type "can't apply non-integer to list")
         | Value.Record (tag, r) as r' ->
             (match (eval env a) with
                | Value.Symbol n -> (try NameMap.find n r
                                     with Not_found ->
                                       Value.fail_value
                                         Names.e_field_nf (Value.Symbol n))
                | _ as arg -> 
                    (try
                       (match Value.generic (Name.of_string "call") tag r' with
                          | Value.Function f -> f arg
                          | _ -> Value.fail Names.e_type "non-function in generics table")
                     with Not_found ->
                       Value.fail Names.e_type ("call is not defined for " ^ (Name.to_string tag))))
         | _ as v -> Value.fail Names.e_type ("call of non-function: " ^ (Value.to_string v)))
  | AST.Fun (AST.EmptyPat, ast) ->
      Value.Function
        (function
           | Value.None -> eval (ref !env) ast
           | _ -> Value.fail Names.e_type "function takes no arguments")
  | AST.Fun (AST.ScalarPat name, ast) ->
      Value.Function (fun arg ->
                        eval (ref (NameMap.add name (ref arg) !env)) ast)
  | AST.Fun (AST.ListPat names, ast) ->
      Value.Function
        (function
           | Value.List args ->
               let new_env =
                 (try
                    List.fold_left2
                      (fun a n v -> NameMap.add n (ref v) a) !env names args
                  with
                    | Invalid_argument "List.fold_left2" ->
                        Value.fail
                          Names.e_type
                          (sprintf
                             "function requires a list of size %d"
                             (List.length names))) in
               eval (ref new_env) ast
           | _ -> Value.fail Names.e_type "function requires a list argument")
  | AST.Fun (AST.RecordPat names, ast) ->
      Value.Function
        (function
           | Value.Record (_, args) ->
               let new_env =
                 List.fold_left
                   (fun a n ->
                      try NameMap.add n (ref (NameMap.find n args)) a
                      with Not_found ->
                        Value.fail
                          Names.e_type
                          (sprintf
                             "function requires a record with field %s"
                             (Name.to_string n))) !env names in
               eval (ref new_env) ast
           | _ ->
               Value.fail Names.e_type "function requires a record argument")
  | AST.Try (expr, catches) ->
      (try (eval env expr)
       with Value.Error e as exn ->
         let rec loop = function
           | [] -> raise exn
           | (catch :: catches) ->
               (match catch with
                  | AST.ValCatch (n, a) ->
                      let new_env = NameMap.add n (ref e) !env in
                      (eval (ref new_env) a)
                  | AST.PatCatch (AST.EmptyPat, a)
                    -> (eval env a)
                  | AST.PatCatch (AST.ScalarPat n, a) ->
                      let new_env = NameMap.add n (ref e) !env in
                      (eval (ref new_env) a)
                  | AST.PatCatch (AST.ListPat names, a) ->
                      (match e with
                         | Value.List l ->
                             (try
                                (let new_env = List.fold_left2
                                   (fun a n v -> NameMap.add n (ref v) a)
                                   !env names l in
                                 (eval (ref new_env) a))
                              with Invalid_argument "List.fold_left2" ->
                                loop catches)
                         | _ -> loop catches)
                  | AST.PatCatch (AST.RecordPat names, a) ->
                      (match e with
                         | Value.Record (_, r) ->
                             (try
                                (let new_env =
                                   List.fold_left
                                     (fun a n ->
                                        NameMap.add
                                          n (ref (NameMap.find n r)) a)
                                     !env names in
                                 (eval (ref new_env) a))
                              with Not_found -> loop catches)
                         | _ -> loop catches))
         in loop catches)

let eval_stream env stream =
  let ast_stream = (parse_stream (lexer stream)) in
  let result = ref Value.None in
  Stream.iter (fun ast -> result := eval env ast) ast_stream;
  result

let eval_string env string =
  eval_stream env (Stream.of_string string)
