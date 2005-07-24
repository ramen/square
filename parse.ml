open Ast
open Name
open Names

let keywords = ["["; "]"; "{"; "}"; "("; ")"; "."; ","; ":"; ";"; "!";
                "do"; "def"; "set"; "undef"; "let"; "letrec";
                "if"; "then"; "elif"; "else"; "and"; "or";
                "fun"; "defun"; "try"; "catch"; "->"]

let lexer = Genlex.make_lexer keywords

let rec parse = parser
  | [< expr = parse_expr; rest = parse_rest expr >] -> rest

and parse_rest expr = parser
  | [< arg = parse_expr; rest = parse_rest (AST.App (expr, arg)) >] -> rest
  | [< >] -> expr

and parse_expr = parser
  | [< 'Genlex.String s >] -> AST.String s
  | [< 'Genlex.Int i >] -> AST.Int i
  | [< 'Genlex.Float f >] -> AST.Float f
  | [< 'Genlex.Char c >] -> AST.Char c
  | [< 'Genlex.Ident s >] -> AST.Name (Name.of_string s)
  | [< 'Genlex.Kwd ".";
       'Genlex.Ident s ?? "symbol name expected after '.'" >]
    -> AST.Symbol (Name.of_string s)
  | [< 'Genlex.Kwd "(";
       list = parse_list [] ?? "unexpected end of list" >]
    -> AST.List (List.rev list)
  | [< 'Genlex.Kwd "{";
       items = parse_record [] ?? "unexpected end of record" >]
    -> AST.record (List.rev items)
  | [< 'Genlex.Kwd "[";
       list = parse_do [] ?? "unexpected end of do-block" >]
    -> AST.Do (List.rev list)
  | [< 'Genlex.Kwd "do";
       'Genlex.Kwd "[" ?? "'[' expected after 'do'";
       list = parse_do [] ?? "unexpected end of do-block" >]
    -> AST.Do (List.rev list)
  | [< 'Genlex.Kwd "def";
       'Genlex.Ident name ?? "name expected after 'def'";
       expr = parse ?? "missing expression after 'def'" >]
    -> AST.Def ((Name.of_string name), expr)
  | [< 'Genlex.Kwd "set";
       'Genlex.Ident name ?? "name expected after 'set'";
       expr = parse ?? "missing expression after 'set'" >]
    -> AST.Set ((Name.of_string name), expr)
  | [< 'Genlex.Kwd "!";
       func = parse_mutator;
       'Genlex.Ident arg;
       rest = parse_rest (AST.App (func,
                                   AST.Name (Name.of_string arg))) >]
    -> AST.Set ((Name.of_string arg), rest)
  | [< 'Genlex.Kwd "undef";
       'Genlex.Ident name ?? "name expected after 'undef'" >]
    -> AST.Undef (Name.of_string name)
  | [< 'Genlex.Kwd "let";
       'Genlex.Kwd "{" ?? "'{' expected after 'let'";
       items = parse_record [] ?? "unexpected end of let bindings";
       expr = parse ?? "missing expression after 'let'" >]
    -> AST.Let (List.rev items, expr)
  | [< 'Genlex.Kwd "letrec";
       'Genlex.Kwd "{" ?? "'{' expected after 'letrec'";
       items = parse_record [] ?? "unexpected end of letrec bindings";
       expr = parse ?? "missing expression after 'letrec'" >]
    -> (let inits = List.map (fun (k, _) -> (k, AST.Do [])) items in
        let binds = List.map (fun (k, v) -> AST.Set (k, v)) (List.rev items) in
        AST.Let (inits, AST.Do (binds @ [expr])))
  | [< 'Genlex.Kwd "if";
       clauses = parse_if ?? "missing expression after 'if'" >]
    -> AST.If (List.rev clauses)
  | [< 'Genlex.Kwd "and";
       'Genlex.Kwd "(";
       list = parse_list [] >]
    -> (List.fold_left
          (fun a v -> AST.If [v; a; (AST.Symbol Names.false_)])
          (AST.Symbol (Names.true_))
          list)
  | [< 'Genlex.Kwd "or";
       'Genlex.Kwd "(";
       list = parse_list [] >]
    -> (List.fold_left
          (fun a v -> AST.If [v; (AST.Symbol Names.true_); a])
          (AST.Symbol (Names.false_))
          list)
  | [< 'Genlex.Kwd "fun";
       pat = parse_pattern ?? "pattern expected after 'fun'";
       'Genlex.Kwd "->" ?? "'->' expected after 'fun'";
       expr = parse ?? "missing expression after 'fun'" >]
    -> AST.Fun (pat, expr)
  | [< 'Genlex.Kwd "defun";
       'Genlex.Ident name ?? "name expected after 'defun'";
       pat = parse_pattern ?? "pattern expected after 'defun'";
       'Genlex.Kwd "->" ?? "'->' expected after 'defun'";
       expr = parse ?? "missing expression after 'defun'" >]
    -> AST.Def ((Name.of_string name), AST.Fun (pat, expr))
  | [< 'Genlex.Kwd "try";
       expr = parse ?? "missing expression after 'try'";
       'Genlex.Kwd "catch" ?? "'try' requires at least one 'catch'";
       catches = parse_catch [] >]
    -> AST.Try (expr, (List.rev catches))

and parse_if = parser
  | [< ifcond = parse;
       'Genlex.Kwd "then" ?? "'then' expected after 'if'";
       ifexpr = parse ?? "missing expression after 'then'";
       clauses = parse_if_else (ifexpr :: ifcond :: []) >]
    -> clauses
and parse_if_else clauses = parser
  | [< 'Genlex.Kwd "elif";
       elifcond = parse ?? "missing expression after 'elif'";
       'Genlex.Kwd "then" ?? "'then' expected after 'elif'";
       elifexpr = parse ?? "missing expression after 'then'";
       rest = parse_if_else (elifexpr :: elifcond :: clauses) >]
    -> rest
  | [< 'Genlex.Kwd "else";
       elseexpr = parse ?? "missing expression after 'else'" >]
    -> elseexpr :: clauses
  | [< >] -> clauses

and parse_list list = parser
  | [< 'Genlex.Kwd ")" >] -> list
  | [< expr = parse;
       next = parse_list_next (expr :: list) ?? "unexpected end of list" >]
    -> next
and parse_list_next list = parser
  | [< 'Genlex.Kwd ")" >] -> list
  | [< 'Genlex.Kwd ","; rest = parse_list list ?? "unexpected end of list" >]
    -> rest

and parse_record items = parser
  | [< 'Genlex.Kwd "}" >] -> items
  | [< 'Genlex.Ident field;
       'Genlex.Kwd ":" ?? "missing ':' in record";
       expr = parse ?? "missing expression in record";
       next = parse_record_next ((Name.of_string field, expr) :: items)
        ?? "unexpected end of record" >]
    -> next
and parse_record_next items = parser
  | [< 'Genlex.Kwd "}" >] -> items
  | [< 'Genlex.Kwd ",";
       rest = parse_record items ?? "unexpected end of record" >]
    -> rest

and parse_do do_list = parser
  | [< 'Genlex.Kwd "]" >] -> do_list
  | [< expr = parse;
       next = parse_do_next (expr :: do_list) ?? "unexpected end of list" >]
    -> next
and parse_do_next do_list = parser
  | [< 'Genlex.Kwd "]" >] -> do_list
  | [< 'Genlex.Kwd ";";
       rest = parse_do do_list ?? "unexpected end of list" >]
    -> rest

and parse_mutator = parser
  | [< 'Genlex.Kwd "[";
       do_list = parse_do [] >] -> (AST.Do do_list)
  | [< 'Genlex.Ident name >] -> (AST.Name (Name.of_string name))

and parse_pattern = parser
  | [< 'Genlex.Kwd "[";
       'Genlex.Kwd "]" ?? "empty pattern expected" >]
      -> AST.EmptyPat
  | [< 'Genlex.Kwd "(";
       list = parse_list_pattern [] ?? "unexpected end of list pattern" >]
      -> AST.ListPat (List.rev list)
  | [< 'Genlex.Kwd "{";
       list = parse_record_pattern [] ?? "unexpected end of record pattern" >]
      -> AST.RecordPat (List.rev list)
  | [< 'Genlex.Ident name >] -> AST.ScalarPat (Name.of_string name)
and parse_list_pattern list = parser
  | [< 'Genlex.Kwd ")" >] -> list
  | [< 'Genlex.Ident name;
       next = parse_list_pattern_next ((Name.of_string name) :: list)
        ?? "unexpected end of list pattern" >]
    -> next
and parse_list_pattern_next list = parser
  | [< 'Genlex.Kwd ")" >] -> list
  | [< 'Genlex.Kwd ","; rest = parse_list_pattern list
        ?? "unexpected end of list pattern" >]
    -> rest
and parse_record_pattern list = parser
  | [< 'Genlex.Kwd "}" >] -> list
  | [< 'Genlex.Ident name;
       next = parse_record_pattern_next ((Name.of_string name) :: list)
        ?? "unexpected end of record pattern" >]
    -> next
and parse_record_pattern_next list = parser
  | [< 'Genlex.Kwd "}" >] -> list
  | [< 'Genlex.Kwd ",";
       rest = parse_record_pattern list ?? "unexpected end of record pattern" >]
    -> rest

and parse_catch catches = parser
  | [< 'Genlex.Ident name;
       'Genlex.Kwd "->" ?? "'-> expected after 'catch'";
       expr = parse ?? "missing expression after 'catch'";
       more = parse_catch_more ((AST.ValCatch (Name.of_string name, expr)) :: catches) >]
    -> more
  | [< pat = parse_pattern;
       'Genlex.Kwd "->" ?? "'-> expected after 'catch'";
       expr = parse ?? "missing expression after 'catch'";
       more = parse_catch_more ((AST.PatCatch (pat, expr)) :: catches) >]
    -> more
and parse_catch_more catches = parser
  | [< 'Genlex.Kwd "catch"; more = parse_catch catches >] -> more
  | [< >] -> catches

and parse_stream = parser
  | [< ast = parse; 'Genlex.Kwd ";"; strm >] -> [< 'ast; parse_stream strm >]
  | [< _ = Stream.empty >] -> [< >]
