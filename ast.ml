open Printf
open Name

module AST = struct
  type ast =
    | Name of Name.t
    | Symbol of Name.t
    | String of string
    | Int of int
    | Float of float
    | Char of char
    | List of ast list
    | Record of ast NameMap.t
    | Do of ast list
    | Def of (Name.t * ast)
    | Set of (Name.t * ast)
    | Undef of Name.t
    | Let of (((Name.t * ast) list) * ast)
    | If of ast list
    | Fun of (pattern * ast)
    | App of (ast * ast)
    | Try of (ast * (catch list))

  and pattern =
    | EmptyPat
    | ScalarPat of Name.t
    | ListPat of Name.t list
    | RecordPat of Name.t list

  and catch =
    | ValCatch of (Name.t * ast)
    | PatCatch of (pattern * ast)

  let rec to_string = function
    | Name n -> sprintf "Name \"%s\"" (Name.to_string n)
    | Symbol n -> sprintf "Symbol \"%s\"" (Name.to_string n)
    | String s -> sprintf "String \"%s\"" s
    | Int i -> sprintf "Int %d" i
    | Float f -> sprintf "Float %f" f
    | Char c -> sprintf "Char %c" c
    | List l -> sprintf "List [%s]" (String.concat "; " (List.map to_string l))
    | Record r ->
        sprintf "Record [%s]"
          (String.concat "; "
             (NameMap.fold
                (fun k v a ->
                   (sprintf "\"%s\", %s"
                      (Name.to_string k)
                      (to_string v)) :: a) r []))
    | Do l -> sprintf "Do [%s]" (String.concat "; " (List.map to_string l))
    | Def (n, a) -> sprintf "Def (\"%s\", %s)" (Name.to_string n) (to_string a)
    | Set (n, a) -> sprintf "Set (\"%s\", %s)" (Name.to_string n) (to_string a)
    | Undef n -> sprintf "Undef \"%s\"" (Name.to_string n)
    | Let (xs, a) ->
        sprintf "Let ([%s], %s)"
          (String.concat "; "
             (List.map
                (fun (n, a) ->
                   sprintf "\"%s\", %s" (Name.to_string n) (to_string a)) xs))
          (to_string a)
    | If l -> sprintf "If [%s]" (String.concat "; " (List.map to_string l))
    | Fun (p, a) -> sprintf "Fun (%s, %s)" (pattern_to_string p) (to_string a)
    | App (f, a) -> sprintf "App (%s, %s)" (to_string f) (to_string a)
    | Try (a, l) ->
        sprintf "Try (%s, [%s])"
          (to_string a) (String.concat "; " (List.map catch_to_string l))

  and pattern_to_string = function
    | EmptyPat -> "EmptyPat"
    | ScalarPat n -> sprintf "ScalarPat \"%s\"" (Name.to_string n)
    | ListPat l ->
        sprintf "ListPat [%s]"
          (String.concat "; "
             (List.map (fun n -> sprintf "\"%s\"" (Name.to_string n)) l))
    | RecordPat r ->
        sprintf "RecordPat [%s]"
          (String.concat "; "
             (List.map (fun n -> sprintf "\"%s\"" (Name.to_string n)) r))

  and catch_to_string = function
    | ValCatch (n, a) ->
        sprintf "ValCatch (\"%s\", %s)" (Name.to_string n) (to_string a)
    | PatCatch (p, a) ->
        sprintf "PatCatch (%s, %s)" (pattern_to_string p) (to_string a)

  let rec to_xml = function
    | Name n -> sprintf "<Name>%s</Name>" (Name.to_string n)
    | Symbol n -> sprintf "<Symbol>%s</Symbol>" (Name.to_string n)
    | String s -> sprintf "<String>%s</String>" s
    | Int i -> sprintf "<Int>%d</Int>" i
    | Float f -> sprintf "<Float>%f</Float>" f
    | Char c -> sprintf "<Char>%c</Char>" c
    | List l ->
        sprintf "<List>%s</List>" (String.concat " " (List.map to_xml l))
    | Record r ->
        sprintf "<Record>%s</Record>"
          (String.concat " "
             (NameMap.fold
                (fun k v a ->
                   (sprintf "%s %s"
                      (Name.to_string k)
                      (to_xml v)) :: a) r []))
    | Do l -> sprintf "<Do>%s</Do>" (String.concat " " (List.map to_xml l))
    | Def (n, a) -> sprintf "<Def>%s %s</Def>" (Name.to_string n) (to_xml a)
    | Set (n, a) -> sprintf "<Set>%s %s</Set>" (Name.to_string n) (to_xml a)
    | Undef n -> sprintf "<Undef>%s</Undef>" (Name.to_string n)
    | Let (xs, a) ->
        sprintf "<Let>%s %s</Let>"
          (String.concat " "
             (List.map
                (fun (n, a) ->
                   sprintf "%s %s" (Name.to_string n) (to_xml a)) xs))
          (to_xml a)
    | If l -> sprintf "<If>%s</If>" (String.concat " " (List.map to_xml l))
    | Fun (p, a) -> sprintf "<Fun>%s %s</Fun>" (pattern_to_xml p) (to_xml a)
    | App (f, a) -> sprintf "<App>%s %s</App>" (to_xml f) (to_xml a)
    | Try (a, l) ->
        sprintf "<Try>%s %s</Try>"
          (to_xml a) (String.concat " " (List.map catch_to_xml l))

  and pattern_to_xml = function
    | EmptyPat -> "<EmptyPat />"
    | ScalarPat n -> sprintf "<ScalarPat>%s</ScalarPat>" (Name.to_string n)
    | ListPat l ->
        sprintf "<ListPat>%s</ListPat>"
          (String.concat " "
             (List.map (fun n -> sprintf "%s" (Name.to_string n)) l))
    | RecordPat r ->
        sprintf "<RecordPat>%s</RecordPat>"
          (String.concat " "
             (List.map (fun n -> sprintf "%s" (Name.to_string n)) r))

  and catch_to_xml = function
    | ValCatch (n, a) ->
        sprintf "<ValCatch>%s %s</ValCatch>" (Name.to_string n) (to_xml a)
    | PatCatch (p, a) ->
        sprintf "<PatCatch>%s %s</PatCatch>" (pattern_to_xml p) (to_xml a)

  let record items =
    Record (List.fold_left
              (fun a (k, v) -> NameMap.add k v a)
              NameMap.empty items)
end
