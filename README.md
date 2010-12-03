# Square - An experimental programming language in OCaml

This is a language I started writing in 2005. I used camlp4 stream parsers
to create a top-down recursive-descent parser for a language that is a sort
of hybrid of features from Scheme, ML, Python, and some ideas from Arc as
well. I haven't worked on the project in years, but thought it might be
interesting for others who are experimenting with OCaml, parsers, little
languages, etc.

Here is a simple example to demonstrate Square's syntax. It prints "3",
"2", "1", and "blastoff" on separate lines.

    def tailrec
        let {recur: fun x -> throw {Recur: x}}
        fun f -> fun arg -> [
            try
                forever fun [] -> [
                try throw {Exit: f (recur, arg)}
                catch {Recur} -> := arg Recur
            ]
            catch {Exit} ->
                Exit
            ];

    def countdown fun start ->
        let {
            aux: tailrec fun (aux', arg) ->
                if < 0 arg
                then [print arg; aux' [- arg 1]]
                else [print "blastoff!"]
        }
        aux start;

    countdown 3;
