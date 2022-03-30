# JSON REPL

This is a tool designed to aim communication between Prolog with Logtalk and
other languages by responding to queries in an ubiquitous text-format that
other languages can parse.

Hijacks the REPL to read Prolog queries and respond with a single line JSON
response:

```prolog
?- repl_hijack::go.
LOGTALK READY
|: X = foo.
{"query":"X=foo","status":"success","unifications":{"X":{"type":"atom","value":"foo"}},"variable_names":["X"]}
|: list::member(X, [1, 2]).
{"query":"list::member(X,[1,2])","status":"success","unifications":{"X":{"type":"integer","value":1}},"variable_names":["X"]}
|: '$next'.
{"query":"list::member(X,[1,2])","status":"success","unifications":{"X":{"type":"integer","value":2}},"variable_names":["X"]}
|: '$next'.
{"query":"list::member(X,[1,2])","status":"fail"}
|: oops I = \ not valid.
{"error":"syntax_error(operator_expected)","status":"error"}
|: not_a_predicate(P).
{"error":"existence_error(procedure,not_a_predicate\/1)","status":"error"}
|: halt.
```

The command for `;` "next answer" is `'$next'.`
