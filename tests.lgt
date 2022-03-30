:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paul Brown',
		date is 2022-03-30,
		comment is 'Unit tests for repl_hijack'
	]).

	cover(repl_hijack).

	test(do_query_success_status, true(Status == success)) :-
		repl_hijack<<set_query(X = foo, ['X'=X]),
		repl_hijack<<do_query(Response),
		avltree::lookup(status, Status, Response).
	test(do_query_fail, fail) :-
		repl_hijack<<set_query((X = foo, X = bar), ['X'=X]),
		repl_hijack<<do_query(_Response).
	% predicate undefined error
	test(do_query_error,
	     error(existence_error(procedure, foo_bar_baz/1))) :-
		repl_hijack<<set_query(foo_bar_baz(X), ['X'=X]),
		repl_hijack<<do_query(_Response).

	test(do_query_contains_query, true(Query == 'X=foo')) :-
		repl_hijack<<set_query(X = foo, ['X'=X]),
		repl_hijack<<do_query(Response),
		avltree::lookup(query, Query, Response).

	test(do_query_contains_unification_value, true(Foo == foo)) :-
		repl_hijack<<set_query(X = foo, ['X'=X]),
		repl_hijack<<do_query(Response),
		avltree::(lookup(unifications, Unifications, Response),
		    lookup('X', UX, Unifications),
		    lookup(value, Foo, UX)).
	test(do_query_contains_unification_type_atom, true(Type == atom)) :-
		repl_hijack<<set_query(X = foo, ['X'=X]),
		repl_hijack<<do_query(Response),
		avltree::(lookup(unifications, Unifications, Response),
		    lookup('X', UX, Unifications),
		    lookup(type, Type, UX)).
	test(do_query_contains_unification_type_var, true(Type == variable)) :-
		repl_hijack<<set_query(X = Var, ['X'=X, 'Var'=Var]),
		repl_hijack<<do_query(Response),
		avltree::(lookup(unifications, Unifications, Response),
		    lookup('X', UX, Unifications),
		    lookup(type, Type, UX)).
	test(do_query_contains_unification_type_integer, true(Type == integer)) :-
		repl_hijack<<set_query(X = 1, ['X'=X]),
		repl_hijack<<do_query(Response),
		avltree::(lookup(unifications, Unifications, Response),
		    lookup('X', UX, Unifications),
		    lookup(type, Type, UX)).
	test(do_query_contains_unification_type_float, true(Type == float)) :-
		repl_hijack<<set_query(X = 1.1, ['X'=X]),
		repl_hijack<<do_query(Response),
		avltree::(lookup(unifications, Unifications, Response),
		    lookup('X', UX, Unifications),
		    lookup(type, Type, UX)).
	test(do_query_contains_unification_type_compound, true(Type == compound)) :-
		repl_hijack<<set_query(X = foo(bar), ['X'=X]),
		repl_hijack<<do_query(Response),
		avltree::(lookup(unifications, Unifications, Response),
		    lookup('X', UX, Unifications),
		    lookup(type, Type, UX)).
	test(do_query_contains_unification_type_compound_pair, true(Type == compound)) :-
		repl_hijack<<set_query(X = foo-bar, ['X'=X]),
		repl_hijack<<do_query(Response),
		avltree::(lookup(unifications, Unifications, Response),
		    lookup('X', UX, Unifications),
		    lookup(type, Type, UX)).
	test(do_query_contains_unification_type_compound_colon, true(Type == compound)) :-
		repl_hijack<<set_query(X = foo:bar, ['X'=X]),
		repl_hijack<<do_query(Response),
		avltree::(lookup(unifications, Unifications, Response),
		    lookup('X', UX, Unifications),
		    lookup(type, Type, UX)).
	test(do_query_contains_unification_type_compound_curly, true(Type == compound)) :-
		repl_hijack<<set_query(X = {foo,bar}, ['X'=X]),
		repl_hijack<<do_query(Response),
		avltree::(lookup(unifications, Unifications, Response),
		    lookup('X', UX, Unifications),
		    lookup(type, Type, UX)).

	test(do_query_contains_unification_type_list, true(Type == list)) :-
		repl_hijack<<set_query(X = [], ['X'=X]),
		repl_hijack<<do_query(Response),
		avltree::(lookup(unifications, Unifications, Response),
		    lookup('X', UX, Unifications),
		    lookup(type, Type, UX)).

	test(do_query_contains_unification_list_values_empty, true(Value == [])) :-
		repl_hijack<<set_query(X = [], ['X'=X]),
		repl_hijack<<do_query(Response),
		avltree::(lookup(unifications, Unifications, Response),
		    lookup('X', UX, Unifications),
		    lookup(value, Value, UX)).

	test(do_query_contains_unification_list_values_one, true(Value == [Atom])) :-
		repl_hijack<<set_query(X = [foo], ['X'=X]),
		repl_hijack<<do_query(Response),
		avltree::(lookup(unifications, Unifications, Response),
		    lookup('X', UX, Unifications),
		    lookup(value, Value, UX)),
	    % Make test case
		repl_hijack<<set_query(Y = foo, ['Y'=Y]),
		repl_hijack<<do_query(TestResponse),
		avltree::(lookup(unifications, TestUnifications, TestResponse),
		    lookup('Y', Atom, TestUnifications)).

	test(do_query_contains_unification_compound_args, true(Args == List)) :-
		repl_hijack<<set_query(X = bar(foo), ['X'=X]),
		repl_hijack<<do_query(Response),
		avltree::(lookup(unifications, Unifications, Response),
		    lookup('X', UX, Unifications),
		    lookup(args, Args, UX)),
	    % Make test case
		repl_hijack<<set_query(Y = [foo], ['Y'=Y]),
		repl_hijack<<do_query(TestResponse),
		avltree::(lookup(unifications, TestUnifications, TestResponse),
		    lookup('Y', List, TestUnifications)).

	test(do_query_contains_unification_compound_functor, true(Functor == bar)) :-
		repl_hijack<<set_query(X = bar(foo), ['X'=X]),
		repl_hijack<<do_query(Response),
		avltree::(lookup(unifications, Unifications, Response),
		    lookup('X', UX, Unifications),
		    lookup(functor, Functor, UX)).
	test(do_query_contains_unification_compound_arity, true(Arity == 1)) :-
		repl_hijack<<set_query(X = bar(foo), ['X'=X]),
		repl_hijack<<do_query(Response),
		avltree::(lookup(unifications, Unifications, Response),
		    lookup('X', UX, Unifications),
		    lookup(arity, Arity, UX)).

:- end_object.
