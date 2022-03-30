:- object(repl_hijack).

	:- info([
		version is 1:0:0,
		author is 'Paul Brown',
		date is 2021-10-01,
		comment is 'REPL Hijack to respond with a single line of JSON'
	]).

	:- uses(navltree, [as_curly_bracketed/2]).
	:- uses(avltree,  [as_dictionary/2]).
	:- uses(term_io,  [write_term_to_atom/3]).
	:- uses(json,     [generate/2]).

	:- dynamic(current_query/2).
	:- public(set_query/2).
    :- mode(set_query(+term, +list), one).
    :- info(set_query/2, [
	    comment is 'Set the query under consideration',
		argnames is ['Query', 'VariableNames']
		]).
	set_query(Query, VariableNames) :-
		retractall(current_query(_, _)),
		assertz(current_query(Query, VariableNames)).
		

	:- public(go/0).
	:- info(go/0, [
		comment is 'Hijack the REPL'
	]).
	go :-
		% Tag so Client knows it can send queries
		write(user_output, 'LOGTALK READY\n'),
		% Flush required for ECLiPSe
		flush_output(user_output),
		repeat,
			catch(nondet_try_read_query, error(Error, _), error_response(Error)),
		fail,
		% Silence linter warning
		!.

	:- private(nondet_try_read_query/0).
	:- info(nondet_try_read_query/0, [
		comment is 'Read a query from input and try it, giving the response'
	]).
	nondet_try_read_query :-
		read_term(user_input, Input, [variable_names(VariableNames)]),
		set_query(Input, VariableNames),
		nondet_try_query.
	
	:- private(nondet_try_query/0).
	:- info(nondet_try_query/0, [
		comment is 'Try a query, respond, accept next input and act on it'
		]).
	nondet_try_query :-
		forall(	do_query(Response), (
		    json_response(Response),
			read_term(user_input, Next, [variable_names(NextVariableNames)]),
			once(( Next == '$next'
			; set_query(Next, NextVariableNames), nondet_try_query
			))
		)),
		fail_response.


	:- private(do_query/1).
	:- info(do_query/1, [
		comment is 'Do the current query generating a response term',
		argnames is ['Response']
		]).
	do_query(Response) :-
		current_query(Input, VariableNames),
		write_term_to_atom(Input, Query, [variable_names(VariableNames)]),
		{Input}, % execution of the query
		ground_pairs_keys(VariableNames, VariableNames, Variables, Names),
		as_dictionary(Variables, Unifications),
		as_dictionary([
			variable_names-Names,
			status-success,
			query-Query,
			unifications-Unifications
		], Response).

	:- private(error_response/1).
	:- info(error_response/1, [
		comment is 'Send an error response',
		argnames is ['Error']
		]
	).
	error_response(ErrorTerm) :-
		write_term_to_atom(ErrorTerm, Error, []),
		as_dictionary([status-error, error-Error], Dict),
		json_response(Dict).

	:- private(fail_response/0).
	:- info(fail_response/0, [
		comment is 'Send a fail response'
		]
	).
	fail_response :-
		current_query(Input, VariableNames),
		retractall(current_query(_, _)),
		write_term_to_atom(Input, Query, [variable_names(VariableNames)]),
		as_dictionary([status-fail, query-Query], Dict),
		json_response(Dict).

	:- private(json_response/1).
	:- info(json_response/1, [
		comment is 'Send a successful response',
		argnames is ['Response']
		]
	).
	json_response(Response) :-
		as_curly_bracketed(Response, JSON),
		generate(stream(user_output), JSON),
		nl(user_output),
		flush_output.

	% In one pass filter any non-ground values, convert from `=` pairs to `-`
	% pairs, and accumulate the pairs keys
	ground_pairs_keys(Pairs, VariableNames, Ground, Keys) :-
		ground_pairs_keys(Pairs, VariableNames, H-H, Ground, I-I, Keys), !.

	ground_pairs_keys([], _, Ground-[], Ground, Keys-[], Keys).
	ground_pairs_keys([K=V|Pairs], VariableNames, GAcc-GH, Ground, KAcc-KH, Keys) :-
		describe_term(V, VariableNames, Value),
		GH = [K-Value|NGH], KH = [K|NKH],
		ground_pairs_keys(Pairs, VariableNames, GAcc-NGH, Ground, KAcc-NKH, Keys).

	:- public(describe_term/3).
	:- info(describe_term/3, [
		comment is 'Turn the term into a dictionary describing it',
		argnames is ['Term', 'VariableNames', 'Description']
		]).
	describe_term(Term, VariableNames, Description) :-
		(	% Is a variable
			var(Term)
		->  as_dictionary([type-variable], Description)

			% Or is a list
		; 	list::valid(Term)  % Must be before atom test (`atom([])` is true on most Prologs)
		->	findall(Desc, (list::member(Member, Term), describe_term(Member, VariableNames, Desc)), Values),
		  	as_dictionary([type-list, value-Values], Description)

			% Or is an atom
		; 	atom(Term)
		->	as_dictionary([type-atom, value-Term], Description)

			% Or is an integer
		; 	integer(Term)
		->	as_dictionary([type-integer, value-Term], Description)

			% Or is a float
		; 	float(Term)
		->	as_dictionary([type-float, value-Term], Description)

			% Or is a compound term (`foo(bar)`, or `[1]`, which has been caught already)
		; 	compound(Term)
		->	functor(Term, Functor, Arity),
		  	Term =.. [Functor|Args],
		  	describe_term(Args, VariableNames, ArgsDesc),
		  	as_dictionary([type-compound, functor-Functor, arity-Arity, args-ArgsDesc], Description)

			% Or we're in some bother and haven't handled it
		; 	write_term_to_atom(Term, TermAtom, [variable_names(VariableNames)]),
		  	as_dictionary([type-unknown, value-TermAtom], Description)
		).

:- end_object.
