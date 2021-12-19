%% FIXME: replace this with setof

% %% the number_of_clauses/1 will avoid an error
rules_and_facts_for_m(M,IntoVar) :-
	findall(
		P :- B,
		(
		 current_predicate(_,M:P),
		 \+ predicate_property(M:P,imported_from(_)),
		 predicate_property(M:P, number_of_clauses(_)),
		 clause(M:P,B)
		),
		IntoVar
	       ).
