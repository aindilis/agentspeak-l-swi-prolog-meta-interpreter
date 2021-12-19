%% /var/lib/myfrdcsa/codebases/minor/free-life-planner/projects/pfc-clone/state.pl
%% /var/lib/myfrdcsa/codebases/minor/free-life-planner/data-git/systems/planning/state-exporter.pl

:- discontiguous plan/3, rule/2, belief/1, '#'/2.
:- multifile plan/3, rule/2, belief/1, '#'/2.
:- dynamic plan/3, rule/2, belief/1, '#'/2.

compound_zero_arity(Term,Pred) :-
        compound(Term),
        compound_name_arity(Term,Pred,0).

load_agent(AgentFullFile) :-
	atom_concat(AgentFile,'.pasl',AgentFullFile),
	atomic_list_concat(List,'/',AgentFile),
	reverse(List,[AgentName|_]),
	use_module(AgentFullFile),
	AgentName:rules_and_facts_for_m(AgentName,State),
	%% print_term(State,[]),nl,
	parse_raw_state(State,Parsed),
	%% print_term(Parsed,[]),nl,
	assert_all(Parsed).

parse_raw_state(State,Parsed) :-
	findall(P,(
		   member(S,State),
		   parse_raw(S,P),
		   kosher(P)
		  ), Parsed).

parse_raw(X::Y<-Z :- true,plan(X,Y,Z)) :- !.
parse_raw(X<-Z :- true,plan(X,true,Z)) :- !.
parse_raw(Y#A :- true,Y#A) :- !.
parse_raw(Y :- true,Y) :- !.
%% parse_raw(Y :- true,Y#[source(self)]) :- !.
parse_raw(X :- Y,rule(X,Y)) :- !.

kosher(P) :-
	(   P = rule(R,_) ->
	    (	
		(   (	compound_zero_arity(R,P1),!) ; (R =.. [P1|_],!)),
		not(member(P1,[rules_and_facts_for_m]))
		%% not(member(P1,[pred_for_m,fluents_for_state,list_modules,rules_and_facts_for_m]))
	    ) ; true).

assert_all(Parsed) :-
	member(P,Parsed),
	assert(P),
	print_term([asserted,P],[]),nl,
	fail.
assert_all(_).