:- consult('agentspeak-l.pl').

:- start_agent.

t1 :-
	ensure_loaded('t/t1'),
	l,
	solve(!test1).

t2 :-
	ensure_loaded('t/t2'),
	l,
	solve(!evaluate_salary).

t3 :-
	ensure_loaded('t/t3'),
	l,
	solve(!test3).

t4 :-
	ensure_loaded('t/t4'),
	l,
	solve(!test4).

t5 :-
	ensure_loaded('t/t5'),
	l,
	solve(!test5).

t6 :-
	ensure_loaded('t/t6'),
	l,
	solve(!testFailureHandlingMechanism).

t7 :-
	ensure_loaded('t/t7'),
	l,
	solve(!is_good_job(10000,cashier)),
	solve(!is_good_job(20000,doctor)),
	solve(!find_best_job).



t20 :-
	ensure_loaded('t/t20'),
	l,
	solve(!isolate).
