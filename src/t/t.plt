:- begin_tests(lists).
:- use_module(library(lists)).

:- consult('../agentspeak-l.pl').

:- start_agent.

%%%%%%%%%%%%%%%%%%%%%%

test(test_case_1) :-
	ensure_loaded(t1),
	with_output_to(atom(R),solve(!test1)),
	write(R),nl,
	R = 'test 3 -ok. (Checking Plan Conditions)
test 24 -ok. (Annotated Plan asked to be achieved)
test 32 -ok. (Solving a test goal, by executing an annotated test plan) 
',
	clean_base.

%%%%%%%%%%%%%%%%%%%%%%

test(test_case_2) :-
	ensure_loaded(t2),
	with_output_to(atom(R),solve(!evaluate_salary)),
	write(R),nl,
	R = 'test 28 -ok. (Solving a test goal following a mental rule) 
',
	clean_base.

%%%%%%%%%%%%%%%%%%%%%%

test(test_case_3) :-
	ensure_loaded(t3),
	write(R),nl,
	with_output_to(atom(R),solve(!test3)),
	R = '',
	clean_base.

%%%%%%%%%%%%%%%%%%%%%

test(test_case_4) :-
	ensure_loaded(t4),
	with_output_to(atom(R),(solve(!test4),wait(100))),
	write(R),nl,
	R = 'test 16 -ok. (Add directly an annotated belief) 
test 19 -ok. (Remove directly an annotated belief) 
',
	clean_base.

%% %%%%%%%%%%%%%%%%%%%%%%

test(test_case_5) :-
	ensure_loaded(t5),
	with_output_to(atom(R),solve(!test5)),
	write(R),nl,
	R = 'test 21 -ok. (Plan triggered by an annotated belief addition)
test 23 -ok. (Plan triggered by an annotated belief deletion)
',
	clean_base.

%% %%%%%%%%%%%%%%%%%%%%%%

test(test_case_6) :-
	ensure_loaded(t6),
	with_output_to(atom(R),solve(!testFailureHandlingMechanism)),
	write(R),nl,
	R = 'test FH_A1 -ok. Annotated failure handling plan for error(non_applicable_plan)
test FH_A2 -ok. Annotated failure handling plan for error(no_relevant_plan)
test FH_A3 -ok. Non-annotated failure handling plan
',
	clean_base.

%% %%%%%%%%%%%%%%%%%%%%%%

test(test_case_7) :-
	ensure_loaded(t7),
	with_output_to(atom(R),(solve(!is_good_job(9000,cashier)),solve(!is_good_job(20000,doctor)),solve(!find_best_job))),
	write(R),nl,
	R = 'Not accepted job. Too low wages.
[Job ,doctor,provides a good salary, offering ,20000,per year.]
[Job ,broker,provides the best salary, offering ,25000,per year.]
',
	clean_base.

%% %%%%%%%%%%%%%%%%%%%%%%

test(test_case_20) :-
	ensure_loaded(t20),
	with_output_to(atom(R),solve(!isolate)),
	write(R),nl,
	R = 'Cart Cleaned!
Room Cleaned!
Pantry Stocked!
Locked Door!
',
	clean_base.

%% %%%%%%%%%%%%%%%%%%%%%%

test(test_case_22) :-
	ensure_loaded('../parser.pl'),
	load_agent('t22.pl'),
	with_output_to(atom(R),solve(!isolate)),
	write(R),nl,
	R = '',
	clean_base.

%% %%%%%%%%%%%%%%%%%%%%%%

test(test_case_23) :-
	ensure_loaded('../parser.pl'),
	load_agent('t23.pl'),
	with_output_to(atom(R),solve(!evaluate_salary)),
	write(R),nl,
	R = '',
	clean_base.

%% %%%%%%%%%%%%%%%%%%%%%%

test(test_case_24) :-
	ensure_loaded('../parser.pl'),
	load_agent('t24.pl'),
	with_output_to(atom(R),solve(!test)),
	write(R),nl,
	R = '',
	clean_base.

%% %%%%%%%%%%%%%%%%%%%%%%

:- end_tests(lists).
