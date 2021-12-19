%% %% Test Case 1
%% %% Beliefs
%% time(morning).
%% day(friday).
%% %% Plans
%% +!test1 :: true <-
%% 	!message(friday);
%% 	!testAnotPlan#[source(agent13)];
%% 	?is_limit(100)#[source(agent21)].
%% +!message(D) :: time(morning) & day(D) <-
%% 	write('test 3 -ok. (Checking Plan Conditions)');nl.
%% +!testAnotPlan#[source(agent13)] :: true <-
%% 	write("test 24 -ok. (Annotated Plan asked to be achieved)");nl.
%% +?is_limit(L)#[source(agent21)] :: L>10 <-
%% 	write("test 32 -ok. (Solving a test goal, by executing an annotated test plan) "),nl.

%%%%%%%%%%%%%%%%%%%%%%%%

%% ?- load_agent('tester.pl').
%% true.
%% ?- test 3 -ok. (Checking Plan Conditions)
%% test 24 -ok. (Annotated Plan asked to be achieved)
%% test 32 -ok. (Solving a test goal, by executing an annotated test plan)



:- dynamic is_limit/1.

belief(time(morning),[source(self)]).
belief(day(friday),[source(self)]).

time(morning)#[source(self)].
day(friday)#[source(self)].

plan(+!test1,true,!message(friday);!testAnotPlan#[source(agent13)];?is_limit(100)#[source(agent21)]).
plan(+!message(D),time(morning)&day(D),write("test 3 -ok. (Checking Plan Conditions)");nl).
plan(+!testAnotPlan#[source(agent13)],true,write("test 24 -ok. (Annotated Plan asked to be achieved)");nl).
plan(+?is_limit(L)#[source(agent21)],L>10,write("test 32 -ok. (Solving a test goal, by executing an annotated test plan) ");nl).
