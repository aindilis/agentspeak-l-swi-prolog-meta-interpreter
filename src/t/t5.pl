%% %% Test Case 5
%% %% Beliefs
%% bad(mood)#[source(agentB)].
%% %% Plans
%% +!test :: true <-
%% 	+good(day)#[source(agentA)];
%% 	-bad(mood)#[source(agentB)].
%% +good(day)#[source(agentA)] :: true <-
%% 	write('test 21 -ok. (Plan triggered by an annotated
%% 	belief addition)');nl.
%% -bad(mood)#[source(agentB)] :: true <-
%% 	write('test 23 -ok. (Plan triggered by an annotated
%% 	belief deletion)');nl.

%%%%%%%%%%%%%%%%%%%%%%

%% load_agent('tester.pl').
%% true.
%% ?- test 21 -ok. (Plan triggered by an annotated belief
%% 		addition)
%% test 23 -ok.  (Plan triggered by an annotated belief
%% deletion)

belief(bad(mood),[source(agentB)]).

bad(mood)#[source(agentB)].

plan(+!test5,true,+good(day)#[source(agentA)];-bad(mood)#[source(agentB)]).
plan(+good(day)#[source(agentA)],true,write('test 21 -ok. (Plan triggered by an annotated belief addition)');nl).
plan(-bad(mood)#[source(agentB)],true,write('test 23 -ok. (Plan triggered by an annotated belief deletion)');nl).
