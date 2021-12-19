%% %% Test Case 3
%% %% Beliefs
%% %% Plans
%% +!test :: true <-
%% 	 ^^separateIntention.
%% +!separateIntention :: true <-
%% 	write("test 33 ok. (Goal pursued as a separate
%% 	intention)"),nl.

%%%%%%%%%%%%%%%%%%%%%%

%% ?- load_agent('tester.pl').
%% true.
%% ?- test 33 ok. (Goal pursued as a separate intention)

%% Rules
rule(lowest_salary(Salary,Person),salary(Salary,Person) & not ( (salary(S2,_) & S2 < Salary) )).

%% Plans
plan(+!test3,true,^^separateIntention).
plan(+!separateIntention,true,(write("test 33 -ok. (Goal pursued as a separate intention)"),nl)).
