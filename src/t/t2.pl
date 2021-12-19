%% %% Test Case 2
%% %% Beliefs
%% salary(10000,captain).
%% salary(15000,major).
%% salary(18000,colonel).
%% %% Rules
%% lowest_salary(Salary,Person) :-
%% 	salary(Salary,Person) &
%% 	not ( (salary(S2,_) & S2 < Salary) ) .
%% %% Plans
%% +!evaluate_salary :: true <-
%% 	?lowest_salary(S,P);
%% 	S=10000;
%% 	write("test 28 ok. (Solving a test goal following a
%% 	mental rule) "),nl.

%%%%%%%%%%%%%%%%%%%%%%

%% ?- load_agent('tester.pl').
%% true.
%% ?- test 28 ok. (Solving a test goal following a mental rule) 

%% Test Case 2
%% Beliefs
belief(salary(10000,captain),[source(self)]).
belief(salary(15000,major),[source(self)]).
belief(salary(13000,colonel),[source(self)]).

salary(10000,captain). % #[source(self)].
salary(15000,major). % #[source(self)].
salary(13000,colonel). % #[source(self)].

%% Rules
rule(lowest_salary(Salary,Person),salary(Salary,Person) & not ( (salary(S2,_) & S2 < Salary) )).

%% Plans
plan(+!evaluate_salary,true,?lowest_salary(S,_P);S=10000;(write("test 28 -ok. (Solving a test goal following a mental rule) "),nl)).
