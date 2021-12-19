%% %% Salaries’ Example
%% %% Beliefs %%
%% limit(10000).
%% salary(8000,cashier).
%% salary(12000,employee).
%% salary(18500,doctor).
%% salary(25000,broker).
%% %% Mental Rules %%
%% over_salary_limit(S) :-
%% 	limit(L) & L=<S.
%% highest_salary(Salary,Description) :-
%% 	salary(Salary,Description) &
%% 	not ( (salary(S2,_) & S2 > Salary) ) .
%% %% Plans %%
%% % The Mental rule is inside plan's context %
%% +!is_good_job(S,P) :: over_salary_limit(S) <-
%% 	write(['Job ', P , 'provides a good salary,
%% 	offering ', S , 'per year.']),nl.
%% +!is_good_job(S,P) :: true <-
%% 	write('Not accepted job. Too low wages.'),nl.
%% % The Mental rule's getting triggered from a test goal
%% +!find_best_job :: true <-
%% 	?highest_salary(S,P);
%% 	write(['Job ', P , 'provides the best salary,
%% 	offering ', S , 'per year.']),nl.

belief(limit(10000),[source(self)]).
belief(salary(8000,cashier),[source(self)]).
belief(salary(12000,employee),[source(self)]).
belief(salary(18500,doctor),[source(self)]).
belief(salary(25000,broker),[source(self)]).

limit(10000).%#[source(self)].
salary(8000,cashier).%#[source(self)].
salary(12000,employee).%#[source(self)].
salary(18500,doctor).%#[source(self)].
salary(25000,broker).%#[source(self)].

rule(over_salary_limit(S),limit(L) & L=<S).
rule(highest_salary(Salary,Description),salary(Salary,Description) & not ( ( salary(S2,_) & S2 > Salary ) )).

plan(+!is_good_job(S,P), over_salary_limit(S), (write(['Job ', P , 'provides a good salary, offering ', S , 'per year.']),nl)).
plan(+!is_good_job(_,_), true, (write('Not accepted job. Too low wages.'),nl)).
plan(+!find_best_job, true, ?highest_salary(S,P); (write(['Job ', P , 'provides the best salary, offering ', S , 'per year.']),nl)).