%% %% Test Case 4
%% %% Beliefs
%% obstacle(back)#[source(agent1)].
%% obstacle(back)#[source(agent99)].
%% %% Plans
%% +!test :: true <-
%%	+obstacle(back)#[source(agent100)];
%% 	write("test 16 -ok. (Add directly an annotated
%%         belief)");nl;
%% 	-obstacle(back)#[source(agent99)];
%% 	write("test 19 -ok. (Remove directly an annotated
%% 	belief)");nl.

%%%%%%%%%%%%%%%%%%%%%%

%% load_agent('tester.pl').
%% true.
%% ?- test 16 -ok. (Add directly an annotated belief)
%% test 19 -ok. (Remove directly an annotated belief)




belief(obstacle(back),[source(agent1)]).
belief(obstacle(back),[source(agent99)]).

obstacle(back)#[source(agent1)].
obstacle(back)#[source(agent99)].

plan(+!test4,true,
     +obstacle(back)#[source(agent100)];
     write("test 16 -ok. (Add directly an annotated belief) ");
     nl;
     -obstacle(back)#[source(agent99)];
     write("test 19 -ok. (Remove directly an annotated belief) ");
     nl).
