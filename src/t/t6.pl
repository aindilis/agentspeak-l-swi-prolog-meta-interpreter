%% % Plans
%% +!testFailureHandlingMechanism :: true <-
%% 	!failureHandlingAchievePlan_NA;
%% 	!failureHandlingAchievePlan_NR;
%% 	!failureHandlingAchievePlan_.
%% +!failureHandlingAchievePlan_NA :: 1>2 <-
%% 	%this is expected to fail, the message should not appear
%% 	write("test !! -ok. (Solving an achieve plan - )"),nl.
%% -!failureHandlingAchievePlan_NA#[error(non_applicable_plan)] ::
%% true <-
%% 	write("test FH_A1 ok. Annotated failure handling plan for
%% 	error(non_applicable_plan)"),nl.
%% -!failureHandlingAchievePlan_NR#[error(no_relvevant_plan)] ::
%% true <-
%% 	write("test FH_A2 ok. Annotated failure handling plan for
%% 	error(no_relvevant_plan)"),nl.
%% -!failureHandlingAchievePlan_ :: true <-
%% 	write("test FH_A3 ok. Non-annotated failure handling plan"),nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ?- start_agent.
%% true.
%% ?- load_agent('failurehandling.pl').
%% true.
%% ?- test FH_A1 ok. Annotated failure handling plan for
%%	error(non_applicable_plan)

plan(+!testFailureHandlingMechanism,true,!failureHandlingAchievePlan_NA;!failureHandlingAchievePlan_NR;!failureHandlingAchievePlan_).
plan(+!failureHandlingAchievePlan_NA,1>2,
     (	 write("test !! -ok. (Solving an achieve plan - )"),nl)).
plan(-!failureHandlingAchievePlan_NA#[error(non_applicable_plan)],true,
     (	 write("test FH_A1 -ok. Annotated failure handling plan for error(non_applicable_plan)"),nl)).
plan(-!failureHandlingAchievePlan_NR#[error(no_relevant_plan)],true,
     (	 write("test FH_A2 -ok. Annotated failure handling plan for error(no_relevant_plan)"),nl)).
plan(-!failureHandlingAchievePlan_#[error(no_relevant_plan)],true,
     (	 write("test FH_A3 -ok. Non-annotated failure handling plan"),nl)).
