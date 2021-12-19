%% Most of this code in the file (save for some small additions and
%% tweaks) was extracted from: Dimitriadis Dimitrios' MSc Thesis here:

%% https://dspace.lib.uom.gr/bitstream/2159/24611/1/DimitriadisDimitriosMSc2019.pdf

:- discontiguous solve/1.
:- discontiguous find_plan/3.
:- discontiguous add_belief/2.
:- discontiguous process_message/1.
:- discontiguous fix_message_for_main/3.

:- multifile solve/1.
:- multifile find_plan/3.
:- multifile add_belief/2.
:- multifile process_message/1.
:- multifile fix_message_for_main/3.

:- multifile rule/2.
:- multifile belief/2.
:- multifile plan/3.
:- multifile '#'/2.

:- discontiguous rule/2.
:- discontiguous belief/2.
:- discontiguous plan/3.
:- discontiguous '#'/2.

:- dynamic rule/2.
:- dynamic belief/2.
:- dynamic plan/3.
:- dynamic '#'/2.

%% Precedence Type Name Functionality
:- op(1190, xfx, <-).      %% as in AgentSpeak(L)
:- op(1150, xfx, ::).      %% as ‘:’ in AgentSpeak(L)
:- op(1050, xfy, &).       %% Conjunction Operator
:- op(900, fy, not).      %% Negation, Strong Negation
:- op(900, fy, ~).      %% Negation, Strong Negation
:- op(200, fx, !). %% as in AgentSpeak(L)
:- op(200, fx, ?). %% as in AgentSpeak(L)
:- op(200, fx, +?). %% as in AgentSpeak(L)
:- op(200, fx, -?). %% as in AgentSpeak(L)
:- op(220, fx, '+!'). %% as in AgentSpeak(L)
:- op(220, fx, '-!'). %% as in AgentSpeak(L)
:- op(200, fx, ^^).        %% as ‘!g’ but with ‘^^g’ the goal is pursued as a separate intention
:- op(195, fx, '.').       %% as in AgentSpeak(L)
:- op(190, xfx, #).        %% Annotations
:- op(190, xfx, @).        %% 
:- op(190, fx, @).        %% 
:- op(210, fx, +). %% as in AgentSpeak(L)
:- op(210, fx, -). %% as in AgentSpeak(L)

solve(true).
solve(fail):- !,fail.
% Sequencing actions in plan body
solve(P1;P2):-
	!,nb_setval(remaining,P2),
	solve(P1), !, solve(P2).
% Solving an achieve goal
solve(!E):-
	!,
	find_plan(+!E,_,P),
	%% commit to an applicable achieve plan
	!,
	solve(P).
% Solving a test goal
solve(?E):-
	test_condition(E).
% Solving a test goal (with test plan)
solve(?E):-
	!,
	find_plan(+?E,_,P),
	%% commit to an applicable test plan
	!,
	solve(P).
% Solving a goal pursued as a separate intention
solve(^^E):-
	generate_agentspeak_event(achieve(!E)).

%%%%%%% Adding, Removing & Updating Beliefs %%%%%%%%
solve(+B#A):-
	!,add_belief(B,A).
solve(+B):-
	!,add_belief(B,[source(self)]).
solve(-B#A):-
	!,remove_belief(B,A).
solve(-B):-
	!,remove_belief(B,[source(self)]).
	      %%%% Plans Triggered from belief addition/deletion %%%%
solve(addb(B,A)):-
	find_plan(+B#A,_,P),
	% commit to an applicable triggered plan
	!,solve(P).
%% In case there is no plan for belief addition.
solve(addb(_,_)):-!.
solve(delb(B,A)):-
	find_plan(-B#A,_,P),
% commit to an applicable triggered plan
	!,solve(P).
%% In case there is no plan for belief deletion.
solve(delb(_,_)):-!.
solve(.E) :- E.
solve(E):- E.

%% block(3234,5471).
%% quantity(wood,23).
%% ~like(john,movies).
%% book_price(prolog,120).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% +!sell_Flowers : flowers(Q) & price(P) & not flowers(0)
%% 	<-
%% 		broadcast(achieve, propose(Q,P));
%% 		wait(1500);
%% 		!eval_proposals.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @ask_to_open_door
%% +!open_door : true <-
%% 	.send(bob, achieve, open(door));
%% 	.wait(1000);
%% 	.send(bob, askOne, window(Status), Reply);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% +!list(books) : true <-
%% 	.findall(Book,price_book(Book,_), Books);
%% 	.print(Books).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% [sender, illocutionary_force, content]
%% .send(bob,achieve, close(window));
%% .send(alice,tell, nice(weather));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% book(prolog).
%% ~book(erlang).
%% object(front)#[source(agent1)].
%% object(front)#[source(agent2)].
%% car(volvo)#[source(ag1),color(white)].
%% (   ~like(cookies))#[source(nick)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% (~like(cookies))#[source(nick)].
%% ~like(cookies)#[source(nick)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

findMaxTemp(Temp):-
	temp(Temp) &
	not ( (temp(Y) & Y > Temp) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

findAllTemp(List):-
	findall(X,temp(X),List1),
	sort(List1,List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% !close_valve
%% !shut_window
%% !turn_left

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ?has_leak
%% ?sufficient_funds
%% ?cheapest_book

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ^^book_table;
%% !start_car;
%% !pick_up_friends;
%% !go_for_dinner.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% +b(t) Belief Addition
%% -b(t) Belief Delition
%% +!g(t) Achievement Goal
%% -!g(t) Achievement Goal (Failure Handling)
%% +?g(t) Test Goal
%% -?g(t) Test Goal (Failure Handling)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% +!speak :: true <- actions.
%% +?move :: true <- action1;action2;action3.
%% -book(Prolog) :: true <- actions.
%% +book(Erlang) :: true <- actions.
%% -!walk :: true <- actions.
%% -?check :: true <- actions.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% like(cookies)
%% size(small)
%% gas(petrol)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% light(green)#[source(agent26)].
%% weather(cloudy)#[probability(0.8)].
%% champion(teamA)#[confidence(0.7)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% colour(yellow).
%% colour(yellow)#[source(self)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% object(front)#[source(self)].
%% object(front)#[source(agent22)].
%% object(front)#[source(agent21)].
%% object(front)#[source(agent21),source(agent22)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% % Bookstore Example
%% % Beliefs
%% bookstore_book(prolog,25).
%% bookstore_book(java,23).
%% bank_account(100).
%% % Plans
%% +!can_buy_book(Book) :: bookstore_book(Book, Price)
%% <-
%% ?bank_account(Balance) ;
%% Balance > Price ;
%% write([‘You have the money to buy ‘, Book]);
%% !buy_book(Book);
%% +my_books(Book).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% write/1
%% max/2, min/2
%% findall/3, setof/3, bagof/3

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Internal action wait _time in milliseconds
wait(Time):-
	get_time(T1),
	repeat,
	get_time(T2),
	Z is Time/1000,
	T2 > T1 + Z.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% +!make_cake :: true <-
%% 	!heat_oven;
%% 	!add_ingredients;
%% 	!mix;
%% 	!bake.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% +!can_buy_book(Book) :: bookstore_book(Book, Price)
%% <-
%% 	?check_bank_ac(Balance,Price);
%% 	Balance > Price;
%% 	write([‘You have the money to buy ‘, Book]);
%% 	!buy_book(Book);
%% 	+my_books(Book).
%% +?check_bank_ac(Bal,Pr) :: true
%% 	<-
%% 	?bank_account(Bal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% +!can_buy_book(Book) :: bookstore_book(Book, Price)
%% <-
%% 	?check_bank_ac(Balance,Price);
%% 	write([‘You have the money to buy ‘, Book]);
%% 	!buy_book(Book);
%% 	+my_books(Book).
%% +!can_buy_book(Book) :: true <-
%% 	write([‘The book ‘ ,Book, ‘ does not exist.’]).
%% +?check_bank_ac(Bal,Pr) :: true <-
%% 	?bank_account(Bal).
%% +?check_bank_ac(Bal,Pr) :: true <-
%% 	alternative_actions[...]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% +car(broken) :: true <-
%% 	!go_on_foot.
%% -weather(rainy) :: true <-
%% 	!go_for_a_ride.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% % Beliefs
%% book(anne_frank, 19).
%% book(harry_potter, 25).
%% book(hamlet, 28).
%% money(30).
%% % Plans
%% +!buy_book(Book) :: book(Book,Price) <-
%% 	?money(Amount) ;
%% 	Amount >= Price ;
%% 	write([‘You can afford buying’ , Book]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% +!g#[confidence(A)] :: true <- actionsA.
%% +!g#[confidence(B)] :: true <- actionsB.
%% +b#[source(A)] :: true <- actionsA.
%% +b#[source(B)] :: true <- actionsB.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% object(front).
%% object(front)#[source(agent1)].
%% object(front)#[source(agent2)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% belief(object(front),[source(self),source(agent1),source(
%% agent2)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% findMaxTemp(Temp):-
%% 	temp(Temp) &
%% 	not ( (temp(Y) & Y > Temp) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% rule(findMaxTemp(VAR_Temp), temp(VAR_Temp) & not (
%% (   temp(VAR_Temp_2) & VAR_Temp_2 > VAR_Temp) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% +!speak :: weather(nice) <-
%% 	!say_hello;
%% 	!go_for_a_ride.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% plan(+!speak,weather(nice),!say_hello;!go_for_a_ride).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% +!speak <-
%% 	!say_hello.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% plan(+!speak,true,!say_hello). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% plan(+!speak,context,true).
%% plan(+!speak,true,true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The Belief already exists with different annotation(s)
add_belief(B,Atts):-
	retract(belief(B,PreviousAtts)),
	!,
	union(Atts,PreviousAtts,NewAtts),
	assert(belief(B,NewAtts)),
	generate_agentspeak_event(addb(B,Atts)).
%% Completely New Belief
add_belief(B,Atts):-
	is_list(Atts),!,
	assert(belief(B,Atts)),
	%% this is to work with higher order predicates.
	assert(B),add_record(B),
	generate_agentspeak_event(addb(B,Atts)).
%%% The belief exists
remove_belief(B,Atts):-
	belief(B,PreviousAtts),
	!,
	subtract(PreviousAtts,Atts,NewAtts),
	retract(belief(B,PreviousAtts)),
	!,
	length(NewAtts,N),
	%% if the belief has no other annotations,
	%% there is no reason to be kept in BB
	(   N > 0 -> assert(belief(B,NewAtts)) ; retract(B#Atts)),
	generate_agentspeak_event(delb(B,Atts)).
%%% If the belief does not exist, then simply succeed

%%% without triggering any plan
remove_belief(_,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% findMaxTemp(Temp):-
%% 	temp(Temp) &
%% 	not ( (temp(Y) & Y > Temp) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% Plans getting triggered from test goals - TEST PLANS
%% Searching for a test plan where source(self) is omitted
find_plan(+?E#[source(self)],C,P):-
	plan(+?E,C,P),
	test_condition(C),
	!.
%% Searching for an annotated triggered test plan
find_plan(+?E#A,C,P):-
	plan(+?E#Annotations,C,P),
	subset(A,Annotations),
	test_condition(C),
	!.
%% Searching for a non-annotated triggered test plan
find_plan(+?E,C,P):-
	plan(+?E,C,P),
	test_condition(C),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Testing Conditions
test_condition(true):-!.
%%% Conjunction
test_condition(G1 & G2):-
	!,
	test_condition(G1),
	test_condition(G2).
%%% Disjunction
test_condition(G1 | G2):-
	!,
	(   test_condition(G1) ; test_condition(G2)).
%% Strong negation on beliefs
test_condition(~G):-
	!,exists_belief(~G).
%%% Negation NOT on anything
test_condition(not G):-
	test_condition(G),!,fail.
test_condition(not _):-
	!,true.
%%% Annotated beliefs
test_condition(G # A):-
	exists_belief(G,A),!.
%%% Non-annotated Beliefs
test_condition(G):-
	exists_belief(G,_).
%%% Handling a mental rule
test_condition(G):-
	rule(G,B),
	test_condition(B).
test_condition(G):-
	%% FIXME: do we also have to say this is not a belief?
	not(rule(G,_)),
	G.
%% A belief exists in BB.
exists_belief(B):-
	belief(B,_).
%% Belief Annotation on lists of annotations
%% A belief condition holds if it is a subset of beliefs.
exists_belief(B,Att):-
	belief(B,Attributes),
	subset(Att,Attributes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% % Solving a test goal
%% solve(?E):-
%% 	test_condition(E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% -g#[error(non_applicable_plan)]
%% -g#[error(no_relvevant_plan)]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% GUESS %%

find_plan(+!E#[source(self)],C,P):-
	plan(+!E,C,P),
	test_condition(C),
	!.
%% Searching for an annotated triggered test plan
find_plan(+!E#A,C,P):-
	plan(+!E#Annotations,C,P),
	subset(A,Annotations),
	test_condition(C),
	!.
%% Searching for a non-annotated triggered test plan
find_plan(+!E,C,P):-
	plan(+!E,C,P),
	test_condition(C),
	!.

%%%%%%

find_plan(+E#[source(self)],C,P):-
	plan(+E,C,P),
	test_condition(C),
	!.
%% Searching for an annotated triggered test plan
find_plan(+E#A,C,P):-
	plan(+E#Annotations,C,P),
	subset(A,Annotations),
	test_condition(C),
	!.
%% Searching for a non-annotated triggered test plan
find_plan(+E,C,P):-
	plan(+E,C,P),
	test_condition(C),
	!.

%%%%%%

find_plan(-E#[source(self)],C,P):-
	plan(-E,C,P),
	test_condition(C),
	!.
%% Searching for an annotated triggered test plan
find_plan(-E#A,C,P):-
	plan(-E#Annotations,C,P),
	subset(A,Annotations),
	test_condition(C),
	!.
%% Searching for a non-annotated triggered test plan
find_plan(-E,C,P):-
	plan(-E,C,P),
	test_condition(C),
	!.

%%%%%%

add_record(_).

clean_base :-
	retractall(belief(_,_)),
	retractall(rule(_,_)),
	retractall(plan(_,_,_)),
	retractall('#'(_,_)).

look :-
	findall(find_plan(X,Y,Z),find_plan(X,Y,Z),A),print_term(A,[]),nl.

l :-
	findall(plan(X,Y,Z),plan(X,Y,Z),A),print_term(A,[]),nl,look.

print(X) :-
	write(X),nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%% Failure Handling Mechanism%%%%%%%%%%%%%%
%%%%%%%%%%% Failure Conditions in Achievement Plans%%%%%%%%%%
%% Annotated Failure Handling Plan(error(non_applicable_plan))
find_plan(+!E,_,_):-
	% at least one plan (+!E) exists but is not applicable
	plan(+!E,_,_),
	% error provisioned
	plan(-!E#[error(non_applicable_plan)],C,P),
	test_condition(C),
	!,
	solve(P).
%% Annotated Failure Handling Plan (error(no_relevant_plan))
find_plan(+!E,_,_):-
	not(plan(+!E,_,_)),
	plan(-!E#[error(no_relevant_plan)],C,P),
	test_condition(C),
	!,
	solve(P).
%% NON-ANNOTATED Failure Handling Plan (catch all)
find_plan(+!E,_,_):-
	plan(-!E,C,P),
	nl,
	test_condition(C),
	solve(P),
	!.
%% NON-applicable Failure Handling Plan
find_plan(+!E,_,_):-
	plan(-!E,_,_),
	nl,
	write(['No Applicable Plan found for goal event',+!E]),nl,
	write(['Failure Plan found but NOT applicable for goal event ',+!E]),nl,
	nb_getval(remaining,P2),
	write(['Could not finish intention:' ,P2]),nl,
	!,
	fail,
	!.
%% In case there is NO applicable plan at all
find_plan(+!E,_,_):-
	plan(+!E,_,_),
	nl,
	write(['No Applicable Plan found for goal event ',+!E]),nl,
	write(['No Fail event was generated for goal ',!E]),nl,
	nb_getval(remaining,P2),
	write(['Could not finish intention:' ,P2]),nl,
	!,
	fail,
	!.
%% In case there is NO relevant plan at all
find_plan(+!E,_,_):-
	not(plan(+!E,_,_)),
	nl,
	write(['No Relevant Plan found for goal event ',+!E]),nl,
	nb_getval(remaining,P2),
	write(['Could not finish intention:' ,P2]),nl,
	!,
	fail,
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve(P1;P2):-
	!,nb_setval(remaining,P2),
	solve(P1), !, solve(P2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% test FH_A2 ok. Annotated failure handling plan for
%% error(no_relvevant_plan)
%% test FH_A3 ok. Non-annotated failure handling plan

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Code for thread handling - threads.pl
%%% start_agent/0
%%% Starts the main_agent_thread
start_agent:-
	thread_create(main_agent,_,[alias(main_agent_thread)]).
%%% stop_agent/0
%%% Stops the agent
stop_agent:-
	thread_send_message(main_agent_thread,end),
	thread_join(main_agent_thread,_).
%%% main_agent/0
%%% Main Agent thread.As usual this is a repeat fail loop.
main_agent:-
	repeat,
	thread_get_message(Mess),
	once(process_message(Mess)),
	once(clear_processes),
	off(Mess).
%%% off/0
%%% Succeeds and exits the main failure drive loop (Shut Down)
off(end):-!.
%%% Message to stop agent Execution.
process_message(end).
%%% The role of the clear message is to do any joins
process_message(clear).
%%% creates a thread to handle any plans triggered from adding a belief in BB
process_message(addb(X,Atts)):-
	name_thread(add_belief(X,Atts),Alias),
	thread_create(solve(addb(X,Atts)),_,[alias(Alias)]).
%%% creates a thread to handle any plans triggered from deleting a belief from BB
process_message(delb(X,Atts)):-
	name_thread(del_belief(X,Atts),Alias),
	thread_create(solve(delb(X,Atts)),_,[alias(Alias)]).
%%% creates a thread to handle an achieve message
process_message(achieve(X)):-
	name_thread(achieve(X),Alias),
	thread_create(solve(X),_,[alias(Alias)]).
%%% creates a thread to handle an ask message
process_message(asks(X,Content)):-
	name_thread(asks(X,Content),Alias),
	thread_create(answer_message(X,Content),_,[alias(Alias)]).
%%% name_thread/2
%%% Create a unique alias for the thread.
name_thread(Task,Alias):-
	get_time(T),
	term_string(iStack(Task,T),S),
	atom_string(Alias,S).
%%% clear_processes/0
%%% Succeeds by joining any threads that have succeeded.
clear_processes:-
	thread_property(I,status(true)),
	thread_join(I,_),
	write([I,_Status]),nl,
	fail.
%% Clearing threads that have failed.
clear_processes:-
	thread_property(I,status(false)),
	thread_join(I,_),
	write(['***',I,failed]),nl,
	fail.
clear_processes.
%%% Predicates used by others
generate_agentspeak_event(AGL_Event):-
	thread_send_message(main_agent_thread,AGL_Event).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve(^^E):-
	generate_agentspeak_event(achieve(!E)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_term(Belief):-  %% parsing the belief
	!,
	add_belief(Belief,[source(self)]),
	!,
	fail.

add_belief(B,Atts):- %% sending a “process message” to the main_agent_thread
	write(hi),
	is_list(Atts),!, 
	write(hi),
	assert(belief(B,Atts)), 
	write(hi),
	assert(B),add_record(B),
	write(hi),
	generate_agentspeak_event(addb(B,Atts)).
process_message(addb(X,Atts)):- %% creating a thread to execute the belief addition
	name_thread(add_belief(X,Atts),Alias),
	thread_create(solve(addb(X,Atts)),_,[alias(Alias)]).

solve(addb(B,A)):- %% executing the plan that is triggered from the belief addition
	find_plan(+B#A,_,P), 
	!,
	solve(P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% agent_Name@IP

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% p2pmsg(registry@10.140.5.212,bob@10.140.5.212,registering
%%       _agent)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_agent_communication(nocom):-!.
%% The default port for Pedro server is 4550
start_agent_communication(Name):-
	establish_connection(Name,4550),
	thread_create(main_agent_com_code,_,[alias(com_thread)]).
stop_agent_communication:-
	whoami(D),pedro_send_pp_message(D,off),
	wait(500),
	pedro_deregister,
	close_pedro_connection.
main_agent_com_code:-
	repeat,
	once(get_pedro_message_data(Rock,Content)),
	once(process_p_message(Rock,Content)),
	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% p2pmsg(alice@192.168.43.251,bob@192.168.43.251,tell(hello))
%% p2pmsg(bob@192.168.43.251,alice@192.168.43.251,tell(speak))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Sending point to point agent messages.
pedro_send_pp_message(Receiver,Content):-
	match_list(Receiver,Addr),Addr \= none,
	whoami(Me),
	send_ack_pedro_message(p2pmsg(Addr,Me,Content),success),
	!.
pedro_send_pp_message(Receiver,Content):-
	write(['Failed to send message',Receiver,Content]).
%%% Already in correct form.
match_list(Receiver@IP,Receiver@IP):-!.
%%% Matching a name
match_list(Receiver,Receiver@IP):-
	agent_list(ListofAgents),
	member(Receiver@IP,ListofAgents),!.
match_list(_Receiver,none).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% [1,agent_list([alice@192.168.43.251])]
%% [1,agent_list([alice@192.168.43.251,bob@192.168.43.251])]

%% [0,p2pmsg(alice@192.168.43.251,bob@192.168.43.251,test(ok))]
%% [0,p2pmsg(bob@192.168.43.251,alice@192.168.43.251,tell(yes))]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% send(Receiver, ACT, Term).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send(Receiver,ACT,Term):-
	Content=..[ACT,Term],
	pedro_send_pp_message(Receiver,Content).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% tell(temp(42))
%% untell(weather(rainy))
%% ask(colour(green))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Message received from com_thread  Message to the main_agent_thread
%% tell(Content)		addextb(Content,[source(Sender)])
%% untell(Content)		delextb(Content,[source(Sender)])
%% achieve(Content) 	extachieve(Content#[source(Sender)])
%% test(Content) 		exttest(Content#[source(Sender)])
%% ask(Content) 		asks(Sender,Content)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_p_message(_,p2pmsg(_,Sender,Message)):-
	fix_message_for_main(Sender,Message,ToMain),
	generate_agentspeak_event(ToMain).
%% Forward process messages to main agent thread
fix_message_for_main(Sender,tell(Content),addextb(Content,[source(Sender)])).
fix_message_for_main(Sender,untell(Content),delextb(Content,[source(Sender)])).
fix_message_for_main(Sender,achieve(Content),extachieve(Content#[source(Sender)])).
fix_message_for_main(Sender,test(Content),exttest(Content#[source(Sender)])).
fix_message_for_main(Sender,ask(Content),asks(Sender,Content)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_message(addextb(Content,Atts)):-
	add_belief(Content,Atts).
process_message(delextb(Content,Atts)):-
	remove_belief(Content,Atts).
process_message(extachieve(X)):-
	name_thread(extachieve(X),Alias),
	thread_create(solve(!X),_,[alias(Alias)]).
process_message(exttest(X)):-
	name_thread(exttest(X),Alias),
	thread_create(solve(?X),_,[alias(Alias)]).
process_message(asks(X,Content)):-
	name_thread(asks(X,Content),Alias),
	thread_create(answer_message(X,Content),_,[alias(Alias)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

answer_message(Sender,Content):-
	test_condition(Content),
	pedro_send_pp_message(Sender,answer(Content)).
fix_message_for_main(Sender,answer(Content),addextb(Content,[source(Sender)])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 1. Start the Pedro server.
%% 2. Start the Registry.
%% 3. Start the agent Alice.
%% 4. Start the agent Bob.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Registry Ready
%% Waiting
%% p2pmsg(registry@192.168.1.10,alice@192.168.1.10,registering_agent)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Waiting
%% p2pmsg(registry@192.168.1.10,bob@192.168.1.10,registering_agent)
%% Waiting

%% [1,agent_list([alice@192.168.1.10,bob@192.168.1.10])]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% %%% This is alice
%% %%% Beliefs
%% test(aa).
%% test(bb).
%% %%% Plans
%% +hello#[source(S)] :: true <-
%% 	write([received,hello,S]);
%% 	send(S,tell,hithere).
%% -hello#[source(S)] :: true <-
%% 	write([received,delbel,S]);
%% 	send(S,tell,delbelief).
%% +!speak#[source(S)] :: true <-
%% 	write([received,speak,S]);
%% 	send(S,tell,speak).
%% +?check#[source(S)] :: true <-
%% 	write([received,test_check,S]);
%% 	send(S,tell,checked).
%% %% --------------------------------------------------------------------------------------------------------
%% %%% This is bob
%% %%% Beliefs
%% test(1).
%% test(2).
%% %%% Plans
%% +!com_test:: true <-
%% 	send(alice,tell,hello);
%% 	send(alice,untell,hello);
%% 	send(alice,achieve,speak);
%% 	send(alice,test,check);
%% 	send(alice,ask,test(aa));
%% 	wait(300);
%% 	write('test 5 -ok. ask message');nl;
%% +hithere#[source(_)] :: true <-
%% 	write('test 1 -ok. message received');nl;
%% 	!testBel.
%% +!testBel :: hithere <-
%% 	write('test 2 -ok. tell message');nl.
%% +!testBel::true <-
%% 	write('test 2 -failed. tell message').
%% +delbelief#[source(_)] ::true <-
%% 	write('test 3 -ok. untell message');nl.
%% +speak#[source(_)] ::true <-
%% 	wait(200);
%% 	write('test 4 -ok. achieve message');nl.
%% +checked#[source(_)] :: true <-
%% 	wait(500);
%% 	write('test 6 -ok. test message').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Content = test(1),
%% Annotation = [source(self)] ;
%% Content = test(2),
%% Annotation = [source(self)] ;
%% Content = hithere,
%% Annotation = [source(alice@'192.168.43.251')] ;
%% Content = speak,
%% Annotation = [source(alice@'192.168.43.251')] ;
%% Content = delbelief,
%% Annotation = [source(alice@'192.168.43.251')] ;
%% Content = checked,
%% Annotation = [source(alice@'192.168.43.251')] ;
%% Content = test(aa),
%% Annotation = [source(alice@'192.168.43.251')] ;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
