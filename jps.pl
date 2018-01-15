:- include("jps-normal.pl").
:- include("graph2.pl").
:- include("permut.pl").



%start_A_star

start_A_star( InitState, PathCost, ChoiceLength, PathLength) :-
	score(InitState, 0, 0, InitCost, InitScore),
	write("Uruchamiam A* z maksymalnym krokiem: "), write(PathLength), write("\n"),
	search_A_star( [node(InitState, nil, nil, InitCost , InitScore ) ], [ ], PathCost, ChoiceLength, PathLength).

start_A_star( InitState, PathCost, ChoiceLength, PathLength) :-
	NewPathLength is PathLength + 2,
	start_A_star( InitState, PathCost, ChoiceLength, NewPathLength).



%search_A_star

search_A_star(Queue, ClosedSet, PathCost, ChoiceLength, PathLength) :-
    write("-----------------------------------------------\n"),
	write("Porownaj postepy (-1)\n"),
	write("Wybierz kolejnosc wezlow: (1) \n"),
	read(Choice),
	make_choice(Choice, Queue, ClosedSet, PathCost, ChoiceLength, PathLength).

%make_choice

make_choice( -1, Result, [] , PathCost, ChoiceLength, PathLength) :-
	write("Nie ma czego porównać!\n"),
	search_A_star(Result, PathCost, ChoiceLength, PathLength), !.

make_choice( -1, Result, ClosedSet , PathCost, ChoiceLength, PathLength) :-
	first(node( FState, Action, Parent, Cost, Score ), ClosedSet ),
	last(node(LState, _, _, _, _), ClosedSet ),
	std_start_A_star( LState, path_cost( STDPath, STDCost ), FState ),
	build_path(node(Parent, _ ,_ , _ , _ ) , ClosedSet, [Action/FState], Path),
	write("Sciezka A Star: "), write(STDPath), write("\n"),
	write("Koszt A Star: "), write(STDCost), write("\n"),
	write("Twoja sciezka: "), write(Path), write("\n"),
	write("Twoj koszt: "), write(Cost), write("\n"),
	show_compare_message( Cost, STDCost ),
	search_A_star(Result, ClosedSet, PathCost, ChoiceLength, PathLength), !.

make_choice( 1, Queue, ClosedSet , PathCost, ChoiceLength, PathLength) :-
	PathLength > -1,
	fetch_list(ChoiceLength, Result,  Queue, ClosedSet ),
	write("Wezly: "), write(Result), write("\n"),
	write("Podaj kolejne numery wezlow: \n"),
	read_list(Result, NodePermut),
	fetch_permut(Result, NodePermut, Node),
	continue(Node, ClosedSet, PathCost, ChoiceLength, PathLength).

read_list([], [] ) :- !.

read_list([ A | B ] , [ Choice | NodePermut ] ) :-
	read(Choice),
	read_list(B, NodePermut).


%show_compare_message

show_compare_message( Cost, STDCost ) :-
	Cost < STDCost,
	write("Twoja sciezka jest lepsza!\n"), !.

show_compare_message( Cost, STDCost ) :-
	Cost > STDCost,
	write("Sciezka A star jest lepsza!\n"), !.

show_compare_message( Cost, Cost ) :-
	write("Sciezki są tej samej jakosci!\n"), !.



%get_parent

get_parent( node( State, Action, Parent, Cost, Score ), Parent, ParentScore ) :-
	succ( Parent, Action, StepCost, State),
	ParentScore is Cost - StepCost, !.



%continue

continue(node(State, Action, Parent, Cost, _ ) , ClosedSet,
							path_cost(Path, Cost), ChoiceLength, PathLength) :-
	goal( State), ! ,
	build_path(node(Parent, _ ,_ , _ , _ ) , ClosedSet, [Action/State], Path) .


continue(Node, ClosedSet, Path, ChoiceLength, PathLength)   :-
	PathLength > 0,
	write("Rozwijanie wezla: "), write( Node ), write("\n"),
	expand( Node, NewNodes),
	insert_new_nodes(NewNodes, [], NewQueue),
	NewPathLength is PathLength - 1, !,
	search_A_star(NewQueue, [Node | ClosedSet ], Path, ChoiceLength, NewPathLength).



%fetch_list

fetch_list(_, [], [], _) :- !.

fetch_list(N, [ node(State, Action,Parent, Cost, Score) | RestResult ], [node(State, Action,Parent, Cost, Score)  | RestQueue], ClosedSet) :-
    N > 0,
    \+ member(node(State, _ ,_  , _ , _ ) , ClosedSet), !,
    NN is N - 1,
	fetch_list(NN, RestResult, RestQueue, ClosedSet ).

fetch_list(N, RestResult, [ _ |RestQueue], ClosedSet) :-
	fetch_list(N, RestResult, RestQueue, ClosedSet), !.


%fetch_choice

fetch_choice( _, [], [], []) :- !.

fetch_choice( 1, [H|T], H, T ).

fetch_choice( 1, [H|T], Node, [ H |T ] ) :-
	fetch_choice( 1, T, Node, Rest).

fetch_choice( N, [H | T], Node
, [H | Rest] ) :-
    NN is N - 1,
    fetch_choice( NN, T, Node, Rest ).



%expand

expand(node(State, _ ,_ , Cost, _ ), NewNodes)  :-
	findall(node(ChildState, Action, State, NewCost, ChildScore) ,
			(succ(State, Action, StepCost, ChildState),
			    score(ChildState, Cost, StepCost, NewCost, ChildScore) ) , NewNodes) .



%score

score(State, ParentCost, StepCost, Cost, FScore)  :-
	Cost is ParentCost + StepCost ,
	hScore(State, HScore),
	FScore is Cost + HScore .



%insert_new_nodes

insert_new_nodes( [ ], Queue, Queue) .

insert_new_nodes( [Node|RestNodes], Queue, NewQueue) :-
	insert_p_queue(Node, Queue, Queue1),
	insert_new_nodes( RestNodes, Queue1, NewQueue).



%insert_p_queue

insert_p_queue(Node,  [ ], [Node] )      :-    !.


insert_p_queue(node(State, Action, Parent, Cost, FScore),
		[node(State1, Action1, Parent1, Cost1, FScore1)|RestQueue],
			[node(State1, Action1, Parent1, Cost1, FScore1)|Rest1] )  :-
	FScore >= FScore1,  ! ,
	insert_p_queue(node(State, Action, Parent, Cost, FScore), RestQueue, Rest1).


insert_p_queue(node(State, Action, Parent, Cost, FScore),  Queue,
				[node(State, Action, Parent, Cost, FScore)|Queue]).



%build_path

build_path(node(nil, _, _, _, _ ), _, Path, Path) :-    !.

build_path(node(EndState, _ , _ , _, _ ), Nodes, PartialPath, Path)  :-
	del(Nodes, node(EndState, Action, Parent , _ , _  ) , Nodes1),
	build_path( node(Parent,_ ,_ , _ , _ ) , Nodes1,
						[Action/EndState|PartialPath],Path).



%delall

delall( [X | Result], NewNodes, NewResult ) :-
	del_ob(NewNodes, X, Rest),
	delall( Result, Rest, NewResult ).

delall( [], NewNodes, NewNodes ).



%del_ob

del_ob([], X, []).

del_ob([X|R],X,R).

del_ob([Y|R],X,[Y|R1]):-
    X\=Y,
    del_ob(R,X,R1).



%del

del([X|R],X,R).
del([Y|R],X,[Y|R1]) :-
	X\=Y,
	del(R,X,R1).



%last

last(X,[X]).
last(X,[_|Z]) :- last(X,Z).



%first

first(X,[X | _]).









