:- include("jps-normal.pl").

succ(nil, nil, 0, a).
succ(a, ax, 1, x).
succ(a, ay, 2, y).
succ(x, xz, 1, z).
succ(y, yz, 1, z).

succ(z, zb, 10, b).

hScore(a, 10).
hScore(x, 8).
hScore(y, 8).
hScore(z, 6).
hScore(b, 0).

goal(b).



start_A_star( InitState, PathCost) :-

	score(InitState, 0, 0, InitCost, InitScore) ,

	search_A_star( [node(InitState, nil, nil, InitCost , InitScore ) ], [ ], PathCost) .




search_A_star(Queue, ClosedSet, PathCost) :-

	fetch_list( Result, Queue, ClosedSet ),
	write("Wybierz wezel(1-N): "),write(Result), write("\n"),
	write("Cofnij krok (0)\n"),
	write("Porownaj postepy (-1)\n"),
	read(Choice),
	make_choice(Choice, Result, ClosedSet, PathCost ).




make_choice( -1, Result, [] , PathCost) :-
	write("Nie ma czego porównać!\n"),
	search_A_star(Result, [], PathCost).

make_choice( -1, Result, ClosedSet , PathCost) :-
	first(node( FState, Action, Parent, Cost, Score ), ClosedSet ),
	last(node(LState, _, _, _, _), ClosedSet ),
	std_start_A_star( LState, path_cost( STDPath, STDCost ), FState ),
	build_path(node(Parent, _ ,_ , _ , _ ) , ClosedSet, [Action/FState], Path),
	write("Standard A Star Path: "), write(STDPath), write("\n"),
	write("Standard A Star Cost: "), write(STDCost), write("\n"),
	write("Your Path: "), write(Path), write("\n"),
	write("Your Cost: "), write(Cost), write("\n"),
	show_compare_message( Cost, STDCost ),
	search_A_star(Result, ClosedSet, PathCost).


make_choice( 0, Result, [], PathCost) :-
	write("Nie można cofnąć na pustym !\n"),
	search_A_star( Result, [], PathCost ).


make_choice( 0, Result, [ Node | ClosedSet ], PathCost) :-
	write("Cofanie kroku\n"),
	expand( Node, NewNodes ),
	delall( NewNodes, Result, NewResult ),
	get_parent( Node , Parent, ParentCost ),
	insert_new_nodes([ Node ], NewResult, NewQueue),
	search_A_star( NewQueue, ClosedSet, PathCost ).


make_choice( Choice, Result, ClosedSet , PathCost ) :-
	fetch_choice(Choice, Result, Node, RestQueue),
	write("Rozwijanie wezla: "), write( Node ), write("\n"),
	continue(Node, RestQueue, ClosedSet, PathCost).



show_compare_message( Cost, STDCost ) :-
	Cost > STDCost,
	write("Twoja sciezka jest lepsza!\n").

show_compare_message( Cost, STDCost ) :-
	Cost < STDCost,
	write("Standardowa sciezka jest lepsza!\n").

show_compare_message( Cost, Cost ) :-
	write("Sciezki są tej samej jakosci!\n").



get_parent( node( State, Action, Parent, Cost, Score ), Parent, ParentScore ) :-
	succ( Parent, Action, StepCost, State),
	ParentScore is Cost - StepCost.


continue( [] , RestQueue, ClosedSet, PathCost ) :-
	write("Numer wiekszy niz ilosc wezlow!\n"),
	search_A_star( RestQueue, ClosedSet, PathCost ).

continue([ node(State, Action, Parent, Cost, _ ) | _ ] , _  ,  ClosedSet,
							path_cost(Path, Cost) ) :-

	goal( State), ! ,

	build_path(node(Parent, _ ,_ , _ , _ ) , ClosedSet, [Action/State], Path) .


continue([ Node | _ ], RestQueue, ClosedSet, Path)   :-

	expand( Node, NewNodes),

	insert_new_nodes(NewNodes, RestQueue, NewQueue),

	search_A_star(NewQueue, [Node | ClosedSet ], Path).





fetch(node(State, Action,Parent, Cost, Score),
			[node(State, Action,Parent, Cost, Score)  |RestQueue], ClosedSet, RestQueue) :-

	\+ member(node(State, _ ,_  , _ , _ ) , ClosedSet),   ! .


fetch(Node, [ _ |RestQueue], ClosedSet, NewRest) :-

	fetch(Node, RestQueue, ClosedSet , NewRest).







fetch_list([ node(State, Action,Parent, Cost, Score) | RestResult ] ,
			[node(State, Action,Parent, Cost, Score)  | RestQueue], ClosedSet) :-

	\+ member(node(State, _ ,_  , _ , _ ) , ClosedSet),

	fetch_list( RestResult, RestQueue, ClosedSet ).


fetch_list( RestResult, [ _ |RestQueue], ClosedSet) :-

	fetch_list( RestResult, RestQueue, ClosedSet).

fetch_list( [] , [], ClosedSet).



fetch_choice( _, [], [], []) :- !.

fetch_choice( 1, [H|T], [H], T ) :- !.

fetch_choice( N, [H | T], Node
, [H | Rest] ) :-
    NN is N - 1,
    fetch_choice( NN, T, Node, Rest ).






expand(node(State, _ ,_ , Cost, _ ), NewNodes)  :-

	findall(node(ChildState, Action, State, NewCost, ChildScore) ,
			(succ(State, Action, StepCost, ChildState),
			    score(ChildState, Cost, StepCost, NewCost, ChildScore) ) , NewNodes) .


score(State, ParentCost, StepCost, Cost, FScore)  :-

	Cost is ParentCost + StepCost ,

	hScore(State, HScore),

	FScore is Cost + HScore .



insert_new_nodes( [ ], Queue, Queue) .

insert_new_nodes( [Node|RestNodes], Queue, NewQueue) :-

	insert_p_queue(Node, Queue, Queue1),

	insert_new_nodes( RestNodes, Queue1, NewQueue) .



insert_p_queue(Node,  [ ], [Node] )      :-    ! .


insert_p_queue(node(State, Action, Parent, Cost, FScore),
		[node(State1, Action1, Parent1, Cost1, FScore1)|RestQueue],
			[node(State1, Action1, Parent1, Cost1, FScore1)|Rest1] )  :-

	FScore >= FScore1,  ! ,

	insert_p_queue(node(State, Action, Parent, Cost, FScore), RestQueue, Rest1) .


insert_p_queue(node(State, Action, Parent, Cost, FScore),  Queue,
				[node(State, Action, Parent, Cost, FScore)|Queue]) .








build_path(node(nil, _, _, _, _ ), _, Path, Path) :-    ! .

build_path(node(EndState, _ , _ , _, _ ), Nodes, PartialPath, Path)  :-
	
	del(Nodes, node(EndState, Action, Parent , _ , _  ) , Nodes1) ,

	build_path( node(Parent,_ ,_ , _ , _ ) , Nodes1,
						[Action/EndState|PartialPath],Path) .





delall( [X | Result], NewNodes, NewResult ) :-
	del_ob(NewNodes, X, Rest),
	delall( Result, Rest, NewResult ).

delall( [], NewNodes, NewNodes ).


del_ob([], X, []).

del_ob([X|R],X,R).

del_ob([Y|R],X,[Y|R1]):-
    X\=Y,
    del_ob(R,X,R1).


del([X|R],X,R).
del([Y|R],X,[Y|R1]) :-
	X\=Y,
	del(R,X,R1).


last(X,[X]).
last(X,[_|Z]) :- last(X,Z).

first(X,[X | _]).



