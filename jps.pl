:- include("jps-normal.pl").
:- include("graph2.pl").



%start_A_star

start_A_star( InitState, PathCost, ChoiceLength) :-
	score(InitState, 0, 0, InitCost, InitScore),
	search_A_star( [node(InitState, nil, nil, InitCost , InitScore ) ], [ ], PathCost, ChoiceLength).



%search_A_star

search_A_star(Queue, ClosedSet, PathCost, ChoiceLength) :-
	fetch_list(ChoiceLength, Result, AllNonMembers,  Queue, ClosedSet ),
    write("-----------------------------------------------\n"),
	write("Wybierz wezel(1-N): "),write(Result), write("\n"),
	write("Cofnij krok (0)\n"),
	write("Porownaj postepy (-1)\n"),
	read(Choice),
	make_choice(Choice, Result, AllNonMembers, ClosedSet, PathCost, ChoiceLength).



%make_choice 

make_choice( -1, Result, AllNonMembers, [] , PathCost, ChoiceLength) :-
	write("Nie ma czego porównać!\n"),
	search_A_star(AllNonMembers, [], PathCost, ChoiceLength).

make_choice( -1, Result, AllNonMembers, ClosedSet , PathCost, ChoiceLength) :-
	first(node( FState, Action, Parent, Cost, Score ), ClosedSet ),
	last(node(LState, _, _, _, _), ClosedSet ),
	std_start_A_star( LState, path_cost( STDPath, STDCost ), FState ),
	build_path(node(Parent, _ ,_ , _ , _ ) , ClosedSet, [Action/FState], Path),
	write("Sciazka A Star: "), write(STDPath), write("\n"),
	write("Koszt A Star: "), write(STDCost), write("\n"),
	write("Twoja sciezka: "), write(Path), write("\n"),
	write("Twoj koszt: "), write(Cost), write("\n"),
	show_compare_message( Cost, STDCost ),
	search_A_star(AllNonMembers, ClosedSet, PathCost, ChoiceLength).


make_choice( 0, Result, AllNonMembers, [], PathCost, ChoiceLength) :-
	write("Nie można cofnąć na pustym !\n"),
	search_A_star( AllNonMembers, [], PathCost ).


make_choice( 0, Result, AllNonMembers, [ Node | ClosedSet ], PathCost, ChoiceLength) :-
	write("Cofanie kroku\n"),
	expand( Node, NewNodes ),
	delall( NewNodes, AllNonMembers, NewResult ),
	get_parent( Node , Parent, ParentCost ),
	insert_new_nodes([ Node ], NewResult, NewQueue),
	search_A_star( NewQueue, ClosedSet, PathCost, ChoiceLength).


make_choice( Choice, Result, AllNonMembers, ClosedSet , PathCost, ChoiceLength) :-
	fetch_choice(Choice, Result, Node, RestQueue),
    delall( Node, AllNonMembers, RestNonMembers),
	write("Rozwijanie wezla: "), write( Node ), write("\n"),
	continue(Node, RestNonMembers, ClosedSet, PathCost, ChoiceLength).



%show_compare_message

show_compare_message( Cost, STDCost ) :-
	Cost < STDCost,
	write("Twoja sciezka jest lepsza!\n").

show_compare_message( Cost, STDCost ) :-
	Cost > STDCost,
	write("Sciezka A star jest lepsza!\n").

show_compare_message( Cost, Cost ) :-
	write("Sciezki są tej samej jakosci!\n").



%get_parent

get_parent( node( State, Action, Parent, Cost, Score ), Parent, ParentScore ) :-
	succ( Parent, Action, StepCost, State),
	ParentScore is Cost - StepCost.



%continue

continue( [] , RestQueue, ClosedSet, PathCost, ChoiceLength) :-
	write("Numer wiekszy niz ilosc wezlow!\n"),
	search_A_star( RestQueue, ClosedSet, PathCost, ChoiceLength).

continue([ node(State, Action, Parent, Cost, _ ) | _ ] , _  ,  ClosedSet,
							path_cost(Path, Cost), ChoiceLength) :-
	goal( State), ! ,
	build_path(node(Parent, _ ,_ , _ , _ ) , ClosedSet, [Action/State], Path) .


continue([ Node | _ ], RestQueue, ClosedSet, Path, ChoiceLength)   :-
	expand( Node, NewNodes),
	insert_new_nodes(NewNodes, RestQueue, NewQueue),
	search_A_star(NewQueue, [Node | ClosedSet ], Path, ChoiceLength).



%fetch_list

fetch_list(_, [], [], [], _).

fetch_list(N, [ node(State, Action,Parent, Cost, Score) | RestResult ], [node(State, Action,Parent, Cost, Score)  | RestNonMembers], [node(State, Action,Parent, Cost, Score)  | RestQueue], ClosedSet) :-
    N > 0,	
    \+ member(node(State, _ ,_  , _ , _ ) , ClosedSet),
    NN is N - 1,
	fetch_list(NN, RestResult, RestNonMembers, RestQueue, ClosedSet ).

fetch_list(0, RestResult , [node(State, Action,Parent, Cost, Score)  | RestNonMembers], [node(State, Action,Parent, Cost, Score)  | RestQueue], ClosedSet) :-
	\+ member(node(State, _ ,_  , _ , _ ) , ClosedSet),
	fetch_list(NN, RestResult, RestNonMembers, RestQueue, ClosedSet ).


fetch_list(N, RestResult, RestNonMembers, [ _ |RestQueue], ClosedSet) :-
	fetch_list(N, RestResult, RestNonMembers, RestQueue, ClosedSet).


%fetch_choice

fetch_choice( _, [], [], []) :- !.

fetch_choice( 1, [H|T], [H], T ) :- !.

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









