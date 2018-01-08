#standard a star

std_start_A_star( InitState, PathCost, Goal) :-

	std_score(InitState, 0, 0, InitCost, InitScore) ,

	std_search_A_star( [node(InitState, nil, nil, InitCost , InitScore ) ], [ ], Goal, PathCost) .




std_search_A_star(Queue, ClosedSet, Goal, PathCost) :-

	std_fetch(Node, Queue, ClosedSet , RestQueue),

	std_continue(Node, RestQueue, ClosedSet, Goal, PathCost).



std_continue(node(Goal, Action, Parent, Cost, _ ) , _  ,  ClosedSet, Goal, path_cost(Path, Cost) ) :-

	! ,

	std_build_path(node(Parent, _ ,_ , _ , _ ) , ClosedSet, [Action/Goal], Path) .


std_continue(Node, RestQueue, ClosedSet, Goal, Path)   :-

	std_expand( Node, NewNodes),

	std_insert_new_nodes(NewNodes, RestQueue, NewQueue),

	std_search_A_star(NewQueue, [Node | ClosedSet ], Goal, Path).





std_fetch(node(State, Action,Parent, Cost, Score),
			[node(State, Action,Parent, Cost, Score)  |RestQueue], ClosedSet, RestQueue) :-

	\+ member(node(State, _ ,_  , _ , _ ) , ClosedSet),   ! .


std_fetch(Node, [ _ |RestQueue], ClosedSet, NewRest) :-

	std_fetch(Node, RestQueue, ClosedSet , NewRest).



std_expand(node(State, _ ,_ , Cost, _ ), NewNodes)  :-

	findall(node(ChildState, Action, State, NewCost, ChildScore) ,
			(succ(State, Action, StepCost, ChildState),
			    std_score(ChildState, Cost, StepCost, NewCost, ChildScore) ) , NewNodes) .


std_score(State, ParentCost, StepCost, Cost, FScore)  :-

	Cost is ParentCost + StepCost ,

	hScore(State, HScore),

	FScore is Cost + HScore .



std_insert_new_nodes( [ ], Queue, Queue) .

std_insert_new_nodes( [Node|RestNodes], Queue, NewQueue) :-

	std_insert_p_queue(Node, Queue, Queue1),

	std_insert_new_nodes( RestNodes, Queue1, NewQueue) .



std_insert_p_queue(Node,  [ ], [Node] )      :-    ! .


std_insert_p_queue(node(State, Action, Parent, Cost, FScore),
		[node(State1, Action1, Parent1, Cost1, FScore1)|RestQueue],
			[node(State1, Action1, Parent1, Cost1, FScore1)|Rest1] )  :-

	FScore >= FScore1,  ! ,

	std_insert_p_queue(node(State, Action, Parent, Cost, FScore), RestQueue, Rest1) .


std_insert_p_queue(node(State, Action, Parent, Cost, FScore),  Queue,
				[node(State, Action, Parent, Cost, FScore)|Queue]) .








std_build_path(node(nil, _, _, _, _ ), _, Path, Path) :-    ! .

std_build_path(node(EndState, _ , _ , _, _ ), Nodes, PartialPath, Path)  :-

	std_del(Nodes, node(EndState, Action, Parent , _ , _  ) , Nodes1) ,

	std_build_path( node(Parent,_ ,_ , _ , _ ) , Nodes1,
						[Action/EndState|PartialPath],Path) .


std_del([X|R],X,R).
std_del([Y|R],X,[Y|R1]) :-
	X\=Y,
	std_del(R,X,R1).



