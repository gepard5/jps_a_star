
%fetches permutation of InputList using Order as
%list of element positions on InputList
%ResultList is Order-sized permutation of proper elements
%from InputList
%fetch_permut(InputList, Order, ResultList)
%If position put in Order list is incorrect for InputList (ex. exceeds %list size) then this fetch_permut() fails



fetch_permut(InputList, [N | _], NthElem) :-
	fetch_Nth(InputList, N, NthElem).

fetch_permut(InputList, [_ | Order], NthElem) :-
    fetch_permut(InputList, Order, NthElem).



fetch_Nth([X | _], 1, X).

fetch_Nth([X | InputList], N, NthElement) :-
    NN is N - 1,
    fetch_Nth(InputList, NN, NthElement).
