isBinTree(empty).
isBinTree(node(L,_,R)) :- isBinTree(L), isBinTree(R).

isLeafTree(leaf(_)).
isLeafTree(branch(L,R)) :- isLeafTree(L), isLeafTree(R).

% flatten for Bintree
flatten(empty, []).
flatten(node(L,X,R),List) :- flatten(L,LL), flatten(R,RL),append(LL, [X|RL], List).

% flatten for Leaftree
flatten(leaf(X),[X]).
flatten(branch(L,R), List) :- flatten(L, LL), flatten(R,RL), append(LL, RL, List).


% using pivot point to divde list into two sublists
sort(_,[],[],[]).
sort(P,[H|T],[H|Small],Big) :- H =< P, sort(P,T,Small,Big).
sort(P,[H|T],Small,[H|Big]) :- H > P, sort(P,T,Small,Big).

quicksort([],[]).
quicksort([H|T], SortList) :- sort(H,T,S,B), quicksort(S,LL),
    quicksort(B,RL), append(LL,[H|RL], SortList).

elemsOrdered(T,L) :- flatten(T, List),quicksort(List,L).