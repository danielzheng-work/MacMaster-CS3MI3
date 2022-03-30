% PArt 1: Primer checker predicate

hasDivisorLessThanOrEqualTo(_,1) :- !, false.
hasDivisorLessThanOrEqualTo(X,Y) :- 0 is X mod Y, !.
hasDivisorLessThanOrEqualTo(X,Y) :- Z is Y - 1, hasDivisorLessThanOrEqualTo(X,Z).

% Prime number cannot have divisor less than itself
isPrime(X) :- X > 1 , Z is X -1, not(hasDivisorLessThanOrEqualTo(X,Z)).

% Part 2: Number to list of digits predicate 

isDigitList(_,[]) :- false.
isDigitList(X,[X]):- X < 10.
% Matching the ones digit in Integer with The first element of the list
isDigitList(X,[H|T]) :- Q is X div 10, M is X mod 10, H is M, isDigitList(Q,T). 


% Part 3: Palindrome

% dropLast(L1,L2) if L2 is the list L1, leaving off the last item.
dropLast([_],[]). % The last element is dropped.
dropLast([H|T],[H|T2]) :-
  % Aside from the base case above, the lists must match.
  dropLast(T,T2).

% consider empty list and single element list are palindrome
isPalindrome([]).
isPalindrome([_]).

isPalindrome([H|T]) :- last(T,H), dropLast(T,T2), isPalindrome(T2).

% Part 4 : Prime palindromes

primePalindrome(X) :- isPrime(X), isDigitList(X,L), isPalindrome(L).

% Part 5: Efficiency
 
% Only thing can proent backtracking is add cut in isDigitList(_,[]) :- !, false.


