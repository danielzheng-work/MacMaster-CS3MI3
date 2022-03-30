% Experssion recogniser

isExpr(constE(_)).
isExpr(negE(E)) :- isExpr(E).
isExpr(absE(E)) :- isExpr(E).
isExpr(plusE(L,R)) :- isExpr(L), isExpr(R).
isExpr(timesE(L,R)) :- isExpr(L), isExpr(R).
isExpr(minusE(L,R)) :- isExpr(L), isExpr(R).
isExpr(expE(L,R)) :- isExpr(L), isExpr(R).


% Interpreter

interpretExpr(constE(N), X) :- X is N.
interpretExpr(negE(E), X) :- interpretExpr(E, S), X is -S.
interpretExpr(absE(E), X) :- interpretExpr(E, S), X is abs(S).
interpretExpr(plusE(L,R), X) :- interpretExpr(L, LR), interpretExpr(R, RR), X is LR + RR.
interpretExpr(timesE(L,R), X) :- interpretExpr(L, LR), interpretExpr(R, RR), X is LR * RR.
interpretExpr(minusE(L,R), X) :- interpretExpr(L, LR), interpretExpr(R, RR), X is LR - RR.
interpretExpr(expE(L,R), X) :- interpretExpr(L, LR), interpretExpr(R, RR), X is LR ** RR.