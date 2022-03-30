% Part 4 Experssion recogniser

isMixedExpr(constE(_)).
isMixedExpr(negE(E)) :- isMixedExpr(E).
isMixedExpr(absE(E)) :- isMixedExpr(E).
isMixedExpr(plusE(L,R)) :- isMixedExpr(L), isMixedExpr(R).
isMixedExpr(timesE(L,R)) :- isMixedExpr(L), isMixedExpr(R).
isMixedExpr(minusE(L,R)) :- isMixedExpr(L), isMixedExpr(R).
isMixedExpr(expE(L,R)) :- isMixedExpr(L), isMixedExpr(R).

isMixedExpr(tt).
isMixedExpr(ff).
isMixedExpr(bnot(E)) :- isMixedExpr(E).
isMixedExpr(band(L,R)) :- isMixedExpr(L), isMixedExpr(R).
isMixedExpr(bor(L,R)) :- isMixedExpr(L), isMixedExpr(R).

% Interpreter

interpretMixedExpr(constE(N), X) :- X is N.
interpretMixedExpr(negE(E), X) :- interpretMixedExpr(E, S), X is -S.
interpretMixedExpr(absE(E), X) :- interpretMixedExpr(E, S), X is abs(S).
interpretMixedExpr(plusE(L,R), X) :- interpretMixedExpr(L, LR), interpretMixedExpr(R, RR), X is LR + RR.
interpretMixedExpr(timesE(L,R), X) :- interpretMixedExpr(L, LR), interpretMixedExpr(R, RR), X is LR * RR.
interpretMixedExpr(minusE(L,R), X) :- interpretMixedExpr(L, LR), interpretMixedExpr(R, RR), X is LR - RR.
interpretMixedExpr(expE(L,R), X) :- interpretMixedExpr(L, LR), interpretMixedExpr(R, RR), X is LR ** RR.

interpretMixedExpr(tt).
interpretMixedExpr(ff) :- false.
interpretMixedExpr(tt, X) :- X = true.
interpretMixedExpr(ff,X) :- X = false.

interpretMixedExpr(bnot(E),X) :- E == tt -> X = false; X = true.
interpretMixedExpr(band(L,R),X) :- interpretMixedExpr(L,XL), XL == false, X = false; interpretMixedExpr(R,X).
interpretMixedExpr(bor(L,R),X) :- interpretMixedExpr(L,XL), XL == false, interpretMixedExpr(R,X); X = true.