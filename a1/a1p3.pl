% Part 3 Experssion recogniser

isVarExpr(constE(_)).
isVarExpr(negE(E)) :- isVarExpr(E).
isVarExpr(absE(E)) :- isVarExpr(E).
isVarExpr(plusE(L,R)) :- isVarExpr(L), isVarExpr(R).
isVarExpr(timesE(L,R)) :- isVarExpr(L), isVarExpr(R).
isVarExpr(minusE(L,R)) :- isVarExpr(L), isVarExpr(R).
isVarExpr(expE(L,R)) :- isVarExpr(L), isVarExpr(R).

isVarExpr(var(_)).
isVarExpr(subst(L,N,R)) :- isVarExpr(L), atom(N),isVarExpr(R).


% substitution 
substitution(constE(X),N,C,R):- R = constE(X).
substitution(var(X),N,C,R):- X==N -> R = C; R = var(X).
substitution(negE(E),N,C,R):- substitution(E,N,C,RR), R = negE(RR).
substitution(absE(E),N,C,R):- substitution(E,N,C,RR), R = absE(RR).
substitution(plusE(EL,ER),N,C,R):- substitution(EL,N,C,RL), substitution(ER,N,C,RE), R = plusE(RL,RE).
substitution(timesE(EL,ER),N,C,R):- substitution(EL,N,C,RL), substitution(ER,N,C,RE), R = timesE(RL,RE).
substitution(minusE(EL,ER),N,C,R):- substitution(EL,N,C,RL), substitution(ER,N,C,RE), R = minusE(RL,RE).
substitution(expE(EL,ER),N,C,R):- substitution(EL,N,C,RL), substitution(ER,N,C,RE), R = expE(RL,RE).
substitution(subst(L,X,XC),N,C,R):- substitution(L,X,XC,XR), substitution(XR,N,C,R).


% Interpreter

interpretVarExpr(constE(N), X) :- X is N.
interpretVarExpr(negE(E), X) :- interpretVarExpr(E, S), X is -S.
interpretVarExpr(absE(E), X) :- interpretVarExpr(E, S), X is abs(S).
interpretVarExpr(plusE(L,R), X) :- interpretVarExpr(L, LR), interpretVarExpr(R, RR), X is LR + RR.
interpretVarExpr(timesE(L,R), X) :- interpretVarExpr(L, LR), interpretVarExpr(R, RR), X is LR * RR.
interpretVarExpr(minusE(L,R), X) :- interpretVarExpr(L, LR), interpretVarExpr(R, RR), X is LR - RR.
interpretVarExpr(expE(L,R), X) :- interpretVarExpr(L, LR), interpretVarExpr(R, RR), X is LR ** RR.
interpretVarExpr(subst(L,N,C),X) :- substitution(L,N,C,RS), interpretVarExpr(RS, X).