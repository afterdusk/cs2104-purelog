s(X,Y) :- q(X,Y).
s(zero,zero).
q(X,Y) :- i(X),!,j(Y).
i(one).
i(two).
j(one).
j(two).
j(three).