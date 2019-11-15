lessthan(two,three).
greaterthan(three,two).
equals(two,two).
equals(three,three).
max(X,Y,Z) :- lessthan(X,Y),!,equals(Y,Z).
max(X,Y,X).