lessthan(two,three).
greaterthan(three,two).
max(X,Y,Y) :- lessthan(X,Y),!.
max(X,Y,X) :- greaterthan(X,Y).