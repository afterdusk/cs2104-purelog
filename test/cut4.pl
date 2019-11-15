a(X):-b(X),c(X),!.
a(X):-e(X).
b(X):-d(X),!,e(X).
c(b).
d(a).
e(a).