% male(+Person)
% Specifies the Person is a male.
%
male(abraham).
male(homer).
male(bartholomew).

% female(+Person)
% Specifies the Person is a female.
%
female(mona).
female(marjorie).
female(lisa).
female(margaret).

% mother(+Parent,+Child)
% Specifies Parent is a mother of Child.
%
mother(mona,homer).
mother(marjorie,bartholomew).
mother(marjorie,lisa).
mother(marjorie,margaret).

% father(+Parent,+Child)
% Specifies Parent is a father of Child.
%
father(abraham,homer).
father(homer,bartholomew).  	
father(homer,lisa).  	
father(homer,margaret).

% sibling(+Person1,+Person2)
% Specifies Person1 is a sibling of Person2
%
sibling(X,Y) :- father(Z,X), father(Z,Y).
sibling(X,Y) :- mother(Z,X), mother(Z,Y).

% brother(+Person1,+Person2)
% Specifies Person1 is a brother of Person2
%
brother(X,Y) :- sibling(X,Y), male(X).

% grandfather(+Person1,+Person2)
% Specifies Person1 is a grandfather of Person2
%
grandfather(X,Y) :- father(Z,Y), father(X,Z).



% my_list(+Item)
% Decides if Item is a list.
%
my_list([]).
my_list([X|Xs]) :- my_list(Xs).

% my_member(+Item,+List)
% Decides if Item is a member of List.
%
my_member(X, [X|_]).
my_member(X, [_|Ys]) :- my_member(X, Ys).

% my_length(+As,-N)
% Returns the length of list As in the variable N.
%
my_length([], 0).
my_length([A|As], N) :- my_length(As, N2), N is N2 + 1.

% my_append(+As,+Bs,-Cs)
% Returns the append of two lists As and Bs in the list Cs.
%
my_append([],Bs,Bs).
my_append([A|As],Bs, [A|Cs]) :- my_append(As, Bs, Cs). 

% my_reverse(+As,-Bs)
% Returns the reverse of list As in the list Bs.
%
my_reverse([],[]).
my_reverse([A|As], Bs) :- my_reverse(As, RevAs), my_append(RevAs, [A], Bs).

% my_prefix(+Pattern,+List)
% Decides if Pattern is a prefix of List.
%
my_prefix([],_).
my_prefix([X|Xs],[X|Ys]) :- my_prefix(Xs,Ys).

% my_subsequence(+Pattern,+List)
% Decides if Pattern is a subsequence (consecutive) of List.
%
my_subsequence([],_).
my_subsequence([X|Xs], [X|Ys]) :- my_prefix(Xs, Ys).
my_subsequence([X|Xs], [_|Ys]) :- my_subsequence([X|Xs], Ys).

% my_delete(+Item,+List,-Answer)
% Deletes all occurrences of Item from List and returns the result in Answer.
%
my_delete(_,[],[]).
my_delete(X,[X|Ys],Z) :- my_delete(X, Ys, Z).
my_delete(X,[Y|Ys],Z) :- my_append([Y],Znew,Z), my_delete(X,Ys,Znew).
