:- module(_,_,[classic,assertions,regtypes]).

% AUTHOR INFORMATION
author_data('Dobra','','Mihai','240912').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Type definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%--------------------------------------------
% Predicate 1: split/3
%--------------------------------------------


% @pred split(Todo, Parte1, Parte2)
% @arg Todo: the sequence to split
% @arg Parte1: the first part of the sequence
% @arg Parte2: the second part of the sequence

% This predicate is true if Parte1 and Parte2 are two non-empty
% subsequences that, when concatenated, form the sequence Todo.



split([H|T], [], []) :- !, fail.
split([H|T], [H|Parte1], Parte2) :-
    split(T, Parte1, Parte2).
split([H|T], Parte1, [H|Parte2]) :-
    split(T, Parte1, Parte2).
%--------------------------------------------


