:- module(_,_,[classic,assertions,regtypes]).

% AUTHOR INFORMATION
author_data('Dobra','','Mihai','240912').


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

% The implementation uses the append/3 predicate to concatenate the two parts
% and checks that both parts are non-empty.

split(Todo, Parte1, Parte2):-
    append(Parte1, Parte2, Todo),
    Parte1 \== [],
    Parte2 \== [].

%--------------------------------------------
% Predicate 2: group/3
%--------------------------------------------

% @pred group(Parte, Num, Grupo)
% @arg Parte: the list of characters
% @arg Num: the number of repetitions
% @arg Grupo: the list of characters and the number of repetitions

% This predicate is true if Grupo is a list that contains the elements of Parte, whit parentsis if Parte has 2 or more elements,
% and the number Num, which is the result of composing the list Parte with a number of repetitions Num.

% The implementation use two clauses:
% 1. The first clause is used when Grupo is instantiated, and it checks if Parte is a list of characters and Num is a positive integer.
%    It checks if the first element of Grupo is a list that contains the elements of Parte, and the last element is the number Num.
% 2. The second clause is used when Grupo is not instantiated, and it checks if Parte is a list of characters and Num is a positive integer.
%    It appends the elements of Parte with the number Num, and if Parte has more than 1 element, it adds parentheses around it.

% The implementation uses the append/3 predicate to concatenate the elements of Parte and Num,
% and the length/2 predicate to check the length of Parte.
% The predicate var/1 is used to check if Grupo is uninstantiated, nonvar/1 is used to check if Parte and Grupo is instantiated,
% and integer/1 is used to check if Num is a positive integer.

group(Parte, Num, Grupo):-
    nonvar(Grupo),
    integer(Num),
    Num > 0,
    append(TempParte, [Num], Grupo),
    length(TempParte, L1),
    (   L1 > 1 -> 
        (   append(['<'], Temp2, TempParte),
            append(Parte, ['>'], Temp2)
        )
        ;
        Parte = TempParte
    ).
group(Parte, Num, Grupo):-
    var(Grupo),
    nonvar(Parte),
    integer(Num),
    Num > 0,
    (   length(Parte, L),    
        ( L > 1 -> 
            (   append(['<'], Parte, Temp1),
                append(Temp1, ['>'], Temp2),
                append(Temp2, [Num], Grupo)
            )
        ;
            append(Parte, [Num], Grupo)
        )
    ).

%--------------------------------------------
% Predicate 3: is_repeated/3
%--------------------------------------------

% @pred is_repeated(Cs, Parte, Num)
% @arg Cs: the sequence Parte repeated Num times
% @arg Parte: the list of characters
% @arg Num: the number of repetitions

% This predicate is true if Cs is a list that contains the elements of Parte, repeated Num times.
% The implementation uses recursion with two clauses:
% 1. Base case: An empty list is repeated 0 times.
% 2. Recursive case: If Cs is not empty and Parte is not empty, it checks if Cs starts with Parte,
%    and then recursively checks the rest of the list (Aux) to see if it is also a repetition of Parte.

% The implementation uses the append/3 predicate to check if Cs starts with Parte and
% the is/2 predicate to check if Num is the sum of the number of repetitions.
% The predicate var/1 is used to check if Num is uninstantiated, and if so, it generates the value of Num.

is_repeated([], _, 0).  
is_repeated(Cs, Parte, Num):-
    Cs \== [], 
    Parte \== [], 
    append(Parte , Aux, Cs), 
    is_repeated(Aux, Parte, Num1), 
    ( var(Num) -> Num is Num1 + 1;
        Num > 0,
        Num is Num1 + 1
    ).

%--------------------------------------------
% Predicate 4: simple_repetition/2
%--------------------------------------------

% @pred simple_repetition(Inicial, Comprimida)
% @arg Inicial: the sequence to compress
% @arg Comprimida: the compressed sequence

% This predicate is true if Comprimida is a list that contains the elements of Inicial compressed in only one group of repetitions.

% The implementation uses the split/3 predicate to split Inicial into two parts, Parte1 and Parte2.
% It then checks if Parte1 is a repetition of Inicial using the is_repeated/3 predicate.
% If it is, it groups Parte1 and the number of repetitions using the group/3 predicate.
% Uses backtracking to find all possible splits of Inicial.

simple_repetition(Inicial, Comprimida):-
    Inicial \== [],
    Inicial \== [_,_],
    split(Inicial, Parte1, _),
    is_repeated(Inicial, Parte1, Num),
    group(Parte1, Num, Comprimida).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%--------------------------------------------
% Tests for split/3
%--------------------------------------------

% Test that the specified sequence is split correctly   
:- test split(Todo, Parte1, Parte2) : (Todo = [1,2,3,4,5], Parte1 = [1,2], Parte2 = [3,4,5]) + not_fails.
% Test if the predicate generates the first solution expected
:- test split(Todo, Parte1, Parte2) : (Todo = [a,b,c], Parte1 = [a], Parte2 = [b,c]) + not_fails.
% Test that the predicate issues if the sequence is too short
:- test split(Todo, Parte1, Parte2) : (Todo = [a]) + fails.
% Test that the predicate issues if the sequence is empty
:- test split(Todo, Parte1, Parte2) : (Todo = []) + fails.
% Test that the predicate issues if the subsequence Parte1 is empty
:- test split(Todo, Parte1, Parte2) : (Todo = [1,2,3,4,5], Parte1 = [], Parte2 = [1,2,3,4,5]) + fails.
% Test that the predicate issues if the subsequence Parte2 is empty
:- test split(Todo, Parte1, Parte2) : (Todo = [1,2,3,4,5], Parte1 = [1,2,3,4,5], Parte2 = []) + fails.

%--------------------------------------------
% Tests for group/3
%--------------------------------------------

% Test if with more than 2 elements, the predicate generates the first solution expected, with parentsis.
:- test group(Parte, Num, Grupo) : (Parte = [a,b,c,d,e], Num = 3) => (Grupo = ['<',a,b,c,d,e,'>',3]) + not_fails.
% Test if with 2 elementes, the predicate generates the first solution expected, with parentsis.
:- test group(Parte, Num, Grupo) : (Parte = [a,b], Num = 2) => (Grupo = ['<',a,b,'>',2]) + not_fails.
% Test if with 1 element, the predicate generates the first solution expected, without parentsis.
:- test group(Parte, Num, Grupo) : (Parte = [z], Num = 5) => (Grupo = [z,5]) + not_fails.
% Test if with 0 element, the predicate generates the first solution expected, without parentsis.
:- test group(Parte, Num, Grupo) : (Parte = [], Num = 5) => (Grupo = [5]) + not_fails.
% Test if fails with Num = 0
:- test group(Parte, Num, Grupo) : (Parte = [a,b,c], Num = 0) + fails.
% Test if Num is not a number
:- test group(Parte, Num, Grupo) : (Parte = [a,b,c], Num = not_a_number) + fails.
% Test if parte is not a list
:- test group(Parte, Num, Grupo) : (Parte = not_a_list, Num = 3) + fails.
% Test if the generated Parte plus Num is the list Grupo
:- test group(Parte, Num, Grupo) : (Num = 3, Grupo = ['<',a,b,c,d,e,'>',3]) => (Parte = [a,b,c,d,e])+ not_fails.
% Test if Num is different from the num of grupo
:- test group(Parte, Num, Grupo) : (Num = 3, Grupo = ['<',a,b,c,d,e,'>',2]) + fails.

%--------------------------------------------
% Tests for is_repeated/3
%--------------------------------------------

% Test if Cs is a repetition of Parte for 3 times
:- test is_repeated(Cs, Parte, Num) : (Cs = [a,b,a,b,a,b], Parte = [a,b]) => (Num = 3) + not_fails.
% Test if Cs is empty, Num is 0
:- test is_repeated(Cs, Parte, Num) : (Cs = [], Parte = [a,b,c]) => (Num = 0) + not_fails.
% Test if Cs and Parte are empty, Num is 0
:- test is_repeated(Cs, Parte, Num) : (Cs = [], Parte = []) => (Num = 0) + not_fails.
% Test if cs not is a repetition of Parte
:- test is_repeated(Cs, Parte, Num) : (Cs = [a,b,c], Parte = [a,b]) + fails.
% Test if Parte cant forms cs
:- test is_repeated(Cs, Parte, Num) : (Cs = [a,b], Parte = [a,b,c]) + fails.
% Test if num is not a good number
:- test is_repeated(Cs, Parte, Num) : (Cs = [a,a,a], Parte = [a], Num = 0) + fails.

%--------------------------------------------
% Tests for simple_repetition/2
%--------------------------------------------

% Test if Comprimida is a repetition of Inicial for 4 times
:- test simple_repetition(Inicial, Comprimida) : (Inicial = [a,a,a,a]) => (Comprimida = [a,4]; Comprimida = ['<',a,a,'>',2]) + not_fails.
% Test if Comprimida is a repetition of Inicial, with multiple elements, for 3 times
:- test simple_repetition(Inicial, Comprimida) : (Inicial = [x,y,x,y,x,y]) => (Comprimida = ['<',x,y,'>',3]) + not_fails.
% Test if Inicial is empty fails
:- test simple_repetition(Inicial, Comprimida) : (Inicial = []) + fails.
% Test if Inicial is not a repeated sequence
:- test simple_repetition(Inicial, Comprimida) : (Inicial = [a,a,a,b,b,b]) + fails.
% Test if Inicial is not a repeated sequence
:- test simple_repetition(Inicial, Comprimida) : (Inicial = [a,b,c,d,e]) + fails.
