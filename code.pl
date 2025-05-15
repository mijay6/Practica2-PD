:- module(_,_,[classic,assertions,regtypes]).

:- doc(title, "Práctica 2: ISO-Prolog").

:- doc(author_data/4, "Define los datos del autor del módulo.").

author_data('Dobra','','Mihai','240912').

:- doc(module, "
@section{Introducción}

Este módulo implementa un compresor y descompresor de secuencias de caracteres.
El objetivo es reducir la longitud de una secuencia representando patrones repetitivos
de forma más concisa. La compresión es con pérdidas si la secuencia comprimida
es más larga que la original, pero el objetivo es encontrar la representación más corta.

@section{Representación de datos}

Las secuencias son listas de Prolog. Los elementos de estas listas sin comprimir estan compuestas por caracteres.

Una secuencia comprimida también es una lista, pero puede contener:
@begin{itemize}
    @item Átomos originales.
    @item Pares @tt{[Elemento, Num]}, donde @tt{Elemento} es un átomo y @tt{Num} es el número de veces que se repite.
    @item Grupos complejos @tt{['<', S1, S2, ..., Sk, '>', Num]}, donde @tt{S1...Sk} es una subsecuencia (que puede estar ya comprimida o ser una mezcla de átomos y grupos simples) 
    y @tt{Num} indica cuántas veces se repite toda la subsecuencia @tt{S1...Sk}.
@end{itemize}
La descompresión revierte estos formatos a la secuencia original de átomos.

@section{Funcionalidades}

El módulo implementa las siguientes funcionalidades principales:

@begin{enumerate}
    @item Compresión de secuencias: Busca la representación más corta de una secuencia dada, utilizando memoización para optimizar el proceso.
    @item Descompresión de secuencias: Reconstruye la secuencia original a partir de su forma comprimida.
    @item Predicados auxiliares para dividir secuencias, agrupar elementos repetidos, y encontrar la compresión óptima entre varias candidatas.
@end{enumerate}


@section{Ejemplos de uso}

A continuación, se presenta un ejemplo de cada funcionalidad principal del módulo.

@bf{split/3 - Dividir una secuencia:}
@begin{verbatim}
?- split([a,b,c], Parte1, Parte2).
Parte1 = [a],
Parte2 = [b, c] ? ;
Parte1 = [a, b],
Parte2 = [c] ? ;
@end{verbatim}

@bf{group/3 - Formar un grupo comprimido:}
@begin{verbatim}
?- group([a,b,c,d,e], 3, Grupo).
Grupo = ['<', a, b, c, d, e, '>', 3]
@end{verbatim}

@bf{is_repeated/3 - Verificar si una secuencia es una repetición:}
@begin{verbatim}
?- is_repeated([a,b,a,b,a,b], [a,b], Num).
Num = 3
@end{verbatim}

@bf{simple_repetition/2 - Compresión por repetición simple:}
@begin{verbatim}
?- simple_repetition([x,y,x,y,x,y], Comprimida).
Comprimida = ['<', x, y, '>', 3]
@end{verbatim}

@bf{repetition/2 - Compresión por repetición (con compresión interna):}
@begin{verbatim}
?- repetition([a,a,a,a], Comprimida) 
Comprimida = [a, 4] ? ; 
Comprimida = ['<',a,a,'>',2]
@end{verbatim}

@bf{compress/2 - Compresión completa de una secuencia:}
@begin{verbatim}
?- compress([a,b,b,b,b,a,b,b,b,b], Comprimida).
Comprimida = ['<', a, b, 4, '>', 2]
@end{verbatim}

@bf{min_list/2 - Encontrar la lista más corta en una lista de listas:}
@begin{verbatim}
?- min_list([[a,b,c],[a,b],[a,b,a,b]], Min).
Min = [a, b]
@end{verbatim}

@bf{decompress/2 - Descompresión de una secuencia:}
@begin{verbatim}
?- decompress(['<',a,a,b,b,'>',3,c], Descomprimida).
Descomprimida = [a, a, b, b, a, a, b, b, a, a, b, b, c]
@end{verbatim}

@bf{expand_sequence/3 - Expandir una secuencia repetidamente:}
@begin{verbatim}
?- expand_sequence([a,b,c], 3, SecuenciaExpandida).
SecuenciaExpandida = [a, b, c, a, b, c, a, b, c]
@end{verbatim}
").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Preliminary Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- doc(section, preliminary_predicates).

%--------------------------------------------
% Predicate 1: split/3
%--------------------------------------------
:- doc(split/3, "Este predicado es cierto si @var{Parte1} y @var{Parte2} son dos subsecuencias no vacías que, cuando se concatenan, forman la secuencia @var{Todo}.").
:- pred split(Todo, Parte1, Parte2) :: list * list * list 
   # "Su implementación es: @includedef{split/3}".

split(Todo, Parte1, Parte2):-
    append(Parte1, Parte2, Todo),
    Parte1 \== [],
    Parte2 \== [].

%--------------------------------------------
% Predicate 2: group/3
%--------------------------------------------
:- doc(group/3, "Este predicado relaciona una secuencia @var{Parte} y un número de repeticiones @var{Num} con su representación agrupada @var{Grupo}.
    Si @var{Parte} tiene más de un elemento, @var{Grupo} será de la forma @tt{['<', Parte..., '>', Num]}.
    Si @var{Parte} tiene un solo elemento @tt{[X]}, @var{Grupo} será @tt{[X, Num]}.
    Si @var{Parte} es vacía, @var{Grupo} será @tt{[Num]}.
    El predicado puede funcionar en ambas direcciones (componiendo o descomponiendo @var{Grupo}).").
:- pred group(Parte, Num, Grupo) :: list * integer * list
   # "Su implementación es: @includedef{group/3}".

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
:- doc(is_repeated/3, "Este predicado es cierto si la secuencia @var{Cs} consiste en la secuencia @var{Parte} repetida @var{Num} veces.
    @var{Num} debe ser mayor que 0 para que la repetición sea válida en el contexto de compresión.").
:- pred is_repeated(Cs, Parte, Num) :: list * list * integer
   # "Su implementación es: @includedef{is_repeated/3}".

is_repeated([], _, 0).  
is_repeated(Cs, Parte, Num):-
    Cs \== [], 
    Parte \== [], 
    append(Parte , Aux, Cs), 
    is_repeated(Aux, Parte, Num1), 
    ( var(Num) -> Num is Num1 + 1
    ;
        Num > 0,
        Num is Num1 + 1
    ).

%--------------------------------------------
% Predicate 4: simple_repetition/2
%--------------------------------------------
:- doc(simple_repetition/2, "Este predicado es cierto si @var{Comprimida} es una compresión de @var{Inicial} que consiste en una única repetición de alguna @var{Parte} de @var{Inicial}.
    Busca una @var{Parte} tal que @var{Inicial} sea @var{Parte} repetida @var{Num} veces, y luego forma @var{Comprimida} usando @pred{group/3}.").
:- pred simple_repetition(Inicial, Comprimida) :: list * list
   # "Su implementación es: @includedef{simple_repetition/2}".

simple_repetition(Inicial, Comprimida):-
    Inicial \== [],
    split(Inicial, Parte1, _),
    is_repeated(Inicial, Parte1, Num),
    group(Parte1, Num, Comprimida).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Compression Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- doc(section, compression_predicates).

%--------------------------------------------
% Principal predicate: compress/2
%--------------------------------------------
:- doc(compress/2, "Predicado principal para la compresión. Es cierto si @var{Comprimida} es la versión comprimida (óptima según la heurística implementada) de la secuencia @var{Inicial}.
    Utiliza memoización para mejorar la eficiencia.").
:- pred compress(Inicial, Comprimida) :: list * list
   # "Su implementación es: @includedef{compress/2}".

compress(Inicial, Comprimida):-
    clean_memo,
    recursive_compression(Inicial, Comprimida).

%---------------------------------------------
% Predicate: memo/2
%---------------------------------------------
:- doc(memo/2, "Predicado dinámico utilizado para la memoización. Almacena los resultados de compresiones ya calculadas (@var{Inicial} -> @var{Comprimida}) para evitar recálculos.").
:- pred memo(Inicial, Comprimida) :: list * list
   # "Almacena una @var{Comprimida} para una @var{Inicial} dada.".

:- dynamic memo/2.

% ---------------------------------------------
% Predicate: clean_memo/0
% ---------------------------------------------
:- doc(clean_memo/0, "Limpia todos los hechos almacenados por el predicado @pred{memo/2}. Se llama antes de iniciar una nueva compresión completa.").
:- pred clean_memo
   # "Elimina todas las entradas de la tabla de memoización.
      Su implementación es: @includedef{clean_memo/0}".

clean_memo:-
    retractall(memo(_, _)).

%---------------------------------------------
% Predicate: recursive_compression/2
%---------------------------------------------
:- doc(recursive_compression/2, "Núcleo recursivo de la compresión. Intenta encontrar la mejor compresión para @var{Inicial} usando @pred{better_compression_memo/2}.
    Si no se encuentra una compresión mejor que la original, devuelve la @var{Inicial} misma. El corte asegura que se devuelve una única solución óptima.").
:- pred recursive_compression(Inicial, Comprimida) :: list * list
   # "Su implementación es: @includedef{recursive_compression/2}".

recursive_compression(Inicial, Comprimida):-
    better_compression_memo(Inicial, Comprimida), !.
recursive_compression(Inicial, Inicial).

%---------------------------------------------
% Predicate: better_compression_memo/2
%---------------------------------------------
:- doc(better_compression_memo/2, "Implementa la lógica de memoización para @pred{better_compression/2}.
    Primero consulta si la compresión para @var{Inicial} ya está en @pred{memo/2}.
    Si no, llama a @pred{better_compression/2} para calcularla y luego la almacena en @pred{memo/2}.").
:- pred better_compression_memo(Inicial, Comprimida) :: list * list
   # "Su implementación es: @includedef{better_compression_memo/2}".

better_compression_memo(Inicial, Comprimida):-
    memo(Inicial, Comprimida),
    !.
better_compression_memo(Inicial, Comprimida):-
    better_compression(Inicial, Comprimida),
    assert(memo(Inicial, Comprimida)).

%----------------------------------------------
% Predicate: better_compression/2
%---------------------------------------------
:- doc(better_compression/2, "Encuentra la mejor compresión para @var{Inicial} entre todas las posibles compresiones generadas por @pred{compression/2}.
    Si ninguna compresión es más corta que @var{Inicial}, @var{Comprimida} se unifica con @var{Inicial}.").
:- pred better_compression(Inicial, Comprimida) :: list * list
   # "Selecciona la mejor @var{Comprimida} para @var{Inicial}.
      Su implementación es: @includedef{better_compression/2}".

better_compression(Inicial, Comprimida):-
    findall(Comprimida, compression(Inicial, Comprimida), Lista),
    min_list(Lista, Posible_Comprimida),
    length(Inicial, L1),
    length(Posible_Comprimida, L2),
    (   L1 > L2 ->
        Comprimida = Posible_Comprimida
    ;
        Comprimida = Inicial
    ).

%---------------------------------------------
% Predicate: compression/2
%---------------------------------------------
:- doc(compression/2, "Generador de posibles compresiones para @var{Inicial}.
    Intenta comprimir por @pred{repetition/2} o por @pred{division/2}.").
:- pred compression(Inicial, Comprimida) :: list * list
   # "Su implementación es: @includedef{compression/2}".

compression(Inicial, Comprimida):-
    repetition(Inicial, Comprimida).
compression(Inicial, Comprimida):-
    division(Inicial, Comprimida).

%---------------------------------------------
% Predicate: repetition/2
%---------------------------------------------
:- doc(repetition/2, "Intenta comprimir @var{Inicial} encontrando una @var{Parte1} que se repite @var{Num} veces.
    La @var{Parte1} se comprime recursivamente antes de ser agrupada.").
:- pred repetition(Inicial, Comprimida) :: list * list
   # "Su implementación es: @includedef{repetition/2}".

repetition(Inicial, Comprimida):-
    Inicial \== [],
    split(Inicial, Parte1, _),
    is_repeated(Inicial, Parte1, Num),
    Num > 1,
    recursive_compression(Parte1, Precomprimida),
    group(Precomprimida, Num, Comprimida).

%---------------------------------------------
% Predicate: division/2
%---------------------------------------------
:- doc(division/2, "Intenta comprimir @var{Inicial} dividiéndola en dos @var{Parte1} y @var{Parte2}, comprimiendo cada una recursivamente,
    y luego concatenando sus compresiones. Solo tiene éxito si al menos una de las partes se comprime").
:- pred division(Inicial, Comprimida) :: list * list
   # "Su implementación es: @includedef{division/2}".

division(Inicial, Comprimida):-
    Inicial \== [],
    split(Inicial, Parte1, Parte2),
    recursive_compression(Parte1, Precomprimida1),
    recursive_compression(Parte2, Precomprimida2),
    ( Precomprimida1 \== Parte1 ; Precomprimida2 \== Parte2 ),
    append(Precomprimida1, Precomprimida2, Comprimida).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Decompression Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- doc(section, decompression_predicates).

%---------------------------------------------
% Principal predicate: decompress/2
%---------------------------------------------
:- doc(decompress/2, "Predicado principal para la descompresión. Es cierto si @var{Descomprimida} es la secuencia original que resulta de descomprimir la secuencia @var{Comprimida}.
    Maneja los diferentes formatos de compresión (grupos simples, grupos complejos, caracteres sueltos).").
:- pred decompress(Comprimida, Descomprimida) :: list * list
   # "Su implementación es: @includedef{decompress/2}".

decompress([], []).
decompress(['<' | T1], Descomprimida) :-
    !, 
    extract_group(T1, Patron, Num, Resto), 
    decompress(Patron, PatronDesc),
    expand_sequence(PatronDesc, Num, GrupoDesc),
    decompress(Resto, RestoDesc),
    append(GrupoDesc, RestoDesc, Descomprimida).
decompress([Caracter, Num | Resto], Descomprimida) :-
    atom(Caracter), Caracter \== '<', Caracter \== '>',
    integer(Num), Num > 1,
    !,
    expand_sequence([Caracter], Num, GrupoDesc),
    decompress(Resto, RestoDesc),
    append(GrupoDesc, RestoDesc, Descomprimida).
decompress([Caracter| RestoComp], [Caracter | RestoDesc]) :-
    atom(Caracter), Caracter \== '<', Caracter \== '>',
    !,
    decompress(RestoComp, RestoDesc).

%---------------------------------------------
% Predicate: extract_group/4
%---------------------------------------------
:- doc(extract_group/4, "Predicado auxiliar para @pred{decompress/2}. Dada una @var{Lista} (la parte de una secuencia comprimida después de un '<'),
    extrae el @var{Patron} interno (hasta el '>'), el @var{Num} de repetición que sigue al '>', y el @var{Resto} de la @var{Lista} original después de @var{Num}.").
:- pred extract_group(Lista, Patron, Num, Resto) :: list * list * integer * list
   # "Su implementación es: @includedef{extract_group/4}".

extract_group(Lista, Patron, Num, Resto) :-
    append(Parte1, Resto, Lista),
    append(ParteInterna, [Num], Parte1),
    integer(Num), Num >=0,
    append(Patron, ['>'], ParteInterna).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% AUXILIARY PREDICATES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- doc(section, auxiliary_predicates).

%---------------------------------------------
% Predicate: min_list/2
%---------------------------------------------
:- doc(min_list/2, "Encuentra la secuencia @var{Min} de menor longitud en una @var{Lista} de secuencias.").
:- pred min_list(Lista, Min) :: list(list) * list
   # "Su implementación es: @includedef{min_list/2}".

min_list([H|T], Min):-
    length(H, L1),
    min_list_aux(T, H, L1, Min).

%---------------------------------------------
% Predicate: min_list_aux/4
%---------------------------------------------
:- doc(min_list_aux/4, "Auxiliar recursivo de cola para @pred{min_list/2}.
    Mantiene el @var{Min_Actual} y su longitud @var{L}, comparándolos con el siguiente elemento de la @var{Lista} para encontrar el @var{Min_Final}.").
:- pred min_list_aux(Lista, Min_Actual, L, Min_Final) :: list(list) * list * integer * list
   # "Auxiliar para @pred{min_list/2}.
      Su implementación es: @includedef{min_list_aux/4}".

min_list_aux([], Min_Actual, _, Min_Actual).
min_list_aux([H|T], Min_Actual, L, Min_Final):-    
    length(H, L2),
    ( L2 < L ->
        min_list_aux(T, H, L2, Min_Final)
    ;
        min_list_aux(T, Min_Actual, L, Min_Final)
    ).

%---------------------------------------------
% Predicate: expand_sequence/3
%---------------------------------------------
:- doc(expand_sequence/3, "Crea una @var{SecuenciaExpandida} repitiendo la @var{Secuencia} de entrada @var{Num} veces.").
:- pred expand_sequence(Secuencia, Num, SecuenciaExpandida) :: list * integer * list
   # "Su implementación es: @includedef{expand_sequence/3}".

expand_sequence(_, 0, []).
expand_sequence(Secuencia, Num, SecuenciaExpandida) :-
    Num > 0,
    Num1 is Num - 1,
    expand_sequence(Secuencia, Num1, SecuenciaParcial),
    append(Secuencia, SecuenciaParcial, SecuenciaExpandida).

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
:- test simple_repetition(Inicial, Comprimida) : (Inicial = [a,a,a,a]) => (member(Comprimida, [[a,4], ['<',a,a,'>',2]])) + not_fails.
% Test if Comprimida is a repetition of Inicial, with multiple elements, for 3 times
:- test simple_repetition(Inicial, Comprimida) : (Inicial = [x,y,x,y,x,y]) => (Comprimida = ['<',x,y,'>',3]) + not_fails.
% Test if Inicial is empty fails
:- test simple_repetition(Inicial, Comprimida) : (Inicial = []) + fails.
% Test if Inicial is not a repeated sequence
:- test simple_repetition(Inicial, Comprimida) : (Inicial = [a,a,a,b,b,b]) + fails.
% Test if Inicial is not a repeated sequence
:- test simple_repetition(Inicial, Comprimida) : (Inicial = [a,b,c,d,e]) + fails.

%---------------------------------------------
% Test for repetition/2 (the same as simple_repetition/2)
%---------------------------------------------

% Test if Comprimida is a repetition of Inicial for 4 times
:- test repetition(Inicial, Comprimida) : (Inicial = [a,a,a,a]) => (member(Comprimida, [[a,4], ['<',a,a,'>',2]])) + not_fails.
% Test if Comprimida is a repetition of Inicial, with multiple elements, for 3 times
:- test repetition(Inicial, Comprimida) : (Inicial = [x,y,x,y,x,y]) => (Comprimida = ['<',x,y,'>',3]) + not_fails.
% Test if Inicial is empty fails
:- test repetition(Inicial, Comprimida) : (Inicial = []) + fails.
% Test if Inicial is not a repeated sequence
:- test repetition(Inicial, Comprimida) : (Inicial = [a,a,a,b,b,b]) + fails.
% Test if Inicial is not a repeated sequence
:- test repetition(Inicial, Comprimida) : (Inicial = [a,b,c,d,e]) + fails.

%---------------------------------------------
% Test for compress/2
%---------------------------------------------

% Test if Comprimida is the compressed sequence of Inicial
:- test compress(Inicial, Comprimida) : (Inicial = [a,a,a,a]) => (Comprimida = [a,4]) + not_fails.
% Test if Comprimida is the compressed sequence of Inicial, with multiple elements
:- test compress(Inicial, Comprimida) : (Inicial = [x,y,x,y,x,y]) => (Comprimida = ['<',x,y,'>',3]) + not_fails.
% Test if Comprimida is the compressed sequence of Inicial, with mixed elements and a simple group
:- test compress(Inicial, Comprimida) : (Inicial = [a,a,b,b,b]) => (Comprimida = [a,a,b,3]) + not_fails.
% Test if Comprimida is the compressed sequence of Inicial,  with mixed elements and a simple group in the middle
:- test compress(Inicial, Comprimida) : (Inicial = [a,b,c,d,d,d,e,f]) => (Comprimida = [a,b,c,d,3,e,f]) + not_fails.
% Test if a sequence with no repetitions is not compressed
:- test compress(Inicial, Comprimida) : (Inicial = [a,b,c,d,e]) => (Comprimida = [a,b,c,d,e]) + not_fails.
% Test if an empty sequence is not compressed
:- test compress(Inicial, Comprimida) : (Inicial = []) => (Comprimida = []) + not_fails.
% Test if Comprimida is the compressed sequence of Inicial, with multiple elements
:- test compress(Inicial, Comprimida) : (Inicial = [a,b,b,b,b,a,b,b,b,b]) => (Comprimida = [<,a,b,4,>,2]) + not_fails.
% Test if Comprimida is the compressed sequence of Inicial, with multiple elements
:- test compress(Inicial, Comprimida) : (Inicial = [a,a,b,b,a,a,b,b,a,a,b,b,c]) => (Comprimida = [<,a,a,b,b,>,3,c]) + not_fails.

%----------------------------------------------
% Test for min_list/2
%----------------------------------------------

% Test if Min is the minimum sequence in the list
:- test min_list(Lista, Min) : (Lista = [[a,b,c],[a,b],[a,b,a,b]]) => (Min = [a,b]) + not_fails.
% Test if Min is the minimum sequence in the list
:- test min_list(Lista, Min) : (Lista = [[a,b,c],[a],[a,b],[a]]) => (Min = [a]) + not_fails.
% Test if Lista is a list with one element
:- test min_list(Lista, Min) : (Lista = [[a,b,c]]) => (Min = [a,b,c]) + not_fails.
% Test if Lista is empty
:- test min_list(Lista, Min) : (Lista = []) + fails.

%---------------------------------------------
% Tests for decompress/2
%---------------------------------------------

% Test if Descomprimida is the decompressed sequence of Comprimida
:- test decompress(Comprimida, Descomprimida) : (Comprimida = [a,4]) => (Descomprimida = [a,a,a,a]) + not_fails.
% Test if Descomprimida is the decompressed sequence of Comprimida, with a complex group
:- test decompress(Comprimida, Descomprimida) : (Comprimida = ['<',x,y,'>',3]) => (Descomprimida = [x,y,x,y,x,y]) + not_fails.
% Test if Descomprimida is the decompressed sequence of Comprimida, with mixed elements and a simple group
:- test decompress(Comprimida, Descomprimida) : (Comprimida = [a,a,b,3]) => (Descomprimida = [a,a,b,b,b]) + not_fails.
% Test if Descomprimida is the decompressed sequence of Comprimida, with mixed elements and a simple group in the middle
:- test decompress(Comprimida, Descomprimida) : (Comprimida = [a,b,c,d,3,e,f]) => (Descomprimida = [a,b,c,d,d,d,e,f]) + not_fails.
% Test if an already decompressed sequence remains unchanged
:- test decompress(Comprimida, Descomprimida) : (Comprimida = [a,b,c,d,e]) => (Descomprimida = [a,b,c,d,e]) + not_fails.
% Test if Comprimida is empty
:- test decompress(Comprimida, Descomprimida) : (Comprimida = []) => (Descomprimida = []) + not_fails.
% Test if Descomprimida is the decompressed sequence of Comprimida, with a complex group containing a simple group
:- test decompress(Comprimida, Descomprimida) : (Comprimida = ['<',a,b,4,'>',2]) => (Descomprimida = [a,b,b,b,b,a,b,b,b,b]) + not_fails.
% Test if Descomprimida is the decompressed sequence of Comprimida, with a complex group followed by a single element
:- test decompress(Comprimida, Descomprimida) : (Comprimida = ['<',a,a,b,b,'>',3,c]) => (Descomprimida = [a,a,b,b,a,a,b,b,a,a,b,b,c]) + not_fails.
% Test if Descomprimida is the decompressed sequence of Comprimida, which is a partially compressed sequence
:- test decompress(Comprimida, Descomprimida) : (Comprimida = [a,a,a,b,3]) => (Descomprimida = [a,a,a,b,b,b]) + not_fails.

%---------------------------------------------
% Test for expand_sequence/3
%---------------------------------------------

% Test if SecuenciaExpandida is the expanded sequence of Secuencia repeated Num times
:- test expand_sequence(Secuencia, Num, SecuenciaExpandida) : (Secuencia = [a,b,c], Num = 3) => (SecuenciaExpandida = [a,b,c,a,b,c,a,b,c]) + not_fails.
% Test if the sequence is empty
:- test expand_sequence(Secuencia, Num, SecuenciaExpandida) : (Secuencia = [], Num = 0) => (SecuenciaExpandida = []) + not_fails.
% Test if Num is 0 and Secuencia is not empty
:- test expand_sequence(Secuencia, Num, SecuenciaExpandida) : (Secuencia = [a,b,c], Num = 0) => (SecuenciaExpandida = []) + not_fails.
