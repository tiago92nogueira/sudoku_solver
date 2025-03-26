%between
my_between(Low, High, Low) :- Low =< High.
my_between(Low, High, X) :-
    Low < High,
    Next is Low + 1,
    my_between(Next, High, X).

%append
my_append([], L, L).
my_append([H|T], L, [H|R]) :- my_append(T, L, R).

%(main)
sudoku(Puzzle, Solution) :-
    length(Puzzle, 9), % garante que a entrada é um sudoku 9x9
    transpose(Puzzle, _Columns), 
    solve(Puzzle, Solution).

transpose([[], [], [], [], [], [], [], [], []], []).
transpose(Matrix, [Row|Rows]) :-
    first_column(Matrix, Row, RestMatrix),
    transpose(RestMatrix, Rows).

first_column([], [], []). %1a col de uma matriz
first_column([[H|T]|Rows], [H|Hs], [T|Ts]) :-  
    first_column(Rows, Hs, Ts).

sublist(List, 0, N, SubList) :-
    my_append(SubList, _, List),
    length(SubList, N).
sublist([_|T], Start, N, SubList) :-
    Start > 0,
    Start1 is Start - 1,
    sublist(T, Start1, N, SubList).

%solver
solve(Puzzle, Solution) :-
    solve(Puzzle, Solution, 0, 0).

solve(Solution, Solution, 9, _) :- !. %todas as linhas preenchidas (chegou a linha 9)
solve(Puzzle, Solution, Row, 9) :-  
    NextRow is Row + 1, %se a coluna for a 9, passa para a linha seguinte
    solve(Puzzle, Solution, NextRow, 0).

solve(Puzzle, Solution, Row, Col) :-
    get_value(Puzzle, Row, Col, 0), % verifica se está vazia
    my_between(1, 9, N),  
    valid_placement(Puzzle, Row, Col, N),  
    replace(Puzzle, Row, Col, N, NewPuzzle), 
    NextCol is Col + 1, % preencheu a célula vazia e passa para a próxima
    solve(NewPuzzle, Solution, Row, NextCol).

solve(Puzzle, Solution, Row, Col) :-
    get_value(Puzzle, Row, Col, Value),
    Value \= 0, % se no quadrado está um valor dif de 0, avança
    NextCol is Col + 1,
    solve(Puzzle, Solution, Row, NextCol).

%le um valor da matriz
get_value([Row|_], 0, Col, Value) :- get_value_row(Row, Col, Value).
get_value([_|Rows], Row, Col, Value) :-
    Row > 0, 
    Row1 is Row - 1, 
    get_value(Rows, Row1, Col, Value).

get_value_row([H|_], 0, H).
get_value_row([_|T], Col, Value) :-
    Col > 0, 
    Col1 is Col - 1, 
    get_value_row(T, Col1, Value).

% verifica se um n pode ser posto num quadradinho
valid_placement(Puzzle, Row, Col, N) :-
    get_row(Puzzle, Row, RowVals),
    get_column(Puzzle, Col, ColVals),
    get_block(Puzzle, Row, Col, BlockVals),
    my_append(RowVals, ColVals, Temp1),  
    my_append(Temp1, BlockVals, AllVals),  
    \+ member(N, AllVals).  

%linha
get_row([Row|_], 0, Row).
get_row([_|Rows], Row, Result) :-
    Row > 0, 
    Row1 is Row - 1, 
    get_row(Rows, Row1, Result).

%coluna
get_column([], _, []).
get_column([Row|Rows], Col, [Value|ColVals]) :-
    get_value_row(Row, Col, Value),  
    get_column(Rows, Col, ColVals).

%  valores do quadrado 3×3
get_block(Puzzle, Row, Col, Block) :-
    BlockRowStart is (Row // 3) * 3,  
    BlockColStart is (Col // 3) * 3,  
    get_block_rows(Puzzle, BlockRowStart, Rows),  
    extract_block_values(Rows, BlockColStart, Block).  

get_block_rows(Puzzle, Start, Rows) :- %extrai 3 linhas do sudoku
    sublist(Puzzle, Start, 3, Rows).

%forma uma lista com os valores de um quadrado 3x3 
extract_block_values([], _, []).
extract_block_values([Row|Rows], Start, Block) :-
    sublist(Row, Start, 3, Sub),
    extract_block_values(Rows, Start, Remaining),  
    my_append(Sub, Remaining, Block).  

% poe numero num quadradinho
replace([Row|Rows], 0, Col, N, [NewRow|Rows]) :-  
    replace_in_row(Row, Col, N, NewRow).  
replace([Row|Rows], RowIndex, Col, N, [Row|NewRows]) :-  
    RowIndex > 0,  
    RowIndex1 is RowIndex - 1,  
    replace(Rows, RowIndex1, Col, N, NewRows).  

%poe um numero num local especifico de uma lista
replace_in_row([_|T], 0, N, [N|T]).  
replace_in_row([H|T], Col, N, [H|NewT]) :-  
    Col > 0,  
    Col1 is Col - 1,  
    replace_in_row(T, Col1, N, NewT).
