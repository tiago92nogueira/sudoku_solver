%between
my_between(Low, High, Low) :- Low =< High. % caso base low dentro do intervalo
my_between(Low, High, X) :- % caso recursivo aumenta low até high
    Low < High,
    Next is Low + 1,
    my_between(Next, High, X).

%implementcao de append
my_append([], L, L).
my_append([H|T], L, [H|R]) :- my_append(T, L, R).

%(main)
sudoku(Puzzle, Solution) :-
    length(Puzzle, 9), % garante que a entrada é um sudoku 9x9
    transpose(Puzzle, _Columns), 
    solve(Puzzle, Solution).

transpose([[], [], [], [], [], [], [], [], []], []). %matriz vazia gera matriz vazia
transpose(Matrix, [Row|Rows]) :- 
    first_column(Matrix, Row, RestMatrix), %tira a primeira coluna para transpor recursivamente 
    transpose(RestMatrix, Rows).

%extrai a primeira coluna de uma matriz e retorna o restante da matriz sem essa coluna
first_column([], [], []). %vazio retorna vazio
first_column([[H|T]|Rows], [H|Hs], [T|Ts]) :-  
    first_column(Rows, Hs, Ts).

%obtem uma sublista de uma lista a partir de um índice e com um tamanho específico
sublist(List, 0, N, SubList) :-
    my_append(SubList, _, List),
    length(SubList, N).
sublist([_|T], Start, N, SubList) :-% ignora elementos até chegar ao índice escolhido
    Start > 0,
    Start1 is Start - 1,
    sublist(T, Start1, N, SubList).

%solver
solve(Puzzle, Solution) :-
    solve(Puzzle, Solution, 0, 0).% começa da posição (0,0)

solve(Solution, Solution, 9, _) :- !. %todas as linhas preenchidas (chegou a linha 9)
solve(Puzzle, Solution, Row, 9) :- % Se chegou ao final de uma linha, passa para a próxima 
    NextRow is Row + 1, 
    solve(Puzzle, Solution, NextRow, 0).

solve(Puzzle, Solution, Row, Col) :-
    get_value(Puzzle, Row, Col, 0), % verifica se está vazia
    my_between(1, 9, N), % Tenta numeros de 1 a 9  
    valid_placement(Puzzle, Row, Col, N),  % Verifica se o número pode ser colocado
    replace(Puzzle, Row, Col, N, NewPuzzle), %troca o 0 pelo numero
    NextCol is Col + 1, % preencheu a célula vazia e passa para a próxima
    solve(NewPuzzle, Solution, Row, NextCol).

solve(Puzzle, Solution, Row, Col) :-
    get_value(Puzzle, Row, Col, Value),
    Value \= 0, % se no quadrado está um valor dif de 0, avança
    NextCol is Col + 1,
    solve(Puzzle, Solution, Row, NextCol).

%obtem um valor da matriz
get_value([Row|_], 0, Col, Value) :- get_value_row(Row, Col, Value).
get_value([_|Rows], Row, Col, Value) :-
    Row > 0, 
    Row1 is Row - 1, 
    get_value(Rows, Row1, Col, Value).

% obtem um valor de uma linha da matriz dado um índice
get_value_row([H|_], 0, H).
get_value_row([_|T], Col, Value) :-
    Col > 0, 
    Col1 is Col - 1, 
    get_value_row(T, Col1, Value).

% verifica se um n pode ser posto num quadradinho
valid_placement(Puzzle, Row, Col, N) :-
    get_row(Puzzle, Row, RowVals), %obtem linha
    get_column(Puzzle, Col, ColVals), %obtem col
    get_block(Puzzle, Row, Col, BlockVals), %obtem bloco 3x3
    my_append(RowVals, ColVals, Temp1),  
    my_append(Temp1, BlockVals, AllVals),  
    \+ member(N, AllVals).  % garante que o número não está na linha, coluna ou no bloco 3x3
%/+ nega o member, que verifica se um n pertence a uma lista

%obtem uma linha da matriz
get_row([Row|_], 0, Row). %se linha é zero, retorna a primeira
get_row([_|Rows], Row, Result) :-
    Row > 0, 
    Row1 is Row - 1, 
    get_row(Rows, Row1, Result).

%obtem uma coluna da matriz
get_column([], _, []).
get_column([Row|Rows], Col, [Value|ColVals]) :-
    get_value_row(Row, Col, Value),  
    get_column(Rows, Col, ColVals).

%  obtem valores do quadrado 3×3 
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

% substitui um zero por um valor
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
