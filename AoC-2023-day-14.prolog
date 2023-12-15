
:- module('AoC-2023-day-14', [solve/2, case/1, test_case/1, input/2]).
:- consult('AoC-2023-_.prolog').


%% 第十四日：彈珠戲
%%
%% 第一題問若將整個盤面向上傾斜，則彈珠堆疊在哪些位置。


test_case("O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....").

case(Case) :-
    'AoC-2023-_':input(14, 1, Case).


solve(test_case_1, Result) :-
    test_case(Case), input(Case, Map),
    scan(Map, R0), append(R0,R2), sum_list(R2,Result).

solve(1, Result) :-
    case(Case), input(Case, Map),
    scan(Map, R0), append(R0,R2), sum_list(R2,Result).

solve(2, Result) :-
    Result.



input(Input, Map) :-
    split_string(Input, "\n", "\n", Input0),
    findall(C, (member(E, Input0),
                 string_chars(E,C)), Map).


count('#', (_,Acc), (_,Acc)).
count('O', (CountDown,Acc), (CountDown-1,[CountDown|Acc])).
count('.', Acc, Acc).
    
scan(Map, Result) :-
    [Line|_] = Map,
    length(Line, N),
    once(findnsols(N,(N,[]),repeat,Acc)),
    scan(Map, Acc, R0),
    findall(L, member((_,L),R0), Result).
scan([], Acc, Acc).
scan([Line|Map], Acc, Result) :-
    maplist(count, Line, Acc, Acc2),
    term_variables(Acc2, Vars),
    length(Map, N),
    maplist(=(N), Vars),
    scan(Map, Acc2, Result).
