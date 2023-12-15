
:- module('AoC-2023-day-14', [solve/2, case/1, test_case/1, input/2]).
:- [library(clpfd)].
:- consult('AoC-2023-_.prolog').

%:- table map_sort/2, rotate_right_and_tilt/2, spin/3.
:- table spin/3.
:- table clpfd:transpose/2.


%% 第十四日：靠北支持度
%%
%% 你找到曲面反射鏡，是由許多鏡子排成的弧面，可聚集光線。
%% 但在曲面上有許多圓形石頭，所以要玩個像倉庫番的遊戲。
%%
%% 在曲面鏡周邊有些操作桿，能控制鏡面朝北、西、南、東之一方傾斜。
%%
%% 第一題問若將整個盤面向北傾斜，則靠北支持度，即彈珠堆疊位置乘載多少？
%%
%% 靠北支持度的算法，是從盤面北緣起，向南逐行計算；
%% 設盤面長度為 N ，則北緣第一排堆積的彈珠權重為 N ，第二排權重 (N - 1)，
%% 第三排權重 (N - 2)，依序算到南緣最後一排權重為 1 。
%%
%%%%%%% 第二題
%%
%% 你找到個「旋轉」按鈕，作用是照著北、西、南、東各方向依序傾斜一次。
%%
%% 每次依序朝著北、西、南、東傾斜一輪之後，可推算出盤面的靠北支持度改變了一些。
%%
%% 第二題想問：那麼，假如持續旋轉個 1,000,000,000 次呢？靠北支持度為多少？


test_case(
"O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....").

test_case_1(".....#....
....#...O#
...OO##...
.OO#......
.....OOO#.
.O#...O#.#
....O#....
......OOOO
#...O###..
#..OO#....").
test_case_2(".....#....
....#...O#
.....##...
..O#......
.....OOO#.
.O#...O#.#
....O#...O
.......OOO
#..OO###..
#.OOO#...O").
test_case_3(".....#....
....#...O#
.....##...
..O#......
.....OOO#.
.O#...O#.#
....O#...O
.......OOO
#...O###.O
#.OOO#...O").


case(Case) :-
    'AoC-2023-_':input(14, 1, Case).


solve(test_case_1, Result) :-
    member(P, [test_case,test_case_1,test_case_2,test_case_3]),
    call(P, Case), input(Case, Map),
    scan_northward(Map, R0), append(R0,R2), sum_list(R2,Result),
    writeln(R0).

solve((test_case_2,N), Result) :-
    test_case(Case), input(Case, Map),
    upside_down(Map, MapInit),
    spin(N, MapInit, M2),
    Result = M2.
%% 我做了以下確認：從第三搖開始就有循環例子 69,69,65,64,65,63,68
%% 所以循環數是 ( 2 + (7 x N - 3) , N > 0 ) ；
%% 不必真的搖 1_000_000_000 次，那太久了。
% ?- foreach((between(1,22,N),
%     once(solve((test_case_2,N),R)),
%     upside_down(R,U), scan(U,Result),
%     append(Result,RR),sum_list(RR,P)),(writeln((N,P)),flush_output)).
% Correct to: "'AoC-2023-day-14':upside_down(R,U)"? yes
% Correct to: "'AoC-2023-day-14':scan_northward(U,Result)"? yes
% 1,87
% 2,69
% 3,69
% 4,69
% 5,65
% 6,64
% 7,65
% 8,63
% 9,68
% 10,69
% 11,69
% 12,65
% 13,64
% 14,65
% 15,63
% 16,68
% 17,69
% 18,69
% 19,65
% 20,64
% 21,65
% 22,63
%% 推算搖 1_000_000_000 的位置是 ( 1_000_000_000 - 2 ) % 7 = 4 。
%% ( 1_000_000_000 - 2 ) ≡ ( 6 - 2 ) (mod 7)
%% 所以搖 1_000_000_000 的靠北支撐度為 64。


solve(1, Result) :-
    case(Case), input(Case, Map),
    scan_northward(Map, R0), append(R0,R2), sum_list(R2,Result).

solve((2,N), Result) :-
    case(Case), input(Case, Map),
    upside_down(Map, MapInit),
    spin(N, MapInit, M2),
    Result = M2.
% :- solve(2,R), upside_down(R,U), scan(U,Result), append(Result,RR),
%   sum_list(RR,P), writeln(P).
% ?- foreach((between(78,122,N),once(solve((2,N),R)),upside_down(R,U),
%   scan(U,Result),append(Result,RR),sum_list(RR,P)),(writeln((N,P)),
%   flush_output)).
% Correct to: "'AoC-2023-day-14':upside_down(R,U)"? yes
% Correct to: "'AoC-2023-day-14':scan(U,Result)"? yes
%  78,102446
%  79,102506
%  80,102556
%  81,102632
%  82,102723
%  83,102763
%  84,102791
%  85,102787
%  86,102796
%  87,102792
%  88,102785
%  89,102767
%  90,102761
%  91,102782
%  92,102808
%  93,102837 <---
%  94,102851
%  95,102834
%  96,102831
%  97,102829
%  98,102827
%  99,102828
% 100,102837 <---
% 101,102851
% 102,102834
% 103,102831
% 104,102829
% 105,102827
% 106,102828
% 107,102837 <---
% 108,102851
% 109,102834
% 110,102831
% 111,102829
% 112,102827
% 113,102828
% 114,102837 <---
% 115,102851
% 116,102834
% 117,102831
% 118,102829
% 119,102827
% 120,102828
% 121,102837 <---
% 122,102851
% true.
% :- N is 92 + (1_000_000_000-92) rem 7.
% N = 97.


input(Input, Map) :-
    split_string(Input, "\n", "\n", Input0),
    findall(C, (member(E, Input0),
                 string_chars(E,C)), Map).


count_n('#', (_,Acc), (_,Acc)).
count_n('O', (CountDown,Acc), (CountDown-1,[CountDown|Acc])).
count_n('.', Acc, Acc).
    
scan_northward(Map, Result) :-
    [Line|_] = Map,
    length(Line, N),
    once(findnsols(N,(N,[]),repeat,Acc)),
    scan_northward(Map, Acc, R0),
    findall(L, member((_,L),R0), Result).
scan_northward([], Acc, Acc).
scan_northward([Line|Map], Acc, Result) :-
    maplist(count_n, Line, Acc, Acc2),
    term_variables(Acc2, Vars),
    length(Map, N),
    maplist(=(N), Vars),
    scan_northward(Map, Acc2, Result).

count('#', (CountDown,Acc), (CountDown-1,Acc)).
count('O', (CountDown,Acc), (CountDown-1,[CountDown|Acc])).
count('.', (CountDown,Acc), (CountDown-1,Acc)).

scan(Map, Result) :-
    [Line|_] = Map, length(Line, N),
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
    


map_sort(Map, Result) :- findall(TT0, (member(S0, Map),
                                       string_chars(S1, S0),
                                       split_string(S1, "#", "", S2),
                                       findall(['#'|T2],
                                               (member(T0, S2),
                                                string_chars(T0, T1),
                                                msort(T1, T2)
                                               ),
                                               TT),
                                       append(TT, [_|TT0])
                                      ), Result).

rotate_right_and_tilt(Map, Result) :-
    transpose(Map, T),
    map_sort(T, SS),
    findall(R, (member(E,SS),reverse(E,R)), Result).

rotate_right(Map, Result) :-
    transpose(Map, T),
    findall(R, (member(E,T),reverse(E,R)), Result).

upside_down(Map, Result) :-
    reverse(Map, MapR),
    findall(R, (member(E,MapR),reverse(E,R)), Result).


spin(0, Map, Map).
spin(Times, Map, Result) :-
    Times > 0,
    rotate_right_and_tilt(Map, M1),
    rotate_right_and_tilt(M1, M2),
    rotate_right_and_tilt(M2, M3),
    rotate_right_and_tilt(M3, M4),
    T2 is Times-1,
    spin(T2, M4, Result).


vis(Map) :-
    foreach((member(C,Map),string_chars(S,C)),format("~p~n",[S])),
    write('\n').
