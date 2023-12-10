
:- module('AoC-2023-day-10', [solve/2, test_case/1, case/1, input/2]).
:- consult('AoC-2023-_.prolog').
:- set_prolog_flag(encoding, utf8).

%% 第十日：水管迷宮
%%
%% 話說這時你乘滑翔翼順著熱流上升，但經過冰冷的金屬空島時，
%% 氣溫已經不足以提供熱流，你便降在島上。
%% 你眼角餘光發現有個東西跑掉並鑽進一個管子，你便跟上去觀察。
%% 然後聽到一陣管路的聲響，聽起來是某個小動物在地下管路裡跑來跑去。
%% 看起來有個環形管道，那隻小動物在繞著管子鑽。
%%
%% 輸入檔是那些管路的平面圖。
%% 有六個符號 - | J L 7 F 表示兩種直管與四種彎管，
%% 全圖由這六種管子佈局。
%% 輸入檔空餘的部分，以點符號表示。
%%
%% 第一題：在平面圖上有個符號 S 標示為環狀管路的起始點，
%% 你同時朝二方向巡查管路，問巡到第幾節管子時，二方向聚合為同一截管子？
%% 設巡 S 位置管子為第 0 號，依序巡每一節數 1, 2, 3, ...
%% 則當巡到聚合處時，是第幾數？
%%
%%%%%%%%%%%%%%%%%% 第二題
%%
%% 你觀察了好久才想到，那隻小動物潛伏在環狀管子的包圍圈裡面。
%%
%% 所謂包圍圈定義，雖然管子可以繞出不規則形狀，但仍可分別出包圍圈以外
%% 與包圍圈以內。
%%
%% 第二題：問環狀管道包圍的內部區域有幾格。

test_case("..F7.
.FJ|.
SJ.L7
|F--J
LJ...").

test_case_2("FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L").

case(Case) :-
    input(10, 1, Case).

solve(test_case_1, Result) :-
    test_case(Case), input(Case, Input),
    S = (_,_,'S'), member(S, Input),
    findall(N2, (almost_circle_from([S],Input,R),
                 length(R, N),
                 N2 is (N+1) div 2), Result).
% 算出差不多的路徑長，然後，猜猜看。
% :- solve(test_case_1, R).
% R = [8,8,8,8,8,8];
% false.

solve(test_case_2, Result) :-
    test_case_2(Case), input(Case, Input),
    reverse(Input, [(H,W,_)|_]),
    S = (_,_,'S'), member(S, Input),
    [(R0,_)|_] = Result,
    findall((R,N2), (almost_circle_from([S],Input,R),
                     length(R, N),
                     N2 is (N+1) div 2), Result),
    foreach((between(1, H, J),
             between(1, W, I),
             ( member((J,I,'-'), R0)
             -> K = "▬"
             ; member((J,I,'|'), R0)
               -> K = "|"
             ; member((J,I,'L'), R0)
               -> K = "└"
             ; member((J,I,'7'), R0)
               -> K = "┐"
             ; member((J,I,'J'), R0)
               -> K = "┘"
             ; member((J,I,'F'), R0)
               -> K = "┌"
             ; K = '.'
             )
            ),
            (
                write(K),
                ( I =:= W,
                  write('\n')
                ; I =\= W
                )
            )
           ).
% 會將圖形版面以 Extented ASCII 印出來。

solve(1, Result) :-
    case(Case), input(Case, Input),
    S = (_,_,'S'), member(S, Input),
    findall(N2, (almost_circle_from([S],Input,R),
                length(R, N),
                N2 is (N+1) div 2), Result).
% 算出差不多的路徑長，然後，猜猜看。
% 這題要跑大約五分鐘。

solve(2, Result) :-
    case(Case), input(Case, Input),
    reverse(Input, [(H,W,_)|_]),
    S = (_,_,'S'), member(S, Input),
    [(_,R0)|_] = Result,
    findall((N2,R), (almost_circle_from([S],Input,R),
                     length(R, N),
                     N2 is (N+1) div 2), Result),
    foreach((between(1, H, J),
             between(1, W, I),
             ( member((J,I,'-'), R0)
             -> K = "▬"
             ; member((J,I,'|'), R0)
               -> K = "|"
             ; member((J,I,'L'), R0)
               -> K = "└"
             ; member((J,I,'7'), R0)
               -> K = "┐"
             ; member((J,I,'J'), R0)
               -> K = "┘"
             ; member((J,I,'F'), R0)
               -> K = "┌"
             ; K = "."
             )
            ),
            (
                write(K),
                ( I =:= W,
                  write('\n')
                ; I =\= W
                )
            )
           ).
% 第二題解，會將圖形版面以 Extented ASCII 印出來。
% 這題要跑大約五分鐘。


input(Input, Result) :-
    split_string(Input, "\n", "\n", Input0),
    length(Input0, N),
    findall(I, between(1, N, I), Index0),
    maplist([A,B,(A,B)]>>true, Index0, Input0, Input2),
    findall(R, (member((I,X), Input2),
                string_chars(X, C),
                length(C, N0),
                findall(K, between(1,N0,K), I2),
                once(findnsols(N0, I, repeat, I0)),
                maplist([A,B,C,(A,B,C)]>>true, I0, I2, C, R)), Result0),
    append(Result0, Result).


almost_circle_from([Start|Path], Set, Result) :-
    coalitions(Set, Start, Adjacent, Non_adjacent),
    ( Adjacent = [], !,
      Result = [Start|Path]
    ; member(Dst, Adjacent),
      almost_circle_from([Dst,Start|Path], Non_adjacent, Result)
    ).


conn_h('-', '-') :- !.
conn_h('-', 'J') :- !.
conn_h('-', '7') :- !.
conn_h('L', '-') :- !.
conn_h('L', 'J') :- !.
conn_h('L', '7') :- !.
conn_h('F', '-') :- !.
conn_h('F', 'J') :- !.
conn_h('F', '7') :- !.
conn_h('S', X) :-
    member(A, ['-','L','F','J','7']),
    conn_h(A, X).

conn_v('|', '|') :- !.
conn_v('7', '|') :- !.
conn_v('7', 'J') :- !.
conn_v('7', 'L') :- !.
conn_v('F', '|') :- !.
conn_v('F', 'J') :- !.
conn_v('F', 'L') :- !.
conn_v('|', 'L') :- !.
conn_v('|', 'J') :- !.
conn_v('S', X) :-
    member(A, ['|','7','F','L','J']),
    conn_v(A, X).

adjacent((I,X,A), (I,Y,B)) :- X-Y =:= 1, !, conn_h(B, A).
adjacent((I,X,A), (I,Y,B)) :- Y-X =:= 1, !, conn_h(A, B).
adjacent((X,J,A), (Y,J,B)) :- X-Y =:= 1, !, conn_v(B, A).
adjacent((X,J,A), (Y,J,B)) :- Y-X =:= 1, !, conn_v(A, B).

coalitions(Set, Term, Adjacent, Not_adjacent) :-
    findall(E, (member(E, Set),
                adjacent(Term, E)), Adjacent),
    subtract(Set, [Term|Adjacent], Not_adjacent).

cons_all(Head, Tails, Result) :-
    length(Tails, N),
    once(findnsols(N, Head, repeat, Heads)),
    maplist([A,B,[A|B]]>>true, Heads, Tails, Result).
