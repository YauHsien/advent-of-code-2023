
:- module('AoC-2023-day-21', [solve/2, case/1, test_case/1, input/2]).
:- consult('AoC-2023-_.prolog').

%% 第二十一日：幫園丁算算今天做了多少塊土地
%%
%% 花園有一些石頭障礙物圍起一些空地，空地分為一格一格的。
%% 你要幫忙算算，園丁從花園中間的起點往外走，走多少步之後，算是整理得多少塊空地。
%%
%% 算法為：
%% 一、從標記 S 開始踏著，算第一步；從第一步之後還要走幾步，假設 6 步，算是一天的工作量。
%% 二、由 S 開始走第一步，是同時朝著上、下、左、右與標記 S 空格相連的格子跨出；所以，
%%     由 S 開始走第一步，佔了 4 格空地。
%% 三、由 S 開始走第二步，是將剛才佔的 4 隔空地，分別同時朝該格子的上、下、左、右與該格
%%     相連的格子跨出；
%%
%% 如果是全空地，從 S 開始走第一步與走第二步的踩踏方式變化如下：
%%
%% .....    ..O..
%% ..O..    .O.O.
%% .OSO. => O.O.O
%% ..O..    .O.O.
%% .....    ..O..
%%
%% 也就是，簡化問題來講：
%% 開始踩著標記 S 空格；第一步取 S 外圍一圈 4 格；第二步取第一步外圍一圈 8 格，再加上
%% 第一步已有的格子，再加上標記 S 格子，共 13 格。
%%
%% 第一題問輸入檔裡的地圖表示，照著上述走法，由標記 S 開始走 64 步，能走過多少塊土地。
%%
%%%%%%%%% 第二題
%%
%% 設這片土地為循環世界，可視為由標記 S 土地向外無限度延伸。在這片土地上，園丁希望知道
%% 由標記 S 的空格向外走 26,501,365 步，總共佔多少塊土地。


case(Case) :-
    'AoC-2023-_':input(21, 1, Case).

test_case("...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........").

solve(test_case_1, Result) :-
    test_case(Case), input(Case, (H,W,Data)),
    once(member((I0,J0,'S'),Data)),
    Steps = 6, Steps0 = 3,
    Map = (H,W,Data),
    step(Steps0, Map, (I0,J0), Steps, (H,W,Data2)),
    draw_map(H, W, Data2),
    findall((I,J,'O'), (member((I,J,'O'),Data2);member((I,J,'S'),Data2)), Result0),
    Result = (H,W,Result0),
    true.
% :- solve(1,(H,W,R)), length(R,N).
% N = 16.

solve(1, Result) :-
    case(Case), input(Case, (H,W,Data)),
    once(member((I0,J0,'S'),Data)),
    Steps = 64, Steps0 = 32,
    Map = (H,W,Data),
    step(Steps0, Map, (I0,J0), Steps, (H,W,Data2)),
    draw_map(H, W, Data2),
    findall((I,J,'O'), (member((I,J,'O'),Data2);member((I,J,'S'),Data2)), Result0),
    Result = (H,W,Result0),
    true.
% :- solve(1,(H,W,R)), length(R,N).

solve((2,Step), Result) :-
    case(Case), input(Case, (H,W,Data)),
    once(member((I0,J0,'S'),Data)),
    Steps = 64, Steps0 = 32,
    Map = (H,W,Data),
    step(Steps0, Map, (I0,J0), Steps, (H,W,Data2)),
    draw_map(H, W, Data2),
    findall((I,J,'O'), (member((I,J,'O'),Data2);member((I,J,'S'),Data2)), Result0),
    Result = (H,W,Result0),
    true.

input(Input, (H,W,Data)) :-
    split_string(Input, "\n", "\n", Input0),
    length(Input0, H),
    [Line0|_] = Input0, string_length(Line0, W),
    once(findnsols(H,I0,(repeat,between(1,H,I0)),Js)),
    maplist([A,B,(A,B)]>>true, Js, Input0, Input2),
    findall(D0,
            (member((I0,L0), Input2),
             string_chars(L0, C0),
             once(findnsols(W,(I0,J2),(repeat,between(1,W,J2)),I2)),
             maplist([(A,B),C,(A,B,C)]>>true, I2, C0, D0)), D0),
    append(D0, Data).

step(N, Map, _, _, Map) :- N =:= 0, %write('\n'),
                           true.
step(N, Map, S, M, Map2) :- N > 0,
                            step(Map, S, M, Map0),
                            %Map0=(_,_,M0),length(M0,L), write(N), write(:), write(L), write(' '), flush_output,
                            step(N-1, Map0, S, M, Map2).

step((H,W,Map), S, M, (H,W,Map2)) :-
    findall(R, (between(1,H,I), between(1,W,J),
                distance((I,J), S, Dist), Dist =< M,
                ( member((I,J,D),Map), member(D,[#,'O','S']) ->
                  R = (I,J,D)
                ; once(( next_step((I,J), (Vi,Vj), (I2,J2)),
                         ( \+ member((Vi,Vj,_),Map) -> true
                         ; member((Vi,Vj,Dv),Map), Dv \= # ),
                         ( member((I2,J2,'O'),Map) -> true
                         ; member((I2,J2,'S'),Map) )
                       )) ->
                  R = (I,J,'O')
                )), Map2).

next_step((X,Y), (X,Vy), (X,K)) :- Vy is Y-1, K is Y-2.
next_step((X,Y), (Vx,Vy), (J,K)) :-
    ( X = Vx, Vy is Y-1; Vx is X-1, Y = Vy ), J is X-1, K is Y-1.
next_step((X,Y), (Vx,Y), (J,Y)) :- Vx is X-1, J is X-2.
next_step((X,Y), (Vx,Vy), (J,K)) :-
    ( X = Vx, Vy is Y-1; Vx is X+1, Y = Vy ), J is X+1, K is Y-1.
next_step((X,Y), (X,Vy), (X,K)) :- Vy is Y+1, K is Y+2.
next_step((X,Y), (Vx,Vy), (J,K)) :-
    ( X = Vx, Vy is Y+1; Vx is X-1, Vy = Y ), J is X-1, K is Y+1.
next_step((X,Y), (Vx,Y), (J,Y)) :- Vx is X+1, J is X+2.
next_step((X,Y), (Vx,Vy), (J,K)) :-
    ( X = Vx, Vy is Y+1; Vx is X+1, Vy = Y ), J is X+1, K is Y+1.

%clear_between((X,Y), (J,K), Data) :-
%    clear_between(X, (Y,K)), clear_between((X,J), Y).

neighbor((X,Y), (J,K)) :- X = J, K is Y-1.
neighbor((X,Y), (J,K)) :- J is X-1, Y = K.
neighbor((X,Y), (J,K)) :- X = J, K is Y+1.
neighbor((X,Y), (J,K)) :- J is X+1, Y = K.

open_plot((I,J,K), Data) :-
    member((I,J,'.'), Data),
    \+ forall(neighbor((I,J),(X,Y)), member((X,Y,'#'),Data)).

distance((X,Y), (J,K), D) :- D is abs(X-J) + abs(Y-K).

draw_map(H, W, Data) :-
    writeln(Data),
    foreach(between(1,H,I),
            (foreach((between(1,W,J), (member((I,J,D),Data) -> true; D = '.')),
                     write(D)),
             write('\n')
            )).
factors(1,[1]):- true, !.
factors(X,[Factor1|T]):- X > 0,
                         between(2,X,Factor1), 
                         NewX is X // Factor1, (X mod Factor1) =:= 0,
                         factors(NewX,T), !.
