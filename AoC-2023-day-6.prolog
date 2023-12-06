
:- module('AoC-2023-day-6', [solve/2, test_case/1, case/1]).

%%%%% 第六日：船賽
%%
%% 你拿到一張競速表
%%
%% Time     7 15  30
%% Duration 9 40 200
%%
%% 遊戲規則說，競賽的玩具船上有一顆按鈕，一開始要按壓一段時間，
%% 之後放開按鈕後船獲得加速。
%%
%% 要打敗競速表上的第一場賽局：他花了時間 7 而行進距離 9 。
%% 你要贏他，可以有幾種方法：
%%
%% 1. 不按壓，而從頭到尾船沒有前進。
%% 2. 按壓 1 時間，放開後，獲得每秒行進 1 距離，於是，
%%    經過 6 時間之後，共耗 7 時間但行進 6 距離；沒有超越紀錄。
%% 3. 按壓 2 時間，放開後，獲得每秒行進 2 距離，於是，
%%    經過 5 時間之後，共耗 10 時間且行進 10 距離；可超越紀錄。
%% ...
%% 8. 按壓 7 時間，放開後，已耗 7 時間但船尚未行進；沒有超越紀錄。
%%
%% 對上述競速表三場比賽，你能盤算出依序分別有 4, 8, 9 種可超越紀錄的辦法。
%% 於是將 4x8x9 乘為 288 。
%%
%% 第一題，競速表為
%%
%% Time       41   96   88   94
%% Duration  214 1789 1127 1055
%%
%% 求如同上述乘積。
%%
%%%%%%%% 第二題：原來競速表有印刷跑版呢！
%%
%% 如最前例子，競速表其實只有一場比賽
%%
%% Time      71530
%% Duration 940200
%%
%% 求如同上述乘積。
%%


test_case([(7, 9), (15, 40), (30, 200)]).
test_case_2(71530, 940200).

case([(41, 214), (96, 1789), (88, 1127), (94, 1055)]).
case_2(41968894, 214178911271055).

solve(test_case_1, M) :-
    test_case(Case),
    findall(C, (member((A,B), Case),
                findall(_,beat_case(A, B, _),K),
                length(K, C)), L),
    multiply(L, 1, M).

solve(test_case_2, M) :-
    test_case_2(N, Limit),
    start_to_beat(0, N, Limit, (A,_)),
    M is N-2*A+1.

solve(1, M) :-
    case(Case),
    findall(C, (member((A,B), Case),
                findall(_,beat_case(A, B, C),K),
                length(K, C)), L),
    multiply(L, 1, M).

solve(2, M) :-
    case_2(N, Limit),
    start_to_beat(0, N, Limit, (A,_)),
    M is N-2*A+1.



start_to_beat(K, N, Limit, (K,R)) :-
    K =< N,
    R is N-K,
    K*R > Limit, !.
start_to_beat(K, N, Limit, R) :-
    K =< N,
    K2 is K+1,
    start_to_beat(K2, N, Limit, R).


beat_case(N, M, (A,B)) :-
    split_num(N, A, B),
    A*B > M.

split_num(N, A, B) :-
    ground(N), !,
    between(0, N, A), between(0, N, B),
    N =:= A+B.

multiply([], Acc, M) :-
    M is Acc.
multiply([N|L], Acc, M) :-
    multiply(L, N*Acc, M).
