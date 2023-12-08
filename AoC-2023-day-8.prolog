
:- module('AoC-2023-day-8', [solve/2, case/1, test_case/1, input/2]).
:- import('AoC-2023-_').

%% 第八日：沙漠風暴
%%
%% 風暴來了。而你在駱駝背上看到有好幾份路徑指示文件。
%% 第一行指令可循環使用。
%%
%% 第一題問從 AAA 走到 ZZZ 要走幾步。
%%
%% 第二題問，如果有一種鬼步，可找到全部結尾是 A 的符號，同時依循指令逐步走，
%% 直到全部同時走的步數都走到結尾是 Z 的符號，要走幾步。

test_case("RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)").

test_case_2("LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)").

test_case_3("LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)").

case(Case) :-
    input(8, 1, Case).


solve(test_case_1, Steps) :-
    test_case(Case), input(Case, Commands),
    walk("AAA", "ZZZ", Commands, 0, Steps).

solve(test_case_2, Steps) :-
    test_case_2(Case), input(Case, Commands),
    walk("AAA", "ZZZ", Commands, 0, Steps).

solve(test_case_3, Steps) :-
    test_case_3(Case), input(Case, Commands),
    ghost_walk("A", "Z", Commands, 0, Steps).
% solve(test_case_3,N) 依序得 2, 3
% 接著求最小公倍數 Lcm is lcm(2,3)

solve(1, Steps) :-
    case(Case), input(Case, Commands),
    walk("AAA", "ZZZ", Commands, 0, Steps).

solve(2, Steps) :-
    case(Case), input(Case, Commands),
    ghost_walk("A", "Z", Commands, 0, Steps).
% solve(2,N) 依序所得結果，求 LCM 。

input(Input, Commands) :-
    split_string(Input, "\n", "\n", [Cmd0|Terms]),
    string_chars(Cmd0, Commands),
    abolish(transit/3),
    foreach((member(Term, Terms),
             split_string(Term, "(=, )", "(=, )", [A,B,C])
            ),
            assertz(transit(A, B, C))).

walk(Start, End, Commands, Acc, Steps) :-
    walk(Start, End, Commands, Commands, [], Acc, Steps).
walk(Start, End, Cmd0, [], Commands, Acc, Steps) :-
    walk(Start, End, Cmd0, Cmd0, Commands, Acc, Steps).
walk(Start, End, _, [C|_], _, Acc, Steps) :-
    ( C == 'L', transit(Start, End, _)
    ; C == 'R', transit(Start, _, End)
    ),
    Steps is Acc+1.
walk(Start, End, Cmd0, [C|Commands], Cmd2, Acc, Steps) :-
    ( C == 'L', transit(Start, Start0, _)
    ; C == 'R', transit(Start, _, Start0)
    ),
    Start0 \= End,
    walk(Start0, End, Cmd0, Commands, [C|Cmd2], Acc+1, Steps).

ghost_transit_by(Z, Start, Left, Right) :-
    transit(Start, Left, Right),
    string_concat(_, Z, Start).

ghost_walk(Start, End, Commands, Acc, Steps) :-
    ghost_walk(Start, End, Commands, Commands, [], Acc, Steps).
ghost_walk(Start, End, Cmd0, [], Commands, Acc, Steps) :-
    ghost_walk(Start, End, Cmd0, Cmd0, Commands, Acc, Steps).
ghost_walk(Start, End, _, [C|_], _, Acc, Steps) :-
    ( C == 'L', ghost_transit_by(Start, _, End0, _),
      string_concat(_, End, End0)
    ; C == 'R', ghost_transit_by(Start, _, _, End0),
      string_concat(_, End, End0)
    ),
    Steps is Acc+1.
ghost_walk(Start, End, Cmd0, [C|Commands], Cmd2, Acc, Steps) :-
    ( C == 'L', ghost_transit_by(Start, _, Start2, _)
    ; C == 'R', ghost_transit_by(Start, _, _, Start2)
    ),
    \+ string_concat(_, End, Start2),
    ghost_walk(Start2, End, Cmd0, Commands, [C|Cmd2], Acc+1, Steps).


list_lcm([], 1).
list_lcm([N|R], M) :-
    list_lcm(R, L),
    M is lcm(N, L).
