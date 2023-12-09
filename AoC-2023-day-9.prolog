
:- module('AoC-2023-day-9', [solve/2, test_case/1, case/1, input/2]).
:- consult('AoC-2023-_.prolog').

%% 第九日：沙漠幻影
%%
%% 問從每一列數字紀錄，求梯度數差序列：例如
%%
%% 0 3 6 9 12 15
%%
%% 第一列間隔差額為
%%  3 3 3 3  3
%%
%% 二次間隔差額為
%%   0 0 0  0
%%
%% 於是可由差額階梯推論出下一個數字 18
%% 0 3 6 9 12 15 _18_
%%  3 3 3 3  3  _3_
%%   0 0 0  0 _0_
%%
%% 也能推出之前一個數字 -3
%% _3_ 0 3 6 9 12 15
%%  _3_ 3 3 3 3  3
%%   _0_ 0 0 0  0
%%
%% 第一題問將每列輸入數列推論出下一個數字並加總。
%% 第二題問將每列輸入數列推論出之前一個數字並加總。

test_case("0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45").

case(Case) :-
    'AoC-2023-_':input(9, 1, Case).

solve(test_case_1, Result) :-
    test_case(Case), input(Case, Input),
    findall(R, (member(L, Input),
                diff_lists(L, D),
                extrapolate_e(D, [R|_])), Result).
% Result=[18,28,68] 加總為 114

solve(test_case_2, Result) :-
    test_case(Case), input(Case, Input),
    findall(R, (member(L, Input),
                diff_lists(L, D),
                extrapolate_b(D, [R|_])), Result).
% Result=[-3,0,5] 加總為 2

solve(1, Result) :-
    case(Case), input(Case, Input),
    findall(R, (member(L, Input),
                diff_lists(L, D),
                extrapolate_e(D, [R|_])), Result).

solve(2, Result) :-
    case(Case), input(Case, Input),
    findall(R, (member(L, Input),
                diff_lists(L, D),
                extrapolate_b(D, [R|_])), Result).


input(Input, Result) :-
    split_string(Input, "\n", "\n", Input0),
    findall(N0, (member(S, Input0),
                 split_string(S, " ", "", S0),
                 findall(N, (member(E, S0),
                             number_string(N, E)), N0)), Result).


diff_list(List, Result) :-
    diff_list(List, [], Result).
diff_list([_], Acc, Result) :-
    reverse(Acc, Result).
diff_list([A,B|L], Acc, Result) :-
    C is B-A,
    diff_list([B|L], [C|Acc], Result).


diff_lists(List, Result) :-
    diff_lists(List, [], Result).
diff_lists(List, Acc, [List0,List|Acc]) :-
    diff_list(List, [], List0),
    forall(member(E,List0), E=:=0), !.
diff_lists(List, Acc, Result) :-
    diff_list(List, [], List2),
    diff_lists(List2, [List|Acc], Result).


extrapolate_e(List, Result) :-
    extrapolate_e(List, [0], Result).
extrapolate_e([], Acc, Acc).
extrapolate_e([List|Rest], [Diff|Acc], Result) :-
    reverse(List, [A|_]),
    Diff2 is A+Diff,
    extrapolate_e(Rest, [Diff2,Diff|Acc], Result).

extrapolate_b(List, Result) :-
    extrapolate_b(List, [0], Result).
extrapolate_b([], Acc, Acc).
extrapolate_b([[A|_]|Rest], [Diff|Acc], Result) :-
    Diff2 is A-Diff,
    extrapolate_b(Rest, [Diff2,Diff|Acc], Result).
