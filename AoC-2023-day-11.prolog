
:- module('AoC-2023-day-11', [solve/2, case/1, test_case/1, input/2]).
:- dynamic expansion_rate/1.

%%%%% 第十一日：星空觀測
%%
%% 此地的精靈在瞭望塔上觀測星空，並計算宇宙擴展效應。
%%
%% 第一題：問凡任一行列沒有星系佔據，則此行可擴展為兩行，此列可擴展為兩列；
%% 在此宇宙擴展下，加總全部任二星系的距離。
%%
%% 第二題：設宇宙擴展時間較長，使空行列擴展為 1_000_000 倍，問星系加總值。


test_case("...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....").

case(Case) :-
    'AoC-2023-_':input(11, 1, Case).


solve(test_case_1, Result) :-
    abolish(expansion_rate/1), asserta(expansion_rate(1)),
    test_case(C), input(C, Result).

solve(test_case_2, Result) :-
    abolish(expansion_rate/1), asserta(expansion_rate(9)),
    test_case(C), input(C, Result).
solve(test_case_3, Result) :-
    abolish(expansion_rate/1), asserta(expansion_rate(99)),
    test_case(C), input(C, Result).

solve(1, Result) :-
    abolish(expansion_rate/1), asserta(expansion_rate(1)),
    case(C), input(C, Result).

solve(2, Result) :-
    abolish(expansion_rate/1), asserta(expansion_rate(999_999)),
    case(C), input(C, Result).


input(Input, Result) :-
    split_string(Input, "\n", "\n", Input0),
    findall(L, (member(E, Input0),
                string_chars(E, L)), R0),
    ( indexed(0, R0, List0),
      findall(L, (member(((_,J),E), List0),
                  indexed(J, E, L0),
                  findall((I,K), (member((I,K),L0),K\='.'), L)), List2),
      append(List2, List1)
    ),
    ( empty_cords(R0, 1, Ehs0, Evs0),
      expansion_step(Ehs0, Ehs),
      expansion_step(Evs0, Evs),
      reverse(Ehs, She),
      reverse(Evs, Sve)
    ),
    expansion(List1, Sve, She, Expansion),
    total_length(Expansion, N0), N is N0,
    Result = (List1, Ehs, Evs, Expansion, N).


indexed(Prefix, List, Indexed) :-
    length(List, N),
    findall((Prefix,I), between(1,N,I), Indices),
    maplist([A,B,(A,B)]>>true, Indices, List, Indexed).


empty_cords([], _, [], []).
empty_cords([Line|Rest], Y, H_lines, V_lines) :-
    Y2 is Y+1,
    empty_cords(Rest, Y2, H0, V0),
    ( empty_line(Line)
    -> H_lines = [Y|H0]
    ; H_lines = H0
    ),
    ( V0 == []
    -> length(Line, N),
       findall(B, between(1,N,B), Is),
       maplist([A,B,(A,B)]>>true, Is, Line, M),
       findall(I, member((I,'.'), M), V2)
    ; V2 = V0
    ),
    findall(E, (member(E, V2),
                nth1(E, Line, '.')), V_lines).

empty_line(Line) :-
    forall(member(E, Line),
           E == '.').


expansion_step(List, Expansion_Steps) :-
    expansion_step(List, [], Expansion_Steps0),
    maplist([A,B,(A,B)]>>true, List, Expansion_Steps0, Expansion_Steps).
expansion_step([], Acc, Result) :-
    reverse(Acc, Result).
expansion_step([_|L], [], Result) :-
    expansion_rate(Rt),
    expansion_step(L, [plus(Rt)], Result).
expansion_step([_|L], [plus(N)|R], Result) :-
    expansion_rate(Rt),
    N2 is N+Rt,
    expansion_step(L, [plus(N2),plus(N)|R], Result).
% :- expansion_step([3,6,9], R), !.
% R = [(3, plus(1)), (6, plus(2)), (9, plus(3))].


expansion([], _, _, []).
expansion([((Y,X),T)|Rest], H_steps, V_steps, [((Y2,X2),T)|Result]) :-
    ( once((member((I,X_add),H_steps), X>I, !; X_add=plus(0))),
      once((member((J,Y_add),V_steps), Y>J, !; Y_add=plus(0))),
      call(Y_add, Y, Y2),
      call(X_add, X, X2)
    ),
    expansion(Rest, H_steps, V_steps, Result).

path_length(((A,B),_), ((X,Y),_), N) :-
    N is abs(A-X) + abs(B-Y).

total_length([], 0).
total_length([H|T], N) :-
    N = N0 + N2,
    findall(M, (member(T0, T),
                path_length(H, T0, M)), M),
    sum_list(M, N0),
    total_length(T, N2).
