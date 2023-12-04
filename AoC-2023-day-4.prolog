
:- module('AoC-2023-day-4', [solve/2]).
:- import('AoC-2023-day-_').

%%%%%%% 第四日：突然開始岔題了，遇到攔路的刮刮卡愛好者
%%
%% 說有一疊刮刮卡，豎槓左邊是兌獎號碼，右邊是所持號碼。
%%
%% 第一題問每一張中 N 個號碼，就給點數 Point = 2^(N-1) ；若沒中，得 0 點。
%%
%% 第二題問若規則為第 N 張刮刮卡得 k 點，則給予由第 N_1, N_2 ...
%% 到 N_k 刮刮卡各一張。

test_case("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11").



solve(test_case_1, Point) :-
    test_case(Case), split_string(Case, "\n:", "", Games),
    point(Games, 0, Point).

solve(1, Point) :-
    input(4, 1, Input), split_string(Input, "\n:", "", Games),
    point(Games, 0, Point).

solve(test_case_2, Count) :-
    test_case(Case), split_string(Case, "\n:", "", Games),
    scratchcards(Games, [], Cards),
    length(Cards, Count).

solve(2, Count) :-
    input(4, 1, Input), split_string(Input, "\n:", "", Games),
    scratchcards(Games, [], Cards),
    length(Cards, Count).



point([], Acc, Acc).
point([_Game_head,Game|Rest], Acc, Point) :-
    ( split_string(Game, "|", "", [Winning_part0,My_part0]),
      split_string(Winning_part0, " ", " ", Winning_part),
      split_string(My_part0, " ", " ", My_part),
      intersection(Winning_part, My_part, Win),
      length(Win, N),
      ( N =:= 0, !,
        Acc2 = Acc
      ; Acc2 is Acc + 2^(N-1)
      )
    ),
    point(Rest, Acc2, Point).


scratchcards([], Acc, Acc).
scratchcards([Head,Game|Rest], Acc, Cards) :-
    ( split_string(Head, " ", " ", ["Card",C0]),
      number_string(C, C0)
    ),
    ( findall(X, (member(X, Acc), X =:= C), Cs),
      length(Cs, Times0),
      Times is Times0+1
    ),
    ( split_string(Game, "|", "", [Winning_part0,My_part0]),
      split_string(Winning_part0, " ", " ", Winning_part),
      split_string(My_part0, " ", " ", My_part),
      intersection(Winning_part, My_part, Matches),
      length(Matches, N),
      From is C+1, To is C+N,
      findall(W, between(From, To, W), Win0),
      findall(W, (between(1, Times, _),
                  W = Win0), Win_0),
      append([[C]|Win_0], Win)
    ),
    append(Win, Acc, Acc2),
    scratchcards(Rest, Acc2, Cards).
