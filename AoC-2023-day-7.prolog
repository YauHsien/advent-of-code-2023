
:- module('AoC-2023-day-7', [solve/2, test_case/1, case/1, input/2, type/2]).
:- import('AoC-2023-_').

%%%%  第七日：打駱駝牌
%%
%% 駱駝牌的牌號大小有 A K Q J T 9 8 7 6 5 4 3 2
%%
%% 牌組強度有 Five of a kind, Four of a kind, Full house,
%% Three of a kind, Two pair, One pair, High card 等等。
%%
%% 問一手牌五張，先依牌組強度排序：
%% 1. One pair; 2. Two pair; 3. Three of a kind;
%% 4. Full house; 5. Four of a kind; 6. Five of a kind
%% 當兩手牌牌組強度相等時，再依牌順序，以牌號大小排序。
%%
%% 一套手牌遊戲的例子：
%% 32T3K 765
%% T55J5 684
%% KK677 28
%% KTJJT 220
%% QQQJA 483
%% 有五手牌，每一手右邊為該手牌的分數。
%% 依規則，這套遊戲手牌順序依序為 1 4 3 2 5 ，
%% 於是總成績為 765x1 + 684x4 + 28x3 + 220x2 + 483x5 = 6440
%%
%% 第一題問輸入檔的總成績。
%%
%%%%%%%%% 第二題
%%
%% 改牌號大小為 A K Q T 9 8 7 6 5 4 3 2 J
%%
%% J 是丑角牌，可以假裝為其他任何一個牌號。
%% 在一手牌裡，丑角牌 J 要盡量變另一個牌號，讓手牌強度最強。
%%
%% 但是，之後在算牌號時， J 還是 J ，比 2 小，最弱的牌。
%% 例如 JKKK2 和 QQQQ2 手牌強度相同，但 J 比 Q 小。
%%
%% 第二題也問輸入檔的總成績。


test_case(Case) :-
    Case0 = "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483",
    split_string(Case0, "\n ", "\n ", Case).

case(Case) :-
    'AoC-2023-_':input(7, 1, Case0),
    split_string(Case0, "\n ", "\n ", Case).


solve(test_case_1, Result) :-
    test_case(Case), input(Case, Input),
    predsort(card_sort, Input, Result0),
    rank_sum(Result0, (rank,1), (acc,0), Result).
% ~ 6440

solve(test_case_2, Result) :-
    test_case(Case), input_2(Case, Input),
    %predsort(card_sort, Input, Result0),
    findall((A,(7,high_card),B), member((A,(7,_),B),Input), R7_0),
    predsort(card_sort_2, R7_0, R7),
    findall((A,(6,one_pair),B), member((A,(6,_),B),Input), R6_0),
    predsort(card_sort_2, R6_0, R6),
    findall((A,(5,two_pair),B), member((A,(5,_),B),Input), R5_0),
    predsort(card_sort_2, R5_0, R5),
    findall((A,(4,three_of_a_kind),B), member((A,(4,_),B),Input), R4_0),
    predsort(card_sort_2, R4_0, R4),
    findall((A,(3,full_house),B), member((A,(3,_),B),Input), R3_0),
    predsort(card_sort_2, R3_0, R3),
    findall((A,(2,four_of_a_kind),B), member((A,(2,_),B),Input), R2_0),
    predsort(card_sort_2, R2_0, R2),
    findall((A,(1,five_of_a_kind),B), member((A,(1,_),B),Input), R1_0),
    predsort(card_sort_2, R1_0, R1),
    append([R7,R6,R5,R4,R3,R2,R1], Result0),
    rank_sum(Result0, (rank,1), (acc,0), Result).
% ~ 5905

solve(1, Result) :-
    case(Case), input(Case, Input),
    predsort(card_sort, Input, Result0),
    rank_sum(Result0, (rank,1), (acc,0), Result).

solve(2, Result) :-
    case(Case), input_2(Case, Input),
    findall((A,(7,high_card),B), member((A,(7,_),B),Input), R7_0),
    predsort(card_sort_2, R7_0, R7),
    findall((A,(6,one_pair),B), member((A,(6,_),B),Input), R6_0),
    predsort(card_sort_2, R6_0, R6),
    findall((A,(5,two_pair),B), member((A,(5,_),B),Input), R5_0),
    predsort(card_sort_2, R5_0, R5),
    findall((A,(4,three_of_a_kind),B), member((A,(4,_),B),Input), R4_0),
    predsort(card_sort_2, R4_0, R4),
    findall((A,(3,full_house),B), member((A,(3,_),B),Input), R3_0),
    predsort(card_sort_2, R3_0, R3),
    findall((A,(2,four_of_a_kind),B), member((A,(2,_),B),Input), R2_0),
    predsort(card_sort_2, R2_0, R2),
    findall((A,(1,five_of_a_kind),B), member((A,(1,_),B),Input), R1_0),
    predsort(card_sort_2, R1_0, R1),
    append([R7,R6,R5,R4,R3,R2,R1], Result0),
    rank_sum(Result0, (rank,1), (acc,0), Result).


input(Case, Input) :-
    (test_case(Case); case(Case)), !,
    input(Case, [], Input).
input([], Acc, Result) :-
    reverse(Acc, Result).
input([A,B|Rest], Acc, Result) :-
    string_chars(A, C),
    number_string(N, B),
    type(C, T), !,
    input(Rest, [(C,T,N)|Acc], Result).

input_2(Case, Input) :-
    (test_case(Case); case(Case)), !,
    input_2(Case, [], Input).
input_2([], Acc, Result) :-
    reverse(Acc, Result).
input_2([A,B|Rest], Acc, Result) :-
    string_chars(A, C),
    number_string(N, B),
    mimic(C, M),
    type(M, T), !,
    input_2(Rest, [(C,T,N)|Acc], Result).

type([A,A,A,A,A], (1,five_of_a_kind)).
type(Hand, (2,four_of_a_kind)) :- length(Hand, 5),
                                  ( msort(Hand, [A,B,B,B,B])
                                  ; msort(Hand, [A,A,A,A,B])
                                  ),
                                  A \= B.
type(Hand, (3,full_house)) :- length(Hand, 5),
                              ( msort(Hand, [A,A,B,B,B])
                              ; msort(Hand, [A,A,A,B,B])
                              ),
                              A \= B.
type(Hand, (4,three_of_a_kind)) :- length(Hand, 5),
                                   ( msort(Hand, [A,A,A,B,C])
                                   ; msort(Hand, [B,A,A,A,C])
                                   ; msort(Hand, [B,C,A,A,A])
                                   ),
                                   A \= B, B \= C.
type(Hand, (5,two_pair)) :- length(Hand, 5),
                            ( msort(Hand, [A,B,B,C,C])
                            ; msort(Hand, [A,A,B,C,C])
                            ; msort(Hand, [A,A,B,B,C])
                            ),
                            A \= B, B \= C.
type(Hand, (6,one_pair)) :- length(Hand, 5),
                            sort(Hand, Sorted), length(Sorted, 4).
type(Hand, (7,high_card)) :- sort(Hand, Sorted),
                             length(Sorted, 5).

strength('A', 14).
strength('K', 13).
strength('Q', 12).
strength('J', 11).
strength('T', 10).
strength(A, N) :- atom(A), !,
                  atom_number(A, N), between(2, 9, N).
strength(List, Strength) :-
    is_list(List), !,
    findall(S, (member(E,List),strength(E,S)), Strength).


mimic(['J','J','J','J','J'], ['A','A','A','A','A']) :- !.
mimic(List, Mimic) :-
    is_list(List),
    once(member('J', List)), !,
    subtract(List, ['J'], Rest),
    sort(Rest, Rest0),
    findall((T,M), (member(E, Rest0),
                    replace(List, 'J', E, M),
                    type(M, T)), S),
    sort(S, Sorted),
    [(Strength,_Mimic)|_] = Sorted,
    findall(E, member((Strength,E),Sorted), Sorted_2_0),
    predsort(str_sort, Sorted_2_0, Sorted_2),
    %reverse(Sorted_2, [Mimic|_]),
    [Mimic|_] = Sorted_2_0,
    % 250994085
    % 250348531
    % 251268065 -
    % 253033085 ~ Bad ~ too high
    write('Mimic: '),write(List),write('->'),write(Mimic),
    write(' '),write(Strength),write('\n').
mimic(List, List) :-
    is_list(List), !.

replace([], _, _, []).
replace([J|L], J, X, [X|R]) :- !,
    replace(L, J, X, R).
replace([X|L], J, Y, [X|R]) :-
    replace(L, J, Y, R).

strength_2('A', 14).
strength_2('K', 13).
strength_2('Q', 12).
strength_2('J',  1).
strength_2('T', 10).
strength_2(A, N) :- atom(A), !,
                  atom_number(A, N), between(2, 9, N).
strength_2(List, Strength) :-
    is_list(List), !,
    findall(S, (member(E,List),strength_2(E,S)), Strength).

card_sort(<, (Hand,Type,_), (Hand2,Type2,_)) :-
    ( Type @> Type2, !
    ; Type == Type2,
      strength(Hand, St),
      strength(Hand2, St2),
      St @< St2
    ).
card_sort(>, (Hand,Type,_), (Hand2,Type2,_)) :-
    ( Type @< Type2, !
    ; Type == Type2,
      strength(Hand, St),
      strength(Hand2, St2),
      St @> St2
    ).
card_sort(=, (Hand,Type,_), (Hand2,Type2,_)) :-
    Type == Type2,
    ( msort(Hand,Sorted),
      msort(Hand2,Sorted)
    ).

card_sort_2(<, (Hand,Type,_), (Hand2,Type2,_)) :-
    ( Type @> Type2, !
    ; Type == Type2,
      strength_2(Hand, St),
      strength_2(Hand2, St2),
      St @< St2
    ).
card_sort_2(>, (Hand,Type,_), (Hand2,Type2,_)) :-
    ( Type @< Type2, !
    ; Type == Type2,
      strength_2(Hand, St),
      strength_2(Hand2, St2),
      St @> St2
    ).
card_sort_2(=, (Hand,Type,_), (Hand2,Type2,_)) :-
    Type == Type2,
    ( msort(Hand,Sorted),
      msort(Hand2,Sorted)
    ).

str_sort(<, Hand, Hand2) :-
    strength(Hand, St), strength(Hand2, St2),
    St @< St2.
str_sort(>, Hand, Hand2) :-
    strength(Hand, St), strength(Hand2, St2),
    St @> St2.
str_sort(=, Hand, Hand).

rank_sum([], (rank,_), (acc,Acc), Result) :-
    Result is Acc.
rank_sum([(_Hand,_Type,Point)|L], (rank,R), (acc,A), Result) :-
    R0 is R,
    rank_sum(L, (rank,R0+1), (acc,A+Point*R0), Result).

