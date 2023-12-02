
:- module('AoC-2023-day-1', [proc/2]).

%% 投石機器
%%
%% 話說投石機器的參數表，被年輕氣盛的精靈作業員給改了，
%% 不容易讀懂。
%%
%% 例如，以下為參數表
%%
%% "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"
%%
%% 每行開頭第一個數字與最後一個數字都要讀入，則依序讀出
%%
%% 12 38 15 77
%%
%% 加總為 142

%%%%%%%%%%%%%%%%%%% 解法
%% first_read: 表示該行第一個數字已經讀入；
%%
%% Acc_10: 表示十位數累計值；
%%
%% Acc: 表示個位數累計值；
%%
%% Num_10: 表示該行第一個數字；
%%
%% Num: 表示該行最後一個數字。
%%
%% 解法為一套自動機，包括以上五個變數。
%%
%% proc 走訪輸入 List 並運用以上變數，方式恰如
%% 一般累積變數的技巧。

proc(Input, Result) :-
    string(Input), !,
    string_codes(Input, Input_0),
    proc(Input_0, nope, 48, 48, 0, 0, R_10, R),
    Result is (R_10)*10 + R.


proc([], _First_read, Num_10, Num, Acc_10, Acc, Acc_10+Num_10_2, Acc+Num_2) :- !,
    Num_10_2 is Num_10-48,
    Num_2 is Num-48.
proc([10|T], _, Num_10, Num, Acc_10, Acc, R_10, R) :- !,
    Num_10_2 is Num_10-48,
    Num_2 is Num-48,
    proc(T, not_first_read, 48, 48, Acc_10+Num_10_2, Acc+Num_2, R_10, R).

%%%%%%%%%% 第二段：處理英文數字拼法
proc([A,B,C,D|T], ReadState, Num_10, _, Acc_10, Acc, R_10, R) :-
    string_codes("zero", [A,B,C,D]), !,
    ( ReadState = first_read, !, V = Num_10; V = 48 ),
    proc([B,C,D|T], first_read, V, 48, Acc_10, Acc, R_10, R).
proc([A,B,C|T], ReadState, Num_10, _, Acc_10, Acc, R_10, R) :-
    string_codes("one", [A,B,C]), !,
    ( ReadState = first_read, !, V = Num_10; V = 49 ),
    proc([B,C|T], first_read, V, 49, Acc_10, Acc, R_10, R).
proc([A,B,C|T], ReadState, Num_10, _, Acc_10, Acc, R_10, R) :-
    string_codes("two", [A,B,C]), !,
    ( ReadState = first_read, !, V = Num_10; V = 50 ),
    proc([B,C|T], first_read, V, 50, Acc_10, Acc, R_10, R).
proc([A,B,C,D,E|T], ReadState, Num_10, _, Acc_10, Acc, R_10, R) :-
    string_codes("three", [A,B,C,D,E]), !,
    ( ReadState = first_read, !, V = Num_10; V = 51 ),
    proc([B,C,D,E|T], first_read, V, 51, Acc_10, Acc, R_10, R).
proc([A,B,C,D|T], ReadState, Num_10, _, Acc_10, Acc, R_10, R) :-
    string_codes("four", [A,B,C,D]), !,
    ( ReadState = first_read, !, V = Num_10; V = 52 ),
    proc([B,C,D|T], first_read, V, 52, Acc_10, Acc, R_10, R).
proc([A,B,C,D|T], ReadState, Num_10, _, Acc_10, Acc, R_10, R) :-
    string_codes("five", [A,B,C,D]), !,
    ( ReadState = first_read, !, V = Num_10; V = 53 ),
    proc([B,C,D|T], first_read, V, 53, Acc_10, Acc, R_10, R).
proc([A,B,C|T], ReadState, Num_10, _, Acc_10, Acc, R_10, R) :-
    string_codes("six", [A,B,C]), !,
    ( ReadState = first_read, !, V = Num_10; V = 54 ),
    proc([B,C|T], first_read, V, 54, Acc_10, Acc, R_10, R).
proc([A,B,C,D,E|T], ReadState, Num_10, _, Acc_10, Acc, R_10, R) :-
    string_codes("seven", [A,B,C,D,E]), !,
    ( ReadState = first_read, !, V = Num_10; V = 55 ),
    proc([B,C,D,E|T], first_read, V, 55, Acc_10, Acc, R_10, R).
proc([A,B,C,D,E|T], ReadState, Num_10, _, Acc_10, Acc, R_10, R) :-
    string_codes("eight", [A,B,C,D,E]), !,
    ( ReadState = first_read, !, V = Num_10; V = 56 ),
    proc([B,C,D,E|T], first_read, V, 56, Acc_10, Acc, R_10, R).
proc([A,B,C,D|T], ReadState, Num_10, _, Acc_10, Acc, R_10, R) :-
    string_codes("nine", [A,B,C,D]), !,
    ( ReadState = first_read, !, V = Num_10; V = 57 ),
    proc([B,C,D|T], first_read, V, 57, Acc_10, Acc, R_10, R).
%%%%%%%%%% 第二段：處理英文數字拼法 %%%%%%%%%%%%%%%

proc([H|T], first_read, Num_10, _, Acc_10, Acc, R_10, R) :-
    between(49,58,H), !,
    proc(T, first_read, Num_10, H, Acc_10, Acc, R_10, R).
proc([H|T], _, _, _, Acc_10, Acc, R_10, R) :-
    between(49,58,H), !,
    proc(T, first_read, H, H, Acc_10, Acc, R_10, R).

proc([_|T], ReadState, Num_10, Num, Acc_10, Acc, R_10, R) :-
    proc(T, ReadState, Num_10, Num, Acc_10, Acc, R_10, R).

%%%%%%%%%%%%%% 用法
%% ?- string_codes("1abc2
%% |    pqr3stu8vwx
%% |    a1b2c3d4e5f
%% |    treb7uchet", Codes),
%% |    proc(Codes, nope, 0, 0, 0, 0, R_10, R),
%% |    N is R_10*10+R).
%% Codes = [49, 97, 98, 99, 50, 10, 112, 113, 114|...],
%% R_10 = 0+1+3+1+7,
%% R = 0+2+8+5+7,
%% N = 142.

%%%%%%%%%%%%% 檔案輸入
%% input(1, 1, Input) 適用於單刷式處理 (one-pass scanning) 。
%%
%% proc/3 通用於 Part I 與 Part II ；
%% 但當用於 Part I 時，要將 proc/3 中段處理 [A,B,C,D,E|T] 處遮蔽。

test_case(1, "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet").

test_case(2, "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen").
