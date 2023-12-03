
:- module('AoC-2023-day-3', [solve/3, test_case/2]).



%% 第三日：圖譜搜尋
%%
%% 你要搭乘吊纜車下去水源地，但吊纜車系統有些零件缺損。
%% 精靈遞給你一張零件圖譜，要你根據圖示找到零件。
%% 要在圖譜裡找到與符號相接的料號，包括縱向相接、橫向相接、斜向相接。
%%
%% 將與符號相接的料號加總。
%%
%%%%%%%%%%%% 第二題
%%
%% 算錯了啊！
%% 真該算的是每個齒輪 * 接到二個料號，將二個料號相乘，
%% 然後將全部齒輪的料號乘積加總。
%%
%% 只相接一個料號的 * 不是齒輪。


indexed_list([], _, Acc, Result) :- !,
    reverse(Acc, Result).
indexed_list([Term|List], Index, Acc, Result) :-
    Index_2 is Index+1,
    indexed_list(List, Index_2, [(Index,Term)|Acc], Result).

tag_list([], _, []) :- !.
tag_list([Term|List], Tag, [(Tag,Term)|Result]) :-
    tag_list(List, Tag, Result).


symbols([], []).
symbols([(I,J,Symbol)|Rest], [(I,J,Symbol)|Result]) :-
    \+ atom_codes('.', [Symbol]),
    atom_codes('0', [D0]),
    atom_codes('9', [D9]),
    \+ between(D0, D9, Symbol), !,
    symbols(Rest, Result).
symbols([_|Rest], Result) :-
    symbols(Rest, Result).


gears([], []).
gears([(I,J,Gear)|Rest], [(I,J,Gear)|Result]) :-
    atom_codes('*', [Gear]), !,
    gears(Rest, Result).
gears([_|Rest], Result) :-
    gears(Rest, Result).


adjacent((I,J,T), (I2,J2,T)) :-
    ( I2 is I-1, J2 is J-1
    ; I2 is I-1, J2 is J
    ; I2 is I-1, J2 is J+1
    ; I2 is I,   J2 is J-1
    ; I2 is I,   J2 is J+1
    ; I2 is I+1, J2 is J-1
    ; I2 is I+1, J2 is J
    ; I2 is I+1, J2 is J+1
    ).


occupied((I,J,num(N)), (I,J,N)) :-
    N < 10, !.
occupied((I,J,num(N)), (I2,J2,N)) :-
    N < 100, !,
    ( I2 is I, J2 is J
    ; I2 is I, J2 is J+1
    ).
occupied((I,J,num(N)), (I2,J2,N)) :-
    N < 1000, !,
    ( I2 is I, J2 is J
    ; I2 is I, J2 is J+1
    ; I2 is I, J2 is J+2
    ).


%%%% For '..35..633..'
% ?- numbers([46,46,51,53,46,46,54,51,51,46,46], List).
% List = [(3, 35), (7, 633)]
numbers(List, Result) :-
    reverse(List, Rev),
    numbers(Rev, [], [], Result).

numbers([], [], Acc, Acc) :- !.
numbers([], Acc_0, Acc, Result) :-
    Acc_0 \= [], !,
    atom_codes(A, Acc_0), atom_number(A, Num),
    numbers([], [], [(1,num(Num))|Acc], Result).
numbers([N|Rest], Acc_0, Acc, Result) :-
    between(48, 57, N), !,
    numbers(Rest, [N|Acc_0], Acc, Result).
numbers([_|Rest], Acc_0, Acc, Result) :-
    Acc_0 \= [], !,
    atom_codes(A, Acc_0), atom_number(A, Num),
    length(Rest, L0), L2 is L0+2,
    numbers(Rest, [], [(L2,num(Num))|Acc], Result).
numbers([_|Rest], Acc_0, Acc, Result) :-
    numbers(Rest, Acc_0, Acc, Result).


test_case(1, [Raw,Result]) :-
    Case = "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..",
    split_string(Case, "\n", "\n", Case_0),
    findall(C, (member(E, Case_0),
                string_to_list(E, C)), Raw),
    indexed_list(Case_0, 1, [], Case_2),
    findall(R, (member((I,L), Case_2),
                string_to_list(L, R0),
                indexed_list(R0, 1, [], R2),
                tag_list(R2, I, R)), Result0),
    append(Result0, Result).


solve(test_case, Adj, Num) :-
    test_case(1, [Raw,Result]),
    symbols(Result, Sym),
    findall(A, (member(E, Sym),
                adjacent(E, A)), Adj),
    findall(N, (member(E, Raw),
                numbers(E, N)), Num_0),
    indexed_list(Num_0, 1, [], Num_2),
    findall(N, (member((I,L), Num_2),
                findall((I,E), member(E, L), N)), Num_3),
    append(Num_3, Num_4),
    adjacent(Num_4, Adj, Num).

solve(test_case_2, Adj, Num) :-
    test_case(1, [Raw,Result]),
    gears(Result, Sym),
    findall((A,E), (member(E, Sym),
                    adjacent(E, A)), Adj),
    findall(N, (member(E, Raw),
                numbers(E, N)), Num_0),
    indexed_list(Num_0, 1, [], Num_2),
    findall(N, (member((I,L), Num_2),
                findall((I,E), member(E, L), N)), Num_3),
    append(Num_3, Num_4),
    adjacent_2_gear(Num_4, Adj, Num_5),
    redundency(Num_5, Num_6),
    findall(A*B, member((_,A,B),Num_6), Num).

solve(1, Adj, Num) :-
    input(3, 1, Input),
    (
        split_string(Input, "\n", "\n", Case_0),
        findall(C, (member(E, Case_0),
                    string_to_list(E, C)), Raw),
        indexed_list(Case_0, 1, [], Case_2),
        findall(R, (member((I,L), Case_2),
                    string_to_list(L, R0),
                    indexed_list(R0, 1, [], R2),
                    tag_list(R2, I, R)), Result0),
        append(Result0, Result)
    ),
    symbols(Result, Sym),
    findall(A, (member(E, Sym),
                adjacent(E, A)), Adj),
    findall(N, (member(E, Raw),
                numbers(E, N)), Num_0),
    indexed_list(Num_0, 1, [], Num_2),
    findall(N, (member((I,L), Num_2),
                findall((I,E), member(E, L), N)), Num_3),
    append(Num_3, Num_4),
    adjacent(Num_4, Adj, Num).

solve(2, Adj, Num) :-
    input(3, 1, Input),
    (
        split_string(Input, "\n", "\n", Case_0),
        findall(C, (member(E, Case_0),
                    string_to_list(E, C)), Raw),
        indexed_list(Case_0, 1, [], Case_2),
        findall(R, (member((I,L), Case_2),
                    string_to_list(L, R0),
                    indexed_list(R0, 1, [], R2),
                    tag_list(R2, I, R)), Result0),
        append(Result0, Result)
    ),
    gears(Result, Sym),
    findall((A,E), (member(E, Sym),
                    adjacent(E, A)), Adj),
    findall(N, (member(E, Raw),
                numbers(E, N)), Num_0),
    indexed_list(Num_0, 1, [], Num_2),
    findall(N, (member((I,L), Num_2),
                findall((I,E), member(E, L), N)), Num_3),
    append(Num_3, Num_4),
    adjacent_2_gear(Num_4, Adj, Num_5),
    redundency(Num_5, Num_6),
    findall(A*B, member((_,A,B),Num_6), Num).

adjacent([], _, []).
adjacent([Case|Rest], Adj, [Part_num|Result]) :-
    findall(N, (occupied(Case, (I,J,N)),
                member((I,J,_), Adj)), [Part_num|_]), !,
    adjacent(Rest, Adj, Result).
adjacent([_|Rest], Adj, Result) :-
    adjacent(Rest, Adj, Result).


adjacent_2_gear([], _, []).
adjacent_2_gear([Case|Rest], Adj, [(Symbol,Part_num)|Result]) :-
    findall((S,N), (occupied(Case, (I,J,N)),
                    member(((I,J,_),S), Adj)), [(Symbol,Part_num)|_]), !,
    adjacent_2_gear(Rest, Adj, Result).
adjacent_2_gear([_|Rest], Adj, Result) :-
    adjacent_2_gear(Rest, Adj, Result).


redundency(List, Dup) :-
    redundency(List, [], [], Dup).
redundency([], _, Acc, Acc) :- !.
redundency([(Symbol,Num)|Rest], Checklist, Acc, Result) :-
    once(member((Symbol,N0), Checklist)), !,
    redundency(Rest, Checklist, [(Symbol,(N0,Num))|Acc], Result).
redundency([(Symbol,Num)|Rest], Checklist, Acc, Result) :-
    redundency(Rest, [(Symbol,Num)|Checklist], Acc, Result).
