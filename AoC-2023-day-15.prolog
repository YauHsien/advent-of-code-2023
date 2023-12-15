
:- module('AoC-2023-day-15', [solve/2, test_case/1, case/1, input/2]).
:- consult('AoC-2023-_.prolog').

%% 第十五日：編碼秘術
%%
%% 編碼規則：
%%
%% 對一字元，取 ASCII 並加以特定的起始值，乘以 17，又除以 256 並取其餘數。
%% 對一字串，起始值為 0；將字串第一字元取編碼值，做為第二字元的起始值；
%% 對字串的每一字元，以其起始值取編碼值，並將該編碼值做為下一字元的起始值。
%%
%%%%%%%%% 第二題
%%
%% 應用輸入檔為 HASHMAP 指令。
%% HASHMAP 為「假期 ASCII 文字幫手──手動編排程序」的縮寫。
%% ("Holiday ASCII String Helper Manual Arrangement Procedure")
%%
%% 如指令 rn=1 分解為三個段落
%%
%% rn     表示鏡片的標籤
%% =      表示配入鏡片或替換鏡片
%% 1~9    表示鏡片焦距
%%
%% 作業現場有 256 個箱子，依序編號為 0~255。上述指令標籤，可藉由第一題
%% 對應為 0~255 數字，並洽巧符合任一箱子編號。
%%
%% 當你要處理標示為 rn=1 格式命令時，即找到第 0 號箱子，並看箱子裡面
%% 如果有標籤為 rn 的鏡片，就替換該鏡片；否則，將一片新的 rn 鏡片塞到
%% 箱子裡現有鏡片堆的後面。
%%
%% 當你要處理標籤為 rn- 格式命令時，也找到第 0 號箱子，並看箱子裡面
%% 如果有標籤為 rn 的鏡片，就取走該鏡片，並讓排在該鏡片後面的其他鏡片
%% 都往前推。
%%
%% 計分：求各箱聚焦力的總和。
%%
%% 聚焦力 = ( 箱號 + 1 ) x ( 鏡片順序號碼由 1 開始 ) x ( 鏡片焦距 )
%%


test_case("rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7\n").

case(Case) :-
    input(15, 1, Case).


solve(test_case_1, Result) :-
    test_case(Case), input(Case, Input),
    InitValue = 0,
    findall(R0, (member(String, Input),
                 hash(String, InitValue, R0)), Result).

solve(test_case_2, (Info,FocusingPower)) :-
    test_case(Case), input(Case, Input),
    findall(Command, (member(String, Input),
                      command(String, Command)), Commands),
    operation(Commands, R0),
    findall(C0,
            (between(0, 255, N0),
             N2 is N0+1,
             findall(E, (member(E,R0),E=(N0,_,_)), Collection),
             findall((N0,L0,F0,N2*N*F0), nth1(N,Collection,(N0,L0,F0)), C0)
            ),
            R2),
    append(R2, Info),
    findall(E, member((_,_,_,E),Info), FocusingPower).

solve(1, Result) :-
    case(Case), input(Case, Input),
    InitValue = 0,
    findall(R0, (member(String, Input),
                 hash(String, InitValue, R0)), Result).

solve(2, (Info,FocusingPower)) :-
    case(Case), input(Case, Input),
    findall(Command, (member(String, Input),
                      command(String, Command)), Commands),
    operation(Commands, R0),
    findall(C0,
            (between(0, 255, N0),
             N2 is N0+1,
             findall(E, (member(E,R0),E=(N0,_,_)), Collection),
             findall((N0,L0,F0,N2*N*F0), nth1(N,Collection,(N0,L0,F0)), C0)
            ),
            R2),
    append(R2, Info),
    findall(E, member((_,_,_,E),Info), FocusingPower).

input(Input, Result) :-
    split_string(Input, "\n,", "\n", Result).


hash(String, InitValue, Result) :-
    contract(String, (Var,Res)),
    Var = InitValue,
    Result is Res.

contract(String, (InitVariable,Result)) :-
    string(String),
    string_chars(String, Characters),
    maplist(contract, Characters, _, Expressions),
    term_variables(Expressions, Variables),
    reverse(Expressions, [Result|Er0]), reverse(Er0, Es0),
    [InitVariable|Vs0] = Variables,
    maplist(=, Es0, Vs0).

contract(CharAsAtom, InitValue, Expression) :-
    atom_codes(CharAsAtom, [Code]),
    Expression = (InitValue+Code)*17 rem 256.



command(String, (Box,Label,FocalLength)) :-
    string_chars(String, Characters), append(L0, ['=',F0], Characters), !,
    string_chars(Label, L0),
    hash(Label, 0, Box),
    atom_number(F0, FocalLength).
command(String, (Box,Label,remove)) :-
    string_chars(String, Characters), reverse(Characters, ['-'|R0]), !,
    reverse(R0, L0), string_chars(Label, L0),
    hash(Label, 0, Box).

operation(Commands, Result) :-
    operation(Commands, [], Result).
operation([], Boxes, Result) :-
    reverse(Boxes, Result).
operation([Command|Commands], Boxes, Result) :-
    operation(Command, Boxes, B0),
    operation(Commands, B0, Result).
operation((Box,Label,remove), Boxes, Result) :- !,
    subtract(Boxes, [(Box,Label,_)], Result).
operation((Box,Label,FocalLength), Boxes, Result) :-
    member((Box,Label,F0), Boxes), !,
    select((Box,Label,F0), Boxes, (Box,Label,FocalLength), Result).
operation((Box,Label,FocalLength), Boxes, [(Box,Label,FocalLength)|Boxes]).
