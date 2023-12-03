
:- module('AoC-2023-day-2', [solve/1, test_case/2]).

%% 三色拼合題
%%
%% 精靈從袋子裡，每回合抓一把東西，你快速地抄下 RGB 每個顏色東西的數量。
%% 抄下的格式如下
%%
%% Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
%% Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
%% Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
%%
%% 有幾行 Game 格式可能抄錯；若在 RGB 為 12 R, 13 G, 14 B 的袋子裡，
%% 不可能抽出 Game 3 的情況。
%%
%% 第一個問題，找出全部合理的 Game 紀錄，並把 Game 號加總來。
%%
%%%%%%%%%%%%%%% 第二題
%% 從另一面來講，不設定 RGB 實際有多少，而改由抄錄的資料推測出
%% 每個 Game 袋子至少有多少個 RGB 東西。
%%
%% 算法是將每個 Game 袋子最少的 RGB 數量找出，並且 R x G x B
%% 稱為 Power ；將所有 Game Power 加總來。

rgb([12, 13, 14]).

support([R,G,B], [R2,G2,B2]) :-
    (var(R2), !; R >= R2),
    (var(G2), !; G >= G2),
    (var(B2), !; B >= B2).

possible(_RGB, []) :- !.
possible(RGB, [RGB_2|Cases]) :-
    support(RGB, RGB_2),
    possible(RGB, Cases).



rgb_case(Input, Case) :-
    string(Input), !,
    split_string(Input, ", ", ",; ", Input_0),
    rgb_case(Input_0, Case).

rgb_case([], [_,_,_]) :- !.
rgb_case([Num_str, "red"|Input], [R,G,B]) :-
    number_string(R, Num_str),
    rgb_case(Input, [_,G,B]).
rgb_case([Num_str, "green"|Input], [R,G,B]) :-
    number_string(G, Num_str),
    rgb_case(Input, [R,_,B]).
rgb_case([Num_str, "blue"|Input], [R,G,B]) :-
    number_string(B, Num_str),
    rgb_case(Input, [R,G,_]).


rgb_cases(Input, Cases) :-
    string(Input), !,
    split_string(Input, ";", ";\n", Input_0),
    rgb_cases(Input_0, Cases).

rgb_cases([], []) :- !.
rgb_cases([Term|Input], [Result|Cases]) :-
    rgb_case(Term, Result),
    rgb_cases(Input, Cases).


game(Input, Game) :-
    string(Input), !,
    split_string(Input, ":", ":\n", Input_0),
    game(Input_0, Game).

game([Game_str,Cases_str], [Game_num,Cases]) :-
    split_string(Game_str, " ", " ", ["Game",Num_str]),
    number_string(Game_num, Num_str),
    rgb_cases(Cases_str, Cases).


fewest([], [0,0,0]) :- !.
fewest([[R0,G0,B0]|Cases], [R,G,B]) :-
    fewest(Cases, [R2,G2,B2]),
    (var(R0), !, R = R2; R0 > R2, !, R = R0; R = R2),
    (var(G0), !, G = G2; G0 > G2, !, G = G0; G = G2),
    (var(B0), !, B = B2; B0 > B2, !, B = B0; B = B2).



test_case(1, Result) :-
    solve("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green", Result).

test_case(2, Result) :-
    solve_2("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green", Result).



solve(Result) :-
    input(2, 1, Input),
    solve(Input, Result).

solve(Input, Result) :-
    split_string(Input, "\n", "\n", Input_0),
    rgb(Support_context),
    findall(Game_num, (member(Game_str, Input_0),
                       game(Game_str, [Game_num,Cases]),
                       possible(Support_context, Cases)), Result).

solve_2(Result) :-
    input(2, 1, Input),
    solve_2(Input, Result).
solve_2(Input, Result) :-
    split_string(Input, "\n", "\n", Input_0),
    findall(Power, (member(Game_str, Input_0),
                    game(Game_str, [_,Cases]),
                    fewest(Cases, [R,G,B]),
                    Power = R*G*B), Result).
