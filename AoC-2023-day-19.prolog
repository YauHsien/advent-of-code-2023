
:- module('AoC-2023-day-19', [solve/2, case/1, test_case/1, input/2]).
:- consult('AoC-2023-_.prolog').

:- dynamic p/1.

%% 第十九日：沒想到過節還要做品保啊！
%%
%% 你幫忙帶足夠的零件回到齒輪島，而現在要檢查零件規格。
%%
%% 規格有四種比分：
%%
%% 1. "X": 「超酷的外觀」 "E-x-tremely cool looking"
%% 2. "M": 「有樂音」 "M-usical"
%% 3. "A": 「符合流線動力」 "A-erodynamic"
%% 4. "S": 「閃亮光澤」 "S-hiny"
%%
%% 每個零件都該符合上述 XMAS 評比。
%%


test_case("px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}").

case(Case) :-
    'AoC-2023-_':input(19, 1, Case).

solve(test_case_1, Result) :-
    test_case(Case), input(Case, (Rules,Parts)),
    handle(Rules),
    findall(Part, (member(Part, Parts),
                   in(Part,R0), R0=accepted), Accepted),
    findall(N, (member({x=X,m=M,a=A,s=S}, Accepted),
                N = X+M+A+S), Result),
    true.

solve(1, Result) :-
    case(Case), input(Case, (Rules,Parts)),
    handle(Rules),
    findall(Part, (member(Part, Parts),
                   in(Part,R0), R0=accepted), Accepted),
    findall(N, (member({x=X,m=M,a=A,s=S}, Accepted),
                N = X+M+A+S), Result),
    true.

solve(2, _) :-
    get_time(T), stamp_date_time(T,D,local), writeln(D),
    foreach((between(1,4000,X),
             between(1,4000,M),
             between(1,4400,A),
             between(1,4000,S)
            ),
            true
           ),
    get_time(T2), stamp_date_time(T2,D2,local), writeln(D2),
    true.

input(Input, (Rules,Parts)) :-
    split_string(Input, "\n", "", Input0),
    append(Rules0, [""|Parts0], Input0),
    findall((Head,Rule),
            (member(Rule0, Rules0),
             split_string(Rule0, "{,}", "}", [Head0|Body0]),
             string_concat(Head0,
                           "(Part, Result) :- Part = {x=X,m=M,a=A,s=S}, ",
                           Lead0),
             term_string(Head, Head0),
             findall(Statement,
                     (member(Statement0, Body0),
                      (term_string((K0<N0:T0), Statement0),
                       ground(K0), ground(N0),
                       ( ground(T0) -> conclude(T0, Conclude0);
                         string_concat(_, ":A", Statement0) -> conclude("A", Conclude0);
                         string_concat(_, ":R", Statement0) -> conclude("R", Conclude0)
                       )
                      ->
                          atom_string(K0,S0), string_upper(S0,U0),
                          interpolate_string(
                              "{Variable} < {Value} -> {Conclude}; ",
                              Statement,
                              [Variable=U0, Value=N0, Conclude=Conclude0], [])
                      ; term_string((K0>N0:T0), Statement0),
                        ground(K0), ground(N0),
                        ( ground(T0) -> conclude(T0, Conclude0);
                          string_concat(_, ":A", Statement0) -> conclude("A", Conclude0);
                          string_concat(_, ":R", Statement0) -> conclude("R", Conclude0)
                        ) 
                        ->
                        atom_string(K0,S0), string_upper(S0,U0),
                        interpolate_string(
                            "{Variable} > {Value} -> {Conclude}; ",
                            Statement,
                            [Variable=U0, Value=N0, Conclude=Conclude0], [])
                      ; term_string(T0, Statement0),
                        ( ground(T0) -> conclude(T0, Statement);
                          Statement0 = "A" -> conclude("A", Statement);
                          Statement0 = "R" -> conclude("R", Statement)
                        )
                      )),
                     Statements),
             atomic_list_concat(Statements, Rule2),
             atom_string(Rule2, Following0),
             interpolate_string("{Lead} ({Following}).", Rule,
                                [Lead=Lead0, Following=Following0], [])
            ),
            Rules),
    findall(Part, (member(Part0,Parts0),term_string(Part,Part0)), Parts).

conclude("A", "Result = accepted") :- !.
conclude("R", "Result = rejected") :- !.
conclude(Name, Result) :-
    interpolate_string("{Name0}(Part, Result)", Result, [Name0=Name], []).


handle(Rules) :-
    foreach(p(Pred), abolish(Pred/2)),
    foreach(member((Head,_),Rules), (dynamic(Head/2),asserta(p(Head)))),
    foreach((member((_,Rule),Rules),term_string(R0,Rule)), assertz(R0)).
