
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
%%%%%% 第二題
%%
%% 問任一零件的 XMAS 比分值分別可能在 1 到 4000 之間，
%% 共有 4000 * 4000 * 4000 * 4000 種組合，
%% 於是，依規格檢查規則，有多少種組合合規？
%%
%%%%%%
%%
%% 這題非常適合用 Prolog 求解！


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
%% :- solve(test_case_1, Result), sum_list(Result, N), !.
%% N = 19114.

solve(1, Result) :-
    case(Case), input(Case, (Rules,Parts)),
    handle(Rules),
    findall(Part, (member(Part, Parts),
                   in(Part,R0), R0=accepted), Accepted),
    findall(N, (member({x=X,m=M,a=A,s=S}, Accepted),
                N = X+M+A+S), Result),
    true.
%% :- solve(1, Result), sum_list(Result, N), !.

solve(test_case_2, Result) :-
    test_case(Case), input2(Case, (Rules,_)),

    writeln('Loaded:'),
    foreach(member(R0,Rules), writeln(R0)),

    handle(Rules),
    init_stat(Range),
    findall(R0, in(Range,R0), Result),
    true.
%% :- solve(test_case_2, Result), sum_list(Result, N), !.
%% N = 167409079868000.

solve(2, Result) :-
    case(Case), input2(Case, (Rules,_)),
    handle(Rules),
    init_stat(Range),
    findall(R0, in(Range,R0), Result),
    true.
%% :- solve(2, Result), sum_list(Result, N), !.

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

conclude2("A", "accepted(State2, Result)") :- !.
conclude2("R", "rejected(State2, Result)") :- !.
conclude2(Name, Result) :-
    interpolate_string("{Name0}(State2, Result)", Result, [Name0=Name,Part2="{Part2}"], []).

accepted({x={Lx,Ux},m={Lm,Um},a={La,Ua},s={Ls,Us}}, Result) :-
    Result = (Ux-Lx+1)*(Um-Lm+1)*(Ua-La+1)*(Us-Ls+1).

rejected(_, 0).

init_stat({x={1,4000},m={1,4000},a={1,4000},s={1,4000}}).

input2(Input, (Rules,Parts)) :-
    split_string(Input, "\n", "", Input0),
    append(Rules0, [""|Parts0], Input0),
    findall((Head,Rule),
            (member(Rule0, Rules0),
             split_string(Rule0, "{,}", "}", [Head0|Body0]),
             term_string(Head, Head0),
             findall((Cond0,Conclude0),
                     (member(Statement0, Body0),
                      (term_string((K0<N0:T0), Statement0),
                       ground(K0), ground(N0) ->
                           Cond0 = (K0<N0),
                           ( ground(T0) -> conclude2(T0, Conclude0);
                             string_concat(_, ":A", Statement0) -> conclude2("A", Conclude0);
                             string_concat(_, ":R", Statement0) -> conclude2("R", Conclude0) )
                      ; term_string((K0>N0:T0), Statement0),
                        ground(K0), ground(N0) ->
                        Cond0 = (K0>N0),
                        ( ground(T0) -> conclude2(T0, Conclude0);
                          string_concat(_, ":A", Statement0) -> conclude2("A", Conclude0);
                          string_concat(_, ":R", Statement0) -> conclude2("R", Conclude0) ) 
                      ; Cond0 = nil,
                        term_string(T0, Statement0),
                        ( ground(T0) -> conclude2(T0, Conclude0);
                          Statement0 = "A" -> conclude2("A", Conclude0);
                          Statement0 = "R" -> conclude2("R", Conclude0)
                        )
                      )),
                     Statements),
             maplist([(Condition,_),Condition]>>true, Statements, Conditions0),
             cascade_cond(Conditions0, CascadeConds0),
             maplist(rule2, CascadeConds0, Statements, Statements0),
             maplist([(_,St,_),St]>>true, Statements0, Statements2),
             atomic_list_concat(Statements2, "; ", Body2),
             interpolate_string("{Head_}(State, Result) :- ({Body_})",
                                Rule, [Head_=Head0,Body_=Body2], [])
            ),
            Rules),
    findall(Part, (member(Part0,Parts0),term_string(Part,Part0)), Parts).

cascade_cond(Conditions, CascadeConditions) :-
    cascade_cond(Conditions, [], CascadeConditions).
cascade_cond([], Acc, Conditions) :- reverse(Acc, Conditions).
cascade_cond([Cond|Conditions], [], Result) :-
    cascade_cond(Conditions, [[Cond]], Result).
cascade_cond([nil|Conditions], [[Cond2|Acc0]|Acc], Result) :- !,
    complement_cond(Cond2, RevCond2),
    cascade_cond(Conditions, [[RevCond2|Acc0],[Cond2|Acc0]|Acc], Result).
cascade_cond([Cond|Conditions], [[Cond2|Acc0]|Acc], Result) :-
    complement_cond(Cond2, RevCond2),
    cascade_cond(Conditions, [[Cond,RevCond2|Acc0],[Cond2|Acc0]|Acc], Result).
complement_cond((K<N), (K>M)) :- M is N-1.
complement_cond((K>N), (K<M)) :- M is N+1.

rule2(Conditions, (_,Conclusion), (Conditions,Statement,Conclusion)) :-
    interpolate_string("cloak_stat({Cond}, State, State2), {Conc}",
                       Statement, [Cond=[Conditions],Conc=Conclusion], []).

cloak_stat([], State, State) :- !.
cloak_stat([Cond|Conditions], State, State2) :- !,
    cloak_stat(Cond, State, State0),
    cloak_stat(Conditions, State0, State2).
cloak_stat(Condition, State, State2) :-
    (Condition = (_<_) -> cloak_stat_lt(Condition, State, State2);
     Condition = (_>_) -> cloak_stat_gt(Condition, State, State2)).
cloak_stat_lt((x<N), {x={Lx,Ux0},m=M,a=A,s=S}, {x={Lx,Ux},m=M,a=A,s=S}) :- N < Ux0, !, Ux is N-1.
cloak_stat_lt((m<N), {x=X,m={Lm,Um0},a=A,s=S}, {x=X,m={Lm,Um},a=A,s=S}) :- N < Um0, !, Um is N-1.
cloak_stat_lt((a<N), {x=X,m=M,a={La,Ua0},s=S}, {x=X,m=M,a={La,Ua},s=S}) :- N < Ua0, !, Ua is N-1.
cloak_stat_lt((s<N), {x=X,m=M,a=A,s={Ls,Us0}}, {x=X,m=M,a=A,s={Ls,Us}}) :- N < Us0, !, Us is N-1.
cloak_stat_lt(_, State, State).
cloak_stat_gt((x>N), {x={Lx0,Ux},m=M,a=A,s=S}, {x={Lx,Ux},m=M,a=A,s=S}) :- N > Lx0, !, Lx is N+1.
cloak_stat_gt((m>N), {x=X,m={Lm0,Um},a=A,s=S}, {x=X,m={Lm,Um},a=A,s=S}) :- N > Lm0, !, Lm is N+1.
cloak_stat_gt((a>N), {x=X,m=M,a={La0,Ua},s=S}, {x=X,m=M,a={La,Ua},s=S}) :- N > La0, !, La is N+1.
cloak_stat_gt((s>N), {x=X,m=M,a=A,s={Ls0,Us}}, {x=X,m=M,a=A,s={Ls,Us}}) :- N > Ls0, !, Ls is N+1.
cloak_stat_gt(_, State, State).

statement2((_,State,Template), Statement) :-
    interpolate_string(Template, Statement, [Part2=State], []).


handle(Rules) :-
    foreach(p(Pred), abolish(Pred/2)),
    foreach(member((Head,_),Rules), (dynamic(Head/2),asserta(p(Head)))),
    foreach((member((_,Rule),Rules),term_string(R0,Rule)), assertz(R0)).
