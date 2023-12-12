
:- module('AoC-2023-day-12', [case/1, test_case/1, solve/2, input/2]).
:- ensure_loaded('AoC-2023-_.prolog').
:- set_prolog_flag(stack_limit, 2_147_483_648).


test_case("???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1").

case(Case) :-
    'AoC-2023-_':input(12, 1, Case).


solve(test_case_1, Result) :-
    test_case(Case), input(Case, Input),
    findall(N, (member(E, Input),
                findall(M, match(E, M), Matches),
                length(Matches, N)), Result).

solve(test_case_2, Result) :-
    test_case(Case), input(Case, Input),
    ( length(Input, Ni),
      findall(Ii, between(1,Ni,Ii), Is),
      maplist([A,B,(A,B)]>>true, Is, Input, Input2)
    ),
    findall(S, (member((I,E), Input2),
                (findall(M, match(E,M), M0),
                 length(M0, N0)
                ),
                (findall(M, match_with_prefix('?',E,M), M2),
                 length(M2, N2)
                ),
                Rt is N2/N0,
                S is N0*Rt^4,
                write(I),write(' ')), Result),
    write('\n').

solve(1, Result) :-
    case(Case), input(Case, Input),
    findall(N, (member(E, Input),
                findall(M, match(E, M), Matches),
                length(Matches, N)), Result).

solve(2, Result) :-
    case(Case), input(Case, Input),
    ( length(Input, Ni),
      findall(Ii, between(1,Ni,Ii), Is),
      maplist([A,B,(A,B)]>>true, Is, Input, Input2)
    ),
    findall(S, (member((I,E), Input2),
                (findall(_, match(E,M), M0),
                 length(M0, N0)
                ),
                (findall(_, match_with_prefix('?',E,M), M2),
                 length(M2, N2)
                ),
                S = N0*N2^4,
                write(I),write(' '),flush_output), Result),
    write('\n').

input(Input, Result) :-
    split_string(Input, "\n", "\n", Result).



match(Line, Match) :-
    split_string(Line, "\n ", "\n ", [Line2,Criteria]),
    ( string_chars(Line2, L0), length(L0, N0),
      clumped(L0, L2)
    ),
    ( split_string(Criteria, ",", "", C0),
      findall('#'-N, (member(S,C0),number_string(N,S)), C2)
    ),
    dot_part(N0, C2, D0), riffled(D0, C2, R0),

    %( inflate(R0,A), string_chars(A0,A), length(A,NNN),
    %  inflate(L2,B),writeln(B),
    %  write(A0), writeln(NNN)
    %),
    % writeln(D0),

    ( match_clumped(L2, R0),

      %(
      %    inflate(L2,A), inflate(R0,B),
      %    writeln(L0),
      %    writeln(A),
      %    writeln(R0),
      %    writeln(B),
      %    write('\n')
      %),


      inflate(R0, Match)
    ).



sum_list_([], 0).
sum_list_([H|T], N) :-
    ground(N), !,
    between(0, N, N2),
    sum_list_(T, N2),
    H is N-N2.


inflate(Clumped, Result) :-
    inflate(Clumped, [], Result).
inflate([], Acc, Result) :-
    reverse(Acc, R), append(R, Result).
inflate([K-N|Rest], Acc, Result) :-
    once(findnsols(N, K, repeat, R0)),
    inflate(Rest, [R0|Acc], Result).


dot_part(Length, Clumped, Dot_part) :-
    findall(N, member(_-N,Clumped), Os), sum_list(Os, O),
    length(Clumped, N),
    ( ( N3 is N-1
      ; N3 is N
      ),
      length(Ns0, N3), N0 is Length-O-N3,
      sum_list_(Ns0, N0),
      ( N3 =:= N-1
      -> append([[-1],Ns0,[-1]], Ns)
      ; N3 =:= N
        -> ( append([[-1],Ns0], Ns)
           ; append([Ns0,[-1]], Ns)
           )
      )
    ; N2 is N+1, N0 is Length-O-N2,
      length(Ns, N2), sum_list_(Ns, N0)
    ),
    findall('.'-M2, (member(M,Ns),M2 is M+1), Dot_part).

riffled([Term], [], [Term]).
riffled([A|As], [B|Bs], [A,B|Result]) :-
    riffled(As, Bs, Result).


list_case([], []).
list_case([H|List], [H2|Result]) :-
    case(H, H2),
    list_case(List, Result).
case('.', '.').
case('#', '#').
case('?', '#').
case('?', '.').


match_clumped([], []) :- !.
match_clumped([_-0|Rest], List) :- !, match_clumped(Rest, List).
match_clumped(List, [_-0|Rest]) :- !, match_clumped(List, Rest).
match_clumped([K0-A|Rest], [K-A|Rest2]) :- ( K0 == K, !,
                                             member(K, ['?','.','#'])
                                           ; K0 == '?',
                                             member(K, ['.','#'])
                                           ), !,
                                           match_clumped(Rest, Rest2).
match_clumped([K-A|Rest], [K-B|Rest2]) :- member(K, ['.','#']),
                                          A > B, !, A0 is A-B,
                                          match_clumped([K-A0|Rest], Rest2).
match_clumped([K-A|Rest], [K-B|Rest2]) :- member(K, ['.','#']), !,
                                          A < B, !, B0 is B-A,
                                          match_clumped(Rest, [K-B0|Rest2]).
match_clumped(['?'-A|Rest], [K-B|Rest2]) :- member(K, ['.','#']),
                                            A > B, !, A0 is A-B,
                                            match_clumped(['?'-A0|Rest], Rest2).
match_clumped(['?'-A|Rest], [K-B|Rest2]) :- member(K, ['.','#']),
                                            A < B, !, B0 is B-A,
                                            match_clumped(Rest, [K-B0|Rest2]).


count(List, Result) :-
    clumped(List, R0),
    findall(N, (member(E-N, R0),
                E == '#'), Result).


match_with_prefix('?', Line, Match) :-
    split_string(Line, "\n ", "\n ", [Line0,Criteria]),
    ( string_chars(Line0, L0),
      append([L0,['?'],L0], Line2),
      string_chars(Line2, L3),
      list_case(L3, L4)
    ),
    ( split_string(Criteria, ",", "", C0),
      findall(N, (member(S,C0),number_string(N,S)), C2),
      append([C2,C2], C4)
    ),
    count(L4, R0),
    R0 == C4,
    string_chars(Match, L4).
