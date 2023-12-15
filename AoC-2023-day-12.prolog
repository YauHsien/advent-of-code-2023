
:- module('AoC-2023-day-12', [case/1, test_case/1, solve/2, input/2]).
:- ensure_loaded('AoC-2023-_.prolog').
:- set_prolog_flag(stack_limit, 2_147_483_648).
:- table map/2.
:- table match/2.

:- set_prolog_flag(stack_limit, 4_294_967_296).


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
    findall(N, (member((I,E), Input2),
                %map(unfold(5,'?',E), N),
                findall(_,match_unfolded2(E,_),M0), length(M0,N0),
                findall(_,match_unfolded(E,_),M2), length(M2,N2),
                N is N0*N2,
                write(I),write(': '),write(N),write(' '),flush_output), R0),
    sum_list(R0, Result).

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
% 執行 :- solve(2, R), sum_list(R, N). 之後 N 為總數 525152 。

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
    findall(N, (member((I,E), Input2),
                %map(unfold(5,'?',E), N),
                findall(_,match_unfolded(E,_),M), length(M,N),
                write(I),write(': '),write(N),write(' '),flush_output), R0),
    sum_list(R0, Result).
% 硬算，總會算完的。

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
                Rt is round(N2/N0*100)/100,
                S = N0*Rt^4,
                write(I),write(' '),flush_output), Result),
    write('\n').
% 執行 :- solve(2, R), sum_list(R, N). 之後 N 為總數。
% 但我的例子得到小數結果，顯然有估算的因素。
% ?- solve(2,R), sum_list(R,N).
% R = [1*3^4, 12*28.166666666666668^4, 8*15.75^4, 4*4.5^4, 46*82.69565217391305^4, 1*1^4, 4*11^4, 2* ... ^ ..., ... * ...|...],
% N = 34072876851981.414.
% ====> 結果數值太低
%
% 34080938908485.387
% 4734359361500

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

%:- table match_clumped/2 as incremental.
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
    split_string(Line, "\n ", "\n ", [Left0,Right0]),
    ( string_chars(Left0, L0), string_chars(Right0, R0),
      append([L0,['?'],L0,[' '],R0,[','],R0], Ln2),
      string_chars(Line2, Ln2)
    ),
    match(Line2, Match).

match_unfolded2(Line, Match) :-
    split_string(Line, "\n ", "\n ", [Left0,Right0]),
    ( string_chars(Left0, L0), string_chars(Right0, R0),
      append([L0,['?'],L0,[' '],
              R0,[','],R0], Ln2),
      string_chars(Line2, Ln2)
    ),
    match(Line2, Match).

match_unfolded(Line, Match) :-
    split_string(Line, "\n ", "\n ", [Left0,Right0]),
    ( string_chars(Left0, L0), string_chars(Right0, R0),
      append([L0,['?'],L0,['?'],L0,['?'],L0,['?'],L0,[' '],
              R0,[','],R0,[','],R0,[','],R0,[','],R0], Ln2),
      string_chars(Line2, Ln2)
    ),
    match(Line2, Match).

match_unfolded(Line, [], Match) :-
    split_string(Line, "\n ", "\n ", [Left0,Right0]),
    ( string_chars(Left0, L0), string_chars(Right0, R0),
      append([L0,[' '],R0], Ln2),
      string_chars(Line2, Ln2)
    ),
    match(Line2, Match).
match_unfolded(Line, [S1], Match) :-
    split_string(Line, "\n ", "\n ", [Left0,Right0]),
    ( string_chars(Left0, L0), string_chars(Right0, R0),
      append([L0,[S1],L0,[' '],
              R0,[','],R0], Ln2),
      string_chars(Line2, Ln2)
    ),
    match(Line2, Match).
match_unfolded(Line, [S1,S2], Match) :-
    split_string(Line, "\n ", "\n ", [Left0,Right0]),
    ( string_chars(Left0, L0), string_chars(Right0, R0),
      append([L0,[S1],L0,[S2],L0,[' '],
              R0,[','],R0,[','],R0], Ln2),
      string_chars(Line2, Ln2)
    ),
    match(Line2, Match).
match_unfolded(Line, [S0,S1,S2], Match) :-
    split_string(Line, "\n ", "\n ", [Left0,Right0]),
    ( string_chars(Left0, L0), string_chars(Right0, R0),
      append([L0,[S0],L0,[S1],L0,[S2],L0,[' '],
              R0,[','],R0,[','],R0,[','],R0], Ln2),
      string_chars(Line2, Ln2)
    ),
    match(Line2, Match).
match_unfolded(Line, [S1,S2,S3,S4], Match) :-
    split_string(Line, "\n ", "\n ", [Left0,Right0]),
    ( string_chars(Left0, L0), string_chars(Right0, R0),
      append([L0,[S1],L0,[S2],L0,[S3],L0,[S4],L0,[' '],
              R0,[','],R0,[','],R0,[','],R0,[','],R0], Ln2),
      string_chars(Line2, Ln2)
    ),
    match(Line2, Match).


map(unfold(1,String), Combinations) :- !,
    findall(_, match(String,_), R), length(R, Combinations).
map(unfold(2,Sep,String), Combinations) :-
    member(Sep, ['.','#']), !,
    split_string(String, "\n ", "\n ", [Left0,Right0]),
    ( string_chars(Left0, L0), string_chars(Right0, R0),
      append([L0,[Sep],L0,[' '],R0,[','],R0], Ln2),
      string_chars(Line2, Ln2)
    ),
    findall(_, match(Line2,_), R), length(R, Combinations).
map(unfold(N,'.',String), Combinations) :-
    N > 2, N2 is N-1,
    map(unfold(1,String), C0),
    map(unfold(N2,'?',String), C1),
    map(unfold(N2,'#',String), C2),
    Combinations is C0*C1 + C0*C2.
map(unfold(N,'#',String), Combinations) :-
    N > 2, N2 is N-2, N1 is N-1,
    map(unfold(2,'#',String), C0), map(unfold(N2,'?',String), C1),
    map(unfold(N1,'#',String), C2),
    Combinations is C0*C1 + C2.
map(unfold(N,'?',String), Combinations) :-
    N > 1, !,
    map(unfold(N,'.',String), C0),
    map(unfold(N,'#',String), C2),
    Combinations is C0+C2.
map(unfold(1,_,String), Combinations) :- map(unfold(1,String), Combinations).



patternize(String, Pattern) :-
    programize(String, (Vars,Prog)),
    maplist([A]>>(member(E,['.','#']),A=E),Vars),
    clumped(Prog, Clumped),
    findall(S, (member(E-N,Clumped),E\='.',
                number_chars(N, Cs),
                string_chars(S, Cs)), Pattern0),
    atomics_to_string(Pattern0, ",", Pattern).



programize(String, (Vars,Prog)) :- string(String), !,
                                   string_chars(String, Chars),
                                   findall(C0, (member(Char, Chars),
                                                ((Char=='.'; Char=='#')
                                                -> C0 = Char
                                                ; Char=='?'
                                                  -> C0 = _
                                                )), Prog),
                                   term_variables(Prog, Vars).

profile(Line) :-
    ( findall(_,'AoC-2023-day-12':match(Line,M),M),length(M,C),
      writeln(C)
    ; (A=[_];A=[_,_];A=[_,_,_]),
      foreach(
          (maplist([B]>>(member(E,['.','#']),B=E),A),
           findall(_,'AoC-2023-day-12':match_unfolded(Line,A,M),M),
           length(M,C)),
          (format("~p~p~n",[A,C]),flush_output))).
