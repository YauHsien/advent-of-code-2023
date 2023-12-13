
:- module('AoC-2023-day-13', [solve/2, case/1, test_case/1, input/2]).
:- consult('AoC-2023-_.prolog').
:- import('AoC-2023-_').


%% 第十三日：「鏡」地
%%
%% 當你探索熔岩來源時，你來到一處灰色的火成岩地區。
%% 你細看之下，發現滿地都有大片的鏡子，插在那些火山灰地面上，但
%% 在灰色的土地與折射的灰色土地之間，不容易分辨。
%%
%% 你做了一點觀察紀錄，注意到可藉由對稱的地貌猜測哪些地方有鏡子。
%%
%% 觀察記錄如下例：
%% #.##..##.
%% ..#.##.#.
%% ##......#
%% ##......#
%% ..#.##.#.
%% ..##..##.
%% #.#.##.#.
%%
%% #...##..#
%% #....#..#
%% ..##..###
%% #####.##.
%% #####.##.
%% ..##..###
%% #....#..#
%% 以上兩份紀錄稿，第一張在縱軸介於第五、六欄之間看起來有鏡子，
%% 因為左右邊對稱，除第一欄應該對照到篇幅之外的第十欄。
%% 同理，第二章紀錄稿裡頭，橫軸介於第四、五列之間看起來有鏡子。
%%
%% 鏡子分布的摘要報告，以全部所在橫軸的鏡面之上的列數乘以 100，
%% 再加上全部所在縱軸的鏡面之左的欄數。以上圖為例，加總為 405。
%%
%% 第一題問輸入檔的鏡子摘要報告。
%%
%%%%%%%%%%% 第二題：找出差「一點」就對稱的鏡子所在地
%%
%% 因為在滿是塵埃的地形，鏡子應該沾上了「一點」污漬，使得反映的
%% 鏡像都差「一點」就成為完美鏡像。
%%
%% 第二題要找出差一點是完美鏡像的鏡子橫軸與縱軸，並求加總摘要。


test_case("").


case(Case) :-
    'AoC-2023-_':input(13, 1, Case).


solve(1, Result) :-
    case(Case), input(Case, Input),
    length(Input, Ni),

    findall((Id, Orientation, CountRowsOrColumns, Digest),
            (between(1, Ni, I),
             once(member((I,Nh,Nv,Dots), Input)),
             (two_in(Nv, A, B), A+1 =:= B,
              mirror_in((I,Nh,Nv,Dots), v, A, B)
             -> (Id,Orientation,CountRowsOrColumns,Digest)
                = (I,v,A,100*A)
             ; two_in(Nh, A, B), A+1 =:= B,
               mirror_in((I,Nh,Nv,Dots), h, A, B),
               (Id,Orientation,CountRowsOrColumns,Digest)
               = (I,h,A,A)
             )
            ),
            Result).
% 求 :- solve(1,R), findall(S,member((_,_,_,S),R),S), sum_list(S,Sum).

solve(2, Result) :-
    case(Case), input(Case, Input),
    length(Input, Ni),

    findall((Id, Orientation, CountRowsOrColumns, Digest),
            (between(1, Ni, I),
             once(member((I,Nh,Nv,Dots), Input)),
             (two_in(Nv, A, B), A+1 =:= B,
              mirror_but_smudges_in((I,Nh,Nv,Dots), v, A, B)
             -> (Id,Orientation,CountRowsOrColumns,Digest)
                = (I,v,A,100*A)
             ; two_in(Nh, A, B), A+1 =:= B,
               mirror_but_smudges_in((I,Nh,Nv,Dots), h, A, B),
               (Id,Orientation,CountRowsOrColumns,Digest)
               = (I,h,A,A)
             )
            ),
            Result).
% 求 :- solve(2,R), findall(S,member((_,_,_,S),R),S), sum_list(S,Sum).


input(Input, Result) :-
    split_string(Input, "\n", "", Input0),
    findall("", member("",Input0), Newlines),
    ( length(Newlines, N0), N is N0+1,
      once(findnsols(N, _, repeat, Lines))
    ),

    findall([Nl], member(Nl,Newlines), Newlines0),
    riffled(Lines, Newlines0, Riffled),
    append(Riffled, Input0),
    findall(I0, (member(I0,Riffled),I0 \= [""]), Input2),
    length(Input2, Nr), findall(A, between(1,Nr,A), Ir),
    maplist([A,B,(A,B)]>>true, Ir, Input2, Items),

    ( findall((I0,Nh,Nv,R0),
              (member((I0,X0),Items),
               length(X0, Nv), findall(J,between(1, Nv, J),Js),
               maplist([A,B,(A,B)]>>true, Js, X0, I1),
               findall(I4, (member((J,I2), I1),
                            string_chars(I2,C0), length(C0,Nh),
                            findall(I, between(1,Nh,I), Is),
                            maplist([A,B,(A,B)]>>true, Is, C0, I3),
                            once(findnsols(Nh, J, repeat, Js2)),
                            maplist([A,B,(A,B)]>>true, Js2, I3, I4)
                           ), R),
               [S|_] = R,
               length(S, Nh),
               append(R, R0)
              ),
              Result0)
    ), !,
    Result = Result0.
riffled([Term], [], [Term]).
riffled([A|As], [B|Bs], [A,B|Result]) :-
    riffled(As, Bs, Result).


two_in(N, A, B) :- between(1, N, A), between(1, N, B), A < B.


mirror_in((_,Nh,_,Dots), h, A, _) :-
    ground(A),
    \+ between(1, Nh, A), \+ member((_,A,_), Dots), !.
mirror_in((_,Nh,_,Dots), h, _, B) :-
    ground(B),
    \+ between(1, Nh, B), \+ member((_,B,_), Dots).
mirror_in((_,_,Nv,Dots), v, A, _) :-
    ground(A),
    \+ between(1, Nv, A), \+ member((A,_,_), Dots), !.
mirror_in((_,_,Nv,Dots), v, _, B) :-
    ground(B),
    \+ between(1, Nv, B), \+ member((B,_,_), Dots).

mirror_in((Id,Nh,Nv,Dots), h, A, B) :-
    two_in(Nh, A, B), forall(between(1, Nv, J),
                             forall((member((J,A,D), Dots),
                                     member((J,B,D2), Dots)
                                    ),
                                    D == D2)), !,
    A2 is A-1, B2 is B+1, mirror_in((Id,Nh,Nv,Dots),h,A2,B2).

mirror_in((Id,Nh,Nv,Dots), v, A, B) :-
    two_in(Nv, A, B), forall(between(1, Nh, I),
                             forall((member((A,I,D), Dots),
                                     member((B,I,D2), Dots)),
                                    D == D2)), !,
    A2 is A-1, B2 is B+1, mirror_in((Id,Nh,Nv,Dots),v,A2,B2).


mirror_but_smudges_in(Context, Orientation, Pos1, Pos2) :-
    mirror_but_smudges_in(Context, 1, Orientation, Pos1, Pos2).


mirror_but_smudges_in((Id,Nh,Nv,Dots), Smudges, h, A, B) :-
    Smudges > 0,
    two_in(Nh, A, B), forall(between(1, Nv, J),
                             forall((member((J,A,D), Dots),
                                     member((J,B,D2), Dots)
                                    ),
                                    D == D2)), !,
    A2 is A-1, B2 is B+1,
    mirror_but_smudges_in((Id,Nh,Nv,Dots), Smudges, h, A2, B2).

mirror_but_smudges_in((Id,Nh,Nv,Dots), Smudges, h, A, B) :-
    Smudges > 0,
    two_in(Nh, A, B), findall(_, (between(1, Nv, J),
                                  findall(_, \+((member((J,A,D),Dots),
                                                 member((J,B,D2),Dots),
                                                 D == D2)), [_])
                                 ), [_]),
    A2 is A-1, B2 is B+1,
    mirror_but_smudges_in((Id,Nh,Nv,Dots), Smudges-1, h, A2, B2).

mirror_but_smudges_in(Context, Smudges, h, A, B) :-
    Smudges =:= 0, mirror_in(Context, h, A, B).

mirror_but_smudges_in((Id,Nh,Nv,Dots), Smudges, v, A, B) :-
    Smudges > 0,
    two_in(Nv, A, B), forall(between(1, Nh, I),
                             forall((member((A,I,D), Dots),
                                     member((B,I,D2), Dots)),
                                    D == D2)), !,
    A2 is A-1, B2 is B+1,
    mirror_but_smudges_in((Id,Nh,Nv,Dots), Smudges, v, A2, B2).

mirror_but_smudges_in((Id,Nh,Nv,Dots), Smudges, v, A, B) :-
    Smudges > 0,
    two_in(Nv, A, B), findall(_, (between(1, Nh, I),
                                  findall(_, \+((member((A,I,D),Dots),
                                                 member((B,I,D2),Dots),
                                                 D == D2)), [_])
                                 ), [_]),
    A2 is A-1, B2 is B+1,
    mirror_but_smudges_in((Id,Nh,Nv,Dots), Smudges-1, v, A2, B2).

mirror_but_smudges_in(Context, Smudges, v, A, B) :-
    Smudges =:= 0, mirror_in(Context, v, A, B).
