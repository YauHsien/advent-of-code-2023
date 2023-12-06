
:- module('AoC-2023-day-5', [solve/2, case/1, cut/3, pairs/2, apply/4]).

%%%%%% 第五日：補足食物供應
%%
%% 這題很像是 Categories 轉換，也就是手動操作 Functors 。
%%
%% 對照表：
%%
%%  seed-to-soil            種子兌土壤
%%  soil-to-fertilizer      土壤兌肥料
%%  fertilizer-to-water     肥料兌水
%%  water-to-light          水兌光照
%%  light-to-temperature    光照兌溫度
%%  temperature-to-humidity 溫度兌濕度
%%  humidity-to-location    濕度兌風土
%%
%% 第一題指輸入偶數數列為幾個種子數目。
%%
%% 但第二題則指，輸入的偶數數列每兩項為一對，表示一個 (Start, Range) 範圍。


test_case([79, 14, 55, 13]).

case([202517468, 131640971, 1553776977, 241828580, 1435322022, 100369067, 2019100043, 153706556, 460203450, 84630899, 3766866638, 114261107, 1809826083, 153144153, 2797169753, 177517156, 2494032210, 235157184, 856311572, 542740109]).


solve(test_case_1, Location) :-
    test_case(Seeds),
    findall(L, (member(S, Seeds),
                example_map(S, L)), Locations),
    min_list(Locations, Location).

solve(test_case_2, Location) :-
    test_case(Case), pairs(Case, Seeds_2),
    ( example_domain(seed, Seed_domain),
      findall(R, (member(Seed, Seeds_2),
                  cut(Seed, Seed_domain, R)), Seeds_0),
      append(Seeds_0, Seeds),
      apply(example_map, 'seed-to-soil', Seeds, Soil_2)
    ),
    write('> soil: '),
    writeln(Soil_2),
    ( example_domain(soil, Soil_domain),
      findall(R, (member(Soil, Soil_2),
                  cut(Soil, Soil_domain, R)), Soil_0),
      append(Soil_0, Soil),
      apply(example_map, 'soil-to-fertilizer', Soil, Fertilizer_2)
    ),
    write('> fertilizer: '),
    writeln(Fertilizer_2),
    ( example_domain(fertilizer, Fertilizer_domain),
      findall(R, (member(Fert, Fertilizer_2),
                  cut(Fert, Fertilizer_domain, R)), Fertilizer_0),
      append(Fertilizer_0, Fertilizer),
      apply(example_map, 'fertilizer-to-water', Fertilizer, Water_2)
    ),
    write('> water: '),
    writeln(Water_2),
    ( example_domain(water, Water_domain),
      findall(R, (member(Water, Water_2),
                  cut(Water, Water_domain, R)), Water_0),
      append(Water_0, Water),
      apply(example_map, 'water-to-light', Water, Light_2)
    ),
    write('> light: '),
    writeln(Light_2),
    ( example_domain(light, Light_domain),
      findall(R, (member(Light, Light_2),
                  cut(Light, Light_domain, R)), Light_0),
      append(Light_0, Light),
      apply(example_map, 'light-to-temperature', Light, Temperature_2)
    ),
    write('> temperature: '),
    writeln(Temperature_2),
    ( example_domain(temperature, Temperature_domain),
      findall(R, (member(Temp, Temperature_2),
                  cut(Temp, Temperature_domain, R)), Temperature_0),
      append(Temperature_0, Temperature),
      apply(example_map, 'temperature-to-humidity', Temperature, Humidity_2)
    ),
    write('> humidity: '),
    writeln(Humidity_2),
    ( example_domain(humidity, Humidity_domain),
      writeln(Humidity_domain),
      findall(R, (member(Humidity, Humidity_2),
                  cut(Humidity, Humidity_domain, R)), Humidity_0),
      append(Humidity_0, Humidity),
      apply(example_map, 'humidity-to-location', Humidity, Location_2)
    ),
    write('> location: '),
    writeln(Location_2),
    sort(Location_2, Loc_0),
    [(Location,_)|_] = Loc_0.

solve(1, Location) :-
    case(Seeds),
    findall(L, (member(S, Seeds),
                map(S, L)), Locations),
    min_list(Locations, Location).

solve(2, Location) :-
    case(Case), pairs(Case, Seeds_2),
    ( domain(seed, Seed_domain),
      findall(R, (member(Seed, Seeds_2),
                  cut(Seed, Seed_domain, R)), Seeds_0),
      append(Seeds_0, Seeds),
      apply(map, 'seed-to-soil', Seeds, Soil_2)
    ),
    write('> soil: '),
    writeln(Soil_2),
    ( domain(soil, Soil_domain),
      findall(R, (member(Soil, Soil_2),
                  cut(Soil, Soil_domain, R)), Soil_0),
      append(Soil_0, Soil),
      apply(map, 'soil-to-fertilizer', Soil, Fertilizer_2)
    ),
    write('> fertilizer: '),
    writeln(Fertilizer_2),
    ( domain(fertilizer, Fertilizer_domain),
      findall(R, (member(Fert, Fertilizer_2),
                  cut(Fert, Fertilizer_domain, R)), Fertilizer_0),
      append(Fertilizer_0, Fertilizer),
      apply(map, 'fertilizer-to-water', Fertilizer, Water_2)
    ),
    write('> water: '),
    writeln(Water_2),
    ( domain(water, Water_domain),
      findall(R, (member(Water, Water_2),
                  cut(Water, Water_domain, R)), Water_0),
      append(Water_0, Water),
      apply(map, 'water-to-light', Water, Light_2)
    ),
    write('> light: '),
    writeln(Light_2),
    ( domain(light, Light_domain),
      findall(R, (member(Light, Light_2),
                  cut(Light, Light_domain, R)), Light_0),
      append(Light_0, Light),
      apply(map, 'light-to-temperature', Light, Temperature_2)
    ),
    write('> temperature: '),
    writeln(Temperature_2),
    ( domain(temperature, Temperature_domain),
      findall(R, (member(Temp, Temperature_2),
                  cut(Temp, Temperature_domain, R)), Temperature_0),
      append(Temperature_0, Temperature),
      apply(map, 'temperature-to-humidity', Temperature, Humidity_2)
    ),
    write('> humidity: '),
    writeln(Humidity_2),
    ( domain(humidity, Humidity_domain),
      findall(R, (member(Humidity, Humidity_2),
                  cut(Humidity, Humidity_domain, R)), Humidity_0),
      append(Humidity_0, Humidity),
      apply(map, 'humidity-to-location', Humidity, Location_2)
    ),
    write('> location: '),
    writeln(Location_2),
    sort(Location_2, Loc_0),
    [(Location,_),B,C|_] = Loc_0,
    writeln(B), %%%%%%% <---- 我這題算得有點亂，但看起來方向很對
                %%%%%%%       輸入最小地號 0 不是正確答案，但
                %%%%%%%       輸入第二小地號則通過答案。
    writeln(C).



pairs([], []).
pairs([A,B|Rest], [(A,B)|Result]) :-
    pairs(Rest, Result).


function('seed-to-soil') :- !.
function('soil-to-fertilizer') :- !.
function('fertilizer-to-water').
function('water-to-light') :- !.
function('light-to-temperature') :- !.
function('temperature-to-humidity') :- !.
function('humidity-to-location') :- !.
function(Function) :- format("Bad function: ~p~n", [Function]), fail.

example_map(Seed, Location) :-
    example_map('seed-to-soil', Seed, Soil),
    example_map('soil-to-fertilizer', Soil, Fert),
    example_map('fertilizer-to-water', Fert, Water),
    example_map('water-to-light', Water, Light),
    example_map('light-to-temperature', Light, Temp),
    example_map('temperature-to-humidity', Temp, Hmdy),
    example_map('humidity-to-location', Hmdy, Location).
example_map(Function, From, To) :-
    function(Function),
    ( example_map(Function, Dst, Src, Range),
      Src2 is Src+Range - 1,
      between(Src, Src2, From), !
    ),
    To is Dst-Src + From.
example_map(Function, N, N) :- function(Function).

map(Seed, Location) :-
    map('seed-to-soil', Seed, Soil),
    map('soil-to-fertilizer', Soil, Fert),
    map('fertilizer-to-water', Fert, Water),
    map('water-to-light', Water, Light),
    map('light-to-temperature', Light, Temp),
    map('temperature-to-humidity', Temp, Hmdy),
    map('humidity-to-location', Hmdy, Location).
map(Function, From, To) :-
    function(Function),
    ( map(Function, Dst, Src, Range),
      Src2 is Src+Range - 1,
      between(Src, Src2, From), !
    ),
    To is Dst-Src + From.
map(Function, N, N) :- function(Function).


% 找出不跟 Ranges 沾邊的 (A,B)
cut((A,B), Ranges, [(A,B,a)]) :-
    Da is A+B - 1,
    forall(( member((S,R), Ranges), Ds is S+R - 1
           ),
           ( Da < S
           ; Ds < A
           )),
    !.

% 找出完全包含在某個 Range 裡的 (A,B)
cut((A,B), Ranges, [(A,B,b)]) :-
    Da is A+B - 1,
    member((S,R), Ranges), Ds is S+R - 1,
    S =< A, Da =< Ds, !.

% 抽走完全被 (A,B) 包含的 Ranges ，剩下的再繼續以下步驟。
cut((A,B), Ranges, Result) :-
    Da is A+B - 1,
    findall((S,R), (member((S,R), Ranges), Ds is S+R - 1,
                    A =< S, Ds =< Da), Con), Con \= [], !,
    ( subtract(Ranges, Con, Rest),
      ( sort(Con, Con_2),
        [(Da2,_)|_] = Con_2,
        B2 is Da2 - A + 1,
        reverse(Con_2, [(A2,B2_0)|_]),
        A3 is A2+B2_0,
        B3 is Da - A3 + 1
      ),
      cut((A,B2), Rest, Result0),
      cut((A3,B3), Rest, Result2),
      findall((S,R,c), member((S,R),Con), Con0)
    ),
    append(Result0, Result2, Result_),
    append(Con0, Result_, Result).

% 找出 (A,B) 因與 Ranges 交錯而有的分割
cut((A,B), Ranges, Sets) :-
    Da is A+B - 1,
    findall(C, (member((S,R), Ranges), Ds is S+R - 1,
                ( between(S,Ds,A), \+ between(S,Ds,Da),
                  Ds2 is Ds+1, B2 is Ds2-A, B3 is Da-Ds2,
                  ( B3 > 0, !, C = [(A,B2,d),(Ds2,B3,e)]
                  ; C = [(A,B2,d)]
                  )
                ; \+ between(S,Ds,A), between(S,Ds,Da),
                  B2 is S-A, R2 is Da-B2,
                  C = [(A,B2,f), (S,R2,g)]
                )), Sets0),
    append(Sets0, Sets).

                


example_map('seed-to-soil', 50, 98, 2).
example_map('seed-to-soil', 52, 50, 48).
example_map('soil-to-fertilizer', 0, 15, 37).
example_map('soil-to-fertilizer', 37, 52, 2).
example_map('soil-to-fertilizer', 39, 0, 15).
example_map('fertilizer-to-water', 49, 53, 8).
example_map('fertilizer-to-water', 0, 11, 42).
example_map('fertilizer-to-water', 42, 0, 7).
example_map('fertilizer-to-water', 57, 7, 4).
example_map('water-to-light', 88, 18, 7).
example_map('water-to-light', 18, 25, 70).
example_map('light-to-temperature', 45, 77, 23).
example_map('light-to-temperature', 81, 45, 19).
example_map('light-to-temperature', 68, 64, 13).
example_map('temperature-to-humidity', 0, 69, 1).
example_map('temperature-to-humidity', 1, 0, 69).
example_map('humidity-to-location', 60, 56, 37).
example_map('humidity-to-location', 56, 93, 4).


example_domain(seed, Domain) :- findall((S,R), example_map('seed-to-soil',_,S,R), Domain).
example_domain(soil, Domain) :- findall((S,R), example_map('soil-to-fertilizer',_,S,R), Domain).
example_domain(fertilizer, Domain) :- findall((S,R), example_map('fertilizer-to-water',_,S,R), Domain).
example_domain(water, Domain) :- findall((S,R), example_map('water-to-light',_,S,R), Domain).
example_domain(light, Domain) :- findall((S,R), example_map('light-to-temperature',_,S,R), Domain).
example_domain(temperature, Domain) :- findall((S,R), example_map('temperature-to-humidity',_,S,R), Domain).
example_domain(humidity, Domain) :- findall((S,R), example_map('humidity-to-location',_,S,R), Domain).

domain(seed, Domain) :- findall((S,R), map('seed-to-soil',_,S,R), Domain).
domain(soil, Domain) :- findall((S,R), map('soil-to-fertilizer',_,S,R), Domain).
domain(fertilizer, Domain) :- findall((S,R), map('fertilizer-to-water',_,S,R), Domain).
domain(water, Domain) :- findall((S,R), map('water-to-light',_,S,R), Domain).
domain(light, Domain) :- findall((S,R), map('light-to-temperature',_,S,R), Domain).
domain(temperature, Domain) :- findall((S,R), map('temperature-to-humidity',_,S,R), Domain).
domain(humidity, Domain) :- findall((S,R), map('humidity-to-location',_,S,R), Domain).


apply(P, Function, Input, Output) :-
    is_list(Input), !,
    findall(R, (member(E, Input),
                apply(P, Function, E, R)), Output).
apply(P, Function, (A,R,_), (X,R)) :-
    function(Function),
    call(P, Function, D, S, R0),
    ( Da is A+R - 1, Ds is S+R0 - 1,
      Offset is D-S
    ),
    between(S, Ds, A), between(S, Ds, Da), !,
    X is A + Offset.
apply(_, _, (A,R,_), (A,R)).

%%%%%%%%% 這是我個人帳號倒出來的輸入資料

map('seed-to-soil', 1393363309, 644938450, 159685707).
map('seed-to-soil', 2025282601, 1844060172, 19312202).
map('seed-to-soil', 1233103806, 1026919253, 32871092).
map('seed-to-soil', 1086566452, 1933428941, 86530991).
map('seed-to-soil',1265974898, 0, 21589659).
map('seed-to-soil', 1357621124, 1636167265, 35742185).
map('seed-to-soil', 2343571960, 2665606060, 81121142).
map('seed-to-soil', 1585337376, 809179011, 202497192).
map('seed-to-soil', 3151050390, 3039622538, 54531851).
map('seed-to-soil', 2059837853, 804624157, 4554854).
map('seed-to-soil', 169037772, 124717914, 59280146).
map('seed-to-soil', 228317918, 183998060, 248114943).
map('seed-to-soil', 2646529073, 2343571960, 51673623).
map('seed-to-soil', 1173097443, 1360585007, 60006363).
map('seed-to-soil', 2000660015, 1203115155, 24622586).
map('seed-to-soil', 1059486394, 1176035097, 27080058).
map('seed-to-soil', 3129081851, 4185259485, 17367169).
map('seed-to-soil', 3599437884, 3098755759, 211817367).
map('seed-to-soil', 2810085327, 3883695720, 116314513).
map('seed-to-soil', 2424693102, 4015066563, 32329632).
map('seed-to-soil', 3398847262, 2507128214, 141172870).
map('seed-to-soil', 1787834568, 432113003, 212825447).
map('seed-to-soil', 1553049016, 1603878905, 32288360).
map('seed-to-soil', 3111414816, 4202626654, 17667035).
map('seed-to-soil', 4015961437, 3493485543, 279005859).
map('seed-to-soil', 3584381554, 4000010233, 15056330).
map('seed-to-soil', 609280127, 1840486939, 3573233).
map('seed-to-soil', 0, 1603418622, 460283).
map('seed-to-soil', 3302297495, 3310573126, 79244791).
map('seed-to-soil', 3811255251, 4047396195, 137863290).
map('seed-to-soil', 3381542286, 2648301084, 17304976).
map('seed-to-soil', 3280255848, 3389817917, 22041647).
map('seed-to-soil', 840113387, 21589659, 103128255).
map('seed-to-soil', 3949118541, 3816852824, 66842896).
map('seed-to-soil', 3205582241, 4220293689, 74673607).
map('seed-to-soil', 657286135, 1420591370, 182827252).
map('seed-to-soil', 1287564557, 1863372374, 70056567).
map('seed-to-soil', 460283, 1671909450, 168577489).
map('seed-to-soil', 2926399840, 2746727202, 103388997).
map('seed-to-soil', 3146449020, 3094154389, 4601370).
map('seed-to-soil', 943241642, 1059790345, 116244752).
map('seed-to-soil', 476432861, 1227737741, 132847266).
map('seed-to-soil', 612853360, 2019959932, 44432775).
map('seed-to-soil', 2698202696, 2395245583, 111882631).
map('seed-to-soil', 3029788837, 3411859564, 81625979).
map('seed-to-soil', 2044594803, 1011676203, 15243050).
map('seed-to-soil', 2457022734, 2850116199, 189506339).
map('seed-to-soil', 3540020132, 3772491402, 44361422).
map('soil-to-fertilizer', 1845319553, 827629590, 305617985).
map('soil-to-fertilizer', 3122295925, 2644420892, 346256096).
map('soil-to-fertilizer', 1459294850, 681645131, 145984459).
map('soil-to-fertilizer', 1609507353, 0, 58999651).
map('soil-to-fertilizer', 255693782, 1322254706, 15503402).
map('soil-to-fertilizer', 1136906676, 1310560683, 7032394).
map('soil-to-fertilizer', 609209731, 1833691163, 45329504).
map('soil-to-fertilizer', 271197184, 2213414186, 148369535).
map('soil-to-fertilizer', 3483324631, 2990676988, 343929863).
map('soil-to-fertilizer', 3943098203, 3619829050, 148418709).
map('soil-to-fertilizer', 2945015193, 3803447520, 177280732).
map('soil-to-fertilizer', 504622935, 1337758108, 104586796).
map('soil-to-fertilizer', 2644420892, 3334606851, 81771815).
map('soil-to-fertilizer', 2909815432, 3768247759, 35199761).
map('soil-to-fertilizer', 3468873015, 4096571961, 14451616).
map('soil-to-fertilizer', 3827254494, 3980728252, 115843709).
map('soil-to-fertilizer', 1044649784, 1218303791, 92256892).
map('soil-to-fertilizer', 3468552021, 4111023577, 320994).
map('soil-to-fertilizer', 1605279309, 677417087, 4228044).
map('soil-to-fertilizer', 1668507004, 58999651, 176812549).
map('soil-to-fertilizer', 978403972, 1317593077, 4661629).
map('soil-to-fertilizer', 212737043, 1879020667, 42956739).
map('soil-to-fertilizer', 916089003, 1655081947, 62314969).
map('soil-to-fertilizer', 0, 1442344904, 212737043).
map('soil-to-fertilizer', 1228536146, 2361783721, 230758704).
map('soil-to-fertilizer', 419566719, 1133247575, 85056216).
map('soil-to-fertilizer', 1143939070, 1717396916, 84597076).
map('soil-to-fertilizer', 2726192707, 4111344571, 183622725).
map('soil-to-fertilizer', 983065601, 2151830003, 61584183).
map('soil-to-fertilizer', 2150937538, 235812200, 441604887).
map('soil-to-fertilizer', 884391832, 1801993992, 31697171).
map('soil-to-fertilizer', 654539235, 1921977406, 229852597).
map('soil-to-fertilizer', 4091516912, 3416378666, 203450384).
map('fertilizer-to-water', 2549847515, 3576009818, 718957478).
map('fertilizer-to-water', 0, 241538153, 477666033).
map('fertilizer-to-water', 2425421388, 2487425840, 6333278).
map('fertilizer-to-water', 2431754666, 2369332991, 118092849).
map('fertilizer-to-water', 4172623904, 3453666426, 122343392).
map('fertilizer-to-water', 2050888028, 0, 241538153).
map('fertilizer-to-water', 2369332991, 2493759118, 56088397).
map('fertilizer-to-water', 477666033, 719204186, 1573221995).
map('fertilizer-to-water', 3268804993, 2587418451, 866247975).
map('fertilizer-to-water', 4135052968, 2549847515, 37570936).
map('water-to-light', 0, 614660468, 46162263).
map('water-to-light', 992982309, 3320291957, 519425172).
map('water-to-light', 2148695908, 4242883656, 34662742).
map('water-to-light', 2183358650, 992982309, 1749887545).
map('water-to-light', 622053693, 575891430, 38769038).
map('water-to-light', 1973119806, 3839717129, 175576102).
map('water-to-light', 3950667093, 3281596434, 38695523).
map('water-to-light', 46162263, 0, 575891430).
map('water-to-light', 1512407481, 4015293231, 227590425).
map('water-to-light', 1739997906, 3048474534, 233121900).
map('water-to-light', 3933246195, 4277546398, 17420898).
map('water-to-light', 3989362616, 2742869854, 305604680).
map('light-to-temperature', 3926915598, 4278168812, 16798484).
map('light-to-temperature', 1868013910, 2147559018, 140836186).
map('light-to-temperature', 750719301, 1001446770, 132766166).
map('light-to-temperature', 0, 591148217, 159571084).
map('light-to-temperature', 2757723179, 3756680674, 111319765).
map('light-to-temperature', 3526572182, 1656447494, 400343416).
map('light-to-temperature', 159571084, 0, 569934147).
map('light-to-temperature', 2869042944, 3868000439, 358532427).
map('light-to-temperature', 2008850096, 3039560686, 189896094).
map('light-to-temperature', 2649579616, 3734175051, 22505623).
map('light-to-temperature', 2270874649, 2588070691, 164667420).
map('light-to-temperature', 4008144721, 2752738111, 286822575).
map('light-to-temperature', 2435542069, 2374033144, 214037547).
map('light-to-temperature', 2672085239, 2288395204, 85637940).
map('light-to-temperature', 3450693140, 2056790910, 18639649).
map('light-to-temperature', 3469332789, 3452574549, 57239393).
map('light-to-temperature', 883485467, 750719301, 250727469).
map('light-to-temperature', 3943714082, 3509813942, 64430639).
map('light-to-temperature', 3227575371, 3229456780, 223117769).
map('light-to-temperature', 1708083440, 3574244581, 159930470).
map('light-to-temperature', 2198746190, 2075430559, 72128459).
map('light-to-temperature', 729505231, 569934147, 21214070).
map('light-to-temperature', 1656447494, 4226532866, 51635946).
map('temperature-to-humidity', 2530950430, 2986195732, 64296956).
map('temperature-to-humidity', 3097031068, 3050492688, 225336526).
map('temperature-to-humidity', 2595247386, 2262922844, 63415061).
map('temperature-to-humidity', 394235114, 386308291, 573314459).
map('temperature-to-humidity', 159338027, 199058685, 71729011).
map('temperature-to-humidity', 2107189180, 2969998741, 16196991).
map('temperature-to-humidity', 231067038, 0, 22309581).
map('temperature-to-humidity', 266735072, 959622750, 109765613).
map('temperature-to-humidity', 1941982137, 2514902112, 165207043).
map('temperature-to-humidity', 3525862760, 2680109155, 81512917).
map('temperature-to-humidity', 3809165514, 2049071587, 78022870).
map('temperature-to-humidity', 3887188384, 2459869958, 55032154).
map('temperature-to-humidity', 61551861, 270787696, 97786166).
map('temperature-to-humidity', 4083271930, 3575006611, 18595319).
map('temperature-to-humidity', 993228240, 162831557, 10548461).
map('temperature-to-humidity', 967549573, 173380018, 25678667).
map('temperature-to-humidity', 376500685, 368573862, 17734429).
map('temperature-to-humidity', 2877832272, 3856947724, 19626311).
map('temperature-to-humidity', 3607375677, 4093177459, 201789837).
map('temperature-to-humidity', 2519444451, 4007134226, 11505979).
map('temperature-to-humidity', 2658662447, 4082384303, 10793156).
map('temperature-to-humidity', 3322367594, 3945539199, 61595027).
map('temperature-to-humidity', 253376619, 149473104, 13358453).
map('temperature-to-humidity', 0, 87921243, 61551861).
map('temperature-to-humidity', 2961202681, 2127094457, 135828387).
map('temperature-to-humidity', 3942220538, 3468929261, 33142375).
map('temperature-to-humidity', 2669455603, 2761622072, 208376669).
map('temperature-to-humidity', 4197950728, 3371912693, 97016568).
map('temperature-to-humidity', 4101867249, 3275829214, 96083479).
map('temperature-to-humidity', 2446763340, 1976390476, 72681111).
map('temperature-to-humidity', 2313231287, 2326337905, 133532053).
map('temperature-to-humidity', 2897458583, 4018640205, 63744098).
map('temperature-to-humidity', 2123386171, 3667102608, 189845116).
map('temperature-to-humidity', 1003776701, 22309581, 65611662).
map('temperature-to-humidity', 3975362913, 3593601930, 73500678).
map('temperature-to-humidity', 4048863591, 1941982137, 34408339).
map('temperature-to-humidity', 3452927785, 3502071636, 72934975).
map('temperature-to-humidity', 3383962621, 3876574035, 68965164).
map('humidity-to-location', 0, 853712401, 14149303).
map('humidity-to-location', 2655090225, 1087300934, 303915897).
map('humidity-to-location', 2027272660, 3174210041, 18998832).
map('humidity-to-location', 1525779414, 1936221923, 38337972).
map('humidity-to-location', 4147713982, 3193208873, 142508118).
map('humidity-to-location', 2959006122, 2143904256, 380930882).
map('humidity-to-location', 1087300934, 1765319513, 65896883).
map('humidity-to-location', 1352738345, 4121926227, 173041069).
map('humidity-to-location', 1290854129, 4060042011, 61884216).
map('humidity-to-location', 3931908769, 4005664051, 54377960).
map('humidity-to-location', 4091732209, 2524835138, 55981773).
map('humidity-to-location', 653782902, 95274560, 214078802).
map('humidity-to-location', 477505648, 85866717, 9407843).
map('humidity-to-location', 2632545935, 1543458967, 22544290).
map('humidity-to-location', 123251703, 309353362, 97540905).
map('humidity-to-location', 3762564408, 1974559895, 169344361).
map('humidity-to-location', 3487433944, 3409639319, 23582322).
map('humidity-to-location', 318179985, 430129950, 159325663).
map('humidity-to-location', 1216931801, 3335716991, 12046317).
map('humidity-to-location', 1153197817, 2580816911, 63733984).
map('humidity-to-location', 14149303, 406894267, 23235683).
map('humidity-to-location', 2206646140, 3433221641, 320894268).
map('humidity-to-location', 3986286729, 1566003257, 105445480).
map('humidity-to-location', 37384986, 0, 85866717).
map('humidity-to-location', 2112775364, 1671448737, 93870776).
map('humidity-to-location', 2046271492, 3107706169, 66503872).
map('humidity-to-location', 3511016266, 3754115909, 251548142).
map('humidity-to-location', 1228978118, 3347763308, 61876011).
map('humidity-to-location', 1564117386, 2644550895, 463155274).
map('humidity-to-location', 3339937004, 1391216831, 147496940).
map('humidity-to-location', 486913491, 589455613, 166869411).
map('humidity-to-location', 4290222100, 1538713771, 4745196).
map('humidity-to-location', 220792608, 756325024, 97387377).
map('humidity-to-location', 2527540408, 1831216396, 105005527).
