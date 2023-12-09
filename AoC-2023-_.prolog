
:- module('AoC-2023-_', [input/3]).

%% 公用程式
%%
%% 載入這個程式，並載入任一天相應模組，
%% 接著輸入（例如）
%%
%% ?- input(1, 2, Input).
%%
%% 意思要載入檔案 AoC-2023-day-1.input-2 。
%%
%% 如果沒載入模組，系統回應 "Module AoC-2023-day-N did not loaded."
%% 如果輸入檔不存在，系統回應 "ERROR: source_sink ..."

input(Day, Seq, Input) :-
    atom_concat('AoC-2023-day-', Day, Module_name),
    module_property(Module_name, file(File)), !,
    file_name_extension(Base, '.prolog', File),
    atom_concat('input-', Seq, Ext),
    file_name_extension(Base, Ext, File2),
    read_file_to_string(File2, Input, []).
input(Day, _Seq, _Input) :-
    atom_concat('AoC-2023-day-', Day, Module_name),
    format("Module ~w did not loaded.~n", [Module_name]).
