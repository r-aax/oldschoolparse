%% @doc
%% Diablo II Lord of Destruction (v 1.13).

% Module name.
-module(diablo2lod113).

% Include.
-include("defines.hrl").

% Export functions.
-export([rol32/1,
         compare/3,
         edit/2,
         start/0]).

%---------------------------------------------------------------------------------------------------
% Functions.
%---------------------------------------------------------------------------------------------------

%% @doc
%% Compare files of game saves.
%% Compare following files:
%%   *.map
%%   *.ma0
%%   *.ma1
%%   *.key
%%   *.d2s
%% Arguments:
%%   Dir1 - first directory,
%%   Dir2 - second directory,
%%   Basename - basename of save files.
compare(Dir1, Dir2, Basename) ->
    io:format("diablo2lod113 : compare ~s, ~s, basename ~s:~n", [Dir1, Dir2, Basename]),
    Extensions = ["map", "ma0", "ma1", "key", "d2s"],
    lists:map
    (
        fun(E) ->
            compare(Dir1 ++ "/" ++ Basename ++ "." ++ E,
                    Dir2 ++ "/" ++ Basename ++ "." ++ E)
        end,
        Extensions
    ).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Compare two files.
%%   Name1 - first name,
%%   Name2 - second name.
compare(Name1, Name2) ->
    io:format("diablo2lod113 : compare ~s and ~s files:~n", [Name1, Name2]),
    ?OK(Bin1) = file:read_file(Name1),
    ?OK(Bin2) = file:read_file(Name2),
    compare_binaries(Bin1, Bin2, {0}).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Compare binaries.
%%   Bin1 - first binary,
%%   Bin2 - second binary,
%%   Stat - statistics.
compare_binaries(<<>>, <<>>, {Pos}) ->
    io:format("diablo2lod113 : compare results : final position = ~w.~n", [Pos]);
compare_binaries(<<B1:?BYTE, Rest1/binary>>,
                 <<B2:?BYTE, Rest2/binary>>,
                 {Pos}) ->
    if
        B1 /= B2 ->
            io:format("diablo2lod113 : diff bytes in pos ~w : ~w vs. ~w~n", [Pos, B1, B2]);
        true ->
            ok
    end,
    compare_binaries(Rest1, Rest2, {Pos + 1}).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Edit file.
%%   Name - name of file,
%%   Map - map of changes.
edit(Name, Map) ->
    io:format("diablo2lod113 : edit file ~s~n", [Name]),
    ?OK(Bin) = file:read_file(Name),
    NewBin = list_to_binary(edit_bytes_list(binary_to_list(Bin), Map)),
    ChSumBin = correct_check_sum(NewBin),
    file:write_file(Name ++ ".new", ChSumBin).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Edit bytes list.
%%   L - list,
%%   M - map of changes.
edit_bytes_list(L, []) ->
    L;
edit_bytes_list(L, [H | T]) ->
    NewL = single_edit_bytes_list(L, H),
    edit_bytes_list(NewL, T).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Single edit bytes list.
%%   L - bytes list,
%%   E - single element of modification.
single_edit_bytes_list(L, {Pos, Value}) when is_integer(Pos) ->

    % Direct value in position.
    io:format("diablo2lod113 : set value ~w in position ~w~n", [Value, Pos]),
    {F, [_ | T]} = lists:split(Pos, L),
    F ++ [Value] ++ T;

single_edit_bytes_list(L, _) ->
    L.

%---------------------------------------------------------------------------------------------------

%% @doc
%% ROL of 32-bit value.
rol32(X) ->
    ((X band 16#7FFFFFFF) bsl 1) + ((X bsr 31) band 1).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Correct checksum.
correct_check_sum(Bin) ->
    L = binary_to_list(Bin),

    % Zero 4 bytes of checksum.
    {F, [_, _, _, _ | T]} = lists:split(12, L),
    ZL = F ++ [0, 0, 0, 0] ++ T,

    % Calculate checksum.
    CS =
        lists:foldl
        (
            fun(B, S) ->
                (rol32(S) + B) band 16#FFFFFFFF
            end,
            0,
            ZL
        ),

    % Insert checksum.
    FB = list_to_binary(F),
    TB = list_to_binary(T),
    <<FB/binary, CS:?LONG, TB/binary>>.

%---------------------------------------------------------------------------------------------------

%% @doc
%% Start function.
start() ->
    compare("/home/alex/Data/Shared/Save",
            "/home/alex/Data/Shared/Save/old",
            "Mephala"),
    edit("/home/alex/Data/Shared/Save/Mephala.d2s",
         []),
    compare("/home/alex/Data/Shared/Save/Mephala.d2s",
            "/home/alex/Data/Shared/Save/Mephala.d2s.new"),
    halt().

%---------------------------------------------------------------------------------------------------
