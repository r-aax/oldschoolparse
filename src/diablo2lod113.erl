%% @doc
%% Diablo II Lord of Destruction (v 1.13).

% Module name.
-module(diablo2lod113).

% Include.
-include("defines.hrl").

% Print prefix.
-define(PF, "diablo2lod113 : ").

% Export functions.
-export([compare/2,
         edit/2,
         start/0]).

%---------------------------------------------------------------------------------------------------
% Functions.
%---------------------------------------------------------------------------------------------------

%% @doc
%% Compare two files.
%%   Filename1 - first name,
%%   Filename2 - second name.
compare(Filename1, Filename2) ->
    io:format(?PF ++ "compare ~s and ~s files:~n", [Filename1, Filename2]),
    ?OK(Bin1) = file:read_file(Filename1),
    ?OK(Bin2) = file:read_file(Filename2),
    compare_binaries(Bin1, Bin2, {0}).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Compare binaries.
%%   Bin1 - first binary,
%%   Bin2 - second binary,
%%   Stat - statistics.
compare_binaries(<<>>, <<>>, {Pos}) ->
    io:format(?PF ++ "compare results : final position = ~w.~n", [Pos]);
compare_binaries(<<B1:?BYTE, Rest1/binary>>,
                 <<B2:?BYTE, Rest2/binary>>,
                 {Pos}) ->
    if
        B1 /= B2 ->
            io:format(?PF ++ "diff bytes in pos ~w : ~w vs. ~w~n", [Pos, B1, B2]);
        true ->
            ok
    end,
    compare_binaries(Rest1, Rest2, {Pos + 1}).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Edit file.
%%   Filename - name of file,
%%   Map - map of changes.
edit(Filename, Map) ->
    io:format(?PF ++ "edit file ~s:~n", [Filename]),
    ?OK(Bin) = file:read_file(Filename),
    NewBin = list_to_binary(edit_bytes_list(binary_to_list(Bin), Map)),
    ChSumBin = correct_check_sum(NewBin),
    file:write_file(Filename ++ ".new", ChSumBin).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Edit bytes list.
%%   L - list,
%%   M - map of changes.
edit_bytes_list(L, []) ->
    io:format(?PF ++ "edit file completed."),
    L;
edit_bytes_list(L, [H | T]) ->
    NewL = edit_bytes_list_step(L, H),
    edit_bytes_list(NewL, T).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Edit bytes list (one step).
%%   L - bytes list,
%%   E - single element of modification.
edit_bytes_list_step(L, reset_stats) ->

    % After this you can go to Acara and reset you stats again.
    % Taken from https://github.com/sohan/Diablo-2-Hero-Editor-Hacks
    io:format(?PF ++ "reset_stats~n"),
    edit_bytes_list_step(L, {427, 2});

edit_bytes_list_step(L, {Pos, Value}) when is_integer(Pos) ->

    % Direct value in position.
    io:format(?PF ++ "set value ~w in position ~w~n", [Value, Pos]),
    {F, [_ | T]} = lists:split(Pos, L),
    F ++ [Value] ++ T;

edit_bytes_list_step(L, E) ->
    io:format(?PF ++ "unknown element of edit : ~w~n", [E]),
    L.

%---------------------------------------------------------------------------------------------------

%% @doc
%% ROL of 32-bit value.
%%   X - value.
rol32(X) ->
    ((X band 16#7FFFFFFF) bsl 1) bor ((X bsr 31) band 1).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Correct checksum.
%%   Bin - binary.
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
    edit("/home/alex/Data/Shared/Save/Mephala.d2s",
         [reset_stats]),
    compare("/home/alex/Data/Shared/Save/Mephala.d2s",
            "/home/alex/Data/Shared/Save/Mephala.d2s.new"),
    halt().

%---------------------------------------------------------------------------------------------------
