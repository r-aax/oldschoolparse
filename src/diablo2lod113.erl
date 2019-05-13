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
         inspect/1,
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
    io:format(?PF ++ "COMPARE ~s and ~s files:~n", [Filename1, Filename2]),
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
    io:format(?PF ++ "compare completed : final position = ~w.~n~n", [Pos]);
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
%% Inspect file.
%%   Filename - name of file.
inspect(Filename) ->
    io:format(?PF ++ "INSPECT ~s file:~n", [Filename]),
    ?OK(Bin) = file:read_file(Filename),
    inspect_binary(Bin, 0).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Inspect binary.
%%   Bin - binary,
%%   Pos - position.
%% Format description is taken from
%%   https://github.com/krisives/d2s-format
inspect_binary(<<16#AA55AA55:?LONG, Rest/binary>>, 0) ->
    inspect_binary(Rest, 16#4);
inspect_binary(<<V:?LONG, Rest/binary>>, 16#4) ->
    Str =
        case V of
            71 ->
                "v1.00 through v1.06";
            87 ->
                "v1.07 or Expansion Set v1.08";
            89 ->
                "standard game v1.08";
            92 ->
                "v1.09, both the standard game and the Expansion Set";
            96 ->
                "v1.10+";
            _ ->
                "unknown version"
        end,
    io:format(?PF ++ "inspect : version = ~s (~w)~n", [Str, V]),
    inspect_binary(Rest, 16#8);
inspect_binary(<<S:?LONG, CS:?LONG, Rest/binary>>, 16#8) ->
    io:format(?PF ++ "inspect : size = ~w, checksum = ~.16x~n", [S, CS, "0x"]),
    inspect_binary(Rest, 16#10);
inspect_binary(<<_:4/binary, Rest/binary>>, 16#10) ->
    % TODO
    % Active Weapon.
    inspect_binary(Rest, 16#14);
inspect_binary(<<Name:16/binary, Rest/binary>>, 16#14) ->
    io:format(?PF ++ "inspect : name = ~s~n", [binary_to_list(Name)]),
    inspect_binary(Rest, 16#24);
inspect_binary(<<_:4/binary, Rest/binary>>, 16#24) ->
    % TODO
    % Character Status, Character Progression, 2 unknown bytes.
    inspect_binary(Rest, 16#28);
inspect_binary(<<C:?BYTE, Rest/binary>>, 16#28) ->
    Class =
        case C of
            0 ->
                "Amazon";
            1 ->
                "Sorceress";
            2 ->
                "Necromancer";
            3 ->
                "Paladin";
            4 ->
                "Barbarian";
            5 ->
                "Druid";
            6 ->
                "Assassin";
            _ ->
                "unknown class"
        end,
    io:format(?PF ++ "inspect : class = ~s~n", [Class]),
    inspect_binary(Rest, 16#29);
inspect_binary(<<_:2/binary, Rest/binary>>, 16#29) ->
    % TODO
    % 2 unknown bytes.
    inspect_binary(Rest, 16#2B);
inspect_binary(<<L:?BYTE, Rest/binary>>, 16#2B) ->
    io:format(?PF ++ "inspect : level = ~w~n", [L]),
    inspect_binary(Rest, 16#2C);
inspect_binary(<<HK:64/binary, Rest/binary>>, 16#38) ->
    FF =
        fun
            F([], R) ->
                lists:reverse(R);
            F([H1, H2, H3, H4 | T], R) ->
                F(T, [[H1, H2, H3, H4] | R])
        end,
    G = FF(binary_to_list(HK), []),
    QuickInfo =
        lists:map
        (
            fun
                ([16#FF, 16#FF, 0, 0]) ->
                    0;
                (_) ->
                    1
            end,
            G
        ),
    io:format(?PF ++ "inspect : hotkeys = ~w~n", [QuickInfo]),
    inspect_binary(Rest, 16#78);
inspect_binary(<<_:?BYTE, Rest/binary>>, Pos) ->
    inspect_binary(Rest, Pos + 1);
inspect_binary(<<>>, Pos) ->
    io:format(?PF ++ "inspect completed : final position = ~w.~n~n", [Pos]).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Edit file.
%%   Filename - name of file,
%%   Map - map of changes.
edit(Filename, Map) ->
    io:format(?PF ++ "EDIT file ~s:~n", [Filename]),
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
    io:format(?PF ++ "edit file completed.~n~n"),
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
    D2S = "/home/alex/Data/Shared/Save/Mephala.d2s",
    inspect(D2S),
    edit(D2S,
         [reset_stats]),
    compare(D2S, D2S ++ ".new"),
    halt().

%---------------------------------------------------------------------------------------------------
