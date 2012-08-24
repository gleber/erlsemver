-module(semver).

-export([new/1, new/2, new/3, new/4, new/5,

         parse/1,

         get_x/1, get_y/1, get_z/1, get_tag/1, get_pre/1, get_build/1,

         inc/2,
         inc_x/1,
         inc_y/1,
         inc_z/1,

         to_str/1,
         to_tag/1,

         from_str/1,
         from_tag/1,
         from_git_describe/1,

         compare/2
        ]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlsemver/include/semver.hrl").

-type semver() :: #semver{}.
-type level() :: 'x' | 'y' | 'z' | 'pre' | 'build'.
-export_type([semver/0]).

%% =============================================================================
%%
%% API
%%
%% =============================================================================

-spec new(integer()) -> semver().
-spec new(integer(), integer()) -> semver().
-spec new(integer(), integer(), integer()) -> semver().
-spec new(integer(), integer(), integer(), string()) -> semver().
-spec new(integer(), integer(), integer(), string(), string()) -> semver().

new(X) when is_integer(X) ->
    #semver{x = X}.
new(X, Y) when is_integer(X),
               is_integer(Y) ->
    #semver{x = X, y = Y}.
new(X, Y, Z) when is_integer(X),
                  is_integer(Y),
                  is_integer(Z) ->
    #semver{x = X, y = Y, z = Z}.
new(X, Y, Z, ["-" | Pre]) ->
    (new(X, Y, Z))#semver{pre   = parse_pre(Pre)};
new(X, Y, Z, ["+" | Build]) ->
    (new(X, Y, Z))#semver{build = parse_build(Build)};
new(X, Y, Z, Pre) ->
    (new(X, Y, Z))#semver{pre   = parse_pre(Pre)}.
new(X, Y, Z, Pre, Build) ->
    (new(X, Y, Z))#semver{pre   = parse_pre(Pre),
                          build = parse_build(Build)}.

-spec parse(string()) -> {'ok', semver()} | {'error', atom()}.
parse(S) ->
    try
        {ok, parse0(S)}
    catch
        error:{improper_version, _Str} ->
            {error, badarg}
    end.

-spec from_str(string()) -> semver().
from_str(Str) ->
    Re = "^v?(?<major>\\d+)\.(?<minor>\\d+)\.(?<patchlevel>\\d+)(?<pre>-[0-9A-Za-z-\.]+)?(?<build>\\+[0-9A-Za-z-\.]+)?\$",
    from_re(Str, Re).

-spec from_tag(string()) -> semver().
from_tag(Str) ->
    Re = "^v(?<major>\\d+)\.(?<minor>\\d+)\.(?<patchlevel>\\d+)(?<pre>-[0-9A-Za-z-\.]+)?(?<build>\\+[0-9A-Za-z-\.]+)?\$",
    from_re(Str, Re).

-spec get_x(semver()) -> integer().
get_x(#semver{x = X}) -> X.

-spec get_y(semver()) -> integer().
get_y(#semver{y = Y}) -> Y.

-spec get_z(semver()) -> integer().
get_z(#semver{z = Z}) -> Z.

-spec get_tag(semver()) -> string().
get_tag(#semver{} = V) -> to_str1("", V).

-spec get_pre(semver()) -> string().
get_pre(#semver{pre = P}) -> stringify_tag(P).

-spec get_build(semver()) -> string().
get_build(#semver{build = B}) -> stringify_tag(B).

-spec inc(semver(), level()) -> semver().
inc(V, x) ->
    inc_x(V);
inc(V, y) ->
    inc_y(V);
inc(V, z) ->
    inc_z(V).

-spec inc_x(semver()) -> semver().
inc_x(V = #semver{x = X0}) ->
    V#semver{x = X0 + 1, y = 0, z = 0, pre = ?UND_PRE, build = ?UND_BUILD}.

-spec inc_y(semver()) -> semver().
inc_y(V = #semver{y = Y0}) ->
    V#semver{y = Y0 + 1, z = 0, pre = ?UND_PRE, build = ?UND_BUILD}.

-spec inc_z(semver()) -> semver().
inc_z(V = #semver{z = Z0}) ->
    V#semver{z = Z0 + 1, pre = ?UND_PRE, build = ?UND_BUILD}.

-spec to_str(semver()) -> string().
to_str(#semver{x = X, y = Y, z = Z} = SV) ->
    to_str1(lists:flatten(io_lib:format("~b.~b.~b", [X, Y, Z])), SV).

-spec to_tag(semver()) -> string().
to_tag(#semver{} = SV) ->
    [$v | to_str(SV)].

-spec compare(semver(), semver()) -> boolean().
compare(#semver{} = A, #semver{} = B) ->
    A < B.

from_git_describe(Str) ->
    Re = "^v?(?<major>\\d+)\.(?<minor>\\d+)\.(?<patchlevel>\\d+)?(?<other>.*)",
    case  re:run(Str, Re, [{capture,['major','minor','patchlevel', 'other'], list}]) of
        {match, Match0} ->
            case strip_empty(Match0) of
                [X0, Y0, Z0, All] ->
                    Tag = analyze_git_describe_tag(All),
                    #semver{x = list_to_integer(X0),
                            y = list_to_integer(Y0),
                            z = list_to_integer(Z0),
                            build = intify(Tag)};
                [X0, Y0, Z0] ->
                    #semver{x = list_to_integer(X0),
                            y = list_to_integer(Y0),
                            z = list_to_integer(Z0)};
                [X0, Y0] ->
                    #semver{x = list_to_integer(X0),
                            y = list_to_integer(Y0)}
            end;
        nomatch ->
            erlang:error(improper_version, Str)
    end.

%% =============================================================================
%%
%% Internal
%%
%% =============================================================================

analyze_git_describe_tag(Str) ->
    Re = "^(?<tag>[a-zA-z][a-zA-Z0-9-]+)?-(?<distance>[0-9]+)-(?<commit>[a-z0-9-]+)\$",
    case re:run(Str, Re, [{capture, ['tag', 'distance', 'commit'], list}]) of
        {match, ["", Dist, Commit]}  ->
            ["git", Dist, Commit];
        _ ->
            [Str]
    end.

parse0([$v | S]) ->
    from_tag([$v | S]);
parse0(S) ->
    from_str(S).

to_str1(Str, #semver{pre = ?UND_PRE} = SV) ->
    to_str2(Str, SV);
to_str1(Str, #semver{pre = Pre} = SV) ->
    to_str2(Str++"-"++stringify_tag(Pre), SV).
to_str2(Str, #semver{build = ?UND_BUILD}) ->
    Str;
to_str2(Str, #semver{build = Build}) ->
    Str++"+"++stringify_tag(Build).

stringify_tag(?UND_PRE) -> undefined;
stringify_tag(?UND_BUILD) -> undefined;
stringify_tag(L) ->
    string:join(stringify_tag0(L), ".").

stringify_tag0([]) ->
    [];
stringify_tag0([A|T]) when is_integer(A) ->
    [integer_to_list(A) | stringify_tag0(T)];
stringify_tag0([A|T]) when is_list(A) ->
    [A | stringify_tag0(T)].

parse_pre(P) when is_list(P) ->
    und(parse_tag(string:strip(P, left, $-))).
parse_build(B) when is_list(B)  ->
    und2(parse_tag(string:strip(B, left, $+))).

parse_tag(T) ->
    intify(string:tokens(T, ".")).

intify([]) ->
    [];
intify([A | T]) ->
    A2 = try
             list_to_integer(A)
         catch
             _:_ ->
                 A
         end,
    [A2 | intify(T)].

und([]) ->
    ?UND_PRE;
und(X) ->
    X.
und2([]) ->
    ?UND_BUILD;
und2(X) ->
    X.

from_re(Str, Re) ->
    case re:run(Str, Re, [{capture,['major','minor','patchlevel','pre','build'],list}]) of
        {match, [X0, Y0, Z0, Pre, Build]} ->
            #semver{x = list_to_integer(X0),
                    y = list_to_integer(Y0),
                    z = list_to_integer(Z0),
                    pre = parse_pre(Pre),
                    build = parse_build(Build)};
        nomatch ->
            erlang:error({improper_version, Str})
    end.

strip_empty(Match) ->
    lists:reverse(strip_empty0(lists:reverse(Match))).
strip_empty0([{-1,0} | Match]) ->
    strip_empty0(Match);
strip_empty0(["" | Match]) ->
    strip_empty0(Match);
strip_empty0(Match) ->
    Match.

%% =============================================================================
%%
%% Tests
%%
%% =============================================================================

test_vers_str() ->
    "1.0.0-alpha 1.0.0-alpha.1 1.0.0-beta.2 1.0.0-beta.11 1.0.0-rc.1 1.0.0-rc.1+build.1 1.0.0 1.0.0+0.3.7 1.3.7+build 1.3.7+build.2.b8f12d7 1.3.7+build.11.e0f985a".

parse_test() ->
    Strings0 = test_vers_str(),
    Strings = string:tokens(Strings0, " "),
    Vers = [ semver:from_str(S) || S <- Strings ],
    Pairs = lists:zip(Strings, Vers),
    [ {S, S, V} = {S, semver:to_str(V), V} || {S, V} <- Pairs ],
    Strings0 = string:join([ semver:to_str(V) || V <- Vers ], " ").

compare_test() ->
    Strings = test_vers_str(),
    Vers = [ semver:from_str(S) || S <- string:tokens(Strings, " ") ],
    {Correct, Actual} = lists:unzip(pairwise_compare(Vers)),
    Correct = Actual.

pairwise_compare([A, B]) ->
    [{{true, A, B}, {compare(A, B), A, B}}];
pairwise_compare([A, B | T]) ->
    [{{true, A, B}, {compare(A, B), A, B}} | pairwise_compare([B|T])].
