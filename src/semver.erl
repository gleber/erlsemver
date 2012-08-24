-module(semver).

-export([new/1, new/2, new/3, new/4, new/5,

         parse/1,

         inc/2,
         inc_x/1,
         inc_y/1,
         inc_z/1,

         to_str/1,
         to_tag/1,

         from_str/1,
         from_tag/1,
         from_git_describe/1,

         git_describe_to_dev_dist/1,

         compare/2
        ]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlsemver/include/semver.hrl").

%% =============================================================================
%%
%% API
%%
%% =============================================================================

new(X) ->
    #semver{x = X}.
new(X, Y) ->
    #semver{x = X, y = Y}.
new(X, Y, Z) ->
    #semver{x = X, y = Y, z = Z}.
new(X, Y, Z, ["-" | Pre]) ->
    #semver{x = X, y = Y, z = Z, pre = parse_pre(Pre)};
new(X, Y, Z, ["+" | Build]) ->
    #semver{x = X, y = Y, z = Z, build = parse_build(Build)};
new(X, Y, Z, Pre) ->
    #semver{x = X, y = Y, z = Z, pre = parse_pre(Pre)}.
new(X, Y, Z, Pre, Build) ->
    #semver{x = X, y = Y, z = Z, pre = parse_pre(Pre), build = parse_build(Build)}.

inc(V, x) ->
    inc_x(V);
inc(V, y) ->
    inc_y(V);
inc(V, z) ->
    inc_z(V).

inc_x(V = #semver{x = X0}) ->
    V#semver{x = X0 + 1, y = 0, z = 0, pre = ?UND_PRE, build = ?UND_BUILD}.
inc_y(V = #semver{y = Y0}) ->
    V#semver{y = Y0 + 1, z = 0, pre = ?UND_PRE, build = ?UND_BUILD}.
inc_z(V = #semver{z = Z0}) ->
    V#semver{z = Z0 + 1, pre = ?UND_PRE, build = ?UND_BUILD}.

%% inc_tag(V = #semver{tag = ""}, Default) ->
%%     inc_tag(V#semver{tag = Default});
%% inc_tag(V = #semver{tag = _Tag}, _Default) ->
%%     inc_tag(V).

%% inc_tag(V = #semver{tag = Tag0}) ->
%%     {match, [Tag, Ver]} = re:run(Tag0,
%%                                  "(?<tag>[a-z][a-zA-Z]*)(?<tagver>\\d+)(-.*-.*)?\$",
%%                                  [{capture,['tag', 'tagver'], list}]),
%%     N = list_to_integer(Ver),
%%     N1 = N + 1,
%%     V#semver{tag = Tag++integer_to_list(N1)}.

%% set_tag(V, Tag) ->
%%     V#semver{tag = Tag}.

to_str(#semver{x = X, y = Y, z = Z} = SV) ->
    to_str1(lists:flatten(io_lib:format("~b.~b.~b", [X, Y, Z])), SV).
to_str1(Str, #semver{pre = ?UND_PRE} = SV) ->
    to_str2(Str, SV);
to_str1(Str, #semver{pre = Pre} = SV) ->
    to_str2(Str++"-"++stringy_tag(Pre), SV).
to_str2(Str, #semver{build = ?UND_BUILD}) ->
    Str;
to_str2(Str, #semver{build = Build}) ->
    Str++"+"++stringy_tag(Build).

to_tag(#semver{} = SV) ->
    [$v | to_str(SV)].

parse(S) ->
    try
        {ok, parse0(S)}
    catch
        error:{improper_version, _Str} ->
            {error, badarg}
    end.

parse0([$v | S]) ->
    from_tag([$v | S]);
parse0(S) ->
    from_str(S).

from_tag(Str) ->
    Re = "^v(?<major>\\d+)\.(?<minor>\\d+)\.(?<patchlevel>\\d+)(?<pre>-[0-9A-Za-z-\.]+)?(?<build>\\+[0-9A-Za-z-\.]+)?\$",
    from_re(Str, Re).

from_str(Str) ->
    Re = "^v?(?<major>\\d+)\.(?<minor>\\d+)\.(?<patchlevel>\\d+)(?<pre>-[0-9A-Za-z-\.]+)?(?<build>\\+[0-9A-Za-z-\.]+)?\$",
    from_re(Str, Re).


compare(#semver{} = A, #semver{} = B) ->
    A < B.

%% =============================================================================
%%
%% Internal
%%
%% =============================================================================

stringy_tag(L) ->
    string:join(stringy_tag0(L), ".").

stringy_tag0([]) ->
    [];
stringy_tag0([A|T]) when is_integer(A) ->
    [integer_to_list(A) | stringy_tag0(T)];
stringy_tag0([A|T]) when is_list(A) ->
    [A | stringy_tag0(T)].

parse_pre(P) ->
    und(parse_tag(string:strip(P, left, $-))).
parse_build(B) ->
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
%% GIT related verson handling
%%
%% =============================================================================

git_describe_to_dev_dist(#semver{build = "dev"++Tag} = SM) ->
    Re = "^(?<ver>[0-9]+)(-(?<distance>[0-9]+)-(?<commit>[a-z0-9-]+))?\$",
    case re:run(Tag, Re, [{capture,[ver, distance, commit], list}]) of
        {match, [Ver, "", ""]} ->
            SM#semver{build = "dev"++Ver};
        {match, [Ver, Distance, _]} ->
            SM#semver{build = "dev"++integer_to_list((list_to_integer(Ver) + list_to_integer(Distance)))}
    end;
git_describe_to_dev_dist(#semver{build = "git"++Tag} = SM) ->
    Re = "^(?<dist>\\d+)-",
    {match, [Dist]} = re:run(Tag, Re, [{capture,[dist], list}]),
    SM#semver{build = "dev"++Dist};
git_describe_to_dev_dist(SM) ->
    SM.

analyze_git_describe_tag(Str) ->
    Re = "^(?<tag>[a-zA-z][a-zA-Z0-9-]+)?-(?<distance>[0-9]+)-(?<commit>[a-z0-9-]+)\$",
    case re:run(Str, Re, [{capture, ['tag', 'distance', 'commit'], list}]) of
        {match, ["", Dist, Commit]}  ->
            ["git", Dist, Commit];
        _ ->
            [Str]
    end.

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
