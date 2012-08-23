-module(semver).

-export([new/1, new/2, new/3, new/4,

         inc/2,
         inc_x/1,
         inc_y/1,
         inc_z/1,
         inc_tag/1,
         inc_tag/2,
         set_tag/2,
         to_str/1,
         to_tag/1,
         from_str/1,
         from_tag/1,

         from_git_describe/1,
         git_describe_to_dev_dist/1,

         compare/2
        ]).

-include_lib("erlsemver/include/semver.hrl").

new(X) ->
    #semver{x = X}.
new(X, Y) ->
    #semver{x = X, y = Y}.
new(X, Y, Z) ->
    #semver{x = X, y = Y, z = Z}.
new(X, Y, Z, Tag) ->
    #semver{x = X, y = Y, z = Z, tag = Tag}.

inc(V, tag) ->
    inc_tag(V, "dev0");
inc(V, {tag, Def}) ->
    inc_tag(V, Def);
inc(V, x) ->
    inc_x(V);
inc(V, y) ->
    inc_y(V);
inc(V, z) ->
    inc_z(V).

inc_x(V = #semver{x = X0}) ->
    V#semver{x = X0 + 1, y = 0, z = 0, tag = ""}.
inc_y(V = #semver{y = Y0}) ->
    V#semver{y = Y0 + 1, z = 0, tag = ""}.
inc_z(V = #semver{z = Z0}) ->
    V#semver{z = Z0 + 1, tag = ""}.

inc_tag(V = #semver{tag = ""}, Default) ->
    inc_tag(V#semver{tag = Default});
inc_tag(V = #semver{tag = _Tag}, _Default) ->
    inc_tag(V).

inc_tag(V = #semver{tag = Tag0}) ->
    {match, [Tag, Ver]} = re:run(Tag0,
                                 "(?<tag>[a-z][a-zA-Z]*)(?<tagver>\\d+)(-.*-.*)?\$",
                                 [{capture,['tag', 'tagver'], list}]),
    N = list_to_integer(Ver),
    N1 = N + 1,
    V#semver{tag = Tag++integer_to_list(N1)}.


set_tag(V, Tag) ->
    V#semver{tag = Tag}.

to_str(#semver{x = X, y = Y, z = Z, tag = Tag}) ->
    lists:flatten(io_lib:format("~b.~b.~b~s", [X, Y, Z, Tag])).

to_tag(#semver{x = X, y = Y, z = Z, tag = Tag}) ->
    lists:flatten(io_lib:format("v~b.~b.~b~s", [X, Y, Z, Tag])).

re_get(_Str, {-1, 0}) ->
    "";
re_get(Str, {S, L}) ->
    string:substr(Str, S+1, L).

strip_empty(Match) ->
    lists:reverse(strip_empty0(lists:reverse(Match))).
strip_empty0([{-1,0} | Match]) ->
    strip_empty0(Match);
strip_empty0(["" | Match]) ->
    strip_empty0(Match);
strip_empty0(Match) ->
    Match.

from_tag(Str) ->
    Re = "^v(?<major>\\d+)\.(?<minor>\\d+)\.(?<patchlevel>\\d+)?~?(?<special>[a-z]\\w+[\\d+](-[a-z0-9-]+)?)?\$",
    from_re(Str, Re).

from_str(Str) ->
    Re = "^v?(?<major>\\d+)\.(?<minor>\\d+)\.(?<patchlevel>\\d+)?~?(?<special>[a-z]\\w+[\\d+](-[a-z0-9-]+)?)?\$",
    from_re(Str, Re).

from_re(Str, Re) ->
    case re:run(Str, Re, [{capture,['major','minor','patchlevel','special']}]) of
        {match, Match0} ->
            case strip_empty(Match0) of
                [X0, Y0, Z0, Tag0] ->
                    #semver{x = list_to_integer(re_get(Str, X0)),
                            y = list_to_integer(re_get(Str, Y0)),
                            z = list_to_integer(re_get(Str, Z0)),
                            tag = re_get(Str, Tag0)};
                [X0, Y0, Z0] ->
                    #semver{x = list_to_integer(re_get(Str, X0)),
                            y = list_to_integer(re_get(Str, Y0)),
                            z = list_to_integer(re_get(Str, Z0))};
                [X0, Y0] ->
                    #semver{x = list_to_integer(re_get(Str, X0)),
                            y = list_to_integer(re_get(Str, Y0))}
            end;
        nomatch ->
            erlang:error(improper_version, Str)
    end.



compare(#semver{} = A, #semver{} = B) ->
    A < B.


%% =============================================================================
%%
%% GIT related verson handling
%%
%% =============================================================================

git_describe_to_dev_dist(#semver{tag = "dev"++Tag} = SM) ->
    Re = "^(?<ver>[0-9]+)(-(?<distance>[0-9]+)-(?<commit>[a-z0-9-]+))?\$",
    case re:run(Tag, Re, [{capture,[ver, distance, commit], list}]) of
        {match, [Ver, "", ""]} ->
            SM#semver{tag = "dev"++Ver};
        {match, [Ver, Distance, _]} ->
            SM#semver{tag = "dev"++integer_to_list((list_to_integer(Ver) + list_to_integer(Distance)))}
    end;
git_describe_to_dev_dist(#semver{tag = "git"++Tag} = SM) ->
    Re = "^(?<dist>\\d+)-",
    {match, [Dist]} = re:run(Tag, Re, [{capture,[dist], list}]),
    SM#semver{tag = "dev"++Dist};
git_describe_to_dev_dist(SM) ->
    SM.

analyze_git_describe_tag(Str) ->
    Re = "^(?<tag>[a-zA-z][a-zA-Z0-9-]+)?-(?<distance>[0-9]+)-(?<commit>[a-z0-9-]+)\$",
    case re:run(Str, Re, [{capture, ['tag', 'distance', 'commit'], list}]) of
        {match, ["", Dist, Commit]}  ->
            "git"++Dist++"-"++Commit;
        _ ->
            Str
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
                            tag = Tag};
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


