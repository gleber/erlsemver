erlsemver
=========

Semantic Versioning parsing library compatible with Semantic
Versioning 2.0.0-rc.1. Uses tricky internal representation which
allows for direct comparison of #semver{} records.

Usage
=====

Parsing:
```erlang
1> rr("include/*").
[semver]
2> semver:parse("1.0.0").
{ok, #semver{x = 1,y = 0,z = 0,pre = <<>>,build = undefined}}
3> semver:parse("1-incorrect version string").
{error, badarg}
4> semver:from_str("1.0.0-rc.1+build.1").
#semver{x = 1,y = 0,z = 0,
        pre = ["rc",1],
        build = ["build",1]}
```

Please note that git describe output actually generates a build type
version string (see 11 in SemVer specification), not a pre-release as
string's format might suggest. Hence there's a separate call for it:
```erlang
5) semver:from_git_describe("v0.3.1-1-gc32977c").
#semver{x = 0,y = 3,z = 1,pre = <<>>,
        build = ["git",1,"gc32977c"]}
6) semver:to_str(semver:from_git_describe("v0.3.1-1-gc32977c")).
"0.3.1+git.1.gc32977c"
```


To string:
```erlang
1> semver:to_str(#semver{x = 1,y = 0,z = 0, pre = ["rc",1], build = ["build",1]}).
"1.0.0-rc.1+build.1"
```

Comparing:
```erlang
1> semver:from_str("1.0.0-rc.1+build.1") < semver:from_str("1.0.0").
true
2> semver:compare(semver:from_str("1.0.0-rc.1+build.1"), semver:from_str("1.0.0")).
true
```

Getting tags of current git repositry:

```erlang
1> rr("include/*").
[semver]
2> [ semver:from_tag(T) || T <- string:tokens(os:cmd("git tag"), "\n") ].
[#semver{x = 0,y = 4,z = 0,pre = <<>>,build = undefined},
 #semver{x = 0,y = 5,z = 0,pre = <<>>,build = undefined}]
```

Author
======
Gleb Peregud <gleber.p@gmail.com> for LivePress Inc.

Copyright 2011-2012 LivePress Inc.

License
=======

MIT license
