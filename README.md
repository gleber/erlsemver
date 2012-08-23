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
2> semver:from_str("1.0.0-rc.1+build.1").
#semver{x = 1,y = 0,z = 0,
        pre = ["rc",1],
        build = ["build",1]}
3> semver:to_str(#semver{x = 1,y = 0,z = 0, pre = ["rc",1], build = ["build",1]}).
"1.0.0-rc.1+build.1"
```

Compare:
```erlang
1> semver:from_str("1.0.0-rc.1+build.1") < semver:from_str("1.0.0").
true
2> semver:compare(semver:from_str("1.0.0-rc.1+build.1"), semver:from_str("1.0.0")).
true
```

Get tags of current git repositry:

```erlang
1> rr("include/*").
[semver]
2> [ semver:from_tag(T) || T <- string:tokens(os:cmd("git tag"), "\n") ].
[#semver{x = 0,y = 4,z = 0,tag = []},
 #semver{x = 0,y = 5,z = 0,tag = []}]
```

Author
======
Gleb Peregud <gleber.p@gmail.com> for LivePress Inc.
Copyright 2011-2012 LivePress Inc.

License
=======

MIT license
