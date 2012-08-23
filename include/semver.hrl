-define(UND_PRE,   <<>>).
-define(UND_BUILD, undefined).

-record(semver, {x = 0,
                 y = 0,
                 z = 0,
                 pre = ?UND_PRE,
                 build = ?UND_BUILD}).
