## FCUseCache

`FCUseCache[func,{arg1,...},{opt1...}]` evaluates `func[arg1,...,opt1,...]` and caches the result such that the next evaluation of same expressions occurs almost immediately. This caching also takes into account `DownValues` and global variables that enter into evaluation of func.

For example, `ExpandScalarProduct` can't be naively cached, because its result depends on the `DownValues` of `Pair` and `ScalarProduct`, which may be changed multiple times during the session by setting and erasing values of scalar products. With `FCUseCache`, however, caching will work properly, as `FCUseCache` knows the dependence on `ExpandScalarProduct` on those DownValues. For all this to work, a function should be explicitly white-listed in FCUseCache.

### See also

[Overview](Extra/FeynCalc.md), [FCShowCache](FCShowCache.md).

### Examples
