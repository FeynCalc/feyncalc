## FCCheckVersion

`FCCheckVersion[major, minor, build]` checks if the current version of FeynCalc is larger or equal than `marjor.minor.build`. For example, `FCCheckVersion[9,3,0]` will generate a warning (when running with the frontend) or quit kernel (when running without the frontend) if the loaded FeynCalc version is older than 9.3.0.

Notice that this function is available only since FeynCalc 9.3.

### See also

[Overview](Extra/FeynCalc.md), [$FeynCalcVersion]($FeynCalcVersion.md).

### Examples

```mathematica
FCCheckVersion[8, 2, 0]
```

```mathematica
(*FCCheckVersion[15,2,0]*)
```
