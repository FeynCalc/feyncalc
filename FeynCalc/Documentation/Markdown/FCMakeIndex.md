## FCMakeIndex

`FCMakeIndex[str1, str2, head]` generates an index with the given head out of the string `str1` and `str2`. For example, `FCMakeIndex["Lor","1",LorentzIndex]` yields `LorentzIndex[Lor1]`. The second argument can also be an integer. `FCMakeIndex` is useful for converting the output of different diagram generators such as FeynArts or QGAF into the FeynCalc notation. It uses memoization to improve the performance.

### See also

[Overview](Extra/FeynCalc.md), [FCMakeSymbols](FCMakeSymbols.md).

### Examples

```mathematica
FCMakeIndex["Lor", "1"]
% // StandardForm
```

$$\text{Lor1}$$

```
(*Lor1*)
```

```mathematica
FCMakeIndex["Lor", {3, 1, 4}, LorentzIndex]
% // StandardForm
```

$$\{\text{Lor3},\text{Lor1},\text{Lor4}\}$$

```
(*{LorentzIndex[Lor3], LorentzIndex[Lor1], LorentzIndex[Lor4]}*)
```

```mathematica
FCMakeIndex["Sun", {"a", 1, -4}]
% // StandardForm 
  
 

```

$$\{\text{Suna},\text{Sun1},\text{SunMinus4}\}$$

```
(*{Suna, Sun1, SunMinus4}*)
```
