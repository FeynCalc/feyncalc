## FeynCalcForm

`FeynCalcForm[expr]` changes the printed output to a an easy-to-read form. It allows a readable output also when running a terminal based Mathematica session. Whether the result of `FeynCalcForm[expr]` is displayed or not, depends on the setting of `$PrePrint`.

`$PrePrint = FeynCalcForm` forces displaying everything after applying `FeynCalcForm`. In order to change to the normal (internal) Mathematica OutputForm, do: `$PrePrint=.`.

### See also

[Overview](Extra/FeynCalc.md), [FC](FC.md), [FeynCalcExternal](FeynCalcExternal.md), [FeynCalcInternal](FeynCalcInternal.md).

### Examples

This is the normal notebook display:

```mathematica
SUNTrace[SUNT[a] . SUNT[b] . SUNT[c]]
```

$$\text{tr}(T^a.T^b.T^c)$$

This is the shorthand (terminal) display (easy-to-read form):

```mathematica
$PrePrint = FeynCalcForm;
SetOptions[$FrontEndSession, Evaluate[(Options[$FrontEndSession, "CommonDefaultFormatTypes"] /. ("Output" -> _) -> ("Output" -> OutputForm))[[1]]]];
SUNTrace[SUNT[a] . SUNT[b] . SUNT[c]]
```

$$\text{tr}(T^a.T^b.T^c)$$

Reset to normal notebook display:

```mathematica
$PrePrint =.;
SetOptions[$FrontEndSession, Evaluate[(Options[$FrontEndSession, "CommonDefaultFormatTypes"] /. ("Output" -> _) -> ("Output" -> TraditionalForm))[[1]]]];
SUNTrace[SUNT[a] . SUNT[b] . SUNT[c]]
```

$$\text{tr}(T^a.T^b.T^c)$$
