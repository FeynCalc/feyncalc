## OPEIntegrateDelta

`OPEIntegrateDelta[expr, x, m]` introduces the $\delta(1-x)$ (`DeltaFunction[1-x]`).

The Mathematica `Integrate` function is called and each integration  (from $0$ to $1$) is recorded for reference (and bug-checking) in the list `$MIntegrate`.

Notice that the dimension specified by the option should also be the dimension used in `expr`. It is replaced in OPEIntegrateDelta by (`4+Epsilon`).

### See also

[Overview](Extra/FeynCalc.md).

### Examples