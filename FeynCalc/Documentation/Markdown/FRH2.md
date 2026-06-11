## FRH2

`FRH2[exp_, isoNames_]` is similar `FRH` but is specifically designed to reinsert abbreviations introduced by `Collect2` running in parallel mode.

In such cases the user needs to set the `IsolateNames` option to a list containing  as many elements as there are parallel kernels. Then, each parallel kernel introduces its own set of abbreviations that are not known to other kernels. `FRH2` takes the value of the `IsolateNames` option as its second arguments, fetches abbreviation definitions from each parallel kernel and finally substitutes them back into `exp`.

### See also

[Overview](Extra/FeynCalc.md), [Collect2](Collect2.md), [FRH](FRH.md),  [Isolate](Isolate.md).

### Examples

This code needs to be run on FeynCalc in parallel mode with parallel kernels available!

```mathematica
(*isoSymbols=FCMakeSymbols[KK,Range[1,$KernelCount],List]*)
```

```mathematica
(*exp=Table[Sum[a[i]b[j],{i,1,10}],{j,1,8}]*)
```

```mathematica
(*aux=Collect2[exp,b,IsolateNames->isoSymbols,FCParallelize->True]*)
```

```mathematica
(*FRH2[aux,isoSymbols]*)
```