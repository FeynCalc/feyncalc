## Loading the package

### See also

[Overview](FeynCalc.md).

### Getting started

To load FeynCalc 9 or newer run

```Mathematica
<<FeynCalc`
```


in your Mathematica session. Notice that once FeynCalc has been loaded, it cannot be "reloaded" again. You need to restart the kernel first.

### Add-ons

Extensions such FeynHelpers, FeynOnium, FeynArts loader, PHI etc. are loaded by adding the corresponding text strings to the global variable `$LoadAddOns`. This variable must be set before loading FeynCalc. For example

```Mathematica
$LoadAddOns={"FeynArts","FeynHelpers"};
<<FeynCalc`
```

### Startup messages

To suppress the package startup messages you can set the global variable `$FeynCalcStartupMessages` to `False` before loading FeynCalc. For example

```Mathematica
$FeynCalcStartupMessages=False;
<<FeynCalc`
```
