## $RenameFeynCalcObjects

`$RenameFeynCalcObjects` specifies a list of replacement rules that allow to rename FeynCalc objects on the fly to avoid conflicts with other package before FeynCalc is loaded (monkey patching). The value of `$RenameFeynCalcObjects` must be specified before loading FeynCalc.

### See also

[Overview](Extra/FeynCalc.md), [$LoadAddOns]($LoadAddOns.md).

### Examples

The following code (when executed on a fresh kernel with the last two lines uncommented) allows to load FeynCalc and Roman Lee's LiteRed on the same kernel without shadowing

```mathematica
$RenameFeynCalcObjects = {"MetricTensor" -> "FCMetricTensor", "Factor1" -> "FCFactor1", "Factor2" -> "FCFactor2"};
(*
<<FeynCalc`
<<LiteRed`
*)
```
