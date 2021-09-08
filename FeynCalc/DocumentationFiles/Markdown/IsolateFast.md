## IsolateFast

`IsolateFast` is an option of `Isolate` and other functions using Isolate. When set to `True` and when `varlist` is empty, `Isolate` will not attempt to recognize existing abbreviations, but will immediately abbreviate the whole expression instead. This is useful for very large expressions or prefactors, where
`Isolate` would otherwise require a lot of time to finish.

### See also

[Overview](Extra/FeynCalc.md), [Isolate](Isolate.md), [Collect2](Collect2.md).

### Examples
