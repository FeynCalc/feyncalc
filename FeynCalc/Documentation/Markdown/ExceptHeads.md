## ExceptHeads

`ExceptHeads` is an option of `FCLoopIsolate`, `FCDiracIsolate` and other functions. It takes a list of heads that are not allowed to appear inside isolated expression.

For example, `ExceptHeads -> {DiracGamma}` in `FCLoopIsolate` blocks loop integrals where loop momenta are contracted with `Dirac matrices`.

### See also

[Overview](Extra/FeynCalc.md), [FCLoopIsolate](FCLoopIsolate.md), [FCDiracIsolate](FCDiracIsolate.md).

### Examples