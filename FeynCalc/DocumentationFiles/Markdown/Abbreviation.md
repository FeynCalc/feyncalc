## Abbreviation

`Abbreviation` is a function used by `OneLoop` and `PaVeReduce` for generating smaller files when saving results to the hard disk. The convention is that a definition like `GP = GluonPropagator` should be accompanied by the definition `Abbreviation[GluonPropagator] = HoldForm[GP]`.

### See also

[Overview](Extra/FeynCalc.md), [$Abbreviations]($Abbreviations.md), [OneLoop](OneLoop.md), [PaVeReduce](PaVeReduce.md), [WriteOut](WriteOut.md), [WriteOutPaVe](WriteOutPaVe.md), [GluonPropagator](GluonPropagator.md), [GluonVertex](GluonVertex.md), [QuarkPropagator](QuarkPropagator.md).

### Examples

```mathematica
GP[p, {\[Mu], a}, {\[Nu], b}]
```

$$\Pi _{ab}^{\mu \nu }(p)$$
