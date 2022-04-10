## ExplicitLorentzIndex

`ExplicitLorentzIndex[ind]` is an explicit Lorentz index, i.e., `ind` is an integer.

### See also

[Overview](Extra/FeynCalc.md), [LorentzIndex](LorentzIndex.md), [Pair](Pair.md).

### Examples

```mathematica
Pair[LorentzIndex[1], LorentzIndex[\[Mu]]]
```

$$\bar{g}^{1\mu }$$

```mathematica
Pair[LorentzIndex[1], LorentzIndex[\[Mu]]] // StandardForm

(*Pair[ExplicitLorentzIndex[1], LorentzIndex[\[Mu]]]*)
```