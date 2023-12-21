## RightNablaD

`RightNablaD[i]` denotes $\nabla _{i}$, acting to the right.

### See also

[Overview](Extra/FeynCalc.md), [ExpandPartialD](ExpandPartialD.md), [FCPartialD](FCPartialD.md), [LeftNabalD](LeftNablaD.md).

### Examples

```mathematica
RightNablaD[i]
```

$$\vec{\nabla }^i$$

```mathematica
RightNablaD[i] . QuantumField[A, LorentzIndex[\[Mu]]] 
 
ex = ExpandPartialD[%]
```

$$\vec{\nabla }^i.A_{\mu }$$

$$-\left(\partial _iA_{\mu }\right)$$

```mathematica
ex // StandardForm

(*-QuantumField[FCPartialD[CartesianIndex[i]], A, LorentzIndex[\[Mu]]]*)
```

```mathematica
RightNablaD[i] // StandardForm

(*RightNablaD[CartesianIndex[i]]*)
```