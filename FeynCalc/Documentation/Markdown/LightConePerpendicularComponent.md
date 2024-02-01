## LightConePerpendicularComponent

`LightConePerpendicularComponent[LorentzIndex[mu],Momentum[n],Momentum[nb]]` denotes the perpendicular component of the Lorentz index `mu` with respect to the lightcone momenta `n` and `nb`.

`LightConePerpendicularComponent[Momentum[p],Momentum[n],Momentum[nb]]` denotes the perpendicular component of the 4-momentum `p` with respect to the lightcone momenta `n` and `nb`.

### See also

[Overview](Extra/FeynCalc.md), [LorentzIndex](LorentzIndex.md), [Momentum](Momentum.md).

### Examples

$4$-dimensional Lorentz vector

```mathematica
Pair[LightConePerpendicularComponent[LorentzIndex[\[Mu]], Momentum[n],Momentum[nb]], 
  LightConePerpendicularComponent[Momentum[p], Momentum[n], Momentum[nb]]]
```

$$\overline{p}^{\mu }{}_{\perp }$$

Metric tensor

```mathematica
Pair[LightConePerpendicularComponent[LorentzIndex[\[Mu]], Momentum[n],Momentum[nb]], 
  LightConePerpendicularComponent[LorentzIndex[\[Nu]], Momentum[n], Momentum[nb]]]
```

$$\bar{g}^{\mu \nu }{}_{\perp }$$

Dirac matrix

```mathematica
DiracGamma[LightConePerpendicularComponent[LorentzIndex[\[Mu]], Momentum[n], Momentum[nb]]]
```

$$\bar{\gamma }^{\mu }{}_{\perp }$$

Contractions

```mathematica
DiracGamma[LightConePerpendicularComponent[LorentzIndex[\[Mu]], 
      Momentum[n], Momentum[nb]]] FV[p, \[Mu]] // Contract 
 
% // StandardForm
```

$$\bar{\gamma }\cdot \overline{p}_{\perp }$$

```mathematica
(*DiracGamma[LightConePerpendicularComponent[Momentum[p], Momentum[n], Momentum[nb]]]*)
```