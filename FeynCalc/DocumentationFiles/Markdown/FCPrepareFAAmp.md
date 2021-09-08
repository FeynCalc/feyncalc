## FCPrepareFAAmp

`FCPrepareFAAmp[exp]`  is an auxiliary function for a partial conversion of a FeynArts amplitude to FeynCalc.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
FCClearScalarProducts[]
FeynArts`FAFeynAmpDenominator[FeynArts`FAPropagatorDenominator[Momentum[P, D], MW Sqrt[FeynArts`FAGaugeXi[W]]], FeynArts`FAPropagatorDenominator[Momentum[k, D], m]]
FCPrepareFAAmp[%]
```

$$\text{FeynArts$\grave{ }$FAFeynAmpDenominator}\left(\text{FeynArts$\grave{ }$FAPropagatorDenominator}\left(P,\text{MW} \sqrt{\text{FeynArts$\grave{ }$FAGaugeXi}(W)}\right),\text{FeynArts$\grave{ }$FAPropagatorDenominator}(k,m)\right)$$

$$\frac{1}{\left(\overline{P}^2-\text{MW}^2 \xi _W\right).\left(\overline{k}^2-m^2\right)}$$

```mathematica
FeynArts`IndexDelta[FeynArts`Index[Global`Gluon, 1], FeynArts`Index[Global`Gluon, 2]]
FCPrepareFAAmp[%]
```

$$\text{FeynArts$\grave{ }$IndexDelta}(\text{FeynArts$\grave{ }$Index}(\text{Gluon},1),\text{FeynArts$\grave{ }$Index}(\text{Gluon},2))$$

$$\delta ^{\text{Glu1}\;\text{Glu2}}$$
