## TypesettingExplicitLorentzIndex

`TypesettingExplicitLorentzIndex` determines the `TraditionalForm` typesetting of explicit Lorentz indices.

### See also

[Overview](Extra/FeynCalc.md).

### Examples

Current setting

```mathematica
TypesettingExplicitLorentzIndex
% // InputForm
```

$$\text{FeynCalc$\grave{ }$SharedObjects$\grave{ }$Private$\grave{ }$x}\to \;\text{FeynCalc$\grave{ }$SharedObjects$\grave{ }$Private$\grave{ }$x}$$

```mathematica
Function[FeynCalc`SharedObjects`Private`x, 
 FeynCalc`SharedObjects`Private`x]
```

Make explicit Lorentz indices look red

```mathematica
TypesettingExplicitLorentzIndex = Function[x, Style[x, Red]];
4 M^2 u FV[k, 0]^2 - 4 M^2 u FV[k, 3]^2 - 4 M SP[k, k] - 2 M u FV[k, 0] FV[k, 3]^2 + 4 M u FV[k, 0] FV[k, 2] - u^2 FV[k, 2]^2
```

$$-4 M^2 u \left(\overline{k}^3\right)^2-2 k^0 M u \left(\overline{k}^3\right)^2+4 k^0 M u \overline{k}^2-4 M \overline{k}^2-u^2 \left(\overline{k}^2\right)^2+4 \left(k^0\right)^2 M^2 u$$

Back to the standard settings

```mathematica
TypesettingExplicitLorentzIndex = Function[x, x]
```

$$x\to x$$
