## PaVeLimitTo4

`PaVeLimitTo4[expr]`  simplifies products of Passarino-Veltman functions and $D$-dependent prefactors by evaluating the prefactors at $D=4$ and adding an extra term from the product of $(D-4)$ and the UV pole of the Passarino-Veltman function.

This is possible because the UV poles of arbitrary Passarino-Veltman functions can be determined via `PaVeUVPart`. The result is valid up to 0th order in Epsilon, i.e. it is sufficient for 1-loop calculations.

Warning! This simplification always ignores possible IR poles of Passarino-Veltman functions. Therefore, it can be used only if all IR poles are regulated without using dimensional regularization (e.g. by assigning gluons or photons a fake mass) or if it is known in advance that the given expression is free of IR singularities.

The application of `PaVeLimitTo4` is equivalent to using the old `OneLoop` routine with the flags `$LimitTo4` and `$LimitTo4IRUnsafe` set to `True`.

### See also

[Overview](Extra/FeynCalc.md), [$LimitTo4]($LimitTo4.md).

### Examples

```mathematica
ex = (D - 2)/(D - 3) A0[m^2]
PaVeLimitTo4[ex]
```

$$\frac{(D-2) \;\text{A}_0\left(m^2\right)}{D-3}$$

$$2 \;\text{A}_0\left(m^2\right)+2 m^2$$

Simplify the 1-loop amplitude for $H \to g g$

```mathematica
ex = (-(1/((-2 + D) mH^2 mW sinW)) 2 I (-4 + D) e gs^2 mt^2 \[Pi]^2 B0[mH^2, mt^2, mt^2] 
     SD[Glu2, Glu3] (-2 SPD[k1, Polarization[k2, -I, Transversality -> True]] 
        SPD[k2, Polarization[k1, -I, Transversality -> True]] + 
       mH^2 SPD[Polarization[k1, -I, Transversality -> True], 
         Polarization[k2, -I, Transversality -> True]]) - 1/((-2 + 
          D) mH^2 mW sinW) I e gs^2 mt^2 (-2 mH^2 + D mH^2 - 
       8 mt^2) \[Pi]^2 C0[0, 0, mH^2, mt^2, mt^2, mt^2] SD[Glu2, Glu3] (-2 SPD[k1, 
         Polarization[k2, -I, Transversality -> True]] SPD[k2, Polarization[k1, 
          -I, Transversality -> True]] + mH^2 SPD[Polarization[k1, -I, 
          Transversality -> True], Polarization[k2, -I, Transversality -> True]]))
```

$$-\frac{2 i \pi ^2 (D-4) e \;\text{gs}^2 \;\text{mt}^2 \delta ^{\text{Glu2}\;\text{Glu3}} \;\text{B}_0\left(\text{mH}^2,\text{mt}^2,\text{mt}^2\right) \left(\text{mH}^2 \left(\varepsilon ^*(\text{k1})\cdot \varepsilon ^*(\text{k2})\right)-2 \left(\text{k1}\cdot \varepsilon ^*(\text{k2})\right) \left(\text{k2}\cdot \varepsilon ^*(\text{k1})\right)\right)}{(D-2) \;\text{mH}^2 \;\text{mW} \;\text{sinW}}-\frac{i \pi ^2 e \;\text{gs}^2 \;\text{mt}^2 \left(D \;\text{mH}^2-2 \;\text{mH}^2-8 \;\text{mt}^2\right) \delta ^{\text{Glu2}\;\text{Glu3}} \;\text{C}_0\left(0,0,\text{mH}^2,\text{mt}^2,\text{mt}^2,\text{mt}^2\right) \left(\text{mH}^2 \left(\varepsilon ^*(\text{k1})\cdot \varepsilon ^*(\text{k2})\right)-2 \left(\text{k1}\cdot \varepsilon ^*(\text{k2})\right) \left(\text{k2}\cdot \varepsilon ^*(\text{k1})\right)\right)}{(D-2) \;\text{mH}^2 \;\text{mW} \;\text{sinW}}$$

```mathematica
PaVeLimitTo4[ex]
```

$$\frac{i \pi ^2 e \;\text{gs}^2 \;\text{mt}^2 \left(\text{mH}^2-4 \;\text{mt}^2\right) \delta ^{\text{Glu2}\;\text{Glu3}} \;\text{C}_0\left(0,0,\text{mH}^2,\text{mt}^2,\text{mt}^2,\text{mt}^2\right) \left(2 \left(\overline{\text{k1}}\cdot \bar{\varepsilon }^*(\text{k2})\right) \left(\overline{\text{k2}}\cdot \bar{\varepsilon }^*(\text{k1})\right)-\text{mH}^2 \left(\bar{\varepsilon }^*(\text{k1})\cdot \bar{\varepsilon }^*(\text{k2})\right)\right)}{\text{mH}^2 \;\text{mW} \;\text{sinW}}-\frac{2 i \pi ^2 e \;\text{gs}^2 \;\text{mt}^2 \delta ^{\text{Glu2}\;\text{Glu3}} \left(2 \left(\overline{\text{k1}}\cdot \bar{\varepsilon }^*(\text{k2})\right) \left(\overline{\text{k2}}\cdot \bar{\varepsilon }^*(\text{k1})\right)-\text{mH}^2 \left(\bar{\varepsilon }^*(\text{k1})\cdot \bar{\varepsilon }^*(\text{k2})\right)\right)}{\text{mH}^2 \;\text{mW} \;\text{sinW}}$$