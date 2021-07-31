## FCFeynmanParametrize

`FCFeynmanParametrize[int, {q1, q2, ...}]` introduces Feynman parameters for the scalar multi-loop integral int. The function returns `{fpInt,pref,vars}`, where fpInt is the integrand without the `prefactor`, `pref` is the prefactor free of Feynman parameters and `vars` is the list of integration variables. The overall Dirac delta in the integrand is omitted unless the option `DiracDelta` is set to True.

By default FCFeynmanParametrize uses normalization that is common in multi-loop calculations. If you want to have the standard $\frac{1}{(2 \pi)^D}$ normalization or yet another value, please set the option `FeynmanIntegralPrefactor` accordingly.

To calculate $D$-dimensional Euclidean integrals (as opposed to $D-1$-dimensional Cartesian or $D$-dimensional Minkowski integrals) written in terms of `FVD`, `SPD`, `FAD`, `SFAD` etc., you need to set the option `"Euclidean"` to `True`.

### See also

[FCFeynmanPrepare](FCFeynmanPrepare), [FCFeynmanProjectivize](FCFeynmanProjectivize).

### Examples

1-loop tadpole

```mathematica
FCFeynmanParametrize[FAD[{q, m}], {q}, Names -> x]
```

$$\left\{1,-\left(m^2\right)^{\frac{D}{2}-1} \Gamma \left(1-\frac{D}{2}\right),\{\}\right\}$$

Massless 1-loop 2-point function

```mathematica
FCFeynmanParametrize[FAD[q, q - p], {q}, Names -> x]
```

$$\left\{(x(1)+x(2))^{2-D} \left(-p^2 x(1) x(2)\right)^{\frac{D}{2}-2},\Gamma \left(2-\frac{D}{2}\right),\{x(1),x(2)\}\right\}$$

With $p^2$ replaced by `pp` and `D` set to `4 - 2 Epsilon`

```mathematica
FCFeynmanParametrize[FAD[q, q - p], {q}, Names -> x, FinalSubstitutions -> SPD[p] -> pp, FCReplaceD -> {D -> 4 - 2 Epsilon}]
```

$$\left\{(x(1)+x(2))^{2 \varepsilon -2} (-\text{pp} x(1) x(2))^{-\varepsilon },\Gamma (\varepsilon ),\{x(1),x(2)\}\right\}$$

Standard text-book prefactor of the loop integral measure

```mathematica
FCFeynmanParametrize[FAD[q, q - p], {q}, Names -> x, FinalSubstitutions -> SPD[p] -> pp, FCReplaceD -> {D -> 4 - 2 Epsilon}, 
  FeynmanIntegralPrefactor -> "Textbook"]
```

$$\left\{(x(1)+x(2))^{2 \varepsilon -2} (-\text{pp} x(1) x(2))^{-\varepsilon },i 2^{2 \varepsilon -4} \pi ^{\varepsilon -2} \Gamma (\varepsilon ),\{x(1),x(2)\}\right\}$$

Same integral but with the Euclidean metric signature

```mathematica
FCFeynmanParametrize[FAD[q, q - p], {q}, Names -> x, FinalSubstitutions -> SPD[p] -> pp, FCReplaceD -> {D -> 4 - 2 Epsilon}, 
  FeynmanIntegralPrefactor -> "Textbook", "Euclidean" -> True]
```

$$\left\{(x(1)+x(2))^{2 \varepsilon -2} (\text{pp} x(1) x(2))^{-\varepsilon },2^{2 \varepsilon -4} \pi ^{\varepsilon -2} \Gamma (\varepsilon ),\{x(1),x(2)\}\right\}$$

A tensor integral

```mathematica
FCFeynmanParametrize[FAD[{q, m}] FAD[{q - p, m2}] FVD[q, mu] FVD[q, nu], {q}, Names -> x, FCE -> True]
```

$$\left\{(x(1)+x(2))^{-D} \left(m^2 x(1)^2+m^2 x(1) x(2)+\text{m2}^2 x(2)^2+\text{m2}^2 x(1) x(2)-p^2 x(1) x(2)\right)^{\frac{D}{2}-2} \left(x(2)^2 \Gamma \left(2-\frac{D}{2}\right) p^{\text{mu}} p^{\text{nu}}-\frac{1}{2} \Gamma \left(1-\frac{D}{2}\right) g^{\text{mu}\text{nu}} \left(m^2 x(1)^2+m^2 x(1) x(2)+\text{m2}^2 x(2)^2+\text{m2}^2 x(1) x(2)-p^2 x(1) x(2)\right)\right),1,\{x(1),x(2)\}\right\}$$

1-loop master formulas for Minkowski integrals (cf. Eq. 9.49b in Sterman's An introduction to QFT)

```mathematica
SFAD[{{k, 2 p . k}, M^2, s}]
FCFeynmanParametrize[%, {k}, Names -> x, FCE -> True, FeynmanIntegralPrefactor -> 1, FCReplaceD -> {D -> n}]
```

$$(k^2+2 (k\cdot p)-M^2+i \eta )^{-s}$$

$$\left\{1,\frac{i \pi ^{n/2} (-1)^s \Gamma \left(s-\frac{n}{2}\right) \left(M^2+p^2\right)^{\frac{n}{2}-s}}{\Gamma (s)},\{\}\right\}$$

```mathematica
FVD[k, \[Mu]] SFAD[{{k, 2 p . k}, M^2, s}]
FCFeynmanParametrize[%, {k}, Names -> x, FCE -> True, FeynmanIntegralPrefactor -> 1, FCReplaceD -> {D -> n}]
```

$$k^{\mu } (k^2+2 (k\cdot p)-M^2+i \eta )^{-s}$$

$$\left\{1,-\frac{i \pi ^{n/2} (-1)^s p^{\mu } \Gamma \left(s-\frac{n}{2}\right) \left(M^2+p^2\right)^{\frac{n}{2}-s}}{\Gamma (s)},\{\}\right\}$$

```mathematica
FVD[k, \[Mu]] FVD[k, \[Nu]] SFAD[{{k, 2 p . k}, M^2, s}]
FCFeynmanParametrize[%, {k}, Names -> x, FCE -> True, FeynmanIntegralPrefactor -> 1, FCReplaceD -> {D -> n}]
```

$$k^{\mu } k^{\nu } (k^2+2 (k\cdot p)-M^2+i \eta )^{-s}$$

$$\left\{1,\frac{i \pi ^{n/2} (-1)^s \left(M^2+p^2\right)^{\frac{n}{2}-s} \left(p^{\mu } p^{\nu } \Gamma \left(s-\frac{n}{2}\right)-\frac{1}{2} \left(M^2+p^2\right) g^{\mu \nu } \Gamma \left(-\frac{n}{2}+s-1\right)\right)}{\Gamma (s)},\{\}\right\}$$

1-loop master formulas for Euclidean integrals (cf. Eq. 9.49a in Sterman's An introduction to QFT)

```mathematica
SFAD[{{k, 2 p . k}, -M^2, s}]
FCFeynmanParametrize[%, {k}, Names -> x, FCE -> True, "Euclidean" -> True, FeynmanIntegralPrefactor -> I]
```

$$(k^2+2 (k\cdot p)+M^2+i \eta )^{-s}$$

$$\left\{1,\frac{i \pi ^{D/2} \Gamma \left(s-\frac{D}{2}\right) \left(M^2-p^2\right)^{\frac{D}{2}-s}}{\Gamma (s)},\{\}\right\}$$

```mathematica
FVD[k, \[Mu]] SFAD[{{k, 2 p . k}, -M^2, s}]
FCFeynmanParametrize[%, {k}, Names -> x, FCE -> True, FeynmanIntegralPrefactor -> I, FCReplaceD -> {D -> n}, "Euclidean" -> True]
```

$$k^{\mu } (k^2+2 (k\cdot p)+M^2+i \eta )^{-s}$$

$$\left\{1,-\frac{i \pi ^{n/2} p^{\mu } \Gamma \left(s-\frac{n}{2}\right) \left(M^2-p^2\right)^{\frac{n}{2}-s}}{\Gamma (s)},\{\}\right\}$$

```mathematica
FVD[k, \[Mu]] FVD[k, \[Nu]] SFAD[{{k, 2 p . k}, -M^2, s}]
FCFeynmanParametrize[%, {k}, Names -> x, FCE -> True, FeynmanIntegralPrefactor -> I, FCReplaceD -> {D -> n}, "Euclidean" -> True] 
  
 

```

$$k^{\mu } k^{\nu } (k^2+2 (k\cdot p)+M^2+i \eta )^{-s}$$

$$\left\{1,\frac{i \pi ^{n/2} \left(M^2-p^2\right)^{\frac{n}{2}-s} \left(\frac{1}{2} \left(M^2-p^2\right) g^{\mu \nu } \Gamma \left(-\frac{n}{2}+s-1\right)+p^{\mu } p^{\nu } \Gamma \left(s-\frac{n}{2}\right)\right)}{\Gamma (s)},\{\}\right\}$$