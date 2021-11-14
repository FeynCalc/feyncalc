## FCFeynmanParametrize

`FCFeynmanParametrize[int, {q1, q2, ...}]` introduces Feynman parameters for the multi-loop integral int.

The function returns `{fpInt,pref,vars}`,  where `fpInt` is the integrand in Feynman parameters, `pref` is the prefactor free of Feynman parameters and `vars` is the list of integration variables.

If the chosen parametrization contains a Dirac delta multiplying the integrand, it will be omitted unless the option `DiracDelta` is set to True.

By default `FCFeynmanParametrize` uses normalization that is common in multi-loop calculations, i.e. $\frac{1}{i \pi^{D/2}}$ or $\frac{1}{\pi^{D/2}}$ per loop for Minkowski or Euclidean/Cartesian integrals respectively.

If you want to have the standard $\frac{1}{(2 \pi)^D}$ normalization or yet another value, please set the option `FeynmanIntegralPrefactor` accordingly. Following values are available

- "MultiLoop1" - default value explained above
- "MultiLoop2" - like the default value but with an extra $e^{\gamma_E \frac{4-D}{2}}$ per loop
- "Textbook" - $\frac{1}{(2 \pi)^D}$ per loop
- "Unity" - no extra prefactor multiplying the integral measure

The calculation of $D$-dimensional Minkowski integrals and $D-1$-dimensional Cartesian integrals is straightforward.

To calculate a $D$-dimensional Euclidean integral (i.e. an integral defined with the Euclidean
metric signature $(1,1,1,1)$ you need to write it in terms of `FVD`, `SPD`, `FAD`, `SFAD` etc. and set the option `"Euclidean"` to `True`.

The function can derive different representations of a loop integral. The choice of the representation is controlled by the option `Method`. Following representations are available

- "Feynman" - the standard Feynman representation (default value). Both tensor integrals and integrals with scalar products in the numerator are supported.
- "Lee-Pomeransky" - this representation was first introduced in [1308.6676](https://arxiv.org/abs/1308.6676) by Roman Lee and Andrei Pomeransky. Currently, only scalar integrals without numerators are supported.

`FCFeynmanParametrize` can also be employed in conjunction with `FCFeynmanParameterJoin`, where one first joins suitable propagators using auxiliary Feynman
parameters and then finally integrates out loop momenta.

For a proper analysis of a loop integral one usually needs the `U` and `F` polynomials separately. Since internally `FCFeynmanParametrize` uses `FCFeynmanPrepare`, the information available from the latter is also accessible to `FCFeynmanParametrize`.

By setting the option `FCFeynmanPrepare` to `True`, the output of `FCFeynmanPrepare` will be added the the output of `FCFeynmanParametrize` as the 4th list element.

### See also

[Overview](Extra/FeynCalc.md), [FCFeynmanPrepare](FCFeynmanPrepare.md), [FCFeynmanProjectivize](FCFeynmanProjectivize.md), [FCFeynmanParameterJoin](FCFeynmanParameterJoin.md), [SplitSymbolicPowers](SplitSymbolicPowers.md).

### Examples

#### Feynman representation

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

$$\left\{(x(1)+x(2))^{-D} \left(m^2 x(1)^2+m^2 x(1) x(2)+\text{m2}^2 x(2)^2+\text{m2}^2 x(1) x(2)-p^2 x(1) x(2)\right)^{\frac{D}{2}-2} \left(x(2)^2 \Gamma \left(2-\frac{D}{2}\right) p^{\text{mu}} p^{\text{nu}}-\frac{1}{2} \Gamma \left(1-\frac{D}{2}\right) g^{\text{mu}\;\text{nu}} \left(m^2 x(1)^2+m^2 x(1) x(2)+\text{m2}^2 x(2)^2+\text{m2}^2 x(1) x(2)-p^2 x(1) x(2)\right)\right),1,\{x(1),x(2)\}\right\}$$

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

1-loop massless box

```mathematica
FAD[p, p + q1, p + q1 + q2, p + q1 + q2 + q3]
FCFeynmanParametrize[%, {p}, Names -> x, FCReplaceD -> {D -> 4 - 2 Epsilon}]
```

$$\frac{1}{p^2.(p+\text{q1})^2.(p+\text{q1}+\text{q2})^2.(p+\text{q1}+\text{q2}+\text{q3})^2}$$

$$\left\{(x(1)+x(2)+x(3)+x(4))^{2 \varepsilon } \left(-2 x(1) x(3) (\text{q1}\cdot \;\text{q2})-2 x(1) x(4) (\text{q1}\cdot \;\text{q2})-2 x(1) x(4) (\text{q1}\cdot \;\text{q3})-\text{q1}^2 x(1) x(2)-\text{q1}^2 x(1) x(3)-\text{q1}^2 x(1) x(4)-2 x(4) x(2) (\text{q2}\cdot \;\text{q3})-2 x(1) x(4) (\text{q2}\cdot \;\text{q3})-\text{q2}^2 x(3) x(2)-\text{q2}^2 x(4) x(2)-\text{q2}^2 x(1) x(3)-\text{q2}^2 x(1) x(4)-\text{q3}^2 x(4) x(2)-\text{q3}^2 x(1) x(4)-\text{q3}^2 x(3) x(4)\right)^{-\varepsilon -2},\Gamma (\varepsilon +2),\{x(1),x(2),x(3),x(4)\}\right\}$$

3-loop self-energy with two massive lines

```mathematica
SFAD[{{p1, 0}, {m^2, 1}, 1}, {{p2, 0}, {0, 1}, 1}, {{p3, 0}, {0, 1}, 1}, 
  {{p2 + p3, 0}, {0, 1}, 1}, {{p1 - Q, 0}, {m^2, 1}, 1}, {{p2 - Q, 0}, {0, 1}, 1}, 
  {{p2 + p3 - Q, 0}, {0, 1}, 1}, {{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}]
FCFeynmanParametrize[%, {p1, p2, p3}, Names -> x, FCReplaceD -> {D -> 4 - 2 Epsilon}]
```

$$\frac{1}{(\text{p1}^2-m^2+i \eta ).(\text{p2}^2+i \eta ).(\text{p3}^2+i \eta ).((\text{p2}+\text{p3})^2+i \eta ).((\text{p1}-Q)^2-m^2+i \eta ).((\text{p2}-Q)^2+i \eta ).((\text{p2}+\text{p3}-Q)^2+i \eta ).((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )}$$

$$\left\{(x(1) x(2) x(3)+x(1) x(4) x(3)+x(2) x(5) x(3)+x(4) x(5) x(3)+x(1) x(6) x(3)+x(5) x(6) x(3)+x(1) x(7) x(3)+x(5) x(7) x(3)+x(1) x(8) x(3)+x(2) x(8) x(3)+x(4) x(8) x(3)+x(5) x(8) x(3)+x(6) x(8) x(3)+x(7) x(8) x(3)+x(1) x(2) x(4)+x(2) x(4) x(5)+x(1) x(4) x(6)+x(4) x(5) x(6)+x(1) x(2) x(7)+x(2) x(5) x(7)+x(1) x(6) x(7)+x(5) x(6) x(7)+x(1) x(2) x(8)+x(2) x(4) x(8)+x(2) x(5) x(8)+x(1) x(6) x(8)+x(4) x(6) x(8)+x(5) x(6) x(8)+x(2) x(7) x(8)+x(6) x(7) x(8))^{4 \varepsilon } \left(x(2) x(3) x(5)^2 m^2+x(2) x(4) x(5)^2 m^2+x(3) x(4) x(5)^2 m^2+x(1)^2 x(2) x(3) m^2+x(1)^2 x(2) x(4) m^2+x(1)^2 x(3) x(4) m^2+2 x(1) x(2) x(3) x(5) m^2+2 x(1) x(2) x(4) x(5) m^2+2 x(1) x(3) x(4) x(5) m^2+x(3) x(5)^2 x(6) m^2+x(4) x(5)^2 x(6) m^2+x(1)^2 x(3) x(6) m^2+x(1)^2 x(4) x(6) m^2+2 x(1) x(3) x(5) x(6) m^2+2 x(1) x(4) x(5) x(6) m^2+x(2) x(5)^2 x(7) m^2+x(3) x(5)^2 x(7) m^2+x(1)^2 x(2) x(7) m^2+x(1)^2 x(3) x(7) m^2+2 x(1) x(2) x(5) x(7) m^2+2 x(1) x(3) x(5) x(7) m^2+x(1)^2 x(6) x(7) m^2+x(5)^2 x(6) x(7) m^2+2 x(1) x(5) x(6) x(7) m^2+x(2) x(5)^2 x(8) m^2+x(3) x(5)^2 x(8) m^2+x(1)^2 x(2) x(8) m^2+x(1)^2 x(3) x(8) m^2+x(1) x(2) x(3) x(8) m^2+x(1) x(2) x(4) x(8) m^2+x(1) x(3) x(4) x(8) m^2+2 x(1) x(2) x(5) x(8) m^2+2 x(1) x(3) x(5) x(8) m^2+x(2) x(3) x(5) x(8) m^2+x(2) x(4) x(5) x(8) m^2+x(3) x(4) x(5) x(8) m^2+x(1)^2 x(6) x(8) m^2+x(5)^2 x(6) x(8) m^2+x(1) x(3) x(6) x(8) m^2+x(1) x(4) x(6) x(8) m^2+2 x(1) x(5) x(6) x(8) m^2+x(3) x(5) x(6) x(8) m^2+x(4) x(5) x(6) x(8) m^2+x(1) x(2) x(7) x(8) m^2+x(1) x(3) x(7) x(8) m^2+x(2) x(5) x(7) x(8) m^2+x(3) x(5) x(7) x(8) m^2+x(1) x(6) x(7) x(8) m^2+x(5) x(6) x(7) x(8) m^2-Q^2 x(1) x(2) x(3) x(5)-Q^2 x(1) x(2) x(4) x(5)-Q^2 x(1) x(3) x(4) x(5)-Q^2 x(1) x(2) x(3) x(6)-Q^2 x(1) x(2) x(4) x(6)-Q^2 x(1) x(3) x(4) x(6)-Q^2 x(1) x(3) x(5) x(6)-Q^2 x(2) x(3) x(5) x(6)-Q^2 x(1) x(4) x(5) x(6)-Q^2 x(2) x(4) x(5) x(6)-Q^2 x(3) x(4) x(5) x(6)-Q^2 x(1) x(2) x(3) x(7)-Q^2 x(1) x(2) x(4) x(7)-Q^2 x(1) x(3) x(4) x(7)-Q^2 x(1) x(2) x(5) x(7)-Q^2 x(1) x(3) x(5) x(7)-Q^2 x(2) x(3) x(5) x(7)-Q^2 x(2) x(4) x(5) x(7)-Q^2 x(3) x(4) x(5) x(7)-Q^2 x(1) x(2) x(6) x(7)-Q^2 x(1) x(4) x(6) x(7)-Q^2 x(1) x(5) x(6) x(7)-Q^2 x(2) x(5) x(6) x(7)-Q^2 x(4) x(5) x(6) x(7)-Q^2 x(1) x(2) x(3) x(8)-Q^2 x(1) x(2) x(4) x(8)-Q^2 x(1) x(3) x(4) x(8)-Q^2 x(1) x(2) x(5) x(8)-Q^2 x(1) x(3) x(5) x(8)-Q^2 x(1) x(2) x(6) x(8)-Q^2 x(2) x(3) x(6) x(8)-Q^2 x(1) x(4) x(6) x(8)-Q^2 x(2) x(4) x(6) x(8)-Q^2 x(3) x(4) x(6) x(8)-Q^2 x(1) x(5) x(6) x(8)-Q^2 x(2) x(5) x(6) x(8)-Q^2 x(3) x(5) x(6) x(8)-Q^2 x(2) x(3) x(7) x(8)-Q^2 x(2) x(4) x(7) x(8)-Q^2 x(3) x(4) x(7) x(8)-Q^2 x(2) x(5) x(7) x(8)-Q^2 x(3) x(5) x(7) x(8)-Q^2 x(2) x(6) x(7) x(8)-Q^2 x(4) x(6) x(7) x(8)-Q^2 x(5) x(6) x(7) x(8)\right)^{-3 \varepsilon -2},\Gamma (3 \varepsilon +2),\{x(1),x(2),x(3),x(4),x(5),x(6),x(7),x(8)\}\right\}$$

An example of using `FCFeynmanParametrize` together with `FCFeynmanParameterJoin`

```mathematica
props = {SFAD[{p1, m^2}], SFAD[{p3, m^2}], SFAD[{{0, 2 p1 . n}}], SFAD[{{0, 2 (p1 + p3) . n}}]}
```

$$\left\{\frac{1}{(\text{p1}^2-m^2+i \eta )},\frac{1}{(\text{p3}^2-m^2+i \eta )},\frac{1}{(2 (n\cdot \;\text{p1})+i \eta )},\frac{1}{(2 (n\cdot (\text{p1}+\text{p3}))+i \eta )}\right\}$$

```mathematica
intT = FCFeynmanParameterJoin[{{props[[1]] props[[2]], 1, x}, 
    props[[3]] props[[4]], y}, {p1, p3}]
```

$$\left\{\frac{1}{(\left(-x(1) m^2-x(2) m^2+\text{p1}^2 x(1)+\text{p3}^2 x(2)\right) y(1)+2 (n\cdot \;\text{p1}) y(2)+(2 (n\cdot \;\text{p1})+2 (n\cdot \;\text{p3})) y(3)+i \eta )^4},6 y(1),\{x(1),x(2),y(1),y(2),y(3)\}\right\}$$

Here the Feynman parameter variables $x_i$ and $y_i$ are independent from each other, i.e. we have $\delta(1-x_1-x_2-x_3) \times \delta(1-y_1-y_2-y_3)$.
This gives us much more freedom when exploiting the Cheng-Wu theorem.

```mathematica
FCFeynmanParametrize[intT[[1]], intT[[2]], {p1, p3}, Indexed -> True, FCReplaceD -> {D -> 4 - 2 ep}, 
  FinalSubstitutions -> {SPD[n] -> 1, m -> 1}, Variables -> intT[[3]]]
```

$$\left\{y(1) \left(x(1) x(2) y(1)^2\right)^{3 \;\text{ep}-2} \left(y(1) \left(x(1) x(2)^2 y(1)^2+x(1)^2 x(2) y(1)^2+x(2) y(2)^2+x(1) y(3)^2+x(2) y(3)^2+2 x(2) y(2) y(3)\right)\right)^{-2 \;\text{ep}},\Gamma (2 \;\text{ep}),\{x(1),x(2),y(1),y(2),y(3)\}\right\}$$

In the case that we need `U` and `F` polynomials in addition to the normal output (e.g. for HyperInt)

```mathematica
(SFAD[{{0, 2*k1 . n}}]*SFAD[{{0, 2*k2 . n}}]*SFAD[{k1, m^2}]*
   SFAD[{k2, m^2}]*SFAD[{k1 - k2, m^2}])
out = FCFeynmanParametrize[%, {k1, k2}, Names -> x, FCReplaceD -> {D -> 4 - 2 Epsilon}, 
   FCFeynmanPrepare -> True]
```

$$\frac{1}{(\text{k1}^2-m^2+i \eta ) (\text{k2}^2-m^2+i \eta ) ((\text{k1}-\text{k2})^2-m^2+i \eta ) (2 (\text{k1}\cdot n)+i \eta ) (2 (\text{k2}\cdot n)+i \eta )}$$

$$\left\{(x(3) x(4)+x(5) x(4)+x(3) x(5))^{3 \varepsilon -1} \left(m^2 x(3) x(4)^2+m^2 x(3) x(5)^2+m^2 x(4) x(5)^2+m^2 x(3)^2 x(4)+m^2 x(3)^2 x(5)+m^2 x(4)^2 x(5)+3 m^2 x(3) x(4) x(5)+n^2 x(2)^2 x(3)+n^2 x(1)^2 x(4)+n^2 x(2)^2 x(4)+2 n^2 x(1) x(2) x(4)+n^2 x(1)^2 x(5)\right)^{-2 \varepsilon -1},-\Gamma (2 \varepsilon +1),\{x(1),x(2),x(3),x(4),x(5)\},\left\{x(3) x(4)+x(5) x(4)+x(3) x(5),m^2 x(3) x(4)^2+m^2 x(3) x(5)^2+m^2 x(4) x(5)^2+m^2 x(3)^2 x(4)+m^2 x(3)^2 x(5)+m^2 x(4)^2 x(5)+3 m^2 x(3) x(4) x(5)+n^2 x(2)^2 x(3)+n^2 x(1)^2 x(4)+n^2 x(2)^2 x(4)+2 n^2 x(1) x(2) x(4)+n^2 x(1)^2 x(5),\left(
\begin{array}{ccc}
 x(1) & \frac{1}{(2 (\text{k1}\cdot n)+i \eta )} & 1 \\
 x(2) & \frac{1}{(2 (\text{k2}\cdot n)+i \eta )} & 1 \\
 x(3) & \frac{1}{(\text{k1}^2-m^2+i \eta )} & 1 \\
 x(4) & \frac{1}{((\text{k1}-\text{k2})^2-m^2+i \eta )} & 1 \\
 x(5) & \frac{1}{(\text{k2}^2-m^2+i \eta )} & 1 \\
\end{array}
\right),\left(
\begin{array}{cc}
 x(3)+x(4) & -x(4) \\
 -x(4) & x(4)+x(5) \\
\end{array}
\right),\left\{x(1) \left(-n^{\text{FCGV}(\text{mu})}\right),x(2) \left(-n^{\text{FCGV}(\text{mu})}\right)\right\},-m^2 (x(3)+x(4)+x(5)),1,0\right\}\right\}$$

From this output we can easily extract the integrand, its $x_i$-independent prefactor and the two Symanzik polynomials

```mathematica
{integrand, pref} = out[[1 ;; 2]]
{uPoly, fPoly} = out[[4]][[1 ;; 2]]
```

$$\left\{(x(3) x(4)+x(5) x(4)+x(3) x(5))^{3 \varepsilon -1} \left(m^2 x(3) x(4)^2+m^2 x(3) x(5)^2+m^2 x(4) x(5)^2+m^2 x(3)^2 x(4)+m^2 x(3)^2 x(5)+m^2 x(4)^2 x(5)+3 m^2 x(3) x(4) x(5)+n^2 x(2)^2 x(3)+n^2 x(1)^2 x(4)+n^2 x(2)^2 x(4)+2 n^2 x(1) x(2) x(4)+n^2 x(1)^2 x(5)\right)^{-2 \varepsilon -1},-\Gamma (2 \varepsilon +1)\right\}$$

$$\left\{x(3) x(4)+x(5) x(4)+x(3) x(5),m^2 x(3) x(4)^2+m^2 x(3) x(5)^2+m^2 x(4) x(5)^2+m^2 x(3)^2 x(4)+m^2 x(3)^2 x(5)+m^2 x(4)^2 x(5)+3 m^2 x(3) x(4) x(5)+n^2 x(2)^2 x(3)+n^2 x(1)^2 x(4)+n^2 x(2)^2 x(4)+2 n^2 x(1) x(2) x(4)+n^2 x(1)^2 x(5)\right\}$$

Symbolic propagator powers are fully supported

```mathematica
SFAD[{I k, 0, -1/2 + ep}, {I (k + p), 0, 1}, EtaSign -> -1]
v1 = FCFeynmanParametrize[%, {k}, Names -> x, FCReplaceD -> {D -> 4 - 2 ep}, FinalSubstitutions -> {SPD[p] -> 1}]
```

$$\frac{1}{(-k^2-i \eta )^{\text{ep}-\frac{1}{2}}.(-(k+p)^2-i \eta )}$$

$$\left\{(-x(1)-x(2))^{3 \;\text{ep}-\frac{7}{2}} x(2)^{\text{ep}-\frac{3}{2}} (-x(1) x(2))^{\frac{3}{2}-2 \;\text{ep}},\frac{(-1)^{\text{ep}+\frac{1}{2}} \Gamma \left(2 \;\text{ep}-\frac{3}{2}\right)}{\Gamma \left(\text{ep}-\frac{1}{2}\right)},\{x(1),x(2)\}\right\}$$

An alternative representation for symbolic powers can be obtained using the option `SplitSymbolicPowers`

```mathematica
SFAD[{I k, 0, -1/2 + ep}, {I (k + p), 0, 1}, EtaSign -> -1]
v2 = FCFeynmanParametrize[%, {k}, Names -> x, FCReplaceD -> {D -> 4 - 2 ep}, FinalSubstitutions -> {SPD[p] -> 1}, SplitSymbolicPowers -> True]
```

$$\frac{1}{(-k^2-i \eta )^{\text{ep}-\frac{1}{2}}.(-(k+p)^2-i \eta )}$$

$$\left\{x(2)^{\text{ep}-\frac{1}{2}} \left(\left(\frac{1}{2} (1-2 \;\text{ep})+\frac{1}{2} (4-2 \;\text{ep})-1\right) x(1) (-x(1)-x(2))^{3 \;\text{ep}-\frac{7}{2}} (-x(1) x(2))^{\frac{1}{2}-2 \;\text{ep}}+\left(2 \;\text{ep}+\frac{1}{2} (2 \;\text{ep}-1)-3\right) (-x(1)-x(2))^{3 \;\text{ep}-\frac{9}{2}} (-x(1) x(2))^{\frac{3}{2}-2 \;\text{ep}}\right),\frac{(-1)^{\text{ep}+\frac{1}{2}} \Gamma \left(2 \;\text{ep}-\frac{3}{2}\right)}{\Gamma \left(\text{ep}+\frac{1}{2}\right)},\{x(1),x(2)\}\right\}$$

Even though the parametric integrals evaluate to different values, the product of the integral and its prefactor remains the same

```mathematica
Integrate[Normal[Series[v1[[1]] /. x[1] -> 1, {ep, 0, 0}]] /. x[1] -> 1, {x[2],0, Infinity}]
Normal@Series[v1[[2]] %, {ep, 0, 0}]
```

$$\frac{2}{5}$$

$$-\frac{4 i}{15}$$

```mathematica
Integrate[Normal[Series[v2[[1]] /. x[1] -> 1, {ep, 0, 0}]] /. x[1] -> 1, {x[2],0, Infinity}]
Normal@Series[v2[[2]] %, {ep, 0, 0}]
```

$$-\frac{1}{5}$$

$$-\frac{4 i}{15}$$

#### Lee-Pomeransky representation

1-loop tadpole

```mathematica
FCFeynmanParametrize[FAD[{q, m}], {q}, Names -> x, Method -> "Lee-Pomeransky"]
```

$$\left\{\left(m^2 x(1)^2+x(1)\right)^{-D/2},-\frac{\Gamma \left(\frac{D}{2}\right)}{\Gamma (D-1)},\{x(1)\}\right\}$$

Massless 1-loop 2-point function

```mathematica
FCFeynmanParametrize[FAD[q, q - p], {q}, Names -> x, Method -> "Lee-Pomeransky"]
```

$$\left\{\left(-p^2 x(2) x(1)+x(1)+x(2)\right)^{-D/2},\frac{\Gamma \left(\frac{D}{2}\right)}{\Gamma (D-2)},\{x(1),x(2)\}\right\}$$

2-loop self-energy with 3 massive lines and two eikonal propagators

```mathematica
FCFeynmanParametrize[{SFAD[{ p1, m^2}], SFAD[{ p3, m^2}], 
   SFAD[{(p3 - p1), m^2}], SFAD[{{0, 2 p1 . n}}], SFAD[{{0, 2 p3 . n}}]}, {p1, p3}, 
  Names -> x, Method -> "Lee-Pomeransky", FCReplaceD -> {D -> 4 - 2 ep}, 
  FinalSubstitutions -> {SPD[n] -> 1, m -> 1}]
```

$$\left\{\left(x(4) x(1)^2+x(5) x(1)^2+2 x(2) x(5) x(1)+x(3) x(4)^2+x(3) x(5)^2+x(4) x(5)^2+x(2)^2 x(3)+x(3)^2 x(4)+x(3) x(4)+x(2)^2 x(5)+x(3)^2 x(5)+x(4)^2 x(5)+x(3) x(5)+3 x(3) x(4) x(5)+x(4) x(5)\right)^{\text{ep}-2},-\frac{\Gamma (2-\text{ep})}{\Gamma (1-3 \;\text{ep})},\{x(1),x(2),x(3),x(4),x(5)\}\right\}$$