## ApartFF

`ApartFF[amp, {q1, q2, ...}]` partial fractions loop integrals by decomposing them into simpler integrals that contain only linearly independent propagators. It uses `FCApart` as a backend and is equally suitable for 1-loop and  multi-loop integrals.

`FCApart`  implements an algorithm based on [arXiv:1204.2314](https://arxiv.org/abs/1204.2314) by F. Feng that seems to employ a variety Leinartas's algorithm (cf. [arXiv:1206.4740](https://arxiv.org/abs/1206.4740)). Unlike Feng's [$Apart](https://github.com/F-Feng/APart) that is applicable to general multivariate polynomials, `FCApart` is tailored to work only with FeynCalc's `FeynAmpDenominator`, `Pair` and `CartesianPair` symbols, i.e. it is less general in this respect.

`ApartFF[amp * extraPiece1, extraPiece2, {q1, q2, ...}]` is a special working mode of `ApartFF`, where the final result of partial fractioning `amp*extraPiece1` is multiplied by `extraPiece2`. It is understood, that `extraPiece1*extraPiece2` should be unity, e. g. when `extraPiece1` is an `FAD`, while extraPiece is an `SPD` inverse to it. This mode should be useful for nonstandard integrals where the desired partial fraction decomposition can be performed only after multiplying `amp` with `extraPiece1`.

### See also

[Overview](Extra/FeynCalc.md), [FCApart](FCApart.md), [FeynAmpDenominatorSimplify](FeynAmpDenominatorSimplify.md), [FCLoopFindTensorBasis](FCLoopFindTensorBasis.md).

### Examples

```mathematica
FCClearScalarProducts[]
```

```mathematica
SPD[q, q] FAD[{q, m}] 
 
ApartFF[%, {q}]
```

$$\frac{q^2}{q^2-m^2}$$

$$\frac{m^2}{q^2-m^2}$$

```mathematica
SPD[q, p] SPD[q, r] FAD[{q}, {q - p}, {q - r}] 
 
ApartFF[%, {q}]
```

$$\frac{(p\cdot q) (q\cdot r)}{q^2.(q-p)^2.(q-r)^2}$$

$$\frac{p^2 r^2}{4 q^2.(q-p)^2.(q-r)^2}+\frac{p^2+2 (q\cdot r)+2 r^2}{4 q^2.(-p+q+r)^2}+-\frac{p^2}{4 q^2.(q-p)^2}-\frac{r^2}{4 q^2.(q-r)^2}$$

```mathematica
FAD[{q}, {q - p}, {q + p}] 
 
ApartFF[%, {q}]
```

$$\frac{1}{q^2.(q-p)^2.(p+q)^2}$$

$$\frac{1}{p^2 q^2.(q-p)^2}-\frac{1}{p^2 q^2.(q-2 p)^2}$$

```mathematica
SPD[p, q1] SPD[p, q2]^2 FAD[{q1, m}, {q2, m}, q1 - p, q2 - p, q1 - q2] 
 
ApartFF[%, {q1, q2}]
```

$$\frac{(p\cdot \;\text{q1}) (p\cdot \;\text{q2})^2}{\left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q1}-p)^2.(\text{q2}-p)^2.(\text{q1}-\text{q2})^2}$$

$$\frac{\left(m^2+p^2\right)^3}{8 \left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q1}-p)^2.(\text{q1}-\text{q2})^2.(\text{q2}-p)^2}-\frac{\left(m^2+p^2\right)^2}{4 \left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q1}-p)^2.(\text{q1}-\text{q2})^2}+\frac{\left(m^2+p^2\right) \left(m^2+2 p^2\right)}{4 \;\text{q1}^2.\text{q2}^2.\left((\text{q1}-p)^2-m^2\right).(\text{q1}-\text{q2})^2}-\frac{\left(m^2+p^2\right) (p\cdot \;\text{q1})}{4 \;\text{q1}^2.\text{q2}^2.(\text{q1}-\text{q2})^2.\left((\text{q2}-p)^2-m^2\right)}-\frac{\left(m^2+p^2\right) (p\cdot \;\text{q1})}{4 \left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q1}-\text{q2})^2.(\text{q2}-p)^2}-\frac{p\cdot \;\text{q1}}{4 \left(\text{q2}^2-m^2\right).(\text{q1}-p)^2.(\text{q1}-\text{q2})^2}-\frac{m^2+p\cdot \;\text{q1}+p^2}{4 \left(\text{q1}^2-m^2\right).(\text{q1}-\text{q2})^2.(\text{q2}-p)^2}+\frac{m^2+2 (p\cdot \;\text{q1})+p^2}{8 \left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q1}-\text{q2})^2}$$

```mathematica
SPD[q, p] FAD[{q, m}, {q - p, 0}] 
 
ApartFF[%, {q}]
```

$$\frac{p\cdot q}{\left(q^2-m^2\right).(q-p)^2}$$

$$\frac{m^2+p^2}{2 q^2.\left((q-p)^2-m^2\right)}-\frac{1}{2 \left(q^2-m^2\right)}$$

If the propagators should not be altered via momentum shifts (e.g. because they belong to a previously identified topology), use the option `FDS->False`

```mathematica
int = SPD[q2, p] SPD[q1, p] FAD[{q1, m}, {q2, m}, q1 - p, q2 - p, q2 - q1]
```

$$\frac{(p\cdot \;\text{q1}) (p\cdot \;\text{q2})}{\left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q1}-p)^2.(\text{q2}-p)^2.(\text{q2}-\text{q1})^2}$$

```mathematica
ApartFF[int, {q1, q2}]
```

$$\frac{\left(m^2+p^2\right)^2}{4 \left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q1}-p)^2.(\text{q1}-\text{q2})^2.(\text{q2}-p)^2}+\frac{m^2+p^2}{2 \;\text{q1}^2.\text{q2}^2.\left((\text{q1}-p)^2-m^2\right).(\text{q1}-\text{q2})^2}-\frac{m^2+p^2}{2 \left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q1}-p)^2.(\text{q1}-\text{q2})^2}-\frac{1}{2 \left(\text{q1}^2-m^2\right).(\text{q1}-\text{q2})^2.(\text{q2}-p)^2}+\frac{1}{4 \left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q1}-\text{q2})^2}$$

```mathematica
ApartFF[int, {q1, q2}, FDS -> False]
```

$$\frac{\left(m^2+p^2\right)^2}{4 \left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q1}-p)^2.(\text{q2}-p)^2.(\text{q2}-\text{q1})^2}+\frac{m^2+p^2}{4 \left(\text{q1}^2-m^2\right).(\text{q1}-p)^2.(\text{q2}-p)^2.(\text{q2}-\text{q1})^2}-\frac{m^2+p^2}{4 \left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q1}-p)^2.(\text{q2}-\text{q1})^2}-\frac{m^2+p^2}{4 \left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q2}-p)^2.(\text{q2}-\text{q1})^2}+\frac{m^2+p^2}{4 \left(\text{q2}^2-m^2\right).(\text{q1}-p)^2.(\text{q2}-p)^2.(\text{q2}-\text{q1})^2}-\frac{1}{4 \left(\text{q1}^2-m^2\right).(\text{q2}-p)^2.(\text{q2}-\text{q1})^2}-\frac{1}{4 \left(\text{q2}^2-m^2\right).(\text{q1}-p)^2.(\text{q2}-\text{q1})^2}+\frac{1}{4 \left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q2}-\text{q1})^2}+\frac{1}{4 (\text{q1}-p)^2.(\text{q2}-p)^2.(\text{q2}-\text{q1})^2}$$

If the partial fractioning should be performed only w. r. t. the denominators but not numerators, use the option `Numerator->False`

```mathematica
int = FAD[k, p - k, {k, m}] SPD[p, k]
```

$$\frac{k\cdot p}{k^2.(p-k)^2.\left(k^2-m^2\right)}$$

```mathematica
ApartFF[int, {k}]
```

$$\frac{m^2+p^2}{2 m^2 k^2.\left((k-p)^2-m^2\right)}+-\frac{1}{2 m^2 \left(k^2-m^2\right)}-\frac{p^2}{2 m^2 k^2.(k-p)^2}$$

```mathematica
ApartFF[int, {k}, Numerator -> False]
```

$$\frac{k\cdot p}{m^2 k^2.\left((k-p)^2-m^2\right)}-\frac{k\cdot p}{m^2 k^2.(k-p)^2}$$

Using the option `FeynAmpDenominator ->False` we can specify that integrals without numerators should not be partial fractioned

```mathematica
int = FAD[k, p - k, {k, m}] (SPD[q] + SPD[p, k])
```

$$\frac{k\cdot p+q^2}{k^2.(p-k)^2.\left(k^2-m^2\right)}$$

```mathematica
ApartFF[int, {k}]
```

$$\frac{m^2+p^2+2 q^2}{2 m^2 k^2.\left((k-p)^2-m^2\right)}+-\frac{1}{2 m^2 \left(k^2-m^2\right)}-\frac{p^2+2 q^2}{2 m^2 k^2.(k-p)^2}$$

```mathematica
ApartFF[int, {k}, FeynAmpDenominator -> False]
```

$$\frac{q^2}{k^2.\left(k^2-m^2\right).(p-k)^2}+\frac{m^2+p^2}{2 m^2 k^2.\left((k-p)^2-m^2\right)}+-\frac{1}{2 m^2 \left(k^2-m^2\right)}-\frac{p^2}{2 m^2 k^2.(k-p)^2}$$

The `extraPiece`-trick is useful for cases where a direct partial fractioning is not possible

```mathematica
int = (SFAD[{{0, k . l}}, p - k] SPD[k, p])
```

$$\frac{k\cdot p}{(k\cdot l+i \eta ).((p-k)^2+i \eta )}$$

Here `ApartFF` cannot do anything

```mathematica
ApartFF[int, {k}]
```

$$\frac{k\cdot p}{(k\cdot l+i \eta ).((p-k)^2+i \eta )}$$

Multiplying the integral with unity `FAD[k]*SPD[k]` we can cast into a more desirable form

```mathematica
ApartFF[int FAD[k], SPD[k], {k}] // ApartFF[#, {k}] &
```

$$\frac{k^2}{2 (k\cdot l+i \eta ).((p-k)^2+i \eta )}+\frac{p^2}{2 (k\cdot l+i \eta ).((k-p)^2+i \eta )}$$

Here we need a second call to `ApartFF` since the first execution doesn't drop scaleless integrals or perform any shifts in the denominators.

Other examples of doing partial fraction decomposition for eikonal integrals (e.g. in SCET) 

```mathematica
int1 = SFAD[{{0, nb . k2}}, {{0, nb . (k1 + k2)}}]
```

$$\frac{1}{(\text{k2}\cdot \;\text{nb}+i \eta ).((\text{k1}+\text{k2})\cdot \;\text{nb}+i \eta )}$$

```mathematica
ApartFF[int1, {k1, k2}]
```

$$\frac{1}{(\text{k2}\cdot \;\text{nb}+i \eta ).(\text{k1}\cdot \;\text{nb}+\text{k2}\cdot \;\text{nb}+i \eta )}$$

```mathematica
ApartFF[ApartFF[SFAD[{{0, nb . k1}}] int1, SPD[nb, k1], {k1, k2}], {k1, k2}]
```

$$\frac{1}{(\text{k1}\cdot \;\text{nb}+i \eta ).(\text{k2}\cdot \;\text{nb}+i \eta )}-\frac{1}{(\text{k1}\cdot \;\text{nb}+i \eta ).(\text{k1}\cdot \;\text{nb}+\text{k2}\cdot \;\text{nb}+i \eta )}$$

```mathematica
int2 = SPD[nb, k2] SFAD[{{0, nb . (k1 + k2)}}]
```

$$\frac{\text{k2}\cdot \;\text{nb}}{((\text{k1}+\text{k2})\cdot \;\text{nb}+i \eta )}$$

```mathematica
ApartFF[int2, {k1, k2}]
```

$$\frac{\text{k2}\cdot \;\text{nb}}{(\text{k1}\cdot \;\text{nb}+\text{k2}\cdot \;\text{nb}+i \eta )}$$

```mathematica
ApartFF[ApartFF[SFAD[{{0, nb . k1}}] int2, SPD[nb, k1], {k1, k2}], {k1, k2}]
```

$$-\frac{\text{k1}\cdot \;\text{nb}}{(\text{k1}\cdot \;\text{nb}+\text{k2}\cdot \;\text{nb}+i \eta )}$$

If we are working with a subset of propagators from a full integral, one should better turn off loop momentum shifts and the dropping of scaleless integrals

```mathematica
ApartFF[ApartFF[SFAD[{{0, nb . k1}}] int2, SPD[nb, k1], {k1, k2}], {k1, k2}, FDS -> False, 
  DropScaleless -> False]
```

$$1-\frac{\text{k1}\cdot \;\text{nb}}{(\text{k1}\cdot \;\text{nb}+\text{k2}\cdot \;\text{nb}+i \eta )}$$

When we need to deal with linearly dependent external momenta, there might be some relations between scalar products involving those momenta and loop momenta. Since the routine cannot determine those relations automatically, we need to supply them by hand via the option `FinalSubstitutions`

```mathematica
FCClearScalarProducts[];
int3 = SPD[p1, q] FAD[{q, m}, {q - p1 - p2}]
```

$$\frac{\text{p1}\cdot q}{\left(q^2-m^2\right).(-\text{p1}-\text{p2}+q)^2}$$

Supplying the kinematics alone doesn't work

```mathematica
kinRules = {SPD[p1] -> M^2, SPD[p2] -> M^2, SPD[p1, p2] -> M^2}
```

$$\left\{\text{p1}^2\to M^2,\text{p2}^2\to M^2,\text{p1}\cdot \;\text{p2}\to M^2\right\}$$

```mathematica
ApartFF[SPD[p1, q] FAD[{q, m}, {q - p1 - p2}], {q}, FinalSubstitutions -> kinRules]
```

$$\frac{\text{p1}\cdot q}{\left(q^2-m^2\right).(-\text{p1}-\text{p2}+q)^2}$$

Using `FCLoopFindTensorBasis` we see that `p2` is proportional to `p1`. Hence, we have an equality between `p1.q` and `p2.q`

```mathematica
FCLoopFindTensorBasis[{p1, p2}, kinRules, n]
```

$$\left(
\begin{array}{c}
 \;\text{p1} \\
 \;\text{p2} \\
 \;\text{p2}\to \;\text{p1} \;\text{FCGV}(\text{Prefactor})(1) \\
\end{array}
\right)$$

Supplying this information to `ApartFF` we can finally achieve the desired simplification

```mathematica
ApartFF[SPD[p1, q] FAD[{q, m}, {q - p1 - p2}], {q}, FinalSubstitutions -> Join[kinRules, 
    {SPD[q, p2] -> SPD[q, p1]}]]
```

$$\frac{m^2+4 M^2}{4 \left(q^2-m^2\right).(-\text{p1}-\text{p2}+q)^2}-\frac{1}{4 \left(q^2-m^2\right)}$$