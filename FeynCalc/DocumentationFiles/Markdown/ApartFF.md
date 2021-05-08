##  ApartFF 

ApartFF[amp, {q1, q2, ...}] partial fractions loop integrals by decomposing them into simpler integrals that contain only linearly independent propagators. It uses FCApart as a backend and works and is suitable also for multiloop integrals.ApartFF uses FCApart as the backend for partial fractioning. The latter implements an algorithm which is very much based on the work and code of F. Feng (arXiv:1204.2314) that seems	to employ a variety Leinartas's algorithm (see arXiv:1206.4740 for its description). Unlike Feng's $Apart that works on general multivariate polynomials,	 FCApart is tailored to work only with FeynCalc's FeynAmpDenominator and Pair objects, i.e. it is less general. For the original $Apart see https://github.com/F-Feng/APartApartFF[amp * extraPiece1, extraPiece2, {q1, q2, ...}] is a special working mode of ApartFF, where the final result of partial fractioning amp*extraPiece1 is multiplied by extraPiece2. It is understood, that extraPiece1*extraPiece2 should be unity, e.g. when extraPiece1 is an FAD, while extraPiece is an SPD inverse to it. This mode should be useful for nonstandard integrals where the desired partial fraction decomposition can be performed only after multiplying amp with extraPiece1.

###  Examples 

```mathematica
FCApart
FCClearScalarProducts[]
SPD[q, q] FAD[{q, m}] 
 
ApartFF[%, {q}] 
 
SPD[q, p] SPD[q, r] FAD[{q}, {q - p}, {q - r}] 
 
ApartFF[%, {q}] 
 
FAD[{q}, {q - p}, {q + p}] 
 
ApartFF[%, {q}] 
 
SPD[p, q1] SPD[p, q2]^2 FAD[{q1, m}, {q2, m}, q1 - p, q2 - p, q1 - q2] 
 
ApartFF[%, {q1, q2}] 
 
SPD[q, p] FAD[{q, m}, {q - p, 0}] 
 
ApartFF[%, {q}]
```

$$\text{FCApart}$$

$$\frac{q^2}{q^2-m^2}$$

$$\frac{m^2}{q^2-m^2}$$

$$\frac{(p\cdot q) (q\cdot r)}{q^2.(q-p)^2.(q-r)^2}$$

$$\frac{p^2 r^2}{4 q^2.(q-r)^2.(q-p)^2}+\frac{p^2+2 (q\cdot r)+2 r^2}{4 q^2.(-p+q+r)^2}+-\frac{p^2}{4 q^2.(q-p)^2}-\frac{r^2}{4 q^2.(q-r)^2}$$

$$\frac{1}{q^2.(q-p)^2.(p+q)^2}$$

$$\frac{1}{p^2 q^2.(q-p)^2}-\frac{1}{p^2 q^2.(q-2 p)^2}$$

$$\frac{(p\cdot \text{q1}) (p\cdot \text{q2})^2}{\left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q1}-p)^2.(\text{q2}-p)^2.(\text{q1}-\text{q2})^2}$$

$$\frac{\left(m^2+p^2\right)^3}{8 \left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q2}-p)^2.(\text{q1}-\text{q2})^2.(\text{q1}-p)^2}-\frac{\left(m^2+p^2\right)^2}{4 \left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q1}-\text{q2})^2.(\text{q1}-p)^2}+\frac{\left(m^2+p^2\right) \left(m^2+2 p^2\right)}{4 \text{q2}^2.\text{q1}^2.\left((\text{q1}-p)^2-m^2\right).(\text{q1}-\text{q2})^2}-\frac{\left(m^2+p^2\right) (p\cdot \text{q1})}{4 \left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q2}-p)^2.(\text{q1}-\text{q2})^2}-\frac{\left(m^2+p^2\right) (p\cdot \text{q1})}{4 \text{q2}^2.\text{q1}^2.(\text{q1}-\text{q2})^2.\left((\text{q2}-p)^2-m^2\right)}-\frac{p\cdot \text{q1}}{4 \left(\text{q2}^2-m^2\right).(\text{q1}-\text{q2})^2.(\text{q1}-p)^2}-\frac{m^2+p\cdot \text{q1}+p^2}{4 \left(\text{q1}^2-m^2\right).(\text{q2}-p)^2.(\text{q1}-\text{q2})^2}+\frac{m^2+2 (p\cdot \text{q1})+p^2}{8 \left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q1}-\text{q2})^2}$$

$$\frac{p\cdot q}{\left(q^2-m^2\right).(q-p)^2}$$

$$\frac{m^2+p^2}{2 q^2.\left((q-p)^2-m^2\right)}-\frac{1}{2 \left(q^2-m^2\right)}$$

If the propagators should not altered via momentum shifts (e.g. because they belong to a previously identified topology), use $text{FDS}to text{False}$

```mathematica
ApartFF[%%, {q}, FDS -> False] 
 
SPD[q2, p] SPD[q1, p] FAD[{q1, m}, {q2, m}, q1 - p, q2 - p, q2 - q1] 
 
ApartFF[%, {q1, q2}] 
 
ApartFF[%%, {q1, q2}, FDS -> False]
```

$$\frac{m^2+p^2}{2 \left(q^2-m^2\right).(q-p)^2}+-\frac{1}{2 \left(q^2-m^2\right)}+\frac{1}{2 (q-p)^2}$$

$$\frac{(p\cdot \text{q1}) (p\cdot \text{q2})}{\left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q1}-p)^2.(\text{q2}-p)^2.(\text{q2}-\text{q1})^2}$$

$$\frac{\left(m^2+p^2\right)^2}{4 \left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q2}-p)^2.(\text{q1}-\text{q2})^2.(\text{q1}-p)^2}-\frac{m^2+p^2}{2 \left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q1}-\text{q2})^2.(\text{q1}-p)^2}+\frac{m^2+p^2}{2 \text{q2}^2.\text{q1}^2.\left((\text{q1}-p)^2-m^2\right).(\text{q1}-\text{q2})^2}-\frac{1}{2 \left(\text{q1}^2-m^2\right).(\text{q2}-p)^2.(\text{q1}-\text{q2})^2}+\frac{1}{4 \left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q1}-\text{q2})^2}$$

$$\frac{\left(m^2+p^2\right)^2}{4 \left(\text{q2}^2-m^2\right).(\text{q2}-p)^2.(\text{q2}-\text{q1})^2.\left(\text{q1}^2-m^2\right).(\text{q1}-p)^2}-\frac{m^2+p^2}{4 \left(\text{q1}^2-m^2\right).(\text{q1}-p)^2.\left(\text{q2}^2-m^2\right).(\text{q2}-\text{q1})^2}+\frac{m^2+p^2}{4 \left(\text{q1}^2-m^2\right).(\text{q1}-p)^2.(\text{q2}-p)^2.(\text{q2}-\text{q1})^2}-\frac{m^2+p^2}{4 \left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q2}-p)^2.(\text{q2}-\text{q1})^2}+\frac{m^2+p^2}{4 (\text{q1}-p)^2.\left(\text{q2}^2-m^2\right).(\text{q2}-p)^2.(\text{q2}-\text{q1})^2}-\frac{1}{4 (\text{q2}-\text{q1})^2.\left(\text{q1}^2-m^2\right).(\text{q2}-p)^2}-\frac{1}{4 (\text{q2}-\text{q1})^2.(\text{q1}-p)^2.\left(\text{q2}^2-m^2\right)}+\frac{1}{4 (\text{q2}-\text{q1})^2.\left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right)}+\frac{1}{4 (\text{q2}-\text{q1})^2.(\text{q1}-p)^2.(\text{q2}-p)^2}$$

If the partial fractioning should be performed only w. r. t. the denominators but not numerators, use $text{Numerator}to text{False}$

```mathematica
int = FAD[k, p - k, {k, m}] SPD[p, k] 
 
ApartFF[int, {k}] 
 
ApartFF[int, {k}, Numerator -> False] 
 
ClearAll[int]

```

$$\frac{k\cdot p}{k^2.(p-k)^2.\left(k^2-m^2\right)}$$

$$\frac{m^2+p^2}{2 m^2 k^2.\left((k-p)^2-m^2\right)}+-\frac{1}{2 m^2 \left(k^2-m^2\right)}-\frac{p^2}{2 m^2 k^2.(k-p)^2}$$

$$\frac{k\cdot p}{m^2 k^2.\left((k-p)^2-m^2\right)}-\frac{k\cdot p}{m^2 k^2.(k-p)^2}$$

Using $text{FeynAmpDenominator}to text{False}$ we can specify that integrals without numerators should not be partial fractioned

```mathematica
int = FAD[k, p - k, {k, m}] (SPD[q] + SPD[p, k]) 
 
ApartFF[int, {k}] 
 
ApartFF[int, {k}, FeynAmpDenominator -> False] 
 
ClearAll[int]

```

$$\frac{k\cdot p+q^2}{k^2.(p-k)^2.\left(k^2-m^2\right)}$$

$$\frac{m^2+p^2+2 q^2}{2 m^2 k^2.\left((k-p)^2-m^2\right)}+-\frac{1}{2 m^2 \left(k^2-m^2\right)}-\frac{p^2+2 q^2}{2 m^2 k^2.(k-p)^2}$$

$$\frac{q^2}{k^2.(p-k)^2.\left(k^2-m^2\right)}+\frac{m^2+p^2}{2 m^2 k^2.\left((k-p)^2-m^2\right)}+-\frac{1}{2 m^2 \left(k^2-m^2\right)}-\frac{p^2}{2 m^2 k^2.(k-p)^2}$$

The extraPiece-trick is useful for cases where a direct partial fractioning is not possible

```mathematica
int = (SFAD[{{0, k . l}}, p - k] SPD[k, p])
```

$$![0ocpgev9p29h7](img/0ocpgev9p29h7.png)$$

Here ApartFF cannot do anything 

```mathematica
ApartFF[int, {k}]
```

$$![07rltucz1g2ou](img/07rltucz1g2ou.png)$$

Multiplying the integral with unity FAD[k]*SPD[k] we can cast into a more desirable form

```mathematica
ApartFF[int FAD[k], SPD[k], {k}] // ApartFF[#, {k}] &
```

$$![1bqot991oq8ko](img/1bqot991oq8ko.png)$$

We need a second call to ApartFF since the first execution doesn't drop scaleless integrals or perform any shifts in the denominators.

```mathematica
ClearAll[int]
```