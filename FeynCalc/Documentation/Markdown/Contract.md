## Contract

`Contract[expr]` contracts pairs of Lorentz or Cartesian indices of metric tensors, vectors and (depending on the value of the option `EpsContract`) of Levi-Civita tensors in `expr`.

For contractions of Dirac matrices with each other use `DiracSimplify`.

`Contract[exp1, exp2]` contracts `(exp1*exp2)`, where `exp1` and `exp2` may be larger products of sums of metric tensors and 4-vectors. This can be also useful when evaluating polarization sums, where `exp2` should be the product (or expanded sum) of the polarization sums for the vector bosons.

### See also

[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [CartesianPair](CartesianPair.md), [DiracSimplify](DiracSimplify.md), [MomentumCombine](MomentumCombine.md).

### Examples

```mathematica
MT[\[Mu], \[Nu]] FV[p, \[Mu]]
Contract[%]
```

$$\overline{p}^{\mu } \bar{g}^{\mu \nu }$$

$$\overline{p}^{\nu }$$

```mathematica
FV[p, \[Mu]] GA[\[Mu]]
Contract[%]
```

$$\bar{\gamma }^{\mu } \overline{p}^{\mu }$$

$$\bar{\gamma }\cdot \overline{p}$$

The default dimension for a metric tensor is 4.

```mathematica
MT[\[Mu], \[Mu]]
Contract[%]
```

$$\bar{g}^{\mu \mu }$$

$$4$$

A quick way to enter $D$-dimensional metric tensors is given by `MTD`.

```mathematica
MTD[\[Mu], \[Nu]]  MTD[\[Mu], \[Nu]]
Contract[%]
```

$$(g^{\mu \nu})^2$$

$$D$$

```mathematica
FV[p, \[Mu]] FV[q, \[Mu]]
Contract[% ]
```

$$\overline{p}^{\mu } \overline{q}^{\mu }$$

$$\overline{p}\cdot \overline{q}$$

```mathematica
FV[p - q, \[Mu]] FV[a - b, \[Mu]]
Contract[%]
```

$$\left(\overline{a}-\overline{b}\right)^{\mu } \left(\overline{p}-\overline{q}\right)^{\mu }$$

$$\overline{a}\cdot \overline{p}-\overline{a}\cdot \overline{q}-\overline{b}\cdot \overline{p}+\overline{b}\cdot \overline{q}$$

```mathematica
FVD[p - q, \[Nu]] FVD[a - b, \[Nu]]
Contract[%]
```

$$(a-b)^{\nu } (p-q)^{\nu }$$

$$a\cdot p-a\cdot q-b\cdot p+b\cdot q$$

```mathematica
LC[\[Mu], \[Nu], \[Alpha], \[Sigma]] FV[p, \[Sigma]]
Contract[%]
```

$$\overline{p}^{\sigma } \bar{\epsilon }^{\mu \nu \alpha \sigma }$$

$$\bar{\epsilon }^{\alpha \mu \nu \overline{p}}$$

```mathematica
LC[\[Mu], \[Nu], \[Alpha], \[Beta]] LC[\[Mu], \[Nu], \[Alpha], \[Sigma]] 
Contract[%]
```

$$\bar{\epsilon }^{\mu \nu \alpha \beta } \bar{\epsilon }^{\mu \nu \alpha \sigma }$$

$$-6 \bar{g}^{\beta \sigma }$$

```mathematica
LCD[\[Mu], \[Nu], \[Alpha], \[Beta]] LCD[\[Mu], \[Nu], \[Alpha], \[Sigma]]
Contract[%] // Factor2
```

$$\overset{\text{}}{\epsilon }^{\mu \nu \alpha \beta } \overset{\text{}}{\epsilon }^{\mu \nu \alpha \sigma }$$

$$(1-D) (2-D) (3-D) g^{\beta \sigma }$$

Contractions of Cartesian tensors are also possible. They can live in $3$, $D-1$ or $D-4$ dimensions.

```mathematica
KD[i, j] CV[p, i]
Contract[%]
```

$$\overline{p}^i \bar{\delta }^{ij}$$

$$\overline{p}^j$$

```mathematica
CV[p, i] CGA[i]
Contract[%]
```

$$\overline{\gamma }^i \overline{p}^i$$

$$\overline{\gamma }\cdot \overline{p}$$

```mathematica
KD[i, i]
Contract[%]
```

$$\bar{\delta }^{ii}$$

$$3$$

```mathematica
KD[i, j]^2
Contract[%]
```

$$(\bar{\delta}^{ij})^2$$

$$3$$

```mathematica
CV[p - q, j] CV[a - b, j]
Contract[%]
```

$$\left(\overline{a}-\overline{b}\right)^j \left(\overline{p}-\overline{q}\right)^j$$

$$(\overline{a}-\overline{b})\cdot (\overline{p}-\overline{q})$$

```mathematica
CLC[i, j, k] CV[p, k]
Contract[%]
```

$$\overline{p}^k \bar{\epsilon }^{ijk}$$

$$\bar{\epsilon }^{ij\overline{p}}$$

```mathematica
CLC[i, j, k] CLC[i, j, l] 
Contract[%]
```

$$\bar{\epsilon }^{ijk} \bar{\epsilon }^{ijl}$$

$$2 \bar{\delta }^{kl}$$

```mathematica
CLCD[i, j, k] CLCD[i, j, l] 
Contract[%] // Factor2 
  
 

```

$$\overset{\text{}}{\epsilon }^{ijk} \overset{\text{}}{\epsilon }^{ijl}$$

$$(2-D) (3-D) \delta ^{kl}$$
