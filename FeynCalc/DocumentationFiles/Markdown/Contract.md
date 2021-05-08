##  Contract 

Contract[expr] contracts pairs of Lorentz indices of metric tensors, four-vectors and (depending on the optionEpsContract) of Levi-Civita tensors in expr. For the contraction of Dirac matrices with each other use DiracSimplify. Contract[exp1, exp2] contracts (exp1*exp2), where exp1 and exp2 may be larger products of sums of metric tensors and 4-vectors..

###  See also 

Pair, CartesianPair, DiracSimplify, MomentumCombine.

###  Examples 

```mathematica
MT[\[Mu], \[Nu]] FV[p, \[Mu]] 
 
Contract[%] 
 
FV[p, \[Mu]] GA[\[Mu]] 
 
Contract[%] 
 
MT[\[Mu], \[Mu]]
```

$$\overline{p}^{\mu } \bar{g}^{\mu \nu }$$

$$\overline{p}^{\nu }$$

$$\bar{\gamma }^{\mu } \overline{p}^{\mu }$$

$$\bar{\gamma }\cdot \overline{p}$$

$$\bar{g}^{\mu \mu }$$

The default dimension for a metric tensor is 4.

```mathematica
Contract[%]
```

$$4$$

A short way to enter D-dimensional metric tensors is given by MTD.

```mathematica
MTD[\[Mu], \[Nu]]  MTD[\[Mu], \[Nu]] 
 
Contract[%] 
 
FV[p, \[Mu]] FV[q, \[Mu]] 
 
Contract[% ] 
 
FV[p - q, \[Mu]] FV[a - b, \[Mu]] 
 
Contract[%] 
 
FVD[p - q, \[Nu]] FVD[a - b, \[Nu]] 
 
Contract[%] 
 
LC[\[Mu], \[Nu], \[Alpha], \[Sigma]] FV[p, \[Sigma]] 
 
Contract[%] 
 
LC[\[Mu], \[Nu], \[Alpha], \[Beta]] LC[\[Mu], \[Nu], \[Alpha], \[Sigma]] 
 
Contract[%] 
 
LCD[\[Mu], \[Nu], \[Alpha], \[Beta]] LCD[\[Mu], \[Nu], \[Alpha], \[Sigma]] 
 
Contract[%] // Factor2
```

$$g^{\mu \nu }^2$$

$$D$$

$$\overline{p}^{\mu } \overline{q}^{\mu }$$

$$\overline{p}\cdot \overline{q}$$

$$\left(\overline{a}-\overline{b}\right)^{\mu } \left(\overline{p}-\overline{q}\right)^{\mu }$$

$$\overline{a}\cdot \overline{p}-\overline{a}\cdot \overline{q}-\overline{b}\cdot \overline{p}+\overline{b}\cdot \overline{q}$$

$$(a-b)^{\nu } (p-q)^{\nu }$$

$$a\cdot p-a\cdot q-b\cdot p+b\cdot q$$

$$\overline{p}^{\sigma } \bar{\epsilon }^{\mu \nu \alpha \sigma }$$

$$\bar{\epsilon }^{\alpha \mu \nu \overline{p}}$$

$$\bar{\epsilon }^{\mu \nu \alpha \beta } \bar{\epsilon }^{\mu \nu \alpha \sigma }$$

$$-6 \bar{g}^{\beta \sigma }$$

$$\overset{\text{}}{\epsilon }^{\mu \nu \alpha \beta } \overset{\text{}}{\epsilon }^{\mu \nu \alpha \sigma }$$

$$(1-D) (2-D) (3-D) g^{\beta \sigma }$$

Contractions of Cartesian tensors are also possible. They can live in $3$, $D-1$ or $D-4$ dimensions.

```mathematica
KD[i, j] CV[p, i] 
 
Contract[%] 
 
CV[p, i] CGA[i] 
 
Contract[%] 
 
KD[i, i] 
 
Contract[%] 
 
KD[i, j]^2 
 
Contract[%] 
 
CV[p - q, j] CV[a - b, j] 
 
Contract[%] 
 
CLC[i, j, k] CV[p, k] 
 
Contract[%] 
 
CLC[i, j, k] CLC[i, j, l] 
 
Contract[%] 
 
CLCD[i, j, k] CLCD[i, j, l] 
 
Contract[%] // Factor2
```

$$\overline{p}^i \bar{\delta }^{ij}$$

$$\overline{p}^j$$

$$\overline{\gamma }^i \overline{p}^i$$

$$\overline{\gamma }\cdot \overline{p}$$

$$\bar{\delta }^{ii}$$

$$3$$

$$\bar{\delta }^{ij}^2$$

$$3$$

$$\left(\overline{a}-\overline{b}\right)^j \left(\overline{p}-\overline{q}\right)^j$$

$$(\overline{a}-\overline{b})\cdot (\overline{p}-\overline{q})$$

$$\overline{p}^k \bar{\epsilon }^{ijk}$$

$$\bar{\epsilon }^{ij\overline{p}}$$

$$\bar{\epsilon }^{ijk} \bar{\epsilon }^{ijl}$$

$$2 \bar{\delta }^{kl}$$

$$\overset{\text{}}{\epsilon }^{ijk} \overset{\text{}}{\epsilon }^{ijl}$$

$$(2-D) (3-D) \delta ^{kl}$$