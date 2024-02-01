## Dimensions

### See also

[Overview](Extra/FeynCalc.md).

### Notation for tensors living in different dimensions

You might have wondered why 4-vectors, scalar products and Dirac matrices all have a bar, like
$\bar{p}^\mu$ or $\bar{p} \cdot \bar{q}$. The bar is there to specify that they are 4-dimensional objects. Objects that live in $D$ dimensions do not have a bar, cf.

```mathematica
FVD[p, \[Mu]]
% // FCI // StandardForm
```

$$p^{\mu }$$

```mathematica
(*Pair[LorentzIndex[\[Mu], D], Momentum[p, D]]*)
```

```mathematica
MTD[\[Mu], \[Nu]]
% // FCI // StandardForm
```

$$g^{\mu \nu }$$

```mathematica
(*Pair[LorentzIndex[\[Mu], D], LorentzIndex[\[Nu], D]]*)
```

This origin of this notation is a [publication](https://inspirehep.net/record/124212) by Breitenlohner and Maison on the treatment of $\gamma^5$ in $D$ dimensions in the t'Hooft-Veltman scheme. The main idea was that we can decompose indexed objects into $4$- and $D-4$-dimensional pieces, e.g. $p^\mu = \bar{p}^\mu + \hat{p}^\mu$. Consequently, in FeynCalc we can also enter $D-4$-dimensional objects

```mathematica
FVE[p, \[Mu]]
% // FCI // StandardForm
```

$$\hat{p}^{\mu }$$

```mathematica
(*Pair[LorentzIndex[\[Mu], -4 + D], Momentum[p, -4 + D]]*)
```

```mathematica
MTE[p, q]
% // FCI // StandardForm
```

$$\hat{g}^{pq}$$

```mathematica
(*Pair[LorentzIndex[p, -4 + D], LorentzIndex[q, -4 + D]]*)
```

When we contract Lorentz tensors from different dimensions, the contractions are resolved according to the rules from the paper of Breitenlohner and Maison, e. g.

```mathematica
FVD[p, \[Mu]] FV[q, \[Mu]]
Contract[%]
```

$$p^{\mu } \overline{q}^{\mu }$$

$$\overline{p}\cdot \overline{q}$$

```mathematica
FV[p, \[Mu]] FVE[q, \[Mu]]
Contract[%]
```

$$\hat{q}^{\mu } \overline{p}^{\mu }$$

$$0$$

Sometimes we need to switch from one dimension to another, e.g. to convert a 4-dimensional object to a $D$-dimensional one or vice versa. This is done via

```mathematica
FVD[p, \[Mu]]
ChangeDimension[%, 4]
```

$$p^{\mu }$$

$$\overline{p}^{\mu }$$

The second argument of `ChangeDimension` is the new dimension . The most common choices are $4$, $D$ or $D-4$

```mathematica
FVD[p, \[Mu]]
ChangeDimension[%, D - 4]
```

$$p^{\mu }$$

$$\hat{p}^{\mu }$$