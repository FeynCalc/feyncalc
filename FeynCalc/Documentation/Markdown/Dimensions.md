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

```mathematica
(FVD[p, \[Mu]] + FVE[p, \[Mu]]) (FVD[q, \[Mu]] + FVE[q, \[Mu]])
Contract[%]
```

$$\left(p^{\mu }+\hat{p}^{\mu }\right) \left(q^{\mu }+\hat{q}^{\mu }\right)$$

$$3 \left(\hat{p}\cdot \hat{q}\right)+p\cdot q$$

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

```mathematica
SP[p, q]
ChangeDimension[%, D]
```

$$\overline{p}\cdot \overline{q}$$

$$p\cdot q$$

To check the dimension of the given expression one can use `FCGetDimensions`

```mathematica
FVD[p, \[Mu]] FV[q, \[Mu]]
FCGetDimensions[%, {}]
```

$$p^{\mu } \overline{q}^{\mu }$$

$$\{4,D\}$$

If one needs to replace the dimensional symbols `D` in the prefactors of the Lorentz tensors, it is better to use `FCReplaceD` instead of a replacement rule. Otherwise, the dimensions of the tensors will get messed up

```mathematica
FCI[(D + 2) MTD[\[Mu], \[Nu]]] 
% /. D -> 4 - 2 Epsilon
```

$$(D+2) g^{\mu \nu }$$

$$(6-2 \varepsilon ) g_{\{4-2 \varepsilon ,4-2 \varepsilon \}}{}^{\mu \nu }$$

```mathematica
(D + 2) MTD[\[Mu], \[Nu]]
FCReplaceD[%, D -> 4 - 2 Epsilon] 

```mathematica

$$(D+2) g^{\mu \nu }$$

$$(6-2 \varepsilon ) g^{\mu \nu }$$