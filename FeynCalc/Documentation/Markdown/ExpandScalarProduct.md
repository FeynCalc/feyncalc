## ExpandScalarProduct

`ExpandScalarProduct[expr]` expands scalar products of sums of momenta in `expr`.

`ExpandScalarProduct` does not use `Expand` on `expr`.

### See also

[Overview](Extra/FeynCalc.md), [Calc](Calc.md), [MomentumExpand](MomentumExpand.md), [MomentumCombine](MomentumCombine.md), [FCVariable](FCVariable.md)

### Examples

```mathematica
SP[p1 + p2 + p3, p4 + p5 + p6] 
 
% // ExpandScalarProduct
```

$$(\overline{\text{p1}}+\overline{\text{p2}}+\overline{\text{p3}})\cdot (\overline{\text{p4}}+\overline{\text{p5}}+\overline{\text{p6}})$$

$$\overline{\text{p1}}\cdot \overline{\text{p4}}+\overline{\text{p1}}\cdot \overline{\text{p5}}+\overline{\text{p1}}\cdot \overline{\text{p6}}+\overline{\text{p2}}\cdot \overline{\text{p4}}+\overline{\text{p2}}\cdot \overline{\text{p5}}+\overline{\text{p2}}\cdot \overline{\text{p6}}+\overline{\text{p3}}\cdot \overline{\text{p4}}+\overline{\text{p3}}\cdot \overline{\text{p5}}+\overline{\text{p3}}\cdot \overline{\text{p6}}$$

```mathematica
SP[p, p - q] 
 
ExpandScalarProduct[%]
```

$$\overline{p}\cdot (\overline{p}-\overline{q})$$

$$\overline{p}^2-\overline{p}\cdot \overline{q}$$

```mathematica
FV[p - q, \[Mu]] 
 
ExpandScalarProduct[%]
```

$$\left(\overline{p}-\overline{q}\right)^{\mu }$$

$$\overline{p}^{\mu }-\overline{q}^{\mu }$$

```mathematica
SPD[p - q, q - r] 
 
ExpandScalarProduct[%]
```

$$(p-q)\cdot (q-r)$$

$$p\cdot q-p\cdot r+q\cdot r-q^2$$

Using the option `Momentum` one can limit the expansion to particular momenta

```mathematica
SP[p1 + p2 + p3, p4 + p5 + p6] 
 
ExpandScalarProduct[%, Momentum -> {p1}]
```

$$(\overline{\text{p1}}+\overline{\text{p2}}+\overline{\text{p3}})\cdot (\overline{\text{p4}}+\overline{\text{p5}}+\overline{\text{p6}})$$

$$\overline{\text{p1}}\cdot (\overline{\text{p4}}+\overline{\text{p5}}+\overline{\text{p6}})+(\overline{\text{p2}}+\overline{\text{p3}})\cdot (\overline{\text{p4}}+\overline{\text{p5}}+\overline{\text{p6}})$$

By default `ExpandScalarProduct` does not apply linearity to Levi-Civita tensors

```mathematica
LC[\[Mu]][p1 + p2, p3 + p4, p5 + p6] 
 
ExpandScalarProduct[%]
```

$$\bar{\epsilon }^{\mu \overline{\text{p1}}+\overline{\text{p2}}\;\overline{\text{p3}}+\overline{\text{p4}}\;\overline{\text{p5}}+\overline{\text{p6}}}$$

$$\bar{\epsilon }^{\mu \overline{\text{p1}}+\overline{\text{p2}}\;\overline{\text{p3}}+\overline{\text{p4}}\;\overline{\text{p5}}+\overline{\text{p6}}}$$

Using the option `EpsEvaluate` takes care of that

```mathematica
LC[\[Mu]][p1 + p2, p3 + p4, p5 + p6] 
 
ExpandScalarProduct[%, EpsEvaluate -> True]
```

$$\bar{\epsilon }^{\mu \overline{\text{p1}}+\overline{\text{p2}}\;\overline{\text{p3}}+\overline{\text{p4}}\;\overline{\text{p5}}+\overline{\text{p6}}}$$

$$\bar{\epsilon }^{\mu \overline{\text{p1}}\;\overline{\text{p3}}\;\overline{\text{p5}}}+\bar{\epsilon }^{\mu \overline{\text{p1}}\;\overline{\text{p3}}\;\overline{\text{p6}}}+\bar{\epsilon }^{\mu \overline{\text{p1}}\;\overline{\text{p4}}\;\overline{\text{p5}}}+\bar{\epsilon }^{\mu \overline{\text{p1}}\;\overline{\text{p4}}\;\overline{\text{p6}}}+\bar{\epsilon }^{\mu \overline{\text{p2}}\;\overline{\text{p3}}\;\overline{\text{p5}}}+\bar{\epsilon }^{\mu \overline{\text{p2}}\;\overline{\text{p3}}\;\overline{\text{p6}}}+\bar{\epsilon }^{\mu \overline{\text{p2}}\;\overline{\text{p4}}\;\overline{\text{p5}}}+\bar{\epsilon }^{\mu \overline{\text{p2}}\;\overline{\text{p4}}\;\overline{\text{p6}}}$$

One can use the options `EpsEvaluate` and `Momentum` together

```mathematica
LC[\[Mu]][p1 + p2, p3 + p4, p5 + p6] 
 
ExpandScalarProduct[%, EpsEvaluate -> True, Momentum -> {p1}]
```

$$\bar{\epsilon }^{\mu \overline{\text{p1}}+\overline{\text{p2}}\;\overline{\text{p3}}+\overline{\text{p4}}\;\overline{\text{p5}}+\overline{\text{p6}}}$$

$$\bar{\epsilon }^{\mu \overline{\text{p1}}\;\overline{\text{p3}}+\overline{\text{p4}}\;\overline{\text{p5}}+\overline{\text{p6}}}+\bar{\epsilon }^{\mu \overline{\text{p2}}\;\overline{\text{p3}}+\overline{\text{p4}}\;\overline{\text{p5}}+\overline{\text{p6}}}$$

Of course, the function is also applicable to Cartesian quantities

```mathematica
CSP[p1 + p2, p3 + p4] 
 
ExpandScalarProduct[%]
```

$$(\overline{\text{p1}}+\overline{\text{p2}})\cdot (\overline{\text{p3}}+\overline{\text{p4}})$$

$$\overline{\text{p1}}\cdot \overline{\text{p3}}+\overline{\text{p1}}\cdot \overline{\text{p4}}+\overline{\text{p2}}\cdot \overline{\text{p3}}+\overline{\text{p2}}\cdot \overline{\text{p4}}$$

```mathematica
CLC[][p1 + p2, p3 + p4, p5 + p6] 
 
ExpandScalarProduct[%, EpsEvaluate -> True]
```

$$\bar{\epsilon }^{\overline{\text{p1}}+\overline{\text{p2}}\;\overline{\text{p3}}+\overline{\text{p4}}\;\overline{\text{p5}}+\overline{\text{p6}}}$$

$$\bar{\epsilon }^{\overline{\text{p1}}\;\overline{\text{p3}}\;\overline{\text{p5}}}+\bar{\epsilon }^{\overline{\text{p1}}\;\overline{\text{p3}}\;\overline{\text{p6}}}+\bar{\epsilon }^{\overline{\text{p1}}\;\overline{\text{p4}}\;\overline{\text{p5}}}+\bar{\epsilon }^{\overline{\text{p1}}\;\overline{\text{p4}}\;\overline{\text{p6}}}+\bar{\epsilon }^{\overline{\text{p2}}\;\overline{\text{p3}}\;\overline{\text{p5}}}+\bar{\epsilon }^{\overline{\text{p2}}\;\overline{\text{p3}}\;\overline{\text{p6}}}+\bar{\epsilon }^{\overline{\text{p2}}\;\overline{\text{p4}}\;\overline{\text{p5}}}+\bar{\epsilon }^{\overline{\text{p2}}\;\overline{\text{p4}}\;\overline{\text{p6}}}$$

Sometimes one would like to have external momenta multiplied by symbolic parameters in the propagators. In this case one should first declare the corresponding variables to be of `FCVariable` type

```mathematica
DataType[a, FCVariable] = True;
DataType[b, FCVariable] = True;
```

```mathematica
ExpandScalarProduct[SP[P, Q] /. P -> a P1 + b P2] 
 
StandardForm[%]
```

$$a \left(\overline{\text{P1}}\cdot \overline{Q}\right)+b \left(\overline{\text{P2}}\cdot \overline{Q}\right)$$

```mathematica
(*a Pair[Momentum[P1], Momentum[Q]] + b Pair[Momentum[P2], Momentum[Q]]*)
```