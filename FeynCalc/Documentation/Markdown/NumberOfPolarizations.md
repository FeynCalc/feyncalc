## NumberOfPolarizations

`NumberOfPolarizations` is an option for `DoPolarizationSums`. It specifies the number of polarizations to sum over in the expression.
This is relevant only for expressions that contain terms free of polarization vectors. This may occur e.g. if the scalar products involving
polarization vectors have already been assigned some particular values. In this case the corresponding terms will be multiplied by the
corresponding number of polarizations.

The default value is `Automatic` which means that the function will attempt to recognize the correct value automatically by
extracting the dimension `dim` of the polarization vectors and putting `(dim-2)` for massless and `(dim-1)` for massive vector bosons.
Notice that if the input expression is free of polarization vectors, the setting `Automatic` will fail, and the user must specify the correct
dimension by hand.

### See also

[Overview](Extra/FeynCalc.md), [DoPolarizationSums](DoPolarizationSums.md).

### Examples

```mathematica
PolarizationVector[p, mu] ComplexConjugate[PolarizationVector[p, mu]]
```

$$\bar{\varepsilon }^{*\text{mu}}(p) \bar{\varepsilon }^{\text{mu}}(p)$$

Here the setting Automatic is sufficient.

```mathematica
FCClearScalarProducts[];
ScalarProduct[p, p] = 0;
PolarizationVector[p, mu] ComplexConjugate[PolarizationVector[p, mu]] + xyz
DoPolarizationSums[%, p, n]
```

$$\bar{\varepsilon }^{*\text{mu}}(p) \bar{\varepsilon }^{\text{mu}}(p)+\text{xyz}$$

$$\text{DoPolarizationSums: The input expression contains terms free of polarization vectors. Those will be multiplied with the number of polarizations given by }2.$$

$$2 \;\text{xyz}-2$$

Here it is not

```mathematica
DoPolarizationSums[xyz, p, n]
```

![0emyzef54vvcu](img/0emyzef54vvcu.svg)

$$\text{\$Aborted}$$

Setting the number of polarizations by hand fixes the issue

```mathematica
DoPolarizationSums[xyz, p, n, NumberOfPolarizations -> 2] 
  
 

```

$$\text{DoPolarizationSums: The input expression contains terms free of polarization vectors. Those will be multiplied with the number of polarizations given by }2.$$

$$2 \;\text{xyz}$$