## Rename

`Rename` is an option for `Contract`. If set to `True`, dummy indices in `Eps` are renamed, using `$MU[i]`.

### See also

[Overview](Extra/FeynCalc.md), [Contract](Contract.md).

### Examples

```mathematica
LC[\[Mu], \[Nu], \[Rho], \[Sigma]] LC[\[Alpha], \[Nu], \[Rho], \[Sigma]]
Contract[%, EpsContract -> False, Rename -> True] 
  
 

```

$$\bar{\epsilon }^{\alpha \nu \rho \sigma } \bar{\epsilon }^{\mu \nu \rho \sigma }$$

$$\bar{\epsilon }^{\alpha \;\text{\$MU}(4)\text{\$MU}(5)\text{\$MU}(6)} \bar{\epsilon }^{\mu \;\text{\$MU}(4)\text{\$MU}(5)\text{\$MU}(6)}$$
