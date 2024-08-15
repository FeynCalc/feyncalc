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

![1umsofjfi8ghr](img/1umsofjfi8ghr.svg)

![0qnch25ldaf78](img/0qnch25ldaf78.svg)

![0gap32avu3bc5](img/0gap32avu3bc5.svg)

![0aknzgv9vc0z9](img/0aknzgv9vc0z9.svg)

$$\bar{\epsilon }^{\alpha \nu \rho \sigma } \bar{\epsilon }^{\mu \nu \rho \sigma }$$