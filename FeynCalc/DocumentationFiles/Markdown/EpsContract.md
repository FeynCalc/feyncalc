## EpsContract

`EpsContract[exp]` handles contractions of two Levi-Civita tensors. It is also an option of `Contract` and other functions that specifies whether such contractions should be done or not.

### See also

[Overview](Extra/FeynCalc.md), [Eps](Eps.md), [Contract](Contract.md).

### Examples

```mathematica
LCD[\[Mu], \[Nu], \[Rho], \[Sigma]]
EpsContract[% %] // Factor2
```

$$\overset{\text{}}{\epsilon }^{\mu \nu \rho \sigma }$$

$$(1-D) (2-D) (3-D) D$$

```mathematica
Contract[LCD[\[Mu], \[Nu], \[Rho], \[Sigma]]^2] // Factor2
```

$$(1-D) (2-D) (3-D) D$$

```mathematica
Contract[LCD[\[Mu], \[Nu], \[Rho], \[Sigma]]^2, EpsContract -> False]
```

$$\left(\overset{\text{}}{\epsilon }^{\mu \nu \rho \sigma }\right)^2$$
