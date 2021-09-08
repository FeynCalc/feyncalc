## $LeviCivitaSign

`$LeviCivitaSign` is a global variable that determines the sign in the result of a Dirac trace of four gamma matrices and $\gamma^5$.  `$LeviCivitaSign` is by default set to `-1` which corresponds to the convention `Tr[LC[a,b,c,d,5]] = -4*I*Eps[a,b,c,d]`. Setting `$LeviCivitaSign=-I`  will switch to the FORM-convention.

### See also

[Overview](Extra/FeynCalc.md), [LC](LC.md), [Eps](Eps.md), [DiracTrace](DiracTrace.md).

### Examples

```mathematica
$LeviCivitaSign
Tr[GA[\[Mu], \[Nu], \[Rho], \[Sigma], 5]]
```

$$-1$$

$$-4 i \bar{\epsilon }^{\mu \nu \rho \sigma }$$

This sets the same convention as in FORM

```mathematica
$LeviCivitaSign = -I;
Tr[GA[\[Mu], \[Nu], \[Rho], \[Sigma], 5]]
```

$$4 \bar{\epsilon }^{\mu \nu \rho \sigma }$$

Back to the standard value

```mathematica
$LeviCivitaSign = -1;
```
