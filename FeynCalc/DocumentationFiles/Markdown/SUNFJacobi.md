## SUNFJacobi

`SUNFJacobi` is an option for `SUNSimplify`, indicating whether the Jacobi identity should be used.

### See also

[SUNF](SUNF), [SUNSimplify](SUNSimplify).

### Examples

```mathematica
SUNF[a, b, c] SUNF[e, f, c] // SUNSimplify[#, SUNFJacobi -> False] &
```

$$f^{cef} f^{abc}$$

```mathematica
SUNF[a, b, c] SUNF[e, f, c] // SUNSimplify[#, SUNFJacobi -> True] &
```

$$f^{bcf} f^{ace}-f^{acf} f^{bce}$$