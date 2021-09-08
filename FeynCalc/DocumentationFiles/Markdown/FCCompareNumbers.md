## FCCompareNumbers

`FCCompareNumbers[x, y]` compares two purely numerical or semi-numerical expressions `x` and `y` and returns the number of agreeing significant digits calculated from the relative differences.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

These two numbers disagree in their 6th significant digit

```mathematica
FCCompareNumbers[5.123542342 + 1.23145 I, 5.123542324 + 1.23146 I]
```

$$\text{FCCompareNumbers: Minimal number of significant digits to agree in: }6$$

$$\text{FCCompareNumbers: Chop is set to }\;\text{1.$\grave{ }$*${}^{\wedge}$-10}$$

$$\text{FCCompareNumbers: No number is set to 0. by Chop at this stage. }$$

$$\text{FCGV}(\text{I}) \;\text{FCGV}(\text{CommonDigits})(5.09042)$$

Requiring agreement only in the first 5 significant digits returns 0

```mathematica
FCCompareNumbers[5.123542342 + 1.23145 I, 5.123542324 + 1.23146 I, DigitCount -> 5]
```

$$\text{FCCompareNumbers: Minimal number of significant digits to agree in: }5$$

$$\text{FCCompareNumbers: Chop is set to }\;\text{1.$\grave{ }$*${}^{\wedge}$-10}$$

$$\text{FCCompareNumbers: No number is set to 0. by Chop at this stage. }$$

$$0$$

Here even the first significant digit doesn't agree (obviously)

```mathematica
FCCompareNumbers[5, 6]
```

$$\text{FCCompareNumbers: Minimal number of significant digits to agree in: }6$$

$$\text{FCCompareNumbers: Chop is set to }\;\text{1.$\grave{ }$*${}^{\wedge}$-10}$$

$$\text{FCCompareNumbers: No number is set to 0. by Chop at this stage. }$$

$$\text{FCGV}(\text{CommonDigits})(0.778151)$$

```mathematica
lhs = (0. + 0.*I) - (0.20132103165327941 - 0.00043434443313399246*I)*coeffO^2*parX^2 + (0.047227066764317975)*coeffO^2*parX^2*parY - (0.00005403882927314103)*coeffO^2*parX^2*parY^2 + (1.4588597782189382*^-6 - 4.06569606476957*^-13*I)*coeffO^2*parX^2*parY^3 + (0.03841797609570242 + 0.000028403733516153446*I)*coeffO^2*parX^2*parZ
```

$$(0.038418\, +0.0000284037 i) \;\text{coeffO}^2 \;\text{parX}^2 \;\text{parZ}-(0.201321\, -0.000434344 i) \;\text{coeffO}^2 \;\text{parX}^2+(0.\, +0. i)+(\text{1.4588597782189382$\grave{ }$*${}^{\wedge}$-6}-\text{4.06569606476957$\grave{ }$*${}^{\wedge}$-13} i) \;\text{coeffO}^2 \;\text{parX}^2 \;\text{parY}^3-0.0000540388 \;\text{coeffO}^2 \;\text{parX}^2 \;\text{parY}^2+0.0472271 \;\text{coeffO}^2 \;\text{parX}^2 \;\text{parY}$$

```mathematica
rhs = (-0.20132103165327922 + 0.0004343444331339952*I)*coeffO^2*parX^2 + (0.0472270672349811)*coeffO^2*parX^2*parY - (0.00005403887000187252)*coeffO^2*parX^2*parY^2 + 1.4588601127764193*^-6*coeffO^2*parX^2*parY^3 + (0.038417976095702376 + 0.000028403733516153537*I)*coeffO^2*parX^2*parZ
```

$$(0.038418\, +0.0000284037 i) \;\text{coeffO}^2 \;\text{parX}^2 \;\text{parZ}-(0.201321\, -0.000434344 i) \;\text{coeffO}^2 \;\text{parX}^2+\text{1.4588601127764193$\grave{ }$*${}^{\wedge}$-6} \;\text{coeffO}^2 \;\text{parX}^2 \;\text{parY}^3-0.0000540389 \;\text{coeffO}^2 \;\text{parX}^2 \;\text{parY}^2+0.0472271 \;\text{coeffO}^2 \;\text{parX}^2 \;\text{parY}$$

Here the two above expressions agree in their first 6 significant digits. Notice that the number of the size $10^{-13}$ is treated as a numerical fluctuation and consequently removed by `Chop`

```mathematica
FCCompareNumbers[lhs, rhs]
```

$$\text{FCCompareNumbers: Minimal number of significant digits to agree in: }6$$

$$\text{FCCompareNumbers: Chop is set to }\;\text{1.$\grave{ }$*${}^{\wedge}$-10}$$

$$\text{FCCompareNumbers: Following numbers on the l.h.s. are set to 0. by Chop: }\{-\text{4.06569606476957$\grave{ }$*${}^{\wedge}$-13}\}$$

$$0$$

The application of `Chop` can be of course disabled

```mathematica
FCCompareNumbers[lhs, rhs, Chop -> False] 
  
 

```

$$\text{FCCompareNumbers: Minimal number of significant digits to agree in: }6$$

$$\text{coeffO}^2 \;\text{parX}^2 \;\text{parY}^3 \;\text{FCGV}(\text{I}) \;\text{FCGV}(\text{Unmatched})(-\text{4.06569606476957$\grave{ }$*${}^{\wedge}$-13})$$
