## FeynmanIntegralPrefactor

`FeynmanIntegralPrefactor` is an option for `FCFeynmanParametrize` and other functions. It denotes an implicit prefactor that has to be understood in front of a loop integral in the usual `FeynAmpDenominator`-notation. The prefactor is the quantity that multiplies the loop integral measure $d^D q_1 \ldots d^D q_n$ and plays an important role e.g. when deriving the Feynman parameter representation of the given integral. Apart from specifying an explicit value, the user may also choose from the following predefined conventions: 

- "Unity" - 1 for each loop
- "Textbook" - $\frac{1}{(2\pi)^D}$ for each loop.
- "Multiloop1" - $\frac{1}{i \pi^{D/2}}$ for each loop if the integral is Minkowskian, $\frac{1}{i \pi^{D/2}}$ or $\frac{1}{i \pi^{(D-1)/2}}$  for each loop if the integral is Euclidean or Cartesian respectively.
- "Multiloop2" - like "Multiloop1" but with an extra $e^{\frac{(4-D)}{2} \gamma_E}$  for each loop

The standard value is "Multiloop1".

### See also

[Overview](Extra/FeynCalc.md), [FCFeynmanParametrize](FCFeynmanParametrize.md).

### Examples

```mathematica
FCFeynmanParametrize[FAD[p, p - q], {p}, Names -> x, FCReplaceD -> {D -> 4 - 2 Epsilon}]
```

$$\left\{(x(1)+x(2))^{2 \varepsilon -2} \left(-q^2 x(1) x(2)\right)^{-\varepsilon },\Gamma (\varepsilon ),\{x(1),x(2)\}\right\}$$

```mathematica
FCFeynmanParametrize[FAD[p, p - q], {p}, Names -> x, FCReplaceD -> {D -> 4 - 2 Epsilon}]
Times @@ Most[%]
```

$$\left\{(x(1)+x(2))^{2 \varepsilon -2} \left(-q^2 x(1) x(2)\right)^{-\varepsilon },\Gamma (\varepsilon ),\{x(1),x(2)\}\right\}$$

$$\Gamma (\varepsilon ) (x(1)+x(2))^{2 \varepsilon -2} \left(-q^2 x(1) x(2)\right)^{-\varepsilon }$$

```mathematica
FCFeynmanParametrize[FAD[p, p - q], {p}, Names -> x, FCReplaceD -> {D -> 4 - 2 Epsilon}, 
 	FeynmanIntegralPrefactor -> "Multiloop1"]
Times @@ Most[%]
```

$$\left\{(x(1)+x(2))^{2 \varepsilon -2} \left(-q^2 x(1) x(2)\right)^{-\varepsilon },\Gamma (\varepsilon ),\{x(1),x(2)\}\right\}$$

$$\Gamma (\varepsilon ) (x(1)+x(2))^{2 \varepsilon -2} \left(-q^2 x(1) x(2)\right)^{-\varepsilon }$$

```mathematica
FCFeynmanParametrize[FAD[p, p - q], {p}, Names -> x, FCReplaceD -> {D -> 4 - 2 Epsilon}, 
 	FeynmanIntegralPrefactor -> "Unity"]
Times @@ Most[%]	
```

$$\left\{(x(1)+x(2))^{2 \varepsilon -2} \left(-q^2 x(1) x(2)\right)^{-\varepsilon },i \pi ^{2-\varepsilon } \Gamma (\varepsilon ),\{x(1),x(2)\}\right\}$$

$$i \pi ^{2-\varepsilon } \Gamma (\varepsilon ) (x(1)+x(2))^{2 \varepsilon -2} \left(-q^2 x(1) x(2)\right)^{-\varepsilon }$$

```mathematica
FCFeynmanParametrize[FAD[p, p - q], {p}, Names -> x, FCReplaceD -> {D -> 4 - 2 Epsilon}, 
 	FeynmanIntegralPrefactor -> "Textbook"]
Times @@ Most[%]	
```

$$\left\{(x(1)+x(2))^{2 \varepsilon -2} \left(-q^2 x(1) x(2)\right)^{-\varepsilon },i 2^{2 \varepsilon -4} \pi ^{\varepsilon -2} \Gamma (\varepsilon ),\{x(1),x(2)\}\right\}$$

$$i 2^{2 \varepsilon -4} \pi ^{\varepsilon -2} \Gamma (\varepsilon ) (x(1)+x(2))^{2 \varepsilon -2} \left(-q^2 x(1) x(2)\right)^{-\varepsilon }$$

```mathematica
FCFeynmanParametrize[FAD[p, p - q], {p}, Names -> x, FCReplaceD -> {D -> 4 - 2 Epsilon}, 
 	FeynmanIntegralPrefactor -> "Multiloop2"]
Times @@ Most[%]	
```

$$\left\{(x(1)+x(2))^{2 \varepsilon -2} \left(-q^2 x(1) x(2)\right)^{-\varepsilon },e^{\gamma  \varepsilon } \Gamma (\varepsilon ),\{x(1),x(2)\}\right\}$$

$$e^{\gamma  \varepsilon } \Gamma (\varepsilon ) (x(1)+x(2))^{2 \varepsilon -2} \left(-q^2 x(1) x(2)\right)^{-\varepsilon }$$

```mathematica
FCFeynmanParametrize[FAD[{p, m}], {p}, Names -> x, FCReplaceD -> {D -> 4 - 2 Epsilon}, 
 	FeynmanIntegralPrefactor -> "Multiloop2"]
Times @@ Most[%]	
Series[%, {Epsilon, 0, 1}] // Normal // FunctionExpand 
  
 

```

$$\left\{1,-e^{\gamma  \varepsilon } \Gamma (\varepsilon -1) \left(m^2\right)^{1-\varepsilon },\{\}\right\}$$

$$-e^{\gamma  \varepsilon } \Gamma (\varepsilon -1) \left(m^2\right)^{1-\varepsilon }$$

$$\frac{m^2}{\varepsilon }+\frac{1}{12} \varepsilon  \left(\pi ^2 m^2+12 m^2+6 m^2 \log ^2\left(m^2\right)-12 m^2 \log \left(m^2\right)\right)+m^2+m^2 \left(-\log \left(m^2\right)\right)$$
