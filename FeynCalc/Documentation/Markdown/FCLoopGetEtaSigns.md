`FCLoopGetEtaSigns[exp]`  is an auxiliary function that extracts the signs of $i \eta$ from propagators present in the input expression.  The result is returned as a list, e.g. `{}`, `{1}`, `{-1}` or `{-1,1}`.

This is useful if one wants ensure that all propagators of the given integral or topology follow a particular $i \eta$ sign convention.

### See also

[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [FCLoopSwitchEtaSign](FCLoopSwitchEtaSign.md).

### Examples

```mathematica
FAD[{p, m}]
FCLoopGetEtaSigns[%]
```

$$\frac{1}{p^2-m^2}$$

$$\{1\}$$

```mathematica
SFAD[{p, m^2}]
FCLoopGetEtaSigns[%]
```

$$\frac{1}{(p^2-m^2+i \eta )}$$

$$\{1\}$$

```mathematica
SFAD[{I p, -m^2}, EtaSign -> -1]
FCLoopGetEtaSigns[%]
```

$$\frac{1}{(-p^2+m^2-i \eta )}$$

$$\{-1\}$$

```mathematica
CFAD[{p, m^2}]
FCLoopGetEtaSigns[%] 
  
 

```

$$\frac{1}{(p^2+m^2-i \eta )}$$

$$\{-1\}$$