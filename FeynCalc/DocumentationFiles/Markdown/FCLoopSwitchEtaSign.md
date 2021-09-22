`FCLoopSwitchEtaSign[exp, s]` switches the sign of $i \eta$ in all integrals to
`s`, where `s` can be `+1` or `-1`.

Notice to change the sign of $i \eta$ the function pulls out a factor $-1$ from the propagator.

### See also

[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [FCLoopGetEtaSigns](FCLoopGetEtaSigns.md).

### Examples

`FAD`s are automatically converted to `SFAD`s, since otherwise their $i \eta$ prescription
cannot be modified

```mathematica
FAD[{p, m}]
FCLoopSwitchEtaSign[%, 1]
```

$$\frac{1}{p^2-m^2}$$

$$\frac{1}{(p^2-m^2+i \eta )}$$

```mathematica
FAD[{p, m}]
FCLoopSwitchEtaSign[%, -1]
```

$$\frac{1}{p^2-m^2}$$

$$-\frac{1}{(-p^2+m^2-i \eta )}$$

```mathematica
SFAD[{p, m^2}]
FCLoopSwitchEtaSign[%, -1]
```

$$\frac{1}{(p^2-m^2+i \eta )}$$

$$-\frac{1}{(-p^2+m^2-i \eta )}$$

```mathematica
CFAD[{p, m^2}]
FCLoopSwitchEtaSign[%, 1] 
  
 

```

$$\frac{1}{(p^2+m^2-i \eta )}$$

$$-\frac{1}{(-p^2-m^2+i \eta )}$$