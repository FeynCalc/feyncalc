## EtaSign

`EtaSign` is an option for `SFAD`, `GFAD`, `CFAD` and other objects representing propagators. It specifies the default sign of the $i \eta$ prescription  in the propagators, e.g. for standard Feynman propagators the value `1` corresponds to $\frac{1}{p^2-m^2 + i \eta}$, while the value `-1` sets $\frac{1}{p^2-m^2 - i \eta}$.

### See also

[Overview](Extra/FeynCalc.md), [SFAD](SFAD.md), [CFAD](CFAD.md), [GFAD](GFAD.md).

### Examples

Notice that if the sign of $i \eta$ is already specified in the propagator, e.g.

```mathematica
CFAD[{q, {m^2, 1}}]
```

$$\frac{1}{(q^2+m^2+i \eta )}$$

then this specification always overrides the EtaSign option. Hence

```mathematica
CFAD[{q, {m^2, 1}}, EtaSign -> -1]
```

$$\frac{1}{(q^2+m^2+i \eta )}$$

still has the positive $i \eta$.
