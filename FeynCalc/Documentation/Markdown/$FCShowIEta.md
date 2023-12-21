## $FCShowIEta

The Boolean setting of `$FCShowIEta` determines whether $i \eta$ should be displayed in the typesetting of propagator objects (except for `FAD`s) or not. This setting affects only the TraditionalForm typesetting and has absolutely no influence on the internal handling of propagator denominators in FeynCalc.

### See also

[Overview](Extra/FeynCalc.md), [SFAD](SFAD.md), [CFAD](CFAD.md), [GFAD](GFAD.md).

### Examples

```mathematica
$FCShowIEta 
 
SFAD[{p, m^2}]
```

$$\text{True}$$

$$\frac{1}{(p^2-m^2+i \eta )}$$

```mathematica
$FCShowIEta = False 
 
SFAD[{p, m^2}]
```

$$\text{False}$$

$$\frac{1}{(p^2-m^2)}$$

```mathematica
$FCShowIEta = True
```

$$\text{True}$$