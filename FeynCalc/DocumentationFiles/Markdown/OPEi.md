## OPEi

`OPEi` etc. are variables with `DataType` `PositiveInteger` which are used in functions relating to the operator product expansion.

### See also

[Overview](Extra/FeynCalc.md), [OPEj](OPEj.md), [OPEk](OPEk.md), [OPEl](OPEl.md), [OPEn](OPEn.md), [OPEo](OPEo.md).

### Examples

```mathematica
OPEi
```

$$i$$

```mathematica
DataType[OPEi, OPEj, OPEk, OPEl, OPEm, OPEn, OPEo, PositiveInteger]
```

$$\{\text{True},\text{True},\text{True},\text{True},\text{True},\text{True},\text{True}\}$$

```mathematica
PowerSimplify[{(-1)^(2 OPEi), (-1)^(2 OPEj), (-1)^(2 OPEk), (-1)^(2 OPEl), (-1)^(2 OPEm), (-1)^(2 OPEn), (-1)^(2 OPEo)}]
```

$$\{1,1,1,1,1,1,1\}$$

Re has been changed:

```mathematica
{Re[OPEi] > -3, Re[OPEi] > -2, Re[OPEi] > -1,   Re[OPEi] > 0, Re[OPEi] > 1}
```

$$\{\Re(i)>-3,\Re(i)>-2,\Re(i)>-1,\Re(i)>0,\Re(i)>1\}$$

```mathematica
{Re[-OPEi + OPEm] > 0, Re[-OPEi + OPEm] > 1, Re[-OPEi + OPEm] > 2}
```

$$\{\Re(m-i)>0,\Re(m-i)>1,\Re(m-i)>2\}$$

```mathematica
{Re[OPEm] > -3, Re[OPEm] > -2, Re[OPEm] > -1,   Re[OPEm] > 0, Re[OPEm] > 1}
```

$$\{\Re(m)>-3,\Re(m)>-2,\Re(m)>-1,\Re(m)>0,\Re(m)>1\}$$
