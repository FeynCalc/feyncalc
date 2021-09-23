## $AL

`$AL` is the head for dummy indices which may be introduced by `Amputate` and `Uncontract`. By default it is unset, but may be set to anything.

### See also

[Overview](Extra/FeynCalc.md), [Amputate](Amputate.md), [Uncontract](Uncontract.md).

### Examples

```mathematica
Uncontract[ScalarProduct[p, q], q, Pair -> All]
```

$$\overline{p}^{\text{\$AL}(\text{\$24})} \overline{q}^{\text{\$AL}(\text{\$24})}$$

```mathematica
$AL = \[Mu];
Uncontract[ScalarProduct[p, q], q, Pair -> All]
```

$$\overline{p}^{\mu (\text{\$25})} \overline{q}^{\mu (\text{\$25})}$$

```mathematica
$AL =.;
```
