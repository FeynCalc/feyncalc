## $AL

`$AL` is the head for dummy indices which may be introduced by different functions. By default it is unset, but may be set to anything.

### See also

[Overview](Extra/FeynCalc.md), [Uncontract](Uncontract.md).

### Examples

```mathematica
Uncontract[ScalarProduct[p, q], q, Pair -> All]
```

$$\overline{p}^{\text{\$AL}(\text{\$11})} \overline{q}^{\text{\$AL}(\text{\$11})}$$

```mathematica
$AL = \[Mu]; 
 
Uncontract[ScalarProduct[p, q], q, Pair -> All]
```

$$\overline{p}^{\mu (\text{\$12})} \overline{q}^{\mu (\text{\$12})}$$

```mathematica
$AL =.;
```