```mathematica
 
```

## Uncontract

`Uncontract[exp, q1, q2, ...]` uncontracts `Eps` and `DiracGamma`.

`Uncontract[exp, q1, q2, Pair -> {p}]` uncontracts also $p \cdot q_1$ and $p \cdot q_2$;

The option `Pair -> All` uncontracts all momenta except `OPEDelta`.

### See also

[Overview](Extra/FeynCalc.md), [Contract](Contract.md).

### Examples

```mathematica
LC[\[Mu], \[Nu]][p, q] 
 
Uncontract[%, p]
```

$$\bar{\epsilon }^{\mu \nu \overline{p}\overline{q}}$$

$$\overline{p}^{\text{\$AL}(\text{\$19})} \bar{\epsilon }^{\mu \nu \;\text{\$AL}(\text{\$19})\overline{q}}$$

```mathematica
GS[p] 
 
Uncontract[%, p]
```

$$\bar{\gamma }\cdot \overline{p}$$

$$\bar{\gamma }^{\text{\$AL}(\text{\$20})} \overline{p}^{\text{\$AL}(\text{\$20})}$$

```mathematica
Uncontract[LC[\[Mu], \[Nu]][p, q], p, q]
```

$$\overline{p}^{\text{\$AL}(\text{\$22})} \overline{q}^{\text{\$AL}(\text{\$21})} \left(-\bar{\epsilon }^{\mu \nu \;\text{\$AL}(\text{\$21})\text{\$AL}(\text{\$22})}\right)$$

By default scalar products are not uncontracted.

```mathematica
Uncontract[SP[p, q], q]
```

$$\overline{p}\cdot \overline{q}$$

Use the option `Pair->All` to make the function take care of the scalar products as well

```mathematica
Uncontract[SP[p, q], q, Pair -> All]
```

$$\overline{p}^{\text{\$AL}(\text{\$23})} \overline{q}^{\text{\$AL}(\text{\$23})}$$

```mathematica
Uncontract[SP[p, q]^2, q, Pair -> All]
```

$$\overline{p}^{\text{\$AL}(\text{\$24})} \overline{p}^{\text{\$AL}(\text{\$25})} \overline{q}^{\text{\$AL}(\text{\$24})} \overline{q}^{\text{\$AL}(\text{\$25})}$$

For Cartesian scalar products you need to use the option `CartesianPair->All`

```mathematica
Uncontract[CSP[p, q], q, Pair -> All]
```

$$\overline{p}\cdot \overline{q}$$

```mathematica
Uncontract[CSP[p, q], q, CartesianPair -> All]
```

$$\overline{p}^{\text{\$AL}(\text{\$26})} \overline{q}^{\text{\$AL}(\text{\$26})}$$