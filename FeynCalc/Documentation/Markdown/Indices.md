```mathematica
 
```

## Handling indices

### See also

[Overview](Extra/FeynCalc.md).

### Manipulations

When you square an expression with dummy indices, you must rename them first. People often do this by hand, e.g. as in

```mathematica
ex1 = (FV[p, \[Mu]] + FV[q, \[Mu]]) FV[r, \[Mu]] FV[r, \[Nu]]
```

$$\overline{r}^{\mu } \overline{r}^{\nu } \left(\overline{p}^{\mu }+\overline{q}^{\mu }\right)$$

```mathematica
ex1 (ex1 /. \[Mu] -> \[Rho])
Contract[%]
```

$$\overline{r}^{\mu } \left(\overline{r}^{\nu }\right)^2 \overline{r}^{\rho } \left(\overline{p}^{\mu }+\overline{q}^{\mu }\right) \left(\overline{p}^{\rho }+\overline{q}^{\rho }\right)$$

$$\overline{r}^2 \left(\overline{p}\cdot \overline{r}+\overline{q}\cdot \overline{r}\right)^2$$

However, since FeynCalc 9 there is a function for that

```mathematica
FCRenameDummyIndices[ex1]
```

$$\overline{r}^{\nu } \overline{r}^{\text{\$AL}(\text{\$19})} \left(\overline{p}^{\text{\$AL}(\text{\$19})}+\overline{q}^{\text{\$AL}(\text{\$19})}\right)$$

```mathematica
ex1 FCRenameDummyIndices[ex1]
Contract[%]
```

$$\overline{r}^{\mu } \overline{r}^{\nu } \overline{r}^{\nu } \left(\overline{p}^{\mu }+\overline{q}^{\mu }\right) \overline{r}^{\text{\$AL}(\text{\$20})} \left(\overline{p}^{\text{\$AL}(\text{\$20})}+\overline{q}^{\text{\$AL}(\text{\$20})}\right)$$

$$\overline{r}^2 \left(\overline{p}\cdot \overline{r}+\overline{q}\cdot \overline{r}\right)^2$$

Notice that `FCRenameDummyIndices` does not canonicalize the indices

```mathematica
FV[p, \[Nu]] FV[q, \[Nu]] - FV[p, \[Mu]] FV[q, \[Mu]]
FCRenameDummyIndices[%]
```

$$\overline{p}^{\nu } \overline{q}^{\nu }-\overline{p}^{\mu } \overline{q}^{\mu }$$

$$\overline{p}^{\text{\$AL}(\text{\$22})} \overline{q}^{\text{\$AL}(\text{\$22})}-\overline{p}^{\text{\$AL}(\text{\$21})} \overline{q}^{\text{\$AL}(\text{\$21})}$$

But since FeynCalc 9.1 there is a function for that too

```mathematica
FV[p, \[Nu]] FV[q, \[Nu]] - FV[p, \[Mu]] FV[q, \[Mu]]
FCCanonicalizeDummyIndices[%]
```

$$\overline{p}^{\nu } \overline{q}^{\nu }-\overline{p}^{\mu } \overline{q}^{\mu }$$

$$0$$

Finally, often we also need to uncontract already contracted indices. This is done by `Uncontract`. By default, it handles only contractions with Dirac matrices and Levi-Civita tensors

```mathematica
LC[][p, q, r, s]
Uncontract[%, p]
Uncontract[%%, p, q]
```

$$\bar{\epsilon }^{\overline{p}\overline{q}\overline{r}\overline{s}}$$

$$\overline{p}^{\text{\$AL}(\text{\$31})} \bar{\epsilon }^{\text{\$AL}(\text{\$31})\overline{q}\overline{r}\overline{s}}$$

$$\overline{p}^{\text{\$AL}(\text{\$33})} \overline{q}^{\text{\$AL}(\text{\$32})} \left(-\bar{\epsilon }^{\text{\$AL}(\text{\$32})\text{\$AL}(\text{\$33})\overline{r}\overline{s}}\right)$$

```mathematica
SP[p, q]
Uncontract[%, p]
```

$$\overline{p}\cdot \overline{q}$$

$$\overline{p}\cdot \overline{q}$$

To uncontract scalar products as well, use the option `Pair->All`

```mathematica
Uncontract[%, p, Pair -> All]
```

$$\overline{p}^{\text{\$AL}(\text{\$34})} \overline{q}^{\text{\$AL}(\text{\$34})}$$