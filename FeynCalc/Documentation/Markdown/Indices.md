## Handling indices

### See also

[Overview](Extra/FeynCalc.md).

### Manipulations of tensorial quantities

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

However, FeynCalc offers a function for that

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

There is a function for that too

```mathematica
FV[p, \[Nu]] FV[q, \[Nu]] - FV[p, \[Mu]] FV[q, \[Mu]]
FCCanonicalizeDummyIndices[%]
```

$$\overline{p}^{\nu } \overline{q}^{\nu }-\overline{p}^{\mu } \overline{q}^{\mu }$$

$$0$$

Often we also need to uncontract already contracted indices. This is done by `Uncontract`. By default, it handles only contractions with Dirac matrices and Levi-Civita tensors

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

Sometimes one might want to define custom symbolic tensors that are not specified in terms of the 4-vectors, metric tensors and Levi-Civitas. This is possible in FeynCalc, but the handling of such objects is not as good as that of the built-in quantities

```mathematica
DeclareFCTensor[myTensor];
```

```mathematica
myTensor[LorentzIndex[\[Mu]], LorentzIndex[\[Nu]]] FV[p, \[Nu]] FV[q, \[Mu]]
ex = Contract[%]
```

$$\overline{p}^{\nu } \overline{q}^{\mu } \;\text{myTensor}(\mu ,\nu )$$

$$\text{myTensor}\left(\overline{q},\overline{p}\right)$$

```mathematica
Uncontract[ex, p, q, Pair -> All]
```

$$\overline{p}^{\text{\$AL}(\text{\$36})} \overline{q}^{\text{\$AL}(\text{\$35})} \;\text{myTensor}(\text{\$AL}(\text{\$35}),\text{\$AL}(\text{\$36}))$$

```mathematica
(myTensor[LorentzIndex[\[Mu]], LorentzIndex[\[Nu]]] MT[LorentzIndex[\[Mu]], LorentzIndex[\[Nu]]] + 
   myTensor[LorentzIndex[\[Alpha]], LorentzIndex[\[Beta]]] MT[LorentzIndex[\[Alpha]], LorentzIndex[\[Beta]]])
FCCanonicalizeDummyIndices[%, LorentzIndexNames -> {i1, i2}]
```

$$\bar{g}^{\alpha \beta } \;\text{myTensor}(\alpha ,\beta )+\bar{g}^{\mu \nu } \;\text{myTensor}(\mu ,\nu )$$

$$2 \bar{g}^{\text{i1}\;\text{i2}} \;\text{myTensor}(\text{i1},\text{i2})$$

To extract the list of free or dummy indices present in the expression, one can use `FCGetFreeIndices` and `FCGetDummyIndices` respectively

```mathematica
FCI[FV[p, \[Mu]] FV[q, \[Nu]]] 
FCGetFreeIndices[%, {LorentzIndex}]
```

$$\overline{p}^{\mu } \overline{q}^{\nu }$$

$$\{\mu ,\nu \}$$

```mathematica
FCI[FV[p, \[Mu]] FV[q, \[Mu]]] 
FCGetDummyIndices[%, {LorentzIndex}]

```mathematica

$$\overline{p}^{\mu } \overline{q}^{\mu }$$

$$\{\mu \}$$