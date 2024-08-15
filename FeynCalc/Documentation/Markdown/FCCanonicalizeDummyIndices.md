## FCCanonicalizeDummyIndices

`FCCanonicalizeDummyIndices[expr]` canonicalizes dummy indices in the expression.

Following index types are supported: `LorentzIndex`, `CartesianIndex`, `SUNIndex`, `SUNFIndex`, `DiracIndex`, `PauliIndex`

In the case of Lorentz indices the option `Momentum` provides a possibility to limit the canonicalization only to particular `Momenta`. The option `LorentzIndexNames` can be used to assign specific names to  the canonicalized indices, to have say $\mu$, $\nu$, $\rho$ etc. instead of some random names.

For other index types the corresponding options are called `CartesianIndexNames`, `SUNIndexNames`, `SUNFIndexNames`, `DiracIndexNames` and `PauliIndexNames`.

### See also

[Overview](Extra/FeynCalc.md), [FCRenameDummyIndices](FCRenameDummyIndices.md).

### Examples

Canonicalization of Lorentz indices

```mathematica
FVD[q, mu] FVD[p, mu] + FVD[q, nu] FVD[p, nu] + FVD[q, si] FVD[r, si] 
 
FCCanonicalizeDummyIndices[%] // Factor2
```

$$p^{\text{mu}} q^{\text{mu}}+p^{\text{nu}} q^{\text{nu}}+q^{\text{si}} r^{\text{si}}$$

$$q^{\text{FCGV}(\text{li191})} \left(2 p^{\text{FCGV}(\text{li191})}+r^{\text{FCGV}(\text{li191})}\right)$$

```mathematica
Uncontract[SPD[q, p]^2, q, p, Pair -> All] 
 
FCCanonicalizeDummyIndices[%, LorentzIndexNames -> {\[Mu], \[Nu]}]
```

$$p^{\text{\$AL}(\text{\$28})} p^{\text{\$AL}(\text{\$29})} q^{\text{\$AL}(\text{\$28})} q^{\text{\$AL}(\text{\$29})}$$

$$p^{\mu } p^{\nu } q^{\mu } q^{\nu }$$

Canonicalization of Cartesian indices

```mathematica
CVD[p, i] CVD[q, i] + CVD[p, j] CVD[r, j] 
 
FCCanonicalizeDummyIndices[%] // Factor2
```

$$p^i q^i+p^j r^j$$

$$p^{\text{FCGV}(\text{ci391})} \left(q^{\text{FCGV}(\text{ci391})}+r^{\text{FCGV}(\text{ci391})}\right)$$

```mathematica
CVD[p, i] CVD[q, i] + CVD[p, j] CVD[r, j] 
 
FCCanonicalizeDummyIndices[%, CartesianIndexNames -> {a}] // Factor2
```

$$p^i q^i+p^j r^j$$

$$p^a \left(q^a+r^a\right)$$

Canonicalization of color indices

```mathematica
SUNT[a, b, a] + SUNT[c, b, c] 
 
FCCanonicalizeDummyIndices[%]
```

$$T^a.T^b.T^a+T^c.T^b.T^c$$

$$2 T^{\text{FCGV}(\text{sun601})}.T^b.T^{\text{FCGV}(\text{sun601})}$$

```mathematica
SUNT[a, b, a] + SUNT[c, b, c] 
 
FCCanonicalizeDummyIndices[%, SUNIndexNames -> {u}]
```

$$T^a.T^b.T^a+T^c.T^b.T^c$$

$$2 T^u.T^b.T^u$$

Canonicalization of Dirac indices

```mathematica
DCHN[GA[mu], i, j] DCHN[GA[nu], j, k] 
 
FCCanonicalizeDummyIndices[%]
```

$$\left(\bar{\gamma }^{\text{mu}}\right){}_{ij} \left(\bar{\gamma }^{\text{nu}}\right){}_{jk}$$

$$\left(\bar{\gamma }^{\text{mu}}\right){}_{i\text{FCGV}(\text{di771})} \left(\bar{\gamma }^{\text{nu}}\right){}_{\text{FCGV}(\text{di771})k}$$

```mathematica
DCHN[GA[mu], i, j] DCHN[GA[nu], j, k] 
 
FCCanonicalizeDummyIndices[%, DiracIndexNames -> {a}]
```

$$\left(\bar{\gamma }^{\text{mu}}\right){}_{ij} \left(\bar{\gamma }^{\text{nu}}\right){}_{jk}$$

$$\left(\bar{\gamma }^{\text{mu}}\right){}_{ia} \left(\bar{\gamma }^{\text{nu}}\right){}_{ak}$$

Canonicalization of Pauli indices

```mathematica
PCHN[CSI[a], i, j] PCHN[CSI[b], j, k] 
 
FCCanonicalizeDummyIndices[%]
```

$$\left(\overline{\sigma }^a\right){}_{ij} \left(\overline{\sigma }^b\right){}_{jk}$$

$$\left(\overline{\sigma }^a\right){}_{i\text{FCGV}(\text{pi921})} \left(\overline{\sigma }^b\right){}_{\text{FCGV}(\text{pi921})k}$$

```mathematica
PCHN[CSI[a], i, j] PCHN[CSI[b], j, k] 
 
FCCanonicalizeDummyIndices[%, PauliIndexNames -> {l}]
```

$$\left(\overline{\sigma }^a\right){}_{ij} \left(\overline{\sigma }^b\right){}_{jk}$$

$$\left(\overline{\sigma }^a\right){}_{il} \left(\overline{\sigma }^b\right){}_{lk}$$

Using the option `Head` one can specify which index heads should be canonicalized,
while the rest will be ignored.

```mathematica
(QuantumField[Superscript[\[Phi], "+"], PauliIndex[k1], PauliIndex[k2], 
     R, r] . QuantumField[FCPartialD[{CartesianIndex[i], r}], 
     FCPartialD[{CartesianIndex[i], r}], \[Phi], PauliIndex[k2], PauliIndex[k1], R, r]) 
 
FCCanonicalizeDummyIndices[%, CartesianIndexNames -> {j}, Head -> {CartesianIndex}]
```

$$\phi ^{+\text{k1}\;\text{k2}Rr}.\left(\partial _{\{i,r\}}\partial _{\{i,r\}}\phi ^{\text{k2}\;\text{k1}Rr}\right)$$

$$\phi ^{+\text{k1}\;\text{k2}Rr}.\left(\partial _{\{j,r\}}\partial _{\{j,r\}}\phi ^{\text{k2}\;\text{k1}Rr}\right)$$