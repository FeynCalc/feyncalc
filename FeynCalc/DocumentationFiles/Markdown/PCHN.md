## PCHN

`PCHN[x, i, j]` is a chain of Pauli matrices `x` and is transformed into `PauliChain[FCI[x],PauliIndex[i],PauliIndex[j]]` by `FeynCalcInternal`.

### See also

[Overview](Extra/FeynCalc.md), [PauliChain](PauliChain.md), [PauliIndex](PauliIndex.md), [PauliIndexDelta](PauliIndexDelta.md), [PauliChainJoin](PauliChainJoin.md), [PauliChainExpand](PauliChainExpand.md), [PauliChainFactor](PauliChainFactor.md).

### Examples

A standalone Pauli matrix with open Pauli indices

```mathematica
PCHN[CSID[a], i, j]
```

$$\left(\sigma ^a\right){}_{ij}$$

A chain of Pauli matrices with open Pauli indices

```mathematica
PCHN[CSID[a] . CSID[b], i, j]
```

$$\left(\sigma ^a.\sigma ^b\right){}_{ij}$$

A single $\xi ^{\dagger}$ spinor with an open Pauli index

```mathematica
PCHN[PauliXi[-I], i]
```

$$\left(\xi ^{\dagger }\right){}_i$$

A single $\eta ^{\dagger}$ spinor with an open Pauli index

```mathematica
PCHN[PauliEta[-I], i]
```

$$\left(\eta ^{\dagger }\right){}_i$$

A single $\xi$ spinor with an open Pauli index

```mathematica
PCHN[i, PauliXi[I]]
```

$$(\xi )_i$$

A single $\eta$ spinor with an open Pauli index

```mathematica
PCHN[i, PauliEta[I]]
```

$$(\eta )_i$$

 $\xi ^{\dagger}$ spinor contracted with a chain of Pauli matrices

```mathematica
PCHN[CSID[a] . CSID[b], PauliXi[-I], j]
```

$$\left(\xi ^{\dagger }.\sigma ^a.\sigma ^b\right){}_j$$

 $\eta ^{\dagger}$ spinor contracted with a chain of Pauli matrices

```mathematica
PCHN[CSID[a] . CSID[b], PauliEta[-I], j]
```

$$\left(\eta ^{\dagger }.\sigma ^a.\sigma ^b\right){}_j$$

 $\xi$ spinor contracted with a chain of Pauli matrices

```mathematica
PCHN[CSID[a] . CSID[b], i, PauliXi[I]]
```

$$\left(\sigma ^a.\sigma ^b.\xi \right){}_i$$

 $\eta$ spinor contracted with a chain of Pauli matrices

```mathematica
PCHN[CSID[a] . CSID[b], i, PauliEta[I]]
```

$$\left(\sigma ^a.\sigma ^b.\eta \right){}_i$$
