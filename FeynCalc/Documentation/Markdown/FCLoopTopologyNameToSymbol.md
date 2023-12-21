## FCLoopTopologyNameToSymbol

`FCLoopTopologyNameToSymbol[exp]` converts topology names in `FCTopology`s and `GLI`s that are strings to expressions. This can be useful when exporting expressions generated with Mathematica to other software tools.

Using the option `Except` one can exclude certain names from the conversion process.

### See also

[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md).

### Examples

```mathematica
FCLoopTopologyNameToSymbol[GLI["a1", {1, 1, 1}]] // InputForm
```

```mathematica
GLI[a1, {1, 1, 1}]
```

```mathematica
FCLoopTopologyNameToSymbol[FCTopology["topo2", {FAD[{p1, m}], FAD[{p1 - q, m}]}, {p1}, {q}, {}, {}]] // InputForm
```

```mathematica
GLI[topo2, {FAD[{p1, m}], FAD[{p1 - q, m}]}, {p1}, {q}, {}, {}]
```

```mathematica
FCLoopTopologyNameToSymbol[GLI["a1", {1, 1, 1}] + GLI["b1", {1, 1, 1}], Except -> {"a"}] // InputForm
```

```mathematica
GLI["a1", {1, 1, 1}] + GLI[b1, {1, 1, 1}]
```