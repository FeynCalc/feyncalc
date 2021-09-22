## FCLoopGLIToSymbol

`FCLoopGLIToSymbol[exp]` converts `GLI`s to symbols.

The option `Head` determines the prefix of the symbol and can be set to `FCTopology` (default) or `GLI`

The option `Character` specifies the separator between to prefix and the indices.

### See also

[Overview](Extra/FeynCalc.md), [GLI](GLI.md), [SMPToSymbol](SMPToSymbol.md), [FCGVToSymbol](FCGVToSymbol.md).

### Examples

```mathematica
FCLoopGLIToSymbol[GLI[topo1, {1, 1, 1, 1, 1}]]
```

$$\text{topo1X11111}$$

```mathematica
FCLoopGLIToSymbol[GLI[topo1, {1, 1, 1, 1, 1}], Head -> GLI]
```

$$\text{GLIXtopo1X11111}$$

```mathematica
FCLoopGLIToSymbol[GLI[topo1, {1, 1, 1, 1, 1}], Character -> "$"]
```

$$\text{topo1\$11111}$$