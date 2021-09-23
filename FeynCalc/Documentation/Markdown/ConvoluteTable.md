## ConvoluteTable

`ConvoluteTable[f, g, x]` yields the convolution of `f` and `g`. `ConvoluteTable` is called by `Convolute`.

### See also

[Overview](Extra/FeynCalc.md), [PlusDistribution](PlusDistribution.md), [Convolute](Convolute.md).

### Examples

```mathematica
ConvoluteTable[1, 1, x]
```

$$-\log (x)$$

```mathematica
ConvoluteTable[x, x]
```

$$\text{False}[x,x]$$

```mathematica
ConvoluteTable[1, x, x]
```

$$1-x$$
