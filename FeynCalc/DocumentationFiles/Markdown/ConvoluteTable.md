##  ConvoluteTable 

ConvoluteTable[f, g, x] yields the convolution of f and g. ConvoluteTable is called by Convolute.Options[Convolute].

###  See also 

PlusDistribution, Convolute.

###  Examples 

```mathematica
ConvoluteTable[1, 1, x] 
 
ConvoluteTable[x, x] 
 
ConvoluteTable[1, x, x]
```

$$-\log (x)$$

$$\text{False}[x,x]$$

$$1-x$$