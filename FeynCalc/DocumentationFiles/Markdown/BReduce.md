##  BReduce 

BReduce is an option for B0, B00, B1, B11 determining whether reductions to A0 and B0 will be done..

###  See also 

A0, B0, B00, B1, B11.

###  Examples 

By default $B_0$ is not expressed in terms of $A_0$.

```mathematica
B0[0, s, s]
```

$$\text{B}_0(0,s,s)$$

With BReduceTrue, transformation is done.

```mathematica
B0[0, s, s, BReduce -> True]
```

$$-\frac{(2-D) \text{A}_0(s)}{2 s}$$