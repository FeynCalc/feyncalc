##  FCLoopBasisPropagatorsToTopology 

`FCLoopBasisPropagatorsToTopology[{pr1, pr2, ...}]` takes the list of Pairs and FeynAmpDenominators `pr1, pr2, ...` and converts it into a list of propagators that can be used to describe a topology.

###  Examples 

```mathematica
{FAD[q]}
FCLoopBasisPropagatorsToTopology[%]
```

$$\left\{\frac{1}{q^2}\right\}$$

$$\left\{q^2\right\}$$

```mathematica
{FAD[{q, m}]}
FCLoopBasisPropagatorsToTopology[%]
```

$$\left\{\frac{1}{q^2-m^2}\right\}$$

$$\left\{q^2-m^2\right\}$$

```mathematica
{FAD[{q, m}], SPD[q, p]}
FCLoopBasisPropagatorsToTopology[%]
```

$$\left\{\frac{1}{q^2-m^2},p\cdot q\right\}$$

$$\left\{q^2-m^2,p\cdot q\right\}$$