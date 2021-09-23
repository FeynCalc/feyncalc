## GLIMultiply

`GLIMultiply` is like `GLI` but with local multiplication properties.

### See also

[Overview](Extra/FeynCalc.md), [GLI](GLI.md).

### Examples

```mathematica
GLI["topo1", {1, 0, 0, 1, 1}] GLI["topo1", {0, -1, -1, 0, 0}]
% /. GLI -> GLIMultiply /. GLIMultiply -> GLI 
  
 

```

$$G^{\text{topo1}}(0,-1,-1,0,0) G^{\text{topo1}}(1,0,0,1,1)$$

$$G^{\text{topo1}}(1,-1,-1,1,1)$$
