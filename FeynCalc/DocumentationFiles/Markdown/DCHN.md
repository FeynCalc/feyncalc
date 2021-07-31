`DCHN[x, i, j]` is a chain of Dirac matrices x and is transformed into `DiracChain[FCI[x],DiracIndex[i],DiracIndex[j]]` by `FeynCalcInternal`.

### See also

[DiracChain](DiracChain), [DCHN](DCHN), [DiracIndex](DiracIndex), [DiracIndexDelta](DiracIndexDelta), [DiracChainJoin](DiracChainJoin), [DiracChainExpand](DiracChainExpand), [DiracChainFactor](DiracChainFactor).

### Examples

A standalone Dirac matrix with open Dirac indices

```mathematica
DCHN[GAD[\[Mu]], i, j]
```

$$\left(\gamma ^{\mu }\right){}_{ij}$$

A chain of Dirac matrices with open Dirac indices

```mathematica
DCHN[GAD[\[Mu]] . GAD[\[Nu]], i, j]
```

$$\left(\gamma ^{\mu }.\gamma ^{\nu }\right){}_{ij}$$

A single $\bar{u}$ spinor with an open Dirac index

```mathematica
DCHN[SpinorUBar[p, m], i]
```

$$\left(\bar{u}(p,m)\right)_i$$

A single $\bar{v}$ spinor with an open Dirac index

```mathematica
DCHN[SpinorVBar[p, m], i]
```

$$\left(\bar{v}(p,m)\right)_i$$

A single $u$ spinor with an open Dirac index

```mathematica
DCHN[i, SpinorU[p, m]]
```

$$(u(p,m))_i$$

A single $v$ spinor with an open Dirac index

```mathematica
DCHN[i, SpinorV[p, m]]
```

$$(v(p,m))_i$$

$\bar{u}$ spinor contracted with a chain of Dirac matrices

```mathematica
DCHN[GAD[\[Mu]] . GAD[\[Nu]], SpinorUBar[p, m], j]
```

$$\left(\bar{u}(p,m).\gamma ^{\mu }.\gamma ^{\nu }\right){}_j$$

$\bar{v}$ spinor contracted with a chain of Dirac matrices

```mathematica
DCHN[GAD[\[Mu]] . GAD[\[Nu]], SpinorVBar[p, m], j]
```

$$\left(\bar{v}(p,m).\gamma ^{\mu }.\gamma ^{\nu }\right){}_j$$

 $u$ spinor contracted with a chain of Dirac matrices

```mathematica
DCHN[GAD[\[Mu]] . GAD[\[Nu]], i, SpinorU[p, m]]
```

$$\left(\gamma ^{\mu }.\gamma ^{\nu }.u(p,m)\right){}_i$$

 $v$ spinor contracted with a chain of Dirac matrices

```mathematica
DCHN[GAD[\[Mu]] . GAD[\[Nu]], i, SpinorV[p, m]]
```

$$\left(\gamma ^{\mu }.\gamma ^{\nu }.v(p,m)\right){}_i$$