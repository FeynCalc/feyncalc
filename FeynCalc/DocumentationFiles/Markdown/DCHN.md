##  DCHN 

DCHN[x, i, j] is a chain of Dirac matrices x and is transformed into DiracChain[FCI[x],DiracIndex[i],DiracIndex[j]] by FeynCalcInternal..

###  See also 

DiracChain, DCHN, DiracIndex, DiracIndexDelta, DiracChainJoin, DiracChainExpand, DiracChainFactor.

###  Examples 

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

A single $overset{-}{u}$ spinor with an open Dirac index

```mathematica
DCHN[SpinorUBar[p, m], i]
```

$$\left(\bar{u}(p,m)\right)_i$$

A single $overset{-}{v}$ spinor with an open Dirac index

```mathematica
DCHN[SpinorVBar[p, m], i]
```

$$\left(\bar{v}(p,m)\right)_i$$

A single $text{u}$ spinor with an open Dirac index

```mathematica
DCHN[i, SpinorU[p, m]]
```

$$(u(p,m))_i$$

A single $text{v}$ spinor with an open Dirac index

```mathematica
DCHN[i, SpinorV[p, m]]
```

$$(v(p,m))_i$$

 $overset{-}{u}$ spinor contracted with a chain of Dirac matrices

```mathematica
DCHN[GAD[\[Mu]] . GAD[\[Nu]], SpinorUBar[p, m], j]
```

$$\left(\bar{u}(p,m).\gamma ^{\mu }.\gamma ^{\nu }\right){}_j$$

 $overset{-}{v}$ spinor contracted with a chain of Dirac matrices

```mathematica
DCHN[GAD[\[Mu]] . GAD[\[Nu]], SpinorVBar[p, m], j]
```

$$\left(\bar{v}(p,m).\gamma ^{\mu }.\gamma ^{\nu }\right){}_j$$

 $text{u}$ spinor contracted with a chain of Dirac matrices

```mathematica
DCHN[GAD[\[Mu]] . GAD[\[Nu]], i, SpinorU[p, m]]
```

$$\left(\gamma ^{\mu }.\gamma ^{\nu }.u(p,m)\right){}_i$$

 $text{v}$ spinor contracted with a chain of Dirac matrices

```mathematica
DCHN[GAD[\[Mu]] . GAD[\[Nu]], i, SpinorV[p, m]]
```

$$\left(\gamma ^{\mu }.\gamma ^{\nu }.v(p,m)\right){}_i$$