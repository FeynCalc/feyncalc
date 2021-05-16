##  FCReplaceD 

FCReplaceD[expr, rule] replaces D in expr accoding to the supplied replacement rule (e.g. D -> 4 - 2*Epsilon) but doesn't touch D inside Pairs and DiracGammas, i.e the dimension of scalar products, metric tensors and dirac matrices is unchanged. The latter can and should be done via ChangeDimension..

###  See also 

Applying the replacement rule directly to the expression doesn't give the desired result

###  Examples 

```mathematica
FCI[D MTD[\[Mu], \[Nu]]]
% /. D -> 4 - 2 Epsilon
```

$$D g^{\mu \nu }$$

$$(4-2 \varepsilon ) g_{\{4-2 \varepsilon ,4-2 \varepsilon \}}{}^{\mu \nu }$$

With FCReplaceD we get what we want

```mathematica
FCReplaceD[D MTD[\[Mu], \[Nu]], D -> 4 - 2 Epsilon]
ChangeDimension[%, 4]
```

$$(4-2 \varepsilon ) g^{\mu \nu }$$

$$(4-2 \varepsilon ) \bar{g}^{\mu \nu }$$