##  ToSFAD 

ToSFAD[exp] converts all propagator denominators written as FAD or FeynAmpDenmoninator[...,PropagatorDenominator[...],...] to SFAD or FeynAmpDenmoninator[...,StandardPropagatorDenominator[...],...] respectively..

###  Examples 

```mathematica
ToSFAD[FAD[p]]
% // StandardForm
```

$$![111aiby8f65o5](img/111aiby8f65o5.png)$$

```
(*SFAD[{{p, 0}, {0, 1}, 1}]*)
```

```mathematica
ToSFAD[FAD[{p, m}]]
% // StandardForm
```

$$![09t78s3xudh8h](img/09t78s3xudh8h.png)$$

```
(*SFAD[{{p, 0}, {m^2, 1}, 1}]*)
```

```mathematica
ToSFAD[FAD[{p + q, m, 2}]]
% // StandardForm

```

$$![0p1osslrtwsqv](img/0p1osslrtwsqv.png)$$

```
(*SFAD[{{p + q, 0}, {m^2, 1}, 1}, {{p + q, 0}, {m^2, 1}, 1}]*)
```