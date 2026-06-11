## InverseMellin

`InverseMellin[exp, y]` performs the inverse Mellin transform of polynomials in OPE. The inverse transforms are not calculated but a table-lookup is done.

WARNING: do not "trust" the results for the inverse Mellin transform involving SumT's; there is an unresolved inconsistency here (related to $(-1)^{m}$).

### See also

[Overview](Extra/FeynCalc.md), [DeltaFunction](DeltaFunction.md), [Integrate2](Integrate2.md), [OPEm](OPEm.md), [SumS](SumS.md), [SumT](SumT.md).

### Examples

```mathematica
InverseMellin[1/OPEm, y]
```

$$y^{m-1}$$

```mathematica
InverseMellin[1/(OPEm + 3), y]
```

$$y^{m+2}$$

```mathematica
InverseMellin[1, y]
```

$$y^{m-1} \delta (1-y)$$

```mathematica
InverseMellin[1/OPEm^4, y]
```

$$-\frac{1}{6} y^{m-1} \log ^3(y)$$

```mathematica
InverseMellin[1/OPEm + 1, y]
```

$$y^{m-1} \delta (1-y)+y^{m-1}$$

```mathematica
InverseMellin[1/i + 1, y, i]
```

$$y^{i-1} \delta (1-y)+y^{i-1}$$

The inverse operation to `InverseMellin` is done by `Integrate2`.

```mathematica
Integrate2[InverseMellin[1/OPEm, y], {y, 0, 1}]
```

$$\frac{1}{m}$$

Below is a list of all built-in basic inverse Mellin transforms .

```mathematica
list = {1, 1/(OPEm + n), 1/(-OPEm + n), PolyGamma[0, OPEm], SumS[1, -1 + OPEm], 
    SumS[1, -1 + OPEm]/(OPEm - 1), SumS[1, -1 + OPEm]/(1 - OPEm), SumS[1, -1 + OPEm]/(OPEm + 1), 
    SumS[1, -1 + OPEm]/OPEm^2, SumS[1, -1 + OPEm]/OPEm, SumS[1, -1 + OPEm]^2/OPEm, 
    SumS[2, -1 + OPEm], SumS[2, -1 + OPEm]/OPEm, SumS[3, -1 + OPEm], SumS[1, 1, -1 + OPEm], 
    SumS[1, OPEm - 1]^2, SumS[1, 2, -1 + OPEm], SumS[2, 1, -1 + OPEm],SumS[1, -1 + OPEm]^3, 
    SumS[1, -1 + OPEm] SumS[2, -1 + OPEm], SumS[1, 1, 1, -1 + OPEm]};
```

```mathematica
im[z_] := z -> InverseMellin[z, y]
```

```mathematica
im[OPEm^(-3)]
```

$$\frac{1}{m^3}\to \frac{1}{2} y^{m-1} \log ^2(y)$$

```mathematica
im[OPEm^(-2)]
```

$$\frac{1}{m^2}\to -y^{m-1} \log (y)$$

```mathematica
im[PolyGamma[0, OPEm]]
```

$$\psi ^{(0)}(m)\to -\gamma  y^{m-1} \delta (1-y)-\left(\frac{1}{1-y}\right)_+ y^{m-1}$$

```mathematica
im[SumS[1, OPEm - 1]]
```

$$S_1(m-1)\to \left(\frac{1}{1-y}\right)_+ \left(-y^{m-1}\right)$$

```mathematica
im[SumS[1, OPEm - 1]/(OPEm - 1)]
```

$$\frac{S_1(m-1)}{m-1}\to -y^{m-2} \log (1-y)$$

```mathematica
im[SumS[1, OPEm - 1]/(OPEm + 1)]
```

$$\frac{S_1(m-1)}{m+1}\to -y^{m-1}+y^m-y^m \log (1-y)+y^m \log (y)$$

```mathematica
im[SumS[1, -1 + OPEm]/OPEm^2]
```

$$\frac{S_1(m-1)}{m^2}\to y^{m-1} \left(\zeta (2)-\text{Li}_2(y)-\frac{1}{2} \log ^2(y)\right)$$

```mathematica
im[SumS[1, -1 + OPEm]/OPEm]
```

$$\frac{S_1(m-1)}{m}\to y^{m-1} (\log (y)-\log (1-y))$$

```mathematica
im[SumS[1, -1 + OPEm]^2/OPEm]
```

$$\frac{S_1^2(m-1)}{m}\to y^{m-1} \left(-3 \zeta (2)+\text{Li}_2(1-y)+2 \;\text{Li}_2(y)+\log ^2(1-y)+\frac{\log ^2(y)}{2}\right)$$

```mathematica
im[SumS[2, OPEm - 1]]
```

$$S_2(m-1)\to y^{m-1} \left(\zeta (2) \delta (1-y)+\frac{\log (y)}{1-y}\right)$$

```mathematica
im[SumS[2, OPEm - 1]/OPEm]
```

$$\frac{S_2(m-1)}{m}\to y^{m-1} \left(\zeta (2)-\text{Li}_2(1-y)-\frac{1}{2} \log ^2(y)\right)$$

```mathematica
im[SumS[3, OPEm - 1]]
```

$$S_3(m-1)\to y^{m-1} \left(\zeta (3) \delta (1-y)-\frac{\log ^2(y)}{2 (1-y)}\right)$$

```mathematica
im[SumS[1, 1, OPEm - 1]]
```

$$S_{11}(m-1)\to y^{m-1} \left(\frac{\log (1-y)}{1-y}\right)_+$$

```mathematica
im[SumS[2, 1, OPEm - 1]]
```

$$S_{21}(m-1)\to y^{m-1} \left(\frac{\text{Li}_2(y)}{1-y}-\zeta (2) \left(\frac{1}{1-y}\right)_++2 \zeta (3) \delta (1-y)\right)$$

```mathematica
im[SumS[1, 1, 1, OPEm - 1]]
```

$$S_{111}(m-1)\to -\frac{1}{2} y^{m-1} \left(\frac{\log ^2(1-y)}{1-y}\right){}_+$$

```mathematica
Clear[im, list];
```