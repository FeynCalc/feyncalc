## SUNFierz

`SUNFierz[exp, {i, j, k, l}]` applies Fierz identity to the product of two Kronecker deltas the in fundamental representation (`SUNFDelta`) carrying indices `i`, `j`, `k` and `l` present in `exp`.

### See also

[Overview](Extra/FeynCalc.md), [SUNSimplify](SUNSimplify.md), [SUNT](SUNT.md), [SUNTF](SUNTF.md), [SUNFDelta](SUNFDelta.md).

### Examples

```mathematica
SUNFDelta[i, j] SUNFDelta[k, l] 
 
SUNFierz[%, {i, j, k, l}, SUNIndexNames -> {a}]

```mathematica

$$\delta _{ij} \delta _{kl}$$

$$2 T_{il}^a T_{kj}^a+\frac{\delta _{il} \delta _{jk}}{N}$$