`Integrate2` is like `Integrate`, but `Integrate2[a_Plus, b__] := Map[Integrate2[#, b]&, a]` ( more linear algebra and partial fraction decomposition is done)

`Integrate2[f[x] DeltaFunction[x], x] -> f[0]`

`Integrate2[f[x] DeltaFunction[x0-x], x] -> f[x0]`

`Integrate2[f[x] DeltaFunction[a + b x], x] -> Integrate[f[x] (1/Abs[b]) DeltaFunction[a/b + x], x]`, where `Abs[b] -> b`, if `b` is a symbol, and if `b = -c`, then `Abs[-c] -> c`, i.e., the variable contained in `b` is supposed to be positive.

 $\pi ^2$ is replaced by `6 Zeta2`.

`Integrate2[1/(1-y),{y,x,1}]` is interpreted as distribution, i.e. as `Integrate2[-1/(1-y)],{y, 0, x}] -> Log[1-y]`.

`Integrate2[1/(1-x),{x,0,1}] -> 0`

Since `Integrate2` does do a reordering and partial fraction decomposition before calling the integral table of `Integrate3`, it will in general be slower compared to Integrate3 for sums of integrals. I.e., if the integrand has already an expanded form and if partial fraction decomposition is not necessary it is more effective to use `Integrate3`.

### See also

[Overview](Extra/FeynCalc.md), [DeltaFunction](DeltaFunction.md), [Integrate3](Integrate3.md), [Integrate4](Integrate4.md), [Integrate5](Integrate5.md), [SumS](SumS.md), [SumT](SumT.md).

### Examples

```mathematica
Integrate2[Log[1 + x] Log[x]/(1 - x), {x, 0, 1}] // Timing
```

$$\left\{0.068947,\zeta (3)-\frac{3}{2} \zeta (2) \log (2)\right\}$$

Since `Integrate2` uses table-look-up methods it is much faster than Mathematica's Integrate.

```mathematica
Integrate2[PolyLog[2, x^2], {x, 0, 1}]
```

$$\zeta (2)-4+4 \log (2)$$

```mathematica
Integrate2[PolyLog[3, -x], {x, 0, 1}]
```

$$\frac{\zeta (2)}{2}-\frac{3 \zeta (3)}{4}+1-2 \log (2)$$

```mathematica
Integrate2[PolyLog[3, 1/(1 + x)], {x, 0, 1}]
```

$$\zeta (2) (-\log (2))+\frac{3 \zeta (3)}{4}+\frac{\log ^3(2)}{3}-\log ^2(2)+2 \log (2)$$

```mathematica
Integrate2[DeltaFunction[1 - x] f[x], {x, 0, 1}]
```

$$f(1)$$

`Integrate2` does integration in a Hadamard sense, i.e., $\int _0^1 \, f(x) \, d x$ means actually expanding the result of $\int _{\delta }^{1-\delta} \, f(x) \, dx$ up to $\mathcal{O}(\delta )$ and neglecting all $\delta$-dependent terms. E.g. $\int_{\delta }^{1-\delta} \frac{1}{1-x} \, d x = - \log (1-x) \biggl |_{\delta }^{1-\delta } = -\log (\delta )+log (1) \Rightarrow 0$

```mathematica
Integrate2[1/(1 - x), {x, 0, 1}]
```

$$0$$

In the physics literature sometimes the "+" notation is used. In FeynCalc the $\left(frac{1}{1-x} \right)_{+}$ is represented by `PlusDistribution}[1/(1-x)]` or just `1/(1-x)`

```mathematica
Integrate2[PlusDistribution[1/(1 - x)], {x, 0, 1}]
```

$$0$$

```mathematica
Integrate2[PolyLog[2, 1 - x]/(1 - x)^2, {x, 0, 1}]
```

$$2-\zeta (2)$$

```mathematica
Integrate2[(Log[x] Log[1 + x])/(1 + x), {x, 0, 1}]
```

$$-\frac{\zeta (3)}{8}$$

```mathematica
Integrate2[Log[x]^2/(1 - x), {x, 0, 1}]
```

$$2 \zeta (3)$$

```mathematica
Integrate2[PolyLog[2, -x]/(1 + x), {x, 0, 1}]
```

$$\frac{\zeta (3)}{4}-\frac{1}{2} \zeta (2) \log (2)$$

```mathematica
Integrate2[Log[x] PolyLog[2, x], {x, 0, 1}]
```

$$3-2 \zeta (2)$$

```mathematica
Integrate2[x PolyLog[3, x], {x, 0, 1}]
```

$$-\frac{\zeta (2)}{4}+\frac{\zeta (3)}{2}+\frac{3}{16}$$

```mathematica
Integrate2[(Log[x]^2 Log[1 - x])/(1 + x), {x, 0, 1}]
```

$$\zeta (4)+\zeta (2) \log ^2(2)-4 \;\text{Li}_4\left(\frac{1}{2}\right)-\frac{\log ^4(2)}{6}$$

```mathematica
Integrate2[PolyLog[2, ((x (1 - z) + z) (1 - x + x z))/z]/(1 - x + x z), {x, 0, 1}]
```

$$\frac{2 i \pi  \;\text{Li}_2(-z)}{1-z}-\frac{4 \;\text{Li}_3\left(\frac{1-z}{2}\right)}{1-z}+\frac{4 \;\text{Li}_3(1-z)}{1-z}+\frac{2 \;\text{Li}_3(-z)}{1-z}+\frac{4 \;\text{Li}_3\left(\frac{1}{z+1}\right)}{1-z}-\frac{4 \;\text{Li}_3\left(\frac{1-z}{z+1}\right)}{1-z}-\frac{4 \;\text{Li}_3\left(\frac{z+1}{2}\right)}{1-z}-\frac{2 \;\text{Li}_2(1-z) \log (z)}{1-z}-\frac{2 \;\text{Li}_2(-z) \log (z)}{1-z}+\frac{4 \;\text{Li}_2(-z) \log (1-z)}{1-z}-\frac{2 S_{12}(1-z)}{1-z}+\frac{i \pi  \zeta (2)}{1-z}-\frac{\zeta (2) \log (z)}{1-z}+\frac{2 \zeta (2) \log (1-z)}{1-z}+\frac{6 \zeta (2) \log (z+1)}{1-z}-\frac{4 \zeta (2) \log (2)}{1-z}+\frac{2 \zeta (3)}{1-z}+\frac{\log ^3(z)}{6 (1-z)}+\frac{4 \log ^3(2)}{3 (1-z)}-\frac{\log (1-z) \log ^2(z)}{1-z}-\frac{\log (z+1) \log ^2(z)}{1-z}-\frac{i \pi  \log ^2(z)}{2 (1-z)}-\frac{2 \log (1-z) \log ^2(z+1)}{1-z}-\frac{2 \log ^2(2) \log (1-z)}{1-z}-\frac{2 \log ^2(2) \log (z+1)}{1-z}+\frac{4 \log (1-z) \log (z+1) \log (z)}{1-z}+\frac{2 i \pi  \log (z+1) \log (z)}{1-z}+\frac{4 \log (2) \log (1-z) \log (z+1)}{1-z}$$

```mathematica
Apart[Integrate2[x^(OPEm - 1) PolyLog[3, 1 - x], {x, 0, 1}], OPEm]
```

$$-\frac{\zeta (2)}{m^2}-\frac{\zeta (2)}{m-1}+\frac{\zeta (2)+\zeta (2) \left(-S_1(m-2)\right)+S_{12}(m)+\zeta (3)}{m}$$

```mathematica
Integrate2[x^(OPEm - 1) Log[1 - x] Log[x] Log[1 + x]/(1 + x), {x, 0, 1}] // Simplify
% /. OPEm -> 2
N[%]
```

$$\frac{1}{24} (-1)^m \left(48 \zeta (4)+30 \zeta (2) \log ^2(2)+6 \zeta (2) S_{-1}^2(m-1)+18 \zeta (2) S_2(m-1)-24 \zeta (2) S_{1-1}(m-1)-12 S_{-2}(m-1) \left(\zeta (2)-\log (4) S_{-1}(m-1)-\log ^2(2)\right)-36 \zeta (2) \log (2) S_1(m-1)+12 S_{-1}(m-1) (\zeta (2) \log (8)-2 \zeta (3))+39 \zeta (3) S_1(m-1)+24 S_{-2-1-1}(m-1)+24 S_{-1-2-1}(m-1)+24 S_{-1-1-2}(m-1)+24 S_{1-21}(m-1)+24 S_{1-12}(m-1)+24 S_{2-11}(m-1)-12 \log ^2(2) S_2(m-1)+24 \log (2) S_3(m-1)-24 \log (2) S_{-21}(m-1)-24 \log (2) S_{-12}(m-1)-48 \;\text{Li}_4\left(\frac{1}{2}\right)-63 \zeta (3) \log (2)-2 \log ^4(2)\right)$$

$$\frac{1}{24} \left(48 \zeta (2)+48 \zeta (4)+30 \zeta (2) \log ^2(2)+12 \left(\zeta (2)-\log ^2(2)+\log (4)\right)-36 \zeta (2) \log (2)-48 \;\text{Li}_4\left(\frac{1}{2}\right)-12 (\zeta (2) \log (8)-2 \zeta (3))+39 \zeta (3)-63 \zeta (3) \log (2)-144-2 \log ^4(2)-12 \log ^2(2)+72 \log (2)\right)$$

$$0.0505138$$

```mathematica
Integrate2[x^(OPEm - 1) (PolyLog[3, (1 - x)/(1 + x)] - PolyLog[3, -((1 - x)/(1 + x))]), {x, 0, 1}]
```

$$\frac{3 \zeta (2) (-1)^m \log (2)}{2 m}-\frac{3 \zeta (2) \log (2)}{2 m}+\frac{\zeta (2) (-1)^m S_{-1}(m)}{m}-\frac{\zeta (2) S_{-1}(m)}{2 m}+\frac{\zeta (2) (-1)^m S_1(m)}{2 m}-\frac{\zeta (2) S_1(m)}{m}+\frac{(-1)^m S_{-3}(m)}{m}+\frac{(-1)^m S_{-2}(m) S_1(m)}{m}+\frac{S_1(m) S_2(m)}{m}+\frac{S_3(m)}{m}+\frac{(-1)^{m+1} S_{-21}(m)}{m}-\frac{S_{-1-2}(m)}{m}+\frac{(-1)^{m+1} S_{-12}(m)}{m}-\frac{S_{21}(m)}{m}-\frac{7 (-1)^m \zeta (3)}{8 m}+\frac{21 \zeta (3)}{8 m}$$

```mathematica
DataType[OPEm, PositiveInteger]
Integrate2[x^(OPEm - 1) DeltaFunction[1 - x], {x, 0, 1}]
```

$$\text{True}$$

$$1$$

This is the polarized non-singlet spin splitting function whose first moment vanishes.

```mathematica
t = SplittingFunction[PQQNS] /. FCGV[z_] :> ToExpression[z]
```

$$-8 C_F \left(C_F-\frac{C_A}{2}\right) \left(\frac{\left(x^2+1\right) \left(-2 \zeta (2)-4 \;\text{Li}_2(-x)+\log ^2(x)-4 \log (x+1) \log (x)\right)}{x+1}+4 (1-x)+2 (x+1) \log (x)\right)+C_A C_F \left(\frac{4 \left(x^2+1\right) \log ^2(x)}{1-x}+8 \zeta (2) (x+1)+\left(\frac{536}{9}-16 \zeta (2)\right) \left(\frac{1}{1-x}\right)_++\delta (1-x) \left(\frac{88 \zeta (2)}{3}-24 \zeta (3)+\frac{17}{3}\right)+\frac{4}{9} (53-187 x)-\frac{4}{3} \left(5 x-\frac{22}{1-x}+5\right) \log (x)\right)+C_F N_f \left(-\frac{8 \left(x^2+1\right) \log (x)}{3 (1-x)}+\left(-\frac{16 \zeta (2)}{3}-\frac{2}{3}\right) \delta (1-x)+\frac{88 x}{9}-\frac{80}{9} \left(\frac{1}{1-x}\right)_+-\frac{8}{9}\right)+C_F^2 \left(-\frac{16 \left(x^2+1\right) \log (1-x) \log (x)}{1-x}+\delta (1-x) (-24 \zeta (2)+48 \zeta (3)+3)-40 (1-x)-4 (x+1) \log ^2(x)-8 \left(2 x+\frac{3}{1-x}\right) \log (x)\right)$$

```mathematica
t // Expand
```

$$8 \zeta (2) C_A C_F-\frac{16 x^2 C_A C_F \;\text{Li}_2(-x)}{x+1}-\frac{16 C_A C_F \;\text{Li}_2(-x)}{x+1}-\frac{8 \zeta (2) x^2 C_A C_F}{x+1}+\frac{4 x^2 C_A C_F \log ^2(x)}{1-x}+\frac{4 x^2 C_A C_F \log ^2(x)}{x+1}-\frac{16 x^2 C_A C_F \log (x) \log (x+1)}{x+1}+\frac{88}{3} \zeta (2) C_A C_F \delta (1-x)+\frac{17}{3} C_A C_F \delta (1-x)+8 \zeta (2) x C_A C_F-\frac{8 \zeta (2) C_A C_F}{x+1}-16 \zeta (2) \left(\frac{1}{1-x}\right)_+ C_A C_F-24 \zeta (3) C_A C_F \delta (1-x)-\frac{892}{9} x C_A C_F+\frac{536}{9} \left(\frac{1}{1-x}\right)_+ C_A C_F+\frac{4 C_A C_F \log ^2(x)}{1-x}+\frac{4 C_A C_F \log ^2(x)}{x+1}+\frac{4}{3} C_A C_F \log (x)+\frac{4}{3} x C_A C_F \log (x)+\frac{88 C_A C_F \log (x)}{3 (1-x)}-\frac{16 C_A C_F \log (x) \log (x+1)}{x+1}+\frac{356 C_A C_F}{9}-\frac{8 x^2 C_F N_f \log (x)}{3 (1-x)}-\frac{16}{3} \zeta (2) C_F N_f \delta (1-x)-\frac{2}{3} C_F N_f \delta (1-x)+\frac{88}{9} x C_F N_f-\frac{80}{9} \left(\frac{1}{1-x}\right)_+ C_F N_f-\frac{8 C_F N_f \log (x)}{3 (1-x)}-\frac{8 C_F N_f}{9}+\frac{32 x^2 C_F^2 \;\text{Li}_2(-x)}{x+1}+\frac{32 C_F^2 \;\text{Li}_2(-x)}{x+1}+\frac{16 \zeta (2) x^2 C_F^2}{x+1}-\frac{8 x^2 C_F^2 \log ^2(x)}{x+1}-\frac{16 x^2 C_F^2 \log (1-x) \log (x)}{1-x}+\frac{32 x^2 C_F^2 \log (x) \log (x+1)}{x+1}-24 \zeta (2) C_F^2 \delta (1-x)+3 C_F^2 \delta (1-x)+\frac{16 \zeta (2) C_F^2}{x+1}+48 \zeta (3) C_F^2 \delta (1-x)+72 x C_F^2-4 x C_F^2 \log ^2(x)-\frac{8 C_F^2 \log ^2(x)}{x+1}-4 C_F^2 \log ^2(x)-32 x C_F^2 \log (x)-\frac{16 C_F^2 \log (1-x) \log (x)}{1-x}-\frac{24 C_F^2 \log (x)}{1-x}-16 C_F^2 \log (x)+\frac{32 C_F^2 \log (x) \log (x+1)}{x+1}-72 C_F^2$$

```mathematica
Integrate2[t, {x, 0, 1}] // Timing
```

$$\{0.080437,0\}$$

Expanding `t` with respect to `x` yields a form already suitable for `Integrate3` and therefore the following is faster:

```mathematica
Integrate3[Expand[t, x], {x, 0, 1}] // Expand // Timing
```

$$\{0.040291,0\}$$

```mathematica
Clear[t];
Integrate2[DeltaFunction[1 - x] f[x], {x, 0, 1}]
```

$$f(1)$$

```mathematica
Integrate2[x^5 Log[1 + x]^2, {x, 0, 1}]
N[%]
```

$$\frac{46 \log (2)}{45}-\frac{6959}{10800}$$

$$0.0641986$$

```mathematica
NIntegrate[x^5 Log[1 + x]^2, {x, 0, 1}]
```

$$0.0641986$$

```mathematica
Integrate2[x^(OPEm - 1) Log[1 + x]^2, {x, 0, 1}]
```

$$-\frac{2 (-1)^m S_1^2(m)}{m}+\frac{(-1)^m S_1\left(\frac{m-1}{2}\right) S_1(m)}{m}-\frac{S_1\left(\frac{m-1}{2}\right) S_1(m)}{m}+\frac{(-1)^m S_1\left(\frac{m}{2}\right) S_1(m)}{m}+\frac{S_1\left(\frac{m}{2}\right) S_1(m)}{m}+\frac{(-1)^m S_2\left(\frac{m-1}{2}\right)}{2 m}-\frac{S_2\left(\frac{m-1}{2}\right)}{2 m}+\frac{(-1)^m S_2\left(\frac{m}{2}\right)}{2 m}+\frac{S_2\left(\frac{m}{2}\right)}{2 m}-\frac{2 (-1)^m S_2(m)}{m}-\frac{2 (-1)^m S_{-11}(m)}{m}+\frac{4 (-1)^m \log (2) S_1(m)}{m}+\frac{(-1)^{m+1} \log (2) S_1\left(\frac{m-1}{2}\right)}{m}+\frac{\log (2) S_1\left(\frac{m-1}{2}\right)}{m}+\frac{(-1)^{m+1} \log (2) S_1\left(\frac{m}{2}\right)}{m}-\frac{\log (2) S_1\left(\frac{m}{2}\right)}{m}+\frac{(-1)^{m+1} \log ^2(2)}{m}+\frac{\log ^2(2)}{m}$$
