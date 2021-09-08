## Kummer

`Kummer[i][exp]` applies Kummer relation number `i` ($i =1, ... 24, 94, 95, 96$) to all `Hypergeometric2F1` in exp.

$i = 94$ corresponds to Eq. 9.131.2, $i = 95$ to Eq. 9.132.1 and $i = 96$ to Eq. 9.132.2 in Gradshteyn & Ryzhik.

### See also

[Overview](Extra/FeynCalc.md), [HypergeometricAC](HypergeometricAC.md).

### Examples

```mathematica
Hypergeometric2F1[a, b, c, z] == Kummer[2][Hypergeometric2F1[a, b, c, z]]
```

$$\, _2F_1(a,b;c;z)=(1-z)^{-a-b+c} \, _2F_1(c-a,c-b;c;z)$$

```mathematica
Hypergeometric2F1[a, b, c, z] == Kummer[3][Hypergeometric2F1[a, b, c, z]]
```

$$\, _2F_1(a,b;c;z)=(1-z)^{-a} \, _2F_1\left(a,c-b;c;-\frac{z}{1-z}\right)$$

```mathematica
Hypergeometric2F1[a, b, c, z] == Kummer[4][Hypergeometric2F1[a, b, c, z]]
```

$$\, _2F_1(a,b;c;z)=(1-z)^{-b} \, _2F_1\left(b,c-a;c;-\frac{z}{1-z}\right)$$

```mathematica
Hypergeometric2F1[a, b, c, 1 - z] == Kummer[6][Hypergeometric2F1[a, b, c, 1 - z]]
```

$$\, _2F_1(a,b;c;1-z)=z^{-a-b+c} \, _2F_1(c-a,c-b;c;1-z)$$

```mathematica
Hypergeometric2F1[a, b, a + b + 1 - c, 1 - z] == Kummer[6][Hypergeometric2F1[a, b, a + b + 1 - c, 1 - z]]
```

$$\, _2F_1(a,b;a+b-c+1;1-z)=z^{1-c} \, _2F_1(a-c+1,b-c+1;a+b-c+1;1-z)$$

```mathematica
Hypergeometric2F1[a, b, c, 1 - z] == Kummer[7][Hypergeometric2F1[a, b, c, 1 - z]]
```

$$\, _2F_1(a,b;c;1-z)=z^{-a} \, _2F_1\left(a,c-b;c;-\frac{1-z}{z}\right)$$

```mathematica
Hypergeometric2F1[a, b, a + b + 1 - c, 1 - z] == Kummer[7][Hypergeometric2F1[a, b, a + b + 1 - c, 1 - z]]
```

$$\, _2F_1(a,b;a+b-c+1;1-z)=z^{-a} \, _2F_1\left(a,a-c+1;a+b-c+1;-\frac{1-z}{z}\right)$$

```mathematica
Hypergeometric2F1[a, b, c, 1 - z] == Kummer[8][Hypergeometric2F1[a, b, c, 1 - z]]
```

$$\, _2F_1(a,b;c;1-z)=z^{-b} \, _2F_1\left(b,c-a;c;-\frac{1-z}{z}\right)$$

```mathematica
Hypergeometric2F1[a, b, a + b + 1 - c, 1 - z] == Kummer[8][Hypergeometric2F1[a, b, a + b + 1 - c, 1 - z]]
```

$$\, _2F_1(a,b;a+b-c+1;1-z)=z^{-b} \, _2F_1\left(b,b-c+1;a+b-c+1;-\frac{1-z}{z}\right)$$

```mathematica
Hypergeometric2F1[a, b, c, z^(-1)] == Kummer[10][Hypergeometric2F1[a, b, c, z^(-1)]]
```

$$\, _2F_1\left(a,b;c;\frac{1}{z}\right)=(1-z)^{-a-b+c} (-z)^{a+b-c} \, _2F_1\left(c-a,c-b;c;\frac{1}{z}\right)$$

```mathematica
Hypergeometric2F1[a, a + 1 - c, a + 1 - b, z^(-1)] == Kummer[10][Hypergeometric2F1[a, a + 1 - c, a + 1 - b, z^(-1)]]
```

$$\, _2F_1\left(a,a-c+1;a-b+1;\frac{1}{z}\right)=(1-z)^{-a-b+c} (-z)^{a+b-c} \, _2F_1\left(1-b,c-b;a-b+1;\frac{1}{z}\right)$$

```mathematica
Hypergeometric2F1[a, b, c, z^(-1)] == Kummer[11][Hypergeometric2F1[a, b, c, z^(-1)]]
```

$$\, _2F_1\left(a,b;c;\frac{1}{z}\right)=\left(-\frac{1-z}{z}\right)^{-a} (-z)^a \, _2F_1\left(a,c-b;c;\frac{1}{1-z}\right)$$

```mathematica
Hypergeometric2F1[a, a + 1 - c, a + 1 - b, z^(-1)] == Kummer[11][Hypergeometric2F1[a, a + 1 - c, a + 1 - b, z^(-1)]]
```

$$\, _2F_1\left(a,a-c+1;a-b+1;\frac{1}{z}\right)=\left(-\frac{1-z}{z}\right)^{-a} (-z)^a \, _2F_1\left(a,c-b;a-b+1;\frac{1}{1-z}\right)$$

```mathematica
Hypergeometric2F1[a, b, c, z^(-1)] == Kummer[12][Hypergeometric2F1[a, b, c, z^(-1)]]
```

$$\, _2F_1\left(a,b;c;\frac{1}{z}\right)=(1-z)^{-b} (-z)^{b-a} \, _2F_1\left(b,c-a;c;\frac{1}{1-z}\right)$$

```mathematica
Hypergeometric2F1[a, a + 1 - c, a + 1 - b, z^(-1)] == Kummer[12][Hypergeometric2F1[a, a + 1 - c, a + 1 - b, z^(-1)]]
```

$$\, _2F_1\left(a,a-c+1;a-b+1;\frac{1}{z}\right)=(-z)^{1-c} (1-z)^{-a+c-1} \, _2F_1\left(1-b,a-c+1;a-b+1;\frac{1}{1-z}\right)$$

```mathematica
Hypergeometric2F1[a, b, c, z^(-1)] == Kummer[14][Hypergeometric2F1[a, b, c, z^(-1)]]
```

$$\, _2F_1\left(a,b;c;\frac{1}{z}\right)=(1-z)^{-a-b+c} (-z)^{a+b-c} \, _2F_1\left(c-a,c-b;c;\frac{1}{z}\right)$$

```mathematica
Hypergeometric2F1[b + 1 - c, b, b + 1 - a, z^(-1)] == Kummer[14][Hypergeometric2F1[b + 1 - c, b, b + 1 - a, z^(-1)]]
```

$$\, _2F_1\left(b,b-c+1;-a+b+1;\frac{1}{z}\right)=(1-z)^{-a-b+c} (-z)^{a+b-c} \, _2F_1\left(1-a,c-a;-a+b+1;\frac{1}{z}\right)$$

```mathematica
Hypergeometric2F1[a, b, c, z^(-1)] == Kummer[15][Hypergeometric2F1[a, b, c, z^(-1)]]
```

$$\, _2F_1\left(a,b;c;\frac{1}{z}\right)=(1-z)^{-b} (-z)^b \, _2F_1\left(b,c-a;c;\frac{1}{1-z}\right)$$

```mathematica
Hypergeometric2F1[b + 1 - c, b, b + 1 - a, z^(-1)] == Kummer[15][Hypergeometric2F1[b + 1 - c, b, b + 1 - a, z^(-1)]]
```

$$\, _2F_1\left(b,b-c+1;-a+b+1;\frac{1}{z}\right)=(1-z)^{-b+c-1} (-z)^{b-c+1} \, _2F_1\left(1-a,b-c+1;-a+b+1;\frac{1}{1-z}\right)$$

```mathematica
Hypergeometric2F1[a, b, c, z^(-1)] == Kummer[16][Hypergeometric2F1[a, b, c, z^(-1)]]
```

$$\, _2F_1\left(a,b;c;\frac{1}{z}\right)=(1-z)^{-a} (-z)^a \, _2F_1\left(a,c-b;c;\frac{1}{1-z}\right)$$

```mathematica
Hypergeometric2F1[b + 1 - c, b, b + 1 - a, z^(-1)] == Kummer[16][Hypergeometric2F1[b + 1 - c, b, b + 1 - a, z^(-1)]]
```

$$\, _2F_1\left(b,b-c+1;-a+b+1;\frac{1}{z}\right)=(1-z)^{-b} (-z)^b \, _2F_1\left(b,c-a;-a+b+1;\frac{1}{1-z}\right)$$

```mathematica
Hypergeometric2F1[a + 1 - c, b + 1 - c, 2 - c, z] == Kummer[18][Hypergeometric2F1[a + 1 - c, b + 1 - c, 2 - c, z]]
```

$$\, _2F_1(a-c+1,b-c+1;2-c;z)=(1-z)^{-a-b+c} \, _2F_1(1-a,1-b;2-c;z)$$

```mathematica
Hypergeometric2F1[a, b, c, z] == Kummer[18][Hypergeometric2F1[a, b, c, z]]
```

$$\, _2F_1(a,b;c;z)=(1-z)^{-a-b+c} \, _2F_1(c-a,c-b;c;z)$$

```mathematica
Hypergeometric2F1[a + 1 - c, b + 1 - c, 2 - c, z] == Kummer[19][Hypergeometric2F1[a + 1 - c, b + 1 - c, 2 - c, z]]
```

$$\, _2F_1(a-c+1,b-c+1;2-c;z)=(1-z)^{-a+c-1} \, _2F_1\left(1-b,a-c+1;2-c;\frac{z}{z-1}\right)$$

```mathematica
Hypergeometric2F1[a, b, c, z] == Kummer[19][Hypergeometric2F1[a, b, c, z]]
```

$$\, _2F_1(a,b;c;z)=(1-z)^{-a} \, _2F_1\left(a,c-b;c;\frac{z}{z-1}\right)$$

```mathematica
Hypergeometric2F1[a + 1 - c, b + 1 - c, 2 - c, z] == Kummer[20][Hypergeometric2F1[a + 1 - c, b + 1 - c, 2 - c, z]]
```

$$\, _2F_1(a-c+1,b-c+1;2-c;z)=(1-z)^{-b+c-1} \, _2F_1\left(1-a,b-c+1;2-c;\frac{z}{z-1}\right)$$

```mathematica
Hypergeometric2F1[a, b, c, z] == Kummer[20][Hypergeometric2F1[a, b, c, z]]
```

$$\, _2F_1(a,b;c;z)=(1-z)^{-b} \, _2F_1\left(b,c-a;c;\frac{z}{z-1}\right)$$

```mathematica
Hypergeometric2F1[c - a, c - b, c + 1 - a - b, 1 - z] == Kummer[22][Hypergeometric2F1[c - a, c - b, c + 1 - a - b, 1 - z]]
```

$$\, _2F_1(c-a,c-b;-a-b+c+1;1-z)=z^{1-c} \, _2F_1(1-a,1-b;-a-b+c+1;1-z)$$

```mathematica
Hypergeometric2F1[a, b, c, 1 - z] == Kummer[22][Hypergeometric2F1[a, b, c, 1 - z]]
```

$$\, _2F_1(a,b;c;1-z)=z^{-a-b+c} \, _2F_1(c-a,c-b;c;1-z)$$

```mathematica
Hypergeometric2F1[c - a, c - b, c + 1 - a - b, 1 - z] == Kummer[23][Hypergeometric2F1[c - a, c - b, c + 1 - a - b, 1 - z]]
```

$$\, _2F_1(c-a,c-b;-a-b+c+1;1-z)=(1-z)^{a-c} \, _2F_1\left(1-a,c-a;-a-b+c+1;1-\frac{1}{1-z}\right)$$

```mathematica
Hypergeometric2F1[a, b, c, 1 - z] == Kummer[23][Hypergeometric2F1[a, b, c, 1 - z]]
```

$$\, _2F_1(a,b;c;1-z)=(1-z)^{-a} \, _2F_1\left(a,c-b;c;1-\frac{1}{1-z}\right)$$

```mathematica
Hypergeometric2F1[c - a, c - b, c + 1 - a - b, 1 - z] == Kummer[24][Hypergeometric2F1[c - a, c - b, c + 1 - a - b, 1 - z]]
```

$$\, _2F_1(c-a,c-b;-a-b+c+1;1-z)=z^{b-c} \, _2F_1\left(1-b,c-b;-a-b+c+1;-\frac{1-z}{z}\right)$$

```mathematica
Hypergeometric2F1[a, b, c, 1 - z] == Kummer[24][Hypergeometric2F1[a, b, c, 1 - z]]
```

$$\, _2F_1(a,b;c;1-z)=z^{-b} \, _2F_1\left(b,c-a;c;-\frac{1-z}{z}\right)$$

```mathematica
Hypergeometric2F1[a, b, c, z] == Kummer[94][Hypergeometric2F1[a, b, c, z]]
```

$$\, _2F_1(a,b;c;z)=\frac{\Gamma (c) (1-z)^{-a-b+c} \Gamma (a+b-c) \, _2F_1(c-a,c-b;-a-b+c+1;1-z)}{\Gamma (a) \Gamma (b)}+\frac{\Gamma (c) \Gamma (-a-b+c) \, _2F_1(a,b;a+b-c+1;1-z)}{\Gamma (c-a) \Gamma (c-b)}$$

```mathematica
Hypergeometric2F1[a, b, c, z] == Kummer[95][Hypergeometric2F1[a, b, c, z]]
```

$$\, _2F_1(a,b;c;z)=\frac{(1-z)^{-a} \Gamma (c) \Gamma (b-a) \, _2F_1\left(a,c-b;a-b+1;\frac{1}{1-z}\right)}{\Gamma (b) \Gamma (c-a)}+\frac{(1-z)^{-b} \Gamma (c) \Gamma (a-b) \, _2F_1\left(b,c-a;-a+b+1;\frac{1}{1-z}\right)}{\Gamma (a) \Gamma (c-b)}$$

```mathematica
Hypergeometric2F1[a, b, c, z] == Kummer[96][Hypergeometric2F1[a, b, c, z]]
```

$$\, _2F_1(a,b;c;z)=\frac{(-1)^a z^{-a} \Gamma (c) \Gamma (b-a) \, _2F_1\left(a,a-c+1;a-b+1;\frac{1}{z}\right)}{\Gamma (b) \Gamma (c-a)}+\frac{(-1)^b z^{-b} \Gamma (c) \Gamma (a-b) \, _2F_1\left(b,b-c+1;-a+b+1;\frac{1}{z}\right)}{\Gamma (a) \Gamma (c-b)}$$
