##  Collect2 

Collect2[expr, x] collects together terms which are not free of any occurrence of x. Collect2[expr, {x1, x2, ...}] (or also Collect2[expr, x1, x2, ...]) collects together terms which are not free of any occurrence of x1, x2, .... The coefficients are put over a common denominator. If expr is expanded before collecting depends on the option Factoring, which may be set to Factor, Factor2, or any other function, which is applied to the coefficients. If expr is already expanded with respect to x (x1,x2, ...), the option Expanding can be set to False..

###  See also 

Isolate.

###  Examples 

```mathematica
Collect2[t1 = a + r a + k^2 f[a] - k f[a] + x/2 - y/w, a] 
 
Collect2[t1, a, Factoring -> False] 
 
Collect2[t1, a, Factoring -> Factor] 
 
Collect2[t1, a, Factoring -> Simplify] 
 
Collect2[2 a (b - a) (h - 1) - b^2 (e a - c) + b^2, {a, b}] 
 
Collect2[Expand[(a - b - c - d)^5], a, IsolateNames -> KK] 
 
FRH[%] 
 
Collect2[Expand[(a - b - c - d)^5], a, Head -> h] 
 
Clear[t1, l]
Collect2[Expand[(a - b - c)^3], a, Factoring -> fun] 
 
% /. fun -> FactorTerms
```

$$(k-1) k f(a)+a (r+1)+\frac{w x-2 y}{2 w}$$

$$\left(k^2-k\right) f(a)+a (r+1)-\frac{y}{w}+\frac{x}{2}$$

$$(k-1) k f(a)+a (r+1)+\frac{w x-2 y}{2 w}$$

$$(k-1) k f(a)+a (r+1)-\frac{y}{w}+\frac{x}{2}$$

$$-2 a^2 (h-1)-a b^2 e+2 a b (h-1)+b^2 (c+1)$$

$$-5 a^4 \text{KK}(24)+10 a^3 \text{KK}(25)-10 a^2 \text{KK}(27)+a^5+5 a \text{KK}(26)-\text{KK}(28)$$

$$-5 a^4 (b+c+d)+10 a^3 (b+c+d)^2-10 a^2 (b+c+d)^3+a^5+5 a (b+c+d)^4-(b+c+d)^5$$

$$-10 h\left(a^2\right) (b+c+d)^3+10 h\left(a^3\right) (b+c+d)^2-5 h\left(a^4\right) (b+c+d)+h\left(a^5\right)+5 h(a) (b+c+d)^4-(b+c+d)^5$$

$$a^2 \text{fun}(-3 b-3 c)+a^3 \text{fun}(1)+a \text{fun}\left(3 b^2+6 b c+3 c^2\right)+\text{fun}\left(-3 b^2 c-b^3-3 b c^2-c^3\right)$$

$$-3 a^2 (b+c)+a^3+3 a \left(b^2+2 b c+c^2\right)-3 b^2 c-b^3-3 b c^2-c^3$$

The options IsolateFast allows to save some time when Isolating prefactors, provided that no factoring is involved.

```mathematica
exp = Sum[h[i], {i, 1, 200000}]*a + Sum[g[i], {i, 1, 200000}]*b + Sum[j[i], {i, 1, 200000}]*c;
AbsoluteTiming[Collect2[exp, {a, b, c}, Factoring -> False, IsolateNames -> KK, Expanding -> False]] 
 
AbsoluteTiming[Collect2[exp, {a, b, c}, Factoring -> False, IsolateNames -> KK, IsolateFast -> True, Expanding -> False]] 
 
ClearAll[exp]
```

$$\{5.48238,a \text{KK}(33)+b \text{KK}(34)+c \text{KK}(32)\}$$

$$\{6.0993,a \text{KK}(33)+b \text{KK}(34)+c \text{KK}(32)\}$$