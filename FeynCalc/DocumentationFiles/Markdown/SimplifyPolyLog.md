##  SimplifyPolyLog 

SimplifyPolyLog[y] performs several simplifications assuming that the variables occuring in the Log and PolyLog functions are between 0 and 1..

###  See also 

Nielsen.

###  Examples 

```mathematica
sip[y_] := y == SimplifyPolyLog[y]
sip[PolyLog[2, 1/x]] 
 
sip[PolyLog[2, x]] 
 
sip[PolyLog[2, 1 - x^2]] 
 
sip[PolyLog[2, x^2]] 
 
sip[PolyLog[2, -x/(1 - x)]] 
 
sip[PolyLog[2, x/(x - 1)]] 
 
sip[Nielsen[1, 2, -x/(1 - x)]] 
 
sip[PolyLog[3, -1/x]] 
 
sip[PolyLog[3, 1 - x]] 
 
sip[PolyLog[3, x^2]] 
 
sip[PolyLog[3, -x/(1 - x)]] 
 
sip[PolyLog[3, 1 - 1/x]] 
 
sip[PolyLog[4, -x/(1 - x)]] 
 
sip[Log[a + b/c]] 
 
sip[Log[1/x]] 
 
sip[ArcTanh[x]] 
 
sip[ArcSinh[x]] 
 
sip[ArcCosh[x]] 
 
Clear[sip]
```

$$\text{Li}_2\left(\frac{1}{x}\right)=\zeta (2)+\text{Li}_2(1-x)-\frac{1}{2} \log ^2(x)+\log (1-x) \log (x)+i \pi  \log (x)$$

$$\text{Li}_2(x)=\zeta (2)-\text{Li}_2(1-x)-\log (1-x) \log (x)$$

$$\text{Li}_2\left(1-x^2\right)=-\zeta (2)+2 \text{Li}_2(1-x)-2 \text{Li}_2(-x)-2 \log (x) \log (x+1)$$

$$\text{Li}_2\left(x^2\right)=2 \zeta (2)-2 \text{Li}_2(1-x)+2 \text{Li}_2(-x)-2 \log (1-x) \log (x)$$

$$\text{Li}_2\left(-\frac{x}{1-x}\right)=-\zeta (2)+\text{Li}_2(1-x)-\frac{1}{2} \log ^2(1-x)+\log (x) \log (1-x)$$

$$\text{Li}_2\left(\frac{x}{x-1}\right)=-\zeta (2)+\text{Li}_2(1-x)-\frac{1}{2} \log ^2(1-x)+\log (x) \log (1-x)$$

$$S_{12}\left(-\frac{x}{1-x}\right)=S_{12}(x)-\frac{1}{6} \log ^3(1-x)$$

$$\text{Li}_3\left(-\frac{1}{x}\right)=\text{Li}_3(-x)+\zeta (2) \log (x)+\frac{\log ^3(x)}{6}$$

$$\text{True}$$

$$\text{Li}_3\left(x^2\right)=4 \text{Li}_3(-x)-4 \text{Li}_2(1-x) \log (x)-4 S_{12}(1-x)+4 \zeta (2) \log (x)-2 \log (1-x) \log ^2(x)+4 \zeta (3)$$

$$\text{Li}_3\left(-\frac{x}{1-x}\right)=-\text{Li}_3(1-x)+\text{Li}_2(1-x) \log (x)+S_{12}(1-x)+\zeta (2) \log (1-x)-\zeta (2) \log (x)+\frac{1}{6} \log ^3(1-x)-\frac{1}{2} \log (x) \log ^2(1-x)+\frac{1}{2} \log ^2(x) \log (1-x)$$

$$\text{Li}_3\left(1-\frac{1}{x}\right)=\text{Li}_3\left(-\frac{1-x}{x}\right)$$

$$\text{Li}_4\left(-\frac{x}{1-x}\right)=-\text{Li}_4(x)+\frac{1}{2} \text{Li}_2(1-x) \log ^2(1-x)-\text{Li}_2(1-x) \log (x) \log (1-x)-S_{13}(x)+S_{22}(x)-S_{12}(1-x) \log (1-x)-S_{12}(x) \log (1-x)-\frac{1}{2} \zeta (2) \log ^2(1-x)+\zeta (2) \log (x) \log (1-x)+\zeta (3) \log (1-x)-\frac{1}{24} \log ^4(1-x)+\frac{1}{2} \log (x) \log ^3(1-x)-\frac{1}{2} \log ^2(x) \log ^2(1-x)$$

$$\log \left(a+\frac{b}{c}\right)=\log \left(\frac{a c+b}{c}\right)$$

$$\log \left(\frac{1}{x}\right)=-\log (x)$$

$$\tanh ^{-1}(x)=\frac{1}{2} \log \left(-\frac{x+1}{1-x}\right)$$

$$\sinh ^{-1}(x)=\log \left(\sqrt{x^2+1}+x\right)$$

$$\cosh ^{-1}(x)=\log \left(\sqrt{x^2-1}+x\right)$$