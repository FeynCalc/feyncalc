## Twist2GluonOperator

`Twist2GluonOperator[{p, mu, a}, {nu, b}]` or `Twist2GluonOperator[p, {mu, a}, {nu, b}]` or `Twist2GluonOperator[p, mu,a, nu,b]` yields the 2-gluon operator (`p` is ingoing momentum corresponding to Lorentz index `mu`).

`Twist2GluonOperator[{p,mu,a}, {q,nu,b}, {k,la,c}]` or `Twist2GluonOperator[ p,mu,a , q,nu,b , k,la,c]` gives the 3-gluon operator.

`Twist2GluonOperator[{p,mu,a}, {q,nu,b}, {k,la,c}, {s,si,d}]` or `Twist2GluonOperator[p,mu,a , q,nu,b , k,la,c , s,si,d]` yields the 4-Gluon operator.

The dimension is determined by the option `Dimension`. The setting of the option `Polarization` (unpolarized: `0`; polarized: `1`) determines whether the unpolarized or polarized Feynman rule is returned.

With the setting `Explicit` set to `False` the color-structure and the (`1+(-1)^OPEm`) (for polarized: `(1-(-1)^OPEm)`) is extracted and the color indices are omitted in the arguments of `Twist2GluonOperator`.

### See also

[Overview](Extra/FeynCalc.md), [Twist2QuarkOperator](Twist2QuarkOperator.md).

### Examples

The setting All for Explicit performs the sums.

```mathematica
Twist2GluonOperator[{p, \[Mu], a}, {q, \[Nu], b}, {r, \[Rho], c}, Polarization -> 1, Explicit -> All]
```

$$\left(1-(-1)^m\right) g_s f^{abc} \left(O_{\nu \, \rho \, \mu }^{\text{G3}}(q,r,p)\right)$$
