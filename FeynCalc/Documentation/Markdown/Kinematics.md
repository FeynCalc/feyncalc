## Kinematics

### See also

[Overview](Extra/FeynCalc.md).

### Manipulations of scalar products

FeynCalc allows you to specify the values of scalar products before doing the calculation.

```mathematica
SP[p, q] = s;
```

```mathematica
SP[p, q]
```

$$s$$

To clear the previously set values, use

```mathematica
FCClearScalarProducts[]
```

```mathematica
SP[p, q]
```

$$\overline{p}\cdot \overline{q}$$

A good habit is to always apply `FCClearScalarProducts[]` before setting the values, like in

```mathematica
FCClearScalarProducts[];
SP[p1, p1] = m1^2;
SP[p2, p2] = m2^2;
```

Setting up the kinematics in advance improves performance of FeynCalc and leads to more compact results. The results with the fully arbitrary kinematics are the most complicated and the longest ones.