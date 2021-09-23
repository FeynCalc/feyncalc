## SetDimensions

`SetDimensions` is an option for `ScalarProduct`, `CartesianScalarProduct` and various `FCLoopBasis*` functions.

For scalar products it specifies the dimensions for which the scalar products will be set when `ScalarProduct` or `CartesianScalarProduct`  are used with the equality sign, e.g. in `ScalarProduct[a, b] = m^2`. By default, the scalar products are set for 4 and D dimensions. By changing this option the user can add other dimensions or remove the existing ones.

In case of the `FCLoopBasis*` functions this option specifies the dimensions of the loop and external momenta to be taken into account when extracting the propagator basis.

### See also

[Overview](Extra/FeynCalc.md), [ScalarProduct](ScalarProduct.md), [CartesianScalarProduct](CartesianScalarProduct.md).

### Examples
