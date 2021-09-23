## FCProgressBar

`FCProgressBar[text, i, total]`  is a simple auxiliary function that can be used to display the progress of a certain evaluation, e.g. mapping a list of integrals to some function. Here i is the number of the current step while total denotes the overall number of steps.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

A simple usage example

```mathematica
Table[FCProgressBar["Calculating integral ", i, 10], {i, 1, 10}];
```

Calculating integral 1 / 10
Calculating integral 2 / 10
Calculating integral 3 / 10
Calculating integral 4 / 10
Calculating integral 5 / 10
Calculating integral 6 / 10
Calculating integral 7 / 10
Calculating integral 8 / 10
Calculating integral 9 / 10
Calculating integral 10 / 10
