##  FCProgressBar 

FCProgressBar[text, i, total]  is a simple auxiliary function that can be used to display the progress of a certain evaluation, e.g. mapping a list of integrals to some function. Here i is the number of the current step while total denotes the overall number of steps..

###  See also 

A simple usage example

###  Examples 

```mathematica
Table[FCProgressBar["Calculating integral ", i, 10], {i, 1, 10}];
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

$$\frac{\text{Calculating} \text{integral}}{10}$$

$$\frac{\text{Calculating} \text{integral}}{5}$$

$$\frac{3 \text{Calculating} \text{integral}}{10}$$

$$\frac{2 \text{Calculating} \text{integral}}{5}$$

$$\frac{\text{Calculating} \text{integral}}{2}$$

$$\frac{3 \text{Calculating} \text{integral}}{5}$$

$$\frac{7 \text{Calculating} \text{integral}}{10}$$

$$\frac{4 \text{Calculating} \text{integral}}{5}$$

$$\frac{9 \text{Calculating} \text{integral}}{10}$$

$$\text{Calculating} \text{integral}$$