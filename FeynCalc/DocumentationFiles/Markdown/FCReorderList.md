## FCReorderList

`FCReorderList[li, ord]` reorders the list `li` according to the given ordering `ord`.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
myList = Table[mat[i], {i, 1, 23}]
```

$$\{\text{mat}(1),\text{mat}(2),\text{mat}(3),\text{mat}(4),\text{mat}(5),\text{mat}(6),\text{mat}(7),\text{mat}(8),\text{mat}(9),\text{mat}(10),\text{mat}(11),\text{mat}(12),\text{mat}(13),\text{mat}(14),\text{mat}(15),\text{mat}(16),\text{mat}(17),\text{mat}(18),\text{mat}(19),\text{mat}(20),\text{mat}(21),\text{mat}(22),\text{mat}(23)\}$$

```mathematica
FCReorderList[myList, {{1, 10}, 23, {11, 20}, 22, 21}]
```

$$\{\text{mat}(1),\text{mat}(2),\text{mat}(3),\text{mat}(4),\text{mat}(5),\text{mat}(6),\text{mat}(7),\text{mat}(8),\text{mat}(9),\text{mat}(10),\text{mat}(23),\text{mat}(11),\text{mat}(12),\text{mat}(13),\text{mat}(14),\text{mat}(15),\text{mat}(16),\text{mat}(17),\text{mat}(18),\text{mat}(19),\text{mat}(20),\text{mat}(22),\text{mat}(21)\}$$
