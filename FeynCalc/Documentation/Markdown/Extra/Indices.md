# Upper and lower indices

Here we list a set of rules that allows to reconstruct the positions of indices 
(upstairs or downstairs) appearing in FeynCalc expressions

- Every expression must satisfy Einsteins's summation convention, both for Lorentz
and Cartesian indices. Single terms containing more than two identical Lorentz or
Cartesian indices are illegal and will lead to inconsistent results.

- In a contraction of two Lorentz indices it is understood that one of them is upstairs
and the other is downstairs.

- In a contraction of two Cartesian indices, both indices are understood to be upper
indices.

- A free Lorentz or Cartesian index is always understood to be an upper index
