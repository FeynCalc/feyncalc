##  $AL 

$AL is the head for dummy indices which may be introduced by Amputate and Uncontract. By default it is unset, but may be set to anything..

###  See also 

Amputate, Uncontract.

###  Examples 

```mathematica
Uncontract[ScalarProduct[p, q], q, Pair -> All] 
 
$AL = \[Mu];
Uncontract[ScalarProduct[p, q], q, Pair -> All] 
 
$AL =.;

```

$$\overline{p}^{\text{$\$$AL}(\text{$\$$24})} \overline{q}^{\text{$\$$AL}(\text{$\$$24})}$$

$$\overline{p}^{\mu (\text{$\$$25})} \overline{q}^{\mu (\text{$\$$25})}$$

See also:  Amputate, Uncontract.