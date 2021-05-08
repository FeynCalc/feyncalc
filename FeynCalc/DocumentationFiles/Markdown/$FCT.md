##  $FCT 

$FCT Setting the global variable $FCT to True changes the default typesetting rules for the noncommutative multiplication operator Dot[]..

###  Examples 

```mathematica
$FCT = True;
GA[\[Mu]] . GA[\[Nu]] 
 
$FCT = False;
GA[\[Mu]] . GA[\[Nu]]
```

$$\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }$$

$$\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }$$