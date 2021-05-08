##  FRH 

FRH[exp_] := FixedPoint[ReleaseHold, exp], i.e., FRH removes all HoldForm and Hold in exp..

###  See also 

Isolate.

###  Examples 

```mathematica
Hold[1 - 1 - Hold[2 - 2]] 
 
FRH[%] 
 
Isolate[Solve[x^3 - x - 1 == 0], x, IsolateNames -> KK] 
 
FRH[%]
```

$$\text{Hold}[-\text{Hold}[2-2]+1-1]$$

$$0$$

$$![07bmgrh6uga0o](img/07bmgrh6uga0o.png)$$

$$![06vqmku4xqhhs](img/06vqmku4xqhhs.png)$$

$$![0aul6qek0set7](img/0aul6qek0set7.png)$$

$$![17ci1rvhek220](img/17ci1rvhek220.png)$$

$$\left\{\left\{x\to \text{Root}\left[\text{$\#$1}^3-\text{$\#$1}-1\&,1,0\right]\right\},\left\{x\to \text{Root}\left[\text{$\#$1}^3-\text{$\#$1}-1\&,2,0\right]\right\},\left\{x\to \text{Root}\left[\text{$\#$1}^3-\text{$\#$1}-1\&,3,0\right]\right\}\right\}$$

$$\left\{\left\{x\to \text{Root}\left[\text{$\#$1}^3-\text{$\#$1}-1\&,1,0\right]\right\},\left\{x\to \text{Root}\left[\text{$\#$1}^3-\text{$\#$1}-1\&,2,0\right]\right\},\left\{x\to \text{Root}\left[\text{$\#$1}^3-\text{$\#$1}-1\&,3,0\right]\right\}\right\}$$