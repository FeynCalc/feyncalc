$LoadPhi=False; 
$LoadTARCER=False;

<<HighEnergyPhysics`FeynCalc`; 
FI;

$VeryVerbose = 3;
tdec53 = Tdec[{{q, mu}, {q,nu}, {q,rho}}, {p1,p2, p3 , p4}, Factoring->Factor2, 
Collect -> Collect2];
tdec53>>"tdec53f.s"
