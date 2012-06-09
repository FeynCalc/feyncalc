<<HighEnergyPhysics`FeynCalc`;


(*
Factor2 = Factor;
*)


Print @ AbsoluteTiming[
$VeryVerbose = 1;

t42 = Tdec[{{q1, mu}, {q1, nu}, {q1,rho}, {q1,si}}, {p1, p2}];

t42 >> t42.s;
]
