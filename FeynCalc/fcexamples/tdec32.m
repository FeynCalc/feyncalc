<<HighEnergyPhysics`FeynCalc`;


(*
Factor2 = Factor;
*)


Print @ AbsoluteTiming[
$VeryVerbose = 3;

t43 = Tdec[{{q1, mu}, {q1, nu}, {q1,rho}}, {p1, p2}];

t32 >> t32.s;
]
