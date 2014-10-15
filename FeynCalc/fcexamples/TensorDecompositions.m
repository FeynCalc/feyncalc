(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: TensorDecompositions                                             *)

(*
   This software is covered by the GNU Lesser General Public License 3.
   Copyright (C) 1990-2014 Rolf Mertig
   Copyright (C) 1997-2014 Frederik Orellana
   Copyright (C) 2014 Vladyslav Shtabovenko
*)

(* :Summary:  Benchmark for doing tensor decompositions of different
              loop integrals                                                *)

(* ------------------------------------------------------------------------ *)


<<HighEnergyPhysics`FeynCalc`;


Print @ AbsoluteTiming[
Print ["Time needed to compute integral transformation for any Int(d^D q f(p1,p2,q) q^mu q^nu q^rho):"];
Tdec[{{q, mu}, {q, nu}, {q, rho}}, {p1, p2}];
]


Print @ AbsoluteTiming[
Print ["Time needed to compute integral transformation for any Int(d^D q f(p1,p2,q) q^mu q^nu q^rho q^1_si):"];
Tdec[{{q, mu}, {q, nu}, {q,rho}, {q,si}}, {p1, p2}];
]


Print @ AbsoluteTiming[
Print ["Time needed to compute integral transformation for any Int(d^D q f(p1,p2,p3,p4,q) q^mu ):"];
Tdec[{{q, mu}}, {p1, p2, p3, p4}];
]


Print @ AbsoluteTiming[
Print ["Time needed to compute integral transformation for any Int(d^D q f(p1,p2,p3,p4,q) q^mu q^nu):"];
Tdec[{{q, mu}, {q, nu}}, {p1, p2, p3, p4}];
]


Print @ AbsoluteTiming[
Print ["Time needed to compute integral transformation for any Int(d^D q f(p1,p2,p3,p4,q) q^mu q^nu q^rho):"];
Tdec[{{q, mu}, {q, nu}, {q, rho}}, {p1, p2, p3, p4}];
]


Print @ AbsoluteTiming[
Print ["Time needed to compute integral transformation for any Int(d^D q f(p1,p2,p3,p4,q) q^mu q^nu q^rho q^si q^tau ):"];
Tdec[{{q, mu}, {q, nu}, {q, rho}, {q, si}, {q, tau}}, {p1, p2, p3, p4}];
]
