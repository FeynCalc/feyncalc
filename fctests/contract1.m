<<HighEnergyPhysics`FeynCalc`

(* this contract example should run in about 3.7 seconds on a 1.1 Ghz Machine*)

(*
$VeryVerbose = 2;
*)

Timing[test=
Contract[
tin=
(\[Xi]*FAD[p - q1]*(FVD[p, li10] - FVD[q1, li10])*
   (FVD[p, li9] - FVD[q1, li9]) - MTD[li10, li9])*
 (\[Xi]*FAD[q1]*FVD[q1, li3]*FVD[q1, li4] - MTD[li3, li4])*
 (\[Xi]*FAD[q2]*FVD[q2, li5]*FVD[q2, li6] - MTD[li5, li6])*
 ((FVD[q1, li7] + FVD[q2, li7])*MTD[li3, li5] + 
  (-2*FVD[q1, li5] + FVD[q2, li5])*MTD[li3, li7] + 
  (FVD[q1, li3] - 2*FVD[q2, li3])*MTD[li5, li7])*
 (\[Xi]*FAD[q1 - q2]*(FVD[q1, li7] - FVD[q2, li7])*
   (FVD[q1, li8] - FVD[q2, li8]) - MTD[li7, li8])*
 (MTD[li10, li8]*MTD[li6, nu] - MTD[li10, li6]*MTD[li8, nu])*
 ((FVD[p, mu] - 2*FVD[q1, mu])*MTD[li4, li9] + 
  (FVD[p, li9] + FVD[q1, li9])*MTD[li4, mu] + 
  (-2*FVD[p, li4] + FVD[q1, li4])*MTD[li9, mu])
];]
