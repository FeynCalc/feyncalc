(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Twist4GluonOperator*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`Twist4GluonOperator`",
             "HighEnergyPhysics`FeynCalc`"];

Twist4GluonOperator::usage= 
"Twist4GluonOperator[{oa, ob, oc, od},
                     {p1,la1,a1}, {p2,la2,a2}, {p3,la3,a3}, {p4,la4,a4}].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   SetAttributes[Twist4GluonOperator, ReadProtected];

MakeContext[FV, FeynCalcInternal, MT, OPEDelta, SO, SP, SUND];

Twist4GluonOperator[{oa_, ob_, oc_, od_}, {p1_, LA1_, a_}, 
                     {p2_,LA2_,b_}, {p3_,LA3_,c_}, {p4_,LA4_,d_}
                   ] := Block[{DL}, DL = OPEDelta;
(FV[DL, LA2]*FV[p2, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA2]*SO[p2] - 
     MT[LA1, LA2]*SO[p1]*SO[p2] - FV[DL, LA1]*FV[DL, LA2]*SP[p1, p2])*
   (FV[DL, LA4]*FV[p4, LA3]*SO[p3] + FV[DL, LA3]*FV[p3, LA4]*SO[p4] - 
     MT[LA3, LA4]*SO[p3]*SO[p4] - FV[DL, LA3]*FV[DL, LA4]*SP[p3, p4])*
   SUND[a, od]*SUND[b, oc]*SUND[c, ob]*SUND[d, oa] + 
  (FV[DL, LA2]*FV[p2, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA2]*SO[p2] - 
     MT[LA1, LA2]*SO[p1]*SO[p2] - FV[DL, LA1]*FV[DL, LA2]*SP[p1, p2])*
   (FV[DL, LA4]*FV[p4, LA3]*SO[p3] + FV[DL, LA3]*FV[p3, LA4]*SO[p4] - 
     MT[LA3, LA4]*SO[p3]*SO[p4] - FV[DL, LA3]*FV[DL, LA4]*SP[p3, p4])*
   SUND[a, oc]*SUND[b, od]*SUND[c, ob]*SUND[d, oa] + 
  (FV[DL, LA3]*FV[p3, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA3]*SO[p3] - 
     MT[LA1, LA3]*SO[p1]*SO[p3] - FV[DL, LA1]*FV[DL, LA3]*SP[p1, p3])*
   (FV[DL, LA4]*FV[p4, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA4]*SO[p4] - 
     MT[LA2, LA4]*SO[p2]*SO[p4] - FV[DL, LA2]*FV[DL, LA4]*SP[p2, p4])*
   SUND[a, od]*SUND[b, ob]*SUND[c, oc]*SUND[d, oa] + 
  (FV[DL, LA3]*FV[p3, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA3]*SO[p3] - 
     MT[LA1, LA3]*SO[p1]*SO[p3] - FV[DL, LA1]*FV[DL, LA3]*SP[p1, p3])*
   (FV[DL, LA4]*FV[p4, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA4]*SO[p4] - 
     MT[LA2, LA4]*SO[p2]*SO[p4] - FV[DL, LA2]*FV[DL, LA4]*SP[p2, p4])*
   SUND[a, ob]*SUND[b, od]*SUND[c, oc]*SUND[d, oa] + 
  (FV[DL, LA4]*FV[p4, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA4]*SO[p4] - 
     MT[LA1, LA4]*SO[p1]*SO[p4] - FV[DL, LA1]*FV[DL, LA4]*SP[p1, p4])*
   (FV[DL, LA3]*FV[p3, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA3]*SO[p3] - 
     MT[LA2, LA3]*SO[p2]*SO[p3] - FV[DL, LA2]*FV[DL, LA3]*SP[p2, p3])*
   SUND[a, oc]*SUND[b, ob]*SUND[c, od]*SUND[d, oa] + 
  (FV[DL, LA4]*FV[p4, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA4]*SO[p4] - 
     MT[LA1, LA4]*SO[p1]*SO[p4] - FV[DL, LA1]*FV[DL, LA4]*SP[p1, p4])*
   (FV[DL, LA3]*FV[p3, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA3]*SO[p3] - 
     MT[LA2, LA3]*SO[p2]*SO[p3] - FV[DL, LA2]*FV[DL, LA3]*SP[p2, p3])*
   SUND[a, ob]*SUND[b, oc]*SUND[c, od]*SUND[d, oa] + 
  (FV[DL, LA2]*FV[p2, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA2]*SO[p2] - 
     MT[LA1, LA2]*SO[p1]*SO[p2] - FV[DL, LA1]*FV[DL, LA2]*SP[p1, p2])*
   (FV[DL, LA4]*FV[p4, LA3]*SO[p3] + FV[DL, LA3]*FV[p3, LA4]*SO[p4] - 
     MT[LA3, LA4]*SO[p3]*SO[p4] - FV[DL, LA3]*FV[DL, LA4]*SP[p3, p4])*
   SUND[a, od]*SUND[b, oc]*SUND[c, oa]*SUND[d, ob] + 
  (FV[DL, LA2]*FV[p2, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA2]*SO[p2] - 
     MT[LA1, LA2]*SO[p1]*SO[p2] - FV[DL, LA1]*FV[DL, LA2]*SP[p1, p2])*
   (FV[DL, LA4]*FV[p4, LA3]*SO[p3] + FV[DL, LA3]*FV[p3, LA4]*SO[p4] - 
     MT[LA3, LA4]*SO[p3]*SO[p4] - FV[DL, LA3]*FV[DL, LA4]*SP[p3, p4])*
   SUND[a, oc]*SUND[b, od]*SUND[c, oa]*SUND[d, ob] + 
  (FV[DL, LA4]*FV[p4, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA4]*SO[p4] - 
     MT[LA1, LA4]*SO[p1]*SO[p4] - FV[DL, LA1]*FV[DL, LA4]*SP[p1, p4])*
   (FV[DL, LA3]*FV[p3, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA3]*SO[p3] - 
     MT[LA2, LA3]*SO[p2]*SO[p3] - FV[DL, LA2]*FV[DL, LA3]*SP[p2, p3])*
   SUND[a, od]*SUND[b, oa]*SUND[c, oc]*SUND[d, ob] + 
  (FV[DL, LA4]*FV[p4, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA4]*SO[p4] - 
     MT[LA1, LA4]*SO[p1]*SO[p4] - FV[DL, LA1]*FV[DL, LA4]*SP[p1, p4])*
   (FV[DL, LA3]*FV[p3, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA3]*SO[p3] - 
     MT[LA2, LA3]*SO[p2]*SO[p3] - FV[DL, LA2]*FV[DL, LA3]*SP[p2, p3])*
   SUND[a, oa]*SUND[b, od]*SUND[c, oc]*SUND[d, ob] + 
  (FV[DL, LA3]*FV[p3, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA3]*SO[p3] - 
     MT[LA1, LA3]*SO[p1]*SO[p3] - FV[DL, LA1]*FV[DL, LA3]*SP[p1, p3])*
   (FV[DL, LA4]*FV[p4, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA4]*SO[p4] - 
     MT[LA2, LA4]*SO[p2]*SO[p4] - FV[DL, LA2]*FV[DL, LA4]*SP[p2, p4])*
   SUND[a, oc]*SUND[b, oa]*SUND[c, od]*SUND[d, ob] + 
  (FV[DL, LA3]*FV[p3, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA3]*SO[p3] - 
     MT[LA1, LA3]*SO[p1]*SO[p3] - FV[DL, LA1]*FV[DL, LA3]*SP[p1, p3])*
   (FV[DL, LA4]*FV[p4, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA4]*SO[p4] - 
     MT[LA2, LA4]*SO[p2]*SO[p4] - FV[DL, LA2]*FV[DL, LA4]*SP[p2, p4])*
   SUND[a, oa]*SUND[b, oc]*SUND[c, od]*SUND[d, ob] + 
  (FV[DL, LA3]*FV[p3, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA3]*SO[p3] - 
     MT[LA1, LA3]*SO[p1]*SO[p3] - FV[DL, LA1]*FV[DL, LA3]*SP[p1, p3])*
   (FV[DL, LA4]*FV[p4, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA4]*SO[p4] - 
     MT[LA2, LA4]*SO[p2]*SO[p4] - FV[DL, LA2]*FV[DL, LA4]*SP[p2, p4])*
   SUND[a, od]*SUND[b, ob]*SUND[c, oa]*SUND[d, oc] + 
  (FV[DL, LA3]*FV[p3, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA3]*SO[p3] - 
     MT[LA1, LA3]*SO[p1]*SO[p3] - FV[DL, LA1]*FV[DL, LA3]*SP[p1, p3])*
   (FV[DL, LA4]*FV[p4, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA4]*SO[p4] - 
     MT[LA2, LA4]*SO[p2]*SO[p4] - FV[DL, LA2]*FV[DL, LA4]*SP[p2, p4])*
   SUND[a, ob]*SUND[b, od]*SUND[c, oa]*SUND[d, oc] + 
  (FV[DL, LA4]*FV[p4, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA4]*SO[p4] - 
     MT[LA1, LA4]*SO[p1]*SO[p4] - FV[DL, LA1]*FV[DL, LA4]*SP[p1, p4])*
   (FV[DL, LA3]*FV[p3, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA3]*SO[p3] - 
     MT[LA2, LA3]*SO[p2]*SO[p3] - FV[DL, LA2]*FV[DL, LA3]*SP[p2, p3])*
   SUND[a, od]*SUND[b, oa]*SUND[c, ob]*SUND[d, oc] + 
  (FV[DL, LA4]*FV[p4, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA4]*SO[p4] - 
     MT[LA1, LA4]*SO[p1]*SO[p4] - FV[DL, LA1]*FV[DL, LA4]*SP[p1, p4])*
   (FV[DL, LA3]*FV[p3, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA3]*SO[p3] - 
     MT[LA2, LA3]*SO[p2]*SO[p3] - FV[DL, LA2]*FV[DL, LA3]*SP[p2, p3])*
   SUND[a, oa]*SUND[b, od]*SUND[c, ob]*SUND[d, oc] + 
  (FV[DL, LA2]*FV[p2, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA2]*SO[p2] - 
     MT[LA1, LA2]*SO[p1]*SO[p2] - FV[DL, LA1]*FV[DL, LA2]*SP[p1, p2])*
   (FV[DL, LA4]*FV[p4, LA3]*SO[p3] + FV[DL, LA3]*FV[p3, LA4]*SO[p4] - 
     MT[LA3, LA4]*SO[p3]*SO[p4] - FV[DL, LA3]*FV[DL, LA4]*SP[p3, p4])*
   SUND[a, ob]*SUND[b, oa]*SUND[c, od]*SUND[d, oc] + 
  (FV[DL, LA2]*FV[p2, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA2]*SO[p2] - 
     MT[LA1, LA2]*SO[p1]*SO[p2] - FV[DL, LA1]*FV[DL, LA2]*SP[p1, p2])*
   (FV[DL, LA4]*FV[p4, LA3]*SO[p3] + FV[DL, LA3]*FV[p3, LA4]*SO[p4] - 
     MT[LA3, LA4]*SO[p3]*SO[p4] - FV[DL, LA3]*FV[DL, LA4]*SP[p3, p4])*
   SUND[a, oa]*SUND[b, ob]*SUND[c, od]*SUND[d, oc] + 
  (FV[DL, LA4]*FV[p4, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA4]*SO[p4] - 
     MT[LA1, LA4]*SO[p1]*SO[p4] - FV[DL, LA1]*FV[DL, LA4]*SP[p1, p4])*
   (FV[DL, LA3]*FV[p3, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA3]*SO[p3] - 
     MT[LA2, LA3]*SO[p2]*SO[p3] - FV[DL, LA2]*FV[DL, LA3]*SP[p2, p3])*
   SUND[a, oc]*SUND[b, ob]*SUND[c, oa]*SUND[d, od] + 
  (FV[DL, LA4]*FV[p4, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA4]*SO[p4] - 
     MT[LA1, LA4]*SO[p1]*SO[p4] - FV[DL, LA1]*FV[DL, LA4]*SP[p1, p4])*
   (FV[DL, LA3]*FV[p3, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA3]*SO[p3] - 
     MT[LA2, LA3]*SO[p2]*SO[p3] - FV[DL, LA2]*FV[DL, LA3]*SP[p2, p3])*
   SUND[a, ob]*SUND[b, oc]*SUND[c, oa]*SUND[d, od] + 
  (FV[DL, LA3]*FV[p3, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA3]*SO[p3] - 
     MT[LA1, LA3]*SO[p1]*SO[p3] - FV[DL, LA1]*FV[DL, LA3]*SP[p1, p3])*
   (FV[DL, LA4]*FV[p4, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA4]*SO[p4] - 
     MT[LA2, LA4]*SO[p2]*SO[p4] - FV[DL, LA2]*FV[DL, LA4]*SP[p2, p4])*
   SUND[a, oc]*SUND[b, oa]*SUND[c, ob]*SUND[d, od] + 
  (FV[DL, LA3]*FV[p3, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA3]*SO[p3] - 
     MT[LA1, LA3]*SO[p1]*SO[p3] - FV[DL, LA1]*FV[DL, LA3]*SP[p1, p3])*
   (FV[DL, LA4]*FV[p4, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA4]*SO[p4] - 
     MT[LA2, LA4]*SO[p2]*SO[p4] - FV[DL, LA2]*FV[DL, LA4]*SP[p2, p4])*
   SUND[a, oa]*SUND[b, oc]*SUND[c, ob]*SUND[d, od] + 
  (FV[DL, LA2]*FV[p2, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA2]*SO[p2] - 
     MT[LA1, LA2]*SO[p1]*SO[p2] - FV[DL, LA1]*FV[DL, LA2]*SP[p1, p2])*
   (FV[DL, LA4]*FV[p4, LA3]*SO[p3] + FV[DL, LA3]*FV[p3, LA4]*SO[p4] - 
     MT[LA3, LA4]*SO[p3]*SO[p4] - FV[DL, LA3]*FV[DL, LA4]*SP[p3, p4])*
   SUND[a, ob]*SUND[b, oa]*SUND[c, oc]*SUND[d, od] + 
  (FV[DL, LA2]*FV[p2, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA2]*SO[p2] - 
     MT[LA1, LA2]*SO[p1]*SO[p2] - FV[DL, LA1]*FV[DL, LA2]*SP[p1, p2])*
   (FV[DL, LA4]*FV[p4, LA3]*SO[p3] + FV[DL, LA3]*FV[p3, LA4]*SO[p4] - 
     MT[LA3, LA4]*SO[p3]*SO[p4] - FV[DL, LA3]*FV[DL, LA4]*SP[p3, p4])*
   SUND[a, oa]*SUND[b, ob]*SUND[c, oc]*SUND[d, od]
]//FeynCalcInternal;

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Twist4GluonOperator | \n "]];
Null
