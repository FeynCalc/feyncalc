(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SymbolicSymbolicSum3 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`SymbolicSum3`",
             "HighEnergyPhysics`FeynCalc`"];

"SymbolicSymbolicSum3 is similar to SymbolicSum (Algegra`SymbolicSum`SymbolicSum was a function to do symbolic summation. It was obsolete from version 3 - all functionality is now autoloaded by Sum), but extended to several double sums.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[FreeQ2,OPEi,OPEj,OPEm];


SymbolicSum3[a_^OPEj b_^(OPEi - OPEj) / (c_^OPEi),       
    {OPEj, 0, OPEi}, {OPEi, 0, -4 + OPEm}
   ] := c^(4-OPEm) a^(OPEm-2)/((a-b) (a-c)) + 
        c^(4-OPEm) b^(OPEm-2)/((b-a) (b-c)) +
        c^2 / ((c-a) (c-b));

(* R.Hamberg, (3A.18) *)
SymbolicSum3[a_^OPEj b_^(any_. + OPEi - OPEj) c_^(opem4_ -OPEi),       
    {OPEi, 0, opem4_}, {OPEj, 0, OPEi} 
   ] :=b^(any)*(
        (a^(opem4+2)/((a-b) (a-c)) + b^(opem4+2)/((b-a) (b-c)) +
         c^(opem4+2)/((c-a) (c-b)) )
               ) /; FreeQ2[{any,opem4},{OPEi,OPEj}]; 

SymbolicSum3[a_^OPEj b_^(- OPEi + OPEj) c_^(opem4_ -OPEi),       
    {OPEi, 0, opem4_}, {OPEj, 0, OPEi} 
   ] :=(
        (a^(opem4+2)/((a-(1/b)) (a-c)) + b^(-opem4-2)/(
         ((1/b)-a) ((1/b)-c)) +
         c^(opem4+2)/((c-a) (c-(1/b))) )
               ) /; FreeQ2[{any,opem4},{OPEi,OPEj}]; 

SymbolicSum3[a_^OPEj b_^(any_. + OPEi - OPEj) c_^(opem4_ -OPEi),       
      {OPEi, 0, opem4_}, {OPEj, 0, OPEi} 
   ] :=b^(any)*(
        (a^(opem4+2)/((a-b) (a-c)) + b^(opem4+2)/((b-a) (b-c)) +
         c^(opem4+2)/((c-a) (c-b)) )
               ) /; FreeQ2[{any,opem4},{OPEi,OPEj}]; 

SymbolicSum3[a_^OPEi b_^(any_. + OPEj - OPEi) c_^(opem4_ -OPEj),
    {OPEi, 0, OPEj}, {OPEj, 0, opem4_}
   ] := b^(any)*(
        (a^(opem4+2)/((a-b) (a-c)) + b^(opem4+2)/((b-a) (b-c)) +
         c^(opem4+2)/((c-a) (c-b)) )
                ) /; FreeQ2[{opem4,any},{OPEi,OPEj}];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SymbolicSum3 | \n "]];
Null
