(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Eps is the head of Levi-Civita tensors *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`Eps`",{"HighEnergyPhysics`FeynCalc`"}];

Eps::"usage" =
"Eps[a, b, c, d] represents the totally antisymmetric epsilon
(Levi-Civita) tensor. The \"a,b, ...\" should have head
LorentzIndex or Momentum or Integer.
In case of integers the Levi-Civita tensor is evaluated immediately.
Eps has an option Dimension (default 4).
As alternative input LeviCivita[mu,nu, ...][p,q,...] can be used.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ChangeDimension];
dimension := dimension = MakeContext["CoreOptions","Dimension"];
lor   := lor = MakeContext["LorentzIndex"];
(*Added ExplicitLorentzIndex. 21/9-2002. F.Orellana*)
exlor   := exlor = MakeContext["ExplicitLorentzIndex"];
mom   := mom = MakeContext["Momentum"];

Options[Eps] = {dimension -> 4};

Eps[a__Symbol, ru___Rule] := (Signature[{a}] (Eps @@ Sort[{a}])) /;
                             Signature[{a}] =!= 1;

Eps[a__?(MatchQ[#,_Integer|exlor[_Integer]]&), ru___Rule] := Signature[{a}];

Eps[a___, n1_. (lor|exlor)[mu_,___], b___, n2_. (lor|exlor)[mu_,___],c___ ] := 0 /;
 NumberQ[n1 n2];
Eps[a___, n1_. mom[mu_,___], b___, n2_. mom[mu_,___],c___ ] := 0 /;
 NumberQ[n1 n2];
Eps[x__] :=  0 /; ((!FreeQ[{x}, lor[_,_Symbol -4]]) ||
                   (!FreeQ[{x}, mom[_,_Symbol -4]]) );

Eps[a___, lor[mu_,_Symbol], b___, ru___Rule] :=
  (Eps@@ {a, lor[mu], b, ru}) /; (dimension /.{ru} /.
                                  Options[Eps])===4;
Eps[a___, mom[mu_,_Symbol], b___, ru___Rule] :=
  (Eps@@ {a, mom[mu], b, ru}) /; (dimension /.{ru} /.
                                  Options[Eps])===4;
   Eps /:
   MakeBoxes[Eps[x__] ,TraditionalForm] :=
   SuperscriptBox["\[Epsilon]", Tbox[x]];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Eps | \n "]];
Null
