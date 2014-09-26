(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: B1 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: scalar integral *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctables`B1`",
             {"HighEnergyPhysics`FeynCalc`",
             (*So the derivative of B1 will work also when DB0 has not been called *)
             "HighEnergyPhysics`fctables`DB1`"}];


B1::"usage"=
"B1[pp,m1^2,m2^2] is the Passarino-Veltman B1-function.
All arguments are scalars and have dimension mass^2.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

ClearAttributes[B1, ReadProtectecd];

B0 = MakeContext["B0"];
BReduce = MakeContext["BReduce"];
Factor2 = MakeContext["Factor2"];
FreeQ2 = MakeContext["FreeQ2"];
small = MakeContext["CoreObjects","SmallVariable"];
(*MakeContext[DB1];*)

 nos[x_] := If[(x =!= 0) && FreeQ[x, small], True, False];

(* ***************************************************************** *)
(*                          pave15                                   *)
(* ***************************************************************** *)
(* B1def *)
 Options[B1] = {BReduce->True};
 B1[a_,b_,c_,ops___Rule] :=  bb1[a, b, c] /; 
                 ((BReduce/.{ops}/.Options[B1])===True) && 
                  (Head[bb1[a,b,c]] =!= bb1) && 
                  FreeQ2[{a,b,c},  
                    {Blank, BlankSequence, BlankNullSequence, Pattern}];
(* Special cases, if photon and fermionic small masses are present *)
 bb1[small[me_]^n_., small[me_]^n_., small[mla_]^m_.]:=
   ( -1/2 B0[small[me]^n, small[me]^n, 0] - 1/2 )/; TrueQ[mla < me];

 bb1[small[me_]^n_., small[mla_]^n_., small[me_]^m_.]:=
   (1/2 - 1/2 B0[small[me]^n,0 ,small[me]^n]) /; TrueQ[mla < me];

(* other special cases of B1 *)

(* B1( p,m,m ) = -1/2 B0( p,m,m )  *)
 bb1[pp_,mm_,mm_] := -1/2 B0[pp,mm,mm];
 bb1[mm_, mm_, 0]:= -1/2 B0[mm, mm, 0] - 1/2;
 bb1[mm_, 0, mm_]:= 1/2 - B0[mm,0,mm]/2;
 bb1[0,0,mm_]:=-1/2 B0[0,0,mm]+1/4;
 bb1[small[_]^n_.,0,mm_]:=( -1/2 B0[0,0,mm] + 1/4 )/;nos[mm];
 bb1[0,small[_]^n_.,mm_]:=( -1/2 B0[0,0,mm] + 1/4 )/;nos[mm];
 bb1[0,mm_,0]           :=( -1/2 B0[0,0,mm] - 1/4 )/;nos[mm];

 bb1[small[_]^n_.,small[_]^n_.,mm_]:=( -1/2 B0[0,0,mm] + 1/4 )/;nos[mm];
 bb1[small[_]^n_.,mm_,small[_]^n_.]:=( -1/2 B0[0,0,mm] - 1/4 )/;nos[mm];
 bb1[small[_]^n_.,mm_,0]:=( -1/2 B0[0,0,mm] - 1/4 )/;nos[mm];
(* smaddef *)
  smanull[_]:=0;
  smad[x_]:=Block[{nx=Factor2[x]/.small->smanull},
                   Factor2[Numerator[nx]]/ Factor2[Denominator[nx]]
                 ];
(* B1 in general *)
 bb1[pp_,ma0_,ma1_ ]:=(smad[ma1-ma0]/(2 pp) (B0[pp,ma0,ma1] -
                                            B0[0,ma0,ma1]) - 
                        1/2 B0[pp,ma0,ma1]
                      ) /; nos[pp];
(* ***************************************************************** *)

   B1 /:
   MakeBoxes[B1[a_,b_,c_,___Rule]  ,TraditionalForm] :=
   RowBox[{SubscriptBox["B","1"], "(", 
        MakeBoxes[a,TraditionalForm],",",
           MakeBoxes[b,TraditionalForm],",",
             MakeBoxes[c,TraditionalForm],")"}
         ];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "B1 | \n "]];
Null
