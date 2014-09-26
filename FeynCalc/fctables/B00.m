(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: B00 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: scalar integral *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctables`B00`",{"HighEnergyPhysics`FeynCalc`"}];

B00::"usage"=
"B00[pp,m1^2,m2^2] is the Passarino-Veltman B00-function, i.e. the
coefficient function of g(mu nu). All arguments are scalars and have
dimension mass^2.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

ClearAttributes[B00, ReadProtectecd];

A0 = MakeContext["A0"];
B0 = MakeContext["B0"];
B1 = MakeContext["B1"];
BReduce = MakeContext["BReduce"];
Factor2 = MakeContext["Factor2"];
FreeQ2 = MakeContext["FreeQ2"];
small = MakeContext["CoreObjects","SmallVariable"];

PaVeReduce := PaVeReduce = MakeContext["PaVeReduce"];

(* ***************************************************************** *)
(*                          pave16                                   *)
(* ***************************************************************** *)

(* smaddef *)
  smanull[_]:=0;
  smad[x_]:=Block[{nx=Factor2[x]/.small->smanull},
                   Factor2[Numerator[nx]]/ Factor2[Denominator[nx]]
                 ];


pcheck[zz__]:=FreeQ2[{zz},{Blank,BlankSequence,BlankNullSequence,Pattern}];

(* B00def *)
 Options[B00]={BReduce->True};
 B00[x__,  BReduce->True]:= b00[x] /; ($LimitTo4 === True) && pcheck[z] ;

 B00[x__,  BReduce->True]:= PaVeReduce[PaVe[0,0,{First[{x}]},Rest[{x}]]] /;
         ($LimitTo4 === False) && pcheck[x];
 B00[x_,y_,z_]:= b00[x,y,z]/;( BReduce/.Options[B00] )===True &&
                             ($LimitTo4 === True) && pcheck[x,y,z];
 B00[x_,y_,z_]:= B00[x,y,z, BReduce->True
                    ]/;(BReduce/.Options[B00])===True &&
                             ($LimitTo4 === False) && pcheck[x,y,z];

 b00[0,mm_,mm_] := mm / 2 ( B0[0,mm,mm] + 1 )/;nos[mm];
 b00[small[em_]^n_.,mm_,mm_] := 
   mm / 2 ( B0[em^n,mm,mm] + 1 )/;nos[mm];
 b00[pp_,mm_,mm_]     :=  1/6 ( A0[mm]+B0[pp,mm,mm] smad[2 mm - pp/2] +
                                  smad[2 mm - pp/3]) /;nos[pp];
 b00[pp_,mm1_,mm2_]         :=  ( 1/6 ( A0[mm2]+
                               (B1[pp,mm1,mm2] ) smad[pp-mm2+mm1] )+
                                     smad[mm1/3] B0[pp,mm1,mm2] +
                                smad[ 1/6 ( mm1 + mm2 - pp/3 ) ] );

  B00 /: 
  MakeBoxes[B00[a_,b_,c_,___Rule]  ,TraditionalForm] :=
  Tbox[Subscript["B","00"],"(",a,  ", ", b, ", ", c,")"] 

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "B00 | \n "]];
Null
