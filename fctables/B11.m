(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: B11 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: scalar integral *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctables`B11`",
             "HighEnergyPhysics`FeynCalc`"];

B11::usage=
"B11[pp,m1^2,m2^2] is the Passarino - Veltman B11-function, i.e.
the coefficient function of p(mu) p(nu).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   SetAttributes[B11, ReadProtected];

ClearAttributes[B11, ReadProtectecd];

A0 = MakeContext["A0"];
B0 = MakeContext["B0"];
BReduce = MakeContext["BReduce"];
Factor2 = MakeContext["Factor2"];
FreeQ2  = MakeContext["FreeQ2"];
PaVe = MakeContext["PaVe"];
small = MakeContext["SmallVariable"];


 Options[B11]={BReduce->True};

pcheck[zz__]:=FreeQ2[{zz},{Blank,BlankSequence,BlankNullSequence,Pattern}];


 nos[x_] := If[(x =!= 0) && FreeQ[x, small] , True, False];

  smanull[_]:=0;
  smad[x_]:=Block[{nx=Factor2[x]/.small->smanull},
                   Factor2[Numerator[nx]]/ Factor2[Denominator[nx]]
                 ];


 B11[pe_, mm1_, mm2_,  BReduce->True]     :=
     b11[pe, mm1, mm2] /; ($LimitTo4 === True) && pcheck[pe,mm1,mm2] &&
                          (nos[pe] || ( (!nos[pe]) && (mm1 === mm2))) ;

 B11[x__,  BReduce->True]     :=
   PaVeReduce[PaVe[1,1,{{x}[[1]]}, Rest[{x}] ]] /;
      ($LimitTo4 === False) && (x[[1]] =!=0) && pcheck[x];
 B11[x_,y_,z_]:= b11[x,y,z]/; (( BReduce/.Options[B11] )===True )&&
                             ($LimitTo4 === True ) && pcheck[x,y,z] && 
                             (nos[x] || ( (!nos[x]) && (y === z)));
 B11[x_,y_,z_]:= B11[x,y,z, BReduce->True]/;( BReduce/.Options[B11] 
                                            )===True && pcheck[x,y,z] && 
                             ($LimitTo4 === False) && nos[x];
 b11[ 0,mm1_,mm1_ ] := 1/3 * B0[ 0,mm1,mm1 ];
(*??
 b11[ small[_]^n_.,mm1_,mm1_ ] := 1/3 * B0[ 0,mm1,mm1 ];
*)
 b11[ small[em_]^n_.,mm1_,mm1_ ] := 1/3 * B0[ small[em]^n,mm1,mm1 ];
 b11[ pp_,mm_,mm_]:= ( 1/(3pp) ( A0[mm]+B0[pp,mm,mm] smad[pp-mm]-
                                         smad[mm - pp/6] )) /;nos[pp];
 b11[ pp_,m1_,m2_ ] := ( 1/(3 pp) ( A0[m2] - smad[2 (pp-m2 + m1)]*
                       (PaVe[1,{pp},{m1,m2}]) - smad[m1] B0[pp,m1,m2] -
                        smad[ 1/2 (m1 + m2 - pp/3 )]) )/;nos[pp];


  B11 /: 
  MakeBoxes[B11[a_,b_,c_,___Rule]  ,TraditionalForm] :=
  Tbox[Subscript["B","11"],"(",a,  ", ", b, ", ", c,")"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "B11 | \n "]];
Null
