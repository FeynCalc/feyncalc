(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: TFi *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 15 February '99 at 22:19 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: scalar integral *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`TFi`",
             "HighEnergyPhysics`FeynCalc`"];

TFi::usage = 
"TFi[d, pp, {{n1,m1},{n2,m2},{n3,m3},{n4,m4},{n5,m5}}] is
the 2-loop d-dimensional integral
1/( (q1^2 - m1^2)^n1  (q2^2 - m2^2)^n2 ((q1-p)^2 - m3^2)^n3 *
    ((q2-p)^2 - m4^2)^n4  ((q1-q2)^2 - m5^2)^n5 ) .

 TFi[d, pp, {x,y,z,v,w}, {{n1,m1},{n2,m2},{n3,m3},{n4,m4},{n5,m5}}] 
has as additional factors in the numerator (q1^2)^x*(q2^2)^y*(q1.p)^z*
(q2.p)^v*(q1.q2)^w .

 TFi[d, pp, dp, {a,b}, {{n1,m1},{n2,m2},{n3,m3},{n4,m4},{n5,m5}}] 
has as additional factors in the numerator  (OPEDelta.q1)^a * (OPEDelta.q2)^b;
dp is (OPEDelta.p).";


(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

              
TFi[depp__,{a___,b_Integer,c___}]:= TFi[depp,{a,{b,0},c}];

TFi[depp__,{a___,{0,m_/;m=!=0},b___}]:= TFi[depp,{a,{0,0},b}];

TFi[a__,{0,0},b__List]:= TFi[a,b];

TFi[d_,pp_,dp_,{0,0},{x1_,x2_,x3_,x4_,x5_},list_List]:=
  TFi[d,pp,{x1,x2,x3,x4,x5},list];

TFi[d_,pp_,dp_,{a_,b_},{0,0,0,0,0},list_List]:=TFi[d,pp,dp,{a,b},list];
TFi[d_,pp_,dp_/;Head[dp]=!=List,{0,0,0,0,0},list_List]:=TFi[d,pp,list];
TFi[d_,pp_,dp_/;Head[dp]=!=List,list_List]:=TFi[d,pp,list];
TFi[d_,pp_,dp_/;Head[dp]=!=List,{x1_,x2_,x3_,x4_,x5_},list_List]:=
  TFi[d,pp,{x1,x2,x3,x4,x5},list];


PQ[_Integer?Positive] := True;
PNQ[0] = True;
PNQ[_Integer?Positive] := True;

(* from tlrule[10] from TARCER *)
TFi[d_, pp_, {v_, w_, (x_)?PNQ, (y_)?PNQ, z_}, {{(n1_)?PQ, m1_}, 
    {(n2_)?PQ, m2_}, {0, _}, {0, _}, nm5_}] := ( 0 /; OddQ[x + y]); 

TFi[dim_,pp_, {a_, b_, (c_)?PNQ, (d_)?PQ, 0}, {nm1_, nm2_, nm3_, {0, 0}, nm5_}
   ] := TFi[dim, pp, {b,a, d, c, 0}, {nm2, nm1, {0,0}, nm3, nm5}] /; d > c;


TFi[__, { {_,_},{0,0},{_,_},{0,0},{_,0} }] := 0 ;
TFi[__, { {0,0},{_,_},{0,0},{_,_},{_,0} }] := 0 ;
TFi[__, { {0,0},{_,_},{_,0},{_,_},{0,0} }] := 0 ;
TFi[__, { {_,0},{_,_},{0,0},{_,_},{0,0} }] := 0 ;
TFi[__, { {_,_},{_,0},{_,_},{0,0},{0,0} }] := 0 ;
TFi[__, { {_,_},{0,0},{_,_},{_,0},{0,0} }] := 0 ;
TFi[__, { {0,0},{0,0},{0,0},{_,0},{_,_} }] := 0 ;
TFi[__, { {0,0},{0,0},{_,0},{0,0},{_,_} }] := 0 ;
TFi[__, { {0,0},{_,0},{0,0},{0,0},{_,_} }] := 0 ;
TFi[__, { {_,0},{0,0},{0,0},{0,0},{_,_} }] := 0 ;
TFi[__, { {_,_},{_,0},{0,0},{0,0},{0,0} }] := 0 ;
TFi[__, { {_,0},{_,_},{0,0},{0,0},{0,0} }] := 0 ;
TFi[__, { {_,_},{0,0},{0,0},{_,0},{0,0} }] := 0 ;
TFi[__, { {_,0},{0,0},{0,0},{_,_},{0,0} }] := 0 ;
TFi[__, { {0,0},{_,0},{_,_},{0,0},{0,0} }] := 0 ;
TFi[__, { {0,0},{_,_},{_,0},{0,0},{0,0} }] := 0 ;
TFi[__, { {0,0},{0,0},{_,_},{_,0},{0,0} }] := 0 ;
TFi[__, { {0,0},{0,0},{_,0},{_,_},{0,0} }] := 0 ;

If[$Notebooks,

mbt[z_] := ToBoxes[z, TraditionalForm]; 
  redblue[z_ /; Head[z] =!= Plus] := mbt[z]; 
  redblue[(z_) - 1] := StyleBox[mbt[z], FontColor -> RGBColor[1, 0, 0]]; 
  redblue[(z_Subscript) - 2] := 
   StyleBox[SubscriptBox[OverscriptBox[z[[1]], "_"], z[[2]]], 
    FontColor -> RGBColor[1, 0, 0]]; 
  redblue[(z_) - 2] := 
   StyleBox[OverscriptBox[mbt[z], "_"], FontColor -> RGBColor[1, 0, 0]]; 
  redblue[(z_) - 3] := 
   StyleBox[UnderscriptBox[OverscriptBox[mbt[z], "_"], "_"], 
    FontColor -> RGBColor[1, 0, 0]]; 
  redblue[(z_) + 1] := StyleBox[mbt[z], FontColor -> RGBColor[0, 0, 1]]; 
  redblue[(z_) + 2] := 
   StyleBox[OverscriptBox[mbt[z], "_"], FontColor -> RGBColor[0, 0, 1]]; 
  redblue[(z_Subscript) + 2] := 
   StyleBox[SubscriptBox[OverscriptBox[z[[1]], "_"], z[[2]]], 
    FontColor -> RGBColor[0, 0, 1]]; 

  TFi/: MakeBoxes[TFi[d_, pp_, {ur__}, {den__}], TraditionalForm] := 
    InterpretationBox @@ 
      {SubsuperscriptBox[StyleBox["F", Rule[SingleLetterItalics, False], 
         Rule[FontWeight, "Bold"]], RowBox @@ {redblue /@ {den}}, 
        RowBox[{"(", ToBoxes[d, TraditionalForm], ")", " ", ur}]], 
       TFi[d, pp, {ur}, {den}], Editable -> True} /; 
     MemberQ[{StandardForm, TraditionalForm}, TraditionalForm]; 

  TFi/: MakeBoxes[TFi[d_, pp_, dp_, any__, {den__}], TraditionalForm] := 
    (InterpretationBox @@ 
       {SubsuperscriptBox[StyleBox["F", 
          Rule[SingleLetterItalics, False], Rule[FontWeight, "Bold"]], 
         RowBox @@ {redblue /@ {den}}, 
         RowBox[{"(", ToBoxes[d, TraditionalForm], ")", " ", 
           Sequence @@ Flatten[{any}]}]], TFi[d, pp, dp, any, {den}], 
        Editable -> True}) /; Head[dp] =!= List; 

TFi/: 
   MakeBoxes[TFi[dpp__, {den__}], TraditionalForm] := 
    InterpretationBox @@ 
      {SubsuperscriptBox[StyleBox["F", Rule[SingleLetterItalics, False], 
         Rule[FontWeight, "Bold"]], RowBox @@ {redblue /@ {den}}, 
        RowBox[{"(", ToBoxes[First[{dpp}], TraditionalForm], ")"}]], 
       TFi[dpp, {den}], Editable -> True} /; 
     MemberQ[{StandardForm, TraditionalForm}, TraditionalForm] && 
      Head[Last[{dpp}]] =!= List; 

];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "TFi | \n "]];
Null
