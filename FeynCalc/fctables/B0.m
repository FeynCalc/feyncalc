(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: B0 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 26 January '98 at 17:55 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: scalar integral *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctables`B0`",
             {"HighEnergyPhysics`FeynCalc`",
              (*So the derivative of B0 will work also when DB0 has not been called *)
              "HighEnergyPhysics`fctables`DB0`"}];

B0::"usage"=
"B0[pp,m1^2,m2^2] is the Passarino-Veltman two point integral.
All arguments are scalars and have dimension mass^2.";

B0Real::"usage"=
"B0Real is an option of B0 (default False). If set to True,
B0 is assumed to be real and
the relation B0[a,0,a] = 2 + B0[0,a,a]  is applied.";

B0Unique::"usage"=
"B0Unique is an option of B0. If set to True, B0[0,0,m2] is replaced
with (B0[0,m2,m2]+1) and B0[m2,0,m2] simplifies to (B0[0,m2,m2]+2).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

ClearAttributes[B0, ReadProtectecd];

A0 = MakeContext["A0"];
BReduce = MakeContext["BReduce"];
small = MakeContext["CoreObjects","SmallVariable"];
(*MakeContext[DB0];*)

(* B0def*)
 Options[B0] = { BReduce -> False, B0Unique -> False, B0Real -> False };

 B0[pe_,me2_,me1_,opt___]:=
    B0 @@ Prepend[ {me1,me2,opt}, Expand[pe]] /; !OrderedQ[{me2,me1}];
 B0[small[pp_]^j_., small[a_]^n_., small[b_]^m_.] := B0[pp^j, a^n, b^m];
 B0[0, small[a_]^n_., small[b_]^m_.] := B0[0, a^n, b^m];
 bop[x___] := BReduce/.Flatten[ Join[{x},Options[B0]] ];
 nos[x_] := True/;(x=!=0) && FreeQ[x,small]&&FreeQ[x,small];

 nos[x_] := If[(x =!= 0) && FreeQ[x, small] && FreeQ[x, small],
               True, False];

 B0[0,0,mm_,opt___]:=( B0[0,mm,mm] + 1 ) /; nos[mm] &&
          ( (B0Unique/.{opt}/.Options[B0]) === True );

 B0[mm_,0,mm_,opt___]:=( B0[0,mm,mm] + 2 ) /;
          ( (B0Unique/.{opt}/.Options[B0]) === True ) &&
          ( (B0Real/.{opt}/.Options[B0]) === True );

(*
??? Changed Jan 97
 B0[small[pp_]^n_., m1_, m2_, opt___] := B0[0, m1, m2, opt
                                           ]/;nos[m1]||nos[m2];
 B0[pp_, small[m1_]^n_., m2_, opt___] := B0[pp, 0, m2, opt
                                           ]/;nos[pp]||nos[m2];
 B0[pp_, m1_, small[m2_]^n_.,opt___ ] := B0[pp, m1, 0, opt
                                           ]/;nos[pp]||nos[m1];
*)
(* special cases *)
 B0[kl_, kmm_, mm_, opt___ ] :=  (A0[mm]/mm) /; nos[mm] && bop[opt] &&
                                     (((kl E+kmm)/.small[_]->0)===0);
 B0[kl_, m_, kmm_, opt___ ] :=  (A0[mm]/mm) /; nos[mm] && bop[opt] &&
                                     (((kl E+kmm)/.small[_]->0)===0);

 B0[0,mm_,mm_,opt___]        := (A0[mm]/mm - 1)/;nos[mm] && bop[opt];


(*fixed bop[opt] option Jan 1998*)
 B0[0,m1_,m2_, opts___Rule] := ((1/(m1-m2) A0[m1] - 1/(m1-m2) A0[m2]) /; 
                                m1 =!= m2) /; bop[opts];

(*
??? Changed Jan 97
 B0[small[_]^n_.,mm_,mm_,opt___] := 
     (A0[mm]/mm - 1)/;nos[mm] && bop[opt];
*)


   B0 /:
   MakeBoxes[B0[a_,b_,c_,___]  ,TraditionalForm] :=
   RowBox[{SubscriptBox["B","0"], "(", 
      MakeBoxes[a,TraditionalForm],",",
         MakeBoxes[b,TraditionalForm], ",",
            MakeBoxes[c,TraditionalForm], ")"}
         ];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "B0 | \n "]];
Null
