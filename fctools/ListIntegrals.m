(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ListIntegrals*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`ListIntegrals`",
             "HighEnergyPhysics`FeynCalc`"];

ListIntegrals::usage= 
"ListIntegrals[exp, {q1, q2}] gives a list of basic integrals 
(FeynAmpDenominator's multiplied by scalar products involving q1, q2). 
Any non-integer exponent (e.g. the (OPEm-1) in 
ScalarProduct[OPEDelta,q1]^(OPEm-1)) is replaced by OPEm. 
If the options Pair is set to True only integrals involving  
scalar products (not counting those with OPEDelta) are given;
with Pair -> False those are excluded.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[Cases2, Expand2, FeynAmpDenominator,FeynCalcInternal,
            FeynCalcExternal,FC2TLI,TLI2FC,
            Momentum, OPEDelta, OPEm, Pair, Power2, Select1, Select2,
            TLI
           ];

Options[ListIntegrals] = {Pair -> All, FC2TLI -> True};

ListIntegrals[exi_, {qq__}, opt___Rule] := 
 Block[{t1, t2, t3, t4, t5, tp, tr, exp,
        null1, null2, pa, splitit, fdsort, 
        pairoption, fctop},
pairoption = Pair /. {opt} /. Options[ListIntegrals];
fctop      = FC2TLI /. {opt} /. Options[ListIntegrals];
If[FreeQ[exi, TLI], exp = exi, exp = TLI2FC[exi]];

t1 = (FeynCalcInternal[exp]/.Power2 -> Power) + null1 + null2;
t2 = Select2[Table[Select2[t1[[i]], {qq}], {i, Length[t1]}], {qq}];
t3 = Union[t2 /. Power[a_ /; !FreeQ[a, Pair], 
                       b_ /; Head[b] =!= Integer] :> Power[a, OPEm] 
          ];

pa = Select1[Cases2[t3, Pair], OPEDelta];
tp = Select2[t3, pa];
Which[ pairoption === True, 
          t4 = t5 = {},
       pairoption === False,
          tp = {},
       pairoption === All,
       t3 = Select1[t3, pa];
(* in case there are integrals with 3 and 4 Momentum *)
If[Union[Length[Cases2[#,Momentum]]& /@ t3] =!= {3,4},
   t4 = {}; t5 = t3,
   t4 = Select[t3, (Length[Cases2[#,Momentum]] === 3)&];
   t5 = Select[t3, (Length[Cases2[#,Momentum]] === 4)&];
  ];
splitit = If[$VersionNumber <= 2.2, Times, Split];
fdsort[a_, b_] := If[(Length[  (Cases2[a, FeynAmpDenominator] /. 
                                 FeynAmpDenominator -> (splitit[{##}]&))
                            ]
                     ) > 
                     (Length[  (Cases2[b, FeynAmpDenominator] /.
                                 FeynAmpDenominator -> (splitit[{##}]&))
                            ]
                     ), True, False];
     ];
tr = FeynCalcExternal[Flatten[Sort[#,fdsort]& /@ {t5, t4, tp}]];
If[fctop === True,
   tr = Map[FC2TLI[#, qq]&, tr];
   tr2 = Select2[tr, TLI[_, {___,0,___,0,___,0,___}]];
   tr  = Select1[tr, tr2];
   tr3 = Select2[tr, TLI[_, {___,0,___,0,___}]];
   tr  = Select1[tr, tr3];
   tr4 = Select2[tr, TLI[_, {___,0,___}]];
   tr = Select1[tr, tr4];
   tr = Join[tr, tr4,tr3,tr2];
  ];
tr];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ListIntegrals | \n "]];
Null
