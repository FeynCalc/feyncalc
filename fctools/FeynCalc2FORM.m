(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FeynCalc2FORM*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 16 March '99 at 9:43 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`FeynCalc2FORM`",
             "HighEnergyPhysics`FeynCalc`"];

FeynCalc2FORM::usage= 
"FeynCalc2FORM[expr] displays expr in FORM syntax.
FeynCalc2FORM[file, x] writes x in FORM syntax to a file.
FeynCalc2FORM[file, x==y] writes x=y to a file in FORM syntax.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   SetAttributes[FeynCalc2FORM, ReadProtected];

MakeContext[
Cases2,
DiracGamma,
DiracGammaExpand,
DiracTrace,
Eps,
EpsDiscard, 
ExpandScalarProduct,
FeynCalcInternal,
FORMEpilog, 
FORMProlog, 
LorentzIndex,
Momentum,
MomentumExpand,
Pair,
SelectFree,
SelectNotFree,
TraceDimension];

(* for taking traces *)
Options[FeynCalc2FORM] = {EpsDiscard -> False, 
FORMEpilog -> "", FORMProlog -> "write statistics;",
Replace -> {"\[Alpha]"-> "al", "\[Beta]"->"be",
"\[Gamma]" -> "ga", "\[Delta]" -> "de",
"\[Mu]" -> "mu", "\[Nu]" -> "nu", "\[Rho]" -> "ro",
"\[Sigma]" -> "si"
                 },
TraceDimension -> 4      };

FeynCalc2FORM[ file_:"tFc2F", xy_, ru___Rule] := Block[
{holdy, lors, lors4, lorsn, lordim, other, noatomic, newx ,x,y, 
 srules, srule2, temp},
If[Head[xy] === Equal,
   x = xy[[1]]; y = xy[[2]]
   ,
   x = False; y = xy
  ];
srules = Replace /. {ru} /. Options[FeynCalc2FORM];
srule2 = Table[StringJoin@@Rest[Drop[
               Characters[ToString[InputForm[srules[[i,1]] ]]],-1]] -> 
               srules[[i,2]],{i,Length[srules]}
              ];
srules = Join[srules,srule2];

holdy = Hold@@
  {(FeynCalcInternal[y]//DiracGammaExpand//MomentumExpand) /.
    Pair -> ExpandScalarProduct /. 
    {Pair[a_,b_]^2 :> (Pair[a,b] . Pair[a,b]) /; 
     !FreeQ[{a,b}, LorentzIndex] 
    }
  };

If[(TraceDimension /. {ru} /. Options[FeynCalc2FORM]) === 4,
   If[!FreeQ[holdy, LorentzIndex[_,_]],
      holdy = holdy /. LorentzIndex[a,_] :> LorentzIndex[a] ];
   If[!FreeQ[holdy, Momentum[_,_]],
      holdy = holdy /. Momentum[a_,_] :> Momentum[a] ];
   If[!FreeQ[holdy, DiracGamma[_,_]],
      holdy = holdy /. DiracGamma[a,_] :> DiracGamma[a] ];
  ];

(* get the list of LorentzIndex *)
lors = Cases2[holdy, LorentzIndex];
lors = Union[lors] /. LorentzIndex[a_] :> a;
lors4 = SelectFree[lors, LorentzIndex];
lorsn = SelectNotFree[lors, LorentzIndex];
lordim = Union[lorsn /. LorentzIndex[_, di_] :> di];
If[Length[lordim] === 1,
   lordim = lordim[[1]],
   If[Length[lordim] >1, Print["too many different dimensions!!"],
      lordim = 4
     ]
  ];
lors  = lors /. LorentzIndex[a_, _] :> a;
lorsn = lorsn /. LorentzIndex[a_, _] :> a;

(* get the list of Momentum*)
moms = Cases2[holdy, Momentum];         (* a may be a sum *)
momentumlist = Union[Flatten[moms /. Momentum[a_,___] :> Variables[a]]];

(* get all other atomic variables *)         (* see p. 725  *)
other = SelectFree[Union[Cases[holdy, _Symbol, -1]],
                   Join[lors, momentumlist]
                  ];

noatomic = Union[Flatten[Map[Variables,
Cases[holdy/.Dot->Times,h_ /; 
      (!MemberQ[{LorentzIndex,Momentum,DiracGamma,Eps,
                 DiracTrace,Pair,Symbol}, Head[y]]
      ),Infinity]]]];
noatomic = Select[noatomic, 
                  (!MemberQ[{LorentzIndex,Momentum,DiracGamma,Eps,
                             DiracTrace,Symbol,Pair}, 
                  Head[#]])&
                 ];

(* replace the non-Symbol arguments of LorentzIndex and 
   Momentum by Symbols *)
nosyml = Select[Join[momentumlist, lors], Head[#] =!= Symbol &];
lm2form = Table[ nosyml[[i]] -> 
                ToExpression[ StringJoin[ "vFC", ToString[i] ] ],
                {i, Length[nosyml]}
              ];

(* for the reverse substitutions *)
form2l = Map[Reverse, lm2form];

index4list = lors4 /. lm2form;
indexnlist = lorsn /. lm2form;
momentumlist = momentumlist /. lm2form;

eps2f[a__] := -I Global`eE[a] /. Momentum[aa_,___] :> 
              aa /. LorentzIndex[bb_,___]:>bb;

pair2f[LorentzIndex[a_,___], LorentzIndex[b_,___]] := 
Global`dD[a/.lm2form, b/.lm2form];
$tracecount = 0;
diracg[5]      :=   Global`gA5[$tracecount];
diracg[6]      :=   Global`gA6[$tracecount]/2;
diracg[7]      :=   Global`gA7[$tracecount]/2;
diracg[_[ls_]] :=   Global`gA[$tracecount, ls];

(* assume that momenta are Symbols *)
pair2f[LorentzIndex[a_Symbol,___], Momentum[b_, ___]] := b[a/.lm2form];
pair2f[Momentum[a_Symbol,___], Momentum[b_, ___]] := b.a;

(* construct the list of substitutions for all noatomics *)
n2form  = Table[ noatomic[[i]] ->
                 ToExpression[ StringJoin[ "syFC", ToString[i] ] ],
                 {i, Length[noatomic]}
               ];
form2fc = Join[form2l, Map[Reverse, n2form]];
newsymlist = noatomic /. n2form;
newsymlist = Join[other, newsymlist];
If[!FreeQ[holdy, Complex], AppendTo[newsymlist, I]];

dirtr[a_] := ($tracecount++;
              a /. diracgamma -> diracg
             );

new = (holdy /. lm2form /. Pair -> pair2f /. Eps -> eps2f /.
       DiracGamma -> diracgamma /. Dot->Times /.
       DiracTrace -> dirtr /. n2form
      )[[1]];


temp = OpenWrite[$TemporaryPrefix<>"teEmpf", FormatType -> InputForm];
Write[temp, new];
newx = ReadList[
(*If[$OperatingSystem === "MacOS", $PathnameSeparator,""]*)
    $TemporaryPrefix <> "teEmpf", String
               ];
Close[temp];

DeleteFile[$TemporaryPrefix <> "teEmpf" ];
                                                                             


If[FileNames[file] =!= {}, DeletFile[file]];

newx = StringReplace[StringReplace[newx,srules],
                          {"\""->"", "dD"->"d_", "["->"(", "\\"->"",
                           "]" -> ")", " " -> "", "I" -> "i_",
                           "gA5" -> "g5_",
                           "gA6" -> "g6_",
                           "gA7" -> "g7_",
                           "gA"->"g_",
                           "eE"->"e_",
                           " . "->".", "$"->"_"
                          }
                    ];

(* construct the id  -  statements *)

downp = Select[DownValues[Pair]/.Momentum[a_,___]:>Momentum[a], 
               FreeQ2[#, {Blank, Pattern}]&];
idlist = {};
For[i = 1, i<=Length[downp], i++,
    dpi = {downp[[i, 1,1,1,1]], downp[[i,1,1,2,1]]};
    If[FreeQ[dpi, Plus], AppendTo[idlist, Append[dpi, downp[[i, 2]]]]]
   ];
idlist = idlist /. lm2form;

If[!FreeQ[momentumlist/.form2fc, Polarization],
   polvecs = SelectNotFree[momentumlist/.form2fc, Polarization];
   nidlist = Table[{polvecs[[j]], polvecs[[j,1]],0},{j,Length[polvecs]}];
   nidlist = nidlist /. lm2form;
   idlist = Join[idlist, nidlist];
  ];

(* there might be additional momenta *)
addmom = Cases[Cases2[downp, Momentum], _Symbol, -1];
momentumlist = Union[momentumlist, addmom];

OpenWrite[file, FormatType -> InputForm];

If[Length[newsymlist] > 0,
   WriteString[file, "Symbols "];
   For[ij = 1, ij < Length[newsymlist], ij++,
       WriteString[file, newsymlist[[ij]]];
       If[ij < Length[newsymlist], WriteString[file, ","]];
      ];
   WriteString[file, Last[newsymlist], ";\n"];
  ];

index4list = Map[StringReplace[ToString[#],srules]&,index4list];
indexnlist = Map[StringReplace[ToString[#],srules]&,indexnlist];
If[Length[index4list] > 0 && x =!= False,
   WriteString[file, "Indices "];
   For[ij = 1, ij < Length[index4list], ij++,
       WriteString[file, index4list[[ij]],","];
      ];
   WriteString[file, Last[index4list], ";\n"];
  ];

If[Length[indexnlist] > 0 && x =!= False,
   WriteString[file, "Indices "];
   For[ij = 1, ij < Length[indexnlist], ij++,
       WriteString[file, indexnlist[[ij]],"=",lordim,","];
      ];
   WriteString[file, Last[indexnlist], "=",lordim," ;\n"];
  ];

If[Length[momentumlist] > 0 && x =!= False,
   WriteString[file, "Vectors "];
   For[ij = 1, ij < Length[momentumlist], ij++,
       WriteString[file, momentumlist[[ij]]];
       If[ij < Length[momentumlist], WriteString[file, ","]];
      ];
   WriteString[file, Last[momentumlist], ";\n"];
  ];

formpro = FORMProlog/. {ru} /. Options[FeynCalc2FORM];
If[formpro =!= "" && x =!= False,
   If[Head[formpro] =!= List, formpro = Flatten[{formpro}]];
   Write[file];
   For[ij = 1, ij <= Length[formpro], ij++,
       WriteString[file, formpro[[ij]],"\n"];
      ];
   Write[file];
  ];

If[x=!= False, 
   WriteString[file, "Local ",x , " = ( \n"];
  ];
   newxstr = "";
   For[jjj = 1, jjj <= Length[newx],jjj++,
       WriteString[file, newx[[jjj]]];
       newxstr = newxstr <> newx[[jjj]];
       If[jjj < Length[newx],
          If[StringLength[newxstr<>newx[[jjj+1]]] > 79,
             newxstr = ""; 
             WriteString[file, "\n"];
            ]
         ];
(*Global`NEWX = newx;*)
       If[x===False && file === "tFc2F", Print[newx[[jjj]]]]
      ];
If[x=!= False, 
   WriteString[file, " ); \n   \n"];
  ];
 
(* in case there are traces *)
If[$tracecount > 0 && x =!= False,
   For[i = 1, i <= $tracecount, i++,
       If[(TraceDimension /. {ru} /. Options[FeynCalc2FORM]) === 4,
          WriteString[file, "trace4,"<>ToString[i]<>";\n"],
          WriteString[file, "tracen,"<>ToString[i]<>";\n"]
         ];
      ];
   WriteString[file, "contract 0;\n\n"]
  ];

If[(EpsDiscard /. {ru} /. Options[FeynCalc2FORM]) &&
   x =!= False,
   WriteString[file,  
                     "if ( count(e_,1) > 0 );\n",
                     "     discard;\n",
                     "endif;\n\n"  ]
  ];
  
If[Length[idlist] > 0 && x =!= False,
   Write[file];
   For[ij = 1, ij <= Length[idlist], ij++,
(*
       If[Head[Expand[idlist[[ij, 3]]]] =!= Plus,
*)
       WriteString[file, "id  ",idlist[[ij, 1]],".",idlist[[ij, 2]],
                         " = ", idlist[[ij, 3]]//InputForm, "; \n"
                  ];
(*
        ];
*)
      ];
   Write[file];
  ];

formepi = FORMEpilog /. {ru} /. Options[FeynCalc2FORM];
If[formepi =!= "" && x=!= False,
   If[Head[formepi] =!= List, formepi = Flatten[{formepi}]];
   Write[file];
   For[ij = 1, ij <= Length[formepi], ij++,
       WriteString[file, formepi[[ij]],"\n"];
      ];
   Write[file];
  ];
If[x=!= False,
   WriteString[file, "Print; \n"];
   WriteString[file, ".end"];
  ];

Close[file];

If[file === "tFc2F", If[FileNames["tFc2F"]=!={}, DeleteFile["tFc2F"]]];

form2fc];
End[]; EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FeynCalc2FORM | \n "]];
Null
