(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: OPE2AI*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`OPE2AI`",
             "HighEnergyPhysics`FeynCalc`"];

OPE2AI::usage= "OPE2AI[tabname, listtodo, q1,q2,p] is ....";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[Cases2, ChangeDimension, FeynCalcInternal, FeynCalcExternal, 
            OPEDelta, OPE2TID, OPEm,
            ScalarProduct, SPD, Uncontract];

Options[OPE2AI] = {Directory -> HomeDirectory[]};

OPE2AI[tabname_, listtodo_List,q1_, q2_, p_, opt___Rule] := 
 Block[
{tabstrings, tabfile, new, len, left = {}, listi, pow, ph, pat, pc,
 pattern, optscal, newtab = {}, opts},
tabfile=(Directory/.{opt}/.Options[OPE2AI])<>
         "/HighEnergyPhysics/FeynCalc/" <> ToString[tabname]<>".m";
optscal = FeynCalcInternal /. Options[ScalarProduct];
(* assume for the moment that no elment of listtodo is in tabname *)
tabstrings = Drop[ReadList[tabfile, String], -1];
checkifempty = StringMatchQ[tabstrings//Last, "Special*"];
len = Length[listtodo];
OpenWrite[tabfile];
WriteString[tabfile, #,"\n"]& /@ tabstrings;

opts = FeynCalcInternal /. Options[ScalarProduct];
SetOptions[ScalarProduct, FeynCalcInternal -> False];
Do[ If[$VeryVerbose > 0, Print["AI: OPE2TID of ",i," out of ",len]];
       (* make a pattern if only OPEm is a non-integer exponent *)
       listi = listtodo[[i]];
       pc = Cases2[listi /. Power[a_, (b_/;Head[b]=!=Integer)] :> 
                   ph[b], ph] /. ph -> List;
       pow = Map[Variables, Flatten[pc]];
       If[pow === {{OPEm}} && Length[pc] === 1,
          If[!ValueQ[Global`em], pat = Global`em, pat = Unique[D]];
          listi = listi /. Power[a_, (b_/;Head[b]=!=Integer)] :> (a^pat)
         ];
   new = ChangeDimension[OPE2TID[
             OPE2TID[listi, q1, q2, p], q1, q2, p, Uncontract->True
                                ], 4] // FeynCalcExternal;
   If[ new === listi,  (*THEN *)AppendTo[left, listtodo[[i]]], (* ELSE *)
       AppendTo[newtab, new];
       If[checkifempty === True, checkifempty = False,
          WriteString[tabfile, " , \n"];
         ];
       listi = FeynCalcExternal[listi] /.  
               ScalarProduct -> SPD;
If[$VeryVerbose > 0, Print["listi = ", listi]];
       new   = FeynCalcExternal[new] /.
               ScalarProduct -> SPD;
       WriteString[tabfile, "FCIntegral[\n"];
       Write[tabfile, 
   listi /. pat :> 
       (ReleaseHold[ 
           Hold[condition[pattern[paat, 
                     Blank[]], unsameQ[head[paat], Integer]
                     ]] /. paat :> Evaluate[pat] /. pattern -> Pattern /. 
                           head -> Head /. 
                      condition -> Condition  /. unsameQ -> UnsameQ 
                   ]
        )
            ]; 
       WriteString[tabfile, "] :>\n"];
       Write[tabfile, new]
     ]
   , {i, len}
  ];
SetOptions[ScalarProduct, FeynCalcInternal -> opts];
WriteString[tabfile, "}; (*ENDOFSPECIALTABLE*)\n"];
(* left*)
newtab];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "OPE2AI | \n "]];
Null
