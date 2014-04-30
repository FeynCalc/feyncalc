(* FAPatch *)

(* Patch to be applied to FeynArts for compatibility with FeynCalc/PHI *)

(* Author:  Frederik Orellana, fjob@cabocomm.dk

   Date:  1/9-2001
   
   Context: HighEnergyPhysics`Phi`FAPatch`

   Package version:  1.2

   Mathematica version:  4.0 *)

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

BeginPackage["HighEnergyPhysics`Phi`FAPatch`", {"HighEnergyPhysics`FeynCalc`"}];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)


FAPatch::"usage" = 
    "If an unpatched copy of FeynArts is present in $FeynArtsDirectory, \
evaluating FAPatch causes the files making up FeynArts to be modified in \
order for FeynArts to be compatible with FeynCalc.";

FilePatch::"usage" = 
    "FilePatch[f, rp] replaces the patterns given by rp in the file f.  \
rp should be a list of the form {string -> string, ..}.";

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

Begin["`Private`"];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(*get the FeynCalc Contexts dynamically : *)
fullFCContext[s_String] := Block[{$ContextPath},
         ToString[HighEnergyPhysics`FeynCalc`MakeContext[s]]];

fa2fc = (# -> (fullFCContext[#]))&/@ {
"Loop", "PolarizationVector", "FeynAmp", "PropagatorDenominator", "GaugeXi", "NonCommutative"};

(* Defaults *)

(*The list of files to be modified*)
Options[FAPatch] = {
  File -> {"FeynArts.m", "Setup.m", 
      "FeynArts" <> $PathnameSeparator <> "Analytic.m", 
      "FeynArts" <> $PathnameSeparator <> "Graphics.m", 
      "FeynArts" <> $PathnameSeparator <> "Initialize.m", 
      "FeynArts" <> $PathnameSeparator <> "Insert.m", 
      "FeynArts" <> $PathnameSeparator <> "Topology.m", 
      "FeynArts" <> $PathnameSeparator <> "Utilities.m",

      "Models" <> $PathnameSeparator <> "SMQCD.mod",
      "Models" <> $PathnameSeparator <> "SMc.mod",
      "Models" <> $PathnameSeparator <> "SMbgf.mod",
      "Models" <> $PathnameSeparator <> "SM.mod",
      "Models" <> $PathnameSeparator <> "QED.mod",
      "Models" <> $PathnameSeparator <> "MSSMQCD.mod",
      "Models" <> $PathnameSeparator <> "MSSM.mod",
      "Models" <> $PathnameSeparator <> "QED.gen",
      "Models" <> $PathnameSeparator <> "Lorentzbgf.gen",
      "Models" <> $PathnameSeparator <> "Lorentz.gen",
      "Models" <> $PathnameSeparator <> "Dirac.gen",
      "Models" <> $PathnameSeparator <> "DiracU.gen"
},

  (*The list of replacements*)
  (*Some regular expression utilities would be VERY nice...*)

  Replace -> Join[
       {
      "InferFormat" -> "tmpInfer",
      "SetLoop" -> "tmpsetloop", 
      "CreateFeynAmp" -> "tmpcreatefeynamp", 
      "$Verbose = 2" -> "$Verbose := HighEnergyPhysics`FeynCalc`$VeryVerbose" ,
      "Format" -> "format1",
      "GS" -> "Gstrong",
      "Tuples" -> "FATuples",
      "Clip" -> "FAClip"
       }, fa2fc,
      {
      "Global`DiracSpinor" -> fullFCContext["DiracSpinor"],
      "FeynArts`DiracSpinor" -> fullFCContext["DiracSpinor"],
      "Global`DiracTrace" -> fullFCContext["DiracTrace"],

      "format1[Global`a, Global`c]" -> "Format[Global`a, Global`c]",
      "tmpInfer" -> "InferFormat",
      "tmpsetloop" -> "SetLoop", 

      "HighEnergyPhysics`FeynCalc`Loop`LoopNr" -> "LoopNr", 
      "\"HighEnergyPhysics`FeynCalc`Loop`Loop\"" -> "\"Loop\"", 
      "HighEnergyPhysics`FeynCalc`Loop`LoopPD" -> "LoopPD",
      "HighEnergyPhysics`FeynCalc`FeynAmp`FeynAmpList" ->
        "HighEnergyPhysics`FeynCalc`FeynAmpList`FeynAmpList",
      "HighEnergyPhysics`FeynCalc`FeynAmp`FeynAmpDenominator" ->
        "HighEnergyPhysics`FeynCalc`FeynAmpDenominator`FeynAmpDenominator",
      "tmpcreatefeynamp" -> "CreateFeynAmp",
      "FourVector/: -FourVector[ mom_, mu___ ] := FourVector[Expand[-mom], mu]" -> " ",
      "FourVector[ 0, ___ ] = 0" -> " "
      }
           ]
};

(*Error message*)
$ok = True;
checkok := 
If[$ok =!= True, 
  If[Global`$FeynCalcStartupMessages =!= False ,
    If[$Notebooks===True,
       CellPrint[Cell[TextData[{
	 "WARNING! Your FeynArts installation is not complete or the version you have cannot be used with this version of FeynCalc.\nFeynArts can be downloaded at ", ButtonBox["www.feynarts.de", ButtonData:>{
	 URL[ "http://www.feynarts.de"], None},
	 ButtonStyle->"Hyperlink", ButtonNote->"http://www.feynarts.de"]}
	],"Text"]],
      WriteString["stdout", "Your FeynArts installation is not complete or the version you have cannot be used with this version of FeynCalc.\nFeynArts can be downloaded at http://www.feynarts.de/.\n"];
    ];
  ];
Return[False]];


(* ------------------------------------------------------------------------------ *)
(*Generic patching function*)

patchPrint[x__]:=WriteString["stdout", StringJoin@@{x,"\n"}];	

FilePatch[filename_, replacements_List] := 
(
(*Read and patch the file*)
fname = ToFileName[{$FeynArtsDirectory}, filename];
patchPrint[fname]; str = ""; linelist = {}; foundrev = False; 
      strm = OpenRead[fname];
While[ToString[str] != "EndOfFile", str = Read[strm, String]; str1 = str;
        Do[
          If[StringMatchQ[ToString[str1], "*" <> replacements[[i, 1]] <> "*"],
             foundrev = True; repl = str1; 
            str1 = StringReplace[
                     StringReplace[
                       str1, {replacements[[i, 1]] -> 
                       (* :> Not valid in mma 3.0, changed 11/10-2002 after
                         bug report by Jeff Forshaw *)
                       replacements[[i, 2]]}, 
                    MetaCharacters -> Automatic], "$1" -> repl]],
		  {i, 1, Length[replacements]}]; 
        If[ToString[str] != "EndOfFile", 
          If[str =!= str1, patchPrint["Old: ", str, ", \nNew: ", str1]]; 
          linelist = Append[linelist, str1]]]; Close[strm];
(*Write the file*)
strm = OpenWrite[fname, PageWidth -> Infinity];
Do[WriteString[strm, linelist[[i]], "\n"], {i, 1, Length[linelist]}]; 
      Close[strm];
);


FAPatch[opts___Rule] := (

(*Check that files are there*)

If[StringQ[$FeynArtsDirectory] =!= True, $ok = False];
If[!checkok, Return[]];
If[FileNames["FeynArts.m", $FeynArtsDirectory] === {}, $ok = False];
If[FileNames["Setup.m", $FeynArtsDirectory] === {}, $ok = False];
If[!checkok, Return[]];


(*Check version number; must be >= 3*)

$ok = False; str = ""; linelist = {}; strm = 
  OpenRead[ToFileName[{$FeynArtsDirectory}, "FeynArts.m"]];
While[ToString[str] != "EndOfFile", str = Read[strm, String]; 
  If[StringMatchQ[ToString[str], "*FeynArts*Version*"], 
    Do[If[SyntaxQ[StringTake[str, -i]], 
        If[NumberQ[num1 = ToExpression[StringTake[str, -i]]], 
          num = num1]], {i, 1, 7}]; 
    If[num >= 3, $ok = True]]]; Close[strm];
If[!checkok, Return[]];

(*Check that patch has not already been applied*)

str = ""; If[FileNames["FeynArts.m", $FeynArtsDirectory] =!= {}, 
  strm = OpenRead[$FeynArtsDirectory <> $PathnameSeparator <> "FeynArts.m"];
While[ToString[str] != "EndOfFile", str = Read[strm, String]; 
    If[StringMatchQ[ToString[str], 
        "*Frederik Orellana*", 
        IgnoreCase -> True], 
(* this is not really necessary to see, right?
      Print["This copy of FeynArts has already been patched!"]; 
*)
Close[strm]; Return[]]]; 
  Close[strm], patchPrint["Cannot find FeynArts.m!"]; Close[strm]; Return[]];


(*Launch confirm dialog*)

If[$ok === True, 
    If[StringMatchQ[
        test=ToString[
          Input["An installation of FeynArts has been found in " <>
    StringReplace[ToString[$FeynArtsDirectory], {"\\" -> "\\\\", "\n" -> ""}] <>
    ". This program will now patch FeynArts to allow interoperation with FeynCalc. Continue (yes/no/abort)?"]],
        "yes", IgnoreCase -> True], patchPrint["OK, starting.."], 
      patchPrint["OK, no files have been modified."]; 
      If[StringMatchQ[test,"abort", IgnoreCase -> True],Abort[],Return[]]], 
    patchPrint["Your FeynArts installation is not complete or the version you have 
cannot be handled by this program"]; Return[]];


(* ------------------------------------------------------------------------------ *)
(*Include the PHI particle patterns and set FeynCalc options*)

patchPrint[ "Altering P$Generic in Setup.m.\n

   >Please check that this is actually done. If not, do it manually."];
strm = OpenAppend[$FeynArtsDirectory <> $PathnameSeparator <> "Setup.m"];
WriteString[strm,
"\nP$Generic = Union[Flatten[P$Generic | HighEnergyPhysics`Phi`Objects`$ParticleHeads]];\n
P$NonCommuting =  Union[Flatten[P$NonCommuting | HighEnergyPhysics`Phi`Objects`$FermionHeads]];\n
(*
SetOptions[FourVector, FeynCalcInternal -> False];\n
SetOptions[MetricTensor, FeynCalcInternal -> False];\n
SetOptions[DiracSlash, FeynCalcInternal -> False];\n
*)
(*Important. OneLoop is broken if FeynAmpDenominator is orderless*)
ClearAttributes[FeynAmpDenominator, Orderless];\n"]; 
Close[strm];


(* ------------------------------------------------------------------------------ *)
(* Add fermion heads + small fixes in Analytic.m*)

FilePatch["FeynArts" <> $PathnameSeparator <> "Analytic.m",
{"F|U" -> "F | U", "F| U" -> "F | U", "F |U" -> "F | U", 
"F | U" -> "F | U | HighEnergyPhysics`Phi`Objects`$FermionHeads", "F]" -> 
"F|HighEnergyPhysics`Phi`Objects`$FermionHeads]", 
"Global`DiracSpinor[*mom_*,*mass_*,*___*]*:=*FeynArts`Spinor[*mom*,*mass*]*;" -> 
"", 
"SequenceForm[StringTake[ToString[type], 3]" -> 
 "SequenceForm[StringTake[ToString[type],Min[3,StringLength[ToString[type]]]]",
"Cases[p, PropagatorDenominator[__]]" (*:>*) ->
"Cases[p, HoldPattern[PropagatorDenominator[__]]]" }];
 
 
(* ------------------------------------------------------------------------------ *)
(* Allow one-vertices in Insert.m*)

FilePatch["FeynArts" <> $PathnameSeparator <> "Insert.m",
{"DeleteCases[Take[#, 2], Vertex[1, ___][_]]&/@ top," -> 
"(DeleteCases[Take[#,2],Vertex[1][_]]&/@(top/.p:Propagator[Internal][___,Vertex[1,___][_],___]:>(p/.Vertex[1]->Vertex[vertexone])))/.vertexone -> 1,",
"MapIndexed[ Append[#1, Field@@ #2]&, top" -> 
"MapIndexed[Append[#1,Field@@#2]&,Sort[Sort[Take[#,2]]&/@ top/. {Incoming->AAA,Outgoing->AAB}]/. {AAA->Incoming,AAB->Outgoing}"}];


(* ------------------------------------------------------------------------------ *)
(* Allow one-vertices in Utilities.m*)

FilePatch["FeynArts" <> $PathnameSeparator <> "Utilities.m",
{"Union[ Cases[top, Vertex[n__][_] /; {n} =!= {1}, {2}] ]" ->
"Union[Join[Cases[Cases[top,Propagator[Internal][__]],Vertex[n__][_],Infinity],Cases[top,Vertex[n__][_]/;{n}=!={1},{2}]]]"}];


(* ------------------------------------------------------------------------------ *)
(* Small fixes in Graphics.m*)

FilePatch["FeynArts" <> $PathnameSeparator <> "Graphics.m",
{"ShortHand[ type_ ] := StringTake[ ToString[type], 3 ]" ->
"ShortHand[ type_ ] := StringTake[ ToString[type], Min[3,StringLength[ToString[type]]]]", 
"MmaChar[ _[c_] ] := FontForm[c, {\"Symbol\", fscale fsize}];" -> 
"(*MmaChar[_[c_]]:=FontForm[c,{\"Symbol\",fscale fsize}];*)", 
"StyleForm[DisplayForm[label], FontFamily -> LabelFont," -> 
 "StyleForm[If[res=True;label//.a_String:>(res=res&&SyntaxQ[a]);res, TraditionalForm[ToExpression[label]/.Null->\"\"],DisplayForm[label]], FontFamily->LabelFont,",
"LabelFont = \"Helvetica\"" -> "LabelFont=\"Times\"",
"End[]" -> 
"(*Below are the codes for arrows used by Mathematica*)
TeXToPS[\"\\\\leftrightarrow\"]:=SymbolChar[\"\\[LeftRightArrow]\"];
TeXToPS[\"\\\\leftarrow\"]:=SymbolChar[\"\\[LeftArrow]\"];
TeXToPS[\"\\\\rightarrow\"]:=SmbolChar[\"\\[RightArrow]\"];
TeXToPS[\"\\\\to\"]:=SymbolChar[\"\\[RightArrow]\"];

End[]",
"Orientation[ p1_, p2_ ] := N[ArcTan@@ (p2 - p1)]" ->
"Orientation[ p1_, p2_ ] := N[(If[{##}=={0,0},0,ArcTan[##]]&)@@ (p2 - p1)]"}];


(* ------------------------------------------------------------------------------ *)
(* Make it known that the FA code has been patched, change context and
   change to formatting in TraditionalForm only *)

FilePatch["FeynArts.m", {"Print[*\"last revis*\"]" -> 
"$1;\nPrint[\"patched for use with FeynCalc by Frederik Orellana and Rolf Mertig\"];\n\n
(*To avoid error messages on reload*)\n
If[NumberQ[HighEnergyPhysics`FeynArts`$FeynArts],\n
ClearAll[HighEnergyPhysics`FeynArts`Greek,HighEnergyPhysics`FeynArts`UCGreek],\n
Remove[HighEnergyPhysics`FeynArts`$FeynArts]];", 
"BeginPackage[\"FeynArts`\"]" -> 
"BeginPackage[\"HighEnergyPhysics`FeynArts`\"];\n\nSetAttributes[SetForm, HoldAll];\nSetForm[Global`a_, Global`b_, Global`c_:TraditionalForm] := (Format[Global`a, Global`c] := Global`b;);\nformat1 /: SetDelayed[format1[Global`a_], Global`b_] := SetForm[Global`a, Global`b];\nformat1 /: Set[format1[Global`a_], Global`b_] := SetForm[Global`a, Global`b];\n\n",
"LoadPackage*:=" -> 
"If[ValueQ[HighEnergyPhysics`FeynCalc`$FeynArtsDirectory], $FeynArtsDir=HighEnergyPhysics`FeynCalc`$FeynArtsDirectory<>$PathnameSeparator,\n
Remove[HighEnergyPhysics`FeynCalc`$FeynArtsDirectory]];\n\n$1",
"F | S | V | U | SV" -> "F | S | V | U | SV | HighEnergyPhysics`Phi`Objects`$ParticleHeads"}];


(* ------------------------------------------------------------------------------ *)
(* The files loop *)

filelist = File /. {opts} /. Options[FAPatch];

replacelist = Replace /. {opts} /. Options[FAPatch];

Do[FilePatch[filelist[[i]], replacelist], {i, 1, Length[filelist]}];


(* ------------------------------------------------------------------------------ *)
(* Copy Automatic.gen and Automatic.mod over *)

patchPrint["Installing model files\n"];

CopyFile[ToFileName[{$FeynCalcDirectory,"Phi","Extras"},"Automatic.gen"],
         ToFileName[{$FeynArtsDirectory,"Models"},"Automatic.gen"]];
CopyFile[ToFileName[{$FeynCalcDirectory,"Phi","Extras"},"Automatic.mod"],
         ToFileName[{$FeynArtsDirectory,"Models"},"Automatic.mod"]];

patchPrint["\nFinished!\n"];

);


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

End[];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

EndPackage[];

If[$Verboseness > 0,WriteString["stdout", "FAPatch | \n "]];

