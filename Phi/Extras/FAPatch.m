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
    "If an unpatched copy of FeynArts is present in $FeynCalcDirectory, \
evaluating FAPatch causes the files making up FeynArts to be modified in \
order for FeynArts to be compatible with FeynCalc";

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

Begin["`Private`"];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(*Error message*)
$ok = True;
checkok := 
    If[$ok =!= True, 
      Print["Your FeynArts installation is not complete or the version you 
have cannot be handled by this program"]; Return[]];


(* ------------------------------------------------------------------------------ *)
(*Generic patching function*)

patch[filename_, replacements_List] := 
(
(*Read and patch the file*)
fname = $FeynCalcDirectory <> $PathnameSeparator <> filename;
Print[fname]; str = ""; linelist = {}; foundrev = False; 
      strm = OpenRead[fname];
While[ToString[str] != "EndOfFile", str = Read[strm, String]; str1 = str;
        Do[
          If[StringMatchQ[ToString[str1], "*" <> replacements[[i, 1]] <> "*"],
             foundrev = True; repl = str1; 
            str1 = StringReplace[
                     StringReplace[
                       str1, {replacements[[i, 1]] :> replacements[[i, 2]]}, 
                    MetaCharacters -> Automatic], "$1" -> repl]],
		  {i, 1, Length[replacements]}]; 
        If[ToString[str] != "EndOfFile", 
          If[str =!= str1, Print["Old: ", str, ", \nNew: ", str1]]; 
          linelist = Append[linelist, str1]]]; Close[strm];
(*Write the file*)
strm = OpenWrite[$FeynCalcDirectory <> $PathnameSeparator <> filename];
Do[WriteString[strm, linelist[[i]], "\n"], {i, 1, Length[linelist]}]; 
      Close[strm];
);


FAPatch := (

(*Check that files are there*)

If[FileNames["FeynArts.m", $FeynCalcDirectory] === {}, $ok = False];
If[FileNames["Setup.m", $FeynCalcDirectory] === {}, $ok = False];
checkok;


(*Check version number; must be >= 3*)

$ok = False; str = ""; linelist = {}; strm = 
  OpenRead[$FeynCalcDirectory <> $PathnameSeparator <> "FeynArts.m"];
While[ToString[str] != "EndOfFile", str = Read[strm, String]; 
  If[StringMatchQ[ToString[str], "*FeynArts*Version*"], 
    Do[If[SyntaxQ[StringTake[str, -i]], 
        If[NumberQ[num1 = ToExpression[StringTake[str, -i]]], 
          num = num1]], {i, 1, 7}]; 
    If[num >= 3, $ok = True]]]; Close[strm];
checkok;


(*Check that patch has not already been applied*)

str = ""; If[FileNames["FeynArts.m", $FeynCalcDirectory] =!= {}, 
  strm = OpenRead[$FeynCalcDirectory <> $PathnameSeparator <> "FeynArts.m"];
While[ToString[str] != "EndOfFile", str = Read[strm, String]; 
    If[StringMatchQ[ToString[str], 
        "*Frederik Orellana*", 
        IgnoreCase -> True], 
      Print["This copy of FeynArts has already been patched!"]; Close[strm]; Return[]]]; 
  Close[strm], Print["Cannot find FeynArts.m!"]; Close[strm]; Return[]];


(*Launch confirm dialog*)

If[$ok === True, 
    If[StringMatchQ[
        test=ToString[
          Input["An installation of FeynArts has been found in " <>
    StringReplace[ToString[$FeynCalcDirectory], {"\\" -> "\\\\", "\n" -> ""}] <>
    ". I will now patch FeynArts to allow interoperation with FeynCalc. Continue (yes/no/abort)?"]],
        "yes", IgnoreCase -> True], Print["OK, starting.."], 
      Print["OK, no files have been modified."]; 
      If[StringMatchQ[test,"abort", IgnoreCase -> True],Abort[],Return[]]], 
    Print["Your FeynArts installation is not complete or the version you have 
cannot be handled by this program"]; Return[]];


(* ------------------------------------------------------------------------------ *)
(*Include the Phi particle patterns*)

Print[ "Altering P$Generic in Setup.m.\n

   >Please check that this is actually done. If not, do it manually."];
strm = OpenAppend[$FeynCalcDirectory <> $PathnameSeparator <> "Setup.m"];
WriteString[strm, "\nP$Generic = 
Union[P$Generic, HighEnergyPhysics`Phi`Objects`$ParticleHeads]\n"]; 
Close[strm];


(* ------------------------------------------------------------------------------ *)
(* Add fermion heads + small fixes in Analytic.m*)

patch["FeynArts" <> $PathnameSeparator <> "Analytic.m",
{"F|U" -> "F | U", "F| U" -> "F | U", "F |U" -> "F | U", 
"F | U" -> "F | U | HighEnergyPhysics`Phi`Objects`$FermionHeads", 
"F]" -> "F|HighEnergyPhysics`Phi`Objects`$FermionHeads]", 
"Global`DiracSpinor[*mom_*,*mass_*,*___*]*:=*FeynArts`Spinor[*mom*,*mass*]*;" -> 
"", 
"SequenceForm[StringTake[ToString[type], 3]" -> 
 "SequenceForm[StringTake[ToString[type],Min[3,StringLength[ToString[type]]]]",
"Cases[p, PropagatorDenominator[__]]" :>
"Cases[p, HoldPattern[PropagatorDenominator[__]]]" }];
 
 
(* ------------------------------------------------------------------------------ *)
(* Allow one-vertices in Insert.m*)

patch["FeynArts" <> $PathnameSeparator <> "Insert.m",
{"DeleteCases[Take[#, 2], Vertex[1, ___][_]]&/@ top," -> 
"(DeleteCases[Take[#,2],Vertex[1][_]]&/@(top/.p:Propagator[Internal][___,Vertex[1,___][_],___]:>(p/.Vertex[1]->Vertex[vertexone])))/.vertexone -> 1,",
"MapIndexed[ Append[#1, Field@@ #2]&, top" -> 
"MapIndexed[Append[#1,Field@@#2]&,Sort[Sort[Take[#,2]]&/@ top/. {Incoming->AAA,Outgoing->AAB}]/. {AAA->Incoming,AAB->Outgoing}"}];


(* ------------------------------------------------------------------------------ *)
(* Allow one-vertices in Utilities.m*)

patch["FeynArts" <> $PathnameSeparator <> "Utilities.m",
{"Union[ Cases[top, Vertex[n__][_] /; {n} =!= {1}, {2}] ]" ->
"Union[Join[Cases[Cases[top,Propagator[Internal][__]],Vertex[n__][_],Infinity],Cases[top,Vertex[n__][_]/;{n}=!={1},{2}]]]"}];


(* ------------------------------------------------------------------------------ *)
(* Small fixes in Graphics.m*)

patch["FeynArts" <> $PathnameSeparator <> "Graphics.m",
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

End[]"}];


(* ------------------------------------------------------------------------------ *)
(* Make it known that the FA code has been patched and change context*)

patch["FeynArts.m", {"Print[*\"last revis*\"]" -> 
"$1;\nPrint[\"patched for use with FeynCalc by Frederik Orellana\"];\n\n
(*To avoid error messages on reload*)\n
If[NumberQ[HighEnergyPhysics`FeynArts`$FeynArts],\n
ClearAll[HighEnergyPhysics`FeynArts`Greek,HighEnergyPhysics`FeynArts`UCGreek],\n
Remove[HighEnergyPhysics`FeynArts`$FeynArts]];", 
"BeginPackage[\"FeynArts`\"]" -> 
"BeginPackage[\"HighEnergyPhysics`FeynArts`\"];",
"LoadPackage*:=" -> 
"If[ValueQ[HighEnergyPhysics`FeynCalc`$FeynCalcDirectory], $FeynArtsDir=HighEnergyPhysics`FeynCalc`$FeynCalcDirectory<>$PathnameSeparator,\n
Remove[HighEnergyPhysics`FeynCalc`$FeynCalcDirectory]];\n\n$1",
"F | S | V | U | SV" -> "F | S | V | U | SV | HighEnergyPhysics`Phi`Objects`$ParticleHeads"}];


(* ------------------------------------------------------------------------------ *)
(* The files loop *)

(*The list of files to be modified*)
filelist = {"FeynArts.m", "Setup.m", 
      "FeynArts" <> $PathnameSeparator <> "Analytic.m", 
      "FeynArts" <> $PathnameSeparator <> "Graphics.m", 
      "FeynArts" <> $PathnameSeparator <> "Initialize.m", 
      "FeynArts" <> $PathnameSeparator <> "Insert.m", 
      "FeynArts" <> $PathnameSeparator <> "Topology.m", 
      "FeynArts" <> $PathnameSeparator <> "Utilities.m"};

(*The list of replacements*)
replacelist = {
      "$Verbose = 2" -> "$Verbose := HighEnergyPhysics`FeynCalc`$VeryVerbose",
      "InferFormat" -> "tmpInfer", "SetLoop" -> "tmpsetloop", 
      "Loop" -> "HighEnergyPhysics`FeynCalc`Loop`Loop", 
      "Indices" -> "FAIndices", 
      "Global`PolarizationVector" -> "Global`FAPolarizationVector", 
      "FeynAmp" -> "FAFeynAmp",
      "PropagatorDenominator" -> 
        "HighEnergyPhysics`FeynCalc`PropagatorDenominator`PropagatorDenominator", 
      "FeynAmpDenominator" -> 
        "HighEnergyPhysics`FeynCalc`FeynAmpDenominator`FeynAmpDenominator", 
      "GaugeXi" -> "HighEnergyPhysics`FeynCalc`GaugeXi`GaugeXi", 
      "NonCommutative" -> "FANonCommutative", 
      "Global`DiracSpinor" -> 
        "HighEnergyPhysics`FeynCalc`DiracSpinor`DiracSpinor", 
      "FeynArts`DiracSpinor" -> 
        "HighEnergyPhysics`FeynCalc`DiracSpinor`DiracSpinor", 
      "Global`DiracTrace" -> 
        "HighEnergyPhysics`FeynCalc`DiracTrace`DiracTrace", 
      "tmpInfer" -> "InferFormat", "tmpsetloop" -> "SetLoop", 
      "HighEnergyPhysics`FeynCalc`Loop`LoopNr" -> "LoopNr", 
      "\"HighEnergyPhysics`FeynCalc`Loop`Loop\"" -> "\"Loop\"", 
      "HighEnergyPhysics`FeynCalc`Loop`LoopPD" -> "LoopPD", 
      "KinematicFAIndices" -> "KinematicIndices", 
      "CreateFAFeynAmp" -> "CreateFeynAmp", 
      "FADiracFASpinor" -> "FADiracSpinor", "FAFA" -> "FA", 
      "FAHighEnergyPhysics" -> "HighEnergyPhysics"};
	
Do[patch[filelist[[i]], replacelist], {i, 1, Length[filelist]}];


(* ------------------------------------------------------------------------------ *)
(* Copy Automatic.gen and Automatic.mod over *)

Print["Installing model files\n"];

CopyFile[ToFileName[{$FeynCalcDirectory,"Phi","Extras"},"Automatic.gen"],
         ToFileName[{$FeynCalcDirectory,"Models"},"Automatic.gen"]];
CopyFile[ToFileName[{$FeynCalcDirectory,"Phi","Extras"},"Automatic.mod"],
         ToFileName[{$FeynCalcDirectory,"Models"},"Automatic.mod"]];

Print["\nFinished!\n"];

);


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

End[];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

EndPackage[];

If[$Verboseness > 0,WriteString["stdout", "FAPatch | \n "]];

