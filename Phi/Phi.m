(* ******************************************************** *)
(*                                                          *)
(*                          Phi                             *)
(*                                                          *)
(*                    Phenomenology of                      *)
(*                 hadronic interactions                    *)
(*                                                          *)
(* ******************************************************** *)


(* ******************************************************** *)

(* Title:    Phi *)

(* Author:   Frederik Orellana (fjob@cabocomm.dk) *)

(* Summary:  Phenomenology of hadronic interactions.
             A collection of packages for calculating
             transition amplitudes in low energy
             hadron physics.
             Keywords: ChPT, SU(2), SU(3), effective
             lagrangians, coupling vectors,
             renormalization, isospin, partial
             waves, Mandelstam variables, dispersion
             relations. *)

(* History:  Version 1.2 May 2001
             Version 1.1 February 1999 *)

(* Contexts: HighEnergyPhysics`Phi`,
             HighEnergyPhysics`Phi`Objects`,
             HighEnergyPhysics`Phi`Couplings`,
             HighEnergyPhysics`Phi`Renormalization`,
             HighEnergyPhysics`Phi`Channels`,
             HighEnergyPhysics`Phi`Palettes`. *)

(* Package Version 1.2 *)

(* Mathematica Version 4.0 *)

(* ******************************************************** *)

(* FeynCalc should be loaded *)

If[!ValueQ[HighEnergyPhysics`FeynCalc`$FeynCalcDirectory],
Phi::nofc = "Before loading Phi, you should load FeynCalc. \
The kernel will exit now.";
Message[Phi::nofc];
Exit[0]]

(* Get rid of old definitions *)

  tmp`pconf=Null;
  tmp`verb=Null;
  tmp`phi=Null;
  tmp`vv=Null;

If[HighEnergyPhysics`Phi`$Phi,
  tmp`pconf=HighEnergyPhysics`Phi`$PaletteConfiguration;
  tmp`verb=HighEnergyPhysics`FeynCalc`$VeryVerbose;
  tmp`phi=HighEnergyPhysics`Phi`$Phi;
  ClearAll["HighEnergyPhysics`Phi`*`*`*"];
  ClearAll["HighEnergyPhysics`Phi`*`*"];
  ClearAll["HighEnergyPhysics`Phi`*"];
  Unprotect[$Packages];
  $Packages=Complement[$Packages,
  {"HighEnergyPhysics`Phi`Renormalization`",
  "HighEnergyPhysics`Phi`Palettes`", "HighEnergyPhysics`Phi`Objects`",
  "HighEnergyPhysics`Phi`Channels`", "HighEnergyPhysics`Phi`Utilities`",
  "HighEnergyPhysics`Phi`Couplings`", "HighEnergyPhysics`Phi`"}];
  Protect[$Packages];
  $ContextPath=Complement[$ContextPath,
  {"HighEnergyPhysics`Phi`Renormalization`",
  "HighEnergyPhysics`Phi`Palettes`", "HighEnergyPhysics`Phi`Objects`",
  "HighEnergyPhysics`Phi`Channels`", "HighEnergyPhysics`Phi`Utilities`",
  "HighEnergyPhysics`Phi`Couplings`", "HighEnergyPhysics`Phi`"}];
];

(* ************************************************************** *)

BeginPackage["HighEnergyPhysics`Phi`"];

(* ************************************************************** *)

(* Usage definitions *)

$Phi::"usage" =
"$Phi is a variable indicating whether or not Phi is loaded - it is set \
to True when loading Phi";

$Configuration::"usage" =
"$Configuration is a string variable determining which configuration is loaded \
at startup or restart.  It should be set in PhiStart.m or with the \
configurations palette.  If the palette is used for restarting, the setting \
in PhiStart.m is overruled.  Possible values include \"ChPT2\" and \"QED\".  Default \
value : \"None\"";

$PaletteConfiguration::"usage" =
"$PaletteConfiguration is a string variable set when clicking on the \
configuration palette and overruling the setting of $Configuration.  Default \
value : \"None\"";

$Verboseness::"usage" =
"$Verboseness is a variable determining how much informative output is \
printed during calculations.  It's range is 0 (no output) to 3\
(maximal output).  Default value : 1";

VerbosePrint::"usage" =
"VerbosePrint[n,s], where n is an integer and s is one or more strings or expressions, \
prints s if $Verboseness>=n";

$Palettes::"usage" =
"$Palettes is a variable determining which palettes should be displayed \
on startup.  The posible values are lists of the file names in the directory \
Palettes (file names should be given as strings).  Default value : {}";

$HEPDir::"usage" =
"$HEPDir is a string variable specifying the full path to the parent \
directory of the directory HighEnergyPhysics (containing Phi). It should be set \
in the startup file First.m.  Default value : Directory[]";

PhiSymbols::"usage" =
"PhiSymbols[\"package\"] returns a list of symbols defined in the \
context package";

$PhiPackages::"usage" =
"$PhiPackages is a list of the contexts (given as strings) of the \
sub-packages of Phi.  Default value : {\"HighEnergyPhysics`Phi`Objects`\", \
\"HighEnergyPhysics`Phi`Couplings`\", \"HighEnergyPhysics`Phi`Channels`\", \
\"HighEnergyPhysics`Phi`Renormalization`\", \
\"HighEnergyPhysics`Phi`Palettes`\"}";

(* ************************************************************** *)

(* Global functions *)

Begin["`Private`"];

(* ************************************************************** *)

(*$Verboseness=1;*)
$Verboseness:=HighEnergyPhysics`FeynCalc`$VeryVerbose;

VerbosePrint[n_Integer,s__]:=If[$Verboseness>=n,Print[s]];

(* ************************************************************** *)

End[];

(* ************************************************************** *)

EndPackage[];

(* ************************************************************** *)

(* Loading of subpackages *)

HighEnergyPhysics`Phi`$HEPDir=
ParentDirectory[HighEnergyPhysics`FeynCalc`$FeynCalcDirectory];

$Path=Union[$Path,{HighEnergyPhysics`Phi`$HEPDir}];

Get["HighEnergyPhysics`Phi`First`"];

$PhiPackages = {"HighEnergyPhysics`Phi`Objects`",
"HighEnergyPhysics`Phi`Couplings`",
"HighEnergyPhysics`Phi`Channels`",
"HighEnergyPhysics`Phi`Utilities`",
"HighEnergyPhysics`Phi`Renormalization`",
"HighEnergyPhysics`Phi`Palettes`"};

tmp`olddir =
Directory[]; SetDirectory[HighEnergyPhysics`Phi`$HEPDir];
SetDirectory["HighEnergyPhysics"];SetDirectory["Phi"];

VerbosePrint[2,"Scanning defs files for symbols to be declared"];

If[StringMatchQ[$OperatingSystem,"*Windows*", IgnoreCase -> True]||
StringMatchQ[$OperatingSystem,"*DOS*", IgnoreCase -> True],
tmp`readopts=DOSTextFormat->True,tmp`readopts=DOSTextFormat->False];

Do[

  str = "";
  strm =
  OpenRead[StringTake[$PhiPackages[[i]],{23,-2}]<>".defs.m",tmp`readopts];
  PhiSymbols[$PhiPackages[[i]]] = {};
  While[str != "EndOfFile", str = Read[strm, String];
    If[str =!= EndOfFile,
      If[StringMatchQ[str, "*::*usage*"] &&
      StringMatchQ[str, "(*::*"] == False,
        pos = StringPosition[str, "::"][[1, 1]];
        sym = StringTake[str, pos - 1];
        PhiSymbols[$PhiPackages[[i]]] =
        Append[PhiSymbols[$PhiPackages[[i]]], sym]] ]
    ];

  Close[strm];,

{i, Length[$PhiPackages]}]

(VerbosePrint[2,"Declaring symbols from ",#];
DeclarePackage[#, PhiSymbols[#]])&/@ $PhiPackages;

SetDirectory[tmp`olddir];
Remove[str,strm,sym,pos];

(* ************************************************************** *)

(* Defaults *)

$Palettes={};

$Configuration="None";

If[tmp`pconf=!=Null,$PaletteConfiguration=tmp`pconf];
If[tmp`verb=!=Null,HighEnergyPhysics`FeynCalc`$VeryVerbose=tmp`verb];
If[tmp`phi=!=Null,$Phi=tmp`phi];

(* ************************************************************** *)

(* Loading of user definitions *)

VerbosePrint[2,"Loading HighEnergyPhysics`Phi`PhiStart`"];

Get["HighEnergyPhysics`Phi`PhiStart`"];

If[tmp`pconf=!=Null,$PaletteConfiguration=tmp`pconf];
If[tmp`verb=!=Null,HighEnergyPhysics`FeynCalc`$VeryVerbose=tmp`verb];
If[tmp`phi=!=Null,$Phi=tmp`phi];

(* ************************************************************** *)

(* Loading of palettes *)

(*KernelNotebookOpenDef =
(Clear[KernelNotebookOpen];KernelNotebookOpen[fn_]:=
Block[{nbl,nbcontents,nbrules,nb},
nbl = Get[fn];
nbcontents = Join @@ Select[nbl, (MatchQ[#, {_Cell ..}] &)];
nbrules =
  Select[nbl, (Head[#] === Rule && #[[1]] =!= WindowTitle) &];
nb = NotebookCreate[Sequence @@ nbrules,
      WindowTitle ->StringReplace[fn, ".nb" -> ""]];
NotebookWrite[nb, nbcontents];
]);*)

If[$Palettes=!={}&&HighEnergyPhysics`Phi`$Phi=!=True,
VerbosePrint[2,"Loading ","Palettes"];
(*(KernelNotebookOpenDef;KernelNotebookOpen[#])&/@$Palettes;*)
NotebookOpen[ToFileName[{HighEnergyPhysics`Phi`$HEPDir,"HighEnergyPhysics",
"Phi","Palettes"},#]]& /@ $Palettes;
];

(* ************************************************************** *)

(* Update particles and path *)

FAUpdate;

$Path=Union[$Path,{HighEnergyPhysics`Phi`$HEPDir}];

(* ************************************************************** *)

(* FeynArts definitions are cleared to avoid error messages *)

If[NumberQ[HighEnergyPhysics`FeynArts`$FeynArts],
ClearAll[HighEnergyPhysics`FeynArts`Greek, HighEnergyPhysics`FeynArts`UCGreek],
Remove[HighEnergyPhysics`FeynArts`$FeynArts]];

(* ************************************************************** *)

(* Phi system variables *)

$PaletteConfiguration="None";

$Phi=True;

(* Startup message *)

(*If[$Verboseness>0,
If[AtomQ[$FrontEnd],
(*no FrontEnd version*)Print["Phi 1.2.  http://phi.cabocomm.dk/"],
(*FrontEnd version with hyperlink*)CellPrint[
   Cell[TextData[StyleBox["Phi", FontSlant -> "Italic"] \
   (*Uncommenting removes hyperlink style*)(*, ""*), "1.2.", " ",
    ButtonBox[
    StyleBox["http://phi.cabocomm.dk/", FontColor -> RGBColor[0, 0, 1]],
    ButtonNote -> "The website of Phi",
    ButtonData -> {URL["http://phi.cabocomm.dk"], None},
    ButtonStyle -> "Hyperlink"]], "Text"]];
]];*)

(* ************************************************************** *)

(**)
