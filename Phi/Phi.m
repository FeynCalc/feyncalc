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

Global`$Configuration::"usage" =
"$Configuration is a string variable determining which configuration is loaded \
at startup or restart.  It can be set before loading FeynCalc, in PhiStart.m or with the \
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

$FAPatch::"usage" = "$FAPatch switches on and off checking for \
an unpatched FeynArts installation on PHI startup.  Default value : True";

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

(*Give first priority to First.m and PhiStart.m in homedir. Added 28/8-2001*)
$Path=Join[FileNames[$HomeDirectory <> $PathnameSeparator <>
    ".Mathematica" <> $PathnameSeparator <> "*" <> $PathnameSeparator <>
    "AddOns" <> $PathnameSeparator <> "Applications"],$Path];

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

{i, Length[$PhiPackages]}];

(VerbosePrint[2,"Declaring symbols from ",#];
DeclarePackage[#, PhiSymbols[#]])&/@ $PhiPackages;

SetDirectory[tmp`olddir];
Remove[str,strm,sym,pos];

(* ************************************************************** *)

(* Defaults *)

If[ValueQ[Global`$Configuration] =!= True,
  Global`$Configuration="None"
];

If[tmp`pconf=!=Null,$PaletteConfiguration=tmp`pconf];
If[tmp`verb=!=Null,HighEnergyPhysics`FeynCalc`$VeryVerbose=tmp`verb];
If[tmp`phi=!=Null,$Phi=tmp`phi];

$FAPatch=True;

(* ************************************************************** *)

(* Loading of user definitions *)

VerbosePrint[2,"Loading HighEnergyPhysics`Phi`PhiStart`"];

Get["HighEnergyPhysics`Phi`PhiStart`"];

If[tmp`pconf=!=Null,$PaletteConfiguration=tmp`pconf];
If[tmp`verb=!=Null,HighEnergyPhysics`FeynCalc`$VeryVerbose=tmp`verb];
If[tmp`phi=!=Null,$Phi=tmp`phi];

(* ************************************************************** *)

(* Update particles *)

FAUpdate;

(* ************************************************************** *)

(* FeynArts definitions are cleared to avoid error messages *)

If[NumberQ[HighEnergyPhysics`FeynArts`$FeynArts],
ClearAll[HighEnergyPhysics`FeynArts`Greek, HighEnergyPhysics`FeynArts`UCGreek],
Remove[HighEnergyPhysics`FeynArts`$FeynArts]];

(* ************************************************************** *)

(* Check if FeynArts is there and patch if wanted and needed *)

If[$FAPatch && $LoadFeynArts,
str = "" ;$patch=True;
If[FileNames["FeynArts.m", $FeynCalcDirectory] =!= {}, 
  strm = OpenRead[$FeynCalcDirectory <> $PathnameSeparator <> "FeynArts.m"];
  While[ToString[str] != "EndOfFile", str = Read[strm, String]; 
    If[StringMatchQ[ToString[str], "*Frederik Orellana*", IgnoreCase -> True], 
      $patch=False]];
  Close[strm]];

If[$patch, <<"HighEnergyPhysics`Phi`Extras`FAPatch`";
  HighEnergyPhysics`Phi`FAPatch`FAPatch]
];

(* ************************************************************** *)

(* Phi system variables *)

$Phi=True;

$PaletteConfiguration="None";

(* ************************************************************** *)

(**)
