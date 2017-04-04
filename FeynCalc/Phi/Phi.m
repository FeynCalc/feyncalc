(* ******************************************************** *)
(*                                                          *)
(*                          PHI                             *)
(*                                                          *)
(*                    Phenomenology of                      *)
(*                 Hadronic Interactions                    *)
(*                                                          *)
(* ******************************************************** *)


(* ******************************************************** *)

(* Title:    PHI *)

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

(* History:  Version 1.3 March 2003
						Version 1.2 May 2001
						Version 1.1 February 1999 *)

(* Contexts: Phi`,
						Phi`Objects`,
						Phi`Couplings`,
						Phi`Renormalization`,
						Phi`Channels`,
						Phi`Utilities`,
						Phi`Palettes`. *)

(* Package Version 1.3 *)

(* Mathematica Version >=3 *)

(* ******************************************************** *)

(* FeynCalc should be loaded *)

If[ !ValueQ[FeynCalc`$FeynCalcDirectory],
	Phi::nofc = "Before loading Phi, you should load FeynCalc. \
The kernel will exit now.";
	Message[Phi::nofc];
	Exit[0]
]

(* Get rid of old definitions *)

	tmp`pconf = Null;
	tmp`phi = Null;
	tmp`vv = Null;

If[ Phi`$Phi,
	tmp`pconf = Phi`$PaletteConfiguration;
	tmp`phi = Phi`$Phi;
	ClearAll["Phi`*`*`*"];
	ClearAll["Phi`*`*"];
	ClearAll["Phi`*"];
	Unprotect[$Packages];
	$Packages = Complement[$Packages,
	{"Phi`Objects`",
	"Phi`Palettes`", "Phi`Renormalization`",
	"Phi`Channels`", "Phi`Utilities`",
	"Phi`Couplings`", "Phi`"}];
	Protect[$Packages];
	$ContextPath = Complement[$ContextPath,
	{"Phi`Objects`",
	"Phi`Palettes`", "Phi`Renormalization`",
	"Phi`Channels`", "Phi`Utilities`",
	"Phi`Couplings`", "Phi`"}];
];

(* ************************************************************** *)

BeginPackage["Phi`"];

(* ************************************************************** *)

(* Usage definitions *)

$Phi::usage =
"$Phi is a variable indicating whether or not Phi is loaded - it is set \
to True when loading Phi.";

Global`$Configuration::usage =
"$Configuration is a string variable determining which configuration is loaded \
at startup or restart.  It can be set before loading FeynCalc, in PhiStart.m or with the \
configurations palette.  If the palette is used for restarting, the setting \
in PhiStart.m is overruled.  Possible values include \"ChPT2\" and \"QED\".  Default \
value : \"None\".";

$PaletteConfiguration::usage =
"$PaletteConfiguration is a string variable set when clicking on the \
configuration palette and overruling the setting of $Configuration.  Default \
value : \"None\".";

$HEPDir::usage =
"$HEPDir is a string variable specifying the full path to the parent \
directory of the directory FeynCalc (containing Phi). It should be set \
in the startup file First.m.  Default value : Directory[].";

PhiSymbols::usage =
"PhiSymbols[\"package\"] returns a list of symbols defined in the \
context package.";

$PhiVersion::usage=
"contains the version of PHI."

(* ************************************************************** *)

Begin["`Private`"];

End[];

(* ************************************************************** *)

EndPackage[];

(* ************************************************************** *)

(* Loading of subpackages *)

Phi`$HEPDir =
ParentDirectory[FeynCalc`$FeynCalcDirectory];

(*
$Path = Union[$Path,{Phi`$HEPDir}];

(*Give first priority to First.m and PhiStart.m in homedir. Added 28/8-2001*)
$Path = Join[FileNames[$HomeDirectory <> $PathnameSeparator <>
		".Mathematica" <> $PathnameSeparator <> "Applications"],
		FileNames[$HomeDirectory <> $PathnameSeparator <>
		".Mathematica" <> $PathnameSeparator <> "*" <> $PathnameSeparator <>
		"AddOns" <> $PathnameSeparator <> "Applications"],$Path];
*)

Get[FileNameJoin[{$FeynCalcDirectory,"Phi","First.m"}]]



listPhi = Map[ToFileName[{$FeynCalcDirectory,"Phi"}, (# <> ".m")] &,
			{"Objects", "Couplings", "Channels", "Utilities", "Renormalization", "Palettes"}];

FCDeclareHeader/@listPhi;
Get/@listPhi;


(* ************************************************************** *)

(* Defaults *)

If[ ValueQ[Global`$Configuration] =!= True,
	Global`$Configuration = "None"
];

If[ tmp`pconf=!=Null,
	$PaletteConfiguration = tmp`pconf
];
If[ tmp`phi=!=Null,
	$Phi = tmp`phi
];

$PhiVersion = "1.3";
(* ************************************************************** *)

(* Loading of user definitions *)

FCPrint[2,"Loading Phi`PhiStart`"];

Get[FileNameJoin[{$FeynCalcDirectory,"Phi","PhiStart.m"}]]

(*Clean out doubles (strings and non-strings)  in $Lagrangians*)
$Lagrangians = Union[(ToExpression[#[[0]]] @@ #) & /@ $Lagrangians];

If[ tmp`pconf=!=Null,
	$PaletteConfiguration = tmp`pconf
];
If[ tmp`phi=!=Null,
	$Phi = tmp`phi
];

(* ************************************************************** *)

(* Update particles *)

FAUpdate;

(* ************************************************************** *)

(* FeynArts definitions are cleared to avoid error messages *)
(*
If[ NumberQ[FeynArts`$FeynArts],
	ClearAll[FeynArts`Greek, FeynArts`UCGreek],
	Remove[FeynArts`$FeynArts]
];*)


(* ************************************************************** *)

(* Phi system variables *)

$Phi = True;

$PaletteConfiguration = "None";

(* ************************************************************** *)

(**)
