System`MyBeginPackage[a_,b___] :=
(NoPrint["MB ", a]; Hold[BeginPackage][a,b]//ReleaseHold);

System`MyEndPackage[] :=
(NoPrint["EE ", Context[]]; EndPackage[]);

(*     F E Y N C A L C 4.1.1 *)

HighEnergyPhysics`FeynCalc`$FeynCalcVersion = "4.1.1";

(* This software is GPL-ed, see:
    http://www.feyncalc.org/licence.txt
*)

(* ------------------------------------------------------------------------ *)
(* Clear all definitions.
   Added 21/9-2000 by F.Orellana to ease debugging *)
(* ------------------------------------------------------------------------ *)

If[MemberQ[$Packages,"HighEnergyPhysics`FeynCalc`"],

Block[{fcallpaths,fcallsymbols,rl,fcv,fcd},

   fcv = HighEnergyPhysics`FeynCalc`$FeynCalcVersion;
   fcd = HighEnergyPhysics`FeynCalc`$FeynCalcDirectory;
   fcvv = HighEnergyPhysics`FeynCalc`$VeryVerbose;

   If[HighEnergyPhysics`FeynCalc`$VeryVerbose>0,
      rl=True;
      If[$Notebooks===True,
         CellPrint[Cell[TextData[{"Clearing all definitions"}],
                  "Text"]],
         Print["Clearing all definitions"]]
   ];

  fcallpaths =
    Select[$ContextPath,
           (StringMatchQ[#, "HighEnergyPhysics`FeynCalc`*"] ||
            StringMatchQ[#, "HighEnergyPhysics`fctables`*"] ||
            StringMatchQ[#, "HighEnergyPhysics`fctables`*"] ||
            StringMatchQ[#, "HighEnergyPhysics`fctools`*"] ||
            StringMatchQ[#, "HighEnergyPhysics`general`*"] ||
            StringMatchQ[#, "HighEnergyPhysics`qcd`*"])&];

  fcallsymbols = (Join[StringJoin[#, "*"] & /@ #, StringJoin[#, "*`*"] & /@ #,
            StringJoin[#, "*`*`*"] & /@ #]) &[fcallpaths];

  ClearAll /@ fcallsymbols;

  Unprotect[$Packages];
  $Packages = Complement[$Packages, fcallpaths];
  Protect[$Packages];
  $ContextPath = Complement[$ContextPath, fcallpaths];

   If[rl===True,
      If[$Notebooks===True,
         CellPrint[Cell[TextData[{"Reloading FeynCalc"}],
                  "Text"]],
         Print["Reloading FeynCalc"]]
   ];

   HighEnergyPhysics`FeynCalc`$FeynCalcVersion = fcv;
   HighEnergyPhysics`FeynCalc`$FeynCalcDirectory = fcd;
   HighEnergyPhysics`FeynCalc`$VeryVerbose = fcvv;
];

];

(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)
(* Find out where HighEnergyPhysics is installed *)
(* ------------------------------------------------------------------------ *)

(* Added support for loading from "~/.Mathematica" and "." . This is not exactly the
   correct FileNames syntax and workds only under Unix. This is ok, since
   that's the only system under which $HomeDirectory is defined.
   Frederik Orellana, 31/7-2000 *)

If[!ValueQ[HighEnergyPhysics`FeynCalc`$FeynCalcDirectory],

   Which[
    (HighEnergyPhysics`FeynCalc`$FeynCalcDirectory =
    FileNames["FeynCalc.m",{Directory[]}]) != {},

    HighEnergyPhysics`FeynCalc`$FeynCalcDirectory = Directory[];
If[MemberQ[$Path,Evaluate[ParentDirectory[Directory[]]]]!=True,
$Path=Append[$Path,ParentDirectory[Directory[]]]],

    (HighEnergyPhysics`FeynCalc`$FeynCalcDirectory =
    FileNames["HighEnergyPhysics",{Directory[]}]) != {},

    HighEnergyPhysics`FeynCalc`$FeynCalcDirectory =
    HighEnergyPhysics`FeynCalc`$FeynCalcDirectory[[1]];
If[MemberQ[$Path,Evaluate[ParentDirectory[
HighEnergyPhysics`FeynCalc`$FeynCalcDirectory]]]!=True,
$Path=Append[$Path,ParentDirectory[HighEnergyPhysics`FeynCalc`$FeynCalcDirectory]]],

   (HighEnergyPhysics`FeynCalc`$FeynCalcDirectory =

   FileNames[$HomeDirectory <> $PathnameSeparator <>
    ".Mathematica" <> $PathnameSeparator <> "*" <> $PathnameSeparator <>
    "AddOns" <> $PathnameSeparator <> "Applications" <> $PathnameSeparator <>
    "HighEnergyPhysics"]) != {} &&
   FileNames[HighEnergyPhysics`FeynCalc`$FeynCalcDirectory[[1]]<>
   $PathnameSeparator <> "FeynCalc.m"] != {},

    HighEnergyPhysics`FeynCalc`$FeynCalcDirectory =
    HighEnergyPhysics`FeynCalc`$FeynCalcDirectory[[1]],

    True,

    HighEnergyPhysics`FeynCalc`$FeynCalcDirectory =
    ToFileName[{$TopDirectory, "AddOns", "Applications", "HighEnergyPhysics"}]

   ]
 ];

If[FileNames["*",{HighEnergyPhysics`FeynCalc`$FeynCalcDirectory}] == {},
   Print["Could not find FeynCalc installation.
Quitting the Mathematica kernel."];
   Quit[]; Exit[];
  ];
(* ------------------------------------------------------------------------ *)

  HighEnergyPhysics`FeynCalc`$ExcludeAutomaticDeclarePackageDirectories=
  {"tarcer",
   "Phi",".AppleDouble",
   "FeynArts","GraphInfo","Models","Documentation"};

HighEnergyPhysics`FeynCalc`Private`configfile= "FCConfig.m";


SetDirectory[HighEnergyPhysics`FeynCalc`$FeynCalcDirectory];
If[
FileNames[HighEnergyPhysics`FeynCalc`Private`configfile] =!= {},
 Get@HighEnergyPhysics`FeynCalc`Private`configfile;
  ];
ResetDirectory[];


(*  AUTHOR (rolfm@xs4all.nl) *)


 (* :Title: FeynCalc *)

 (* :Author: Rolf Mertig  (rolfm@xs4all.nl) *)

 (* :Summary: Tools and Tables *)
 (*           This file contains a lot of basic and small subpackages,
              all in the Context HighEnergyPhysics`FeynCalc`
 *)

 (* :Mathematica Version 3.0 or higher *)

 (* :History:
	Version 1.0 written 1991 by Rolf Mertig.
	Version 3.0 includes typesetting features of Mathematica 3.0
        Version 3.0.1.1 includes two bug-fixes for OneLoop
        Version 4.1.0 : reorganized for open-source and extensibility
 *)

If[($VersionNumber < 3.0),
   Print["You need Mathematica 3.0 to run FeynCalc 3.0.
          Quitting the Mathematica kernel."];
   Quit[]; Exit[];
  ];

(* From Mathematica 4.0 onwards there is  "Tr" functions;
   we'll just rename it in FeynCalc
*)

If[$VersionNumber>3.4,
   Unprotect@@{ToExpression["Tr"]};
   Remove@@{ToExpression["Tr"]},
Scan[ {Remove @@ Names["Global`"<>#], ToExpression["System`"<>#]}&,
  { "CommonDefaultFormatTypes" }];
  ];


savethisdir=Directory[];
 HighEnergyPhysics`FeynCalc`Private`feyncalchepdir =
HighEnergyPhysics`FeynCalc`$FeynCalcDirectory;
SetDirectory[HighEnergyPhysics`FeynCalc`Private`feyncalchepdir];


If[Global`$LoadTARCER === True,

SetDirectory["tarcer"];
If[StringQ[ Global`$LoadTARCER ],
   HighEnergyPhysics`FeynCalc`Private`tarcerfilenames =
   {Global`$LoadTARCER},
HighEnergyPhysics`FeynCalc`Private`tarcerfilenames =
FileNames["tarcer*.mx",IgnoreCase->True];
  ];
ResetDirectory[];

If[HighEnergyPhysics`FeynCalc`Private`tarcerfilenames=!={},

tarcerloadedflag = True;
If[Global`$FeynCalcStartupMessages=!=False,
If[$Notebooks ===True,
   CellPrint[Cell[TextData[{"Loading TARCER ",
HighEnergyPhysics`FeynCalc`Private`tarcerfilenames//Last}],
                  "Text"]],
   Print["Loading TARCER ",
HighEnergyPhysics`FeynCalc`Private`tarcerfilenames//Last]
  ];
Get[Last[HighEnergyPhysics`FeynCalc`Private`tarcerfilenames]];
  ];
Clear[HighEnergyPhysics`FeynCalc`Private`tarcerfilenames];,

If[$Notebooks ===True,
txt = {"WARNING! No TARCER*.mx file found. Please evaluate ",
        ButtonBox["the notebook TARCER.nb",
          ButtonFunction.NotebookOpen1[
              ToFileName[{HighEnergyPhysics`FeynCalc`$FeynCalcDirectory,
                  "tarcer"}, "TARCER.nb"]], ButtonStyle -> "Hyperlink",
          ButtonNote -> "Open the notebook TARCER.nb"],
        " or get one of the preprocessed files at ",
        ButtonBox["www.feyncalc.org/tarcer",
          ButtonData :> {URL["http://www.feyncalc.org/tarcer"], None},
          ButtonStyle -> "Hyperlink",
          ButtonNote -> "http://www.feyncalc.org/tarcer"]} /.
      Dot -> RuleDelayed /. NotebookOpen1 -> NotebookOpen; CellPrint[
  Cell[TextData[txt], "Text"]];Clear[txt,NotebookOpen1];,
   Print["WARNING! No TARCER*.mx file found. Please evaluate \
the notebook TARCER.nb or get one of the preprocessed files at \
http://www.feyncalc.org/tarcer"];
  ];
];

 ];

SetDirectory[savethisdir];
Clear[savethisdir];

 (* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)
 (* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)

  MyBeginPackage["HighEnergyPhysics`FeynCalc`"];

  CheckContext::usage=
  "CheckContext[string] yields True if the packaged associated with
	string is already loaded, and False otherwise.";

If[$VersionNumber<4.0,
FeynCalc::usage=
(*
"This is FeynCalc by Rolf Mertig (visit www.feyncalc.com).
*)
"For installation notes visit www.feyncalc.org\n
For a list of availabe objects type:    $FeynCalcStuff,
which contains a list of all functions and options in StringForm.
You can get on-line information by ?function, (e.g.; ?Contract). Eventually
you have to first enter the command function once (e.g., type Contract
and hit Return), which loads the necessary libraries, and the
?function (?Contract) will work.\n\n
There are several useful functions for short input, type $FCS for a list of
short commands. Then type, e.g., ?GA.\n \n
To get rid of the start-up messages put the line \n
$FeynCalcStartupMessages = False; \n
 into your init.m or the HighEnergyPhysics/FCConfig.m file.",
FeynCalc::usage=
"For installation notes visit www.feyncalc.org\n
For a list of availabe objects type:    $FeynCalcStuff,
which contains a list of all functions and options in StringForm.
You can get on-line information by ?function, e.g., ?Contract.\n
There are several useful functions for short input, type $FCS for a list of
short commands. Then type, e.g., ?GA.\n \n
To get rid of the start-up messages put the line \n
$FeynCalcStartupMessages = False; \n
 into your init.m or the HighEnergyPhysics/FeynCalcConfig.m file."
];

(*
F21::usage="F21 is an abbreviation for Hypergeometric2F1.";
*)
Li2::usage="Li2 is an abbreviation for the dilog function, i.e.,
Li2 = PolyLog[2,#]&.";
Li3::usage="Li3 is an abbreviation for the trilog function, i.e.,
Li3 = PolyLog[3,#]&.";

FI::usage=
"FI changes the output format to InputForm.
This is useful to see the internal representation of FeynCalc
objects. To change back to FeynCalcForm use FC.";

FC::usage=
"FC changes the output format to FeynCalcForm.
To change to InputForm use FI.";

(*
FCInstall::usage=
"After running the installation program fcinst.m from within Mathematica you
can customize your fc.m file. Of course you can also rename and replace
fc.m. Most of the source code of FeynCalc is located in the directory
FeynCalc in the HighEnergyPhysics directory. These functions are loaded
on demand; i.e., when you first invoke a function it may take some time
before everything is loaded. \n
$FeynCalcDirectory is set (in fc.m) to the installation directory.";
*)

  Load::usage=
  "Load[function] loads a function ( =
   Get[HighEnergyPhysics`FeynCalc`function`] )";

  MakeContext::usage=
  "MakeContext[string] constructs the context path of string.
   MakeContext is invoked at startup of FeynCalc.
	MakeContext[a, b] construct the context path of b defined
	in context a.";

SPC::usage=
"SPC is an abbreviation for ScalarProductCancel.";

SPL::usage=
"SPL is an abbreviation for SimplifyPolyLog.";

SubContext::usage=
"SubContext[fun] gives the sub-directory (context) in
 HighEnergyPhysics.";

$V0::usage="V0 is equivalent to $VeryVerbose = 0.";
$V3::usage="V0 is equivalent to $VeryVerbose = 3.";

  $AL::usage=
  "$AL is the head for dummy indices which may be introduced by
	Uncontract.";

$Color::usage=
"$Color is False by default. If set to True, some special variables
will be colored.";

$Covariant::usage =
"The boolean setting of $Covariant determines whether
lorentz indices are displayed as lower indices (True) or as
upper ones (False).";

(*$FeynCalc$::usage=
  "$FeynCalc$ is a list of all $-Variables in FeynCalc.";
*)
$FeynCalcStuff::usage=
  "$FeynCalcStuff is the list of availabe stuff in FeynCalc.";

$FeynCalcCreationDate::usage=
"$FeynCalcCreationDate gives the date this version of FeynCalc was created.";

$FeynCalcVersion::usage=
"$FeynCalcVersion is a string that represents the version of FeynCalc.";

$FCS::usage="FCS is a list of functions with a short name.
E.g. GA[nu] can be used instead of DiracGamma[nu]." ;

$FCT::usage="If $FCT is set to True special typesetting rules are
applied (FeynCalcTypesetting).";

$FortranContinuationCharacter::usage="$FortranContinuationCharacter
is the continuation character used in Write2.";

  $BreitMaison::usage=
"The Breitenlohner-Maison scheme is currently not supported.
Use Tracer if you need it.";

(*
  "The setting of $BreitMaison determines whether the Breitenlohner-
        Maison scheme is applied. If $BreitMaison=True, the so-called
        naive gamma5 prescription is used, i.e. gamma5 anticommutes in
        all dimensions.  The default is False. The setting should
        be chosen in the file FeynCalc.m BEFORE loading the package.
        Reversion during a session is not possible.";
*)

  $Kreimer::usage=
  "experimental setup of the Kreimer-scheme for Gamma5. Better don't use it.";

  $Larin::usage=
  "If set to True, the Larin-Gorishny-Atkyampo-DelBurgo-scheme for
        gamma5 in D-dimensions is used, i.e., before evaluating traces
        (but after moving gamma5 anticommuting in all dimensions to the
        right of the Dirac string) a product  gamma[mu].gamma5  is
        substituted to  -I/6 Eps[mu,al,be,si] gamma[al,be,si],
        where all indices live in D-dimensions now.
        Especially the Levic-Civita tensor is taken to be
        D-dimensional, i.e., contraction of two Eps's results in D's.
        This has (FOR ONE AXIAL-VECTOR-CURRENT ONLY, it is not so clear
        if this scheme also works for more than one fermion line
        involving gamma5) the same effect as the
        Breitenlohner-Maison-'t Hooft-Veltman scheme.";

  $LimitTo4::usage=
  "$LimitTo4 is a global variable with default setting True.
If set to False no limit Dimension -> 4 is
performed after tensor integral decomposition.";

$LorentzIndices::usage=
  "$LorentzIndices is a global variable. If set to True the dimension
of LorentzIndex is displayed as an index.";

  $MemoryAvailable::usage=
  "$MemoryAvailable is  a global variable which is set to an integer
        n, where n is the available amount of main memory in MB.
        The default is 128. It should be increased if possible.
        The higher $MemoryAvailable can be, the more intermediate
        steps do not have to be repeated by FeynCalc.";

  $MIntegrate::usage=
  "$MIntegrate is a global list of integrations done by Mathematica
    inside OPEIntDelta.";

$MomentumIndices::usage=
  "$MomentumIndices is a global variable. If set to True the dimension
of Momentum is displayed as an index.";

  $NonComm::usage=
  "$NonComm contains a list of all non-commutative heads present.";

  $MU::usage=
  "$MU is the head for dummy indices which may be introduced by
        Chisholm (and evtl. Contract and DiracReduce).";

  $NU::usage=
  "$NU is the head for dummy indices introduced by OPE2RHI.";

   $OPEWard::usage=
   "$OPEWard is experimental.";

  $PairBrackets::usage =
   "$PairBrackets determines whether brackets are drawn around " <>
   "scalar products in the notebook interface.";

  $SpinorMinimal::usage=
  "$SpinorMinimal is a global switch for an additional simplification
        attempt in DiracSimplify for more than one Spinor-line.
        The default is False, since otherwise it costs too much time.";

  $VeryVerbose::usage=
  "$VeryVerbose is a global variable with default setting 0.
If set to 1, 2, ..., less and more intermediate comments and informations
are displayed during calculations.";

  $West::usage=
  "If $West is set to True (which is the default),
traces involving more than 4 Dirac matrices
and gamma5 are calculated recursively according to formula (A.5) from
Comp. Phys. Comm 77 (1993) 286-298, which is based on the Breitenlohner
Maison gamma5 - scheme.";

  $AchmedRoss::usage= "experimental";

$Abbreviations::usage=
"$Abbreviations are a list of string substitution rules used by when \
generating names for storing intermediate results. \
It is used by OneLoop and PaVeReduce.\
The elements of the list should be of the form \"name\" -> \"abbreviation\".";

$Abbreviations = {", "->"","^"->"","{"->"", "/" -> "",
                  "Subscript"->"su","SmallVariable"->"sma",
                  "}"->"", "["->"", "]"->"", "*" -> "", " " -> "" ,
		  "\n" -> "", "\r" -> ""};


TBox::usage="TBox[a, b, ...] produces a RowBox[{a,b, ...}] where
a,b, ... are boxed in TraditionalForm.";

Tbox::usage="TBox[a, b, ...]  produces a RowBox[{a,b, ...}] where
a,b, ... are boxed in TraditionalForm.";

 (* ------------------------------------------------------------------- *)
  Begin["`Private`"]

 (* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)
 (* fix several Mma - functions first *)
(*
Unprotect[Hypergeometric2F1];
Hypergeometric2F1[a___,(n_Times)/;!FreeQ[n,Plus],c___]:=
  Hypergeometric2F1[a,Expand[n],c];
*)

(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)

(*
  $FeynCalc$ = {"$FeynCalc$", "$FeynCalcStuff",
                "$FeynCalcVersion",
                "$BreitMaison", "$Kreimer", "$Larin", "$LimitTo4",
                "$MemoryAvailable", "$MU", "$SpinorMinimal",
              "$VeryVerbose"};
*)

  $AchmedRoss = False;
  $BreitMaison =  False;
(* SetAttributes[$BreitMaison, Locked];*)
  $Color       = False;
  $Covariant = False;

  $FCS = {"FAD", "FV", "FVD", "GA", "GA5", "GS",
          "GSD", "LC", "LCD", "MT","MTD", "SD", "SOD",
          "SP", "SPC", "SPD", "SPL", "FCI", "FCE", "FI",
          "FC", "GGV", "GP", "QGV", "QO"
         };

  $FCT  = False;
  $FortranContinuationCharacter = "&";
If[!ValueQ[$Kreimer],  $Kreimer = False];
  $Larin   = False;
  $LimitTo4 = True;
  $LorentzIndices = False;
  $MemoryAvailable = 256;
  $MomentumIndices = False;
  $OPEWard = False;
  $PairBrackets = False;
  $MIntegrate = {};
  $SpinorMinimal = False;
  If[!ValueQ[$VeryVerbose],  $VeryVerbose   = 0];
(*
  $VeryVerbose   = 3;
*)
  $West          = True;

FI := (Format[LineBreak[_]]:= ""; $PrePrint=InputForm);
FC := (Format[LineBreak[_]]:= "\n";
       (If[!$Notebooks, $PrePrint= MakeContext["FeynCalcForm"],
               Unset[$PrePrint]])
      );
(*
V0 := $VeryVerbose = 0;
V3 := $VeryVerbose = 3;
*)
Li3 = PolyLog[3,#]&;
Li2 = PolyLog[2,#]&;
(*
F21 = Hypergeometric2F1;
*)
SPC := ToExpression["ScalarProductCancel"];
SPL := ToExpression["SimplifyPolyLog"];

  CheckContext[{x__String}] := MemberQ[$Packages,
   StringJoin@@Flatten[{"HighEnergyPhysics`",
   Map[SubContext[#] <> # <>"`"&,{x}]}]];

  CheckContext[x_String] := MemberQ[$Packages,
   StringJoin["HighEnergyPhysics`", SubContext[x] <> x, "`"]];

(*
  nonmulall =
  {"DiracGamma", "DiracMatrix", "DiracSlash",
   "ChiralityProjector", "Spinor", "DiracSpinor",
   "Partial", "QuantumField",
   "SUNT"};

  nonmul =
  {"DiracGamma", "DiracGammaT", "DiracMatrix", "DiracSlash",
   "ChargeConjugationMatrix",
   "ChargeConjugationMatrixInv",
   "ChiralityProjector",
   "Spinor",
   "DiracSpinor",
    "SUNT"
  };
*)
If[!ValueQ[$NonComm], $NonComm = {}];

(*Unprotect[MakeBoxes];*)
Unprotect[ToBoxes];

(*
 MyNeeds[x_String] := If[!MemberQ[$Packages, x], Needs[x]];
*)

 MyNeeds[x_String] := ( If[!MemberQ[$Packages, x], Needs[x]]);

  Load[x_Symbol] := Get["HighEnergyPhysics`FeynCalc`" <>
                        ToString[x] <> "`"];

  (* This is for the grouping of files in subdirectories in
     HighEnergyPhysics, like:  general, fctools, fctables, qcd
  *)
  SubContext[_String] := "FeynCalc`";

  MakeContext[x_String, y_String] :=
    If[SubContext[x] === "FeynCalc`",
    ToExpression["HighEnergyPhysics`FeynCalc`"<> x <>"`" <> y],
    (MyNeeds["HighEnergyPhysics`"<> SubContext[x] <> x <>"`"];
     ToExpression["HighEnergyPhysics`"<>SubContext[x]<>x<>"`"<>y]
     (*<>x<>"`" added 6/8-2000, F.Orellana*)
    )];

SetAttributes[MakeContext, HoldAll];

SetAttributes[SetDel, HoldAll];
SetDel[a_, b_] := (a := a = b);

  MakeContext[x_Symbol] := MakeContext[x] =
          With[{s=ToString[x]}, SetDel[x, Apply[MakeContext, {s}]]];

  MakeContext[x___Symbol] := Map[MakeContext, {x}];

  MakeContext[x_String] := MakeContext[x] = (If[SubContext[x] =!= "FeynCalc`",
             MyNeeds["HighEnergyPhysics`"<> SubContext[x] <> x <>"`"]];
             ToExpression["HighEnergyPhysics`"<>SubContext[x] <>
                        x <> "`" <> x]);

fcnames = Names["HighEnergyPhysics`FeynCalc`*"];

DPS[x__] := DPS[x] = DeclarePackage[x];

fcDeclarePackge[{x_, y_List}] := fcDeclarePackge[x, y];
fcDeclarePackge[{x_, y_ /;(Head[y]=!=List),z___}] :=
  fcDeclarePackge[x, {Last[Flatten[{x}]], y, z}];

fcDeclarePackge[x_, y_] :=
(*If[MemberQ[$Packages, "HighEnergyPhysics`FeynArts`"],
   If[!MemberQ[fcnames, x] &&
    (!CheckContext[x](* && FreeQ[multifunpack, x]*)),

       DPS[StringJoin["HighEnergyPhysics`", SubContext[x], x], y];

     ],*)
If[!CheckContext[x] (*&& FreeQ[multifunpack, x]*),
   DPS[StringJoin["HighEnergyPhysics`", SubContext[x], x, "`"], y]
  ];
(*  ];*)

fcDeclarePackge[x_String] := fcDeclarePackge[x, x];

DeclarePackage["HighEnergyPhysics`fctools`OneLoop`",{"OneLoopSum"}];

multifunpack=
{
{"Contract","Contract2", "Contract3"},
{"DiracSlash", "SL"},
{"DiracSimplify", "ChisholmSpinor"},
(*
{"FermionSpinSum", "SpinorCollect"},
*)
{"FeynAmpDenominatorSimplify", "FDS"},
{"FeynCalcForm", "FCF"},
{"FeynCalcInternal", "FCI"},
{"FeynCalcExternal", "FCE"},
{"GluonGhostVertex", "GGV"},
{"Twist2GluonOperator", "GO"},
{"GluonPropagator", "GP"},
{"GhostPropagator", "GHP"},
{"GluonVertex", "GV"},
{"QuarkGluonVertex", "QGV"},
{"QuarkPropagator", "QP"},
{"Twist2QuarkOperator", "QO"},
{"FeynAmpDenominator", "FD"},
{"PropagatorDenominator", "PD"},
(*
{"PropagatorDenominatorSign", "PDS"},
*)
{"Write2", "FUNCTION", "PostFortranFile", "PreFortranFile"}
};


 feyncalcdir =  HighEnergyPhysics`FeynCalc`$FeynCalcDirectory;

   SetDirectory[feyncalcdir];

(* get all the directories (like general, qcd, fctools, fctables *)
hepdirs = Select[FileNames[], FileType[#]===Directory&];

(* fix for Mac OS *)
If[StringMatchQ[$OperatingSystem, "*MacOS*"],
  hepdirs = If[StringMatchQ[#, ":*"], StringDrop[#, 1], #] & /@ hepdirs];

hepdirs = Complement[ hepdirs,
      HighEnergyPhysics`FeynCalc`$ExcludeAutomaticDeclarePackageDirectories];

   declarepackagelist ={};

Do[
    SetDirectory[hesubdir = hepdirs[[i]]];
    fils   = FileNames[{"*.m", "*.mx"}];
    If[fils =!= {},
       dotmfiles = StringReplace[fils,
             {".m" -> "",".mx"->"", $PathnameSeparator->""} ];
       (Hold[Set][Hold[SubContext][#], hesubdir <> "`"]& /@ dotmfiles) //
       ReleaseHold;
       filenams = Select[dotmfiles, FreeQ[multifunpack, #]&];
       declarepackagelist = Join[declarepackagelist, filenams];
      ];
       ResetDirectory[],
       {i, Length[hepdirs]}
   ];


   declarepackagelist = Join[declarepackagelist, multifunpack];

(*
   filenams= StringReplace[FileNames[{"*.m","*.mx"}],
             {".m" -> "",".mx"->"", $PathnameSeparator->""} ];
*)

   ResetDirectory[];

   declarepackagelist = Join[declarepackagelist, multifunpack];
   $FeynCalcStuff = Union[Flatten[declarepackagelist]];

TBox[] = "\[Null]";
TBox[a_] := ToBoxes[a, TraditionalForm];
Tbox[]    = "\[Null]";
Tbox[a_] := totr[a];

TBox[a_,b__] := RowBox @ Map[(ToBoxes @@ {#, TraditionalForm})&, {a, b}];

(*
totr[y_Symbol] := If[MemberQ[$FeynCalcStuff, ToString[y]],
                    ToBoxes[y, TraditionalForm], y];
*)

totr[Subscript[y_,in__Integer]] := SubscriptBox[totr[y],RowBox[{in}]];

totr[y_Symbol] := If[FormatValues[Evaluate[y]] === {},
                     ToString[y],
                     ToBoxes[y, TraditionalForm], y];

totr[y_String] := y;
totr[y_] := ToBoxes[y, TraditionalForm] /; Head[y]=!=Symbol;

(*
Tbox[a__] := RowBox @@ {Map[totr, {a}]};
*)
(*
Unprotect[Insert];
SetAttributes[Insert, SequenceHold];
*)

(* somehow \[NoBreak] does not really work, but it does
the spacing "right" ...*)

(*
Tbox[a__] := RowBox @@ {Map[totr, {a}]};
*)
Tbox[a_,b__] :=
(RowBox @ (Insert[
  Map[totr, {a,b}], "\[NoBreak]",
    Array[{#}&,Length[{a,b}]-1,2]]));

(*
Tbox[a__] :=
(RowBox @ (Insert[
  Map[ToBoxes [#, TraditionalForm]&, {a}], Global`UNNoBreak,
    Array[{#}&,Length[{a}]-1,2]]));
*)

(*
(* a la Robby *)
SetAttributes[Tbox, HoldAllComplete];
Tbox[a_] := MakeBoxes[a, TraditionalForm];
Tbox[a_, b__] := RowBox @ Map[Function[expr, MakeBoxes[expr, TraditionalForm],
  HoldAllComplete], Unevaluated[{a, b}] ]
Tbox[a_, b__] := RowBox @ Thread @ Unevaluated[MakeBoxes[{a, b}, TraditionalForm
]]
*)

Unprotect[Dot];
Dot /:
MakeBoxes[Dot[a__], TraditionalForm] := (
ClearAttributes[Times, Orderless];
embo = MakeBoxes[Times[a], TraditionalForm];
SetAttributes[Times, Orderless];
embo ) /; $FCT === True;

(*
Unprotect[Times];
Times/:
MakeBoxes[HoldPattern[Times[factors___]] /;
  MemberQ[Unevaluated[{factors}], _Dot],
  TraditionalForm
] :=
  Replace[
    Flatten[#, Infinity, Times]& @
      (Flatten[#, Infinity, Dot]&) @
        HoldComplete[factors],
    HoldComplete[operands___]
    :>
    (InterpretationBox[#, Times[factors]]& @
      MakeBoxes[Times[operands], TraditionalForm])
  ] /; $FCT === True;
*)

  End[];
MyEndPackage[];


Map[HighEnergyPhysics`FeynCalc`Private`fcDeclarePackge,
    HighEnergyPhysics`FeynCalc`Private`declarepackagelist];

(* take care of SubContext of the multifunpack values *)

tab =
(
Table[ Map[ setsubcontext[#, HighEnergyPhysics`FeynCalc`Private`multifunpack[[i,1]] ]&,
        Rest[ HighEnergyPhysics`FeynCalc`Private`multifunpack[[i]] ] ],
       {i, Length[HighEnergyPhysics`FeynCalc`Private`multifunpack]}
     ] /. setsubcontext[a_String,b_String] :>
            {Hold[Set][Hold[SubContext][a], Hold[SubContext][b]],
             Hold[Set][Hold[MakeContext][a], Hold[MakeContext][b]]}
) ;
(*
Global`TT=tab;
*)

ReleaseHold[tab];


(*If[HomeDirectory[] === "/home/rolfm", Print["declaring done ",TimeUsed[]]];*)


 (* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)

 (* ------------------------------------------------------------------- *)

If[Global`$FeynCalcStartupMessages =!= False ,
If[$Notebooks===True,
   CellPrint[Cell[TextData[{StyleBox[ "FeynCalc" , FontWeight-> "Bold"], " ",
    $FeynCalcVersion,
     "    ", " Evaluate ?FeynCalc for help or visit ",
     ButtonBox["www.feyncalc.org", ButtonData:>{
      URL[ "http://www.feyncalc.org"], None},
     ButtonStyle->"Hyperlink", ButtonNote->"http://www.feyncalc.org"]}
    ],"Text"]]
,
  WriteString["stdout", "\nFeynCalc" <> $FeynCalcVersion ,
              " Type ?FeynCalc for help or visit http://www.feyncalc.org", "\n"];
];
  ];
If[$Notebooks===True && (!StringMatchQ[$Version, "*1996*"]),
   feversion =
(LinkWriteHeld[$ParentLink,Hold[FrontEnd`Value[
  FrontEnd`$FullVersion]]]; LinkRead[$ParentLink]),
  feversion="Failed"
  ];

If[!StringMatchQ[$Version, "*1996*"] &&
   (!StringMatchQ[feversion,"*3.0.0*"]) &&
   ($Notebooks === True),
SetOptions[$FrontEnd, "CommonDefaultFormatTypes"->{"Output" -> TraditionalForm}]
,
Null
(*
If[($Notebooks === True) && (Global`$FeynCalcStartupMessages =!= False),
   If[(("Output" /.
         (  Flatten[
           {Options[$FrontEnd, "CommonDefaultFormatTypes"]}
                           ][[1,2]] )
       )    ) =!= TraditionalForm
      ,
      CellPrint[ Cell[TextData[{ "Please click on the ", StyleBox["Cell",
                      FontWeight->"Bold"], " menu, then go to the ",
                      StyleBox["Default Output Format Type ",
                      FontWeight->"Bold"], "item and switch to ",
                      StyleBox["TraditionalForm", FontWeight->"Bold"], "."
                               }], "Text"] ]]
  ];
*)
];
Clear[feversion];

(*
Times/:MakeBoxes[Times[x__],TraditionalForm] :=
(	MakeBoxes[Times2[x],TraditionalForm]/;
     ((!FreeQ[{x},Dot] )&&(Length[{x}]<6)) &&
     (HighEnergyPhysics`FeynCalc`$FCT === True) &&
(* one factor at least has to have Head Dot *)
     MemberQ[Head/@{x}, Dot]
);
Times2/:
	MakeBoxes[Times2[before___, dot_Dot,
                      after___], TraditionalForm] :=
  Replace[
    Flatten[#, Infinity, Times]& @
      (Flatten[#, Infinity, Dot]&) @
        HoldComplete[
          before,
          dot,
          after],
    HoldComplete[operands___]
    :>
    Apply[InterpretationBox,{
      MakeBoxes[Times[operands], TraditionalForm],
      Times[before,
            dot,
            after]}]
		] ;
*)
(*
Unprotect[ Equal];
Format[ HoldPattern[Equal][ x___], TraditionalForm] :=
    Infix[ Equal[x], " = "] /;
 HighEnergyPhysics`FeynCalc`$FCT === True;
*)

(* special case *)

(*
MakeBoxes[Log[x_], TraditionalForm] :=
     Tbox["\[ScriptL]","n","(",x,")"] /;
  HighEnergyPhysics`FeynCalc`$FCT === True;
*)
(*
MakeBoxes[Log[x_], TraditionalForm] :=
     MakeBoxes[ln[x],TraditionalForm] /;
 HighEnergyPhysics`FeynCalc`$FCT === True;
*)


(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: A *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Gauge field *)

(* ------------------------------------------------------------------------ *)

(*
MyBeginPackage["HighEnergyPhysics`FeynCalc`A`",
             "HighEnergyPhysics`FeynCalc`"];

A::usage =
"A is the name of a gauge field. The default setting of
FieldStrength for QuantumField is A.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "A | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
*)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: A0ToB0 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`A0ToB0`",
             "HighEnergyPhysics`FeynCalc`"];

A0ToB0::usage =
"A0ToB0 is an option for A0. If set to True, A0[m^2] is expressed
by (1+ B0[0,m^2,m^2])*m^2.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[];
MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "A0ToB0 | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Abbreviation *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Abbreviation`",
             "HighEnergyPhysics`FeynCalc`"];

Abbreviation::usage=
"Abbreviation[name] gives a shortname for name (in HoldForm).
E.g.: Abbreviation[QuarkPropagator] --> HoldForm[QP].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[];
MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Abbreviation | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Anti5 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Anti5 is the head of Levi-Civita Tensor *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Anti5`",
             "HighEnergyPhysics`FeynCalc`"];

Anti5::usage =
"Anti5[exp] anticommutes all gamma5 one time to the right.
Anti5[exp, n] anticommutes all gamma5 n times to the right.
Anti5[exp, -n] anticommutes all gamma5 n times to the left.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[DiracGamma, DOT, FeynCalcInternal, MemSet];

Anti5[a_/;FreeQ[a, DiracGamma[5]],_] := a;
Anti5[x_, Infinity] := FixedPoint[Anti5, x, $RecursionLimit];
Anti5[xx_,n_Integer?Positive] := Nest[Anti5, xx, n];

Anti5[xx_] := (FeynCalcInternal[xx] /. DOT -> doot) /.
            If[$BreitMaison =!= True,
             {doot[a___, DiracGamma[5], DiracGamma[y_[x__], di___], b___] :>
              (-doot[a,DiracGamma[y[x],di],DiracGamma[5],b])
             },
             {doot[a___, DiracGamma[5], DiracGamma[y_[x_]], b___] :>
              (-doot[a,DiracGamma[y[x]],DiracGamma[5],b])
              ,
              doot[a___, DiracGamma[5], DiracGamma[y_[x_,di_Symbol],
                                                           di_Symbol
                                                    ],
                     b___
                    ] :>
              (-doot[a,DiracGamma[y[x], di], DiracGamma[5], b] +
                2 doot[a,DiracGamma[y[x,di-4],di-4],DiracGamma[5],b]
              )
             }
                ] /.doot[a___, DiracGamma[5], DiracGamma[5],b___
                        ] :> doot[a,b] /. doot -> DOT;

Anti5[xx_,-1] :=
           (FeynCalcInternal[xx] /. DOT -> doot) /.
             {doot[a___, DiracGamma[y_[x__], di___],
                         DiracGamma[5],
                  b___] :>
              (-doot[a, DiracGamma[5], DiracGamma[y[x],di], b])
             } /. doot -> DOT

(*
Anti5[xx_] := MemSet[Anti5[xx],
                     (FeynCalcInternal[xx] /. DOT -> doot) /.
             {doot[a___, DiracGamma[5],
                        DiracGamma[y_[x__], di___],
                  b___] :>
              (-doot[a,DiracGamma[y[x],di],DiracGamma[5],b])
             } /. doot -> DOT];
Anti5[xx_,-1] := MemSet[Anti5[xx,-1],
           (FeynCalcInternal[xx] /. DOT -> doot) /.
             {doot[a___, DiracGamma[y_[x__], di___],
                         DiracGamma[5],
                  b___] :>
              (-doot[a, DiracGamma[5], DiracGamma[y[x],di], b])
             } /. doot -> DOT];
*)

Anti5[xx_,n_Integer?Negative] := Nest[Anti5[#,-1]&, xx, -n] /; n <(-1);
Anti5[x_, -Infinity] := Anti5[x, -$RecursionLimit];
Anti5[x_, -Infinity] := FixedPoint[Anti5[#,-1]&, x,
                                   $RecursionLimit];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Anti5 | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: AntiCommutator *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`AntiCommutator`",
             "HighEnergyPhysics`FeynCalc`"];

AntiCommutator::usage=
"AntiCommutator[x, y] = c  defines the anti-commutator of the
 non-commuting objects x and y.
Settings of AntiCommutator (e.g.AntiCommutator[a,b]=c)
are recognized by DotSimplify.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];




MakeContext[DataType, NonCommutative];

AntiCommutator /: Set[AntiCommutator[a_, b_] , c_] := Block[{nd, acom},
                  nd = (RuleDelayed @@ {HoldPattern @@ {acom[a, b]}, c}
                        ) /. acom -> AntiCommutator ;
                If[FreeQ[DownValues[AntiCommutator], nd],
                    PrependTo[DownValues[AntiCommutator], nd]
                  ];
                                                          c];
  AntiCommutator /:
   MakeBoxes[
    AntiCommutator[a_, b_], TraditionalForm
            ] := Tbox["{", a, ",", "\[MediumSpace]", b, "}"];

End[]; MyEndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "AntiCommutator | \n "]];
Null
(* :Title: AntiQuarkField *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`AntiQuarkField`",
             "HighEnergyPhysics`FeynCalc`"];

AntiQuarkField::usage =
"AntiQuarkField is the name of a fermionic field.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

AntiQuarkField /: MakeBoxes[AntiQuarkField, TraditionalForm] :=
  OverscriptBox["\[Psi]","_"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "AntiQuarkField | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Bracket *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Bracket`",
             "HighEnergyPhysics`FeynCalc`"];

Bracket::usage= "Bracket is an option for Convolute.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Bracket | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: CA *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: CA = the N of SU(N) *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`CA`",
             "HighEnergyPhysics`FeynCalc`"];


CA::usage=
"CA is one of the Casimir operators of SU(N); CA = N";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

CA /:
   MakeBoxes[
             CA, TraditionalForm
            ] := SubscriptBox["C", "A"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "CA | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: CF *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: CF *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`CF`",
             "HighEnergyPhysics`FeynCalc`"];

CF::usage=
"CF is one of the Casimir operators of SU(N); CF = (N^2-1)/(2 N)";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

CF /:
   MakeBoxes[
             CF, TraditionalForm
            ] := SubscriptBox["C", "F"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "CF | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Cases2 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Cases2`",
             "HighEnergyPhysics`FeynCalc`"];

Cases2::usage=
"Cases2[expr, f] is equivalent to
Cases[{expr}, HoldPattern[f[___]], Infinity]//Union.
Cases2[expr, f1, f2, ...] or
Cases2[expr, {f1, f2, ...}] is equivalent to
Cases[{expr}, f1[___] | f2[___] ..., Infinity]//Union.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


Options[Cases2] = {Heads -> False};

Cases2[expr_, {f___}, opts___Rule] := Cases2 @@ Prepend[{f,opts}, expr];
If[$VersionNumber >2.2,
   Cases2[expr_, f_, opts___Rule] := Union[Cases[{expr}, HoldPattern[f[___]],
                                           Infinity,opts]]
   ,
   Cases2[expr_, f_,opts___Rule] := Union[Cases[{expr}, HoldPattern[f[___]],
                                          Infinity,opts]]
  ];
Cases2[expr_, f___, g_] := Union[Cases[{expr},
                            Alternatives@@(#[___]&/@{f,g}),Infinity]
                   ] /; Head[g] =!= Rule;

Cases2[expr_, f__, Heads->True] := Union[Cases[{expr},
              Alternatives@@(#[___]&/@{f,g}),Infinity,Heads->True]];

Cases2[expr_, f__, Heads->False] := Union[Cases[{expr},
              Alternatives@@(#[___]&/@{f,g}),Infinity,Heads->False]];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Cases2 | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)


(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Collecting *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Collecting *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Collecting`",
             "HighEnergyPhysics`FeynCalc`"];

Collecting::usage =
"Collecting is an option of Contract2, ScalarProductCancel, SquareAmplitude,
Series2, TID and related functions. Setting it to True will trigger
some kind of collecting of the result.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Collecting | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Commutator *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Commutator`",
             "HighEnergyPhysics`FeynCalc`"];

Commutator::usage=
"Commutator[x, y] = c  defines the commutator between the non-commuting
objects x and y.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];




MakeContext[DataType, NonCommutative];

Commutator /: Set[Commutator[a_, b_] , c_] := Block[{nd, com},
                   nd = (RuleDelayed @@ {HoldPattern @@ {com[a, b]}, c}
                        ) /. com -> Commutator ;
                If[FreeQ[DownValues[Commutator], nd],
                   PrependTo[DownValues[Commutator], nd]
                  ];
                  c];


   Commutator/:
   MakeBoxes[Commutator[a_, b_],
             TraditionalForm
            ] := RowBox[ {"[","\[NoBreak]", Tbox[a] ,"\[NoBreak]", ",",
                          Tbox[b], "\[NoBreak]", "]"}];

End[]; MyEndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Commutator | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: CommutatorExplicit *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`CommutatorExplicit`",
             "HighEnergyPhysics`FeynCalc`"];

CommutatorExplicit::usage=
"CommutatorExplicit[exp] substitutes any Commutator and AntiCommutator
in exp by their definitions.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[
AntiCommutator,
Commutator,
DOT
];

CommutatorExplicit[exp_] := exp /.
   {Commutator :> ((DOT[#1, #2] - DOT[#2, #1])&),
    AntiCommutator :> ((DOT[#1, #2] + DOT[#2, #1])&)
   };

End[]; MyEndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "CommutatorExplicit | \n "]];
Null
(* :Title: ComplexIndex *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: The head for complex conjugated indices *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`ComplexIndex`",
             "HighEnergyPhysics`FeynCalc`"];

ComplexIndex::usage=
"ComplexIndex is the head of a complex conjugate index.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


ComplexIndex[ComplexIndex[x_]] := x;

   ComplexIndex /:
   MakeBoxes[ComplexIndex[x_] ,TraditionalForm] :=
   SuperscriptBox[Tbox[x], "*"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ComplexIndex | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: CounterT*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`CounterT`",
             "HighEnergyPhysics`FeynCalc`"];

CounterT::usage= "CounterT is an option for several Feynman rule
functions.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "CounterT | \n "]];
Null

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: CouplingConstant *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: CouplingConstant *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`CouplingConstant`",
             "HighEnergyPhysics`FeynCalc`"];

CouplingConstant::usage =
"CouplingConstant is an option for several Feynman rule fucntions and
for CovariantD and FieldStrength.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "CouplingConstant | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DOT *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: DOT *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DOT`",
             "HighEnergyPhysics`FeynCalc`"];

DOT::usage =
"DOT[a, b, ...] is the FeynCalc function for non-commutative
multiplication. By default it is set to the Mathematica Dot
functions. By setting  \n
(DOT=.) \n
this can be disabled.
Note that then non-commutative products should to be entered
like DOT[ DiracMatrix[mu], m + DiracSlash[p], DiracMatrix[mu] ],
etc.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

DOT = Dot;

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DOT | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DataType *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: DataType  is just a datatype *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DataType`",
             "HighEnergyPhysics`FeynCalc`"];

DataType::usage=
"DataType[exp, type] = True   defines the object exp to have datatype
type.
DataType[exp1, exp2, ..., type] defines the objects exp1, exp2, ... to
have datatype type.
The default setting is DataType[__, _]:=False.
To assign a certain data type, do e.g.:
DataType[x, PositiveInteger] = True.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"]


MakeContext[NonCommFreeQ, SelectFree];
noncommutative := noncommutative = MakeContext["NonCommutative"];


DataType[_] := soso /; Message[DataType::argrx, DataType, 1, "2 or more"];
DataType[] := soso /; Message[DataType::argrx, DataType, 0, "2 or more"];

DataType /: HoldPattern[Set[DataType[a_, b__,type_], bool_]] :=
            Map[set[dt[#, type], bool]&, {a, b}] /. {set:>Set,dt:>DataType};

DataType[a_, b__, type_] := Flatten[{DataType[a, type], DataType[b, type]}];

(* special rules for NonCommutative *)

DataType /: HoldPattern[Set[DataType[exp_,
            HighEnergyPhysics`FeynCalc`NonCommutative`NonCommutative],
            True]] :=
 Block[{ndt, ndf, dt, ncq, nnn, nnt, set, downvalues},
              If[!MemberQ[$NonComm, exp], AppendTo[$NonComm, exp]];
               ndt = (RuleDelayed @@ {HoldPattern @@
                       {dt[exp, noncommutative]}, True}
                     ) /. dt -> DataType;
               ndf = (RuleDelayed @@ {HoldPattern @@
                       {dt[exp, noncommutative]}, False}
                     ) /. dt -> DataType;
        If[FreeQ[DownValues[DataType], ndt],
           DownValues[DataType] =
           Prepend[SelectFree[DownValues[DataType], ndf], ndt]
          ];
            nnt = (RuleDelayed @@ {HoldPattern @@ {ncq[exp]}, False}
                  ) /. ncq -> NonCommFreeQ;
            set[downvalues[NonCommFreeQ],Prepend[
                SelectFree[DownValues@@{NonCommFreeQ}, exp], nnt
                                                ]
              ] /. {set :> Set, downvalues :> DownValues};
  True];


DataType /: HoldPattern[Set[DataType[exp_,
            HighEnergyPhysics`FeynCalc`NonCommutative`NonCommutative],
            False]] :=
 Block[{ndt, ndf, dt, ncq, nnn, nnt, set, downvalues},
              If[MemberQ[$NonComm, exp],
                  $NonComm = SelectFree[$NonComm, exp];
                 ];
               ndt = (RuleDelayed @@ {HoldPattern @@
                      {dt[exp, noncommutative]}, True}
                     ) /. dt -> DataType;
               ndf = (RuleDelayed @@ {HoldPattern @@
                      {dt[exp, noncommutative]}, False}
                     ) /. dt -> DataType;
        If[FreeQ[DownValues[DataType], ndf],
           DownValues[DataType] =
           Prepend[SelectFree[DownValues[DataType], ndt], ndf]
          ];
                       nnn = (RuleDelayed @@ {HoldPattern @@
                   {ncq[exp]}, _}
                  ) /. ncq -> NonCommFreeQ;
           If[!FreeQ[DownValues[NonCommFreeQ], nnn],
              DownValues[NonCommFreeQ] =
              SelectFree[DownValues[NonCommFreeQ], nnn]
             ];
            nnt = (RuleDelayed @@ {HoldPattern @@ {ncq[exp]}, True}
                  ) /. ncq -> NonCommFreeQ;
            set[downvalues[NonCommFreeQ],Prepend[
                SelectFree[DownValues@@{NonCommFreeQ}, exp], nnt
                                                ]
              ] /. {set :> Set, downvalues :> DownValues};
 False];

HoldPattern[DataType[__, _]] := False;

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DataType | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DeclareNonCommutative *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: test for non-commutativity *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DeclareNonCommutative`",
             "HighEnergyPhysics`FeynCalc`"];

DeclareNonCommutative::usage =
"DeclareNonCommutative[a, b, ...] declares a,b, ... to be
noncommutative, i.e., DataType[a,b, ...,  NonCommutative] is set to
True.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


(*
DataType                 = MakeContext["DataType"];
NonCommutative           = MakeContext["NonCommutative"];
*)

DeclareNonCommutative[] := soso /;
Message[DeclareNonCommutative::argrx, DeclareNonCommutative, 0, "1 or more"];

DeclareNonCommutative[b__] :=
 (Map[Set[HighEnergyPhysics`FeynCalc`DataType`DataType[#,
          HighEnergyPhysics`FeynCalc`NonCommutative`NonCommutative],
          True]&, {b}
     ]; Null);

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DeclareNonCommutative | \n "]];
Null
(* :Title: DeltaFunction *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  Dirac-delta function  (just a name) *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DeltaFunction`",
             "HighEnergyPhysics`FeynCalc`"];

DeltaFunction::usage= "DeltaFunction is the Dirac delta-function.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

(*Added 18 April 2001, Frederik Orellana*)
DeltaFunction[_?((NumericQ[#]===True&&(Positive[#]===True||Negative[#]===True))&)]:=0;
DeltaFunction[0]:=1;

DeltaFunction /:
   MakeBoxes[ DeltaFunction[y_], TraditionalForm] :=
    RowBox[{"\[Delta]", "(", Tbox[y], ")"}];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DeltaFunction | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DeltaFunctionPrime*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  Dirac-delta function derivative (just a name) *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DeltaFunctionPrime`",
             "HighEnergyPhysics`FeynCalc`"];

DeltaFunctionPrime::usage=
"DeltaFunctionPrime denotes the derivative of the Dirac delta-function.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


DeltaFunctionPrime /:
   MakeBoxes[ DeltaFunctionPrime[y_], TraditionalForm] :=
    RowBox[{SuperscriptBox["\[Delta]","\[Prime]"],
           "(", Tbox[y], ")"}
          ];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DeltaFunctionPrime | \n "]];
Null


(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DeltaFunctionDoublePrime*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 10 July '98 at 12:05 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  Dirac-delta function double derivative (just a name) *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DeltaFunctionDoublePrime`",
             "HighEnergyPhysics`FeynCalc`"];

DeltaFunctionDoublePrime::usage=
"DeltaFunctionDoublePrime denotes the second derivative of the
Dirac delta-function.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


DeltaFunctionDoublePrime /:
   MakeBoxes[ DeltaFunctionDoublePrime[y_], TraditionalForm] :=
    RowBox[{SuperscriptBox["\[Delta]","\[DoublePrime]"],
           "(", Tbox[y], ")"}
          ];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DeltaFunctionDoublePrime | \n "]];
Null

(* :Title: Dimension *)

(* :Author: Rolf Mertig *)


(* :Summary: Dimension is an option for several functions *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Dimension`",
               "HighEnergyPhysics`FeynCalc`"];

Dimension::usage =
"Dimension is an option for DiracMatrix, DiracSlash, FourVector,
LeviCivita, MetricTensor, OneLoop and ScalarProduct.
The default setting is sometimes 4, sometimes D.
The setting should always be 4, a symbol (D, n, ...), or
(D-4), (n-4), ... .";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DimensionalReduction *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DimensionalReduction`",
             "HighEnergyPhysics`FeynCalc`"];

DimensionalReduction::usage= "DimensionalReduction is an option
for TID and OneLoopSimplify.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DimensionalReduction | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracBasis *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: DiracBasis is just a auxiliary head for Dirac structures
*)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DiracBasis`",
             "HighEnergyPhysics`FeynCalc`"];

DiracBasis::usage =
"DiracBasis[any] is a head which is wrapped around Dirac structures
(and the 1) as a result of the function DiracReduce.
Eventually you want to substitute DiracBasis by Identity (or
set: DiracBasis[1] = S; DiracBasis[DiracMatrix[mu]] = P; etc.).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracBasis | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracCanonical *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DiracCanonical`",
             "HighEnergyPhysics`FeynCalc`"];

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

DiracCanonical::usage=
"DiracCanonical is an option for DiracSimplify.
If set to True DiracSimplify uses the function DiracOrder
internally.";

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracCanonical | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracEquation *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)

(* :Summary: Dirac equation application; not fully *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DiracEquation`",
             "HighEnergyPhysics`FeynCalc`"];

DiracEquation::usage=
"DiracEquation[exp] applies the Dirac equation without
expanding exp. If that is needed, use DiracSimplify.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[ DiracGamma, FreeQ2, LorentzIndex, Momentum, Spinor, Pair,
 PairContract];

dot := dot               = MakeContext["DOT"];
diractrick := diractrick = MakeContext["DiracTrick"];
dotsimplify:=dotsimplify = MakeContext["DotSimplify"];
expanding:=expanding     = MakeContext["Expanding"];
fci := fci               = MakeContext["FeynCalcInternal"];

DiracEquation[x_]:=(*DiracEquation[x]=*)
    dotsimplify[diraceq[x//fci], expanding -> False];
(* for only internal use *)
DiracEquation[x_,I]:=(*DiracEquation[x]=*)
    dotsimplify[diraceq[x], expanding -> False];

    last[n_. Momentum[pe__]]:=Momentum[pe];
    last[x_Plus]:=PowerExpand[Sqrt[Last[x]^2]];

   diraceq[x_]:=x/;FreeQ[x,Spinor];
   diraceq[x_] := Expand[ x//.spCDieqRules, dot ];

   spCDieqRules = {
    doot_[ z___,Spinor[n_. Momentum[p_] + k_. ,m_, op___],
           DiracGamma[Momentum[p_,___],___],a___
         ] :>(m/n doot[ z,Spinor[n Momentum[p] + k,m,op ],a ] -
             If[(k===0), 0 ,
                If[last[n Momentum[p] + k] =!= Momentum[p],0,
                   1/n doot[ z, Spinor[n Momentum[p] + k,m,op ],
                             DiracGamma[k], a ]
                  ]
               ]
             )/; last[n Momentum[p]+k]===Momentum[p],

    doot_[ a___,DiracGamma[Momentum[p_,___],___],
          Spinor[n_. Momentum[p_] + k_. ,m_,op___],z___
         ]  :>(m/n doot[ a,Spinor[ n Momentum[p] + k,m,op ],z ] -
              If[(k===0), 0 ,
                If[last[n Momentum[p] + k] =!= Momentum[p],0,
                   1/n doot[ a, DiracGamma[k],
                                Spinor[n Momentum[p] + k,m,op ],
                             z ]
                  ]
                ]
              ) /; last[n Momentum[p]+k]===Momentum[p],

    doot_[ a___,DiracGamma[Momentum[y__],___],
           DiracGamma[Momentum[y__],___],b___
         ] :> scev[Momentum[y],Momentum[y]] doot[a,b],

    doot_[ z___,Spinor[n_. Momentum[p_] + k_. ,m_,op___],a___,
           DiracGamma[x_[y__],di___],
           DiracGamma[Momentum[p_,dim___],dim___],b___
         ] :> If[!FreeQ2[{a}, {DiracGamma[5], DiracGamma[6],
                             DiracGamma[7]}],
                 diractrick[Dot[z,Spinor[n Momentum[p]+k,m,op],
                                a, Diracgamma[x[y],di],
                                DiracGamma[Momentum[p,dim],dim],b
                               ]
                           ] /. Dot -> doot,
      ( - doot[ z,Spinor[n Momentum[p]+k,m,op ],a,
               DiracGamma[Momentum[p,dim],dim],
               DiracGamma[x[y],di],b
             ]
          + 2       (( PairContract[x[y],Momentum[p,dim] ]  *
                       doot[ z,Spinor[n Momentum[p]+k,m,op],a,b ]
                     ) /. PairContract -> Pair)
      )         ] /; last[n Momentum[p]+k] === Momentum[p],

    doot_[ a___,DiracGamma[Momentum[p_,___],___],DiracGamma[5],
           Spinor[n_. Momentum[p_] + k_. ,m_,op___],z___
         ] :>
         (-m/n doot[a,DiracGamma[5],Spinor[n Momentum[p]+k,m,op],z ]-
          If[k===0, 0,
             If[last[n Momentum[p] + k] =!= Momentum[p],0,
                   1/n doot[ a, DiracGamma[k], DiracGamma[5],
                             Spinor[n Momentum[p] + k,m,op ], z]
               ]
            ]
         ) /; last[n Momentum[p]+k]===Momentum[p],

    doot_[ a___,DiracGamma[Momentum[p_,___],___],DiracGamma[6],
           Spinor[n_. Momentum[p_] + k_. ,m_,op___],z___
         ] :>
         (m/n doot[a,DiracGamma[7],Spinor[n Momentum[p]+k,m,op],z ]-
          If[k===0, 0,
             If[last[n Momentum[p] + k] =!= Momentum[p],0,
                   1/n doot[ a, DiracGamma[k], DiracGamma[6],
                             Spinor[n Momentum[p] + k,m,op ], z]
               ]
            ]
         ) /; last[n Momentum[p]+k]===Momentum[p],

    doot_[ a___,DiracGamma[Momentum[p_,___],___],DiracGamma[7],
           Spinor[n_. Momentum[p_] + k_. ,m_,op___],z___
         ] :>
         (m/n doot[a,DiracGamma[6],Spinor[n Momentum[p]+k,m,op],z ]-
          If[k===0, 0,
             If[last[n Momentum[p] + k] =!= Momentum[p],0,
                   1/n doot[ a, DiracGamma[k], DiracGamma[7],
                             Spinor[n Momentum[p] + k,m,op ], z]
               ]
            ]
         ) /; last[n Momentum[p]+k]===Momentum[p],

    doot_[ a___,DiracGamma[ Momentum[p_,dim___],dim___],
           DiracGamma[x_[y__],di___],b___,
           Spinor[n_. Momentum[p_] + k_. ,m_,op___],z___
         ] :> (- doot[ a,DiracGamma[x[y],di],
                       DiracGamma[Momentum[p,dim],dim],b,
                       Spinor[n Momentum[p] + k,m,op ],z
                    ]
          + 2  (( PairContract[x[y],Momentum[p,dim]] *
                        doot[ a,b,Spinor[n Momentum[p] +k,m,op],z ]
                )/. PairContract -> Pair)
             ) /; last[n Momentum[p]+k]===Momentum[p]
            };
End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracEquation | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracGamma *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 7 December '98 at 21:05 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: internal head of dirac matrices *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DiracGamma`",
             "HighEnergyPhysics`FeynCalc`"];

DiracGamma::usage =
"DiracGamma[x, dim] is the head of all Dirac
matrices and slashes (in the internal representation).
Use DiracMatrix (or GA, GAD) and DiracSlash (or GS, GSD)
for manual (short) input.
DiraGamma[x, 4] simplifies to DiracGamma[x].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


dot := dot = MakeContext["DOT"];

MakeContext[ LorentzIndex, Momentum, DeclareNonCommutative, DiracGammaT];

DeclareNonCommutative[DiracGamma];

SetAttributes[DiracGamma, Constant];

DiracGamma /: Transpose[DiracGamma[a__]] := DiracGammaT[a];

DiracGamma[] = 1;

DiracGamma[x_ Momentum[pe_, di___], dii___]  :=
x DiracGamma[Momentum[pe, di], dii];
DiracGamma[x_ LorentzIndex[pe_, di___], dii___]  :=
x DiracGamma[LorentzIndex[pe, di], dii];
DiracGamma[x_[y_, di___], 4]              := DiracGamma[x[y,di]];
DiracGamma[5, __] := DiracGamma[5];
DiracGamma[6, __] := DiracGamma[6];
DiracGamma[7, __] := DiracGamma[7];
DiracGamma[_, 0]   := 0;
DiracGamma[0]       = 0;
DiracGamma[0, _]   := 0;
DiracGamma[a_Plus] := Map[DiracGamma, a];
DiracGamma[Momentum[x_,dix___], Momentum[y_,diy___]] := dot[
  DiracGamma[Momentum[x,dix], dix], DiracGamma[Momentum[y,diy], diy]];
DiracGamma[Momentum[x_,dix___], Momentum[y_,diy___], z__] := dot[
  DiracGamma[Momentum[x,dix], dix], DiracGamma[Momentum[y,diy], diy],
DiracGamma[z]];

DiracGamma[LorentzIndex[x_,dix___], LorentzIndex[y_,diy___]] := dot[
  DiracGamma[LorentzIndex[x,dix], dix],
    DiracGamma[LorentzIndex[y,diy], diy]];
DiracGamma[LorentzIndex[x_,dix___], LorentzIndex[y_,diy___], z__] := dot[
  DiracGamma[LorentzIndex[x,dix], dix],
  DiracGamma[LorentzIndex[y,diy], diy], DiracGamma[z]];

DiracGamma[LorentzIndex[x_,dix___], Momentum[y_,diy___]] := dot[
  DiracGamma[LorentzIndex[x,dix], dix], DiracGamma[Momentum[y,diy], diy]];
DiracGamma[LorentzIndex[x_,dix___], Momentum[y_,diy___], z__] := dot[
  DiracGamma[LorentzIndex[x,dix], dix], DiracGamma[Momentum[y,diy], diy],
   DiracGamma[z]];

DiracGamma[Momentum[x_,dix___], LorentzIndex[y_,diy___]] := dot[
  DiracGamma[Momentum[x,dix], dix], DiracGamma[LorentzIndex[y,diy], diy]];
DiracGamma[Momentum[x_,dix___], LorentzIndex[y_,diy___], z__] := dot[
  DiracGamma[Momentum[x,dix], dix], DiracGamma[LorentzIndex[y,diy], diy],
   DiracGamma[z]];

DiracGamma[LorentzIndex[x_], di_Symbol-4 ] := 0;  (*    4, D-4 *)
DiracGamma[Momentum[x_], di_Symbol-4 ] := 0;  (*    4, D-4 *)
DiracGamma[Momentum[x_, di_Symbol -4]] := 0;  (*  D-4, 4   *)
DiracGamma[LorentzIndex[x_, di_Symbol -4]] := 0;  (*  D-4, 4   *)
DiracGamma[LorentzIndex[x_, di_], di_Symbol-4] :=
  DiracGamma[LorentzIndex[x, di-4], di-4];
DiracGamma[Momentum[x_, di_], di_Symbol-4]:=
DiracGamma[Momentum[x, di-4], di-4];
(* D-4, D *)
DiracGamma[LorentzIndex[x_, di_Symbol-4], di_Symbol] :=
 DiracGamma[LorentzIndex[x,di-4], di-4];
 DiracGamma[Momentum[x_, di_Symbol-4], di_Symbol] :=
   DiracGamma[Momentum[x,di-4], di-4];
DiracGamma[ LorentzIndex[x_], di_Symbol]:= DiracGamma[LorentzIndex[x]];
DiracGamma[ n_. Momentum[x_], di_Symbol] :=
(n DiracGamma[Momentum[x]]) /; NumberQ[n];
DiracGamma[Momentum[x_,di_Symbol]]       :=  DiracGamma[Momentum[x]];
(* D, 4 *)
DiracGamma[LorentzIndex[x_,di_Symbol]] :=
  DiracGamma[LorentzIndex[x]];  (* D, 4 *)

   Pair = MakeBoxes["Pair"];

DiracGamma /:
  MakeBoxes[ DiracGamma[a_, di___],
             TraditionalForm ] :=
  MakeBoxes[
  HighEnergyPhysics`FeynCalc`Pair`Pair[
  HighEnergyPhysics`FeynCalc`Momentum`Momentum["\[Gamma]",di],a],
  TraditionalForm
           ] /; !FreeQ[a, Momentum];

DiracGamma /:
  MakeBoxes[ DiracGamma[n_Integer, ___], TraditionalForm ] :=
   SuperscriptBox["\[Gamma]", n];

DiracGamma /:
  MakeBoxes[ DiracGamma[lo_[in_], ru___Rule], TraditionalForm ] :=
   (SuperscriptBox[RowBox[{OverscriptBox["\[Gamma]", "_"]}], Tbox[in]]
   ) /; $BreitMaison === True && lo === LorentzIndex;

DiracGamma /:
  MakeBoxes[ DiracGamma[lo_[in_], ru___Rule], TraditionalForm ] :=
   (SuperscriptBox["\[Gamma]", Tbox[lo[in]]]
   ) /; $BreitMaison === False && lo === LorentzIndex;

DiracGamma /:
  MakeBoxes[ DiracGamma[lo_[in_,d_Symbol], _Symbol,
           ru___Rule], TraditionalForm ] :=
   (SuperscriptBox["\[Gamma]", Tbox[lo[in,d]]]
   ) /; lo === LorentzIndex;

DiracGamma /:
  MakeBoxes[ DiracGamma[lo_[in_, d_Symbol-4], d_Symbol-4,
             ru___Rule], TraditionalForm
           ] :=
      SuperscriptBox[RowBox[{OverscriptBox["\[Gamma]","^"]}], Tbox[in]
                    ] /; lo === LorentzIndex;

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracGamma | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracGammaCombine *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: The inverse of DiracGammaExpand *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DiracGammaCombine`",
             "HighEnergyPhysics`FeynCalc`"];

DiracGammaCombine::usage=
"DiracGammaCombine[exp] is (nearly) the inverse operation to
DiracGammaExpand.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[ DiracGamma, FeynCalcInternal, FreeQ2, GS, GSD, Momentum];

DiracGammaCombine[y_] := If[!FreeQ2[y,{GS, GSD}],
                            FeynCalcInternal[y]//dircg,
                            y//dircg
                           ];
dircg[x_Plus] :=
 If[Length[x] > 8, Map[DiracGammaCombine, x], x //. gasumrules];

dircg[exp_] := exp //. gasumrules;

(* merge sums of DiracGamma's into one *)
   gasumrules =
    {n1_. DiracGamma[Momentum[x_,di___],di___] +
     n2_. DiracGamma[Momentum[y_,di___],di___] :>
             DiracGamma[ Momentum[n1 x + n2 y,di], di ] /;
             (NumberQ[n1] && NumberQ[n2]),
     (n1_. DiracGamma[Momentum[x_, di___], di___] +
      n2_. DiracGamma[Momentum[x_, di___], di___] ):>
       (n1+n2) DiracGamma[Momentum[x, di], di],
     (n3_. Momentum[x_,di___] + n4_. Momentum[y_,di___]):>
       Momentum[ Expand[n3 x + n4 y],di]/;(NumberQ[n3]&&NumberQ[n4])
    };

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracGammaCombine | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* :Title: DiracGammaExpand *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: expands DiracGamma[ exp_Plus ] *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DiracGammaExpand`",
             "HighEnergyPhysics`FeynCalc`"];

DiracGammaExpand::usage=
"DiracGammaExpand[exp] expands all DiracGamma[Momentum[a+b+..]] in
exp into (DiracGamma[Momentum[a]] + DiracGamma[Momentum[b]] + ...).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[ DiracGamma, MomentumExpand, Momentum];

DiracGammaExpand[x_] :=
If[FreeQ[x, DiracGamma], MakeContext["FeynCalcInternal"][x], x
  ] /. DiracGamma -> gaev /. gaevlin -> DiracGamma;
gaev[x_,di___]       := gaevlin[Expand[x//MomentumExpand, Momentum], di];
gaevlin[n_Integer]             := DiracGamma[n]; (* necessary !!!!!! *)
gaevlin[x_Plus, di___]         := Map[gaevlin[#, di]&, x];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracGammaExpand | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracGammaT *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 9 December '98 at 18:49 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: DiracGammaT  denotes the a transposed DiracGamma *)
(* :Comments: still experimental !!!  check SUSY-calculations *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DiracGammaT`",
             "HighEnergyPhysics`FeynCalc`"];

DiracGammaT::usage =
"DiracGammaT[x] denotes the transpose of DiracGamma.
Transpose[DiracGammaT[x]] gives DiracGamma[x].
Note that x must have Head LorentzIndex or Momentum.";

(*
DiracGammaT[a_ /; (Head[a] =!= LorentzIndex)  &&
                  (Head[a] =!= Momentum)
           ] :=
*)


(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[ DeclareNonCommutative, DiracGamma, LorentzIndex, Momentum];

DeclareNonCommutative[DiracGammaT];

(*
DiracGammaT[a_/; (Head[a] =!= Momentum) && (Head[a] =!= LorentzIndex),___
           ] := (Message[] ; False);
*)

DiracGammaT /: Transpose[DiracGammaT[a__]] := DiracGamma[a];

DiracGammaT /: MakeBoxes[DiracGammaT[a_,___], TraditionalForm] :=
               SubsuperscriptBox["\[Gamma]", Tbox[a], "T"] /; (Head[a] ===
               LorentzIndex) || (Head[a] === Integer);

DiracGammaT /: MakeBoxes[DiracGammaT[a_,___], TraditionalForm] :=
               SuperscriptBox[Tbox["(","\[Gamma]", "\[CenterDot]",
                                   a, ")"], "T"] /; Head[a] === Momentum;

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracGammaT | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracMatrix *)

(* :Author: Rolf Mertig *)


(* :Summary: *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DiracMatrix`",
               "HighEnergyPhysics`FeynCalc`"];


DiracMatrix::usage =
"DiracMatrix[m] denotes a Dirac gamma matrix with Lorentz index m.
DiracMatrix[m1, m2, ..] is a product of gamma matrices with Lorentz
indices m1, m2, etc. DiracMatrix[5] is gamma5.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];



MakeContext[ DeclareNonCommutative, Dimension, DiracGamma, LorentzIndex];

fci  := fci = MakeContext["FeynCalcInternal"];

Options[DiracMatrix] = {Dimension -> 4, fci -> True};

DeclareNonCommutative[DiracMatrix];


DiracMatrix[a_Integer] := DiracGamma[a];

DiracMatrix[a_Dot, opt___Rule] := Map[DiracGamma[LorentzIndex[#,
 Dimension /. {opt} /. Options[DiracMatrix]],
 Dimension /. {opt} /. Options[DiracMatrix]]&, a];

DiracMatrix[a_, opt___Rule] := (DiracGamma[LorentzIndex[a,
  Dimension /. {opt} /. Options[DiracMatrix]],
 Dimension /. {opt} /. Options[DiracMatrix]]
                               ) /; Head[a] =!= Integer;

   DiracMatrix /:
   MakeBoxes[DiracMatrix[x_], TraditionalForm
            ] := SuperscriptBox["\[Gamma]",
                                MakeBoxes[x, TraditionalForm]
                               ];
   DiracMatrix /:
   MakeBoxes[DiracMatrix[x_,y___,z_/;Head[z]=!=Rule],
             TraditionalForm
            ] := RowBox @ Map[
                 SuperscriptBox["\[Gamma]",
                                MakeBoxes[#, TraditionalForm]
                               ]&,
                              {x,y,z}
                             ];
End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracMatrix | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracOrder *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: last changed July 19th 2000                                    *)
(* ------------------------------------------------------------------------ *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DiracOrder`",
             "HighEnergyPhysics`FeynCalc`"];

DiracOrder::usage=
"DiracOrder[expr] orders the Dirac matrices in expr alphabetically.
DiracOrder[expr, orderlist] orders the Dirac matrices in expr according
to orderlist.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];



memset:= memset             = MakeContext["MemSet"];
diracgamma := diracgamma    = MakeContext["DiracGamma"];
dotsimplify:= dotsimplify   = MakeContext["DotSimplify"];
expanding := expanding      = MakeContext["Expanding"];
fci := fci                  = MakeContext["FeynCalcInternal"];
pair := pair                = MakeContext["Pair"];
sCO  := sCO                 = MakeContext["PairContract"];
dot  := dot                 = MakeContext["DOT"];
des  := des                 = MakeContext["DiracTrick"];


dotLin[z_] := dotsimplify[z(*/.Dot -> dot*), expanding -> False];

diraccanonical[ x_,y__ ]:=diraccanonical[x.y];
   diraccanonical[x_]:=memset[diraccanonical[x],
         Block[{diraccanres},    (*diraccanonicaldef*)
       diraccanres = x//.{
    de_[a___,diracgamma[vl_[y__],di___],
             diracgamma[lv_[z__],dim___],b___
       ] :>( (-des[a,diracgamma[lv[z],dim], diracgamma[vl[y],di],b
                 ] +( (2 sCO[vl[y],lv[z]] des[a,b])/.sCO->(*scev*)pair)
             )/.sCO->(*scev*)pair
           )/; !OrderedQ[{lv[y],vl[z]}]
                         } /. dot -> des /. des -> dot;
(* change here in Expand : 24.5.93 *)
    diraccanres = Expand[dotLin[ diraccanres ], diracgamma
                        ] /. pair -> sCO /. sCO->pair;
    diraccanres] ];

DiracOrder[x__] := diracord@@fci[{x}];

diracord[x_]              := FixedPoint[diraccanonical, x, 42];
diracord[x_,y___,z_]      := FixedPoint[diraccanonical,
                              dot[x,y,z], 42]/;Head[z]=!=List;
diracord[x_,y__,ord_List] := diracord[dot[x,y],ord];

diracord[x_,ord_List]     := memset[diracord[x,ord], Block[
     {diracordrev=Reverse[ord], diracordz,
      diracordres=x,diracordi},
    Do[ diracordz = diracordrev[[diracordi]];
        diracordres = diracordres//.
            {de_[a___,diracgamma[vl_[y__],di___],
             diracgamma[lv_[diracordz0_,dime___],dim___],b___
       ] :>
   (  (-des[a,diracgamma[lv[diracordz0,dime],dim],
             diracgamma[vl[y],di],b
          ]+
        ( 2 sCO[vl[y],lv[diracordz0,dime]] des[a,b] )/.sCO->pair
      )
   ) /; !FreeQ[lv[diracordz0, dime], diracordz]
            } /. dot -> des /. des -> dot, {diracordi,1,Length[ord]}
      ];
      (Expand[dotLin[diracordres], diracgamma])/.pair->sCO/.sCO->pair]];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracOrder | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracReduce *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: Last changed July 19th 2000 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: contraction and simplification rules for gamma matrices *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DiracReduce`",
             "HighEnergyPhysics`FeynCalc`"];

DiracReduce::usage=
"DiracReduce[exp] reduces all four-dimensional Dirac matrices in exp
to the standard basis (S,P,V,A,T) using the Chisholm identity (see Chisholm).
In the result the basic Dirac structures are wrapped with a head
DiracBasis. I.e.: S corresponds to DiracBasis[1],
P : DiracBasis[DiracMatrix[5]],
V : DiracBasis[DiracMatrix[mu]], A: DiracBasis[DiracMatrix[mu, 5]],
T: DiracBasis[DiracSigma[DiracMatrix[mu, nu]]].
By default DiracBasis is substituted to Identity. \n
Notice that the result of DiracReduce is given in the FeynCalcExternal - way,
i.e., evtl. you may have to use FeynCalcInternal on result.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[ Chisholm, Collect2, Contract];
dot := dot          = MakeContext["DOT"];

MakeContext[ DiracBasis, DiracGamma, DiracMatrix, DiracOrder,
DiracSigma, DiracSigmaExplicit, DiracSimplify, DiracSubstitute67,
Eps, Factor2, Factoring, FinalSubstitutions,
FCE, FCI, LorentzIndex, Pair, Rename];

Options[DiracReduce] = {Factoring -> False,
                        FinalSubstitutions -> {DiracBasis -> Identity}
                       };
DiracReduce[x_, {ops___Rule}] := DiracReduce[x, ops];
DiracReduce[x_, ops___Rule] :=
  Block[{temp = FCI[x], spart, n1, n2, ddb, res, finsub,finsub1,factoring},

finsub1 =  If[ Length[{ops}] === 0, {},
               If[!FreeQ[{ops}, FinalSubstitutions],
                  FinalSubstitutions /. {ops},
                  Select[Flatten[{ops}], FreeQ[#,Factoring]&]]
             ];
finsub = Join[finsub1, FinalSubstitutions /. Options[DiracReduce]];
factoring = Factoring /. {ops} /. Options[DiracReduce];

(* do first usual DiracSimplify *)
temp = DiracSimplify[temp, DiracSubstitute67 -> True,
                           DiracSigmaExplicit -> False];
  If[$VeryVerbose > 1, Print["DiracSimplify done"]];
(* Chisholm identity recursively *)
temp = Chisholm[temp]//DiracOrder;
  If[$VeryVerbose > 1, Print["Chisholm done"]];
temp = Expand[temp, DiracGamma];

(* introduce DiracSigma *)
(* use gamma[m,n, 5] = 1/2 ( eps[m,n,r,s] sig[r,s] + 2 g[m,n] gamma[5] )
*)
temp = temp /. dot[DiracGamma[a_[xx_]], DiracGamma[b_[yy_]], DiracGamma[5]
                  ] :> ( un1 = Unique[mU1]; un2 = Unique[mU2];
                       Expand[
                         1/2 (Eps[a[xx], b[yy], LorentzIndex[un1],
                                                LorentzIndex[un2]] *
                         (I/2) (FCI[ DiracMatrix[un1, un2] -
                                     DiracMatrix[un2, un1] ])  +
                         2 Pair[a[xx], b[yy]] DiracGamma[5])
                             ]
                       );
(* for the renaming of dummy indices *)

temp = Contract[temp, Rename-> True];

(* XXX *)
temp = temp /. dot[DiracGamma[a_[xx_]], DiracGamma[b_[yy_]]] :>
               ( -I DiracSigma[DiracGamma[a[xx]], DiracGamma[b[yy]]] +
                 Pair[b[yy], a[xx]] );
temp = Contract[DiracSimplify[temp, DiracSigmaExplicit -> False]];
temp = Collect2[temp, DiracGamma, Factoring -> factoring];
  If[$VeryVerbose > 1, Print["collecting done"]];

(* get the S - part *)
spart = Select[temp + n1 + n2, FreeQ[#, DiracGamma]&] /. {n1 :> 0, n2 :> 0};

temp = temp - spart;

If[factoring === False, spart = Expand[spart] DiracBasis[1],
   If[factoring === True, spart = Factor2[spart] DiracBasis[1],
      spart = factoring[spart] DiracBasis[1]]
  ];

ddb[y__] := DiracBasis[dot[y]];
res = spart + (temp /. DiracSigma[a__] :> DiracBasis[FCE[DiracSigma[a]]] /.
                       dot[DiracGamma[a_], DiracGamma[5]] :>
                       DiracBasis[FCE[dot[DiracGamma[a], DiracGamma[5]]]] /.
                       DiracGamma[a_] :> DiracBasis[FCE[DiracGamma[a]]]);
res = res /. finsub /. finsub;
res = FCE[res];
res = res /. finsub /. finsub;
res];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracReduce | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracSigma *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 6 August '97 at 0:26 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: DiracSigma[x,y] = I/2 (x  .  y -  y . x )
              DiracSigma[DiracMatrix[x,y]] =
                I/2 (DiracMatrix[x, y] -  DiracMatrix[y, x])
*)

(* :Title: DiracSimpCombine *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DiracSimpCombine`",
             "HighEnergyPhysics`FeynCalc`"];

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

DiracSimpCombine::usage=
"DiracSimpCombine is an option for DiracSimplify. If set to
True sums of DiracGamma's will be merges as much as
possible in DiracGamma[ .. + .. + ]'s.";

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracSimpCombine | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracSimplify *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 20 December '98 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: like DiracTrick, but including non-commutative expansion *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DiracSimplify`",
             "HighEnergyPhysics`FeynCalc`"];

ChisholmSpinor::usage=
"ChisholmSpinor[x] uses for a DiraGamma between spinors
the Chisholm identity. As an optional second argument 1 or 2 may be
given, indicating that ChisholmSpinor should only act on the first
resp. second part of a product of spinor chains.";

DiracSimplify::usage=
"DiracSimplify[expr]   simplifies products of Dirac matrices
in expr and expands non-commutative products.
Double Lorentz indices and four vectors are contracted.
The Dirac equation is applied.
All DiracMatrix[5], DiracMatrix[6] and DiracMatrix[7] are moved to
the right. The order of the Dirac matrices is not changed.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[ Collect2, Contract, DiracCanonical, DiracOrder,
DiracEquation, DiracGamma, DiracGammaCombine, DiracGammaExpand,
DiracMatrix, DiracOrder, DiracSigmaExplicit, DiracSimpCombine, DiracSlash,
DiracSubstitute67, DiracTrace];

dot := dot  = MakeContext["DOT"];
dR  := dR   = MakeContext["DiracTrick"];

Eps := Eps  = MakeContext["Eps"];
fcinte := fcinte = MakeContext["FeynCalcInternal"];

MakeContext[ DotSimplify, EpsContract, Expanding, Expand2,
Factor2, FactorTime, FreeQ2, Factoring, InsideDiracTrace,
LorentzIndex, MemSet, NonCommQ ];

sCO := sCO   = MakeContext["PairContract"];
scev := scev = MakeContext["ExpandScalarProduct"];

MakeContext[ Pair, PartitHead, Spinor, GA, GAD, GS, GSD, SUNT, Tr];

(* SpinorsandPairs := SpinorsandPairs = MakeContext["SpinorsandPairs]"; *)

Options[DiracSimplify] = {
 DiracCanonical -> False,
 DiracSigmaExplicit -> True,
 DiracSimpCombine->False,
 DiracSubstitute67 -> False,
 Expanding -> True,
 Factoring -> False,
 fcinte -> False,
 InsideDiracTrace -> False};

fcinter[x_] := If[ (fcinter /. Options[DiracSimplify]) === True,
                   x, fcinte[x] ];

dotLin[x_] := If[FreeQ[x, Dot], x, DotSimplify[x, Expanding -> False]];
diracEq[x_]:= If[FreeQ[x, Spinor], x, DiracEquation[x]];

Options[diracSimplify] =
        {diracInfo->False, DiracCanonical->False,
         InsideDiracTrace->False, DiracSubstitute67->False,
         Factoring -> False, DiracSimpCombine->False
        };

dit[x_,ops___Rule]:=DiracTrace[diracSimplify@@Join[{x},{ops},
                    Flatten[Prepend[{Options[DiracSimplify]},
                                     InsideDiracTrace -> True]]
                                       ]
                   ];
(* DiracSimplifydef*)
DiracSimplify[x_,y__, z___Rule]:=DiracSimplify[dot[x,y], z];

diracSimplify[z_, ru___Rule]:=
    (Contract[z]/.DiracTrace->dit)/;!FreeQ[z,DiracTrace];

(*
dS[]=1;
*)
dS[x__] := MemSet[dS[x], dR[x]];
(*
dS = HighEnergyPhysics`FeynCalc`DiracTrick`Private`ds;
*)

(* ****************************************************************** *)
DiracSimplify[a_, opt___Rule] := a /;
         FreeQ2[a, {DiracGamma,DiracSlash,DiracMatrix,
                    GA[__],GS[__],GAD[__],GSD[__]}];

DiracSimplify[a_, opts___Rule] :=
  If[ (Expanding /. {opts} /. Options[DiracSimplify]) === False,
     If[(DiracSigmaExplicit /. {opts} /.
                   Options[DiracSimplify]) === True,
        DiracSigmaExplicit[diracEq[dotLin[a // fcinter] /. dot -> dS]
                                                  ],
        diracEq[dotLin[a // fcinter] /. dot -> dS]
       ],
       If[$VeryVerbose>2, Print["doing oldDiracSimplify on ", StandardForm[a]]];
       oldDiracSimplify[
              If[(DiracSigmaExplicit /. {opts} /.
                 Options[DiracSimplify]) === True,
                 DiracSigmaExplicit[
                 fcinter[a] /. Pair -> sCO /. dot -> dS],
                 fcinter[a] /. Pair -> sCO /. dot -> dS
                ],
                        opts
                       ] /. sCO -> Pair
    ];
(* ****************************************************************** *)

oldDiracSimplify[x_,y___Rule] := diracSimplify[x,y] /; FreeQ[x, Spinor];

oldDiracSimplify[x_,yy___Rule] := Block[{dre},
If[$VeryVerbose>2, Print["entering oldDiracSimplify", x]];
(*NEW0796*)
dre = Collect[DotSimplify[dR[DiracGammaCombine[x]]]/.
dot->dooo,dooo[__]]/.dooo->dot;
                     dre =  FixedPoint[ SpinorChainEvaluate, dre, 142];
                     If[ !FreeQ[dre, Eps],
                         dre = Contract[dre, EpsContract -> True];
                         dre = FixedPoint[ SpinorChainEvaluate, dre, 142]
                         ,
                         If[!FreeQ[dre, LorentzIndex],
(*
                            dre = Contract[dre]
*)
                            dre = Contract[dre, Expanding -> False]
                           ];
                         dre = FixedPoint[ SpinorChainEvaluate, dre, 142];
                       ];
   If[!FreeQ[dre, LorentzIndex],
print2["contracting in oldDiracSimpify"];
      dre = Contract[dre];
print2["contracting in oldDiracSimpify done"];
     ];
   If[Length[DownValues[SpinorsandPairs]
            ] > 1,
      dre = (dre /. dot -> SpinorsandPairs/. SpinorsandPairs->dot
            )//dotLin
     ];
(*
   If[FreeQ[dre,dot] || (!FreeQ[dre,StandardMatrixElement]),
      dre = Expand[dre, StandardMatrixElement] ];
*)
   If[!FreeQ[dre, DiracGamma], dre = Expand2[dre, DiracGamma]];
   If[LeafCount[dre] < 420, dre = Factor2[dre, FactorTime->10]];
       dre                 ] /; !FreeQ[x,Spinor];

 collone[x_,y_]:=Collect2[x,y, Factoring -> False];

(* #################################################################### *)

gamma67back[x_] := x/.DiracGamma[6] -> (1/2 + DiracGamma[5]/2)/.
                      DiracGamma[7] -> (1/2 - DiracGamma[5]/2);

contractli[x_] := MemSet[contractli[x], Contract[ x, Expanding -> True,
                                                     Factoring -> False,
                         EpsContract -> False ]
                        ];

(*diracSimplifydef *)
(*XXX1 *)
(*
diracSimplify[x_ /; NonCommQ[x],in___] := x;
*)
diracSimplify[x_,in___] := x /; NonCommQ[x];

(*CHANGE 1298 *)
diracSimplify[x_,in___Rule]:= If[FreeQ[x, DiracGamma], x,
MemSet[diracSimplify[x,in], Block[
       {diracopt,diracdt,diracndt=0,diraccanopt,diracpdt,diracgasu,
        diracldt,diracjj=0,info,diractrlabel,diracga67,diracsifac,
        diracpag,colle
       },
        (* There are several options *)
        diracopt     = Join[ Flatten[{in}],Options[diracSimplify] ];
        info         = diracInfo/.diracopt;
        diraccanopt  = DiracCanonical/.diracopt;
        diractrlabel = InsideDiracTrace/.diracopt;
        diracga67    = DiracSubstitute67/.diracopt;
        diracgasu    = DiracSimpCombine/.diracopt;
        diracsifac   = Factoring/.diracopt;
        diracdt = dotLin[ x//DiracGammaExpand ];
If[$VeryVerbose > 2,Print["dir1"]];
        If[ diracgasu === True,
            diracdt = contractli[DiracGammaCombine[diracdt/.Pair->sCO]
                                ] /. dot -> dS,
            diracdt = contractli[ diracdt ]/.dot->dS
          ];
If[$VeryVerbose > 2,Print["dir2a"]];
        diracdt = Expand2[ scev[diracdt//fEx], {Pair, dot}];
        If[diractrlabel===True,
(*
           diracdt = diracdt/.dR->trIC/.trI->dS//.
                     dR->drCOs/.drCO->trIC/.trI->dS;
*)
           diracdt = diracdt/.dot->trIC/.trI->dS;
              (* optimization *)
           colle[a_]:=If[ (Length[a]<20(*00*))||(Head[a]=!=Plus), a,
                          Collect2[a, dot, Factoring -> False] ];
           dirfun[exp_]:=colle[exp/.dot->dS/.dot -> trIC /. trI->dot];
           diracdt = FixedPoint[dirfun, diracdt]/.dot ->trIC/.trI->dS;
If[$VeryVerbose>2,Print["dir2done"]];
           If[ FreeQ[ diracdt, dot ],
               diracdt = diracdt/.DiracGamma[_[__],___]->0;
               diracpag=PartitHead[diracdt,DiracGamma];
                   If[ diracpag[[2]] === DiracGamma[5], diracdt = 0 ];
                   If[ diracpag[[2]] === DiracGamma[6] ||
                       diracpag[[2]] === DiracGamma[7],
                       diracdt = 1/2  diracpag[[1]]
                     ]
             ]
          ];
If[$VeryVerbose>2,Print["dir3"]];
        If[FreeQ[diracdt,dot],
           diracndt=Expand[(diracdt/.sCO->scev)//DiracGammaExpand];
           If[diracga67 === True, diracndt = Expand[diracndt//gamma67back]]
           ,
If[$VeryVerbose>2,Print["dir3 expanding "]];
           diracdt = Expand[ diracdt ];
If[$VeryVerbose>2,Print["dir3 expanding done ", Length[diracdt]]];
         If[ Head[diracdt] === Plus, diracldt=Length[diracdt],
             If[ diracdt===0, diracldt = 0, diracldt = 1 ]
           ];
If[$VeryVerbose>2,
   Print["in diracSimplify: working with ",diracldt," terms"]];
      While[diracjj<diracldt,diracjj++;
            If[diracldt==1,
               diracpdt = diracdt, diracpdt = diracdt[[diracjj]]
              ];
            If[diractrlabel===True,
               diracpdt = diracpdt/.dot->trIC/.trI->dS//.
                          dot -> trIC/.trI->dS;
               diracpdt = diracpdt//.dot -> dS
              ];
(* maybe insert some TimeConstrained here later *)
If[$VeryVerbose>2,
   Print["in diracSimplify: contraction done, expand now."]];
       diracpdt = scev[ diracpdt ]//Expand;
(*
            diracpdt = Expand[ diracpdt];
*)
            If[diractrlabel===True,
               diracpdt = fEx[(diracpdt//DiracGammaExpand)/.dot->dS]/.
                                    dot->trIC/.trI->dS//.dot->dS/.
                                    dot->trIC/.trI->dS,
               diracpdt = fEx[DiracGammaExpand[diracpdt]/.dot->dS]//.
                                    dot->dS
              ];
             If[ diracga67===True,
                 diracpdt = gamma67back[ diracpdt/.dot->dr67 ],
                 diracpdt = fEx[ diracpdt ]
               ];
             diracndt = diracndt + Expand2[ diracpdt, dot ];
             If[ info===True || $VeryVerbose > 2,
                 Print["# ",diracjj," / ",diracldt," = ",
                        Length[diracndt] ]
               ]
           ];
   diracndt = diracndt/.dr->dot/.sCO->scev;
   diracndt = Expand[dotLin[diracndt]];
   If[ (diraccanopt===True ),
print3["diracordering in diracSimplify"];
        diracndt = DiracOrder[ diracndt ] ;
        diracndt = Expand[dotLin[diracndt]]
     ];
          ] (* If FreeQ[diracdt,dr] *);
If[$VeryVerbose>2, Print["dir4 ",diracdt]];
print3["diracdt = ", diracdt ];
    diracndt = dotLin[diracndt];
If[$VeryVerbose>2, Print["dir5"]];
   If[ diracsifac === True,
       diracndt = Factor2[ diracndt ] ];
If[$VeryVerbose>2, Print["dir6"]];
print3["exiting diracSimplify"];
  diracndt]]];  (* end of diracSimplify *)

(* #################################################################### *)
                                                        (*dr67def*)
   dr67[ b___ ] := dS[ b ]/;FreeQ2[{b},{DiracGamma[6],DiracGamma[7]}];
   dr67[ b___,DiracGamma[6],z___ ] := 1/2 dS[b,z] +
                                      1/2 dS[ b,DiracGamma[5],z ];
   dr67[ b___,DiracGamma[7],z___ ] := 1/2 dS[b,z] -
                                      1/2 dS[ b,DiracGamma[5],z ];

   dIex[ a___,x_ + y_, b___] := dS[a,x,b] + dS[a,y,b];   (*dIexdef*)
                                                         (*dixdef*)

   dix[y_] :=  y/.dot->dIex/.dIex->dS;
(* #################################################################### *)
(* ************************************************************** *)

(* This is the tricky function which does the expansion of the dr's *)
   fEx[z_]:=FixedPoint[ dix, z/.dot -> dS ];                (*fExdef*)
(* ************************************************************** *)

(* cyclic property *)
   trIC[y___]:=If[$Kreimer =!= True,
                  tris @@ cyclic[y],
                  tris[y]
                 ];
   cyclic[x__]:=RotateLeft[{x},Position[{x},First[Sort[{x}]]][[1,1]]];
   cyclic[]:={};

(* ************************************************************** *)
(* fr567def, frlivcdef : two special FreeQ - checking functions *)
   fr567[x__] := True /; FreeQ2[FixedPoint[ReleaseHold, {x}],
    {DiracGamma[5],DiracGamma[6],DiracGamma[7]}];

(* Properties and special cases of traces (up to a factor 4) *)
   tris[x___] := tris[x] = trI[x];                  (*trisdef*)
   trI[a_+b_] := tris[a] + tris[b];                  (*trIdef*)
   trI[] = 1;
   trI[ DiracGamma[5] ] = 0;
   trI[ DiracGamma[6] ] = 1/2;
   trI[ DiracGamma[7] ] = 1/2;

   trI[ a:DiracGamma[_[__]].. ,DiracGamma[n_] ] := 0 /;
      (OddQ[Length[{a}]]&&(n==5 || n==6 || n==7));

    trI[ a:DiracGamma[_[__],___].. ,DiracGamma[n_] ] := 0 /;
         (OddQ[Length[{a}]]&&(n==5 || n==6 || n==7)) &&
         ($BreitMaison === False);

   trI[ d:DiracGamma[__].. ] := 0/;(OddQ[Length[{d}]] && fr567[ d ]);

   trI[ d:DiracGamma[_[__],___].. ,DiracGamma[5] ] := 0/;Length[{d}]<4;

   trI[x_] :=  x /; FreeQ[ {x},DiracGamma ];

   trI[ DiracGamma[a_[b__],___],DiracGamma[c_[d__],___],
        DiracGamma[6] ] := 1/2 scev[ a[b],c[d] ];

   trI[ DiracGamma[a_[b__],___],DiracGamma[c_[d__],___],
        DiracGamma[7] ] := 1/2 scev[ a[b],c[d] ];

   trI[ x__] :=
     HighEnergyPhysics`FeynCalc`DiracTrace`Private`spursav[x]/;
        ( Length[{x}] < 11 && fr567[x]) ||
        ( Length[{x}] <  6 && (!fr567[x]));

(* #################################################################### *)


(* SpinorChainEvaluatedef *)

(* #################################################################### *)
(*                             Main43                                   *)
(* #################################################################### *)

spinlin[x_Plus]:=spinlin/@x;
spinlin[a_] :=( (a/.dot->ddot)//.{
              ddot[x___,z_ b__,c___] :> z ddot[x,b,c]/;NonCommQ[z]===True,
              ddot[x___,z_ ,c___]    :> z ddot[x,c]/;NonCommQ[z]===True,
              ddot[x_Spinor,b___,c_Spinor,d_Spinor,e___,f_Spinor,g___]:>
              ddot[x,b,c] ddot[d,e,f,g] }
              )/.ddot[]->1/.ddot->dot;
SetAttributes[ SpinorChainEvaluate, Listable ];
SpinorChainEvaluate[y_]:=y /; FreeQ[y,Spinor];

 (* #################################################################### *)
 (*                             Main44                                   *)
 (* #################################################################### *)

 SpinorChainEvaluate[z_Plus]:= Block[{nz},
   nz = DotSimplify[z];
   If[Length[nz]>20, nz= Collect2[ nz, Spinor,Factoring -> False] ];
   If[Head[nz]=!=Plus, nz = SpinorChainEvaluate[nz],
      If[$sirlin =!= True, nz = Map[ spcev0, nz ],
         If[ FreeQ[nz, Spinor[p1__] .
                            (a__ /; FreeQ[{a}, DiracGamma[_,_]]
                            ) . Spinor[p2__] *
                       Spinor[p3__] . (b__ /; FreeQ[{b}, DiracGamma[_,_]]
                            ) . Spinor[p4__]
                  ], nz = Map[ spcev0,nz ],
       nz = sirlin00[ Expand[Map[ spcev0,z//sirlin0 ]] ]
           ] ] ];                  nz];
 SpinorChainEvaluate[x_]:=
  If[$sirlin =!= True, Expand[spcev0[x], Spinor],
  If[ FreeQ[x//DotSimplify,
                       Spinor[p1__] .
                            (a__ /; FreeQ[{a}, DiracGamma[_,_]]
                            ) . Spinor[p2__] *
                       Spinor[p3__] . (b__ /; FreeQ[{b}, DiracGamma[_,_]]
                            ) . Spinor[p4__]
           ],
     Expand[spcev0[x]],
     sirlin00[ Expand[FixedPoint[spcev0, x//sirlin0, 3 ]] ]
    ]]/; !Head[x]===Plus;

(* #################################################################### *)
(*                             Main45                                   *)
(* #################################################################### *)

   spcev0[x_] := spcev000[x]/.spcev000->spcev0ev;
(*
   spcev000[ a_ b_ ] := a spcev000[b] /; NonCommQ[a] === True;
*)
   spcev000[y_] := y /; NonCommQ[y] === True;
   spcev000[y_Times] := Select[ y, FreeQ[#, Spinor]& ] spcev0ev[
                       Select[ y,!FreeQ[#, Spinor]& ]          ];
   spcev0ev[x_] := scev[Contract[
                     Expand[spinlin[x](*, Spinor*)]/.dot->spcevs/.
                                     spcev->dot, Expanding->False
                                      ]
                             ](*//Expand*);

   spcevs[xx___] := MemSet[ spcevs[xx], FixedPoint[ spcev,{xx},4 ] ];
(*spcevsdef*)

  (*spcevdef*)
   spcev[y_List]:=spcev@@y;
   spcev[a___,b_ /; FreeQ2[b,{Pattern, BlankSequence, BlankNullSequence}],
         c___] := b spcev[a,c] /; NonCommQ[b] === True;
   spcev[] = 1;
    spcev[x___,Spinor[a__],y___] :=
     Expand[ DiracOrder[ DiracEquation[fEx[DiracGammaExpand[
                                               x.Spinor[a].y]](*/.dR->dot*)
                                          ] ] ]/; FreeQ[{x,y},Spinor];
    spcev[x___,Spinor[a__],b___,Spinor[c__],y___] :=
      Block[ {spcevdi,spcevre,spcevj},
If[$VeryVerbose > 2, Print["entering specv with ",
InputForm[Dot@@{x,Spinor[a],b,Spinor[c],y}]]];
        spcevdi = diracSimplify[dot[Spinor[a],b,Spinor[c]],
                                     InsideDiracTrace->False,
(*
                                     DiracCanonical->True,
*)
                                     DiracCanonical->False,
                                     diracInfo->False,
                                     Factoring->False,
                                     DiracSimpCombine->True
                               ];
        spcevdi = Expand[ scev[ spcevdi ] ];
        spcevdi = Expand[ spcevdi(*,dot*) ];
        If[ !(Head[spcevdi]===Plus),
            spcevre = spinlin[ spcevdi ];
            spcevre = DiracEquation[ spcevre ];
            (*spcevre = DiracOrder[ spcevre ]*),
            spcevre = Sum[(* DiracOrder[*)
                           DiracEquation[ spinlin[ spcevdi[[spcevj]] ] ]
                                        (* ]*),
                           {spcevj,1,Length[spcevdi]}
                         ]
          ];
        spcevre = DotSimplify[spcevs[x].spcevre.spcevs[y]];
        If[ !FreeQ[spcevre, SUNT],
            spcevre = (spcevre/.dot->dS)
          ];
         spcevre = spcevre//DotSimplify;
If[$VeryVerbose > 2, Print["exiting specv with ",InputForm[spcevre]]];
        spcevre] /; FreeQ[{b}, Spinor];

(* Reference of Sirlin-relations: Nuclear Physics B192 (1981) 93-99;
   Note that we take another sign in front of the Levi-Civita tensor
   in eq. (7), since we take (implicitly) \varepsilon^{0123} = 1
*)

 (* #################################################################### *)
 (*                             Main441                                  *)
 (* #################################################################### *)

  $SpinorMinimal = False;

  sirlin00[x_]:= x/;($SpinorMinimal === False) || ($sirlin===False);
  sirlin00[x_]:=MemSet[sirlin00[x],
                     Block[{te, tg5, ntg5},
print3["sirlin001"];
(*
                       te = sirlin0[x]//ExpandAll;
*)
                       te = sirlin0[x]//Expand;
print3["sirlin002"];
                       If[FreeQ2[te,{DiracGamma[6],DiracGamma[7]}]&&
                          Head[te]===Plus && !FreeQ[te,DiracGamma[5]],
                          tg5 = Select[te, !FreeQ[#,DiracGamma[5]]& ];
                          ntg5 = te - tg5;
(*i.e. te = tg5 + ntg5 *)
                          test = Expand[tg5 + ChisholmSpinor[ntg5]];
                          If[nterms[test] < Length[te], te=test]
                         ];
print3["exiting sirlin00"];
                  te]] /; $SpinorMinimal ===  True;

(* ident3def *)

ident3[a_,_]:=a;

 (* #################################################################### *)
 (*                             Main442                                  *)
 (* #################################################################### *)
 (* canonize different dummy indices *)  (*sirlin3def*)
 sirlin3a[x_]:=((sirlin3[Expand[Contract[x](*,Spinor*)]/.
                         $MU->dum$y]/.dum$y->$MU)/.  sirlin3 -> Identity
	       )//Contract;
 sirlin3[a_Plus]:=sirlin3 /@ a;
 sirlin3[ m_. Spinor[p1__]. (ga1___) .
	     DiracGamma[ LorentzIndex[la_] ]. (ga2___) .
	     Spinor[p2__] *
	     Spinor[p3__]. (ga3___) .
	     DiracGamma[ LorentzIndex[la_] ]. (ga4___) .
             Spinor[p4__]
        ]:= Block[{counter},
                   counter = 1;

             While[!FreeQ2[{m,ga1,ga2,ga3,a4},
                           {$MU[counter], dum$y[counter]} ],
                   counter = counter + 1
                  ];
       sirlin3[
         m Spinor[p1] . ga1 .
         DiracGamma[ LorentzIndex[$MU[counter]] ] . ga2 .  Spinor[p2] *
         Spinor[p3] . ga3 .  DiracGamma[ LorentzIndex[$MU[counter]] ] .
                      ga4 .
         Spinor[p4]
              ]  ] /; FreeQ[la, $MU];

 sirlin3[ m_. Spinor[p1__].(ga1___).
             DiracGamma[ LorentzIndex[la_,di_],di_ ]. (ga2___) .
             Spinor[p2__] *
             Spinor[p3__].(ga3___).
             DiracGamma[ LorentzIndex[la_,di_],di_ ]. (ga4___) .
             Spinor[p4__]
        ] := ( m Spinor[p1] . ga1 .
                 DiracGamma[ LorentzIndex[$MU[1], di],di ] . ga2 .
                 Spinor[p2] *
                 Spinor[p3] . ga3 .
                   DiracGamma[LorentzIndex[$MU[1], di], di] . ga4 .
                 Spinor[p4]
              ) /; FreeQ2[{ga1,ga2,ga3,ga4}, DiracGamma[_,_]];


(* this is far from optimal, but for the moment sufficient *)
 $sirlin = True;


 (* #################################################################### *)
 (*                             Main443                                  *)
 (* #################################################################### *)

(* The Sirlin - identities are only valid in 4 dimensions and are
only needed, if Dirac matrices are around
*)
 sirlin0[x_]:=If[$sirlin=!=True, x,
                 If[ FreeQ2[x, {LorentzIndex, Momentum}],  x,
                     If[ FreeQ[x, Spinor], x,
                         If[ !FreeQ[x, DiracGamma[_,_]],
                             sirlin3[x]/.sirlin3->Identity,
                             sirlin0doit[(x//sirlin2)/.sirlin2->Identity]
                   ]   ]   ]
                ];

$sirlintime = 242;
SetAttributes[timeconstrained, HoldAll];
If[$OperatingSystem === "Unix",
   timeconstrained[x__] := TimeConstrained[x],
    timeconstrained[x_,__] := x
  ];

 sirlin0doit[a_Plus]:=timeconstrained[
sirlin3a[Contract[
		   (Expand[Map[sirlin1, a](*, dot*)]/.
		    sirlin1->sirlin2) /.
		   sirlin2 -> sirlin1/.sirlin1->sirlin2/.
                    sirlin2 -> Identity,EpsContract->True]
			 ] // spcev0,
                                     2 $sirlintime, a
                                    ];
 sirlin0doit[a_]:=timeconstrained[
                    (sirlin3a[sirlin1[a]/.sirlin1->sirlin2/.
                        sirlin2 -> Identity
                       ] // spcev0),
                                  $sirlintime, a
                                 ] /;Head[a]=!=Plus;

(*sirlin2def*)
 sirlin2[a_Plus]:=sirlin2/@a;


 sirlin2[m_. Spinor[pa__] . DiracGamma[Momentum[pj_]] .
                            DiracGamma[Momentum[pi_]] .
                            DiracGamma[LorentzIndex[mu_]].(vg5___).
             Spinor[pb__] *
             Spinor[Momentum[pi_],0,qf___] .
                    DiracGamma[LorentzIndex[mu_]] . (vg5___).
             Spinor[Momentum[pj_],0,qf___]
        ] := (-sirlin2[ m Spinor[pa] . DiracSlash[pi,pj] .
                                       DiracMatrix[mu] . vg5 .
                          Spinor[pb] *
                          Spinor[Momentum[pi],0,qf] .
                                       DiracMatrix[mu] . vg5 .
                          Spinor[Momentum[pj],0,qf]
                      ] +
                2 m scev[Momentum[pi],Momentum[pj]] *
                Spinor[pa] . DiracMatrix[mu] . vg5 .
                Spinor[pb] *
                          Spinor[Momentum[pi],0,qf] .
                                       DiracMatrix[mu] . vg5 .
                          Spinor[Momentum[pj],0,qf]
             )/; ({vg5}==={}) || ({vg5}==={DiracGamma[5]});


 sirlin2[m_. Spinor[pa__] . DiracGamma[Momentum[pi_]] .
                            DiracGamma[Momentum[pj_]] .
                            DiracGamma[LorentzIndex[mu_]].(vg5___).
             Spinor[pb__] *
             Spinor[Momentum[pi_],0,qf___] .
                    DiracGamma[LorentzIndex[mu_]] . (vg5___).
             Spinor[Momentum[pj_],0,qf___]
        ] :=(m scev[Momentum[pi], Momentum[pj]] *
              Spinor[pa] . DiracMatrix[$MU[1]] .
              Spinor[pb] *
              Spinor[Momentum[pi],0,qf] . DiracMatrix[$MU[1]] .
              Spinor[Momentum[pj],0,qf] +
             m scev[Momentum[pi], Momentum[pj]] *
              Spinor[pa] . DiracMatrix[$MU[1]]. DiracGamma[5] .
              Spinor[pb] *
              Spinor[Momentum[pi],0,qf] . DiracMatrix[$MU[1]] .
              DiracGamma[5] . Spinor[Momentum[pj],0,qf]
            ) /; ({vg5}==={}) || ({vg5}==={DiracGamma[5]});


 sirlin2[m_. Spinor[p1__]. (ga1___) .
	     DiracGamma[ LorentzIndex[la_] ].
	     DiracGamma[ LorentzIndex[nu_] ].
	     DiracGamma[6] .
	     Spinor[p2__] *
	     Spinor[p3__]. (ga2___) .
	     DiracGamma[ LorentzIndex[la_] ].
	     DiracGamma[ LorentzIndex[nu_] ].
	     DiracGamma[7] .
	     Spinor[p4__] ] :=  (
    m 4 Spinor[p1] . ga1 . DiracGamma[6] . Spinor[p2] *
        Spinor[p3] . ga2 . DiracGamma[7] . Spinor[p4] );

 sirlin2[m_. Spinor[p1__]. (ga1___) .
	     DiracGamma[ LorentzIndex[la_] ].
	     DiracGamma[ LorentzIndex[nu_] ].
	     DiracGamma[7] .
	     Spinor[p2__] *
	     Spinor[p3__]. (ga2___) .
	     DiracGamma[ LorentzIndex[la_] ].
	     DiracGamma[ LorentzIndex[nu_] ].
	     DiracGamma[6] .
	     Spinor[p4__] ] :=  (
    m 4 Spinor[p1] . ga1 . DiracGamma[7] . Spinor[p2] *
        Spinor[p3] . ga2 . DiracGamma[6] . Spinor[p4] );
 (* #################################################################### *)
 (*                             Main444                                  *)
 (* #################################################################### *)


(* eq. (8) *)
 sirlin2[m_. Spinor[p1__]. (ga1___) .
              DiracGamma[ LorentzIndex[mu_] ].
              DiracGamma[ lv_[rho_] ] .
              DiracGamma[ LorentzIndex[nu_] ]. (ga2___) .
            Spinor[p2__] *
            Spinor[p3__]. (ga3___) .
              DiracGamma[ LorentzIndex[mu_] ].
              DiracGamma[ lvt_[tau_] ] .
              DiracGamma[ LorentzIndex[nu_] ]. (ga4___) .
            Spinor[p4__]
       ] := Block[{ii=1, ind, la, grho, gtau, gam5},
                    While[!FreeQ[{ga1,ro,ga2,ga3,tau,ga4}, $MU[ii]],
                          ii++];
             la = DiracGamma[LorentzIndex[$MU[ii]]];
             grho = DiracGamma[lv[rho]]; gtau = DiracGamma[lvt[tau]];
             gam5 = DiracGamma[5];
             Contract[
               2 m Pair[lv[rho], lvt[tau]] *
                   Spinor[p1] . ga1 . la . ga2 .   Spinor[p2] *
                   Spinor[p3] . ga3 . la . ga4 .   Spinor[p4] +
               2 m *
                   Spinor[p1] . ga1 . gtau . ga2 . Spinor[p2] *
                   Spinor[p3] . ga3 . grho . ga4 .   Spinor[p4] +
               2 m Pair[lv[rho], lvt[tau]] *
                   Spinor[p1] . ga1 . la . ga2 . gam5 . Spinor[p2] *
                   Spinor[p3] . ga3 . la . ga4 . gam5 . Spinor[p4] -
               2 m *
                   Spinor[p1] . ga1 . gtau . ga2 . gam5 . Spinor[p2] *
                   Spinor[p3] . ga3 . grho . ga4 . gam5 . Spinor[p4]
                     ]
                   ];

(* eq. (12) of Sirlin *)

 sirlin2[m_. Spinor[p1__]. DiracGamma[ LorentzIndex[mu_] ].
                           DiracGamma[ lv_[rho_] ] .
                           DiracGamma[ LorentzIndex[sigma_] ].
                           DiracGamma[ lvt_[tau_] ].
                           DiracGamma[ LorentzIndex[nu_] ]. om_ .
             Spinor[p2__] *
             Spinor[p3__]. DiracGamma[ LorentzIndex[mu_] ].
                           DiracGamma[ lva_[alpha_] ] .
                           DiracGamma[ LorentzIndex[sigma_] ].
                           DiracGamma[ lvb_[beta_] ].
                           DiracGamma[ LorentzIndex[nu_] ]. om_ .
             Spinor[p4__]
       ] := Contract[ m 16 Pair[lvt[tau],lvb[beta]] *
                            Pair[lv[rho], lva[alpha]] *
                           Spinor[p1] . DiracMatrix[mu] . om .
                           Spinor[p2] *
                           Spinor[p3] . DiracMatrix[mu] . om .
                           Spinor[p4]
                     ] /; (om===DiracGamma[6]) ||
                          (om===DiracGamma[7]);

(* eq. (13) of Sirlin *)
 sirlin2[m_. Spinor[p1__]. DiracGamma[ LorentzIndex[mu_] ].
                           DiracGamma[ lv_[rho_] ] .
                           DiracGamma[ LorentzIndex[sigma_] ].
                           DiracGamma[ lvt_[tau_] ].
                           DiracGamma[ LorentzIndex[nu_] ]. om1_ .
             Spinor[p2__] *
             Spinor[p3__]. DiracGamma[ LorentzIndex[mu_] ].
                           DiracGamma[ lva_[alpha_] ] .
                           DiracGamma[ LorentzIndex[sigma_] ].
                           DiracGamma[ lvb_[beta_] ].
                           DiracGamma[ LorentzIndex[nu_] ]. om2_ .
             Spinor[p4__]
       ] :=(m 4 Spinor[p1] . DiracMatrix[mu].DiracGamma[lv[rho]].
                              DiracGamma[lv[beta]]. om1 .
                 Spinor[p2] *
                 Spinor[p3] . DiracMatrix[mu].DiracGamma[lva[alpha]].
                              DiracGamma[lvt[tau]]. om2 .
                                            Spinor[p4]
            ) /; ( (om1===DiracGamma[6])&& (om2===DiracGamma[7]) )||
                 ( (om1===DiracGamma[7])&& (om2===DiracGamma[6]) );
 (* #################################################################### *)
 (*                             Main445                                  *)
 (* #################################################################### *)


(* in case if no chiral projectors are present: *)
 sirlin2[m_. Spinor[p1__]. DiracGamma[ LorentzIndex[mu_] ].
                           DiracGamma[ lv_[rho_] ] .
                           DiracGamma[ LorentzIndex[sigma_] ].
                           DiracGamma[ lvt_[tau_] ].
                           DiracGamma[ LorentzIndex[nu_] ].
             Spinor[p2__] *
             Spinor[p3__]. DiracGamma[ LorentzIndex[mu_] ].
                           DiracGamma[ lva_[alpha_] ] .
                           DiracGamma[ LorentzIndex[sigma_] ].
                           DiracGamma[ lvb_[beta_] ].
                           DiracGamma[ LorentzIndex[nu_] ].
             Spinor[p4__]
       ] := Block[{tmp,re},
                    tmp[ome1_,ome2_]:= sirlin2[ m Spinor[p1].
   DiracMatrix[mu].DiracGamma[lv[rho]].DiracMatrix[sigma].
   DiracGamma[lvt[tau]].DiracMatrix[nu].DiracGamma[ome1] .
   Spinor[p2] *
   Spinor[p3].DiracMatrix[mu].DiracGamma[lva[alpha]].
   DiracMatrix[sigma].DiracGamma[lvb[beta]].DiracMatrix[nu].
   DiracGamma[ome2].  Spinor[p4]              ];
                   re = tmp[6,6] + tmp[6,7] + tmp[7,6] + tmp[7,7];
               re];

 (* #################################################################### *)
 (*                             Main446                                  *)
 (* #################################################################### *)

(* These are the ones calculated by FeynCalc  *)

sirlin2[
m_.  Spinor[pi__] . x1___ . DiracGamma[ LorentzIndex[mu_] ] .
               DiracGamma[ LorentzIndex[nu_] ] . x2___ .
Spinor[pj__] *
Spinor[pk__] .  x3___ . DiracGamma[ vm_[a_] ] .
                DiracGamma[ LorentzIndex[mu_] ] .
               DiracGamma[ LorentzIndex[nu_] ] . x4___ .
Spinor[pl__]
       ] := Contract[ m (
2*Spinor[pi] . x1 . x2 . Spinor[pj]*
   Spinor[pk] . x3 . DiracGamma[vm[a]] . x4 .
    Spinor[pl] +
  2*Spinor[pk] . x3 . DiracGamma[LorentzIndex[al$mu]] . x4 .
    Spinor[pl]*
   Spinor[pi] . x1 . DiracGamma[vm[a]] .
    DiracGamma[LorentzIndex[al$mu]] . x2 . Spinor[pj] -
  2*Spinor[pi] . x1 . DiracGamma[5] . x2 .
    Spinor[pj]*
   Spinor[pk] . x3 . DiracGamma[vm[a]] . DiracGamma[5] . x4 .
    Spinor[pl] +
  2*Spinor[pk] . x3 . DiracGamma[LorentzIndex[al$mu]] .
    DiracGamma[5] . x4 .Spinor[pl]*
   Spinor[pi] . x1 .  DiracGamma[vm[a]] .
    DiracGamma[LorentzIndex[al$mu]] . DiracGamma[5] . x2 . Spinor[pj]
             )];

sirlin2[ m_. *
Spinor[Momentum[pi_], 0, fq___] . DiracGamma[Momentum[pk_]] .
     Spinor[Momentum[pj_], 0, fq___]*
    Spinor[Momentum[pl_], 0, fq___] . DiracGamma[Momentum[pj_]] .
     Spinor[Momentum[pk_], 0, fq___]
       ] := Contract[ m (
   -((Spinor[Momentum[pi], 0, fq] . DiracGamma[Momentum[pl]] .
          Spinor[Momentum[pj], 0, fq]*
         Spinor[Momentum[pl], 0, fq] . DiracGamma[Momentum[pi]] .
          Spinor[Momentum[pk], 0, fq]*Pair[Momentum[pj], Momentum[pk]])/
       Pair[Momentum[pi], Momentum[pl]]) +
    (Spinor[Momentum[pi], 0, fq] . DiracGamma[LorentzIndex[la]] .
        DiracGamma[5] . Spinor[Momentum[pj], 0, fq]*
       Spinor[Momentum[pl], 0, fq] . DiracGamma[LorentzIndex[la]] .
        DiracGamma[5] . Spinor[Momentum[pk], 0, fq]*
       (-(Pair[Momentum[pi], Momentum[pl]]*
            Pair[Momentum[pj], Momentum[pk]]) +
         Pair[Momentum[pi], Momentum[pk]]*
          Pair[Momentum[pj], Momentum[pl]] -
         Pair[Momentum[pi], Momentum[pj]]*Pair[Momentum[pk], Momentum[pl]]))
      /(2*Pair[Momentum[pi], Momentum[pl]]) +
    (Spinor[Momentum[pi], 0, fq] . DiracGamma[LorentzIndex[la]] .
        Spinor[Momentum[pj], 0, fq]*
       Spinor[Momentum[pl], 0, fq] . DiracGamma[LorentzIndex[la]] .
        Spinor[Momentum[pk], 0, fq]*
       (3*Pair[Momentum[pi], Momentum[pl]]*
          Pair[Momentum[pj], Momentum[pk]] +
         Pair[Momentum[pi], Momentum[pk]]*Pair[Momentum[pj], Momentum[pl]] -
        Pair[Momentum[pi], Momentum[pj]]*Pair[Momentum[pk], Momentum[pl]]))/
     (2*Pair[Momentum[pi], Momentum[pl]])
             ) ];
sirlin2[ m_. *
  Spinor[Momentum[pi_], 0, fq___] . DiracGamma[Momentum[pk_]] .
     Spinor[Momentum[pj_], 0, fq___]*
    Spinor[Momentum[pl_], 0, fq___] . DiracGamma[Momentum[pi_]] .
     Spinor[Momentum[pk_], 0, fq___]
       ] := Contract[ m (
   -((Spinor[Momentum[pi], 0, fq] . DiracGamma[Momentum[pl]] .
          Spinor[Momentum[pj], 0, fq]*
         Spinor[Momentum[pl], 0, fq] . DiracGamma[Momentum[pj]] .
          Spinor[Momentum[pk], 0, fq]*Pair[Momentum[pi], Momentum[pk]])/
       Pair[Momentum[pj], Momentum[pl]]) +
    (Spinor[Momentum[pi], 0, fq] . DiracGamma[LorentzIndex[la]] .
        Spinor[Momentum[pj], 0, fq]*
       Spinor[Momentum[pl], 0, fq] . DiracGamma[LorentzIndex[la]] .
        Spinor[Momentum[pk], 0, fq]*
       (Pair[Momentum[pi], Momentum[pl]]*Pair[Momentum[pj], Momentum[pk]] +
         3*Pair[Momentum[pi], Momentum[pk]]*
          Pair[Momentum[pj], Momentum[pl]] -
         Pair[Momentum[pi], Momentum[pj]]*Pair[Momentum[pk], Momentum[pl]]))
      /(2*Pair[Momentum[pj], Momentum[pl]]) +
    (Spinor[Momentum[pi], 0, fq] . DiracGamma[LorentzIndex[la]] .
        DiracGamma[5] . Spinor[Momentum[pj], 0, fq]*
       Spinor[Momentum[pl], 0, fq] . DiracGamma[LorentzIndex[la]] .
        DiracGamma[5] . Spinor[Momentum[pk], 0, fq]*
       (-(Pair[Momentum[pi], Momentum[pl]]*
            Pair[Momentum[pj], Momentum[pk]]) +
         Pair[Momentum[pi], Momentum[pk]]*Pair[Momentum[pj], Momentum[pl]] +
        Pair[Momentum[pi], Momentum[pj]]*Pair[Momentum[pk], Momentum[pl]]))/
     (2*Pair[Momentum[pj], Momentum[pl]])
               ) ] /; First[
  Spinor[Momentum[pi], 0, fq] . DiracGamma[Momentum[pk]].
    Spinor[Momentum[pj], 0, fq]*
         Spinor[Momentum[pl], 0, fq] . DiracGamma[Momentum[pi]] .
          Spinor[Momentum[pk], 0, fq]]===
    Spinor[Momentum[pi], 0, fq] . DiracGamma[Momentum[pk]].
    Spinor[Momentum[pj], 0, fq];

sirlin2[ m_. *
  Spinor[Momentum[pi_], 0, fq___] . DiracGamma[Momentum[pk_]] .
     DiracGamma[5] .
     Spinor[Momentum[pj_], 0, fq___]*
    Spinor[Momentum[pl_], 0, fq___] . DiracGamma[Momentum[pj_]] .
         DiracGamma[5] .
     Spinor[Momentum[pk_], 0, fq___]
       ] := Contract[ m (
   Spinor[Momentum[pi], 0, fq] . DiracGamma[Momentum[pk]] .
      Spinor[Momentum[pj], 0, fq]*
     Spinor[Momentum[pl], 0, fq] . DiracGamma[Momentum[pj]] .
      Spinor[Momentum[pk], 0, fq] -
    Spinor[Momentum[pi], 0, fq] . DiracGamma[LorentzIndex[la]] .
      Spinor[Momentum[pj], 0, fq]*
     Spinor[Momentum[pl], 0, fq] . DiracGamma[LorentzIndex[la]] .
      Spinor[Momentum[pk], 0, fq]*Pair[Momentum[pj], Momentum[pk]] +
    Spinor[Momentum[pi], 0, fq] . DiracGamma[LorentzIndex[la]] .
      DiracGamma[5] . Spinor[Momentum[pj], 0, fq]*
     Spinor[Momentum[pl], 0, fq] . DiracGamma[LorentzIndex[la]] .
      DiracGamma[5] . Spinor[Momentum[pk], 0, fq]*
     Pair[Momentum[pj], Momentum[pk]]
             )      ];

sirlin2[ m_. *
Spinor[Momentum[pi_], 0, fq___] . DiracGamma[Momentum[pk_]] .
     DiracGamma[5] .
     Spinor[Momentum[pj_], 0, fq___]*
Spinor[Momentum[pl_], 0,fq___]. DiracGamma[Momentum[pi_]] .
      DiracGamma[5] .
     Spinor[Momentum[pk_], 0, fq___]
       ] :=  Contract[ m (
   -(Spinor[Momentum[pi], 0, fq] . DiracGamma[Momentum[pk]] .
        Spinor[Momentum[pj], 0, fq]*
       Spinor[Momentum[pl], 0, fq] . DiracGamma[Momentum[pi]] .
        Spinor[Momentum[pk], 0, fq]) +
    Spinor[Momentum[pi], 0, fq] . DiracGamma[LorentzIndex[la]] .
      Spinor[Momentum[pj], 0, fq]*
     Spinor[Momentum[pl], 0, fq] . DiracGamma[LorentzIndex[la]] .
      Spinor[Momentum[pk], 0, fq]*Pair[Momentum[pi], Momentum[pk]] +
    Spinor[Momentum[pi], 0, fq] . DiracGamma[LorentzIndex[la]] .
      DiracGamma[5] . Spinor[Momentum[pj], 0, fq]*
     Spinor[Momentum[pl], 0, fq] . DiracGamma[LorentzIndex[la]] .
      DiracGamma[5] . Spinor[Momentum[pk], 0, fq]*
     Pair[Momentum[pi], Momentum[pk]]
              ) ];

sirlin2[ m_. *
Spinor[Momentum[pi_], 0, fq___] . DiracGamma[Momentum[pl_]] .
     DiracGamma[5] .
     Spinor[Momentum[pj_], 0, fq___]*
Spinor[Momentum[pl_], 0, fq___] . DiracGamma[Momentum[pj_]] .
        DiracGamma[5] .
     Spinor[Momentum[pk_], 0, fq___]
       ] := Contract[ m (
   -(Spinor[Momentum[pi], 0, fq] . DiracGamma[Momentum[pl]] .
        Spinor[Momentum[pj], 0, fq]*
       Spinor[Momentum[pl], 0, fq] . DiracGamma[Momentum[pj]] .
        Spinor[Momentum[pk], 0, fq]) +
    Spinor[Momentum[pi], 0, fq] . DiracGamma[LorentzIndex[la]] .
      Spinor[Momentum[pj], 0, fq]*
     Spinor[Momentum[pl], 0, fq] . DiracGamma[LorentzIndex[la]] .
      Spinor[Momentum[pk], 0, fq]*Pair[Momentum[pj], Momentum[pl]] +
    Spinor[Momentum[pi], 0, fq] . DiracGamma[LorentzIndex[la]] .
      DiracGamma[5] . Spinor[Momentum[pj], 0, fq]*
     Spinor[Momentum[pl], 0, fq] . DiracGamma[LorentzIndex[la]] .
      DiracGamma[5] . Spinor[Momentum[pk], 0, fq]*
     Pair[Momentum[pj], Momentum[pl]]
              ) ];

 (* #################################################################### *)
 (*                             Main447                                  *)
 (* #################################################################### *)

dig[LorentzIndex[a_,___]]:=a;
dig[Momentum[a_,___]]:=a;
dig[x_]:=x/;(Head[x]=!=LorentzIndex)&&(Head[x]=!=Momentum);
dig[n_?NumberQ]:={};
getV[x_List]:=Select[Flatten[{x}/.dot->List]/.DiracGamma -> dige ,
		     Head[#]===dige&]/.dige->dig;

(* Get a list of equal gamma matrices *)
schnitt[x___][y___]:=Intersection[
Select[Flatten[{x}/.dot->List],!FreeQ[#,LorentzIndex]&],
Select[Flatten[{y}/.dot->List],!FreeQ[#,LorentzIndex]&]
                                 ];

(* get a list of not equal slashes and matrices *)
comp[x___][y___]:=Select[ Complement[Flatten[Union[{x},{y}]/.dot->List],
                             schnitt[x][y] ],
                          !FreeQ2[#, {LorentzIndex, Momentum}]&
                        ];

(* sirlin1def *)
(* do some ordering with sirlin1 ... *)
   sirlin1[m_. Spinor[p1__]. (gam1__) . Spinor[p2__] *
               Spinor[p3__]. (gam2__) . Spinor[p4__]
          ] :=  MemSet[sirlin1[m Spinor[p1].gam1.Spinor[p2] *
                               Spinor[p3].gam2.Spinor[p4]
                              ],
Block[{schnittmenge, compmenge, result,order, orderl,orderr},
                      schnittmenge = schnitt[gam1][gam2];
                       compmenge   = comp[gam1][gam2];
                        leftind    = comp[gam1][schnittmenge];
                        rightind   = comp[gam2][schnittmenge];
print3["entering sirlin1"];
(* We need at least two dummy indices for the sirlin relations *)
                 If[ Length[schnittmenge] > 1,

(* Test for eq. (12) *)
    If[(Length[schnittmenge] === 3) && (Length[compmenge] > 3),
       orderl = Join[ Drop[leftind, {1,2}], {schnittmenge[[1]],
                      leftind[[1]], schnittmenge[[2]],
                      leftind[[2]], schnittmenge[[3]]}
                    ] // getV;
       orderr = Join[ Drop[rightind, {1,2}], {schnittmenge[[1]],
                      rightind[[1]], schnittmenge[[2]],
                      rightind[[2]], schnittmenge[[3]]}
                    ] // getV;
       result =
       Expand[m Contract[
                 DiracOrder[ Spinor[p1].gam1.Spinor[p2], orderl ]*
                 DiracOrder[ Spinor[p3].gam2.Spinor[p4], orderr ] ]
             ]//sirlin2
       ];


(* ... *)
 (* Test for eq. (8) *)
    If[(Length[schnittmenge] === 2) && (Length[compmenge] > 1),
       order = Join[{First[schnittmenge]}, compmenge,
                    {Last[schnittmenge]} ] // getV;
       result = sirlin2[ Expand[ m  DiracOrder[
                         Spinor[p1].gam1.Spinor[p2] *
                         Spinor[p3].gam2.Spinor[p4], order]
                                                ]//Contract
                       ]
       ];
                ];
           If[!ValueQ[result],
              result = sirlin2[m *
                         Spinor[p1].gam1.Spinor[p2] *
                         Spinor[p3].gam2.Spinor[p4]
                                     ]
             ];
print3["exiting sirlin1"];
           result]] /; !FreeQ[{gam1}, LorentzIndex];


(*ChisholmSpinordef*)
 dsimp[x_]:=sirlin0[spcev0[x]];
 ChisholmSpinor[x_, choice_:0]:=MemSet[ChisholmSpinor[x,choice],
                             Block[{new=x, indi},
print3["entering ChisholmSpinor "];
  new = DotSimplify[new];
  If[choice===1, new = new/.{ Spinor[a__].b__ .Spinor[c__] *
                              Spinor[d__].e__ .Spinor[f__]:>
                             nospinor[a].b.nospinor[c] *
                              Spinor[d].e.Spinor[f]
                            }
    ];
  If[choice===2, new = new/.{ Spinor[a__].b__ .Spinor[c__] *
                              Spinor[d__].e__ .Spinor[f__]:>
                              Spinor[a].b.Spinor[c] *
                              nospinor[d].e.nospinor[f]
                            }
    ];

                    dsimp[Contract[dsimp[new/.{
               (Spinor[pe1_, m_, ql___] . DiracGamma[lv_[k_]] . h___ .
                Spinor[pe2_, m2_, ql___]) :> Block[{indi},
                      indi = Unique[alpha];
     -1/Pair[pe1,pe2] ( Spinor[pe1, m, ql]. DiracGamma[pe1].
                        DiracGamma[lv[k]] . DiracGamma[pe2].h.
                        Spinor[pe2, m2, ql] -
                        Pair[pe1,lv[k]] Spinor[pe1, m, ql].
                            DiracGamma[pe2]. h . Spinor[pe2, m2, ql] -
                        Pair[lv[k],pe2] Spinor[pe1, m, ql].
                            DiracGamma[pe1] . h . Spinor[pe2, m2, ql]-
                          I Eps[pe1,lv[k],pe2,LorentzIndex[indi]] *
                        Spinor[pe1, m, ql].
                            DiracGamma[LorentzIndex[indi]].
                            DiracGamma[5].h.
                        Spinor[pe2, m2, ql]
                      )] }/.nospinor->Spinor], EpsContract->True] ] ]];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracSimplify | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracSimplify2*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 15 December '97 at 17:56 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DiracSimplify2`",
             "HighEnergyPhysics`FeynCalc`"];

DiracSimplify2::usage=
"DiracSimplify2[exp] simplifies the Dirac structure but leaves
any DiracGamma[5] untouched.
DiracGamma[6] and DiracGamma[7] are inserted by their definitions.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[Cases2,DiracGammaExpand,DiracSimplify,
            DotSimplify,Contract,DOT,DiracGamma, FeynCalcInternal];

DiracSimplify2[exp_] := Block[{nn,tt},
If[FreeQ[exp,DOT], exp,
nn = DotSimplify[DiracGammaExpand[FeynCalcInternal[exp]] /.
                  DiracGamma[6] -> (1/2 + DiracGamma[5]/2) /.
                  DiracGamma[7] -> (1/2 - DiracGamma[5]/2)
                ];
If[FreeQ[nn, DiracGamma[5]], DiracSimplify[nn],

tt = Cases2[nn, DOT];

nn/.(
Table[tt[[ij]] ->
      DotSimplify[
      Contract[ tt[[ij]] //.
       {doot[a__, DiracGamma[5], b__] :>
       If[FreeQ[{a}, DiracGamma[5] ],
                     DiracSimplify[DOT[a]],
                     DiracSimplify2[DOT[b]]
         ] .
             DiracGamma[5] .
       If[FreeQ[{b}, DiracGamma[5] ],
                     DiracSimplify2[DOT[a]],
                     DiracSimplify[DOT[b]]
         ]
       }      ]], {ij,Length[tt]}

     ])
  ] ]];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracSimplify2 | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracSlash *)

(* :Author: Rolf Mertig *)


(* :Summary: DiracSlash  is a Feynman slash *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DiracSlash`",
             "HighEnergyPhysics`FeynCalc`"];

DiracSlash::usage =
"DiracSlash[p] is the contraction FourVector[p, mu]*DiracSlash[mu].
A product of those can be entered in the form DiracSlash[p1, p2, ..]."

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];



MakeContext[ Dimension];

fci := fci = MakeContext["FeynCalcInternal"];

MakeContext[ DeclareNonCommutative, DiracGamma, DOT, Momentum];

DeclareNonCommutative[DiracSlash];

Options[DiracSlash] = {Dimension -> 4, fci -> True};


DiracSlash[a_Dot, opt___Rule] := Map[DiracGamma[LorentzIndex[#,
 Dimension /. {opt} /. Options[DiracSlash]],
 Dimension /. {opt} /. Options[DiracSlash]]&, a];

DiracSlash[a_, opt___Rule] :=
DiracGamma[Momentum[a, Dimension /. {opt} /. Options[DiracSlash]],
           Dimension /. {opt} /. Options[DiracSlash]
          ] /; ( fci /. {opt} /. Options[DiracSlash] ) === True;

DiracSlash[a__, opt___Rule] :=
Apply[DOT,
 DiracGamma[Momentum[#, Dimension /. {opt} /. Options[DiracSlash]],
            Dimension /. {opt} /. Options[DiracSlash]
           ]& /@ {a}
     ] /; ( fci /. {opt} /. Options[DiracSlash] ) === True;

   DiracSlash /:
      MakeBoxes[
                DiracSlash[x__], TraditionalForm
               ] := MakeBoxes@@{fci[DiracSlash[x]], TraditionalForm};

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracSlash | \n "]];
Null

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DiracSigma`",
             "HighEnergyPhysics`FeynCalc`"];

DiracSigma::usage =
"DiracSigma[a, b] stands for I/2*(a . b - b . a) in 4 dimensions.
a and b must have Head DiracGamma, DiracMatrix or DiracSlash.
Only antisymmetry is implemented.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


dot := dot  = MakeContext["DOT"];
MakeContext[ DiracGamma, DiracMatrix, DiracSlash];

If[FreeQ[$NonComm, DiracSigma] && Head[$NonComm]===List,
   AppendTo[$NonComm, DiracSigma]];

(* by definition *)
DiracSigma[dot[a_,b_]] := DiracSigma[a,b];
DiracSigma[___, 0, ___]       = 0;
DiracSigma[a_, b_] := - DiracSigma[b, a] /; !OrderedQ[{a,b}];

DiracSigma[DiracMatrix[a_, b_]] :=
   - DiracSigma[DiracMatrix[b, a]] /; !OrderedQ[{a,b}];

DiracSigma[DiracSlash[a_, b_]] :=
   - DiracSigma[DiracSlash[b, a]] /; !OrderedQ[{a,b}];

(*NEW 8/97 *)
DiracSigma[a_ DiracGamma[b__], c_. DiracGamma[d__]] :=
 a c DiracSigma[DiracGamma[b], DiracGamma[d]];

DiracSigma[a_. DiracGamma[b__], c_  DiracGamma[d__]] :=
 a c DiracSigma[DiracGamma[b], DiracGamma[d]];

   DiracSigma /:
   MakeBoxes[DiracSigma[_[x_,___], _[y_,___]], TraditionalForm] :=
   SuperscriptBox["\[Sigma]", Tbox[x,y]];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracSigma | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracSigmaExplicit *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: Last changed July 19th 2000 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: substitute DiracSigma in temrs of DiracGamma's.
*)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DiracSigmaExplicit`",
             "HighEnergyPhysics`FeynCalc`"];

DiracSigmaExplicit::usage =
"DiracSigmaExplicit[exp] inserts in exp the definition of
DiracSigma. DiracSigmaExplict is also an option of
DiracSimplify.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


dot := dot  = MakeContext["DOT"];
fci := fci  = MakeContext["FeynCalcInternal"];
MakeContext[ DiracGamma, DiracMatrix, DiracSigma, DiracSlash];

dirsigex[a_DiracGamma, b_DiracGamma] := dirsigex[a,b] =
I/2 (dot[a, b] - dot[b, a]);

dirsigex[DiracMatrix[a_, b_]] := dirsigex[DiracMatrix[a,b]] =
 I/2 (DiracMatrix[a, b] - DiracMatrix[b, a]);

dirsigex[DiracSlash[a_, b_]] := dirsigex[DiracSlash[a,b]] =
 I/2 (DiracSlash[a, b] - DiracSlash[b, a]);

DiracSigmaExplicit[x_] := fci[x]/. DiracSigma -> dirsigex;

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracSigmaExplicit | \n "]];
Null

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracSpinor *)

(* :Author: Rolf Mertig *)



(* :Summary: *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DiracSpinor`",
               "HighEnergyPhysics`FeynCalc`"];

DiracSpinor::usage =
"DiracSpinor[p, m, ind] is a Dirac spinor for a fermion with momentum p
and mass m and indices ind.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];




MakeContext[DeclareNonCommutative];

DeclareNonCommutative[DiracSpinor];


End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* :Title: DiracSubstitute67 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DiracSubstitute67`",
             "HighEnergyPhysics`FeynCalc`"];

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

DiracSubstitute67::usage=
"DiracSubstitute67 is an option for DiracSimplify. If set to
True the chirality-projectors DiracGamma[6] and DiracGamma[7] are
substituted by their definitions.";

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracSubstitute67 | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracTrace *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 21 February '99 at 0:06 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Dirac trace calculation *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DiracTrace`",
             "HighEnergyPhysics`FeynCalc`"];

DiracTrace::usage =
"DiracTrace[expr] is the head of Dirac Traces.
Whether the trace is  evaluated depends on the option
DiracTraceEvaluate. See also Tr.
The argument expr may be a product of Dirac matrices or slashes
separated by the Mathematica Dot \".\".";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[ Contract, Collect2, DiracCanonical, DiracGamma,
DiracGammaCombine, DiracGammaExpand, DiracGammaT, DiracOrder,
DiracSigmaExplicit, DiracSimplify, DiracTrick, DiracTraceEvaluate];

dot := dot  = MakeContext["DOT"];

MakeContext[ DotSimplify, Eps, EpsEvaluate, Factor2, EpsContract,
Expanding, Expand2, Factoring, FeynCalcInternal, FeynCalcExternal,
FreeQ2, InsideDiracTrace, LeviCivitaSign, LorentzIndex, Mandelstam,
MemSet, Momentum, Pair, PairContract, PairCollect, PartitHead ];

sCO := sCO = MakeContext["PairContract"];

MakeContext[ ExpandScalarProduct, Schouten,
Spinor, SUNSimplify, SUNT, Tr, TraceOfOne, TrickMandelstam];

scev[a__] := scev[a] = ExpandScalarProduct[a];

Options[DiracTrace] = {EpsContract         -> False,
                       Factoring           -> False,
                       FeynCalcExternal   -> False,
                       Mandelstam          -> {},
                       PairCollect         -> True,
                       DiracTraceEvaluate  -> False,
                       Schouten            -> 0,
                       LeviCivitaSign      -> (-1)
                      };

dotLin[x_] := DotSimplify[x, Expanding -> False];
 (* gamma67backdef: reinsertion of gamma6 and gamm7 *)
   gamma67back[x_] := x/.DiracGamma[6]->( 1/2 + DiracGamma[5]/2 )/.
                         DiracGamma[7]->( 1/2 - DiracGamma[5]/2 );

DiracTrace[0,___]:=0;

DiracTrace[a_ /; (FreeQ[a, DiracGamma] && !FreeQ[a, DiracGammaT]),
             b___Rule] :=
    DiracTrace[(a//Transpose)//Reverse, b];
DiracTrace[a___, x_,y_, z___]:=DiracTrace[a,x.y,z]/;
       FreeQ2[y,{Rule,BlankNullSequence}]&&
       FreeQ2[x,{Rule,BlankNullSequence}];

                                               (*DiracTracedef*)
fcit[y_] := If[CheckContext["DiracSigma"],
               FeynCalcInternal[DiracSigmaExplicit[y]],
               FeynCalcInternal[y]
              ];

fcex[ops___Rule][z_] := If[(FeynCalcExternal /. {ops} /.
                            Options[DiracTrace]
                           ) === True,
                           FeynCalcExternal[z], z
                          ];
DiracTrace[x_,op___Rule] := fcex[op][
                         ( diractraceevsimple[
                    fcit[x] ,{op}] /.  diractraceevsimple -> diractraceev /.
                         diractraceev->diractraceev2
                         )          ]/;
 (DiracTraceEvaluate/.{op} /. (Join[{op},Options[DiracTrace]]//Flatten)
 ) === True;

 (*  special cases *)
(*XXXX*)
diractraceevsimple[xx_,{opt___}] :=
 ( ( TraceOfOne /. {opt} /.Options[Tr] /. Options[DiracTrace] ) * xx
 )/;  FreeQ[xx, DiracGamma];

diractraceevsimple[y_ x_Dot,{opt___}] :=
 y diractraceevsimple[x,{opt}] /; FreeQ[y, DiracGamma];

(*
diractraceevsimple[x_ + y_, {opt___}]:=
 diractraceevsimple[x, {opt}] + diractraceevsimple[y, {opt}];
*)
diractraceevsimple[x_Plus , {opt___}]:=Map[diractraceevsimple[#,{opt}]&, x];

(* in order to inhibit loops below *)
diractraceevsimpleplus[x_Plus,{opt___}] :=
 Map[diractraceevsimple[#,{opt}]&, x];

(*Fix by Rolf in response to Broniowski's observation that
  Tr[DiracSlash[p,p]] gives p^2 instead of 4p^2*)
diractraceevsimpleplus[x_/;Head[x]=!=Plus,{opt___}] := x *
 (TraceOfOne /. {opt} /.Options[Tr] /. Options[DiracTrace] );
(*diractraceevsimpleplus[x_/;Head[x]=!=Plus,{opt___}] := x;*)

diractraceevsimple[x_Dot, {opt___}]:=
(If[FreeQ[#,LorentzIndex],#, #/.Pair->sCO/.sCO->Pair]&[
     If[Length[x] > Length[Union[Variables /@ Apply[List,x]]],
        Factor[diractraceevsimpleplus[Expand[DiracTrick[x]], {opt}]],
        (TraceOfOne /. {opt} /.Options[Tr] /. Options[DiracTrace] )*
        (spursav @@ x)
      ] ]
)  /; (MatchQ[Apply[doo, x], doo[
       DiracGamma[(LorentzIndex | Momentum)[_,_],_]..]] ||
         MatchQ[Apply[doo, x], doo[
         DiracGamma[(LorentzIndex | Momentum)[_]]..]] ||
       MatchQ[Apply[doo, x], doo[
       DiracGamma[(LorentzIndex | Momentum)[_,_],_]..,
       DiracGamma[5 | 6 | 7]]] ||
         MatchQ[Apply[doo, x], doo[
         DiracGamma[(LorentzIndex | Momentum)[_]]..,
         DiracGamma[5 | 6 | 7]]]
      );

dirli[LorentzIndex[xx_, ___],___] := xx;

diractraceev[DiracGamma[LorentzIndex[a1_,dii_],dii_],
             DiracGamma[LorentzIndex[a2_,dii_],dii_],
             DiracGamma[LorentzIndex[a3_,dii_],dii_],
             a4:DiracGamma[LorentzIndex[_,dii_],dii_]..,
             DiracGamma[LorentzIndex[a1_,dii_],dii_],
             DiracGamma[LorentzIndex[a2_,dii_],dii_],
             DiracGamma[LorentzIndex[a3_,dii_],dii_],
             a4:DiracGamma[LorentzIndex[_,dii_],dii_]..
            ]:=4 dcs[dii]@@Join[{a1,a2,a3}, {a4}/.DiracGamma->dirli,
                              {a1,a2,a3}, {a4}/.DiracGamma->dirli
                             ];

dcs[dim_][x___] := dcs[dim][x] = (dics[dim][x] /. dics->dc);
dc[_][]=1; dics[_][]=1;
dics[dI_][a___, n_, n_, b___] := dI * dics[dI][a, b];
dics[dI_][a___, n_, z_, n_, b___ ] := (2-dI) * dics[dI][a, z, b];
dics[dI_][a___, n_, v_, w_, n_, b___
        ] := (dI-4) * dics[dI][a, v,w, b] + 4 (dics[dI]@@({a, b}/. v -> w));
dics[dI_][a___, n_, v_, w_, z_, n_, b___
        ] := (4-dI) * dics[dI][a, v,w,z, b] - 2 dics[dI][a, z,w,v,b];
dics[dI_][a___, n_, mu_, nu_, ro_,si_, n_, b___
        ] := (dI-4) * dics[dI][a, mu,nu,ro,si, b] +
             2 dics[dI][a, ro,nu,mu,si,b] + 2 dics[dI][a, si,mu,nu,ro,b];
dics[dI_][a___, n_, mu_, nu_, ro_, si_, de_, n_, b___
        ] := (4-dI) * dics[dI][a, mu,nu,ro,si,de, b] -
                 2 dics[dI][a, mu,de,nu,ro,si, b] -
                 2 dics[dI][a, mu,si,ro,nu,de, b] +
                 2 dics[dI][a, nu,ro,si,de,mu, b];
dicsav[dd_][x___] := dicsav[dd][x] = dics[dd][x];
dc[di_][a___, mu_, lim__, mu_, b___] :=
Expand[
Block[{m = Length[{lim}], i, j},
      (-1)^m ( (di-2 m) dicss[di][a,lim,b] -
      4 Sum[(-1)^(j-i) If[{lim}[[j]] === {lim}[[i]],
                           di (dicss[di] @@
                               Join[{a}, Delete[{lim}, {{i},{j}}], {b}]
                              ),
                              dicss[di] @@
                               (Join[{a}, Delete[{lim}, {{i},{j}}], {b}]
                                /. ({lim}[[j]]) -> ({lim}[[i]]))
                         ],
            {i,1,m-1}, {j,i+1,m}])
     ] /. dicss -> dicsav//. dics -> dcs];
 (* ****************************************************** *)
                             (*conalldef*)
conall[ x_,opt_:{}] := Contract[ x,
      Expanding->True, EpsContract->
       (EpsContract /. opt /. Options[DiracTrace]),
        Factoring->False ];


                                                  (*diractraceevdef*)
diractraceev[x_, opt___] := Block[{trfa = 1, enx = x},
 If[Head[x] === Times,
    trfa = Select[x, FreeQ2[#, {DiracGamma, LorentzIndex, SUNIndex, Eps}]&];
    enx = x / trfa;
   ];
  If[!FreeQ[x, SUNT],
     enx = SUNSimplify[DiracTrace[enx,opt]] /.DiracTrace -> diractraceev2;
    ];
(*CHANGE MAy 94 *)
(*
          diractraceev2[enx, opt] trfa;
*)
          diractraceev2[conall[enx], opt] trfa];

diractraceev2[x_,opt_:{}]:=
    ( TraceOfOne /. opt /.Options[Tr] /. Options[DiracTrace] ) * x /;
        FreeQ[x,DiracGamma];

 (* fr567def, special FreeQ - checking function *)
   fr567[x__]:=FreeQ2[FixedPoint[ReleaseHold,{x}],
                      {DiracGamma[5],DiracGamma[6],DiracGamma[7]}];

 (* coneinsdef    *)
   coneins[ x_ ]  := MemSet[coneins[x], x/.Pair->sCO/.sCO->Pair ];

 (* If no dot's  but DiracGamma's are present *)
(*XXX *)
 diractraceev3[y_,opt_:{}]:=Block[
                              {diractrpa,diractrtemp,diractrresu,four},
  four = TraceOfOne/.opt /. Options[Tr] /. Options[DiracTrace];
  diractrtemp = Expand[ conall[ y, opt ]//gamma67back];
  If[ Head[diractrtemp]===Plus,
      diractrresu = Map[ Apply[Tr,Prepend[opt,#]]&,diractrtemp],
        diractrpa = PartitHead[ diractrtemp,DiracGamma ];
        diractrresu = diractrpa[[1]] four spursav[ diractrpa[[2]] ]
    ];
  If[!FreeQ[diractrresu, LorentzIndex],
     diractrresu = diractrresu /. Pair -> sCO /. sCO -> scev
    ];
  diractrresu = Expand[diractrresu];
  If[!FreeQ[diractrresu, LorentzIndex],
     diractrresu = diractrresu /. Pair -> sCO /. sCO -> scev
    ];
                  diractrresu] /;( FreeQ[y,dot] && !FreeQ[y,DiracGamma]);


 (* #################################################################### *)
 (*                             Main48                                   *)
 (* #################################################################### *)

 diractraceev2[nnx_,in_:{}]:= Block[{diractrjj,diractrlnx,diractrres,
                                    diractrny=0,mand,diractrfact,nx ,
                                    diractrcoll,traceofone,schoutenopt},
   opt = Join[ Flatten[{in}],Options[Tr], Options[DiracTrace] ];
   mand=Mandelstam/.opt;
   diractrfact=Factoring/.opt;
   diractrcoll=PairCollect/.opt;
   schoutenopt = Schouten /. opt;
   traceofone = TraceOfOne /.  opt;
   nx = Collect2[coneins[nnx], dot, Factoring -> False];
   nx = DiracGammaCombine[nx];
   If[ Head[nx]===Plus && Length[nx] > 142,
       diractrlnx = Length[nx]; diractrjj = 0;
       While[ diractrjj<diractrlnx,diractrjj++;
If[$VeryVerbose > 1, Print["diractrjj = ", diractrjj,
     " out of ",diractrlnx]
  ];
       diractrny = diractrny +
        If[FreeQ[nx,DiracGamma],
           diractrny = nx[[diractrjj]],
             diractrny = Expand2[
       DiracSimplify[ nx[[diractrjj]],
                                    InsideDiracTrace->True,
                                     Factoring->False,
                                     FeynCalcInternal -> True,
                                     DiracCanonical->False
                    ],           Pair
                                 ];
              If[!FreeQ[diractrny, DiracGamma],
                 diractrny = Expand2[diractrny /.dot->spursav /.
                                      DiracGamma[5]->0/.
                                       DiracGamma[6]->(1/2)/.
                                        DiracGamma[7]->(1/2),
                                      Pair
                                    ];
                ];
         ]
            ]
   ,

        If[FreeQ[nx,DiracGamma],
           diractrny = nx,
             diractrny = Expand2[
       DiracSimplify[ nx,
                                    InsideDiracTrace->True,
                                     Factoring->False,
                                     FeynCalcInternal -> True,
                                     DiracCanonical->False
                    ],           Pair
                                 ];
              If[!FreeQ[diractrny, DiracGamma],
                 diractrny = Expand2[diractrny /.dot->spursav /.
                                      DiracGamma[5]->0/.
                                       DiracGamma[6]->(1/2)/.
                                        DiracGamma[7]->(1/2),
                                      Pair
                                    ];
                ];
         ]
       ];

If[!FreeQ[diractrny, DiracGamma],
   diractrny =
          DiracSimplify[ diractrny,
                                    InsideDiracTrace->True,
                                     Factoring->False,
                                     FeynCalcInternal -> True,
                                     DiracCanonical->False
                    ]
  ];

If[$VeryVerbose > 1, Print["CH2"]; Print[TimeUsed[]]];
   If[!FreeQ[diractrny, LorentzIndex],
      If[!FreeQ[diractrny, Eps],
         es = {Pair[LorentzIndex[a_,D], b_] *
               Eps[c___,LorentzIndex[a_],d___] :>
         Eps[c,b,d],
         Pair[LorentzIndex[a_,D], b_]  Eps[c___,LorentzIndex[a_,D],d___] :>
         Eps[c,b,d]
              };
         diractrny = diractrny //. es
        ];
      diractrny = diractrny /. Pair -> sCO /. sCO -> scev
     ];
If[$VeryVerbose > 1, Print["CH3"]; Print[TimeUsed[]]];

   If[!FreeQ[diractrny, Eps],
(*
      diractrny = Collect2[diractrny, Eps, Factoring -> False];
*)
      diractrny = EpsEvaluate[diractrny]//Expand;
      diractrny = Contract[ diractrny,
         EpsContract -> (EpsContract /. in /. Options[DiracTrace])
          , Schouten->schoutenopt, Expanding -> False ];
     ];
   If[ diractrfact===True, diractrres = Factor2[traceofone diractrny],
 (* this  2@#$^^$#%^@*#$ ... !!!!;
                           diractrres = Expand[ traceofone diractrny ]
 *)
                           diractrres = traceofone diractrny
     ];
   If[ Length[ mand ] >0,
       diractrres = TrickMandelstam @@ Prepend[ {mand}, diractrres ]
     ];

   diractrpc[x__]:=Plus[x]/;FreeQ[{x},Pair];
   If[ diractrcoll===True,
   diractrpc[x__]:=Collect2[ Plus[x],Pair ,Factoring -> False];
       diractrres = diractrres/.Plus->diractrpc ];
                      diractrres]/;!FreeQ2[nnx,{dot,DiracGamma}];
 (* endof diractraceev1 *)
 (* ************************************************************** *)

 (* #################################################################### *)
 (*                             Main49                                   *)
 (* #################################################################### *)
spursav[0 ..] := 0;
fdim[] = 4;
fdim[a_] = a;
spursavg[x___, LorentzIndex[a_, de___], LorentzIndex[a_, de___], y___] :=
  (fdim[de] spursavg[x, y]) /. spursavg -> spug;
diracga[DiracGamma[h_Integer]] := DiracGamma[h];
diracga[LorentzIndex[mu_, dii_]] := diracga[LorentzIndex[mu,dii],dii];
diracga[Momentum[p_, dii_]] := diracga[Momentum[p, dii],dii];
spug[x___] := spursav@@(Map[diracga, {x}] /. diracga -> DiracGamma);

 (* calculation of traces (recursively) --  up to a factor of 4 *)
   spursav[x_DiracGamma,y_DiracGamma,r_DiracGamma,z_DiracGamma,
           DiracGamma[5]]:=Block[{dirsign},
        dirsign = LeviCivitaSign /. Options[Tr];
        dirsign I Apply[ Eps, {x,y,r,z}/.
                              DiracGamma[vl_[mp_,di___],di___]->vl[mp,di]
                 ]//EpsEvaluate];

(* there is the problem with different Gamma5-schemes ...
*)
(*
   spursav[x___DiracGamma]:=MemSet[ spursav[x], spur[x] ];
*)

   spursav[x__DiracGamma] := MemSet[spursav[x], spur[x]];
   (*Added 28/2-2001 by F.Orellana. Fix to bug reported by A.Kyrielei*)
   spursav[x : ((DiracGamma[__] | HoldPattern[Plus[__HighEnergyPhysics`FeynCalc`DiracGamma`DiracGamma]]) ..)] := MemSet[spursav[x], spur[x]];


(*
   spursav = spur;
*)
   spur[]=1;
   spur[DiracGamma[5]]=0;
   spur[x_[y__],DiracGamma[5]]:=0;
   spur[a_[b__],x_[y__],DiracGamma[5]]:=0;
   spur[a_[b__],c_[d__],x_[y__],DiracGamma[5]]:=0;
   spur[a_[b__],c_[d__],x_[y__], _[__], odd__, DiracGamma[5]]:=0 /;
                                         OddQ[Length[{odd}]];
   spur[a__] := (spur @@ Reverse[Transpose[{a}]]) /;
                (!FreeQ[{a}, DiracGammaT]) && FreeQ[{a},DiracGamma];

 (* This is a definition of   Trace( 1.2.3.4. gamma[5] ) *)
   spur[x_,y_,r_,z_,DiracGamma[5]]:=Block[{dirsign},
        dirsign = LeviCivitaSign /. Options[Tr];
        dirsign I Apply[Eps, {x,y,r,z}/.DiracGamma[vl_[mp_,dii___],___
                                                   ]->vl[mp,dii]
                       ]//EpsEvaluate];

   spur[m_,n_,r_,s_,l_,t_,DiracGamma[5]]:= Block[{dirsign, sres, ltr},
     If[($Kreimer === True) && (!OrderedQ[{m,n,r,s,l,t}]),
           Tr[1/(TraceOfOne/.Options[Tr]) DiracOrder[ m.n.r.s.
                                              l.t.DiracGamma[5] ]
             ],
        If[$Larin === True &&
           !FreeQ[{m,n,r,s,l,t}, DiracGamma[LorentzIndex[_,_],_]]
           ,
           ltr[a1_, a2_, a3_, a4_, a5_][
                 DiracGamma[LorentzIndex[in_,di___], di___]
                                       ] :=
    Block[{f1, f2, f3,drsi},
          drsi = LeviCivitaSign /. Options[Tr];
          drsi = drsi/(TraceOfOne/.Options[Tr]);
 (*drsi is usually -1/4 *)
          {f1, f2, f3} = LorentzIndex[#, D]& /@ Unique[{"L","L","L"}];
          Tr[drsi I/6 Eps[LorentzIndex[in, di], f1, f2, f3] *
             a1.a2.a3.a4.a5.DiracGamma[f1, D] . DiracGamma[f2, D] .
                            DiracGamma[f3, D]
            ]
         ];
           Which[ MatchQ[t, DiracGamma[ LorentzIndex[__], ___]],
                  ltr[m,n,r,s,l][t],
                  MatchQ[l, DiracGamma[ LorentzIndex[__], ___]],
                  -ltr[m,n,r,s,t][l],
                  MatchQ[s, DiracGamma[ LorentzIndex[__], ___]],
                  ltr[m,n,r,t,l][s],
                  MatchQ[r, DiracGamma[ LorentzIndex[__], ___]],
                  -ltr[m,n,s,t,l][r],
                  MatchQ[n, DiracGamma[ LorentzIndex[__], ___]],
                  ltr[m,r,s,t,l][n],
                  MatchQ[m, DiracGamma[ LorentzIndex[__], ___]],
                  -ltr[n,r,s,t,l][m]
                ]
      ,(* nix Larin *)
        dirsign = LeviCivitaSign /. Options[Tr];
       Expand[ + dirsign I (
        scev[ m//gc,n//gc ]  Apply[ Eps, {l,r,s,t}//gc ] -
        scev[ m//gc,r//gc ]  Apply[ Eps, {l,n,s,t}//gc ] +
        scev[ n//gc,r//gc ]  Apply[ Eps, {l,m,s,t}//gc ] +
        scev[ s//gc,l//gc ]  Apply[ Eps, {m,n,r,t}//gc ] +
        scev[ l//gc,t//gc ]  Apply[ Eps, {m,n,r,s}//gc ] +
        scev[ s//gc,t//gc ]  Apply[ Eps, {l,m,n,r}//gc ]
                                                       )//EpsEvaluate
                                               ] ] ]
         ] /; $West =!= True;      (*spurdef*)

 (* this trace is calculated via expressing  DiracMatrix[w1,w2,w3]
   by the Chisholm - identity;
   thus it is only valid in four dimensions and in the naive
   gamma5 prescription
 *)
spur[w1_,w2_,w3_,w4_,w5_,w6_,w7_,w8_,DiracGamma[5]
    ]:= Block[{trsign,z1,z2,z3,z4,z5,z6,z7,z8},
{z1,z2,z3,z4,z5,z6,z7,z8} =
{w1,w2,w3,w4,w5,w6,w7,w8} /.DiracGamma[vl_[mp_,dii___],___]->vl[mp,dii];
trsign = LeviCivitaSign /. Options[Tr];
 (* trsign is usually  =  -1 *)
 (* factor 4 is put later *)
trsign*I*(Eps[w5, w6, w7, w8]*Pair[w1, w4]*Pair[w2, w3] -
     Eps[w4, w6, w7, w8]*Pair[w1, w5]*Pair[w2, w3] -
     Eps[w5, w6, w7, w8]*Pair[w1, w3]*Pair[w2, w4] +
     Eps[w4, w6, w7, w8]*Pair[w1, w3]*Pair[w2, w5] +
     Eps[w5, w6, w7, w8]*Pair[w1, w2]*Pair[w3, w4] -
     Eps[w4, w6, w7, w8]*Pair[w1, w2]*Pair[w3, w5] +
     Eps[w3, w6, w7, w8]*Pair[w1, w2]*Pair[w4, w5] -
     Eps[w2, w6, w7, w8]*Pair[w1, w3]*Pair[w4, w5] +
     Eps[w1, w6, w7, w8]*Pair[w2, w3]*Pair[w4, w5] +
     Eps[w1, w2, w3, w8]*Pair[w4, w7]*Pair[w5, w6] -
     Eps[w1, w2, w3, w7]*Pair[w4, w8]*Pair[w5, w6] -
     Eps[w1, w2, w3, w8]*Pair[w4, w6]*Pair[w5, w7] +
     Eps[w1, w2, w3, w6]*Pair[w4, w8]*Pair[w5, w7] +
     Eps[w1, w2, w3, w7]*Pair[w4, w6]*Pair[w5, w8] -
     Eps[w1, w2, w3, w6]*Pair[w4, w7]*Pair[w5, w8] +
     Eps[w3, w4, w5, w8]*Pair[w1, w2]*Pair[w6, w7] -
     Eps[w2, w4, w5, w8]*Pair[w1, w3]*Pair[w6, w7] +
     Eps[w1, w4, w5, w8]*Pair[w2, w3]*Pair[w6, w7] +
     Eps[w1, w2, w3, w8]*Pair[w4, w5]*Pair[w6, w7] -
     Eps[w1, w2, w3, w5]*Pair[w4, w8]*Pair[w6, w7] +
     Eps[w1, w2, w3, w4]*Pair[w5, w8]*Pair[w6, w7] -
     Eps[w3, w4, w5, w7]*Pair[w1, w2]*Pair[w6, w8] +
     Eps[w2, w4, w5, w7]*Pair[w1, w3]*Pair[w6, w8] -
     Eps[w1, w4, w5, w7]*Pair[w2, w3]*Pair[w6, w8] -
     Eps[w1, w2, w3, w7]*Pair[w4, w5]*Pair[w6, w8] +
     Eps[w1, w2, w3, w5]*Pair[w4, w7]*Pair[w6, w8] -
     Eps[w1, w2, w3, w4]*Pair[w5, w7]*Pair[w6, w8] +
     Eps[w3, w4, w5, w6]*Pair[w1, w2]*Pair[w7, w8] -
     Eps[w2, w4, w5, w6]*Pair[w1, w3]*Pair[w7, w8] +
     Eps[w1, w4, w5, w6]*Pair[w2, w3]*Pair[w7, w8] +
     Eps[w1, w2, w3, w6]*Pair[w4, w5]*Pair[w7, w8] -
     Eps[w1, w2, w3, w5]*Pair[w4, w6]*Pair[w7, w8] +
     Eps[w1, w2, w3, w4]*Pair[w5, w6]*Pair[w7, w8])
              ] /; ($Larin =!= True) && ($West =!= True);

 (* this trace has been calculated according to Larin,
   i.e. expression DiracMatrix[w8].DiracGamma[5] by
   (-I/6) LeviCivita[w8,mu,nu,la] DiracMatrix[mu,nu,la]
 *)

spur[w1_,w2_,w3_,w4_,w5_,w6_,w7_,w8_,DiracGamma[5]
    ]:= Block[{trsign,z1,z2,z3,z4,z5,z6,z7,z8},
{z1,z2,z3,z4,z5,z6,z7,z8} =
{w1,w2,w3,w4,w5,w6,w7,w8} /.DiracGamma[vl_[mp_,dii___],___]->vl[mp,dii];
trsign = LeviCivitaSign /. Options[Tr];
 (* trsign is usually  =  -1 *)
 (* factor 4 is put later *)
trsign*I*(Eps[z5, z6, z7, z8]*Pair[z1, z4]*Pair[z2, z3] -
    Eps[z4, z6, z7, z8]*Pair[z1, z5]*Pair[z2, z3] +
    Eps[z4, z5, z7, z8]*Pair[z1, z6]*Pair[z2, z3] -
    Eps[z4, z5, z6, z8]*Pair[z1, z7]*Pair[z2, z3] -
    Eps[z5, z6, z7, z8]*Pair[z1, z3]*Pair[z2, z4] +
    Eps[z3, z6, z7, z8]*Pair[z1, z5]*Pair[z2, z4] -
    Eps[z3, z5, z7, z8]*Pair[z1, z6]*Pair[z2, z4] +
    Eps[z3, z5, z6, z8]*Pair[z1, z7]*Pair[z2, z4] +
    Eps[z4, z6, z7, z8]*Pair[z1, z3]*Pair[z2, z5] -
    Eps[z3, z6, z7, z8]*Pair[z1, z4]*Pair[z2, z5] +
    Eps[z3, z4, z7, z8]*Pair[z1, z6]*Pair[z2, z5] -
    Eps[z3, z4, z6, z8]*Pair[z1, z7]*Pair[z2, z5] -
    Eps[z4, z5, z7, z8]*Pair[z1, z3]*Pair[z2, z6] +
    Eps[z3, z5, z7, z8]*Pair[z1, z4]*Pair[z2, z6] -
    Eps[z3, z4, z7, z8]*Pair[z1, z5]*Pair[z2, z6] +
    Eps[z3, z4, z5, z8]*Pair[z1, z7]*Pair[z2, z6] +
    Eps[z4, z5, z6, z8]*Pair[z1, z3]*Pair[z2, z7] -
    Eps[z3, z5, z6, z8]*Pair[z1, z4]*Pair[z2, z7] +
    Eps[z3, z4, z6, z8]*Pair[z1, z5]*Pair[z2, z7] -
    Eps[z3, z4, z5, z8]*Pair[z1, z6]*Pair[z2, z7] +
    Eps[z5, z6, z7, z8]*Pair[z1, z2]*Pair[z3, z4] -
    Eps[z2, z6, z7, z8]*Pair[z1, z5]*Pair[z3, z4] +
    Eps[z2, z5, z7, z8]*Pair[z1, z6]*Pair[z3, z4] -
    Eps[z2, z5, z6, z8]*Pair[z1, z7]*Pair[z3, z4] +
    Eps[z1, z6, z7, z8]*Pair[z2, z5]*Pair[z3, z4] -
    Eps[z1, z5, z7, z8]*Pair[z2, z6]*Pair[z3, z4] +
    Eps[z1, z5, z6, z8]*Pair[z2, z7]*Pair[z3, z4] -
    Eps[z4, z6, z7, z8]*Pair[z1, z2]*Pair[z3, z5] +
    Eps[z2, z6, z7, z8]*Pair[z1, z4]*Pair[z3, z5] -
    Eps[z2, z4, z7, z8]*Pair[z1, z6]*Pair[z3, z5] +
    Eps[z2, z4, z6, z8]*Pair[z1, z7]*Pair[z3, z5] -
    Eps[z1, z6, z7, z8]*Pair[z2, z4]*Pair[z3, z5] +
    Eps[z1, z4, z7, z8]*Pair[z2, z6]*Pair[z3, z5] -
    Eps[z1, z4, z6, z8]*Pair[z2, z7]*Pair[z3, z5] +
    Eps[z4, z5, z7, z8]*Pair[z1, z2]*Pair[z3, z6] -
    Eps[z2, z5, z7, z8]*Pair[z1, z4]*Pair[z3, z6] +
    Eps[z2, z4, z7, z8]*Pair[z1, z5]*Pair[z3, z6] -
    Eps[z2, z4, z5, z8]*Pair[z1, z7]*Pair[z3, z6] +
    Eps[z1, z5, z7, z8]*Pair[z2, z4]*Pair[z3, z6] -
    Eps[z1, z4, z7, z8]*Pair[z2, z5]*Pair[z3, z6] +
    Eps[z1, z4, z5, z8]*Pair[z2, z7]*Pair[z3, z6] -
    Eps[z4, z5, z6, z8]*Pair[z1, z2]*Pair[z3, z7] +
    Eps[z2, z5, z6, z8]*Pair[z1, z4]*Pair[z3, z7] -
    Eps[z2, z4, z6, z8]*Pair[z1, z5]*Pair[z3, z7] +
    Eps[z2, z4, z5, z8]*Pair[z1, z6]*Pair[z3, z7] -
    Eps[z1, z5, z6, z8]*Pair[z2, z4]*Pair[z3, z7] +
    Eps[z1, z4, z6, z8]*Pair[z2, z5]*Pair[z3, z7] -
    Eps[z1, z4, z5, z8]*Pair[z2, z6]*Pair[z3, z7] +
    Eps[z3, z6, z7, z8]*Pair[z1, z2]*Pair[z4, z5] -
    Eps[z2, z6, z7, z8]*Pair[z1, z3]*Pair[z4, z5] +
    Eps[z2, z3, z7, z8]*Pair[z1, z6]*Pair[z4, z5] -
    Eps[z2, z3, z6, z8]*Pair[z1, z7]*Pair[z4, z5] +
    Eps[z1, z6, z7, z8]*Pair[z2, z3]*Pair[z4, z5] -
    Eps[z1, z3, z7, z8]*Pair[z2, z6]*Pair[z4, z5] +
    Eps[z1, z3, z6, z8]*Pair[z2, z7]*Pair[z4, z5] +
    Eps[z1, z2, z7, z8]*Pair[z3, z6]*Pair[z4, z5] -
    Eps[z1, z2, z6, z8]*Pair[z3, z7]*Pair[z4, z5] -
    Eps[z3, z5, z7, z8]*Pair[z1, z2]*Pair[z4, z6] +
    Eps[z2, z5, z7, z8]*Pair[z1, z3]*Pair[z4, z6] -
    Eps[z2, z3, z7, z8]*Pair[z1, z5]*Pair[z4, z6] +
    Eps[z2, z3, z5, z8]*Pair[z1, z7]*Pair[z4, z6] -
    Eps[z1, z5, z7, z8]*Pair[z2, z3]*Pair[z4, z6] +
    Eps[z1, z3, z7, z8]*Pair[z2, z5]*Pair[z4, z6] -
    Eps[z1, z3, z5, z8]*Pair[z2, z7]*Pair[z4, z6] -
    Eps[z1, z2, z7, z8]*Pair[z3, z5]*Pair[z4, z6] +
    Eps[z1, z2, z5, z8]*Pair[z3, z7]*Pair[z4, z6] +
    Eps[z3, z5, z6, z8]*Pair[z1, z2]*Pair[z4, z7] -
    Eps[z2, z5, z6, z8]*Pair[z1, z3]*Pair[z4, z7] +
    Eps[z2, z3, z6, z8]*Pair[z1, z5]*Pair[z4, z7] -
    Eps[z2, z3, z5, z8]*Pair[z1, z6]*Pair[z4, z7] +
    Eps[z1, z5, z6, z8]*Pair[z2, z3]*Pair[z4, z7] -
    Eps[z1, z3, z6, z8]*Pair[z2, z5]*Pair[z4, z7] +
    Eps[z1, z3, z5, z8]*Pair[z2, z6]*Pair[z4, z7] +
    Eps[z1, z2, z6, z8]*Pair[z3, z5]*Pair[z4, z7] -
    Eps[z1, z2, z5, z8]*Pair[z3, z6]*Pair[z4, z7] +
    Eps[z3, z4, z7, z8]*Pair[z1, z2]*Pair[z5, z6] -
    Eps[z2, z4, z7, z8]*Pair[z1, z3]*Pair[z5, z6] +
    Eps[z2, z3, z7, z8]*Pair[z1, z4]*Pair[z5, z6] -
    Eps[z2, z3, z4, z8]*Pair[z1, z7]*Pair[z5, z6] +
    Eps[z1, z4, z7, z8]*Pair[z2, z3]*Pair[z5, z6] -
    Eps[z1, z3, z7, z8]*Pair[z2, z4]*Pair[z5, z6] +
    Eps[z1, z3, z4, z8]*Pair[z2, z7]*Pair[z5, z6] +
    Eps[z1, z2, z7, z8]*Pair[z3, z4]*Pair[z5, z6] -
    Eps[z1, z2, z4, z8]*Pair[z3, z7]*Pair[z5, z6] +
    Eps[z1, z2, z3, z8]*Pair[z4, z7]*Pair[z5, z6] -
    Eps[z3, z4, z6, z8]*Pair[z1, z2]*Pair[z5, z7] +
    Eps[z2, z4, z6, z8]*Pair[z1, z3]*Pair[z5, z7] -
    Eps[z2, z3, z6, z8]*Pair[z1, z4]*Pair[z5, z7] +
    Eps[z2, z3, z4, z8]*Pair[z1, z6]*Pair[z5, z7] -
    Eps[z1, z4, z6, z8]*Pair[z2, z3]*Pair[z5, z7] +
    Eps[z1, z3, z6, z8]*Pair[z2, z4]*Pair[z5, z7] -
    Eps[z1, z3, z4, z8]*Pair[z2, z6]*Pair[z5, z7] -
    Eps[z1, z2, z6, z8]*Pair[z3, z4]*Pair[z5, z7] +
    Eps[z1, z2, z4, z8]*Pair[z3, z6]*Pair[z5, z7] -
    Eps[z1, z2, z3, z8]*Pair[z4, z6]*Pair[z5, z7] +
    Eps[z3, z4, z5, z8]*Pair[z1, z2]*Pair[z6, z7] -
    Eps[z2, z4, z5, z8]*Pair[z1, z3]*Pair[z6, z7] +
    Eps[z2, z3, z5, z8]*Pair[z1, z4]*Pair[z6, z7] -
    Eps[z2, z3, z4, z8]*Pair[z1, z5]*Pair[z6, z7] +
    Eps[z1, z4, z5, z8]*Pair[z2, z3]*Pair[z6, z7] -
    Eps[z1, z3, z5, z8]*Pair[z2, z4]*Pair[z6, z7] +
    Eps[z1, z3, z4, z8]*Pair[z2, z5]*Pair[z6, z7] +
    Eps[z1, z2, z5, z8]*Pair[z3, z4]*Pair[z6, z7] -
    Eps[z1, z2, z4, z8]*Pair[z3, z5]*Pair[z6, z7] +
    Eps[z1, z2, z3, z8]*Pair[z4, z5]*Pair[z6, z7])
] /; $Larin === True;

   spur[x__,DiracGamma[6]]:=1/2 spur[x] + 1/2 spur[x,DiracGamma[5]];
   spur[x__,DiracGamma[7]]:=1/2 spur[x] - 1/2 spur[x,DiracGamma[5]];


   spur[x__]:=( DiracTrace@@ ( gamma67backj[ {x} ] )
              ) /; !FreeQ2[{x},{DiracGamma[6],DiracGamma[7]}];

   gc[x_]:=x/.DiracGamma->gach;
   gach[ex_,___]:=ex /; Length[ex]>0;                     (*gachdef*)
   gach[n_Integer]=DiracGamma[n];

   spur[y__] :=Block[ {spx,le=Length[{y}],tempres,i,spurjj,tempr,
                       temp2 = 0,fi,spt, resp,scx,dirsign},
                spx = ( {y}//DiracGammaExpand )/.DiracGamma->gach;
                scx[a_,b_]:=scev[spx[[a]],spx[[b]]];

                resp =
   Which[
        OddQ[le] && fr567[spx],
         0 ,
        le===2,
         scev[spx[[1]],spx[[2]]]/.Pair->sCO/.sCO->Pair,
        le===4,
         (scx[1,2] scx[3,4]-scx[1,3] scx[2,4]+scx[1,4] scx[2,3]
         )//Expand,
        le===6,
         (
          scx[1,6] scx[2,5] scx[3,4] - scx[1,5] scx[2,6] scx[3,4] -
          scx[1,6] scx[2,4] scx[3,5] + scx[1,4] scx[2,6] scx[3,5] +
          scx[1,5] scx[2,4] scx[3,6] - scx[1,4] scx[2,5] scx[3,6] +
          scx[1,6] scx[2,3] scx[4,5] - scx[1,3] scx[2,6] scx[4,5] +
          scx[1,2] scx[3,6] scx[4,5] - scx[1,5] scx[2,3] scx[4,6] +
          scx[1,3] scx[2,5] scx[4,6] - scx[1,2] scx[3,5] scx[4,6] +
          scx[1,4] scx[2,3] scx[5,6] - scx[1,3] scx[2,4] scx[5,6] +
          scx[1,2] scx[3,4] scx[5,6]
                )//Expand ,

       FreeQ[spx,DiracGamma[5]],
        For[i=2, i<le+1, i++,
            temp2 += ((-1)^i) * (*coneins[*)
                     scev[spx[[1]],spx[[i]]] spt@@Rest[Drop[spx,{i,i}]]
                                    (* ] *)
           ];
       Expand[ temp2/.spt->spursavg/.spursavg->spug] ,
      True,
       If[($BreitMaison === True) && ($West =!= True),
        dirsign = LeviCivitaSign /. Options[Tr];
    fi = Table[LorentzIndex[ Unique[] ],{spurjj,1,4}];
    DiracTrace @@
           ( {spx}/.DiracGamma[5]->
             (dirsign I/24 (DiracGamma[fi[[1]]].DiracGamma[fi[[2]]].
                    DiracGamma[fi[[3]]].DiracGamma[fi[[4]]]
                   ) (Eps@@fi)
             )
           )
      ,
       If[$Kreimer === True, NochNichtFertig,
          If[$Larin === True,
             {fi1, fi2, fi3} = LorentzIndex[#,D]& /@ Unique[{"a","b","c"}];
              drsi = LeviCivitaSign /. Options[Tr];
              drsi = drsi/(TraceOfOne/.Options[Tr]);
             (*drsi is usually -1/4 *)
             temp2 = spx /. {a___, lomo_[mUU_,di___], DiracGamma[5]} :>
                     Tr[ drsi I/6 Eps[lomo[mUU,di], fi1, fi2, fi3] *
                         dot @@ Map[DiracGamma[#,D]&, {a,fi1,fi2,fi3}]];
         ]  ];

    If[($Larin === False) && ($Kreimer === False) && ($West === True) &&
       FreeQ[Drop[spx,-1], DiracGamma[5]] &&
       Length[spx] > 6,
(*Print["CHECKWEst"];*)
       temp2 = Expand[2/(Length[spx]-5) Sum[(-1)^(i+j+1) *
         scev[spx[[i]], spx[[j]]] spt@@Delete[spx,{{j},{i}}],
                                            {i,2,Length[spx]-1},
                                            {j,1,i-1}
                                           ]
                     ];
(*
    fi = LorentzIndex[Unique[]];
    temp2 =
    scev[spx[[le-3]],spx[[le-2]]] spt@@Append[Drop[
                                              Drop[spx,{le-3,le-2}],-1
                                                  ], DiracGamma[5]]-
    scev[spx[[le-3]],spx[[le-1]]] spt@@Append[Drop[
                                              Drop[spx,{le-3,le-3}],-2
                                                  ], DiracGamma[5]]+
    scev[spx[[le-2]],spx[[le-1]]] spt@@Append[Drop[spx,-3],
                                              DiracGamma[5]
                                             ] +
    ( I Eps[spx[[le-3]],spx[[le-2]],spx[[le-1]],fi] *
       spt @@ Append[Drop[spx,-4],fi]
    );
*)

      ];

    temp2 = temp2/.spt->spursavg/.spursavg->spug;
    temp2
     (*if $BreitMaison===True*)
      ]
    ];
            resp];

 (* #################################################################### *)
 (*                             Main50                                   *)
 (* #################################################################### *)

 (* ************************************************************** *)
 (* Properties and special cases of traces (up to a factor 4) *)
   tris[x___] := tris[x] = trI[x];                  (*trisdef*)
   trI[a_+b_] := tris[a] + tris[b];                  (*trIdef*)
   trI[] = 1;
   trI[ DiracGamma[5] ] = 0;
   trI[ DiracGamma[6] ] = 1/2;
   trI[ DiracGamma[7] ] = 1/2;


   trI[ a:DiracGamma[_[__]].. ,DiracGamma[n_] ] := 0 /;
      (OddQ[Length[{a}]]&&(n==5 || n==6 || n==7));

       trI[ a:DiracGamma[_[__],___].. ,DiracGamma[n_] ] := 0 /;
          ($BreitMaison === False) &&
          (OddQ[Length[{a}]]&&(n==5 || n==6 || n==7))

   trI[ d:DiracGamma[__].. ] := 0/;(OddQ[Length[{d}]] && fr567[ d ]);

   trI[ d:DiracGamma[_[__],___].. ,DiracGamma[5] ] := 0/;Length[{d}]<4;

   trI[x_] :=  x /; FreeQ[ {x},DiracGamma ];

   trI[ DiracGamma[a_[b__],___],DiracGamma[c_[d__],___],
        DiracGamma[6] ] := 1/2 scev[ a[b],c[d] ];

   trI[ DiracGamma[a_[b__],___],DiracGamma[c_[d__],___],
        DiracGamma[7] ] := 1/2 scev[ a[b],c[d] ];

   trI[ x__ ] := spursav[ x ]/;( Length[{x}]<11 && fr567[x]);

 (* #################################################################### *)
 (*                             Main51                                   *)
 (* #################################################################### *)

                       (*trICdef*)
 (* cyclic property *)
   trIC[y___]:=If[$Kreimer =!= True,
                  tris @@ cyclic[y],
                  tris[y]
                 ];
   cyclic[x__]:=RotateLeft[{x},Position[{x},First[Sort[{x}]]][[1,1]]];
   cyclic[]:={};

   DiracTrace /:
   MakeBoxes[DiracTrace[a__, opts___Rule], TraditionalForm
            ] :=
   RowBox[{"tr","(",TBox[a], ")"}]

   DiracTrace /:
   MakeBoxes[DiracTrace[a__], TraditionalForm
            ] :=
   RowBox[{"tr","(",TBox[a], ")"}]

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracTrace | \n "]];
Null
(* :Title: DiracTraceEvaluate *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Option  for DiracTrace and Tr *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DiracTraceEvaluate`",
             "HighEnergyPhysics`FeynCalc`"];

DiracTraceEvaluate::usage =
"DiracTraceEvaluate is an option for DiracTrace and Tr.
If set to False, DiracTrace remains unevaluated.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracTraceEvaluate | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracTrick *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: contraction and simplification rules for gamma matrices *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DiracTrick`",
             "HighEnergyPhysics`FeynCalc`"];

DiracTrick::usage=
"DiracTrick[exp] contracts gamma matrices with each other and
performs several simplifications (no expansion!!,
use DiracSimplify for this).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[ ChargeConjugationMatrix];

dot := dot          = MakeContext["DOT"];

MakeContext[ DiracGamma, DiracGammaT];

expanding := expanding = MakeContext["Expanding"];
fci  := fci =            MakeContext["FeynCalcInternal"];

MakeContext[ FreeQ2, Pair, SUNT];
sCO := sCO          = MakeContext["PairContract"];

MakeContext[LorentzIndex];
memset := memset = MakeContext["MemSet"];

MakeContext[Momentum];
noncommQ := noncommQ = MakeContext["NonCommQ"];
exscalpro := exscalpro = MakeContext["ExpandScalarProduct"];

Options[DiracTrick] = {expanding -> False};

scev[a_, b_] := scev[a, b] = exscalpro[Pair[a,b]];
coneins[x_]  := x /. Pair -> sCO /. sCO -> Pair;

(*By definition:*)
DiracTrick[]=1;
(* for time-saving reasons: here NO fci *)
DiracTrick[y___,z_/;Head[z]=!=Rule] :=
(*
  memset[DiracTrick[y,z],
*)
     drS[y, z]/.drS -> ds //.dr->drCOs /.
                       drCO -> ds/.  dr->ds/.dr->dot(*]*);

DiracTrick[x_,r___Rule] :=
  If[(expanding /. {r} /. Options[DiracTrick]) === True,
     Expand[ fci[x] /. Dot -> dot /. (*Pair -> sCO /.*)
                  dot -> drS /.drS -> ds //. dr -> drCOs/.
                  drCO -> ds /.  dr -> ds /.  dr -> dot
           ] (*/. sCO -> Pair*),
             fci[x] /. Dot -> dot /. (*Pair -> sCO /.*)
                  dot -> drS /.drS -> ds //. dr -> drCOs/.
                  drCO -> ds /.  dr -> ds /. dr -> dot (*/.
                  sCO -> Pair*)
    ];

SetAttributes[DiracTrick, Flat];
ds[x__] := memset[ds[x], dr[x]];

(* drdef *)

ds[] = dr[]=1;
dr[a___,y_ w_,b___] := dr[a, y, w, b] /; Head[y] === SUNT;
dr[a___,y_ w_,b___] := coneins[y ds[a,w,b]]/;(noncommQ[y]&&FreeQ[y,dr]);
dr[a___,y_ ,b___]   := coneins[y ds[a,b] ] /;(noncommQ[y]&&FreeQ[y,dr]);

dr[a_spinor, b___, c_spinor, d_spinor, e___, f_spinor, g___]:=
 dr[a, b, c] dr[d, e, f, g];

dr[a__]:=( ds[a]/.DiracGamma[6]->(1/2 + DiracGamma[5]/2)/.
                  DiracGamma[7]->(1/2 - DiracGamma[5]/2)
         )/;(!FreeQ2[{a}, {DiracGamma[6], DiracGamma[7]}]) &&
            (Head[DiracGamma[6]]===DiracGamma) && $BreitMaison === True;

(*
dr[b___,DiracGamma[7],DiracGamma[x_[c__],di___],d___ ] :=
  ( 1/2 ds[ b,DiracGamma[x[c],di],d ] -
    1/2 ds[ b,DiracGamma[5],DiracGamma[x[c],di],d ]
  ) /; $BreitMaison === True
*)

dr[b___,DiracGamma[5],DiracGamma[5],c___]:= ds[ b,c ];
dr[b___,DiracGamma[5],DiracGamma[6],c___]:= ds[b,DiracGamma[6],c];
dr[b___,DiracGamma[5],DiracGamma[7],c___]:=-ds[b,DiracGamma[7],c];

dr[b___,DiracGamma[6],DiracGamma[x_[c__],di___],d___ ]:=
ds[ b,DiracGamma[x[c],di], DiracGamma[7],d ];

dr[b___,DiracGamma[6], DiracGamma[5], c___]:=ds[b,DiracGamma[6],c];
dr[b___,DiracGamma[6], DiracGamma[7], c___] := 0;
dr[b___,DiracGamma[7], DiracGamma[6], c___] := 0;

dr[b___,DiracGamma[7],DiracGamma[x_[c__],di___],d___ ] :=
   ds[ b,DiracGamma[x[c],di],DiracGamma[6],d ];

dr[b___,DiracGamma[7],DiracGamma[5],c___] := -ds[b, DiracGamma[7], c];
dr[b___,DiracGamma[6],DiracGamma[6],c___] :=  ds[b, DiracGamma[6], c];
dr[b___,DiracGamma[7],DiracGamma[7],c___] :=  ds[b, DiracGamma[7], c];


dr[b___,DiracGamma[5],c:DiracGamma[_[_]].. ,d___] :=
   (-1)^Length[{c}] ds[ b,c,DiracGamma[5],d];

(* o.k., some 4 years after the proposal of M.B., here it is: *)
drS[b___,DiracGamma[7],DiracGamma[_[__],___] + (n_. mass_),
    xy:DiracGamma[_[__],___].. , DiracGamma[6], c___] :=
(n mass drS[b, xy, DiracGamma[6], c]) /; NumberQ[n] &&
   OddQ[Length[{xy}]] && noncommQ[mass];

drS[b___,DiracGamma[6],DiracGamma[_[__],___] + (n_. mass_ ),
   xy:DiracGamma[_[__],___].. , DiracGamma[7], c___] :=
(n mass drS[b, xy, DiracGamma[7], c]) /; NumberQ[n] &&
  OddQ[Length[{xy}]] && noncommQ[mass];

drS[b___,DiracGamma[6],DiracGamma[_[__],___] + (n_. mass_ ),
   xy:DiracGamma[_[__],___].. , DiracGamma[6], c___] :=
(n mass drS[b, xy, DiracGamma[6], c]) /; NumberQ[n] &&
  EvenQ[Length[{xy}]] && noncommQ[mass];

drS[b___,DiracGamma[7],DiracGamma[_[__],___] + (n_. mass_ ),
   xy:DiracGamma[_[__],___].. , DiracGamma[7], c___] :=
(n mass drS[b, xy, DiracGamma[7], c]) /; NumberQ[n] &&
  EvenQ[Length[{xy}]] && noncommQ[mass];

drS[b___,DiracGamma[6],DiracGamma[v_[w__],di___] + (n_. mass_ ),
    DiracGamma[6], c___] :=
(n mass drS[b, DiracGamma[6], c] )/; NumberQ[n] && noncommQ[mass];

drS[b___,DiracGamma[7],DiracGamma[v_[w__],di___] + (n_. mass_ ),
    DiracGamma[7], c___] :=
(n mass drS[b, DiracGamma[7], c] )/; NumberQ[n] && noncommQ[mass];

drS[b___,DiracGamma[6],DiracGamma[v_[w__],di___] + (n_. mass_ ),
    DiracGamma[7], c___] :=
drS[b, DiracGamma[v[w],di], DiracGamma[7], c] /; NumberQ[n] &&
  noncommQ[mass];

drS[b___,DiracGamma[7],DiracGamma[v_[w__],di___] + (n_. mass_),
    DiracGamma[6], c___] :=
drS[b, DiracGamma[v[w],di], DiracGamma[6], c] /; NumberQ[n] &&
  noncommQ[mass];

drS[b___,DiracGamma[6],DiracGamma[v_[w__],di___] + (n_. mass_ ),
    xy:DiracGamma[_[_]].. ,DiracGamma[7], c___] :=
drS[b, DiracGamma[v[w],di], xy, DiracGamma[7], c] /; NumberQ[n] &&
       EvenQ[Length[{xy}]] && noncommQ[mass];

drS[b___,DiracGamma[7],DiracGamma[v_[w__],di___] + (n_. mass_ ),
    xy:DiracGamma[_[__],___].. ,DiracGamma[6], c___] :=
drS[b, DiracGamma[v[w],di], xy, DiracGamma[6], c] /; NumberQ[n] &&
       EvenQ[Length[{xy}]] && noncommQ[mass];

drS[b___,DiracGamma[6],DiracGamma[v_[w__],di___] + (n_. mass_ ),
    xy:DiracGamma[_[__],___].. ,DiracGamma[6], c___] :=
drS[b, DiracGamma[v[w],di], xy, DiracGamma[6], c] /; NumberQ[n] &&
       OddQ[Length[{xy}]] && noncommQ[mass];

drS[b___,DiracGamma[7],DiracGamma[v_[w__],di___] + (n_. mass_),
    xy:DiracGamma[_[__],___].. ,DiracGamma[7], c___] :=
drS[b, DiracGamma[v[w],di], xy, DiracGamma[7], c] /; NumberQ[n] &&
       OddQ[Length[{xy}]] && noncommQ[mass];

dr[b___,DiracGamma[5],c:DiracGamma[_[__],_].. ,d___] :=
   ( (-1)^Length[{c}] ds[ b,c,DiracGamma[5],d ] ) /;
      ($BreitMaison =!= True && $Larin =!= True);

dr[b___,DiracGamma[5],DiracGamma[x_[y__],d_Symbol -4] ,f___] :=
   (ds[ b,DiracGamma[x[y],d-4],DiracGamma[5],f ] ) /;
      ($BreitMaison === True);

dr[b___,DiracGamma[5],DiracGamma[x_[y__],d_Symbol] ,f___] :=
   ( 2 ds[b,DiracGamma[x[y],d-4],DiracGamma[5],f] -
     ds[b,DiracGamma[x[y],d],DiracGamma[5],f] ) /;
      ($BreitMaison === True);

(* gamma[mu] gamma[mu] ---> 4, etc. *)
dr[b___,DiracGamma[LorentzIndex[c_]],
        DiracGamma[LorentzIndex[c_]],d___] := 4 ds[ b,d ];

dr[b___,DiracGamma[LorentzIndex[c_,di_],di_],
        DiracGamma[LorentzIndex[c_,di_],di_],d___] := di ds[ b,d ];

dr[b___,DiracGamma[LorentzIndex[c_,di_],di_],
        DiracGamma[LorentzIndex[c_,di_ -4],di_ -4],d___]:=(di-4) ds[ b,d ];

dr[b___,DiracGamma[LorentzIndex[c_]],
        DiracGamma[LorentzIndex[c_,di_ -4],di_ -4],d___] := 0;

dr[b___,DiracGamma[LorentzIndex[c_]],
        DiracGamma[LorentzIndex[c_,di_ ],di_ ],d___] := 4 ds[ b,d ];

fdim[]=4;    (* fdimdef *)
fdim[dimi_]:=dimi;
dcheck[dii_, diii__] := dimcheck[dii, diii] =
If[Head[dii]===Symbol, True, If[Union[{dii, diii}]==={dii}, True, False]];

dr[b___,DiracGamma[LorentzIndex[c_,dI___],dI___],
        DiracGamma[x_[y__],di1___],
        DiracGamma[LorentzIndex[c_,dI___],dI___],d___
  ] := ( (2-fdim[dI]) ds[b,DiracGamma[x[y],di1],d] ) /; dcheck[dI, di1];

dr[b___,DiracGamma[LorentzIndex[c_,dI___],dI___],
        DiracGamma[x1_[y1__],d1___], DiracGamma[x2_[y2__],d2___],
        DiracGamma[LorentzIndex[c_,dI___],dI___],d___
  ] := ((4 sCO[x1[y1],x2[y2]] ds[b,d] +
         (fdim[dI]-4) ds[b,DiracGamma[x1[y1],d1], DiracGamma[x2[y2],d2], d]
        ) /. sCO -> Pair
       ) /; dcheck[dI, d1, d2];

dr[b___,DiracGamma[LorentzIndex[c_,dI___],dI___],
        DiracGamma[x1_[y1__],d1___], DiracGamma[x2_[y2__],d2___],
        DiracGamma[x3_[y3__],d3___],
        DiracGamma[LorentzIndex[c_,dI___],dI___],d___
  ] := (-2 ds[b,DiracGamma[x3[y3],d3], DiracGamma[x2[y2],d2],
                DiracGamma[x1[y1],d1],
            d] -
        (fdim[dI]-4) ds[b,DiracGamma[x1[y1],d1],
                          DiracGamma[x2[y2],d2],
                          DiracGamma[x3[y3],d3],
                      d]
       ) /; dcheck[dI, d1,d2,d3];
dr[b___,DiracGamma[LorentzIndex[c_,dI___],dI___],
        DiracGamma[x1_[y1__],d1___], DiracGamma[x2_[y2__],d2___],
        DiracGamma[x3_[y3__],d3___], DiracGamma[x4_[y4__],d4___],
        DiracGamma[LorentzIndex[c_,dI___],dI___],d___
   ] := ( 2 ds[b,DiracGamma[x3[y3],d3], DiracGamma[x2[y2],d2],
                   DiracGamma[x1[y1],d1], DiracGamma[x4[y4],d4],
                 d] +
            2 ds[b,DiracGamma[x4[y4],d4], DiracGamma[x1[y1],d1],
                   DiracGamma[x2[y2],d2], DiracGamma[x3[y3],d3],
                 d] +
       (fdim[dI]-4) ds[b,DiracGamma[x1[y1],d1], DiracGamma[x2[y2],d2],
                         DiracGamma[x3[y3],d3], DiracGamma[x4[y4],d4],
                     d]
         ) /; dcheck[dI, d1,d2,d3,d4];
dr[b___,DiracGamma[LorentzIndex[c_,dI___],dI___],
        DiracGamma[x1_[y1__],d1___], DiracGamma[x2_[y2__],d2___],
        DiracGamma[x3_[y3__],d3___], DiracGamma[x4_[y4__],d4___],
        DiracGamma[x5_[y5__],d5___],
        DiracGamma[LorentzIndex[c_,dI___],dI___],d___
  ] := ( 2 ds[b,DiracGamma[x2[y2],d2], DiracGamma[x3[y3],d3],
                DiracGamma[x4[y4],d4], DiracGamma[x5[y5],d5],
                DiracGamma[x1[y1],d1],
            d] -
         2 ds[b,DiracGamma[x1[y1],d1], DiracGamma[x4[y4],d4],
                DiracGamma[x3[y3],d3], DiracGamma[x2[y2],d2],
                DiracGamma[x5[y5],d5],
            d] -
         2 ds[b,DiracGamma[x1[y1],d1], DiracGamma[x5[y5],d5],
                DiracGamma[x2[y2],d2], DiracGamma[x3[y3],d3],
                DiracGamma[x4[y4],d4],
            d] -
      (fdim[dI]-4) ds[b,DiracGamma[x1[y1],d1], DiracGamma[x2[y2],d2],
                        DiracGamma[x3[y3],d3], DiracGamma[x4[y4],d4],
                        DiracGamma[x5[y5],d5],
                    d] ) /; dcheck[dI, d1,d2,d3,d4,d5];

dr[b___,DiracGamma[Momentum[c_,dim1___],___],
        DiracGamma[Momentum[c_,dim2___],___],d___ ] :=
        scev[Momentum[c,dim1],Momentum[c,dim2]] ds[b,d];

dr[ b___,DiracGamma[LorentzIndex[c_]],d:DiracGamma[_[_]].. ,
         DiracGamma[LorentzIndex[c_]],f___ ] :=
    -2 ds @@ Join[ {b},Reverse[{d}],{f} ] /; OddQ[Length[{d}]];

dr[ b___,DiracGamma[Momentum[c__],dim___],
         DiracGamma[Momentum[x__],dii___],
         DiracGamma[Momentum[c__],di___],d___ ] := (
2 scev[Momentum[c],Momentum[x]] ds[b,DiracGamma[Momentum[c],dim],d]
- scev[Momentum[c],Momentum[c]] ds[b,DiracGamma[Momentum[x],dii],d]
                                                  );

(* #################################################################### *)
(*                             Main33                                 *)
(* #################################################################### *)

(* SUNstuff *)
   dr[ a___,b_,c:SUNT[i_].. ,d___] :=
     dr[ a, c, b, d ] /; FreeQ2[b, {SUNT}];

   HoldPattern[dr[ a___,b_ dr[c:(SUNT[_])..], d___]]:=
     ( dr[c] dr[a, b, d] )/;FreeQ[{a, b, d}, SUNT];

   dr[ SUNT[i_], b___ ] := (SUNT[i] ds[b]) /; FreeQ[{b}, SUNT];

   dr[ b__, SUNT[i_] ] := (SUNT[i] ds[b]) /; FreeQ[{b}, SUNT];

   dr[ a__, b:SUNT[_].. ]:=(ds[b] ds[a])/; FreeQ[{a}, SUNT];

   dr[ b:SUNT[_].., a__ ]:=(ds[b] ds[a])/; FreeQ[{a}, SUNT];
(* #################################################################### *)
(*                             Main33a                                 *)
(* #################################################################### *)
   dr[ a___, ChargeConjugationMatrix, ChargeConjugationMatrix, b___ ] :=
     -dr[a, b];
   dr[ a___, ChargeConjugationMatrix, DiracGamma[5], b___ ] :=
     dr[a, DiracGammaT[5], ChargeConjugationMatrix, b];
   dr[ a___, ChargeConjugationMatrix, DiracGamma[6], b___ ] :=
     dr[a, DiracGammaT[6], ChargeConjugationMatrix, b];
   dr[ a___, ChargeConjugationMatrix, DiracGamma[7], b___ ] :=
     dr[a, DiracGammaT[7], ChargeConjugationMatrix, b];

   dr[ a___, ChargeConjugationMatrix, DiracGamma[x_], b___ ] :=
     -dr[a, DiracGammaT[x], ChargeConjugationMatrix, b] /; !NumberQ[x];

   dr[ a___, ChargeConjugationMatrix, DiracGammaT[x_], b___ ] :=
     -dr[a, DiracGamma[x], ChargeConjugationMatrix, b] /; !NumberQ[x];

(* #################################################################### *)
(*                             Main34                                 *)
(* #################################################################### *)

   drCOs[x___] := memset[ drCOs[x], drCO[x] ];    (*drCOsdef*)
(* Dirac contraction rules *) (*drCOdef*)

   drCO[ b___,DiracGamma[LorentzIndex[c_,di_Symbol-4],di_Symbol-4],
         d:DiracGamma[_[_,di_Symbol-4], di_Symbol-4].. ,
         DiracGamma[LorentzIndex[c_,di_Symbol-4],di_Symbol-4],f___
       ]:= (drCO @@  ( { b, DiracGamma[LorentzIndex[c,di-4], di-4],
                         d, DiracGamma[LorentzIndex[c,di-4], di-4],
                         f } /. di -> (di + 4)
                     )) /. di -> (di-4);

   drCO[ b___,DiracGamma[lv_[c_,di_Symbol-4],di_Symbol-4], w___,
              DiracGamma[ww_[y__],dim___],
              DiracGamma[lv_[c_,di_Symbol-4],di_Symbol-4], z___] :=
   (Print["rdCOCheck"];
         -drCO[ b, DiracGamma[lv[c,di-4],di-4],w,
             DiracGamma[lv[c,di-4],di-4],
             DiracGamma[ww[y],dim],z
        ] + 2 drCO[b, DiracGamma[ww[y],di-4], w,z] )/.drCO->ds;


   drCO[ b___,DiracGamma[LorentzIndex[c_]],d:DiracGamma[_[__]].. ,
         DiracGamma[x_[y__]],DiracGamma[LorentzIndex[c_]],f___ ] :=
       ( 2 ds @@ Join[ {b},Reverse[{d}],{DiracGamma[x[y]],f} ] +
         2 ds[ b,DiracGamma[x[y]],d,f ]
        ) /; OddQ[Length[{d}]];


   drCO[ b___,DiracGamma[c_, di___],d:DiracGamma[_[__],___].. ,
         DiracGamma[c_,dim___],f___
       ] :=
        Block[ {drCOij, drCOld = Length[{d}]},
     (-1)^drCOld scev[c,c] ds[b,d,f]
     + 2 Sum[(-1)^(drCOij+1) coneins[ Pair[c,{d}[[drCOij,1]] ]
            * ds@@Join[{b},Drop[{d},{drCOij,drCOij}],{DiracGamma[c,dim],f}]
                                    ],{drCOij,1,drCOld}
            ]
              ]/;((Length[{d}]>0)&&FreeQ[c,LorentzIndex]&&
                 (!NumberQ[c]) && !MatchQ[{di}, {_Symbol -4}]);

(* #################################################################### *)
(*                             Main35                                 *)
(* #################################################################### *)

   drCO[ b___,DiracGamma[LorentzIndex[c_,di_Symbol],di_Symbol],
         d:DiracGamma[_[_,dim___],dim___].. ,
         DiracGamma[LorentzIndex[c_,di_Symbol],di_Symbol],f___
       ]:=
   Block[{idrCO,jdrCO,lddrCO = Length[{d}]},
        (-1)^lddrCO ( di - 2 lddrCO ) ds[b,d,f] -
          4 (-1)^lddrCO  Sum[ (-1)^(jdrCO-idrCO) *
         coneins[ Pair[{d}[[idrCO,1]],{d}[[jdrCO,1]] ] *
                  ds@@Join[ {b},Drop[ Drop[{d},{idrCO,idrCO}],
                                     {jdrCO-1,jdrCO-1}
                                    ],{f}
                          ]
                ],
                       {idrCO,1,lddrCO-1},{jdrCO,idrCO+1,lddrCO}
                            ]/.Pair->scev
         ] /;(Length[{d}]>5);

   drCO[ b___,DiracGamma[lv_[c_,dim___],dim___],
              DiracGamma[vl_[x__],dii___],d___,
              DiracGamma[lv_[c_,di___],di___],f___
       ]:=(-ds[b, DiracGamma[vl[x],dii],
                  DiracTrick[DiracGamma[lv[c,dim],dim],d,
                     DiracGamma[lv[c,di],di]], f
                ] + 2 coneins[Pair[vl[x], lv[c,dim]] *
                              ds[ b,d,DiracGamma[lv[c,di],di],f ]
                             ]
           ) /; {dim} =!= {di};

(* ************************************************************** *)
 SetAttributes[drS,Flat];
(* ************************************************************** *)
 SetAttributes[dr,Flat];   (* quite important!!! *)
(* ************************************************************** *)

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracTrick | \n "]];
Null
(* :Title: Divideout *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Divideout is an option for several functions *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Divideout`",
             "HighEnergyPhysics`FeynCalc`"];

Divideout::usage =
"Divideout is an option for OPEInt and OPEInsert.
The setting is divided out at the end.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Divideout | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DotPower *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: DotPower *)

(* :Package Version 2.1 *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DotPower`",
             "HighEnergyPhysics`FeynCalc`"];

DotPower::usage =
"DotPower is an option for DotSimplify. It determines whether
non-commutative powers are represented by succesive multiplication
or by Power.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DotPower | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DotSimplify *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: last changed July 19th 2000 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: DotSimplify *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DotSimplify`",
             "HighEnergyPhysics`FeynCalc`"];


DotSimplify::usage =
"DotSimplify[expr] expands and reorders noncommutative terms in expr.
Simplifying relations may be specified by the option
DotSimplifyRelations or by Commutator and AntiCommutator definitions.
Whether expr is expanded noncommutatively depends
on the option Expanding.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ Commutator, CommutatorExplicit, DotPower, AntiCommutator,
DOT, Expanding, FreeQ2, NonCommFreeQ,
DotSimplifyRelations, MemSet, SUNT,
SUNTrace, DiracGamma, QuantumField];

DotSimplify[a__, z_/;Head[z] =!= Rule, ___Rule] :=
soso /; Message[DotSimplify::argrx, DotSimplify, Length[{a}]+1, 1];

DotSimplify[___Rule] :=
soso /; Message[DotSimplify::argrx, DotSimplify, 0, 1];

DotSimplifyRelation = DotSimplifyRelations;

Options[DotSimplify] = {Expanding -> True, DotSimplifyRelations -> {},
                        DotPower -> True};

DotSimplify[xx_, opts___Rule] := Block[
 {pid, ex, ne, dlin,dlin0, x, DOTcomm, cru, aru, commm, acommm, acom, cdoot,
  sdoot,simpf, actorules, ctorules, acomall, comall, simrel, dootpow,
  dotpower,tic, dodot
 },
(* *)

simrel = DotSimplifyRelations /. {opts} /. Options[DotSimplify];
dotpower = DotPower /.  {opts} /. Options[DotSimplify];
simrel = simrel /. Power[aa_, bb_Integer?Positive] :>
   (DOT @@ Table[aa, {ijij, bb}]);
x = Catch[
If[simrel =!= {},
(* CHANGE 08/94 *)
   sru[aa_  :> bb_] :=
   (RuleDelayed @@ {sdoot@@Join[{Pattern[xxX, BlankNullSequence[]]},
                                 If[Head[aa]===DOT,
                                    aa /. DOT -> List,
                                    {aa}],
                                {Pattern[yyY, BlankNullSequence[]]}
                               ]
                    ,
                    sdoot[xxX, Hold[bb], yyY]
                  } /. sdoot[] -> 1 /. sdoot -> DOT /. Hold[bb]:> bb
   );
   sru[(aa_ /; FreeQ[aa, Pattern]) -> (bb_ /; FreeQ[bb, Pattern])] :=
   (RuleDelayed @@ {sdoot@@Join[{Pattern[xxX, BlankNullSequence[]]},
(*
                                 Flatten[{aa} /. DOT -> List],
*)
                                 If[Head[aa]===DOT,
                                    aa /. DOT -> List,
                                    {aa}],
                                {Pattern[yyY, BlankNullSequence[]]}
                               ]
                    ,
                    sdoot[xxX, bb, yyY]
                  } /. sdoot[] -> 1 /. sdoot -> DOT
   );
 simrel = Map[sru, simrel];
  ];

If[CheckContext["Commutator"] || CheckContext["AntiCommutator"],
   If[(!FreeQ[xx, Commutator]) || (!FreeQ[xx, AntiCommutator]),
      x = CommutatorExplicit[xx],
      x = xx
     ], x = xx
  ];

(* CHANGE 07/26/94 *)
If[!FreeQ[x, SUNT],
   SetAttributes[TimesDot, HoldAll];
   TimesDot[a__] := If[FreeQ[{a}, SUNT], Times[a], DOT[a]];
   x = x /. Times -> TimesDot
  ];

(*
If[CheckContext["LeftRightPartialD"], x = ExplicitPartialD[x]];
*)

ex = Expanding /. {opts} /. Options[DotSimplify];
(*  maybe this is somewhat slow;  use FORM then ... *)
If[!FreeQ[x, (a_/;!FreeQ2[a, $NonComm])^n_Integer?Positive],
   x = x /. {(a_/;!FreeQ2[a, $NonComm])^n_Integer?Positive :>
             DOT @@ Table[a, {n}]
            };
  ];

(* check special case *)
If[simrel === {},
   vars = Union[Variables[Cases[xx, _, Infinity] ]];
   If[Union[Map[DataType[#, NonCommutative]&, vars]] === {True},
      If[FreeQ2[{DownValues[Commutator], DownValues[AntiCommutator]},
                vars
               ],
(* that means : just expansion, no acomms, comms *)
         x = Distribute[x /. DOT -> doot] //.
                  doot[a___, n_?NumberQ b_, c___] :> (n doot[a, b, c]);
         Throw[x /. doot -> dootpow]
        ]
     ]
  ];


pid[u_,_] := u;

cru[{commm[a_ /; FreeQ[a, Pattern],
           b_ /; FreeQ[b, Pattern]
          ],
     ww_
    }
   ] := (RuleDelayed @@ {cdoot[
    Pattern[xxX, BlankNullSequence[]], a, b,
    Pattern[yyY, BlankNullSequence[]]
                              ],
    cdoot[xxX, ww, yyY] + cdoot[xxX, b, a,  yyY
                             ]
                        } /.  cdoot[]-> 1 /. cdoot -> DOT
        );

cru[{commm[a_ /; !FreeQ[a, Pattern],
           b_ /; !FreeQ[b, Pattern]
          ],
     ww_
    }] := (RuleDelayed @@
                            {cdoot[
   Pattern[xxX, BlankNullSequence[]], a, b, Pattern[yyY, BlankNullSequence[]]
                                  ],
   condition[
   cdoot[xxX, ww, yyY] + cdoot[xxX, b/.Pattern -> pid,
                                   a/.Pattern -> pid ,  yyY
                             ]
             , (!orderedQ[{a /. Pattern :> pid,
                               b /. Pattern :> pid}])
            ]
                            } /.  cdoot[]-> 1 /. cdoot -> DOT
          ) /.
          { orderedQ :> OrderedQ, condition :> Condition};


aru[{acommm[a_ /; FreeQ[a, Pattern],
            b_ /; FreeQ[b, Pattern]
          ],
     ww_
    }] := (RuleDelayed @@ {cdoot[
   Pattern[xxX, BlankNullSequence[]], a, b,
   Pattern[yyY, BlankNullSequence[]]
                                  ],
   cdoot[xxX, ww, yyY] - cdoot[xxX, b, a,  yyY
                             ]
                          } /.  cdoot[]-> 1 /. cdoot -> DOT
          );
aru[{acommm[a_ /; !FreeQ[a, Pattern],
     b_ /; !FreeQ[b, Pattern]], ww_ }] :=
{
  (RuleDelayed @@ {cdoot[ Pattern[xxX, BlankNullSequence[]], a, b,
   Pattern[yyY, BlankNullSequence[]] ],
   condition[ 1/2 cdoot[xxX, ww, yyY],
              sameQ[a /. Pattern :> pid, b /. Pattern :> pid]
            ]
                  } /.  cdoot[]-> 1 /. cdoot -> DOT
  ) /. {sameQ :> SameQ, condition :> Condition},
    (RuleDelayed @@
                            {cdoot[
   Pattern[xxX, BlankNullSequence[]], a, b, Pattern[yyY, BlankNullSequence[]]
                                  ],
   condition[
   cdoot[xxX, ww, yyY] - cdoot[xxX, b/.Pattern -> pid,
                                    a/.Pattern -> pid ,  yyY
                             ]
             , (!orderedQ[{a /. Pattern :> pid, b /. Pattern :> pid}])
            ]
                            } /.  cdoot[]-> 1 /. cdoot -> DOT
    ) /.  {orderedQ :> OrderedQ, condition :> Condition}
 };

cotorules[{}] = {};
cotorules[a__List] := (cotorules[a] =
                      Select[Map[cru,
 a /. Commutator -> commm /.
      {HoldPattern :> Identity, HoldPattern :> Identity} /.
       RuleDelayed -> List      ], FreeQ[#, cru]&]
                      );

actorules[{}] = {};
actorules[a__List] :=
(*actorules[a] = *) Block[{tt},
tt = a /. AntiCommutator -> acommm;
tt = tt /. {HoldPattern :> Identity, HoldPattern :> Identity};
tt = tt /. RuleDelayed -> List;
tt = Select[Map[aru, tt], FreeQ[#,aru]&];
                         tt];

(* first the commutators, then the anticommutators *)
comall[ yy__ ] := yy //. Flatten[cotorules[DownValues@@{Commutator}]];
acomall[ yy__ ]:= yy //. Flatten[actorules[DownValues@@{AntiCommutator}]];

DOTcomm[] = 1;
(* there might be either explicit commutators or anticommutators
   to be inserted, or use: comall, acomall to make use of DownValues.
*)
 Off[Rule::rhs];
If[simrel === {},
   DOTcomm[xy__] := FixedPoint[acomall,
                                FixedPoint[comall, DOT[xy], 242], 242
                               ]
  ,
   DOTcomm[xy__] := FixedPoint[acomall,
                                FixedPoint[comall, DOT[xy]//.simrel , 242
                                          ] //. simrel, 242
                               ] //. simrel
  ];


If[ex === True,
(*
   dlin[a___, b_Plus , c___] := Map[dlin[a,#,c]&,b] /;!FreeQ2[b, $NonComm];
*)

dlin0[a___] := (Distribute[dlin[a]] //. dlin[h___, n_Integer c_, b___] :>
                                       (n dlin[h, c, b])
               );
  ];
(*
dlin[a___, n_Integer c_, b___] := n dlin[a, c, b];
*)
dlin[] = 1;
dlin1[{ok___}, b_/;DataType[b, NonCommutative], c___] :=
   dlin1[{ok, b}, c];
dlin1[{ok___},(n_?NumberQ) b_/;DataType[b, NonCommutative], c___] :=
 n dlin1[{ok, b}, c];
dlin1[{ok___},b_, c___] := If[NonCommFreeQ[b] === True && FreeQ[b, dlin1],
                              b dlin1[{ok}, c],
                              If[Head[b] === Times,
                                 If[Select[b, NonCommFreeQ[#]&] =!= 1,
                                    Select[b, NonCommFreeQ[#]&]*
                                    dlin1[{ok, Select[b,
                                               !NonCommFreeQ[#]&]}, c],
                                    dlin1[{ok},b[[1]]] *
                                    dlin1[{},Rest[b],c]
                                   ],
                                 dlin1[{ok,b},c]
                                ]
                             ];

If[FreeQ[Attributes @@ {DOT}, Flat],
   x = FixedPoint[(# /. DOT -> dlin0/. dlin0 -> dlin //. dlin[a__] :>
                  dlin1[{}, a] //.
                   dlin1[{ookk___}] :> DOT[ookk] //.
                   DOT[aa___, DOT[b__], c___] :>
                   DOT[aa, b, c] /. DOT -> DOTcomm
                  )&, x,  123
                 ] /. dlin -> DOT,

simpf[y_] := MemSet[simpf[y],
                  (y /. DOT -> dlin0 /. dlin0 -> dlin  //.
                  dlin[a__] :> dlin1[{}, a] //.
                   dlin1[{ookk___}] :> DOT[ookk] /. DOT -> DOTcomm
                  ) /. dlin -> DOT
                 ];
x = FixedPoint[simpf, x, 123];

(*
 x = FixedPoint[(# /. DOT -> dlin0 /. dlin0 -> dlin //. dlin[a__] :>
                 dlin1[{}, a] //.
                 dlin1[{ookk___}] :> DOT[ookk] /. DOT -> DOTcomm
                )&, x, 123
               ] /. dlin -> DOT
*)

  ];

x];

If[CheckContext["SUNTrace"],
   If[!FreeQ[x, SUNTrace],
      x = x  //. {DOT[a___,b_SUNTrace,c___] :> (b  DOT[a,c]) ,
                  DOT[a___,b1_SUNTrace - b2_SUNTrace, c___] :>
                  (b1 DOT[a,c] - b2 DOT[a,c])
                 }

     ]
  ];

If[!FreeQ[x, SUNT],
   x  = x /. {DOT[a__DiracGamma,b__SUNT, c___]:>
               DOT[b, a, c],
(*
               DOT[a, c, b],
*)
              DOT[a__, DiracTrace[b__]] :>
               DOT[a] DiracTrace[b] /; FreeQ[{a},DiracGamma]
             }
  ];

(*CHANGE 03/98 *)
If[!FreeQ[x, QuantumField],
   x = x /. DOT->dodot //.
            {dodot[a___,b_/;Head[b] =!= SUNT, c__SUNT,d___] :>
              dodot[a,c,b,d]
             } /. dodot->DOT;
   x = x /. DOT[a__SUNT, b__QuantumField] :> (DOT[a]*DOT[b])
  ];

(*
If[!FreeQ[x, SUNT],
   x  = x /. DOT[a__DiracGamma, b__SUNT] :> (DOT[a] DOT[b])
  ];
If[!FreeQ[x, SUNT],
   x  = x /. DOT[b__SUNT, a__DiracGamma] :> (DOT[a] DOT[b])
  ];
*)

dootpow[a__] := If[FreeQ2[{a}, {DiracGamma,SUNT}],
                   Apply[DOT, (#[[1]]^Length[#])& /@ Split[{a}]],
                   DOT[a]
                  ];

If[dotpower === True,
   x = x /. DOT -> dootpow /. dootpow -> DOT
  ];
x
];

If[MemberQ[$ContextPath,"HighEnergyPhysics`Tarcer`"],
   MakeBoxes[HighEnergyPhysics`Tarcer`SEpsilon[d_]^(n_), fmt_] :=
   InterpretationBox @@
    {StyleBox[SubsuperscriptBox["S", ToBoxes[First[Variables[d]], fmt], n],
      FontWeight -> "Bold"], SEpsilon[d], Editable -> False}
  ];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DotSimplify | \n "]];
Null

(* :Title: DotSimplifyRelations *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: DotSimplifyRelations *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DotSimplifyRelations`",
             "HighEnergyPhysics`FeynCalc`"];

DotSimplifyRelations::usage =
"DotSimplifyRelations is an option for DotSimplify.
Its setting may be a list of substition rules of the form
DotSimplifyRelations -> {a . b -> c, b^2 -> 0, ...}.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DotSimplifyRelations | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DummyIndex *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DummyIndex`",
             "HighEnergyPhysics`FeynCalc`"];

DummyIndex::usage =
"DummyIndex is an option CovariantD."

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DummyIndex | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Eps *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Eps is the head of Levi-Civita Tensor *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Eps`",
             "HighEnergyPhysics`FeynCalc`"];

Eps::usage =
"Eps[a, b, c, d] is the head of the totally antisymmetric epsilon
(Levi-Civita) tensor. The \"a,b, ...\" should have head
LorentzIndex or Momentum or Integer.
In case of integers the Levi-Civita tensor is evaluated immediately.
Eps has an option Dimension (default 4).
As alternative input LeviCivita[mu,nu, ...][p,q,...] can be used.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[ChangeDimension];
dimension := dimension = MakeContext["Dimension"];
lor   := lor = MakeContext["LorentzIndex"];
mom   := mom = MakeContext["Momentum"];

Options[Eps] = {dimension -> 4};

(*
Eps[a__, dimension->n_] := Eps@@ChangeDimension[{a},n];
*)

Eps[a__Symbol, ru___Rule] := (Signature[{a}] (Eps @@ Sort[{a}])) /;
                             Signature[{a}] =!= 1;

Eps[a__Integer, ru___Rule] := Signature[{a}];

Eps[a___, n1_. lor[mu_,___], b___, n2_. lor[mu_,___],c___ ] := 0 /;
 NumberQ[n1 n2];
Eps[a___, n1_. mom[mu_,___], b___, n2_. mom[mu_,___],c___ ] := 0 /;
 NumberQ[n1 n2];
Eps[x__] :=  0 /; ((!FreeQ[{x}, lor[_,_Symbol -4]]) ||
                   (!FreeQ[{x}, mom[_,_Symbol -4]]) );

Eps[a___, lor[mu_,_Symbol], b___, ru___Rule] :=
  (Eps@@ {a, lor[mu], b, ru}) /; (dimension /.{ru} /.
                                  Options[Eps])===4;
Eps[a___, mom[mu_,_Symbol], b___, ru___Rule] :=
  (Eps@@ {a, mom[mu], b, ru}) /; (dimension /.{ru} /.
                                  Options[Eps])===4;
   Eps /:
   MakeBoxes[Eps[x__] ,TraditionalForm] :=
   SuperscriptBox["\[Epsilon]", Tbox[x]];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Eps | \n "]];
Null
(* :Title: EpsContract *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: EpsContract *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`EpsContract`",
             "HighEnergyPhysics`FeynCalc`"];

EpsContract::usage=
"EpsContract is an option of Contract specifying whether Levi-Civita
tensors Eps[...] will be contracted, i.e., products
of two  Eps are replaced via the determinant formula.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "EpsContract | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: EpsDiscard*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`EpsDiscard`",
             "HighEnergyPhysics`FeynCalc`"];


EpsDiscard::usage=
"EpsDiscard is an option for FeynCalc2FORM and SquareAmplitude.
If set to True all
Levi-Civita tensors are replaced by 0 after contraction.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "EpsDiscard | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: EpsEvaluate *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: simplification of Eps *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`EpsEvaluate`",
             "HighEnergyPhysics`FeynCalc`"];

EpsEvaluate::usage =
"EpsEvaluate[expr] applies total antisymmetry and
linearity (w.r.t. Momentum's) to all Levi-Civita tensors (Eps's) in expr .";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[ Cases2, Eps , Expanding,
ExpandScalarProduct, LorentzIndex, Momentum, Pair, PairContract];

EpsEvaluate[x_] := x /; FreeQ[x,Eps];     (*EpsEvaluatedef*)
EpsEvaluate[x_] := Block[{nx,cx, tx, rud},
 If[LeafCount[x] < 1000,    x//.Eps->Epsev,
    cx = Cases2[x, Eps];
    tx = Dispatch[Thread[rud[cx, cx//.Eps->Epsev]] /. rud->RuleDelayed];
    x/.tx
   ]                    ];


Epsev[A__] := ( Expand /@ (Distribute[Dot[A]]//
                    ExpandScalarProduct) )/.
              Dot->Epsevlin/.Epsevlin->Epsevantilin;
Epsevlin[a___,b_ c_Momentum,d___] := b Epsevlin[a,c,d];
Epsevlin[a___,b_ c_LorentzIndex,d___] := b Epsevlin[a,c,d];
Epsevantilin[a__] := Signature[{a}] Eps@@Sort[{a}];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "EpsEvaluate | \n "]];
Null
(* :Title: EpsilonOrder *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: EpsilonOrder *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`EpsilonOrder`",
             "HighEnergyPhysics`FeynCalc`"];

EpsilonOrder::usage =
"EpsilonOrder is an option of OPEIntDelta and RHI. The setting
determines the order n (Epsilon^n) which should be kept.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "EpsilonOrder | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ExpandScalarProduct *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: ExpandScalarProduct expands scalar products *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`ExpandScalarProduct`",
             "HighEnergyPhysics`FeynCalc`"];

ExpandScalarProduct::usage =
"ExpandScalarProduct[expr]  expands scalar products of sums of
momenta in expr.
ExpandScalarProduct[x, y] expands ScalarProduct[x, y], where
x and y may contain sums. ExpandScalarProduct does not use Expand on
expr.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];




fci  := fci                  = MakeContext["FeynCalcInternal"];
lorentzindex := lorentzindex = MakeContext["LorentzIndex"];
memset := memset             = MakeContext["MemSet"];
momentum := momentum         = MakeContext["Momentum"];
momentumexpand := momentumexpand = MakeContext["MomentumExpand"];
sCO := sCO                   = MakeContext["PairContract"];
pair := pair                 = MakeContext["Pair"];

Options[ExpandScalarProduct] = {fci -> True};

(* since one never can remember this function  ...*)

FRH = FixedPoint[ReleaseHold, #]&;

(*
(* schwachsinn *)
ExpandScalarProduct[x_] := If[LeafCount[x]<42,
     Expand[FixedPoint[pairexpand1,fci[x], 3]//momentumexpand ],
            FixedPoint[pairexpand1,fci[x], 3]//momentumexpand
                              ];
*)
ExpandScalarProduct[x_,ru___Rule] :=
If[(fci /. {ru} /. Options[ExpandScalarProduct]),
   FixedPoint[pairexpand1,fci[x], 3]//momentumexpand,
   FixedPoint[pairexpand1, x, 3]//momentumexpand
  ];

ExpandScalarProduct[x_, y_ /;Head[y] =!= Rule] := scev[x, y];

pairexpand1[x_]:=  x/.pair->scevdoit;
(*
pairexpand[x_] :=  x/.pair->scev;  (*pairexpanddef*)
*)

(* not always a good idea (IFPD)
scev[x_,y_]:= memset[ scev[x,y], scevdoit[x,y] ];
*)
scev = scevdoit;
scevdoit[x_,y_] := Distribute[ sceins@@
                              ( Expand[ momentumexpand/@{x,y} ] )
                             ]/.sceins->sczwei/.
                             sczwei(*->sCO/.sCO*)->pair;

sceins[0,_]:=0;                               (*sceinsdef*)
sceins[a_lorentzindex b_, c_] := b sceins[a, c];
sceins[a_momentum b_, c_] := b sceins[a, c];
sczwei[ _[_],_[_,_Symbol-4] ]:=0;             (*sczweidef*)
sczwei[ v_[x_,di_Symbol-4],w_[y_,di_Symbol] ]:= sczwei[v[x,di-4],w[y,di-4]];
sczwei[ w_[y_,di_Symbol],v_[x_] ]:=sczwei[ v[x],w[y] ];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ExpandScalarProduct | \n "]];
Null
(* :Title: Expanding *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Expanding *)

(* :Package Version 2.1 *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Expanding`",
             "HighEnergyPhysics`FeynCalc`"];

Expanding::usage =
"Expanding is an option for Calc, Contract, DiracSimplify, SUNSimplify, etc..
As option for Contract it specifies whether expansion w.r.t.
LorentzIndex is done BEFORE contraction. \n
If set to False in DiracSimplify or SUNSimplify,
only a limited set of simplifications
(multiplicative linearity etc.) is
performed in DiracSimplify.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Expanding | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`ExplicitLorentzIndex`",
             "HighEnergyPhysics`FeynCalc`"];

ExplicitLorentzIndex::usage=
"ExplicitLorentzIndex[ind] is an explicit Lorentz index, i.e., ind is
an integer.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[ExplicitLorentzIndex];

SetAttributes[ExplicitLorentzIndex, Constant ];

ExplicitLorentzIndex /:
   MakeBoxes[ ExplicitLorentzIndex[p_Integer, in___], TraditionalForm
            ] := p;

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ExplicitLorentzIndex | \n "]];
Null
(* :Title: ExtraFactor *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: ExtraFactor *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`ExtraFactor`",
             "HighEnergyPhysics`FeynCalc`"];

ExtraFactor::usage=
"ExtraFactor is an option for SquareAmplitude and FermionSpinSum.
The setting
ExtraFactor -> fa  multiplies the whole amplitude with the
factor fa before squaring.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ExtraFactor | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FAD*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FAD`",
             "HighEnergyPhysics`FeynCalc`"];

FAD::usage= "FAD[q, q-p, ...] denotes 1/(q^2 (q-p)^2 ...).
FAD[{q1,m}, {q1-p,m}, q2, ...] is
1/( (q1^2 - m^2) ( (q1-p)^2 - m^2 ) q2^2 ... ).
(Translation into FeynCalc internal form is performed by
FeynCalcInternal.)";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[Dimension];

Options[FAD] = {Dimension -> D};



ff[{y_,z_}] := SequenceForm["[",y^2, "-", z^2,"]"];

(*
ff[{y_,z_}] := SequenceForm[y^2, "-", z^2];
*)

ff[y_/;Head[y]=!=List] := SequenceForm["[",y^2,"]"];

FAD/:
    MakeBoxes[FAD[a__], TraditionalForm
             ] := ToBoxes[1/ Apply[Times,Map[ff, {a}]
                                  ],
                          TraditionalForm];
End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FAD | \n "]];
Null


(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FeynCalcForm *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 24 November '98 at 16:48 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: FeynCalcForm[expr] formats expr in a short form.
             In FeynCalc.m  $PrePrint can be set to
             $PrePrint = FeynCalcForm
*)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FeynCalcForm`",
             "HighEnergyPhysics`FeynCalc`"];

FCF::uasge=
"FCF is a short form for FeynCalcForm.";

FeynCalcForm::usage=
"FeynCalcForm[expr] changes the printed output to a an easy to read
form. Whether the result of FeynCalcForm[expr] is displayed
or not, depends on the setting of $PrePrint.
$PrePrint = FeynCalcForm forces displaying everything
after applying FeynCalcForm. In order to change to the normal
(internal) Mathematica OutputForm, do: ($PrePrint=.) .
The default setting of $PrePrint is HoldForm ";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


FCF = FeynCalcForm;

bra = "(";
ket = ")";

ca           := ca = MakeContext["CA"];
cf           := cf = MakeContext["CF"];
chiralityprojector := chiralityprojector =
                                MakeContext["ChiralityProjector"];
DeltaFunction:=DeltaFunction = MakeContext["DeltaFunction"];
dimension    := dimension     = MakeContext["Dimension"];
diracgamma   := diracgamma    = MakeContext["DiracGamma"];
diracmatrix  := diracmatrix   = MakeContext["DiracMatrix"];
DiracSigma   := DiracSigma    = MakeContext["DiracSigma"];
diracslash   := diracslash    = MakeContext["DiracSlash"];
diractrace   := diractrace    = MakeContext["DiracTrace"];
DiracGammaT  := DiracGammaT   = MakeContext["DiracGammaT"];
dot          := dot           = MakeContext["DOT"];
eps          := eps           = MakeContext["Eps"];
Epsilon      := Epsilon       = MakeContext["Epsilon"];
FinalSubstitutions :=FinalSubstitutions = MakeContext["FinalSubstitutions"];
freeq2       := freeq2        = MakeContext["FreeQ2"];
Gstrong      := Gstrong        = MakeContext["Gstrong"];
propagatordenominator := propagatordenominator =
                                MakeContext["PropagatorDenominator"];
feynampdenominator := feynampdenominator =
                                MakeContext["FeynAmpDenominator"];
fourvector   := fourvector    = MakeContext["FourVector"];
GluonOperator := GluonOperator= MakeContext["Twist2GluonOperator"];
lorentzindex := lorentzindex  = MakeContext["LorentzIndex"];
levicivita   := levicivita    = MakeContext["LeviCivita"];
Lower        := Lower         = MakeContext["Lower"];
metrictensor := metrictensor  = MakeContext["MetricTensor"];
momentum     := momentum      = MakeContext["Momentum"];
NumericalFactor:= NumericalFactor=MakeContext["NumericalFactor"];
OPEDelta     := OPEDelta      = MakeContext["OPEDelta"];
OPEi         := OPEi          = MakeContext["OPEi"];
OPEj         := OPEj          = MakeContext["OPEj"];
OPEk         := OPEk          = MakeContext["OPEk"];
OPEm         := OPEm          = MakeContext["OPEm"];
OPESum       := OPESum        = MakeContext["OPESum"];
pair         := pair          = MakeContext["Pair"];
partial      := partial       = MakeContext["PartialD"];
field        := field         = MakeContext["QuantumField"];
polarization := polarization  = MakeContext["Polarization"];
polarizationvector := polarizationvector =
                                MakeContext["PolarizationVector"];
Power2       := Power2        = MakeContext["Power2"];
QCDScalemu   := QCDScalemu    = MakeContext["QCDScalemu"];
PlusDistribution := PlusDistribution =
                                MakeContext["PlusDistribution"];
RHO          := RHO           = MakeContext["RHO"];
RHI          := RHI           = MakeContext["RHI"];
scalarproduct:= scalarproduct = MakeContext["ScalarProduct"];
Sn           := Sn            = MakeContext["Sn"];
spinor       := spinor        = MakeContext["Spinor"];
sundelta     := sundelta      = MakeContext["SUNDelta"];
sund         := sund          = MakeContext["SUND"];
sunf         := sunf          = MakeContext["SUNF2"];
sunF         := sunF          = MakeContext["SUNF"];
sunindex     := sunindex      = MakeContext["SUNIndex"];
sunt         := sunt          = MakeContext["SUNT"];
suntrace     := suntrace      = MakeContext["SUNTrace"];
tf           := tf            = MakeContext["Tf"];
nf           := nf            = MakeContext["Nf"];
Zeta2        := Zeta2         = MakeContext["Zeta2"];
Upper        := Upper         = MakeContext["Upper"];

Options[FeynCalcForm] = {FinalSubstitutions -> {}};

(* for future changes ... *)
cdf = Symbol["CommonDefaultFormatTypes"];

FeynCalcForm[x_,opts___] :=
If[$Notebooks === True,
   If[$PrePrint=== FeynCalcForm,
      If[MemberQ[{TraditionalForm, StandardForm, InputForm},
                  "Output" /. (
                  cdf /.
                  Options[$FrontEnd, "CommonDefaultFormatTypes"])
                ]
         ,
         Unset[$PrePrint]; (*Print["UNSETTED"]; *)x
         ,
(*i.e., in OutputForm one can have $PrePrint=FeynCalcForm *)
         feynCalcForm[x,opts]
        ],
      x
     ]
   ,
   feynCalcForm[x,opts]
  ];

(* timefixdef : a more physics - like timing function *)
tim[a_, b_] := If[$VersionNumber > 2.2 && $Notebooks===True,
        SequenceForm[StringInsert[ToString[Floor[10 a]],".",-2]," ",b],
                  a b
                 ];

(* due to Dave Withoff ... *)
feynCalcForm[InputForm[f_]]:=InputForm[f];

SetAttributes[feynCalcForm, HoldAll];
SetAttributes[FeynCalcForm, HoldAll];
(*Unprotect[TimeUsed];*)

(*TimeUsed /:*) HoldPattern[feynCalcForm[TimeUsed[]]] :=
                timefix[TimeUsed[]];
(*
Protect[TimeUsed];
*)

timefix[n_]:= Which[ 0.<=n<0.02,      tim[" < 0.02","s"],
                     0.02<=n<9.5,   tim[N[n,2], "s"],
                     9.5<=n<59.5,  tim[N[n,2], "s"],
                     59.5<=n<600,  tim[N[n/60,2], "min"],
                     600<=n<3570,  tim[N[n/60,2], "min"],
                     3569<n<36000, tim[N[n/3600,2], "h"],
                     36000<n,      tim[N[n/3600,4], "h"]
                   ];
sunfuser[a_,b_,c_,___]:=fsunU[a, b, c]/.fsunU->"f";
sumst[x_Plus]:=SequenceForm["(",x,")"];  sumst[y_]:=y;


diracsldi[di_][x__,dimension -> di_] :=
   diracslash[x, dimension -> di];
diracmadi[di_][x__,dimension -> di_] :=
   If[!FreeQ[{x}, Rule], diracmatrix[x],
      diracmatrix[x, dimension -> di]];
diracmadi[di_][x__] :=
   If[!FreeQ[{x}, Rule],diracmatrix[x],
      diracmatrix[x, dimension -> di]];
iDentity[a_,___] := a;
sunident[a_] := a;

didm[x_,y___]:=x;
didl[x_,y___]:=x;
   Format[fcdot2[a_,b__]] := Infix[fcdot2[a,b], " ", 320];
fcdot2[x-y,x-rd]
   Format[fcdot2[a_]] := a;

diF[x_-4]:=StringJoin[ToString[x],"-4"];
diF[x_]:=x;

double[{a___, x_, x_, b___}] := {a,x,x,b};

dea[yy__]     := double[Map[denfa,{yy}]] /. double -> Identity;

denfa[_[Subscripted[x_[s_]],0]] := SequenceForm["(",x[s]^2,")"];

denfa[_[momentum[Subscripted[x_[s_]],___],0]] :=
      SequenceForm["(",x[s]^2,")"];

denfa[_[x_]] := SequenceForm["(",x^2,")"];

denfa[_[x_,0]] := SequenceForm["(",x^2,")"];

denfa[_[x_,y_]] := SequenceForm["(",x^2,"- ",y^2,")"];

feynden[x__]    := 1 / fcdot2 @@ ( dea @@ {x} );
ditr[x_,___]    := "tr"[x];

fdprop[a__]   := 1 / denfa[dudu[a]];
compind[a_]     := If[Head[a] === Symbol,
                   StringJoin[ToString[a],"*"], a "*"];
myscriptsbox[x_] := x;

SetAttributes[sub, HoldAll];
sub[a_String, b_] := If[CheckContext[a], MakeContext[a] :> b, {}];

CC[x_]     := CheckContext[x];
CC[x_,y__] := CheckContext[x] && CC[y];

(* change as a side effect the ordering Attribute of Plus and Times,
   but reinstall it again at the end.
*)
  epsd[a___, (b_/;(Head[b] ===lorentzindex) ||
                  (Head[b] === momentum)
                 )[c_,di_], d___] :=
      Subscripted["eps"[di//diF]][a,b[c,di],d];
  epsd[a__] := "eps"[a];

(* display only one dimension (for readability) *)
ni[di_]:=ToString[di];
ni[di__]:=ToString[{di}[[1]]];


diracslm[a_] := diracslash[a];
diracslm[a_, rul___Rule] := diracslash[a, rul];
diracslm[a_, b__, rul_Rule] := SequenceForm @@
                                Map[diracslash[#, rul]&, {a, b}];
diracslm[a_, b__] := SequenceForm @@ Map[diracslash[#]&, {a, b}];

feynCalcForm[x_,opt___Rule]:=Block[{xxxx = Evaluate[x], subs},
                 subs = FinalSubstitutions /. {opt} /. Options[FeynCalcForm];
                  xxxx = xxxx /. subs;
                  xxxx = xxxx/.(n_Real Second)->timefix[n];
                  xxxx = (xxxx/.
         dot:>fcdot /.
         sub["SUNN", "N"]/.
         If[CC["SUNTrace"],  suntrace :> "tr", {}] /.
         If[CC["LeviCivita"],  levicivita[v__] :> epsd[v], {}] /.
         If[CC["Eps"],  eps[v__] :> epsd[v], {}] /.
         If[CC["MetricTensor"], metrictensor[v_, w_, ___Rule] :> "g"[v, w], {}
           ] /.
         If[CC["FourVector"], fourvector[Subscripted[p_[s_]], mu_] :>
          (SequenceForm@@Flatten[ {sumst[p[s]],"[",mu,"]"}]), {}
           ] /.
        If[ CC["ScalarProduct"], scalarproduct[ v_,v_ ] :> v^2, {}] /.
        If[ CC["ScalarProduct"], scalarproduct[v_ w_] :>
               (SequenceForm@@Flatten[ {v//sumst ,{"."},w//sumst} ]),{}
          ] /.
        If[CC["PolarizationVecvtor"],
           polarizationvector[ka_, mu_, ___] :> "ep"[ka, mu],
           {}
          ] /.
        If[CC["Polarization"],
           {pair[momentum[polarization[v_,-I,sun___]],
                  lorentzindex[w_] ] :> ("ep(*)"[v,w,sun] ),
            pair[ momentum[polarization[v_,-I,___]] ,
                   lorentzindex[w_] ]:> "ep(*)"[v, w] ,
            pair[ momentum[polarization[v_,I,sun___]],
                   lorentzindex[w_] ]:>
             ("ep"[v,w,sun] (*/.sunindex:>iDentity*))
           } , {}
          ] /.
        If[CC["Pair"],
           {pair[ lorentzindex[v_],lorentzindex[w_] ] :> "g"[v, w],
            pair[ lorentzindex[v_,di_],lorentzindex[w_,di_] ] :>
               (Subscripted["g"[di//diF]][v, w])
           }, {}
          ] /.
        If[CC["Pair"],
           pair[ momentum[v_,___],momentum[v_,___] ] :> v^2,
           {}
          ] /.
(*
        pair[ momentum[v_,di_],momentum[v_,di_] ] :>
               Subscripted[
                (SequenceForm@@Flatten[ {v//sumst ,{"."},v//sumst} ]
                )[di//diF]] /.
        pair[ momentum[v_,di_],momentum[w_,di_] ] :>
               Subscripted[
                (SequenceForm@@Flatten[ {v//sumst ,{"."},w//sumst} ]
                )[di//diF]]  /.
*)
       If[CC["Pair"],
          pair[ momentum[v_,___],momentum[w_,___] ] :>
               (SequenceForm@@Flatten[ {v//sumst ,{"."},w//sumst} ]),
           {}
         ] /.
       If[CC["Pair"],
        pair[ momentum[v_,di_Symbol-4],momentum[w_,di_Symbol-4] ] :>
                 Subscripted[
                  (SequenceForm@@Flatten[{"(",v//sumst ,{"."},w//sumst,")"}]
                  )[di//diF]] ,
           {}
         ] /.
       If[CC["Pair"],
        pair[ lorentzindex[w_,___],momentum[Subscripted[v_[s_]],___ ]]:>
             (SequenceForm@@Flatten[ {sumst[v[s]],"[",w,"]"} ]),
          {}
         ] /.
       If[CC["Pair"],
        pair[ lorentzindex[w_, ___],momentum[v_, ___] ] :>
             (SequenceForm@@Flatten[ {sumst[v],"[",w,"]"} ]),
          {}
         ] /.
        If[CC["Polarization"],
           {polarization[ka_,-I,___]:>"ep(*)"[ka],
             polarization[ka_,I,___]:>"ep"[ka]
           }, {}
          ] /.
         If[CC["ComplexIndex"],
            {MakeContext["ComplexIndex"][i__] :> compind[i]},
            {}
           ] /.
         If[CC["ChiralityProjector"],
            chiralityprojector[+1] :> diracgamma[6],
            {}
           ] /.
         If[CC["ChiralityProjector"],
            chiralityprojector[-1] :> diracgamma[7],
            {}
           ] /.
         If[CC["OPEDelta"], MakeContext["OPEDelta"] :> "De",
            {}
           ]/.
         If[CC["DiracMatrix"],
            diracmatrix[6] :> diracgamma[6], {}
           ] /.
         If[CC["DiracGamma"],
            {diracgamma[lorentzindex[v_]]        :>
             diracmatrix[v, dimension -> 4],
            diracgamma[lorentzindex[v_,di_],di_] :>
             diracmatrix[v, dimension -> 4],
            diracgamma[momentum[v_]]             :>
             diracslash[v, dimension -> 4],
            diracgamma[momentum[v_,di_],di_]     :>
             diracslash[v, dimension -> 4]
            },
            {}
           ]/.
         If[CC["DiracGammaT"],
            DiracGammaT[aa_,___]:> "gat"[aa],
            {}
           ] /.
         If[CC["DiracGamma"],
            {diracgamma[5] :> "ga[5]",
             diracgamma[6] :> "ga[6]",
             diracgamma[7] :> "ga[7]"
            }, {}
           ] /.
         If[CC["DiracMatrix"],
            If[(dimension /. Options[diracmatrix]) =!= 4,
               diracmatrix[v_] :>
                 diracmadi[(dimension /. Options[diracmatrix])][v],
               {}
              ],{}
           ]/.
         If[CC["DiracMatrix"],
            diracmatrix[v__, dimension -> 4] :> diracmatrix[v],
            {}
           ] /.
         If[CC["DiracSlash"],
            diracslash[v__, dimension -> 4] :> diracslash[v],
            {}
           ] /.
         If[CC["DiracSigma"],
            {
            DiracSigma[_[a_], _[b_]]:> "Sigma"[a,b],
            DiracSigma[_[a_, b_]]   :> "Sigma"[a,b]
            },{}
           ] /.
         If[CC["DiracMatrix"],
            {
             diracmatrix[v__, dimension -> di_] :>
              Subscripted["ga"[v][di]],
             diracmatrix[v__]  :>
              "ga"[v]
            },{}
           ] /.
         If[CC["DiracGamma"],
            If[(dimension /. Options[diracslash]) =!= 4,
               diracslash[v__]:>
                diracsldid[(dimension /. Options[diracslash])][v]/.
                 diracsldid :> diracsldi,
               {}
              ],
            {}
           ]/.
         If[CC["DiracGamma"], diracslash[aa_] :> diracslm[aa], {} ] /.
         If[CC["DiracGamma"],
             {diracslash[v_, dimension -> di_] :>
               Subscripted[ToString["gs"][di//diF] ][v] ,
              diracslash[Subscripted[v_[s_]]] :> "gs"[v[s]] ,
             diracslash[v_]:> ToString["gs"[v]]
             }
            , {}
           ] /.
         If[CC["Spinor"],
            {
             fcdot[spinor[-p_, 0, ___], a__] :>
               dot["v"[-p/.momentum->iDentity], a],
             fcdot[spinor[p_, 0, ___], a__]  :>
               dot["u"[p/.momentum->iDentity], a],
             fcdot[a__,spinor[-p_, 0, ___] ] :>
               dot["v"[-p/.momentum->iDentity], a],
             fcdot[a__, spinor[p_, 0, ___]]  :>
               dot[a, "u"[p/.momentum->iDentity]]
            }, {}
           ]/.
         If[CC["Spinor"],
            {
             fcdot[spinor[-p_, mas_, _], a__] :>
               dot["v"[-p/.momentum->iDentity,mas], a],
             fcdot[spinor[p_, mas_, _], a__]  :>
               dot["u"[p/.momentum->iDentity,mas], a],
             fcdot[a__,spinor[-p_, mas_, _] ] :>
               dot[a, "v"[-p/.momentum->iDentity,mas]],
             fcdot[a__, spinor[p_, mas_, _]]  :>
               dot[a, "u"[p/.momentum->iDentity,mas]]
            }, {}
           ]/.
         If[CC["Spinor"],
            {
            spinor[-p_,0,___] :> "v"[p /. momentum -> iDentity],
            spinor[p_,0,___]  :> "u"[p /. momentum -> iDentity],
            spinor[-p_,ma_,_] :> "v"[p /. momentum -> iDentity,ma],
            spinor[p_,ma_,_]  :> "u"[p /. momentum -> iDentity,ma]
            }, {}
           ]/.
         If[CC["SUNDelta"],
            sundelta[a_, b_] :> "d"[a, b],
            {}
           ] /.
         If[CC["SUND"],
            sund[a_, b_, c_] :> "d"[a, b, c],
            {}
           ] /.
         If[CC["SUNF2"],
            sunf[a_, b_, c_] :> "f"[a, b, c],
            {}
           ] /.
         If[CC["SUNF"],
            sunF[a_, b_, c_] :>  "f"[a, b, c],
            {}
           ] /.
         If[CC["SUNT"],
            {
            sunt[a_] :>  "T"[a],
            sunt[a_,b__] :> (fcdot2 @@ Map["T"[#]&,{a, b}])
            }, {}
           ] /.
         If[CC["OPEm"], OPEm :> "m", {} ] /.
         If[CC["OPEi"], OPEi :> "i", {} ] /.
         If[CC["OPEj"], OPEj :> "j", {} ] /.
         If[CC["OPEl"], OPEl :> "l", {} ] /.
         If[CC["OPEk"], OPEk :> "k", {} ] /.
         If[CC["QuantumField"],
            {
             field[a_] :> a,
             field[a_, lori___momentum, suni___sunindex][p___] :>
               "Q"[a, lori,suni][p],
             field[a_, lori___lorentzindex, suni___sunindex][p___] :>
               "Q"[a, lori,suni][p],
             field[a_, lori___lorentzindex, suni___sunindex] :>
               "Q"[a, lori,suni],
             field[a_, lori___momentum, suni___sunindex] :>
               "Q"[a, lori,suni],
             field[pa:partial[_].., a_, lori___lorentzindex,
                                          suni___sunindex][p___] :>
             "Q"[pa, a, lori, suni][p],
             field[pa:partial[_].., a_, lori___momentum,
                                          suni___sunindex][p___] :>
             "Q"[pa, a, lori, suni][p],
             field[pa:partial[_].., a_, lori___lorentzindex,
                                          suni___sunindex]  :>
             ("Q"[pa, a, lori, suni]/.partial -> "P"),
             field[pa:partial[_].., a_, lori___momentum,
                                          suni___sunindex] :>

             ("Q"[pa, a, lori, suni]/.partial -> "P")
            }, {}
            ]/.
         If[CC["QuantumField"],
            { partial[a_] :> "P"[a]
            }, {}
           ]/.
        fcdot:>fcdot2/. (*fcdot2 -> dot /.*)
         If[CC["DiracTrace"], diractrace[v__] :> ditr[v], {}] /.
         lorentzindex[v__] :> didl[v]  /.
         If[CC["QuantumField"],
            field[v__] :> "Q"[v],
            {}
           ] /.
         If[CC["PartialD"],
            partial[v_] :> "P"[v],
            {}
           ] /.
         If[CC["PlusDistribution"],
            PlusDistribution[v_] :> plusdi[v],
            {}
           ] /.
         If[CC["SUNIndex"],
            sunindex[i_] :> sunident[i],
            {}
           ]/.
         If[CC["OPESum"], OPESum :> "OPESum", {} ]/.
         If[CC["DeltaFunction"],  DeltaFunction :> "delta", {}
           ] /.
         If[CC["Twist2GluonOperator"], GluonOperator:>"GO",{}
           ] /.
         If[CC["SunIndex"], sunident :> sunindex, {} ] /.
         If[CC["FeynAmpDenominator"],
            feynampdenominator[v__] :> feynden[v], {}
           ] /.
         If[CC["PropagatorDenominator"],
            propagatordenominator[v__] :> fdprop[v], {}
           ] /.
         If[CC["Lower"], Lower[v_,___] :> v, {}] /.
         If[CC["Upper"], Upper[v_,___] :> v, {}] /.
         If[CC["Momentum"], momentum[v__] :> didm[v], {}]  /.
         lorentzindex[v__] :> didl[v]  /.
         {didm :> momentum, didl :> lorentzindex}
       );
         xxxx];

plusdi[a_] := Subscripted[SequenceForm["(",a,")"][" + "]];


End[]; MyEndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FeynCalcForm | \n "]];
Null

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* :Title: FieldStrength *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 16 March '98 at 11:52 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: F_{\mu \nu}^a *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FieldStrength`",
             "HighEnergyPhysics`FeynCalc`"];

FieldStrength::usage =
"FieldStrength[mu,nu,a] is the field strength tensor
F_{mu nu}^a = partial_mu A_nu^a - partial_nu A_mu^a +
g f^{abc} A_mu^b A_nu^c.
FieldStrength[mu,nu] is the field strength tensor
F_{mu nu}^a = partial_mu A_nu^a - partial_nu A_mu.
The name of the field (A) and the coupling constant (g)
can be set through the options or by additional arguments:
FieldStrength[mu,nu,a, A, g] or, specifying the dummy
color indices: FieldStrength[mu,nu,a, {A,b,c}, g].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[
CouplingConstant,
DeclareNonCommutative,
DOT,
Explicit,
FreeQ2,
GaugeField,
Gstrong,
IndexPosition,
LorentzIndex,
Momentum,
OPEDelta,
PartialD,
RightPartialD,
QuantumField,
SUNF,
SUNIndex];

DeclareNonCommutative[FieldStrength];

   Options[FieldStrength] = {CouplingConstant -> Gstrong,
                             Explicit -> False,
                             IndexPosition -> {0,0},
                             Symbol -> "F",
                             QuantumField -> GaugeField};

   FieldStrength[mu___, OPEDelta, nu___] :=
     FieldStrength[mu, Momentum[OPEDelta], nu];

   FieldStrength[mu_, nu_, a_, {aA_, b_, c_}, g_ /; Head[g] =!= Rule,
                 ru___Rule] :=
       (QuantumField[PartialD[LorentzIndex[mu]],
                     aA, LorentzIndex[nu], SUNIndex[a]] -
        QuantumField[PartialD[LorentzIndex[nu]],aA,
                      LorentzIndex[mu], SUNIndex[a]] +
    (* dat is hEEEEEl belangrijk .... *)
    g SUNF[a, b, c] DOT[QuantumField[aA, LorentzIndex[mu], SUNIndex[b]],
                        QuantumField[aA, LorentzIndex[nu], SUNIndex[c]]
                       ]
       ) /; FreeQ2[{mu,nu}, {Momentum, OPEDelta}] &&
               (Explicit /. {ru} /. Options[FieldStrength]);

   FieldStrength[mu_, Momentum[OPEDelta], a_, {aA_, b_, c_},
                 g_ /; Head[g] =!= Rule, ru___Rule] :=
       (QuantumField[PartialD[LorentzIndex[mu]],
                     aA, Momentum[OPEDelta], SUNIndex[a]] -
        QuantumField[PartialD[Momentum[OPEDelta]],aA,
                      LorentzIndex[mu], SUNIndex[a]] +
       (* dat is hEEEEEl belangrijk .... *)
      g SUNF[a, b, c] DOT[QuantumField[aA, LorentzIndex[mu], SUNIndex[b]],
                          QuantumField[aA, Momentum[OPEDelta], SUNIndex[c]]
                         ]
       ) /; FreeQ2[{mu}, {Momentum, OPEDelta}] &&
               (Explicit /. {ru} /. Options[FieldStrength]);


   FieldStrength[Momentum[OPEDelta], nu_, a_, {aA_, b_, c_},
                 g_ /; Head[g] =!= Rule, ru___Rule] :=
       (QuantumField[PartialD[Momentum[OPEDelta]],
                     aA, LorentzIndex[nu], SUNIndex[a]] -
        QuantumField[PartialD[LorentzIndex[nu]],aA,
                      Momentum[OPEDelta], SUNIndex[a]] +
       (* dat is hEEEEEl belangrijk .... *)
      g SUNF[a, b, c] DOT[QuantumField[aA, Momentum[OPEDelta], SUNIndex[b]],
                          QuantumField[aA, LorentzIndex[nu], SUNIndex[c]]
                         ]
       ) /; FreeQ2[{nu}, {Momentum, OPEDelta}] &&
               (Explicit /. {ru} /. Options[FieldStrength]);


   FieldStrength[mu_, nu_, ru___Rule] := (
   QuantumField[PartialD[mu],
                QuantumField /. {ru} /. Options[FieldStrength],
                LorentzIndex[nu]
               ] -
  QuantumField[PartialD[nu],
                QuantumField /. {ru} /. Options[FieldStrength],
                LorentzIndex[mu]
               ]                         ) /;
               (Explicit /. {ru} /. Options[FieldStrength]);

   FieldStrength[mu_, nu_, a_, ru___Rule] := Block[{g,b,c},
   b = Unique["b"]; c = Unique["c"];
   FieldStrength[mu, nu, a, {QuantumField /. {ru} /. Options[FieldStrength],
                             b, c}, CouplingConstant /. {ru} /.
                                    Options[FieldStrength],
                            ru
                ]                                  ] /;
               (Explicit /. {ru} /. Options[FieldStrength]);

   MakeBoxes[FieldStrength[mu_, nu_, a___, ru___Rule], TraditionalForm
            ] :=
 Catch[
       If[(IndexPosition /. {ru} /. Options[FieldStrength]) === {0,0},
          Throw[SubsuperscriptBox[Evaluate[Symbol /. {ru} /.
                                   Options[FieldStrength]],
                                   Tbox[mu,nu], Tbox[a] ]
               ]
         ];
       If[(IndexPosition /. {ru} /. Options[FieldStrength]) === {1,1},
          Throw[SubsuperscriptBox[Evaluate[Symbol /. {ru} /.
                                   Options[FieldStrength]],
                                  "\[Null]", Tbox[a,mu,nu]
                                  ]
               ]
         ];
       If[(IndexPosition /. {ru} /. Options[FieldStrength]) === {0,1},
          Throw[SubsuperscriptBox[Evaluate[Symbol /. {ru} /.
                                   Options[FieldStrength]],
                                   Tbox[mu], Tbox[a,nu]
                                  ]
               ]
         ];
       If[(IndexPosition /. {ru} /. Options[FieldStrength]) === {1,0},
          Throw[SubsuperscriptBox[Evaluate[Symbol /. {ru} /.
                                   Options[FieldStrength]],
                                   Tbox[nu], Tbox[a,mu]
                                  ]
               ]
         ];
   ];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FieldStrength | \n "]];
Null


(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FinalSubstitutions *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: option for seveal functions *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FinalSubstitutions`",
             "HighEnergyPhysics`FeynCalc`"];

FinalSubstitutions::usage =
"FinalSubstitutions is an option for OneLoop and OneLoopSum
and Write2. All substitutions indicated hereby are done at the
end of the calculation.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FinalSubstitutions | \n "]];
Null

(* :Title: FORM *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: FORM *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FORM`",
             "HighEnergyPhysics`FeynCalc`"];

FORM::usage =
"FORM is an option for RHI. If set to True
a FORM file is generated and run from Mathematica (provided
R. Hambergs FORM-program is installed correctly ... ).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FORM | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FORMEpilog *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: FORMEpilog *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FORMEpilog`",
             "HighEnergyPhysics`FeynCalc`"];

FORMEpilog::usage =
"FORMEpilog is an option for FeynCalc2FORM. It may be set
to a string which is put at the end of the FORM-file.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FORMEpilog | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FORMProlog *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: FORMProlog *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FORMProlog`",
             "HighEnergyPhysics`FeynCalc`"];

FORMProlog::usage =
"FORMProlog is an option for FeynCalc2FORM. It may be set
to a string which is put after the type declarations of the FORM-file.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FORMProlog | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FRH *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: FRH[x_] := FixedPoint[ReleaseHold, x] *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FRH`",
             "HighEnergyPhysics`FeynCalc`"];

FRH::usage =
"FRH[exp_] := FixedPoint[ReleaseHold, exp], i.e., FRH removes all
HoldForm and Hold in exp.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


FRH[x_] := FixedPoint[ReleaseHold, x];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FRH | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FV *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FV`",
             "HighEnergyPhysics`FeynCalc`"];

FV::usage= "FV[p,mu] is a fourvector and is transformed into
Pair[Momentum[p], LorentzIndex[mu]]
by FeynCalcInternal.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[ Momentum SP, SPD];

FV[p_ /; Head[p]=!=Momentum, Momentum[b_]]:= SP[p,b];
FV[Momentum[p_], Momentum[b_]]:= SP[p,b];

   FV /: MakeBoxes[FV[a_Subscripted, b_], TraditionalForm] :=
             SubsuperscriptBox[Tbox[a[[1,0]]], Tbox@@a[[1]], Tbox[b]];

   FV /: MakeBoxes[FV[a_Subscript, b_], TraditionalForm] :=
             SubsuperscriptBox[Tbox[a[[1]]], Tbox@@Rest[a], Tbox[b]];

   FV /: MakeBoxes[FV[a_, b_], TraditionalForm] :=
            SuperscriptBox[Tbox[a], Tbox[b]];


End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FV | \n "]];
Null
(* :Title: FVD *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FVD`",
             "HighEnergyPhysics`FeynCalc`"];

FVD::usage= "FVD[p,mu] is a fourvector and is
transformed into
Pair[Momentum[p,D], LorentzIndex[mu,D]]
by FeynCalcInternal.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

   FVD /: MakeBoxes[FVD[a_Subscripted, b_], TraditionalForm] :=
             SubsuperscriptBox[Tbox[a[[1,0]]], Tbox@@a[[1]], Tbox[b]];

   FVD /: MakeBoxes[FVD[a_Subscript, b_], TraditionalForm] :=
             SubsuperscriptBox[Tbox[a[[1]]], Tbox@@Rest[a], Tbox[b]];

   FVD /: MakeBoxes[FVD[a_, b_], TraditionalForm] :=
            SuperscriptBox[Tbox[a], Tbox[b]];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FVD | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FactorFull *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FactorFull`",
             "HighEnergyPhysics`FeynCalc`"];

FactorFull::usage=
"FactorFull is an option of Factor2 (default False).
If set to False, products like
(a-b) (a+b) will be replaced by (a^2-b^2).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FactorFull | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FactorTime *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FactorTime`",
             "HighEnergyPhysics`FeynCalc`"];

FactorTime::usage=
"FactorTime is an option for Factor2. It denotes the maximum
time (in seconds) during which Factor2 tries to factor.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FactorTime | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Factoring *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Factoring is an option for several functions *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Factoring`",
             "HighEnergyPhysics`FeynCalc`"];

Factoring::usage = "Factoring is an option for Collect2, Contract,
Tr and more functions. If set to True, the result will be
factored, using Factor2. If set to any function f, this function
will be used.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Factoring | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Factorout *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Factorout is an option for OPEInt *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Factorout`",
             "HighEnergyPhysics`FeynCalc`"];

Factorout::usage = "Factorout is an option for OPEInt.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Factorout | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FeynAmp *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FeynAmp`",
             "HighEnergyPhysics`FeynCalc`"];

FeynAmp::usage=
"FeynAmp[q, amp] is the head of a Feynman amplitude.
amp denotes the analytical expression for the amplitude,
where q is the integration variable.
FeynAmp[q1, q2, amp] denotes a two-loop amplitude.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


FeynAmp /:
  MakeBoxes[ FeynAmp[q_Symbol,amp_], TraditionalForm ] :=
RowBox[{"\[Integral]",
      RowBox[{
         StyleBox[ RowBox[{SuperscriptBox["\[DifferentialD]","D"], q}],
             ZeroWidthTimes -> True ]
            }
            ], "(",Tbox[amp],")"
       }
      ];

FeynAmp /:
  MakeBoxes[ FeynAmp[gr_[__],q_Symbol,amp_], TraditionalForm ] :=
RowBox[{"\[Integral]",
      RowBox[{
         StyleBox[ RowBox[{SuperscriptBox["\[DifferentialD]","D"], q}],
             ZeroWidthTimes -> True ]
            }
            ], "(",Tbox[amp],")"
       }
      ];

FeynAmp /:
  MakeBoxes[ FeynAmp[gr_[__],q_Symbol,amp_], TraditionalForm ] :=
RowBox[{"\[Integral]",
      RowBox[{
StyleBox[
             RowBox[{SuperscriptBox["\[DifferentialD]","D"], q}],
             ZeroWidthTimes -> True
                    ]
            }
            ], "(",Tbox[amp],")"
       }
      ];

FeynAmp /:
  MakeBoxes[ FeynAmp[ q1_, q2_, amp_],
             TraditionalForm
           ] :=
RowBox[
{"\[Integral]",
  RowBox[{
StyleBox[
 RowBox[{SuperscriptBox["\[DifferentialD]","D"], Tbox[q1]}],
 ZeroWidthTimes->True
        ] ,
"\[Integral]",
RowBox[{SuperscriptBox["\[DifferentialD]","D"],
            Tbox[q2]}]
         }
        ], "(",Tbox[amp],")"
     }];

FeynAmp /:
  MakeBoxes[ FeynAmp[gr_[__], q1_Symbol, q2_Symbol, amp_],
             TraditionalForm
           ] :=
RowBox[
{"\[Integral]",
  RowBox[{
RowBox[{SuperscriptBox["\[DifferentialD]","D"], Tbox[q1]}] ,
"\[Integral]",
FractionBox[RowBox[{SuperscriptBox["\[DifferentialD]","D"],
            Tbox[q2]}],
            SuperscriptBox[RowBox[{"(", "2", "\[Pi]",")"} ],"D"]
           ]
         }
        ], "(",Tbox[amp],")"
     }];

  MakeBoxes[ HighEnergyPhysics`FeynCalc`FeynAmp`FeynAmp[
             q1_, q2_, q3_, amp_],
             TraditionalForm
           ] :=
RowBox[
{"\[Integral]",
  RowBox[{
FractionBox[
RowBox[{SuperscriptBox["\[DifferentialD]","D"], Tbox[q1]}],
        SuperscriptBox[RowBox[{"(", "2", "\[Pi]",")"} ],"D"]
           ]
         ,
"\[Integral]",
FractionBox[RowBox[{SuperscriptBox["\[DifferentialD]","D"],
            Tbox[q2]}],
            SuperscriptBox[RowBox[{"(", "2", "\[Pi]",")"} ],"D"]
           ]
         ,
"\[Integral]",
FractionBox[RowBox[{SuperscriptBox["\[DifferentialD]","D"],
            Tbox[q3]}],
            SuperscriptBox[RowBox[{"(", "2", "\[Pi]",")"} ],"D"]
           ]
         }
        ], "(",Tbox[amp],")"
     }];

  MakeBoxes[ HighEnergyPhysics`FeynCalc`FeynAmp`FeynAmp[
             gr_[__], q1_Symbol, q2_Symbol, q3_Symbol, amp_],
             TraditionalForm
           ] :=
RowBox[
{"\[Integral]",
  RowBox[{
FractionBox[
RowBox[{SuperscriptBox["\[DifferentialD]","D"], Tbox[q1]}],
        SuperscriptBox[RowBox[{"(", "2", "\[Pi]",")"} ],"D"]
           ]
         ,
"\[Integral]",
FractionBox[RowBox[{SuperscriptBox["\[DifferentialD]","D"],
            Tbox[q2]}],
            SuperscriptBox[RowBox[{"(", "2", "\[Pi]",")"} ],"D"]
           ]
         ,
"\[Integral]",
FractionBox[RowBox[{SuperscriptBox["\[DifferentialD]","D"],
            Tbox[q3]}],
            SuperscriptBox[RowBox[{"(", "2", "\[Pi]",")"} ],"D"]
           ]
         }
        ], "(",Tbox[amp],")"
     }];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FeynAmp | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)


(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* :Title: FeynAmpDenominator *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 16 August '97 at 18:22 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: FeynAmpDenominator *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FeynAmpDenominator`",
             "HighEnergyPhysics`FeynCalc`"];

FeynAmpDenominator::usage =
"FeynAmpDenominator[ PropagatorDenominator[ ... ],
PropagatorDenominator[ ... ], ... ] is the head of
the denominators of the propagators, i.e. FeynAmpDenominator[x]
is the representation of 1/x .";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[ FAD, FeynCalcInternal, Momentum];

FeynAmpDenominator[ar__List] := FeynAmpDenominator[ar]=
FeynCalcInternal[FAD[ar]];

(*
somehow all this is not necessary anymore,
therfore commented out:

   PropagatorDenominator = MakeContext["PropagatorDenominator"];
   Momentum = MakeContext["Momentum"];

(* just for a bug-fix in TraditionalForm *)
  spa[ww_] := sps[Expand[ww /. Momentum[a_,___] :> a]];

  sps[y_] := If[FreeQ[y, Plus], Tbox[y^2],
                If[MatchQ[y, a_ - b_] && Length[y]===2,
                   If[ y =!= (y[[2]] - (-y[[1]])),
                       Tbox[y^2],
                       SuperscriptBox[
                         TBox["(",y[[2]],"-",-y[[1]],")"],2]
                     ],
                   Tbox[y^2]
                  ]
               ];

   psp[{a_}] := prt[a];
   psp[{a_, b__}] := prt[a, Length[{b}]+1];

   prt[{a_, 0}] := TBox["[",spa[a],"]"];
   prt[{a_, b_}] := TBox["[", spa[a], " - ", b^2, "]"];
   prt[{a_, 0},p_] := SuperscriptBox[TBox["[",spa[a],"]"], TBox[p]];
   prt[{a_, b_}, p_] :=
      SuperscriptBox[ TBox["[", spa[a], " - ", b^2, "]"], TBox[p] ];

spfix[a__] := Append[Table[TBox[{a}[[i]]," "], {i,Length[{a}]-1}],
                       Last[{a}]
                    ];

FeynAmpDenominator /:
    MakeBoxes[FeynAmpDenominator[_[a__]], TraditionalForm] :=
      ToBoxes[
HighEnergyPhysics`FeynCalc`PropagatorDenominator`PropagatorDenominator[a],
              TraditionalForm
             ];

FeynAmpDenominator /:
    MakeBoxes[FeynAmpDenominator[a__,b_], TraditionalForm
             ] :=
             (RowBox@@
Apply[spfix,(Map[psp, Split[{a,b}]] /. PropagatorDenominator->List)]
             )
*)


(*
FeynAmpDenominator /:
*)

    MakeBoxes[f_. FeynAmpDenominator[a__], TraditionalForm
             ] := (MakeBoxes[#,TraditionalForm]&)@@{f/ Apply[Dot,
                   Map[( (#[[1]]/.Momentum[aa_,___]:>aa)^2 -
                          #[[2]]^2 )&, {a}
                      ]
                                  ]}



End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FeynAmpDenominator | \n "]];
Null

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FeynAmpList`",
             "HighEnergyPhysics`FeynCalc`"];

FeynAmpList::usage=
"FeynAmpList[info][FeynAmp[...], FeynAmp[...], ...] is a head of a list of
Feynman amplitudes.";

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FeynAmpList | \n "]];
Null

(* :Title: FeynmanParameterNames *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FeynmanParameterNames`",
             "HighEnergyPhysics`FeynCalc`"];

FeynmanParameterNames::usage=
"FeynmanParameterNames is an option for FeynmanParametrize and \
FeynmanParametrize.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FeynmanParameterNames | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FourVector *)

(* :Author: Rolf Mertig *)


(* :Summary: FourVector *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FourVector`",
               "HighEnergyPhysics`FeynCalc`"];

FourVector::usage =
"FourVector[p, mu] is the four Dimensional vector p with Lorentz index m.
A vector with space-time Dimension d is obtained by supplying the option
Dimension->d."

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];



fci := fci = MakeContext["FeynCalcInternal"];

MakeContext[ Dimension, LorentzIndex, Momentum, Pair];

Options[FourVector]  = {Dimension -> 4, FeynCalcInternal -> True};

(* experimentally *)
FourVector[a_,b_, c___Rule] :=
 Pair[Momentum[a, Dimension /. {c} /. Options[FourVector]],
      LorentzIndex[b, Dimension /. {c} /. Options[FourVector]]
     ] /; FreeQ[{a, b}, Momentum] &&
          FreeQ[{a, b}, LorentzIndex] &&
          ((fci /. {c} /. Options[FourVector]) === True);

   FourVector /:
   MakeBoxes[FourVector[a_Plus,b_, ___], TraditionalForm] :=
    SubscriptBox[Tbox["(",HoldForm[a],
                      ")"],Tbox[b]];
   FourVector /:
   MakeBoxes[FourVector[a_,b_, ___], TraditionalForm] :=
    SubscriptBox[Tbox[a],Tbox[b]] /; Head[a] =!= Plus;
(*
   FourVector /:
   MakeBoxes[FourVector[a_Plus,b_, ___], TraditionalForm] :=
    SubscriptBox[TBOX[TextBox["("],HoldForm[a],
                      TextBox[")"]],TBOX[b]];
   FourVector /:
   MakeBoxes[FourVector[a_,b_, ___], TraditionalForm] :=
    SubscriptBox[TBOX[a],TBOX[b]] /; Head[a] =!= Plus;
*)

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* :Title: FreeIndex *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FreeIndex`",
             "HighEnergyPhysics`FeynCalc`"];

FreeIndex::usage=
"FreeIndex is a datatype which is recognized by Contract.
Possible use: DataType[mu, FreeIndex] = True.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FreeIndex | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FreeQ2 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Extension of FreeQ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FreeQ2`",
             "HighEnergyPhysics`FeynCalc`"];

FreeQ2::usage =
"FreeQ2[expr, {form1, form2, ...}] yields True if expr does not
contain any occurence of form1, form2, ... and False otherwise.
FreeQ2[expr, form] is the same as FreeQ[expr, form].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


FreeQ2[_,{}]          := True;
FreeQ2[x_, y_]        := FreeQ[x, y] /; Head[y] =!= List;
FreeQ2[x_, {y_}]      := FreeQ[x, y];
FreeQ2[x_, {y_, z__}] := If[FreeQ[x, y], FreeQ2[x, {z}], False];
(* this is eventually slower ...
FreeQ2[x_, {y_, z__}] := FreeQ[x, Alternatives@@{y,z}];
*)

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FreeQ2 | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: GA *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 8 December '98 at 12:57 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`GA`",
             "HighEnergyPhysics`FeynCalc`"];

GA::usage=
"GA[mu] can be used as input for gamma_mu and is
transformed into DiracMatrix[mu] by FeynCalcInternal.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext["DeclareNonCommutative"][GA];

GA[x_Dot] := Map[GA,x];
GA[x_, y__] := Dot @@ Map[GA,{x,y}];

GA /:
  MakeBoxes[ GA[x_], TraditionalForm ] := If[$Covariant,
                   SubscriptBox["\[Gamma]",MakeBoxes[x,TraditionalForm]],
                   SuperscriptBox["\[Gamma]",MakeBoxes[x,TraditionalForm]]
                                            ];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "GA | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: GA5 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`GA5`",
             "HighEnergyPhysics`FeynCalc`"];

GA5::usage=
"GA5 is equivalent to DiracGamma[5] and denotes gamma5.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[DiracGamma];

GA5 = DiracGamma[5];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "GA5 | \n "]];
Null
(* :Title: GAD *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`GAD`",
             "HighEnergyPhysics`FeynCalc`"];

GAD::usage=
"GAD[mu] can be used as input for a D-dimensional gamma_mu and is
transformed into DiracMatrix[mu, Dimension->D] by FeynCalcInternal.";



(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


GAD[x_Dot] := Map[GAD, x];
GAD[x_, y__] := Dot @@ Map[GAD,{x,y}];

GAD /:
  MakeBoxes[ GAD[x_], TraditionalForm ] := If[$Covariant,
             SubscriptBox["\[Gamma]",MakeBoxes[x,TraditionalForm]],
             SuperscriptBox["\[Gamma]",MakeBoxes[x,TraditionalForm]]
                                             ];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "GAD | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: GS *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 8 December '98 at 12:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`GS`",
             "HighEnergyPhysics`FeynCalc`"];

GS::usage=
"GS[p] is transformed into DiracSlash[p] by FeynCalcInternal.
GS[p,q, ...] is equivalent to GS[p].GS[q]. ...";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext["DeclareNonCommutative"][GS];

GS[x_Dot] := Map[GS,x];

GS[x_, y__] := Dot @@ Map[GS,{x,y}];

GS/:
  MakeBoxes[ GS[a_/;FreeQ[a,Plus]],
             TraditionalForm ] := Tbox["\[Gamma]", "\[CenterDot]", a];
GS/:
  MakeBoxes[ GS[a_/;!FreeQ[a,Plus]],
             TraditionalForm ] :=
  Tbox["\[Gamma]", "\[CenterDot]", "(",a,")"];

gsg[a_]:=If[FreeQ[y, Plus], Tbox["\[Gamma]", a],
                            Tbox["\[Gamma]", "(",a,")"]
           ];

GS/:
  MakeBoxes[ GS[a_, b__],
             TraditionalForm
           ] := Tbox@@Map[gsg, {a,b}]

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "GS | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: GSD *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 8 December '98 at 12:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`GSD`",
             "HighEnergyPhysics`FeynCalc`"];

GSD::usage=
"GSD[p] is transformed into DiracSlash[p,Dimension->D] by FeynCalcInternal.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext["DeclareNonCommutative"][GSD];

GSD[x_Dot] := Map[GSD, x];
GSD[x_, y__] := Dot @@ Map[GSD, {x, y}];

GSD/:
  MakeBoxes[ GSD[a_/;FreeQ[a,Plus]],
             TraditionalForm ] := Tbox["\[Gamma]", "\[CenterDot]", a];
GSD/:
  MakeBoxes[ GSD[a_/;!FreeQ[a,Plus]],
             TraditionalForm ] :=
  Tbox["\[Gamma]", "\[CenterDot]", "(",a,")"];

gsg[a_]:=If[FreeQ[y, Plus], Tbox["\[Gamma]", a],
                            Tbox["\[Gamma]", "(",a,")"]
           ];

GSD/:
  MakeBoxes[ GSD[a_, b__],
             TraditionalForm
           ] := Tbox@@Map[gsg, {a,b}]

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "GSD | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: GTI*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`GTI`",
             "HighEnergyPhysics`FeynCalc`"];

GTI::usage= "GTI is like RHI, but no functional properties.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[Momentum];

Options[GTI] = {Momentum -> Global`p};

(*
   GTI /: MakeBoxes[GTI, TraditionalForm] := "\[CapitalKoppa]"
*)

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "GTI | \n "]];
Null
(* :Title: GammaExpand *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`GammaExpand`",
             "HighEnergyPhysics`FeynCalc`"];

GammaExpand::usage= "GammaExpand[exp] rewrites
Gamma[n + m] (where n has Head Integer).";


(* ------------------------------------------------------------------------ *)

Begin["`Private`"];



GammaExpand[exp_] := Block[{gamma1, gamma2},
gamma1[y_] := gamma1[y] = gamma2[Expand[y]];
gamma2[n_Integer + m_] := (gamma2[n + m] =
                    Pochhammer[m+1,n-1] Gamma[m+1]
                   ) /; (n =!= 1);

gamma2[m_ /; Head[m]=!=Plus] :=  Gamma[1 + m]/m;

exp /. Gamma -> gamma1 /. gamma2 -> Gamma
                          ];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "GammaExpand | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Gauge *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Gauge *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Gauge`",
             "HighEnergyPhysics`FeynCalc`"];

Gauge::usage =
"Gauge is an option for GluonProgagator. If set to 1 the
't Hooft Feynman gauge is used.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Gauge | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: GaugeField *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`GaugeField`",
             "HighEnergyPhysics`FeynCalc`"];

GaugeField::usage =
"GaugeField is a name of a gauge field.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

   GaugeField /: MakeBoxes[GaugeField, TraditionalForm] := "A";

End[]; MyEndPackage[];
If[$VeryVerbose > 0,WriteString["stdout", "GaugeField | \n "]];
Null

(* :Summary: Gluon field *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`GluonField`",
             "HighEnergyPhysics`FeynCalc`"];

GluonField::usage =
"GluonField is a name of a gauge field.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


   GluonField /: MakeBoxes[GluonField, TraditionalForm] := "A";

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "GluonField | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: GaugeXi *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  head for gauge-parameters *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`GaugeXi`",
             "HighEnergyPhysics`FeynCalc`"];

GaugeXi::usage= "GaugeXi is a head for gauge parameters.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

   GaugeXi /:
   MakeBoxes[GaugeXi[a_], TraditionalForm] :=
    SubscriptBox["\[Xi]", TBox[a]];
   GaugeXi /:
   MakeBoxes[GaugeXi, TraditionalForm] :=
    TagBox["\[Xi]", TraditionalForm]

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "GaugeXi | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)


(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: GrassmannParity *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: GrassmannParity  is just a datatype *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`GrassmannParity`",
             "HighEnergyPhysics`FeynCalc`"];


GrassmannParity::usage =  "GrassmannParity is a data type.
E.g. DataType[F, GrassmannParity] = 1 declares F to be of
bosonic type and DataType[F, GrassmannParity] = -1 of fermionic
one.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"]


End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "GrassmannParity | \n "]];
Null


(* :Title: Gstrong *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Gstrong *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Gstrong`",
             "HighEnergyPhysics`FeynCalc`"];

Gstrong::usage =
"Gstrong denotes the strong coupling constant.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

  Gstrong /:
   MakeBoxes[Gstrong, TraditionalForm] :=
    SubscriptBox["g","s"]

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Gstrong | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)


(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* :Title: IFPD *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: IFPD *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`IFPD`",
             "HighEnergyPhysics`FeynCalc`"];

IFPD::usage =
"IFPD[p, m] denotes (p^2 - m^2)."

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[Momentum, OPEDelta];

IFPD[Momentum[OPEDelta,___],0] := 0;

    IFPD /:
    MakeBoxes[IFPD[a_,c_], TraditionalForm] :=
    If[c === 0,
       TBox[a^2],
       TBox["(", a^2," - ", c^2, ")"]
      ]

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "IFPD | \n "]];
Null


(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* :Title: IFPDOn *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 11 November '97 at 14:52 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: IFPDOn *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`IFPDOn`",
             "HighEnergyPhysics`FeynCalc`"];

IFPDOn::usage =
"IFPDOn[exp_,q1_, q2_, ...] changes from
FeynAmpDenominator[ ...] representation to the IFPD one
(Inverse Feynman Propagator Denominator).
I.e., FeynAmpDenominator[PropagatorDenominator[a,b]] is replaced
by 1/IFPD[a,b] and
The q1, q2, ... are the integration momenta.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[Cases2,
ExpandScalarProduct,
(*NN*)FAD,
FeynAmpDenominator,
FeynAmpDenominatorSplit,
(*NN*)FeynCalcInternal,
(*NN*)FreeQ2,
IFPD,
Momentum,
MomentumExpand,
Pair,
PropagatorDenominator,
Select1,
Select2,
(*NN*)SP,SPD
];

IFPDOn[exp_,qu__] :=
If[FreeQ2[exp, {Pair,SP,SPD}] ||FreeQ2[exp,{FeynAmpDenominator,FAD}], exp,
Block[
(*NN*){int,qq,sub,t0,t1,t2,t3,t4,MyHold,feynsub,nit3,ifnu,
       condition,pa,unsameq, checkm, checkp, thr},
(*NN*)If[!FreeQ2[exp,{SP,SPD,FAD}],
(*NN*)   int = FeynAmpDenominatorSplit[FeynCalcInternal[exp]],
(*NN*)   int = FeynAmpDenominatorSplit[exp]
(*NN*)  ];
(*NN*)
(*NN*)t0 = Cases2[int, FeynAmpDenominator];
(*NN*)      t0r= t0 /.
(*NN*)      FeynAmpDenominator[a__] :>
(*NN*)       MomentumExpand[FeynAmpDenominator[a]] /.
(*NN*)        PropagatorDenominator[
(*NN*)         -Momentum[pe_ /; !FreeQ[{qu}, pe], di___] + pl_., em_
(*NN*)                             ] :>
(*NN*)        PropagatorDenominator[Momentum[pe, di] - pl, em];
(*NN*) int = int /. (thr = Thread[Rule[t0,t0r]]);


SetAttributes[MyHold,HoldAll];
int = Apply[MyHold, {int}];
qq = {qu} /. Momentum[a_,___] :> a;
t2 = Cases2[int, FeynAmpDenominator];
feynsub = Table[t2[[i]] -> (1/t2[[i]]/.FeynAmpDenominator :>
                            ((# /. PropagatorDenominator -> IFPD) &)
                           ),
                {i, Length[t2]}
(*NN*)         ]//Dispatch;
t2 = Select2[t2, qq];
t2 = t2 /. FeynAmpDenominator :>
           ((# /. PropagatorDenominator -> IFPD) &);

t3 = Cases2[t2, IFPD];
(* check if there are massless and massive propagators and keep only
   the massless
*)
If[!FreeQ[t3, IFPD[a_,b_/;b=!=0]],
   ifnu = Select[t3, MatchQ[#, IFPD[a_,0]]&
                ] /. IFPD[aa_, 0] ->
          IFPD[aa, condition[pa[b,Blank[]], unsameq[b,0]]];
   ifnu = ifnu /. condition -> Condition /. pa -> Pattern /.
          unsameq -> UnsameQ;
   t3 = Select1[t3, ifnu]
  ];

(* calculate  a canonical q.p  as a side effect*)
If[$VeryVerbose >2, Print["before ifp"]];
t3 = ifp[t3, qq];
If[$VeryVerbose >2, Print["after ifp"]];

(*
   t3 /. IFPD -> asign[qq] /. asign -> assign0 /.
         assign0 -> assign2 /. assign2 -> assign1;
*)

sub = Table[(Pair @@ t3[[ij,1]]) -> t3[[ij,2]],{ij,Length[t3]}];
int = int /. feynsub /. sub;
int /. MyHold -> Identity ]];

ifp[{ww__},{qq__}] := ifp[{ww},{qq}] =
Block[{ct,tt,mt,nt,sq,sqr,
ctm,ctp,ntm,ntp,mtm,mtp,checkp,checkm},
If[$VeryVerbose > 2, Print["entering ifp with ",{ww}]];

(* get all qi^2 *)
sq = Select[{ww}, MatchQ[#, IFPD[a_ /; FreeQ[a, Plus],_]]&];
sq = Select2[sq, {qq}];
sq = Table[{sq[[i,1]],sq[[i,1]],sq[[i]]}, {i,sq//Length}];
(* get all other (qi+pj)^2 -m^2 *)
tt = Flatten[ Table[{ {ww}[[i]], {ww}[[j]] }, {i,1,Length[{ww}]-1},
                                              {j,i+1,Length[{ww}]}
        ], 1];
sfix[{IFPD[a_,b_], IFPD[c_,d_]}] := (* fix the sign *)
  If[Head[a - c] === Times,
     {IFPD[c, d], IFPD[a, b]}, {IFPD[a, b], IFPD[c, d]}
    ];
ntm = sfix /@ Select[tt, FreeQ[(#[[1,1]])-(#[[2,1]]),Plus]&];
ntp = sfix /@ Select[tt, FreeQ[(#[[1,1]])+(#[[2,1]]),Plus]&];
mtm = {};
 Do[
    If[Length[Select2[Cases2[ntm[[i]],Momentum], qq]] > 0 &&
       (ntm[[i,1,1]]-ntm[[i,2,1]]) =!= 0,
       mtm = Join[mtm,
                  {
                   Append[ Union[
                   {ntm[[i,1,1]]-ntm[[i,2,1]]},
                    Select2[Cases2[ntm[[i]],Momentum], qq]
                                ],
                           ntm[[i]]
                         ],
                   Append[ Reverse[Union[
                   {ntm[[i,1,1]]-ntm[[i,2,1]]},
                    Select2[Cases2[ntm[[i]],Momentum], qq]
                                 ]],
                           ntm[[i]]
                         ]
                  }
                 ]
      ]
       , {i, Length[ntm]}
   ];

mtp = {};
 Do[
    If[Length[Select2[Cases2[ntp[[i]],Momentum], qq]] > 0 &&
       (ntp[[i,1,1]]+ntp[[i,2,1]]) =!= 0,
       mtp = Join[mtp,
                  {
                   Append[ Union[
                   {ntp[[i,1,1]] + ntp[[i,2,1]]},
                    Select2[Cases2[ntp[[i]],Momentum], qq]
                                ],
                           ntp[[i]]
                         ],
                   Append[ Reverse[Union[
                   {ntp[[i,1,1]] + ntp[[i,2,1]]},
                    Select2[Cases2[ntp[[i]],Momentum], qq]
                                 ]],
                           ntp[[i]]
                         ]
                  }
                 ]
      ]
       , {i, Length[ntp]}
   ];

(* if :  pe = a - c *)
checkm[{pe_, qu_, {IFPD[a_, b_], IFPD[c_, d_]}}] :=
  If[ Expand[Pair[pe,qu] -
             1/2 ExpandScalarProduct[(Pair[a,a]-b^2) - (Pair[c,c]-d^2)
                                       - ( Pair[a-qu,a-qu] -
                                           Pair[c-qu,c-qu]-b^2+d^2)
                                    ]
            ] === 0, True, False
    ];

(* if :  pe = a + c *)
checkp[{pe_, qu_, {IFPD[a_, b_], IFPD[c_, d_]}}] :=
  If[ Expand[Pair[pe, qu] -
             1/2 ExpandScalarProduct[(Pair[a,a]-b^2) - (Pair[c,c]-d^2)
                                       - ( Pair[a-qu,a-qu] -
                                           Pair[c+qu,c+qu]-b^2+d^2)
                                    ]
            ] === 0, True, False
    ];

(* minus *)
ctm = Select[mtm, checkm];
ctm = Table[{ctm[[i,1]],ctm[[i,2]]} ==
          Expand[1/2 ExpandScalarProduct[
                 ctm[[i,3,1]] - ctm[[i,3,2]] -
                 ( Pair[ctm[[i,3,1,1]]-ctm[[i,2]],ctm[[i,3,1,1]]-ctm[[i,2]]]-
                   Pair[ctm[[i,3,2,1]]-ctm[[i,2]],ctm[[i,3,2,1]]-ctm[[i,2]]]-
                   ctm[[i,3,1,2]]^2 + ctm[[i,3,2,2]]^2 )
              ] ], {i,Length[ctm]} ]; (* plus *)
ctp = Select[mtp, checkp];
ctp = Table[{ctp[[i,1]],ctp[[i,2]]}==
          Expand[1/2 ExpandScalarProduct[
                 ctp[[i,3,1]] - ctp[[i,3,2]] -
                 ( Pair[ctp[[i,3,1,1]]-ctp[[i,2]],ctp[[i,3,1,1]]-ctp[[i,2]]]-
                   Pair[ctp[[i,3,2,1]]+ctp[[i,2]],ctp[[i,3,2,1]]+ctp[[i,2]]]-
                   ctp[[i,3,1,2]]^2 + ctp[[i,3,2,2]]^2 )
              ] ], {i,Length[ctp]}
           ];

sq = Map[Apply[({#1, #2} == #3 +  (#3[[2]]^2))&,#]&, sq];
sqr = Table[(Pair@@sq[[i,1]]) -> sq[[i,2]],{i,Length[sq]}];
ct = Join[ctm/.sqr, ctp/.sqr];
ct = Join[sq, Union[ Map[MapAt[Sort,#,1]&, ct]] ];
If[$VeryVerbose > 2, Print["Exiting ifp with ",ct ]];
ct];

(* Test :
Test[
ifp[{IFPD[Momentum[q1,D],m1],
     IFPD[Momentum[q1,D]+Momentum[p1,D],m2],
     IFPD[Momentum[q1,D]+Momentum[p1,D]+ Momentum[p2,D],m3]
    },{q1}]
,
  {{Momentum[q1, D], Momentum[q1, D]} == m1^2 + IFPD[Momentum[q1, D], m1],
   {Momentum[p1, D], Momentum[q1, D]} ==
    -m1^2/2 + m2^2/2 - IFPD[Momentum[q1, D], m1]/2 +
     IFPD[Momentum[p1, D] + Momentum[q1, D], m2]/2 -
     Pair[Momentum[p1, D], Momentum[p1, D]]/2,
   {Momentum[p2, D], Momentum[q1, D]} ==
    -m2^2/2 + m3^2/2 - IFPD[Momentum[p1, D] + Momentum[q1, D], m2]/2 +
     IFPD[Momentum[p1, D] + Momentum[p2, D] + Momentum[q1, D], m3]/2 -
     Pair[Momentum[p1, D], Momentum[p2, D]] -
     Pair[Momentum[p2, D], Momentum[p2, D]]/2
  }
    ];
*)


End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "IFPDOn | \n "]];
Null


(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* :Title: IFPDOff *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: IFPDOff *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`IFPDOff`",
             "HighEnergyPhysics`FeynCalc`"];

IFPDOff::usage =
"IFPDOff[exp_,q1_, q2_, ...] changes from
IFPD representation to FeynAmpDenominator[ ...].
The q1, q2, ... are the integration momenta.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[Cases2,
ExpandScalarProduct,
FeynAmpDenominator,
FeynAmpDenominatorSimplify,
IFPD,
Momentum,
MomentumExpand,
Pair,
PropagatorDenominator,
Select1,
Select2];

SetAttributes[IFPDOff, HoldAll];
SetAttributes[unset, HoldAll];

(* this is a side effect on Pair ... *)
(* it is undone by IFPDOff *)

unasign[quu__][pp_Plus, m_] :=
  If[Head[Select2[pp,quu]] === Plus,
     unasign[quu][First[Select2[pp,quu]], Rest[Select2[pp,quu]] +
                Select1[pp,quu], m],
     unasign[quu][Select2[pp,quu], Select1[pp,quu], m]
    ];

unassign0[quu__][Momentum[q_, di___],m_] :=
 If[!FreeQ[{quu},q],
 If[Head[Pair[Momentum[q,di],Momentum[q,di]]]=!=Pair,
    uns[pair[Momentum[q,di], Momentum[q,di]]]/.
    uns->unset/.pair->Pair/.unset->Unset;
  ]];

pair[-Momentum[a__], Momentum[b__]] := pair[Momentum[a],Momentum[b]];
pair[-Momentum[a__],-Momentum[b__]] := pair[Momentum[a],Momentum[b]];
pair[ Momentum[a__],-Momentum[b__]] := pair[Momentum[a],Momentum[b]];

unassign1[quu__][Momentum[q_, di___], pes_, m_] :=
 If[!FreeQ[{quu},q],
 If[(Head[Pair[Momentum[q,di],pes]]=!=Pair) &&
    (Head[-Pair[Momentum[q,di],pes]]=!=Pair),
    uns[pair@@Sort[{Momentum[q,di], pes}]]/.
    uns->unset/.pair->Pair/.unset->Unset;
   ];
   ];

unassign2[quu__][-Momentum[q_, di___], pes_, m_] :=
 If[!FreeQ[{quu},q],
 If[Head[Pair[-Momentum[q,di],pes]]=!=Pair,
    uns[pair@@Sort[{-Momentum[q,di], pes}]]/.
    uns->unset/.pair->Pair/.unset->Unset;
  ]];

ifex[a_,b_] := ifex[a,b] = Pair[a,a] - b^2;

FP[y__] := FeynAmpDenominator[PropagatorDenominator[y]];

IFPDOff[exp_,qu__] :=
If[FreeQ[exp, IFPD],
   exp,
Block[{int,qq,sub,t1,t2,t3,t4},
int = Apply[Hold, {exp}];
qq = {qu} /. Momentum[a_,___] :> a;
t2 = Cases2[int, IFPD];
t3 = Map[(#  -> (1/#/.IFPD->FEP))&,t2];
(* unassign *)
Cases2[DownValues@@{Pair},IFPD]/.
IFPD -> unasign[qq] /. unasign -> unassign0 /.
        unassign0 -> unassign1 /. unassign1 -> unassign2;
sub = Dispatch[t3];
int = int /. sub;
int = Operate[# /. Hold -> Identity&, int];
int = int /. FEP[a_, b_]^n_Integer?Negative :>
           (ExpandScalarProduct[a, a] - b^2)^(-n);
int = int /. {FEP :> FP, IFPD :> ifex};
int
                           ]
  ];

Off[Unset::norep];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "IFPDOff | \n "]];
Null

(* :Title: IncludePair *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: IncludePair *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`IncludePair`",
             "HighEnergyPhysics`FeynCalc`"];

IncludePair::usage =
"IncludePair is an option for FC2RHI.
 Possible settings are True and False.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "IncludePair | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: IndexPosition *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`IndexPosition`",
             "HighEnergyPhysics`FeynCalc`"];

IndexPosition::usage=
"IndexPosition is an option for FieldStrength.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "IndexPosition | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Indices *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Indices`",
             "HighEnergyPhysics`FeynCalc`"];

Indices::usage= "Indices is an option for FORM2FeynCalc. Its default
setting is Automatic. It may be set to a list, if the FORM-file does
not contain a I(ndices) statement.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Indices | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: InitialSubstitutions *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: option for seveal functions *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`InitialSubstitutions`",
             "HighEnergyPhysics`FeynCalc`"];

InitialSubstitutions::usage =
"InitialSubstitutions is an option for OneLoop and OneLoopSum
and Write2. All substitutions indicated hereby are done at the
end of the calculation.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "InitialSubstitutions | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: InsideDiracTrace *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: option for several functions *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`InsideDiracTrace`",
             "HighEnergyPhysics`FeynCalc`"];

InsideDiracTrace::usage=
"InsideDiracTrace is and option of DiracSimplify.
If set to True, DiracSimplify assumes to operate
inside a DiracTrace, i.e., products of an odd number
of Dirac matrices are discarded. Furthermore simple
traces are calculated (but divided by a factor 4,
i.e. :  DiracSimplify[DiracMatrix[a,b], InsideDiracTrace->True]
 yields  ScalarProduct[a,b]) \n
Traces involving more than
four DiracGamma's and DiracGamma[5] are not performed."


(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "InsideDiracTrace | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: IntegralTable *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`IntegralTable`",
             "HighEnergyPhysics`FeynCalc`"];

IntegralTable::usage=
"IntegralTable is an option of OneLoopSimplify, TwoLoopSimplify and
FeynAmpDenominatorSimplify.
It may be set to a list of the form :
{FCIntegral[ ... ] :> bla, ...}.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCIntegrate *)

(* :Author: Frederik Orellana *)

(* ------------------------------------------------------------------------ *)
(* :History: created 17 April 2001 at 13:52 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FCIntegrate`",
             "HighEnergyPhysics`FeynCalc`"];

FCIntegrate::usage=
"FCIntegrate is an option of certain Feynman integral related functions. \
It determines which integration function is used to evaluate analytic \
integrals. Possible settings include Integrate, NIntegrate,
(Dot[Integratedx@@#2, #1] &).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCNIntegrate *)

(* :Author: Frederik Orellana *)

(* ------------------------------------------------------------------------ *)
(* :History: created 17 April 2001 at 13:52 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FCNIntegrate`",
             "HighEnergyPhysics`FeynCalc`"];

FCNIntegrate::usage=
"FCNIntegrate is an option of certain Feynman integral related functions \
which may return output containing both integrals that can be evaluated \
and integrals that can only be evaluated numerically. \
It then determines which integration function is used to evaluate numeric \
integrals. Possible settings include NIntegrate, (0*#1)&, \
(Dot[Integratedx@@#2, #1] &).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "IntegralTable | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Integratedx *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  \int_0^1 dx *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Integratedx`",
             "HighEnergyPhysics`FeynCalc`"];

Integratedx::usage=
"Integratedx[x, low, up] is a variable representing the integration
operator Integrate[#, {x,low,up}]&.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

   Integratedx /:
   MakeBoxes[Integratedx[x_, low_, up_], TraditionalForm] :=
   RowBox[{ SubsuperscriptBox["\[Integral]", TBox[low], TBox[up]],
            "\[DifferentialD]", MakeBoxes[TraditionalForm[x]](*x*), "\[VeryThinSpace]" }
         ]

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Integratedx | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: IntermediateSubstitutions *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: option for several functions *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`IntermediateSubstitutions`",
             "HighEnergyPhysics`FeynCalc`"];

IntermediateSubstitutions::usage =
"IntermediateSubstitutions is an option for OneLoop and
and SquareAmplitude. All substitutions indicated hereby are done at
an intermediate stage of the calculation.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "IntermediateSubstitutions | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: IsolateHead *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: option for several functions *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`IsolateHead`",
             "HighEnergyPhysics`FeynCalc`"];

IsolateHead::usage =
"IsolateHead is equivalent to IsolateNames.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[IsolateNames];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "IsolateHead | \n "]];
Null


(* :Title: IsolateNames *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: option for several functions *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`IsolateNames`",
             "HighEnergyPhysics`FeynCalc`"];

IsolateNames::usage =
"IsolateNames is an option for Isolate and Collect2.
Its default setting is KK. Instead of a symbol the
setting may also be a list with the names of the abbrevations.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "IsolateNames | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: IsolatePrint *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: option for several functions *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`IsolatePrint`",
             "HighEnergyPhysics`FeynCalc`"];

IsolatePrint::usage =
"IsolatePrint is an option of Isolate.
If it is set to OutputForm (or any other *Form) the definitions
of the abbreviations are printed during the operation of Isolate.
The setting IsolatePrint -> False suppresses printing.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "IsolatePrint | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: IsolateSplit *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: option for several functions *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`IsolateSplit`",
             "HighEnergyPhysics`FeynCalc`"];

IsolateSplit::usage =
"IsolateSplit is an option for Isolate. Its setting determines the
maximum number of characters of FortranForm[expr] which are
abbreviated by Isolate. If the expression is larger than the
indicated number, it is split into smaller pieces and onto
each subsum Isolate is applied.
With the default setting IsolateSplit -> Infinity no splitting
is done.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "IsolateSplit | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: KK *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: KK *)

(* :Package Version 2.1 *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`KK`",
             "HighEnergyPhysics`FeynCalc`"];

KK::usage =
"KK[i] is the default setting of IsolateNames,
which is the head of abbreviations used by Isolate.
A KK[i] returned by Isolate is given in HoldForm and can be
recovered by ReleaseHold[KK[i]].";


(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "KK | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: KeepOnly*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`KeepOnly`",
             "HighEnergyPhysics`FeynCalc`"];

KeepOnly::usage=
"KeepOnly is an option of OneLoop.
It may be set to B0, C0, D0 keeping only the corresponding
coefficients. The default setting is False. If KeepOnly is set
to {} then the part of the amplitude which is not coefficient
of B0, C0, D0 is kept.";


(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "KeepOnly | \n "]];
Null
(* :Title: LC *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`LC`",
             "HighEnergyPhysics`FeynCalc`"];

LC::usage=
"LC[m,n,r,s] evaluates to LeviCivita[m,n,r,s] applying
FeynCalcInternal.
LC[m,...][p, ...] evaluates to LeviCivita[m,...][p,...]
applying FeynCalcInternal.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

   LC/:
   MakeBoxes[LC[x___][y___] ,TraditionalForm] :=
   SuperscriptBox["\[Epsilon]", Tbox[x,y]];
   LC/:
   MakeBoxes[LC[x__] ,TraditionalForm] :=
   SuperscriptBox["\[Epsilon]", Tbox[x]];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "LC | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: LCD *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`LCD`",
             "HighEnergyPhysics`FeynCalc`"];

LCD::usage=
"LCD[m,n,r,s] evaluates to LeviCivita[m,n,r,s,Dimension->D]
applying FeynCalcInternal.
LCD[m,...][p, ...] evaluates to
LeviCivita[m,...,Dimension->D][p,...,Dimension->D]
applying FeynCalcInternal.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

   LCD /:
   MakeBoxes[LCD [x___][y___] ,TraditionalForm] :=
   SuperscriptBox["\[Epsilon]", Tbox[x,y]];
   LCD /:
   MakeBoxes[LCD [x__] ,TraditionalForm] :=
   SuperscriptBox["\[Epsilon]", Tbox[x]];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "LCD | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: LeftPartialD *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 11 March '98 at 22:04 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: partial derivative *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`LeftPartialD`",
             "HighEnergyPhysics`FeynCalc`"];

LeftPartialD::usage=
"LeftPartialD[mu] denotes partial_mu, acting to the left.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[ Commutator, DeclareNonCommutative,
    DOT, FreeQ2, LorentzIndex, Momentum, OPEDelta, RightPartialD];
(* ******************************************************************** *)

LeftPartialD[xx__] := LeftPartialD @@ (LorentzIndex /@ {xx}) /;
		 FreeQ2[{xx}, {LorentzIndex, Momentum, OPEDelta, RowBox,
			       Pattern, Blank}] && (Union[{xx}]=!={1});

LeftPartialD[(1)..] = 1;
LeftPartialD[c:OPEDelta..] := LeftPartialD @@ (Momentum /@ {c});
LeftPartialD[x_LorentzIndex, y__LorentzIndex] :=
          DOT @@ Map[LeftPartialD, {x, y}];
LeftPartialD[x_Momentum, y__Momentum] := DOT @@ Map[LeftPartialD, {x, y}];

(*
LeftPartialD[Momentum[OPEDelta]^n_Integer?Positive] :=
       DOT @@ Map[LeftPartialD, Table[Momentum[OPEDelta],{n}]];
*)

Commutator[RightPartialD[a_], LeftPartialD[b_]] = 0;

 LeftPartialD /:
   MakeBoxes[LeftPartialD[x_ ^n_],TraditionalForm] :=
    SubsuperscriptBox[RowBox[{
      OverscriptBox["\[PartialD]", "\[LeftArrow]"]}], Tbox[" ",x],
        Tbox[n]
                     ] /; Head[x] === Momentum;

   LeftPartialD /:
   MakeBoxes[LeftPartialD[x_], TraditionalForm] :=
   SubscriptBox[OverscriptBox["\[PartialD]", "\[LeftArrow]"], TBox[x]];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "LeftPartialD | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: LeftRightPartialD *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 13 March '98 at 18:38 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: partial derivative *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`LeftRightPartialD`",
             "HighEnergyPhysics`FeynCalc`"];

LeftRightPartialD::usage=
"LeftRightPartialD[mu] denotes partial_mu, acting to the left and
right. PartialExplit[LeftRightPartialD[mu]] gives
1/2 (RightPartialD[mu] - LeftPartialD[mu]).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[DeclareNonCommutative,
            DOT, FreeQ2, Momentum, LorentzIndex, OPEDelta];

DeclareNonCommutative[LeftRightPartialD];

(* ******************************************************************** *)
LeftRightPartialD[xx__] := LeftRightPartialD@@ (LorentzIndex /@ {xx}) /;
                 FreeQ2[{xx}, {LorentzIndex, Momentum, OPEDelta, RowBox,
                               Pattern, Blank}] && (Union[{xx}]=!={1});

LeftRightPartialD[(1)..] = 1;

LeftRightPartialD[c:OPEDelta..] := LeftRightPartialD @@ (Momentum /@ {c});
LeftRightPartialD[x_LorentzIndex, y__LorentzIndex] :=
 DOT @@ Map[LeftRightPartialD, {x, y}];
LeftRightPartialD[x_Momentum, y__Momentum] :=
 DOT @@ Map[LeftRightPartialD, {x, y}];

(* nonsense;  commented out 9/95
LeftRightPartialD[Momentum[OPEDelta]^n_Integer?Positive] :=
       DOT @@ Map[LeftRightPartialD, Table[Momentum[OPEDelta],{n}]];
*)

LeftRightPartialD /:
   MakeBoxes[ LeftRightPartialD[x_] , TraditionalForm
            ] :=
    SubscriptBox[OverscriptBox["\[PartialD]", "\[LeftRightArrow]"], Tbox[x]];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "LeftRightPartialD | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: LeftRightPartialD2 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 13 March '98 at 18:39 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: partial derivative *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`LeftRightPartialD2`",
             "HighEnergyPhysics`FeynCalc`"];

LeftRightPartialD2::usage=
"LeftRightPartialD2[mu] denotes partial_mu, acting to the left and
right. ExplitPartialD[LeftRightPartialD2[mu]] gives
 (RightPartialD[mu] + LeftPartialD[mu]).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


(*Bar := Bar   = MakeContext["Bar"];*)

MakeContext[DeclareNonCommutative,
       DOT, FreeQ2, Momentum, LorentzIndex, OPEDelta];

DeclareNonCommutative[LeftRightPartialD2];

(* ******************************************************************** *)
LeftRightPartialD2[xx__] := LeftRightPartialD2@@ (LorentzIndex /@ {xx}) /;
                 FreeQ2[{xx}, {LorentzIndex, Momentum, OPEDelta, RowBox,
                               Pattern, Blank}] && (Union[{xx}]=!={1});

LeftRightPartialD2[(1)..] = 1;

LeftRightPartialD2[c:OPEDelta..] := LeftRightPartialD2 @@ (Momentum /@ {c});
LeftRightPartialD2[x_LorentzIndex, y__LorentzIndex] :=
 DOT @@ Map[LeftRightPartialD2, {x, y}];
LeftRightPartialD2[x_Momentum, y__Momentum] :=
 DOT @@ Map[LeftRightPartialD2, {x, y}];

LeftRightPartialD2[Momentum[OPEDelta]^n_Integer?Positive] :=
       DOT @@ Map[LeftRightPartialD2, Table[Momentum[OPEDelta],{n}]];

LeftRightPartialD2 /:
   MakeBoxes[ LeftRightPartialD2[x_] , TraditionalForm
            ] :=
    SubscriptBox[OverscriptBox[
       "\[PartialD]", "\[LeftRightArrow]"], Tbox[x]];


End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "LeftRightPartialD2 | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: LeptonSpinor *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: LeptonSpinor denotes spinors *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`LeptonSpinor`",
             "HighEnergyPhysics`FeynCalc`"];

LeptonSpinor::usage = "LeptonSpinor[p, m, optarg]
is equivalent to Spinor.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


LeptonSpinor:= LeptonSpinor = MakeContext["Spinor"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "LeptonSpinor | \n "]];
Null

(* :Title: LeviCivita *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: LeviCivita *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`LeviCivita`",
             "HighEnergyPhysics`FeynCalc`"];

LeviCivita::usage =
"LeviCivita[mu, nu, ro, si] is an input  function for the
totally antisymmetric Levi-Civita tensor.
It evaluates automatically
to the internal representation Eps[ LorentzIndex[mu],  LorentzIndex[nu],
LorentzIndex[ro], LorentzIndex[si] ]
(or with a second argument in LorentzIndex for the Dimension,
if the option Dimension of LeviCivita is changed).  \n
LeviCivita[mu, nu ...][ p, ...] evaluates to
Eps[LorentzIndex[mu], LorentzIndex[nu], ..., Momentum[p], ...].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[ Dimension, Eps, EpsEvaluate, FreeQ2, LorentzIndex, Momentum];

LeviCivita[a__Integer] := Eps[a];

Options[LeviCivita] = {Dimension -> 4};

frlivc[x_?NumberQ] :=True;
frlivc[x_]         := True/;(Head[x]=!=Momentum) &&
                        (Head[x]=!=LorentzIndex);
frlivc[x_,y__] := True/;FreeQ2[FixedPoint[ReleaseHold,{x,y}]
                               ,{Momentum, LorentzIndex}];

HoldPattern[LeviCivita[a___, b_, c___, b_, k___, ops___Rule]] := 0;
LeviCivita[ a__, ops___Rule ]:= ( Eps @@ ( LorentzIndex /@ {a} )
                                ) /; frlivc[a] && FreeQ[{a}, Rule] &&
                (Length[{a}] === 4) &&
                ( (Dimension /. {ops} /. Options[LeviCivita]) === 4);

LeviCivita[ a__, ops___Rule ]:= ( Eps@@(
   LorentzIndex[#, Dimension/.{ops}/.Options[LeviCivita]]& /@{a}
         )                     ) /;
   frlivc[a] && FreeQ[{a},Rule] && (Length[{a}] === 4) &&
   ( (Dimension /. {ops} /. Options[LeviCivita]) =!= 4);

LeviCivita[x___, ops___Rule][y___, ru___Rule] :=
  Eps @@ Join[Map[
    LorentzIndex[#, Dimension /. {ops} /. Options[LeviCivita]]& ,{x}
                 ],
              Map[
      Momentum[#, Dimension /. {ru} /. Options[LeviCivita]]& ,{y}
                 ]
(*,
              {Dimension-> (Dimension/.{ops}/.Options[LeviCivita])}
*)
             ];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "LeviCivita | \n "]];
Null
(* :Title: LeviCivitaSign *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Option  for DiracTrace and Tr *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`LeviCivitaSign`",
             "HighEnergyPhysics`FeynCalc`"];

LeviCivitaSign::usage =
"LeviCivitaSign is an option for DiracTrace and EpsChisholm. It determines
the sign in the result of a Dirac trace of four gamma matrices and gamma5.";
(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "LeviCivitaSign | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Loop *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Loop`",
             "HighEnergyPhysics`FeynCalc`"];

Loop::usage= "Loop is an option indicating the number of (virtual) loops.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Loop | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* Title LorentzIndex *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 24 March '98 at 16:17 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Lorentz index *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`LorentzIndex`",
             "HighEnergyPhysics`FeynCalc`"];

LorentzIndex::usage= "LorentzIndex[mu] is the head of Lorentz indices.
The internal representation of a four-dimensional mu is
LorentzIndex[mu]. For other than four dimensions:
LorentzIndex[mu, Dimension].
LorentzIndex[mu, 4] simplifies to LorentzIndex[mu].
If the first argument is an integer, LorentzIndex[i] turns into
ExplicitLorentzIndex[i].";


(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


 MakeContext[ExplicitLorentzIndex];

SetAttributes[LorentzIndex, Constant ];
LorentzIndex[LorentzIndex[in_]]  := LorentzIndex[in];
LorentzIndex[x_, 4]              := LorentzIndex[x];
LorentzIndex[_, 0]               := 0;
LorentzIndex[in_Integer,dim___]  := ExplicitLorentzIndex[in,dim];

LorentzIndex /:
   MakeBoxes[ LorentzIndex[p_, in___], TraditionalForm
            ] := If[$LorentzIndices =!= True,
                    ToBoxes[p,TraditionalForm],
                    If[{in} === {},
                       MakeBoxes[p, TraditionalForm],
                       SubscriptBox[ToBoxes[p, TraditionalForm],
                                    ToBoxes[in, TraditionalForm]
                                   ]
                      ]
                   ];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "LorentzIndex | \n "]];
Null
(* :Title: Lower *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Lower`",
             "HighEnergyPhysics`FeynCalc`"];

Lower::usage= "Lower may be used inside LorentzIndex to indicate an
covariant LorentzIndex.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Lower | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: MT *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`MT`",
             "HighEnergyPhysics`FeynCalc`"];

MT::usage=
"MT[mu, nu] is the metric tensor in 4 dimensions.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


fci := fci  = MakeContext["FeynCalcInternal"];

MakeContext[ Momentum, SP, SPD];

MT[Momentum[a_], Momentum[b_]] := SP[a,b];
MT[Momentum[a_,D], Momentum[b_,D]] := SPD[a,b];


   MT /: MakeBoxes[ MT[x_,y__], TraditionalForm ] :=
   SuperscriptBox["g", HighEnergyPhysics`FeynCalc`Tbox[x,y]];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "MT | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: MTD *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`MTD`",
             "HighEnergyPhysics`FeynCalc`"];

MTD::usage=
"MTD[mu, nu] is the metric tensor in D dimensions.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


fci := fci = MakeContext["FeynCalcInternal"];

MTD /:
   MakeBoxes[ MTD[x_,y_], TraditionalForm ] :=
SuperscriptBox["g", HighEnergyPhysics`FeynCalc`Tbox[x,y]];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "MTD | \n "]];
Null
(* :Title: Mandelstam *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Option for several functions *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Mandelstam`",
             "HighEnergyPhysics`FeynCalc`"];

Mandelstam::usage =
"Mandelstam is an option for DiracTrace, OneLoop, OneLoopSum, Tr
and TrickMandelstam.  A typical setting is
Mandelstam -> {s, t, u, m1^2 + m2^2 + m3^2 + m4^2},
which stands for  s + t + u = m1^2 + m2^2 + m3^2 +  m4^2.
If other than four-particle processes are calculated the
setting should be: Mandelstam -> {}. .";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Mandelstam | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: MemSet *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 24 July '98 at 20:19 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: MemSet is like Set *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`MemSet`",
             "HighEnergyPhysics`FeynCalc`"];

MemSet::usage =
"MemSet[f[x_], body] is like f[x_] := f[x] = body,
but dependend on the value of the setting of MemoryAvailable ->
memorycut (memorycut - MemoryInUse[]/10.^6)
MemSet[f[x_], body] may evaluate as f[x_] := body."


(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[MemoryAvailable];

(* memset : a dynamical memory dependent "Set" function *)


SetAttributes[MemSet, HoldFirst];

Options[MemSet] = {MemoryAvailable -> $MemoryAvailable};

(*
If[!ValueQ[$MemoryAvailable], $MemoryAvailable = 0];

MemSet[x_,y_] := If[($MemoryAvailable - MemoryInUse[]/1000000.) <1.,
                    y, Set[x, y]];
*)
MemSet[x_,y_, ops___Rule] :=
If[((MemoryAvailable/.{ops} /. Options[MemSet]) -
      MemoryInUse[]/1000000.) <1.,
   y, Set[x, y]
  ];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "MemSet | \n "]];
Null
(* :Title: MemoryAvailable *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: MemoryAvailable *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`MemoryAvailable`",
             "HighEnergyPhysics`FeynCalc`"];

MemoryAvailable::usage =
"MemoryAvailable is an option of MemSet.
It can be set to an integer n,
where n is the available amount of main memory in Mega Byte.
The default setting is $MemoryAvailable.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "MemoryAvailable | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: MetricTensor *)

(* :Author: Rolf Mertig *)


(* :Summary: *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`MetricTensor`",
               "HighEnergyPhysics`FeynCalc`"];

MetricTensor::usage=
"MetricTensor[mu, nu] is the metric tensor in 4 dimensions.
The metric tensor in d dimensions is obtained by supplying the
option Dimension->d.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];



fci:= fci = MakeContext["FeynCalcInternal"];

MakeContext[ Dimension, LorentzIndex, Pair];

Options[MetricTensor] = {Dimension -> 4, fci -> True};

MetricTensor[a_, b_, opt___Rule] :=
  Pair[LorentzIndex[a, Dimension /. Dimension -> (Dimension /. {opt} /.
                                     Options[MetricTensor]
                                                 )
                   ] ,
       LorentzIndex[b, Dimension /. Dimension -> (Dimension /. {opt} /.
                                     Options[MetricTensor]
                                                 )
                   ]
      ] /; ( fci /. {opt} /. Options[MetricTensor] ) === True;

 MetricTensor /:
   MakeBoxes[ MetricTensor[x_, y_], TraditionalForm] :=
    SuperscriptBox["g", Tbox[x,y]]

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* :Title: Momentum *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: The head for momenta *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Momentum`",
             "HighEnergyPhysics`FeynCalc`"];

Momentum::usage=
"Momentum[p] is the head of a four momentum (p).
The internal representation of a four-dimensional p is
Momentum[p]. For other than four dimensions: Momentum[p, Dimension].
Momentum[p, 4] simplifies to Momentum[p].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[GaugeXi];

SetAttributes[Momentum, Constant];
Momentum[x_ GaugeXi[y_], dim___] := GaugeXi[y] Momentum[x,dim];
Momentum[x_ n_?NumberQ, di___] := n Momentum[x, di];
Momentum[x_, 4]                := Momentum[x];
Momentum[0, ___]               := 0;
Momentum[_, 0]                 := 0;
Momentum[Momentum[x_, di___], di___] := Momentum[x, di];

   Momentum /:
   MakeBoxes[ Momentum[p_, in___], TraditionalForm
            ] := MakeBoxes[p, TraditionalForm];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Momentum | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: MomentumCombine *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: MomentumCombine *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`MomentumCombine`",
             "HighEnergyPhysics`FeynCalc`"];

MomentumCombine::usage =
"MomentumCombine[expr]  is the inverse operation to
MomentumExpand and ExpandScalarProduct.
MomentumCombine combines also Pair`s.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[ FeynCalcInternal];
momentum := momentum = MakeContext["Momentum"];
pair   := pair = MakeContext["Pair"];

(*momentumExpanddef*)

MomentumCombine[expr_] :=
If[FreeQ[expr, momentum], FeynCalcInternal[expr], expr] //. {
 (n3_. momentum[x_,di___] + n4_. momentum[y_,di___]
 ):>
 (momentum[ Expand[n3 x + n4 y],di]/;(NumberQ[n3]&&NumberQ[n4])),
 (n3_. pair[a_, momentum[x_,di___]] + n4_. pair[a_, momentum[y_,di___]]
 ):>
 (pair[a, momentum[ Expand[n3 x + n4 y],di]
      ]/;(NumberQ[n3] && NumberQ[n4]))
                             };
End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "MomentumCombine | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: MomentumCombine2 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: MomentumCombine2 *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`MomentumCombine2`",
             "HighEnergyPhysics`FeynCalc`"];

MomentumCombine2::usage =
"MomentumCombine2[expr]  is the inverse operation to
MomentumExpand and ExpandScalarProduct.
MomentumCombine2 combines also
FourVectors.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[ FreeQ2 LorentzIndex, Momentum, MomentumExpand, Pair];

(*MomentumExpanddef*)

MomentumCombine2[expr_] := expr /. Plus-> plm;

plm[xX__] := If[Length[{xX}] > 10, Plus[xX],
 Plus[xX] //. {
 (n3_. Momentum[x_,di___] + n4_. Momentum[y_,di___]
 ):>
 (Momentum[ Expand[n3 x + n4 y],di]/;(NumberQ[n3]&&NumberQ[n4])),
 (n3_. Pair[a_LorentzIndex, Momentum[x_,di___]] +
  n4_. Pair[a_LorentzIndex, Momentum[y_,di___]]
 ):> (Pair[a, Momentum[ Expand[n3 x + n4 y],di]
          ]/;(NumberQ[n3] && NumberQ[n4])
     )
,
 (n3_ Pair[a_LorentzIndex, Momentum[x_,di___]] +
  n3_ Pair[a_LorentzIndex, Momentum[y_,di___]]
 ):> (n3 Pair[a, Momentum[Expand[x+y], di]]/;(!NumberQ[n3])
     ),
 (n3_. Pair[a_LorentzIndex, Momentum[x_,di___]] +
  n4_. Pair[a_LorentzIndex, Momentum[y_,di___]]
 ):> (Pair[a, Expand[MomentumExpand[
              n3 Momentum[Expand[x], di] + n4 Momentum[Expand[y],di]
                    ]              ]
          ]/;(!NumberQ[n3] || NumberQ[n4]) &&
             FreeQ2[{n3, n4}, {Pair, Momentum, LorentzIndex}]
     )
                                    }];
End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "MomentumCombine2 | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* :Title: MomentumExpand *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: MomentumExpand *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`MomentumExpand`",
             "HighEnergyPhysics`FeynCalc`"];

MomentumExpand::usage =
"MomentumExpand[expr] expands Momentum[a+b+ ...] in expr into
Momentum[a] + Momentum[b] + ... .";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[Momentum];

(*MomentumExpanddef*)

hold[]=Sequence[];
fourvecevlin[n_?NumberQ z_, dime___]  := n Momentum[z, dime];
  fourvecev[y_,di___] := ( fourvecev[y,di] =
    Distribute[fourvecevlin[
      Expand[y, Momentum], hold[di]]
              ] /. {hold :> Identity, fourvecevlin :> Momentum}
                         );

(*
 fourvecev[y_,di___] := (fourvecev[y,di] =
   ReleaseHold[Distribute[fourvecevlin[
     Expand[FixedPoint[ReleaseHold, y], Momentum], Hold[di]]
                         ] /. fourvecevlin -> Momentum ]);
*)
MomentumExpand[x_] := x /. Momentum -> fourvecev;

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "MomentumExpand | \n "]];
Null
(* :Title: NTerms *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  NTerms is like Power *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`NTerms`",
             "HighEnergyPhysics`FeynCalc`"];

NTerms::usage=
"NTerms[x] is equivalent to Length if x is a sum; otherwise
NTerms[x] returns 1, except NTerms[0] -> 0."

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


   NTerms[x_Plus] := Length[x];    (*NTermsdef *)
   NTerms[x_] := Block[{ntermslex = Expand[x]},
                       If[ Head[ntermslex]===Plus,
                           ntermslex = Length[ntermslex],
                           If[x===0, ntermslex = 0, ntermslex = 1]
                         ];
             ntermslex];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "NTerms | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: NegativeInteger *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: NegativeInteger  is just a datatype *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`NegativeInteger`",
             "HighEnergyPhysics`FeynCalc`"];

NegativeInteger::usage =  "NegativeInteger is a data type.
E.g. DataType[n, NegativeInteger] can be set to True.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"]

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "NegativeInteger | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Nf *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Nf *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Nf`",
             "HighEnergyPhysics`FeynCalc`"];

Nf::usage =
"Nf denotes the number of flavors."

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   Nf /:
   MakeBoxes[Nf, TraditionalForm] := SubscriptBox["N", "f"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Nf | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: NonCommFreeQ *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: test for non-commutativity *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`NonCommFreeQ`",
             "HighEnergyPhysics`FeynCalc`"];

NonCommFreeQ::usage =
"NonCommFreeQ[exp] yields True if exp contains no non-commutative
objects (i.e. those objects which are listed in $NonComm)
or is a DiracTrace or SUNTrace.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];




MakeContext[ DiracTrace, FreeQ2, SUNTrace, MemSet];

NonCommFreeQ[x_?NumberQ]   := True;
(*
noncommq[x_SUNTrace]   := True;
noncommq[x_DiracTrace] := True;
*)
NonCommFreeQ[x_]           := MemSet[NonCommFreeQ[x], FreeQ2[x, $NonComm]];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "NonCommFreeQ | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: NonCommQ *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: test for non-commutativity *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`NonCommQ`",
             "HighEnergyPhysics`FeynCalc`"];

NonCommQ::usage =
"NonCommQ[exp] yields True if exp contains no non-commutative
objects (i.e. those objects which are listed in $NonComm)
or is a DiracTrace or SUNTrace.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[ DiracTrace, FreeQ2, SUNTrace, MemSet];

NonCommQ[x_?NumberQ]   := True;
(*
noncommq[x_SUNTrace]   := True;
noncommq[x_DiracTrace] := True;
NonCommQ[x_]           := NonCommQ[x] = FreeQ2[x, $NonComm];
*)
NonCommQ[x_]           := MemSet[NonCommQ[x], FreeQ2[x, $NonComm]];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "NonCommQ | \n "]];
Null
(* :Title: NonCommutative *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: NonCommutative *)

(* :Package Version 2.1 *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`NonCommutative`",
             "HighEnergyPhysics`FeynCalc`"];

NonCommutative::usage=
"NonCommutative is a data type which may be used, e.g.,  as:
DataType[x, NonCommutative] = True.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "NonCommutative | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: NumberOfMetricTensors *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`NumberOfMetricTensors`",
             "HighEnergyPhysics`FeynCalc`"];

NumberOfMetricTensors::usage=
"NumberOfMetricTensors is an option of Tdec.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "NumberOfMetricTensors | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* :Title: NumericalFactor *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: NumericalFactor take out a numerical factor *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`NumericalFactor`",
             "HighEnergyPhysics`FeynCalc`"];

NumericalFactor::usage = "NumericalFactor NumericalFactor[expr]
gives the numerical factor of expr.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


NumericalFactor[a___ /; Length[{a}] =!=1] :=
soso /; Message[NumericalFactor::argrx, NumericalFactor, Length[{a}], 1];
NumericalFactor[x_]:= If[NumberQ[x], x, If[Head[x] === Times,
                         If[NumberQ[First[x]], First[x], 1], 1]];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "NumericalFactor | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: OPE *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`OPE`",
             "HighEnergyPhysics`FeynCalc`"];

OPE::usage= "OPE is used internally in OPE1Loop.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


OPE /: OPE^_Integer?Positive := 0;

   OPE /: MakeBoxes[OPE, TraditionalForm] := "\[CapitalKoppa]"

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "OPE | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PaVeOrderList *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`PaVeOrderList`",
             "HighEnergyPhysics`FeynCalc`"];

PaVeOrderList::usage=
"PaVeOrderList is an option for PaVeOrder and PaVeReduce,
specifying in which order the arguments of D0 are to be permuted.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PaVeOrderList | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Pair*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 20 December '98 at 23:59 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: The head of four-vectors, metric tensor and
             scalar products. *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Pair`",
             "HighEnergyPhysics`FeynCalc`"];

Pair::usage=
"Pair[a , b] is the head of a special pairing used in the internal
representation: a and b may have heads LorentzIndex or Momentum.
If both a and b have head LorentzIndex, the metric tensor is
understood. If a and b have head Momentum, a scalar product is
meant. If one of a and b has head LorentzIndex and the other
Momentum, a Lorentz vector (p_mu) is understood.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[FreeQ2,LorentzIndex, Momentum, MomentumCombine, Polarization];


contract            := contract = MakeContext["Contract"];
expandscalarproduct := expandscalarproduct =
                       MakeContext["ExpandScalarProduct"];

SetAttributes[Pair, Orderless];
Pair[0,_] := 0;
Pair[n_Integer x_,y_] := n Pair[x, y];
Pair[n_ x_Momentum, y_] := n Pair[x, y];

Pair[ lom_[la_,d_Symbol], mol_[pe_]] := Pair[ lom[la], mol[pe] ] /;
  MemberQ[{LorentzIndex, Momentum}, lom] &&
     MemberQ[{LorentzIndex, Momentum}, mol] ;

(*
Pair[y_, n_,x_Momentum] := n Pair[y, x];
Pair[n_ x_LorentzIndex, y_] := n Pair[x, y];
Pair[y_, n_,x_LorentzIndex] := n Pair[y, x];
*)

(*
Pair[Momentum[ n_Integer x_,di___],y_] :=  n Pair[Momentum[x,di],y];
*)

Pair[Momentum[x_,___],Momentum[Polarization[x_, ___],___]] := 0;
Pair[Momentum[x_,___],Momentum[Polarization[n_?NumberQ x_, ___],___]
    ] := 0;
Pair[Momentum[pi_,___],Momentum[Polarization[x_Plus, ki___], dii___]
    ]:= contract[expandscalarproduct[Pair[
             Momentum[x+pi, dii], Momentum[Polarization[x, ki], dii]]]
                ] /; ( pi + Last[x] ) === 0;
Pair[Momentum[pi_,___],Momentum[Polarization[x_Plus, ki___], dii___]
    ]:= contract[expandscalarproduct[Pair[
             Momentum[pi-x,dii], Momentum[Polarization[x, ki],dii]]]
                ] /; ( pi - Last[x] ) === 0;
(* by convention ... *)
Pair[Momentum[Polarization[x_,__],___],
     Momentum[Polarization[x_,__],___] ] := -1;

(* ******************************************************************** *)
Unprotect[Conjugate];
Conjugate[x_Pair] := (x /. {Polarization[k_,a_,in___] :>
                            Polarization[k,Conjugate[a],in] }
                     ) /;!FreeQ[x, Polarization];
Protect[Conjugate];
(* ******************************************************************** *)


Pair /:
   MakeBoxes[Pair[
HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex[a_],
HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex[b_] ],
             TraditionalForm
            ] := SuperscriptBox["g",Tbox[a,b] ];
Pair /:
   MakeBoxes[Pair[
HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex[a__],
HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex[b__] ],
             TraditionalForm
            ] :=
SuperscriptBox[ "g",
                 Tbox[LorentzIndex[a], LorentzIndex[b]]
              ];

MakeBoxes[Pair[a_,b_]^n_Integer?Positive, TraditionalForm] :=
 RowBox[{SuperscriptBox[Tbox[Pair[a,b]],n]}];

initialDownValues = DownValues[Pair];

Pair /:
   MakeBoxes[Pair[
    HighEnergyPhysics`FeynCalc`Momentum`Momentum[a__],
    HighEnergyPhysics`FeynCalc`Momentum`Momentum[a__]
                 ], TraditionalForm
            ] := SuperscriptBox[Tbox[Momentum[a]],2];

MakeBoxes[Pair[
    HighEnergyPhysics`FeynCalc`Momentum`Momentum[a__],
    HighEnergyPhysics`FeynCalc`Momentum`Momentum[a__]
                 ]^2, TraditionalForm
            ] := SuperscriptBox[Tbox[Momentum[a]],4];

MakeBoxes[Pair[
    HighEnergyPhysics`FeynCalc`Momentum`Momentum[a__],
    HighEnergyPhysics`FeynCalc`Momentum`Momentum[a__]
                 ]^3, TraditionalForm
            ] := SuperscriptBox[Tbox[Momentum[a]],6];

(* Changed because of infinite recursion on
   Pair[a Momentum[k] + b Momentum[p], a Momentum[k] + b Momentum[p]]
   Frederik Orellana, 17/3-2001 *)
(*Pair/:
       MakeBoxes[Pair[a_Plus,b_],TraditionalForm] :=
        ToBoxes[Pair[MomentumCombine[a],MomentumCombine[b]],
                TraditionalForm] /; !FreeQ[a, Momentum] &&
                                    !FreeQ[b, Momentum];*)
Pair /: MakeBoxes[Pair[a_Plus, b_], TraditionalForm] :=
    RowBox[{"(", ToBoxes[TraditionalForm[a]], ")",".","(",
          ToBoxes[TraditionalForm[b]], ")"}] /; ! FreeQ[a, Momentum] && !
          FreeQ[b, Momentum];

Pair /:
        MakeBoxes[Pair[
          HighEnergyPhysics`FeynCalc`Momentum`Momentum[a_,di___],
          HighEnergyPhysics`FeynCalc`Momentum`Momentum[b_,dii___]],
                  TraditionalForm
                 ] := Which[
                       FreeQ2[{a,b},{Times,Plus}],
                       If[$PairBrackets === True,
                          Tbox["(", Momentum[a,di], "\[CenterDot]",
                                    Momentum[b,dii], ")"
                              ],
                          Tbox[Momentum[a,di], "\[CenterDot]",
                               Momentum[b,dii]]
                         ],
                       FreeQ2[{a},{Times,Plus}],
                       Tbox[Momentum[a,di],"\[CenterDot]",
                            "(",Momentum[b,dii],")"],
                       FreeQ2[{b},{Times,Plus}],
                       Tbox["(",Momentum[a,di],")","\[CenterDot]",
                            Momentum[b,dii]],
                       !FreeQ2[{a,b},{Times,Plus}],
                       Tbox["(",Momentum[a,di],")","\[CenterDot]",
                            "(",Momentum[b,dii],")"]
                           ];

Pair /:
   MakeBoxes[Pair[
      HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex[a__],
      HighEnergyPhysics`FeynCalc`Momentum`Momentum[
      HighEnergyPhysics`FeynCalc`Polarization`Polarization[
                              b_,Complex[0,1]],___]
                 ], TraditionalForm
            ] := RowBox[{
        SubscriptBox["\[CurlyEpsilon]", Tbox[a]],"(",Tbox[b],")"}];

Pair /:
   MakeBoxes[Pair[
      HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex[a__],
      HighEnergyPhysics`FeynCalc`Momentum`Momentum[
      HighEnergyPhysics`FeynCalc`Polarization`Polarization[
                              b_,Complex[0,-1]],___]
                 ], TraditionalForm
            ] := RowBox[{
        SubsuperscriptBox["\[CurlyEpsilon]", Tbox[a], "*"
                          ], "(", Tbox[b], ")"
                        }
                       ];

Pair /:
   MakeBoxes[Pair[
              HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex[a__],
              HighEnergyPhysics`FeynCalc`Momentum`Momentum[
                   b_Subscripted, di___]
                 ], TraditionalForm
            ] := SubsuperscriptBox[Tbox[b[[1,0]]],
                                   Tbox@@b[[1]],
                                    Tbox[LorentzIndex[a]]];

Pair /:
   MakeBoxes[Pair[
              HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex[a__],
              HighEnergyPhysics`FeynCalc`Momentum`Momentum[
                   b_Subscript,di___]
                 ], TraditionalForm
            ] := SubsuperscriptBox[Tbox[b[[1]]], Tbox@@Rest[b],
                                    Tbox[LorentzIndex[a]]];

Pair /:
   MakeBoxes[Pair[
              HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex[a__],
              HighEnergyPhysics`FeynCalc`Momentum`Momentum[b_,di___] + c_.
                 ],
             TraditionalForm
            ] := SuperscriptBox[
                  Tbox[MomentumCombine[Momentum[b,di] + c]],
                                     Tbox[LorentzIndex[a]]
                               ];
End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Pair | \n "]];
Null
(* :Title: PairCollect *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: PairCollect is an option for several functions *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`PairCollect`",
             "HighEnergyPhysics`FeynCalc`"];

PairCollect::usage =
"PairCollect is an option for DiracTrace specifying if
the result is collected with respect to Pair's.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PairCollect | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PairContract *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: PairContract *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`PairContract`",
             "HighEnergyPhysics`FeynCalc`"];

PairContract::usage =
"PairContract is like Pair, but with (local) contraction properties.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


eps := eps                  = MakeContext["Eps"];
factoring := factoring      = MakeContext["Factoring"];
factor2 := factor2          = MakeContext["Factor2"];
freeq2 := freeq2            = MakeContext["FreeQ2"];
LorentzIndex:= LorentzIndex = MakeContext["LorentzIndex"];
memset := memeset           = MakeContext["MemSet"];
MakeContext[ Momentum, MomentumExpand, Pair];

(* this option is only to by set by SetOptions ... *)
Options[PairContract] = {Factoring -> False};

sCO = PairContract;

SetAttributes@@{{sCO,sceins,scev,sce,scevdoit,sczwei} ,Orderless};

scev[x_,y_]:= memset[ scev[x,y], scevdoit[x,y] ];
scev[x_,y_]:= scevdoit[x,y];
scevdoit[x_,y_] := Distribute[ sceins@@
                              ( Expand[ MomentumExpand/@{x,y} ] )
                    ]/.sceins->sczwei/.sczwei->sCO/.sCO->Pair;

sCO[ LorentzIndex[a_,di___], epsmu_ LorentzIndex[mu_, dimen___] ]:=
( epsmu /. LorentzIndex[mu,dimen]->LorentzIndex[a,di] ) /;
(!freeq2[epsmu, {eps, LorentzIndex[mu, dimen]}]);

sCO[ Momentum[x_,___],Momentum[polarization[x_,___]]]:=0;
sCO[ Momentum[x_,___],Momentum[polarization[n_?NumberQ x_,___]]]:=0;
sCO[Momentum[pi_,___],Momentum[polarization[x_Plus, ki___]]]:=
 scev[Momentum[x+pi], Momentum[polarization[x, ki]]]/;
     ( pi + Last[x] )===0;
sCO[Momentum[pi_,___],Momentum[polarization[x_Plus, ki___]]]:=
 scev[Momentum[pi-x], Momentum[polarization[x, ki]]]/;
             ( pi - Last[x] )===0;

sCO[ LorentzIndex[x_], LorentzIndex[x_] ]  := 4;
(*new ...*)
sCO[ LorentzIndex[x_], LorentzIndex[x_,_Symbol] ]  := 4;

sCO[ LorentzIndex[x_,di_], LorentzIndex[x_,di_] ] := di;
PairContract /: HoldPattern[PairContract[lor_[z_,___],x_]]^2 :=
                (PairContract[x,x]) /; lor === LorentzIndex;
(*
 sCO/: HoldPattern[ sCO[LorentzIndex[z_,___],x_] f_[a__][b___] ] :=
 (f[a][b]/.LorentzIndex[z,___]->x)/;
  (!FreeQ[f[a][b]//Hold,LorentzIndex[z,___]]);
*)


(* CHANGE 09/94 *)
PairContract[Momentum[x_,___],Momentum[Polarization[x_, ___],___]] := 0;
PairContract[Momentum[x_,___],Momentum[Polarization[n_?NumberQ x_, ___],___]
    ] := 0;
PairContract[Momentum[pi_,___],Momentum[Polarization[x_Plus, ki___], dii___]
    ]:= contract[expandscalarproduct[Pair[
             Momentum[x+pi, dii], Momentum[Polarization[x, ki], dii]]]
                ] /; ( pi + Last[x] ) === 0;
PairContract[Momentum[pi_,___],Momentum[Polarization[x_Plus, ki___], dii___]
    ]:= contract[expandscalarproduct[Pair[
             Momentum[pi-x,dii], Momentum[Polarization[x, ki],dii]]]
                ] /; ( pi - Last[x] ) === 0;
(* by convention ... *)
PairContract[Momentum[Polarization[x_,__],___],
     Momentum[Polarization[x_,__],___] ] := -1;
(* CHANGE 09/94 *)

(*
PairContract/: PairContract[LorentzIndex[z_,___],x_] *
               PairContract[LorentzIndex[z_,___],y_] :=
           PairContract[x,y];
*)

PairContract/: PairContract[LorentzIndex[z_,___],x_] f_[a__] :=
(f[a]/.LorentzIndex[z,___]->x)/;
 (!FreeQ[f[a],LorentzIndex[z,___]]);

sCO[Momentum[a_Symbol,b_Symbol]]:=Pair[Momentum[a],Momentum[b]];

PairContract/:
  HoldPattern[ Dot[A___, PairContract[lor_[z_,___],x_],B___,
                 m_. f_[a__], c___ ] ] :=
 Dot[A,B,(m f[a]/.LorentzIndex[z,___]->x),c]/;
    ((!FreeQ[f[a], LorentzIndex[z,___]]) && (lor === LorentzIndex));

PairContract/: HoldPattern[ Dot[A___, m_. f_[a__],B___,
                   PairContract[lor_[z_,___],x_], c___ ] ] :=
 Dot[A.(m f[a]/.LorentzIndex[z,___]->x),B,c]/;
   ((!FreeQ[f[a]//Hold,LorentzIndex[z,___]]) && (lor === LorentzIndex));
(* **************************************************************** *)
(* definitions for dimension = D-4                                  *)
(* **************************************************************** *)
 sCO[ _[_,_Symbol-4],_[_] ]:=0;
 sCO[ v_[x_,di_Symbol-4],w_[y_,di_Symbol] ] :=
                                            sCO[v[x,di-4],w[y,di-4] ];
 sCO[ w_[y_,di_Symbol],v_[x_] ] := sCO[ v[x], w[y] ];
 sCO[ v_[x_], w_[y_,di_Symbol] ] := sCO[ v[x], w[y] ];
 sceins[0,_]:=0;                               (*sceinsdef*)
 sceins[a_LorentzIndex b_, c_] := b sceins[a, c];
 sceins[a_Momentum b_, c_] := b sceins[a, c];
 sczwei[ _[_],_[_,_Symbol-4] ]:=0;             (*sczweidef*)
 sczwei[ v_[x_,di_Symbol-4],w_[y_,di_Symbol] ]:=
                            sczwei[v[x, di-4], w[y, di-4]];
 sczwei[ w_[y_,di_Symbol],v_[x_] ]:=sczwei[ v[x],w[y] ];
 sce[x_,y_] := memset[set[x, y],      (*scedef*)
               If[(factoring /. Options[PairContract]) === True,
               factor2[
               Distribute[sceins@@( Expand[ MomentumExpand/@{x,y} ])
                         ]/.sceins->sczwei/.sczwei->Pair
                     ],
                Distribute[sceins@@( Expand[ MomentumExpand/@{x,y} ])
                         ]/.sceins->sczwei/.sczwei->Pair
                 ]   ];
 sCO[x_,y_] := memset[ sCO[x,y],
                      Block[{sCOt=sce[x,y]},
                       If[ FreeQ[ sCOt, Pair ] ||
                            (Head[sCOt]=!=Plus)
                           , sCOt,Pair[x,y]
                         ] ] ]/;FreeQ[{x,y},LorentzIndex];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PairContract | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PairContract2 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: PairContract2 *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`PairContract2`",
             "HighEnergyPhysics`FeynCalc`"];

PairContract2::usage =
"PairContract2 is like Pair, but with local contraction properties
among PairContract2's.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[ FreeQ2, LorentzIndex, Momentum, Pair];

SetAttributes[PairContract2,Orderless];

(*
PairContract2[Momentum[a__], Momentum[b__] ] :=
Pair[Momentum[a], Momentum[b]];
*)

Clear[PairContract2];
PairContract2/:
PairContract2[LorentzIndex[z_],x_] *
PairContract2[LorentzIndex[z_],y_] :=
If[FreeQ[{x,y},LorentzIndex], Pair[x,y],
PairContract2[x,y]];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PairContract2 | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PairContract3 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: PairContract3 *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`PairContract3`",
             "HighEnergyPhysics`FeynCalc`"];

PairContract3::usage =
"PairContract3 is like Pair, but with local contraction properties
among PairContract3's.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[ ExpandScalarProduct, FreeQ2, LorentzIndex, Momentum, Pair];
SetAttributes[PairContract3,Orderless];

PairContract3[LorentzIndex[z_,di___],
              LorentzIndex[z_,di___]]:= If[{di}==={},4,di];
PairContract3[Momentum[a__], Momentum[b__]]:=
ExpandScalarProduct[Momentum[a], Momentum[b]];

PairContract3 /:
PairContract3[LorentzIndex[z__],LorentzIndex[x__]]^2 :=
PairContract3[LorentzIndex[x],LorentzIndex[x]];

PairContract3 /:
PairContract3[LorentzIndex[z__],x_]^2 :=
ExpandScalarProduct[x,x];


PairContract3/:
PairContract3[LorentzIndex[z__],x_] *
PairContract3[LorentzIndex[z__],y_] :=
If[FreeQ[{x,y},LorentzIndex],
   ExpandScalarProduct[x,y],
   PairContract3[x,y]];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PairContract3 | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PartialD *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 11 March '98 at 22:03 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: partial derivative *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`PartialD`",
             "HighEnergyPhysics`FeynCalc`"];

PartialD::usage=
"PartialD[mu] denotes partial_mu";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[ DOT, FreeQ2, LorentzIndex, Momentum, OPEDelta,
DeclareNonCommutative];

DeclareNonCommutative[PartialD];

(* ******************************************************************** *)
If[!MemberQ[$NonComm, PartialD], AppendTo[$NonComm, PartialD]];

PartialD[xx__] := PartialD @@ (LorentzIndex /@ {xx}) /;
                 FreeQ2[{xx}, {LorentzIndex, Momentum, OPEDelta, RowBox,
                               Pattern, Blank}] && (Union[{xx}]=!={1});

PartialD[(1)..] = 1;
PartialD[c:OPEDelta..] := PartialD @@ (Momentum /@ {c});
PartialD[x_LorentzIndex, y__LorentzIndex] := DOT @@ Map[PartialD, {x, y}];
PartialD[x_Momentum, y__Momentum] := DOT @@ Map[PartialD, {x, y}];

(*
PartialD[Momentum[OPEDelta]^n_Integer?Positive] :=
       DOT @@ Map[PartialD, Table[Momentum[OPEDelta],{n}]];
*)
 PartialD /:
   MakeBoxes[PartialD[x_ ^n_],TraditionalForm] :=
    SubsuperscriptBox["\[PartialD]", Tbox[x], Tbox[n]
                     ] /; Head[x] === Momentum;

   HighEnergyPhysics`FeynCalc`PartialD`PartialD /:
   MakeBoxes[ HighEnergyPhysics`FeynCalc`PartialD`PartialD[x_],
              TraditionalForm
            ] :=
    SubscriptBox["\[PartialD]", ToBoxes[x,TraditionalForm]];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PartialD | \n "]];
Null
(* :Title: PartitHead *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: separation of expression according to a head *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`PartitHead`",
             "HighEnergyPhysics`FeynCalc`"];

PartitHead::usage=
"PartitHead[expr, h] returns a list {ex1, h[ex2]} with ex1 free of
expressions with head h, and h[ex2] having head h.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


PartitHead[x_, y_]     := {1, x} /; Head[x] === y;
PartitHead[x_Times, y_]:= {x, 1} /; FreeQ[x, y];
PartitHead[x_, y_]     := {x, 0} /; FreeQ[x, y];
PartitHead[x_Plus, y_] := {#, x - #}& @ Select[x, FreeQ[#, y[___]]&];
PartitHead[x_Times,y_] := {x/#, #}& @ Select[x,If[Head[#]===y,True]&];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PartitHead | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PauliSigma *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: PauliSigma stands for the vector of Pauli matrices
*)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`PauliSigma`",
             "HighEnergyPhysics`FeynCalc`"];

PauliSigma::usage =
"PauliSigma denotes the vector of the 3 Pauli matrices.
PauliSigma[1], PauliSigma[2], PauliSigma[3] gives the
explicit Pauli matrices. PauliSigma[] yields
{PauliSigma[1], PauliSigma[2], PauliSigma[3]}";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


DeclareNonCommutative[PauliSigma];

PauliSigma[1] = { {0, 1}, {1,0} };
PauliSigma[2] = { {0,-I}, {I,0} };
PauliSigma[3] = { {1, 0}, {0,-1}};

PauliSigma[] = {PauliSigma[1], PauliSigma[2], PauliSigma[3]};

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PauliSigma | \n "]];
Null


(* :Title: PlusDistribution *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  a head for (1/(1-x))_+  and
                         (Log[1-x]/(1-x))_+
*)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`PlusDistribution`",
             "HighEnergyPhysics`FeynCalc`"];

PlusDistribution::usage=
"PlusDistribution[1/(1-x)] denotes the distribution (1/(1-x))_+.
 PlusDistribution[Log[1-x]/(1-x)] denotes the distribution
 (Log[1-x]/(1-x))_+.
PlusDistribution[Log[x (1-x)]/(1-x)] simplifies to
Log[x] /(1-x) + PlusDistribution[Log[1-x]/(1-x)].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


PlusDistribution[Log[x_ (1-x_)]/(1-x_)] :=
Log[x] /(1-x) + PlusDistribution[Log[1-x]/(1-x)];

PlusDistribution /:
   MakeBoxes[
             PlusDistribution[ a_ ], TraditionalForm
            ] :=
   SubscriptBox[ RowBox[{"(",
                 MakeBoxes[a, TraditionalForm],")"}
                       ],"+"
                ];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PlusDistribution | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Polarization *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 30 January '99 at 21:45 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Head for polarization vectors *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Polarization`",
             "HighEnergyPhysics`FeynCalc`"];

Polarization::usage=
"Polarization[k] = Polarization[k, I] is the head of a
polarization momentum with (incoming) momentum k.
A slashed polarization vector (e1(k) slash) has to be entered
as DiracSlash[Polarization[k]].
The internal representation for a polarization vector e1
corresponding to a boson with four momentum k is:
Momentum[ Polarization[ k, I ] ].
With this notation transversality of polarization vectors is
provided, i.e.  Pair[ Momentum[k],
Momentum[ Polarization[k, I] ] ] yields 0.
Polarization[k,-I] denotes the complex conjugate polarization.

Polarization is also an option.
The setting 0 denotes the unpolarized and 1 the polarized case.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[Pair];

(* by convention *)
Polarization[k_ /;FreeQ[k,Blank|BlankSequence|BlankNullSequence] ]:=
  Polarization[k] = Polarization[k, I];

Polarization[k_] := Polarization[k] = Polarization[k, I];

Polarization[-x_, I] := -Polarization[x,I];
Polarization[-x_,-I] := -Polarization[x,-I];

Unprotect[Conjugate];
Conjugate[x_Pair] := (x /. {Polarization[k_,a_,in___] :>
                            Polarization[k,Conjugate[a],in] }
                     ) /;!FreeQ[x, Polarization];
Protect[Conjugate];

Polarization /:
   MakeBoxes[Polarization[a_,Complex[0, 1]], TraditionalForm] :=
        Tbox["\[CurlyEpsilon]","(",a,")"];

Polarization /:
   MakeBoxes[Polarization[a_, Complex[0, -1]], TraditionalForm] :=
        Tbox[Superscript["\[CurlyEpsilon]", "*"], "(", a, ")"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Polarization | \n "]];
Null


(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PolarizationVector *)

(* :Author: Rolf Mertig *)


(* :Summary: PolarizationVector *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`PolarizationVector`",
               "HighEnergyPhysics`FeynCalc`"];


PolarizationVector::usage =
"PolarizationVector[p, mu] gives a polarization vector.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];



dimension := dimension   = MakeContext["Dimension"];
fci := fci               = MakeContext["FeynCalcInternal"];
fourvector := fourvector = MakeContext["FourVector"];
polarization := polarization = MakeContext["Polarization"];
sunindex := sunindex = MakeContext["SUNIndex"];

PolarizationVector[x_,{y_,z_}]:= PolarizationVector[x, y, z];
PolarizationVector[x__]:=
(PolarizationVector[x]=polVec[x] )/; FreeQ[{x}, Pattern];

fourv[x__] := fci[fourvector[x]];
polVec[k_polarization,mu_]:=
     fourv[k, mu, dimension -> 4 ];
polVec[k_polarization,mu_,glu_]:=
     fourv[polarization[
          k, I, sunindex[glu/.sunindex->Identity]],
                mu, dimension->4 ];
polVec[k_,mu_]:=
     fourv[polarization[k, I], mu, dimension->4 ];

polVec[k_,mu_,glu_]:=
     If[FreeQ[glu, Blank],
        fourv[polarization[k, I,
                   sunindex[glu/.sunindex->Identity]],
                   mu, dimension->4 ],
        fourv[polarization[k, I, glu], mu, dimension -> 4]
       ];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)


(* :Title: PositiveInteger *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: PositiveInteger  is just a datatype *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`PositiveInteger`",
             "HighEnergyPhysics`FeynCalc`"];


PositiveInteger::usage =  "PositiveInteger is a data type.
E.g. DataType[OPEm, PositiveInteger] gives True.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"]
End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PositiveInteger | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PositiveNumber *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: PositiveNumber  is just a datatype *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`PositiveNumber`",
             "HighEnergyPhysics`FeynCalc`"];


PositiveNumber::usage =  "PositiveNumber is a data type.
E.g. DataType[Epsilon, PositiveNumber] = True (by default). ";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"]

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PositiveNumber | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)


(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PropagatorDenominator *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 2 July '97 at 13:48 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: PropagatorDenominator *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`PropagatorDenominator`",
             "HighEnergyPhysics`FeynCalc`"];

PropagatorDenominator::usage =
"PropagatorDenominator[Momentum[q], m] is a factor of the denominator of a
propagator.  If p is supposed to be D-dimensional enter:
PropagatorDenominator[Momentum[q, D], m].  What is meant is
1/(q^2-m^2).
PropagatorDenominator[p] evaluates to PropagatorDenominator[p,0].";


(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[FreeQ2];

(*
HoldPattern[PropagatorDenominator[0,0]]:=Block[{},
Print["!@#&**T(&()*&G_*&+#@U(BJ)IM:LM:LM<WPYOU_()FJ_jdfmbw34059hj"];
Print["nononononononononononononononononononononononononononononon"];
Print[" "];
Print[" Gee, somehow a massless and momentumless propagator has been
entered. This is fatal. You would divide by 0, yes.
No way: Go back, renormalize or organize, but this does not work.
Starting Dialog !!!!!"]; Dialog[]
                                 ];
*)

(*
 PropagatorDenominator[0, m_] := -1/m^2;
 PropagatorDenominator[x_]:=PropagatorDenominator[x,0]/;FreeQ[x,Pattern];
 PropagatorDenominator[x_, y_] := (PropagatorDenominator[x, y] =
         PropagatorDenominator[
  MomentumExpand[ Momentum[x] ],y])/;FreeQ2[x,{Momentum, Pattern}];
  MomentumExpandsave[a_] := MomentumExpandsave[a] = MomentumExpand[a];
  PropagatorDenominator[x_, y_] := (PropagatorDenominator[x, y] =
  PropagatorDenominator[x//MomentumExpand, y] )/;
   (FreeQ[{x,y}, Pattern] ) && ( MomentumExpandsave[x] =!= x);
*)

PropagatorDenominator[a_ /; FreeQ2[a, {BlankNullSequence,Pattern}]
                     ] := PropagatorDenominator[a, 0];

PropagatorDenominator/:
   MakeBoxes[PropagatorDenominator[a_, 0], TraditionalForm
            ] := ToBoxes[1/a^2, TraditionalForm];

   MakeBoxes[f_. PropagatorDenominator[a_, b_/;b=!=0], TraditionalForm
            ] := ToBoxes[f/(a^2-b^2), TraditionalForm];


(*
ToBoxes[1/(a^2-b^2), TraditionalForm];

   MakeBoxes[PropagatorDenominator[a_, b_/;b=!=0]^n_, TraditionalForm
            ] := FractionBox[1,
   SuperscriptBox[TBox[a^2," - ", b^2], n]];
*)


End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PropagatorDenominator | \n "]];
Null


(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: QuantumField *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 21 March '98 at 11:52 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: derivation of feynman rules via functional differentiation *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`QuantumField`",
             "HighEnergyPhysics`FeynCalc`"];

QuantumField::usage=
"QuantumField[par1, par2, ..., ftype, {lorind}, {sunind}]
denotes a quantum field of type ftype with (possible)
Lorentz-indices lorind and SU(N)-indices sunind.
the optional first argument par1, par2, ...,  are partial
derivatives (PartialD) acting on the field.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[Momentum, LorentzIndex, OPEDelta, PartialD, SUNIndex];

DeclareNonCommutative = MakeContext["DeclareNonCommutative"];
DeclareNonCommutative[QuantumField];

lori[OPEDelta] := Momentum[OPEDelta];
lori[a_SUNIndex] := a;
lori[a_] := LorentzIndex[a];

QuantumField[f___,g_/;Head[g]=!=List,{lilo___}] :=
 QuantumField@@Join[{f,g},lori/@{lilo}];

QuantumField[f___,g_/;Head[g]=!=List,{lilo___},{suli___}] :=
 QuantumField@@Join[{f,g},lori/@{lilo},SUNIndex/@{suli}];

QuantumField[f1_QuantumField] := f1;


QuantumField /:
   MakeBoxes[ QuantumField[a_][p_], TraditionalForm
            ] := Tbox[a,"(",p,")"];

QuantumField /:
   MakeBoxes[ QuantumField[a_], TraditionalForm
            ] := Tbox[a];

QuantumField /:
   MakeBoxes[ QuantumField[f_, lo_[mu_,___]], TraditionalForm
            ] := SubscriptBox[Tbox[f], Tbox[mu]] /;
                   lo === LorentzIndex;

QuantumField /:
   MakeBoxes[ QuantumField[f_, lori:lo_[_,___].., suni:sun_[_]..
                          ], TraditionalForm
            ] := SubsuperscriptBox[Tbox[f], Tbox[lori], Tbox[suni]
                                  ] /; MatchQ[lo, LorentzIndex | Momentum
                                             ] && sun === SUNIndex;

QuantumField /:
   MakeBoxes[ QuantumField[f_, lori:lo_[_,___].., suni:sun_[_]..
                          ][p_], TraditionalForm
            ] := RowBox[{
           SubsuperscriptBox[Tbox[f], Tbox[lori], Tbox[suni]],
                         "(", Tbox[p], ")"
                        }
                       ] /; MatchQ[lo, LorentzIndex | Momentum] &&
                            sun === SUNIndex;
QuantumField /:
   MakeBoxes[ QuantumField[
    HighEnergyPhysics`FeynCalc`PartialD`PartialD[pa_], a_,
 lori___HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
 suni___HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex
                          ],
              TraditionalForm
            ] := RowBox[{SubscriptBox["\[PartialD]", Tbox[pa]],
                 SubsuperscriptBox[Tbox[a], Tbox[lori], Tbox[suni]]
                        }];

QuantumField /:
   MakeBoxes[ QuantumField[
    HighEnergyPhysics`FeynCalc`PartialD`PartialD[pa_]^m_, a_,
 lori___HighEnergyPhysics`FeynCalc`Momentum`Momentum,
 suni___HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex
                          ],
              TraditionalForm
            ] := RowBox[{SuperscriptBox[Tbox[PartialD[pa]],Tbox[m]],
                 SubsuperscriptBox[Tbox[a], Tbox[lori], Tbox[suni]]
                      }];

QuantumField /:
   MakeBoxes[ QuantumField[
    pa__HighEnergyPhysics`FeynCalc`PartialD`PartialD, a_,
 lori___HighEnergyPhysics`FeynCalc`Momentum`Momentum,
 suni___HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex
                          ],
              TraditionalForm
            ] := RowBox[{TBox[pa],
                 SubsuperscriptBox[Tbox[a], Tbox[lori], Tbox[suni]]
                        }];

QuantumField /:
   MakeBoxes[ QuantumField[
    pa__HighEnergyPhysics`FeynCalc`PartialD`PartialD, a_,
 lori___HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
 suni___HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex
                          ],
              TraditionalForm
            ] := RowBox[{TBox[pa],
                 SubsuperscriptBox[Tbox[a], Tbox[lori], Tbox[suni]]
                        }];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "QuantumField | \n "]];
Null
(* :Title: QuarkField *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: quark field *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`QuarkField`",
             "HighEnergyPhysics`FeynCalc`"];

QuarkField::usage =
"QuarkField is the name of a fermionic field.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

   QuarkField /: MakeBoxes[QuarkField, TraditionalForm] := "\[Psi]";

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "QuarkField | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: QuarkMass *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`QuarkMass`",
             "HighEnergyPhysics`FeynCalc`"];

QuarkMass::usage= "QuarkMass is an option of Amplitudes.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "QuarkMass | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)


(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Rename *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Rename *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Rename`",
             "HighEnergyPhysics`FeynCalc`"];

Rename::usage =
"Rename is an option for Contract. If set to True,
dummy indices in Eps are renamed, using $MU[i].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Rename | \n "]];
Null

(* :Title: SD *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Kronecker delta for SU(N) *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SD`",
             "HighEnergyPhysics`FeynCalc`"];

SD::usage=
"SD[i, j] is the (FeynCalc-external) Kronecker-delta for SU(N) with color
indices i and j. SD[i,j] is transformed into
SUNDelta[SUNIndex[i],SUNIndex[j]] by
FeynCalcInternal.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


SetAttributes[SD, Orderless];

HighEnergyPhysics`FeynCalc`SD`SD /:
MakeBoxes[HighEnergyPhysics`FeynCalc`SD`SD[a_, b_], TraditionalForm] :=
    SubscriptBox["\[Delta]", HighEnergyPhysics`FeynCalc`Tbox[a,b]];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SD | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SO*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 8 February '98 at 1:17 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SO`",
             "HighEnergyPhysics`FeynCalc`"];

SO::usage=
"SO[q] is the four-dimensional scalar product of OPEDelta with q.
It is transformed into
Pair[Momentum[q], Momentum[OPEDelta] by FeynCalcInternal.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


   SO /:
   MakeBoxes[SO[x_],TraditionalForm] :=
    If[Head[x] =!= Plus,
       TBox["\[CapitalDelta]",  "\[CenterDot]", x],
       TBox["\[CapitalDelta]", "\[CenterDot]", "(",x,")"]
      ];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SO | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SOD*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 8 February '98 at 1:16 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SOD`",
             "HighEnergyPhysics`FeynCalc`"];

SOD::usage= "SOD[q] stands for the D-dimensional scalar product of
OPEDelta with q. SOD[q] is transformed into Pair[Momentum[OPEDelta,D],
Momentum[q,D]] by FeynCalcInternal.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


   SOD /:
   MakeBoxes[SOD[x_],TraditionalForm] :=
    If[Head[x] =!= Plus,
       TBox["\[CapitalDelta]",  "\[CenterDot]",x],
       TBox["\[CapitalDelta]", "\[CenterDot]", "(",x,")"]
      ];


End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SOD | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SP*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SP`",
             "HighEnergyPhysics`FeynCalc`"];

SP::usage= "SP[p,q] is the four-dimensional scalar product of p with q.
SP[p, q] is transformed into ScalarProduct[p,q] by FeynCalcInternal.
SP[p] is the same as SP[p,p] (=p^2).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


SetAttributes[SP, Orderless];
SP[a_] := SP[a,a];


   SP/: MakeBoxes[SP[a_, b_], TraditionalForm] :=
    ToBoxes[ MakeContext["FeynCalcInternal"][SP[a,b]],
            TraditionalForm];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SP | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SPD*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SPD`",
             "HighEnergyPhysics`FeynCalc`"];

SPD::usage= "SPD[p, q] is the D-dimensional scalar product of p with q.
SPD[p, q] is transformed into Pair[Momentum[p, D],Momentum[q, D]]
by FeynCalcInternal.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


SetAttributes[SPD, Orderless];
SPD[a_] := SPD[a,a];

   SPD/: MakeBoxes[SPD[a_, b_], TraditionalForm] :=
    ToBoxes[
           MakeContext["FeynCalcInternal"][SPD[a,b]],
            TraditionalForm] ;


End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SPD | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SUND *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SUND`",
             "HighEnergyPhysics`FeynCalc`"];

SUND::usage=
"SUND[a, b, c] is the symmetric SU(N) d_{a,b,c}.";


(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


SetAttributes[SUND, Orderless];

MakeContext[Explicit, SUNT, SUNTrace, SUNIndex];

Options[SUND] = {Explicit -> False};

(* Added check for integers - noint. F.Orellana, 24/9-2000 *)
noint[x___] :=
    Not[Or @@
        Join[IntegerQ /@ {x}, IntegerQ /@
	({x} /. {SUNIndex -> Identity,
	        HighEnergyPhysics`qcd`ExplicitSUNIndex`ExplicitSUNIndex -> Identity})]];

SUND[a_,a_,b_,___Rule] := 0 /; noint[a];
SUND[a_,b_,c_, opt___Rule] :=
 2 SUNTrace[SUNT[a,b,c]] + 2 SUNTrace[SUNT[b,a,c]] /;
  (Explicit /. {opt} /. Options[SUND]) === True;

SUND /:
MakeBoxes[SUND[a_, b_,c_, opt___Rule], TraditionalForm] :=
    SubscriptBox["d", Tbox[a,b,c]]

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SUND | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SUNDelta *)

(* :Author: Rolf Mertig *)


(* :Summary: Kronecker delta for SU(N) *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SUNDelta`",
               "HighEnergyPhysics`FeynCalc`"];


SUNDelta::usage=
"SUNDelta[a, b] is the Kronecker-delta for SU(N) with color
indices a and b.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


SetAttributes[SUNDelta, Orderless];

   SUNDelta /:
   MakeBoxes[SUNDelta[a_, b_], TraditionalForm ] :=
   SubscriptBox["\[Delta]",
     HighEnergyPhysics`FeynCalc`Tbox[a,b]]

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SUNDeltaContract *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 30 October '98 at 12:11 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Kronecker delta for SU(N) *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SUNDeltaContract`",
             "HighEnergyPhysics`FeynCalc`"];

SUNDeltaContract::usage=
"SUNDeltaContract[expr] substitues for all
SUNDelta in expr SUNDeltaContract, contracts
the SUN(N) indices and resubstitutes SUNDelta.
\n
SUNDeltaContract[i, j] is the Kronecker-delta for SU(N)
with contraction properties. SUNDeltaContract wraps also the
head SUNIndex around its arguments.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


sundelta := sundelta = MakeContext["SUNDelta"];
MakeContext[SUNIndex, ExplicitSUNIndex];
sunn     := sunn     = MakeContext["SUNN"];

SetAttributes[SUNDeltaContract, Orderless];

(* Added check for integers - noint. F.Orellana, 11/1-2001 *)
noint[x___] :=
    Not[Or @@
        Join[IntegerQ /@ {x}, IntegerQ /@
	({x} /. {SUNIndex -> Identity,
	        HighEnergyPhysics`qcd`ExplicitSUNIndex`ExplicitSUNIndex -> Identity})]];

SUNDeltaContract[expr_] := expr //. sundelta ->
  SUNDeltaContract /. SUNDeltaContract -> sundelta;

SUNDeltaContract[x_ /; FreeQ[x, SUNIndex] && !IntegerQ[x] &&
                 FreeQ[x, ExplicitSUNIndex],
                 y_ /; FreeQ[y, SUNIndex] && !IntegerQ[y] &&
                 FreeQ[y, ExplicitSUNIndex]
        ] := SUNDeltaContract[SUNIndex[x], SUNIndex[y]];

SUNDeltaContract[x_SUNIndex, x_SUNIndex
       ] := (sunn^2 - 1) /; noint[x];

SUNDeltaContract /: SUNDeltaContract[
                               j_ExplicitSUNIndex, i_SUNIndex]^2 :=
                    SUNDeltaContract[ExplicitSUNIndex[j],
                                     ExplicitSUNIndex[j]];

SUNDeltaContract /: SUNDeltaContract[i_SUNIndex, j_SUNIndex]^2 :=
                     (sunn^2 - 1) /; (i =!= j) && noint[i,j];

SUNDeltaContract/: SUNDeltaContract[i_SUNIndex, j_] *
                   SUNDeltaContract[a_, i_SUNIndex ] :=
                   SUNDeltaContract[a,j] /; noint[i];

SUNDeltaContract/: SUNDeltaContract[a_, i_SUNIndex ] *
                   SUNDeltaContract[i_SUNIndex, j_] :=
                   SUNDeltaContract[a,j] /; noint[i];

SUNDeltaContract/: SUNDeltaContract[i_SUNIndex, j_] *
                   SUNDeltaContract[i_SUNIndex, k_] :=
                   SUNDeltaContract[j,k] /; noint[i];

SUNDeltaContract/: SUNDeltaContract[a_, i_SUNIndex ] *
                   SUNDeltaContract[b_, i_SUNIndex ] :=
                   SUNDeltaContract[a,b] /; noint[i];

SUNDeltaContract/: SUNDeltaContract[i_SUNIndex, j_SUNIndex ] y_[z__] :=
             ( y[z] /. i -> j ) /; !FreeQ[y[z]//Hold, i] &&
                 FreeQ[y[z], SUNDeltaContract[__]^n_Integer?Negative] /;
                 noint[i,j];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SUNDeltaContract | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SUNF *)

(* :Author: Rolf Mertig *)


(* :Summary: SUNF[a, b, c] is the structure constant of SU(N) *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SUNF`",
               "HighEnergyPhysics`FeynCalc`"];

SUNF::usage=
"SUNF[a, b, c] are the structure constants of SU(N).
The arguments a,b,c should be of symbolic type."

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];



fci  := fci = MakeContext["FeynCalcInternal"];

sunindex := sunindex = MakeContext["SUNIndex"];
FreeQ2 := FreeQ2 = MakeContext["FreeQ2"];
Explicit := Explicit = MakeContext["Explicit"];
sunt     := sunt     = MakeContext["SUNT"];
suntrace := suntrace = MakeContext["SUNTrace"];

Options[SUNF] = {Explicit -> False(*, fci -> True*)};

(* IS THIS NECESSARY ?????
SUNF[a___, x_, b___, opt___Rule] := SUNF[a, sunindex[x], b] /;
 FreeQ2[x, {sunindex, Rule, Pattern, BlankSequence}] && FreeQ[{b},Rule] &&
  (FeynCalcInternal /. {opt} /. Options[SUNF])  === True;
*)

(* antisymmetry *)
HoldPattern[SUNF[a___, x_, b___, x_, c___]] := 0 /;
         (Head[x] === sunindex) && FreeQ[x, Pattern];
HoldPattern[SUNF[a___, x_, y_, b___]] := -SUNF[a, y, x, b] /;
FreeQ[{a,x,y,b}, Pattern] &&
(!OrderedQ[{x, y}]) && (Head[x] === sunindex) && Head[y] === sunindex;

SUNF[i_,j_,k_,Explicit -> False] := SUNF[i,j,k];
HoldPattern[SUNF[i_,j_,k_,op___]]:= 2 I (suntrace[ fci[sunt[i,k,j]] ] -
                                      suntrace[ fci[sunt[i,j,k] ] ]
                                     )/;
     (Explicit/.Flatten[Join[{op},Options[SUNF]]]) === True;

   tbox[a__] := RowBox @ Map[(MakeBoxes @@ {#, TraditionalForm})&, {a}];

totr[Subscript[y_,in__Integer]] := SubscriptBox[totr[y],RowBox[{in}]];

totr[y_Symbol] := If[FormatValues[Evaluate[y]] === {},
                     ToString[y],
                     ToBoxes[y, TraditionalForm], y];
totr[y_String] := y;
totr[y_] := ToBoxes[y, TraditionalForm] /; Head[y]=!=Symbol;

Tbox[a__] :=
(RowBox @ (Insert[
  Map[totr, {a}], "\[NoBreak]",
    Array[{#}&,Length[{a}]-1,2]]));

   SUNF /:
   MakeBoxes[
             SUNF[a_,b_,c_], TraditionalForm
            ] := SubscriptBox@@{"f", Tbox[a,b,c]};

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SUNF2 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: SUNF2[a, b, c] is the structure constant of SU(N) *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SUNF2`",
             "HighEnergyPhysics`FeynCalc`"];

SUNF2::usage=
"SUNF2[i, j, k] are the structure constants of SU(N).
SUNF2 has options and its arguments are changed to SUNIndex if
necessary.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


fci     := fci       = MakeContext["FeynCalcInternal"];
sunindex := sunindex = MakeContext["SUNIndex"];
freeq2  := freeq2    = MakeContext["FreeQ2"];
explicit:= explicit  = MakeContext["Explicit"];
sunt     := sunt     = MakeContext["SUNT"];
suntrace := suntrace = MakeContext["SUNTrace"];

Options[SUNF2] = {Explicit -> False};

(*
SUNF2[a___, x_, b___] := SUNF2[a, sunindex[x], b] /;
freeq2[x, {sunindex, Rule, Pattern, BlankSequence}];
*)
(* antisymmetry *)
HoldPattern[SUNF2[a___, x_, b___, x_, c___]] := 0 /;
         (Head[x] === sunindex) && FreeQ[x, Pattern];
HoldPattern[SUNF2[a___, x_, y_, b___]] := -SUNF2[a, y, x, b] /;
FreeQ[{a,x,y,b}, Pattern] &&
(!OrderedQ[{x, y}]) && (Head[x] === sunindex) && Head[y] === sunindex;

SUNF2[i_,j_,k_,Explicit -> False] := SUNF2[i,j,k];
HoldPattern[SUNF2[i_,j_,k_,op_:{}]]:= 2 I (suntrace[ fci[sunt[i,k,j]] ] -
                                      suntrace[ fci[sunt[i,j,k] ] ]
                                     )/;
     (Explicit/.Flatten[Join[{op},Options[SUNF2]]]);

   tbox[a__] := RowBox @ Map[(MakeBoxes @@ {#, TraditionalForm})&, {a}];
   SUNF2/:
   MakeBoxes[
             SUNF2[a_,b_,c_], TraditionalForm
            ] := SuperscriptBox@@{"f", tbox[a,b,c]};

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SUNF2 | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SUNFJacobi*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SUNFJacobi`",
             "HighEnergyPhysics`FeynCalc`"];


SUNFJacobi::usage="SUNFJacobi is an option for SUNSimplify, indicating
whether the Jacobi identity should be used.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SUNFJacobi | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SUNFToTraces *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Option of SUNf and SUNSimplify *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SUNFToTraces`",
             "HighEnergyPhysics`FeynCalc`"];

SUNFToTraces::usage=
"SUNFToTraces is superseded by Explicit.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SUNFToTraces | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SUNIndex *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 24 March '98 at 15:52 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Head for SUN-Indices *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SUNIndex`",
             "HighEnergyPhysics`FeynCalc`"];

SUNIndex::usage=
"SUNIndex[a] is an SU(N) index. If the argument is an integer
SUNIndex[a] turns into ExplicitSUNIndex[a].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


ExplicitSUNIndex = MakeContext["ExplicitSUNIndex"];

SetAttributes[SUNIndex, {Constant, Flat, OneIdentity}];

SUNIndex[i_Integer]:= ExplicitSUNIndex[i];

   SUNIndex /:
   MakeBoxes[ SUNIndex[p_], TraditionalForm
            ] := ToBoxes[p, TraditionalForm];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SUNIndex | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SUNIndexRename *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 19 January '99 at 20:31 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Option of SUNSimplify *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SUNIndexRename`",
             "HighEnergyPhysics`FeynCalc`"];

SUNIndexRename::usage= "SUNIndexRename is an option of SUNSimplify. If set to
False, no automatic renaming of dummy indices is done.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SUNIndexRename | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SUNN *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: SUNN = the N of SU(N) *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SUNN`",
             "HighEnergyPhysics`FeynCalc`"];

SUNN::usage =
"SUNN denotes the number of colors.
Trick[SUNDelta[a, a]] yields (SUNN^2 -1).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

(* add maybe later something to convert SUNN^2 -> CA, CF *)

HighEnergyPhysics`FeynCalc`SUNN`SUNN /:
   MakeBoxes[ HighEnergyPhysics`FeynCalc`SUNN`SUNN,
              TraditionalForm
            ] := "N";

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SUNN | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SUNNToCACF *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Option of SUNf and SUNSimplify *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SUNNToCACF`",
             "HighEnergyPhysics`FeynCalc`"];

SUNNToCACF::usage= "SUNNToCACF is an option of SUNSimplify. If set to
True, the Casimir operators CA (=N) and CF (=(N^2-1)/(2 N))
are introduced.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SUNNToCACF | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SUNT *)

(* :Author: Rolf Mertig *)


(* :Summary: *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SUNT`",
               "HighEnergyPhysics`FeynCalc`"];


SUNT::usage=
"SUNT[a] is the SU(N) T_a generator in
the fundamental representation."

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];



fci := fci = MakeContext["FeynCalcInternal"];
MakeContext["DeclareNonCommutative"][SUNT];

SUNT /:
  MakeBoxes[ SUNT[a_], TraditionalForm] :=
    SubscriptBox["T", ToBoxes[a, TraditionalForm]];

SUNT /:
  MakeBoxes[
            SUNT[a_,b__], TraditionalForm
           ] := RowBox[ Map[SubscriptBox["T",
               ToBoxes[#, TraditionalForm]]&, {a, b}] ];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SUNTrace *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 21 March '98 at 9:07 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: calculation of traces of SUNT-matrices (color factors)*)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SUNTrace`",
             "HighEnergyPhysics`FeynCalc`"];

SUNTrace::usage=
"SUNTrace[expr] calculates the color-trace.";
(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


complexindex := complexindex = MakeContext["ComplexIndex"];
dot := dot                   = MakeContext["DOT"];
fci := fci =  MakeContext["FeynCalcInternal"];

MakeContext[ DotSimplify, Explicit, FreeQ2, SUNF, SUND,
SUNDelta, SUNIndex, SUNT];

sunn := sunn = MakeContext["SUNN"];
SUNN := SUNN = MakeContext["SUNN"];

Options[SUNTrace] = {Explicit -> False};

fcis[z_ /; FreeQ[z, Pattern]] := (fcis[z] = DotSimplify[fci[z]]);
(* change SUNT' which are multiplied with each other to lambdaT's *)
lambdaT[1]=1;
gm2lambdaT[x__]:= (gmlin@@( {x}/.SUNT->lambdaT ) )/.gmlin->Dot;
(********************* linearity  ********************************* *)
(* noncomQdef : checking non-commutativity *)
noncomQ[z_]        := TrueQ[noncQ[z]];
noncQ[x_ ?NumberQ] := True;
noncQ[x_SUNTrace]  := True;
noncQ[x_] := If[FreeQ2[FixedPoint[ReleaseHold, x], $NonComm],
    True, False];

gmlin/: HoldPattern[gmlin[gmlin[x__]]] := gmlin[x];
gmlin[ a___, b_ c_, d___ ] := b gmlin[a,c,d]/;FreeQ[b, lambdaT] &&
                                              noncomQ[b];
gmlin[ a___, b_ , d___ ]   := b gmlin[a,d]/;FreeQ[b, lambdaT] &&
                                                noncomQ[b];
gmlin[]=1;
gellm1[x_, y__] := gellm1[x. y];
gellm2[x_, y__] := gellm2[x. y];
(******************* cyclicity *********************************** *)
gmcyc[x__] := gellm1 @@
              First[NestList[RotateLeft, {x}, Length[{x}]-1]//Sort];
(************* define the properties of trace of T-matrices *)
gellm2[ ] = gmcyc[ ] = sunn;         (* unit trace  *)
(************** each single T-matrix has vanishing trace *)
gellm2[ lambdaT[_] ] := 0;
(************** Cvitanovic - rules ******************************* *)
gellm2[HoldPattern[Dot[a___, lambdaT[i_], b___, lambdaT[i_], c___]]]:=
       (1/2 gmcyc[b] gmcyc[a, c] - 1/2/sunn gmcyc[a, b, c]
       ) /; (Head[i] === SUNIndex) && !IntegerQ[i];

gellm2/: gellm2[HoldPattern[Dot[a___, lambdaT[i_], b___]]]^2 :=
         (1/2 gmcyc[a, b, a, b] - 1/2/sunn gmcyc[a, b]^2
         ) /; (Head[i] === SUNIndex) && !IntegerQ[i];

gellm2/: gellm2[HoldPattern[Dot[a___, lambdaT[i_], b___]]] *
         gellm2[HoldPattern[Dot[c___, lambdaT[i_], d___]]]:=
         (1/2 gmcyc[a, d, c, b] - 1/2/sunn gmcyc[a, b] gmcyc[c, d]
         ) /; (Head[i] === SUNIndex) && !IntegerQ[i];

f2tr[i_,j_,k_,___]:=
  2 I (gmcyc @@ lambdaT/@{i,k,j} - gmcyc @@ lambdaT/@{i,j,k});
(* do the application of the Cvitanovic - relations step by step *)
cvit[x_Plus] := cvit/@x;
cvit[x_]:= (cvit[x]=ExpandAll[ x /. gellm1 -> gellm2 ]);

(* this is the function which puts everything together ********* *)
SUNTrace[expr_Plus, op___Rule] := Map[SUNTrace[#, op]&, expr];

HoldPattern[SUNTrace[n_, ___Rule]] :=
  sunn n /; FreeQ[n, SUNT] && FreeQ[n, Pattern];

(*
HoldPattern[SUNTrace[y_Times,___Rule]] := (Select[y,FreeQ[#,SUNT]&] *
                      SUNTrace[Select[y,!FreeQ[#,SUNT]&]]
                     ) /; (Select[y, FreeQ[#,SUNT]]=!=1) &&
                          (fcis[y] === y);
*)

(*new; suggested by Frederik Orellana *)
SUNTrace[expr_Times,op___Rule] :=
 (Select[expr, FreeQ2[#, {SUNT,SUNF,SUND,SUNDelta}]&] *
  SUNTrace[Select[expr, !FreeQ2[#, {SUNT,SUNF,SUND,SUNDelta}]&], op]
 ) /; (expr === fcis[expr]) &&
   (Select[expr, !FreeQ2[#, {SUNT,SUNF,SUND,SUNDelta}]&] =!= 1);


HoldPattern[SUNTrace[a_, ___Rule]] :=
  (SUNTrace[Dot @@ Reverse[a/.complexindex->Identity]
          ] /. Dot -> dot) /;
    MatchQ[a, Apply[HoldPattern,
                     {dddot[SUNT[SUNIndex[complexindex[_]]]..]}
                   ] /. dddot -> Dot] && (fcis[a] === a);

HoldPattern[SUNTrace[1 .., ___Rule]]= sunn;
HoldPattern[SUNTrace[a_, ___Rule]] := 0 /; MatchQ[a, SUNT[SUNIndex[_]]] &&
                                (fcis[a] === a);

HoldPattern[SUNTrace[a_, o___Rule]] := SUNTrace[fcis[a],o] /; (fcis[a]=!=a);

SUNTrace[SUNT[x_SUNIndex] . SUNT[y_SUNIndex], ___Rule] :=
 SUNDelta[x, y]/2;
SUNTrace[SUNT[a_SUNIndex] . SUNT[b_SUNIndex] . SUNT[c_SUNIndex],
 opt___Rule] :=
  (SUND[a, b, c]/4 + I SUNF[a,b,c]/4) /; Length[Union[{a,b,c}]]===3 &&
   (Explicit /. {opt} /. Options[SUNTrace]) === True  ;

(* recursion suggested by Peter Cho *)
SUNTrace[SUNT[a_] . SUNT[b_] . SUNT[c_] . SUNT[d_] . (more__SUNT),
         opt___Rule] := Block[{f},
 f = Unique["c"];
 SUNDelta[a,b]/2/SUNN SUNTrace[SUNT[c].SUNT[d].more,opt] +
  1/2 SUND[a,b,f] SUNTrace[SUNT[f].SUNT[c].SUNT[d].more,opt] +
   I/2 SUNF[a,b,f] SUNTrace[SUNT[f].SUNT[c].SUNT[d].more,opt]
                             ] /;
     ( Union[Head /@ {a,b,c,d}] === {SUNIndex}) &&
     ( (Explicit /. {opt} /. Options[SUNTrace]) === True );

SUNTrace[SUNT[a_] . SUNT[b_] . SUNT[c_] . SUNT[d_], opt___Rule] :=
Block[{e},
(
 If[ValueQ[Global`e] || !FreeQ[{a,b,c,d}, Global`e], e = Unique["c"],
    e = Global`e
   ];
 Expand[1/4/SUNN (SUNDelta[a, b] SUNDelta[c, d] -
                  SUNDelta[a, c] SUNDelta[b, d] +
                  SUNDelta[a, d] SUNDelta[b, c]
                 ) +
        1/8( SUND[a,b,e] SUND[c,d,e] -
             SUND[a,c,e] SUND[b,d,e] +
             SUND[a,d,e] SUND[b,c,e]
           ) +
        I/8 (SUND[a,d,e] SUNF[b,c,e] -
             SUNF[a,d,e] SUND[b,c,e]
            )
       ]
)    ] /; (Union[Head /@ {a,b,c,d}] === {SUNIndex}) &&
          (Explicit /. {opt} /. Options[SUNTrace]) === True  ;

(*
HoldPattern[SUNTrace[ x_, y___, z_, o___Rule ] ]:=
 SUNTrace[x.y.z,o] /; Head[z] =!= Rule && FreeQ[z, Pattern];
*)

HoldPattern[SUNTrace[ SUNTrace[x__] y_., op___Rule ]]  :=
 SUNTrace[x] SUNTrace[y, op];
SUNTrace/:  SUNTrace[dot[(A___), SUNT[x_SUNIndex], B___],___Rule] *
            SUNTrace[dot[(a___), SUNT[x_SUNIndex], b___],___Rule] :=
             FixedPoint[cvit,  (gmcyc[A.SUNT[x].B] *
                  gmcyc[a.SUNT[x].b])/.
 		 SUNTrace->gellm1/.
 		 Dot->gm2lambdaT/.gellm1->gellm2/.
                               SUNF->f2tr
 	        ]/.lambdaT->SUNT/.gellm2->SUNTrace /. Dot -> dot;

SUNTrace /: HoldPattern[SUNTrace[x_,o___Rule]^2] :=
 SUNTrace[x,o] * SUNTrace[x,o];

SUNTrace[ a_, ___Rule ]:= fixgell[a /. dot -> Dot]/;
                    NumberQ[fixgell[a /. dot -> Dot]] &&
                         FreeQ[a, Pattern] && (fcis[a] === a);

(*
(*XXX*)
SUNTrace[expr_Times,op___Rule] :=
 (Select[expr, FreeQ[#, SUNIndex]&] *
  SUNTrace[Select[expr, !FreeQ[#, SUNIndex]&], op]
 ) /; expr === fcis[expr] && (Select[expr, FreeQ[#, SUNIndex]&] =!= 1);
*)

(*
HoldPattern[SUNTrace[(a_.) ((b__) . (d_- e_))]] :=
 SUNTrace[a (b . d )] - SUNTrace[a (b . e )];
*)

SUNTrace[ expr_, ___Rule ]:= (fixgell[expr /. dot -> Dot (*/.SUNT[1]->1*) ]/.
                            gellm2->SUNTrace /. Dot -> dot)/;
                    ((Head[expr] =!= Times) ||
                     (Select[expr, !FreeQ[#, SUNT]&] ===1 )
                    ) &&
 	            (expr=!=(fixgell[expr/. dot -> Dot] /.
                             gellm2->Identity/.Dot -> dot))&&
                            FreeQ[expr, Pattern] &&
                             (fcis[expr]===expr);
gellm1[x_Plus]:=gellm1 /@ x;
gellm1/: gellm1[x_ y_] := x gellm1[y]/;FreeQ[x,lambdaT];
gellm1/: gellm1[x_Dot gellm1[y___]]:=gellm1[y] gellm1[x];
gellex[z_]:=gellm1[ExpandAll[z]];
fixgell[x_]:=(fixgell[x]=
FixedPoint[cvit, ( gellm1[ExpandAll[x/.SUNTrace->gellex/.
                   Dot->gm2lambdaT/.SUNF->f2tr]]
                 )/.gellm1->gellm2, 19
          ]/.lambdaT->SUNT);

externQ[xx_] := If[!FreeQ[xx, Pattern], False,
                   If[fcis[xx] === xx, False, True]
                  ];
SUNTrace[x_?externQ] := SUNTrace[fcis[x]];

   SUNTrace /:
    MakeBoxes[SUNTrace[a_,___Rule], TraditionalForm] :=
     Tbox["tr","(",a,")"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SUNTrace | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ScalarProduct *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 20 December '98 at 23:59 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: for scalar products *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`ScalarProduct`",
             "HighEnergyPhysics`FeynCalc`"];

ScalarProduct::usage =
"ScalarProduct[p, q] is the input for scalar product.
ScalarProduct[p] is equivalent to ScalarProduct[p, p].
Expansion of sums of momenta in ScalarProduct is done with
ExpandScalarProduct. Scalar product may be set, e.g.,
ScalarProduct[a, b] = m^2; but a and b must not contain sums.
Note that ScalarProduct[a, b] = m^2 actually sets also:
Pair[Momentum[a, ___], Momentum[b, ___]] = m^2.
It is enouraged to always set ScalarProduct's BEFORE any
calculation. This improves the performance of FeynCalc .";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[ ChangeDimension, Dimension];
fci := fci = MakeContext["FeynCalcInternal"];
nf  := nf = MakeContext["NumericalFactor"];

MakeContext[ Momentum, Pair, Select1];


Options[ScalarProduct] = {Dimension->4, fci -> True};

ScalarProduct[a_, b_, c___] := ScalarProduct[b, a, c] /;
                               !OrderedQ[{a, b}];

ScalarProduct[x_, y___Rule] := ScalarProduct[x, x, y];

ScalarProduct[a_,b_, c___Rule] :=
 Pair[Momentum[a, Dimension /. {c} /. Options[ScalarProduct]],
      Momentum[b, Dimension /. {c} /. Options[ScalarProduct]]
     ] /; FreeQ[{a,b}, Momentum] &&
          ((fci /. {c} /. Options[ScalarProduct]) === True);

ScalarProduct/:Set[ScalarProduct[a_,b_,c___],z_]:= Block[
{ste, rst, downv, scal},
If[FreeQ[a, Pattern], ste = fci[ScalarProduct[a, b, c]];
   If[ste === 0 , rst = 0,
      ste = ChangeDimension[ste, ___Symbol];
      If[(Head[a] === Pattern) && (a === b),
         (SetDelayed @@ {ste, ScalarProduct[a[[1]], a[[1]]]})
         ,
         Set@@{ste/nf[ste], z / nf[ste]}
        ];
 If[(nf[a] === 1) && (nf[b] === 1), rst = z,
    If[(a =!= 0) && (b =!= 0),
        rst = z/nf[a]/nf[b]
   ]  ]
  ];
 ];
(* might be a setting before *)
   If[z =!= ste,
      downv = DownValues[ScalarProduct];
      downv = Select1[downv,
                     RuleDelayed@@{HoldPattern@@{scal[a,b,c]}, ste} /.
                     scal -> ScalarProduct];
      DownValues[ScalarProduct] = downv;
If[FreeQ[a,Pattern],
(*
   rst = Set[ScalarProduct[a,b,c], z],
*)
   rst = z,
   rst = z
   ] ,
      rst = ste
     ];
nd = RuleDelayed @@ {HoldPattern @@ {ScalarProduct[a, b, c]}, rst};
If[!MemberQ[DownValues[ScalarProduct], nd],
   AppendTo[DownValues[ScalarProduct], nd] ];
rst];


     ScalarProduct /:
     MakeBoxes[ScalarProduct[a_, a_, ___Rule],
                  TraditionalForm
                 ] := SuperscriptBox@@{MakeBoxes@@{a, TraditionalForm
                                                  }, 2
                                      };
     ScalarProduct /:
     MakeBoxes[ScalarProduct[a_Plus, b_], TraditionalForm] :=
     RowBox[{"(",MakeBoxes[a, TraditionalForm],")", "\[CenterDot]",
             MakeBoxes[b, TraditionalForm]}
           ]/;Head[b]=!=Plus;

     ScalarProduct /:
     MakeBoxes[ScalarProduct[a_, b_Plus], TraditionalForm] :=
     RowBox[{MakeBoxes[a, TraditionalForm], "\[CenterDot]","(",
             MakeBoxes[b, TraditionalForm], ")"}
           ]/;Head[b]=!=Plus;

     ScalarProduct /:
     MakeBoxes[ScalarProduct[a_Plus, b_Plus], TraditionalForm] :=
     RowBox[{MakeBoxes[a, TraditionalForm], "\[CenterDot]","(",
             MakeBoxes[b, TraditionalForm], ")"}
           ];

     ScalarProduct /:
     MakeBoxes[ScalarProduct[a_, b_], TraditionalForm] :=
     RowBox[{MakeBoxes[a, TraditionalForm], "\[CenterDot]",
             MakeBoxes[b, TraditionalForm]}
           ]

initialDownValues = DownValues[ScalarProduct];
initialUpValues = UpValues[ScalarProduct];

(* tentative *)

(*
Unprotect[ReplaceAll];
ReplaceAll[y_, ScalarProduct[a_, b_] -> z_] :=
   (y /. Pair[Momentum[a, ___Symbol], Momentum[b, ___Symbol]] -> z
   ) /; FreeQ[y, ScalarProduct[a,b]];
Protect[ReplaceAll];
*)

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ScalarProduct | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ScalarProductExpand *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: ScalarProductExpand expands scalar products *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`ScalarProductExpand`",
             "HighEnergyPhysics`FeynCalc`"];

ScalarProductExpand::usage =
"ScalarProductExpand[expr]  expands scalar products of sums of
momenta in expr.
ScalarProductExpand[a, b] expands ScalarProduct[a, b].
ScalarProductExpand is equivalent to ExpandScalarProduct.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


ScalarProductExpand = MakeContext["ExpandScalarProduct"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ScalarProductExpand | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SmallDelta *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: SmallDelta is some small delta *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SmallDelta`",
             "HighEnergyPhysics`FeynCalc`"];

SmallDelta::usage = "SmallDelta denotes some small positive number.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


   SmallDelta /:
   MakeBoxes[SmallDelta, TraditionalForm] := "\[Delta]"

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SmallDelta | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SmallEpsilon *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: SmallEpsilon is some small epsilon *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SmallEpsilon`",
             "HighEnergyPhysics`FeynCalc`"];

SmallEpsilon::usage = "SmallEpsilon denotes some small
positive number.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


(*
SmallEpsilon /: SmallEpsilon _ = 0;
SmallEpsilon /: SmallEpsilon^_Integer?Positive = 0;
*)

SmallEpsilon /:
MakeBoxes[SmallEpsilon, TraditionalForm] := \[Epsilon];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SmallEpsilon | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SmallVariable *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: SmallVariable is beautiful *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SmallVariable`",
             "HighEnergyPhysics`FeynCalc`"];

SmallVariable::usage =
"SmallVariable[me] is the head of small (negligible) variables.
This means any mass with this head can be neglected if it
appears in a sum, but not as an argument of Passarino-Veltman
(PaVe) functions or PropagatorDenominator.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


SmallVariable[0] = 0;
SmallVariable[x_^pow_] := SmallVariable[x]^pow;

   MakeBoxes[SmallVariable[a_], TraditionalForm] :=
    MakeBoxes[a, TraditionalForm];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SmallVariable | \n "]];
Null
(* :Title: Spinor *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Spinor denotes spinors *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Spinor`",
             "HighEnergyPhysics`FeynCalc`"];

Spinor::usage = "Spinor[p, m, optarg] is the head of Dirac spinors.
Which of the spinors u, v,u_bar or v_bar
is understood, depends on the sign of the momentum (p)
argument and the relative position of DiracSlash[p]:
Spinor[sign p, mass]  is that spinor which yields
sign*mass*Spinor[p, mass] if the Dirac equation is applied .";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[DeclareNonCommutative];
DeclareNonCommutative[Spinor];

momentum := momentum = MakeContext["Momentum"];
MomentumExpand := MomentumExpand = MakeContext["MomentumExpand"];
freeq2   := freeq2 = MakeContext["FreeQ2"];
small    := small  = MakeContext["SmallVariable"];

HoldPattern[Spinor[a__,{1}]] := Spinor[a];

frp[y___] := freeq2[{y}, {Pattern, Blank,BlankSequence,
                          BlankNullSequence,HoldForm}];

Spinor[n_. x_/; (frp[x]&&FreeQ[x, momentum]), y___/;frp[y]] :=
 (Spinor[n x, y] = Spinor[n momentum[x], y]) /;
    (frp[{n, x, y}] && (n^2)===1);

(* this is convention ... *)
Spinor[momentum[x_, di_], m_, op___] := Spinor[momentum[x], m, op];
Spinor[-momentum[x_, di_], m_, op___] := Spinor[-momentum[x], m, op];
Spinor[kk_.+ n_. momentum[ a_Plus ], m_, y___]:=
       Spinor[kk+ n momentum[a], m, y] = (
              Spinor[MomentumExpand[kk + n momentum[a]] ,m,y] );
Spinor[p_ , _. small[_], in___] := Spinor[p, 0, in] /; frp[p];
Spinor[p_ ]                     := Spinor[p,0,1] /; frp[p];
Spinor[p_, m_ /; FreeQ[m, Pattern]] := Spinor[p, m, 1] /; frp[p];

   Spinor /:
    MakeBoxes[Spinor[p_,0,___], TraditionalForm] :=
     Tbox["\[CurlyPhi]","(",p,")"];
   Spinor /:
    MakeBoxes[Spinor[p_,m_ /; m=!=0,___], TraditionalForm] :=
     Tbox["\[CurlyPhi]","(",p, ",", m, ")"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Spinor | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SpinorU *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SpinorU`",
             "HighEnergyPhysics`FeynCalc`"];

SpinorU::usage = "SpinorU[p, m, optarg] denotes a u-spinor.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[DeclareNonCommutative];
DeclareNonCommutative[SpinorU];

   SpinorU /:
    MakeBoxes[SpinorU[p_], TraditionalForm] := Tbox["u","(",p,")"];
   SpinorU /:
    MakeBoxes[SpinorU[p_,m_,___], TraditionalForm] :=
    Tbox["u","(",p,",",m,")"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SpinorU | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SpinorUBar *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SpinorUBar`",
             "HighEnergyPhysics`FeynCalc`"];

SpinorUBar::usage = "SpinorUBar[p, m, optarg] denotes a ubar-spinor.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[DeclareNonCommutative];
DeclareNonCommutative[SpinorUBar];

   SpinorUBar /:
   MakeBoxes[SpinorUBar[p_], TraditionalForm] :=
   Tbox[OverBar["u"],"(",p,")"];
  SpinorUBar /:
    MakeBoxes[SpinorUBar[p_,m_,___], TraditionalForm] :=
    Tbox[OverBar["u"],"(",p,",",m,")"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SpinorUBar | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SpinorV *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SpinorV`",
             "HighEnergyPhysics`FeynCalc`"];

SpinorV::usage = "SpinorV[p, m, optarg] denotes a v-spinor.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

DeclareNonCommutative = MakeContext["DeclareNonCommutative"];
DeclareNonCommutative[SpinorV];

   SpinorV /:
   MakeBoxes[SpinorV[p__], TraditionalForm] := Tbox["v","(",p,")"];
   SpinorV /:
    MakeBoxes[SpinorV[p_,m_,___], TraditionalForm] :=
    Tbox["v","(",p,",",m,")"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SpinorV | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SpinorVBar *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SpinorVBar`",
             "HighEnergyPhysics`FeynCalc`"];

SpinorVBar::usage = "SpinorVBar[p, m, optarg] denotes a vbar-spinor.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

DeclareNonCommutative = MakeContext["DeclareNonCommutative"];
DeclareNonCommutative[SpinorVBar];

   SpinorVBar /:
   MakeBoxes[SpinorVBar[p__], TraditionalForm] :=
   Tbox[OverBar["v"],"(",p,")"];
 SpinorVBar /:
    MakeBoxes[SpinorVBar[p_,m_,___], TraditionalForm] :=
    Tbox[OverBar["v"],"(",p,",",m,")"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SpinorVBar | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Tr *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 8 December '98 at 14:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Dirac trace calculation  (see also DiracTrace) *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Tr`",
             "HighEnergyPhysics`FeynCalc`"];

Tr::usage=
"Tr[exp] calculates the Dirac trace of exp.
Depending on the setting of the option SUNTrace also
a trace over SU(N) objects is performed.
Tr is identical to
DiracTrace, up to the default setting of DiracTraceEvaluate.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


fci                 = MakeContext["FeynCalcInternal"];

MakeContext[ DiracTrace, DiracTraceEvaluate, DOT,
Factoring, FeynCalcExternal, LeviCivitaSign, Mandelstam,
PairCollect, Schouten, Explicit, SUNIndex, SUNSimplify,
SUNT, SUNTrace, TraceOfOne, Trick];

Options[ Tr ] = { DiracTraceEvaluate -> True,
                  Factoring          -> False,
                  FeynCalcExternal   -> False,
                  LeviCivitaSign     -> (-1),
                  Mandelstam         -> {},
                  PairCollect        -> False,
                  Schouten           -> 442,
                  SUNTrace           -> True,
                  TraceOfOne         -> 4
                };

Tr[x_, rul___Rule] := Block[{tt, doot, diractr, dit, fcex},
                             tt = fci[x];
                          If[(SUNTrace /. {rul} /. Options[Tr])=== True &&
                             (!FreeQ[tt, SUNIndex]),
                             tt = SUNSimplify[DiracTrace[tt, rul],
                                              SUNTrace -> True,
                                              Explicit -> False
                                             ],
                             If[FreeQ[tt, SUNIndex],
                                tt = DiracTrace[tt],
                                tt = DiracTrace[Trick[tt]//SUNSimplify]
                               ]
                            ];

                          If[!FreeQ[tt, SUNIndex],
                             tt = tt /. DiracTrace-> dit /.DOT -> doot;
                             tt = tt /. {doot[a__SUNT, b__] :>
                                         (doot[a] doot[b]) /;
                                         FreeQ[{b}, SUNIndex]
                                        } /. doot -> DOT /.
                                         dit -> DiracTrace;
                            ];
                          diractr[y_] := (DiracTrace @@
                            Join[{y}, {rul}, Options[Tr]]);

                          tt = tt /. DiracTrace -> diractr;
                          If[FeynCalcExternal /. {rul} /. Options[Tr],
                             tt = FeynCalcExternal[tt]
                            ];
                           tt];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Tr | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Tr2 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Dirac trace calculation  (see also DiracTr2ace) *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Tr2`",
             "HighEnergyPhysics`FeynCalc`"];

Tr2::usage=
"Tr2[exp] simplifies exp and calculates the Dirac traces unless
more that 4 gamma matrices and DiracGamma[5] occur.
Tr2[exp] also separates the color-strucure,
takeing the color trac (or not, depending
whether Tf occurs in exp).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[
Collect2,
Contract,
DiracGamma,
DiracGammaExpand,
DiracOrder,
DiracSimplify,
DiracTrace,
DiracTraceEvaluate,
DOT,
EpsContract,
ExpandScalarProduct,
Factoring,
FeynCalcForm];

FCI := FCI = MakeContext["FeynCalcInternal"];

MakeContext[ FreeQ2, InsideDiracTrace,
LorentzIndex, NonCommQ, Select1, Select2, Explicit,
SUNIndex, SUNSimplify, SUNNToCACF, SUNTrace, Tf, Tr, Trick];

Options[ Tr2 ] = {Factoring -> False};


dirtr[x_] := If[FreeQ[x, SUNIndex], DiracTrace[x],
             If[!FreeQ[x, Tf],
                SUNSimplify[DiracTrace[x], SUNTrace->True,
                                           Explicit -> False],
                SUNSimplify[DiracTrace[x], SUNTrace->False,
                                           Explicit -> False]
               ]];

treasy[0] = 0;
treasy[y_Plus] := Map[treasy, y];
treasy[a_] := Tr[a] /; NonCommQ[a];

treasy[fa_. DiracGamma[5]] := 0 /; FreeQ[fa, DiracGamma];
treasy[fa_. DOT[x_,y__]] :=
 If[FreeQ[fa, DOT] &&
    (FreeQ2[{x,y}, {DiracGamma[5], DiracGamma[6],  DiracGamma[7]} ] ||
     Length[{x,y}] < 6
    ), Tr[fa DOT[x,y]],
    DiracTrace[fa DOT[x,y]]
   ];

trup /: (f_/;FreeQ2[f, {DiracTrace,DOT}]) trup[x_,ops___Rule] :=
  DiracTrace[f x, ops];
trap[y_,ops___Rule] := If[Head[y] =!= Times,
               DiracTrace[y,ops],
               Select1[y, {DiracGamma, LorentzIndex}] *
               DiracTrace[y/Select1[y, {DiracGamma, LorentzIndex}],ops]
              ];

trdifficult[y_, ops___Rule] :=
 If[MatchQ[y, _. DOT[(a__) /; FreeQ2[{a}, {DiracGamma[5],DiracGamma[6],
                                           DiracGamma[7]}], DiracGamma[5]]
          ],
    treasy[Expand[ExpandScalarProduct[
           Contract[DiracOrder[
            Collect2[
             DiracSimplify[y, InsideDiracTrace -> True],
                     DOT, Factoring -> False]],
                               EpsEvaluate->False]],
                 DiracGamma],ops
          ],
    treasy[Expand[ExpandScalarProduct[
           Contract[DiracOrder[y],EpsEvaluate->False]],
              DiracGamma], ops
          ]
   ] /. treasy -> DiracTrace /. DiracTrace -> trup /.
        trup -> DiracTrace /. DiracTrace -> trap;

Tr2[x_] := Block[{tt},
tt = FCI[x];
If[FreeQ[tt, DiracTrace], tt = DiracTrace[tt]];
tt = Trick[tt];
If[!FreeQ[tt, SUNIndex],
   tt = SUNSimplify[tt /. DiracTrace -> dirtr, SUNNToCACF -> True]
  ];
tt = tt /. DiracTrace -> treasy /. treasy -> DiracTrace;
tt = tt /. DiracTrace -> trup /.trup -> DiracTrace /. DiracTrace -> trap;
tt = tt /. DiracTrace -> trdifficult;
tt];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Tr2 | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)


(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: TraceOfOne *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: TraceOfOne *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`TraceOfOne`",
             "HighEnergyPhysics`FeynCalc`"];

TraceOfOne::usage =
"TraceOfOne is an option for Tr and DiracTrace.
Its setting determines the value of the unit trace.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "TraceOfOne | \n "]];
Null

(* :Title: Trick *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 28 January '98 at 15:27 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Trick does non-commutative expansion and simple contractions *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Trick`",
             "HighEnergyPhysics`FeynCalc`"];

Trick::usage =
"Trick[exp] uses Contract, DotSimplify and SUNDeltaContract.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[
DOT, DotSimplify, EpsContract,
Expanding, FeynAmpDenominator, FeynAmpDenominatorCombine,
FeynCalcInternal, LorentzIndex, SUNDelta, SUNF,
SUNIndex, SUNDeltaContract, SUNSimplify, SUNT,
CovariantD,
CrossProduct,
DiracGamma,
DotProduct,
Expanding,
Explicit,
PauliSigma,
Contract];

Trick[x_] := Block[{tt, paulisigsimp, sigident,doot,cov,palr},
             SetAttributes[cov,HoldFirst];
             cov[y_] := If[CheckContext["CovariantD"],
                           y /. CovariantD[a__] :>
                           CovariantD[a, Explicit -> True],
                           y
                          ];
             SetAttributes[palr,HoldFirst];
             tt = DotSimplify[FeynCalcInternal[x]//cov(*//palr*),
                              Expanding -> False
                             ] /. SUNDelta -> SUNDeltaContract /.
                                  SUNDeltaContract -> SUNDelta;
             If[!FreeQ[tt, LorentzIndex],
                tt = Contract[tt, EpsContract -> False,
                                  Expanding -> False]];
             If[!FreeQ[tt, SUNT],
                tt = (tt /. DOT -> doot) //.
                {doot[a___,b_ /; FreeQ[b,SUNT], c__SUNT, d___] :>
                 doot[a,c,b,d]} /.
                 {doot[a__SUNT, b__] :>
                 (doot[a] doot[b]) /; FreeQ[{b},SUNIndex]} /. doot -> DOT
               ];
             If[!FreeQ[tt, SUNF],
                tt = tt /. ( SUNF[a_,b_,c_] SUNF[d_,e_,f_] :>
                             SUNSimplify[SUNF[a,b,c] SUNF[d,e,f]] ) /.
                     SUNDelta->SUNDeltaContract /. SUNDeltaContract->SUNDelta
               ];

             If[CheckContext["PauliSigma"],
                paulisigsimp[y_] := FixedPoint[sigident, y, 1442];
                sigident[z_] := DotSimplify[(z /. DOT -> doot //.
                {doot[w1___, DotProduct[PauliSigma, a_],
                             DotProduct[PauliSigma, b_], w2___
                     ] :> (doot[w1, DotProduct[a, b], w2] +
                           I doot[w1, DotProduct[PauliSigma,
                                            CrossProduct[a, b]], w2
                                 ]
                          )
                } /. doot -> DOT), Expanding -> False];
                tt = paulisigsimp[tt]
               ];
              If[CheckContext["FeynAmpDenominator"],
                 tt = FeynAmpDenominatorCombine[tt];
                ];
                tt];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Trick | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: UnDeclareNonCommutative *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: test for non-commutativity *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`UnDeclareNonCommutative`",
             "HighEnergyPhysics`FeynCalc`"];

UnDeclareNonCommutative::usage =
"UnDeclareNonCommutative[a, b, ...] undeclares a,b, ... to be
noncommutative, i.e., DataType[a,b, ..., NonCommutative] = False
is performed.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


(*MakeContext[DataType, NonCommutative];
*)

UnDeclareNonCommutative[] := soso /;
Message[UnDeclareNonCommutative::argrx,
        UnDeclareNonCommutative, 0, "1 or more"];

UnDeclareNonCommutative[b__] :=
 (Map[Set[HighEnergyPhysics`FeynCalc`DataType`DataType[#,
          HighEnergyPhysics`FeynCalc`NonCommutative`NonCommutative],
          False]&, {b}
     ]; Null);

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "UnDeclareNonCommutative | \n "]];
Null
(* :Title: Upper *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Upper`",
             "HighEnergyPhysics`FeynCalc`"];

Upper::usage= "Upper may be used inside LorentzIndex to indicate an
contravariant LorentzIndex.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Upper | \n "]];
Null

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: XYT*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`XYT`",
             "HighEnergyPhysics`FeynCalc`"];

XYT::usage= "XYT[exp, x,y] transforms  (x y)^m away ...";


(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


MakeContext[ Factor2, PowerSimplify];

(* integral transformation only valid if nonsingular in x, y = 0,1 *)
XYT[exp_, x_, y_] := Block[{z, t, u},
    t = 1/x Factor2[PowerSimplify[Factor2[exp] /. y -> (z/x)]];
    Factor2[PowerSimplify[(1-z) (t /. x :> (1-z) u + z)]/.{u:>y,z:>x}]
                          ];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "XYT | \n "]];
Null

(* :Title: ChiralityProjector *)

(* :Author: Rolf Mertig *)


(* :Summary: left and right handed projectors *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`ChiralityProjector`",
               "HighEnergyPhysics`FeynCalc`"];

ChiralityProjector::usage =
"ChiralityProjector[+1] denotes DiracGamma[6] (=1/2(1 + DiracMatrix[5])).
ChiralityProjector[-1] denotes DiracGamma[7] (=1/2(1 - DiracMatrix[5])).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];



MakeContext[DeclareNonCommutative, DiracGamma];

DeclareNonCommutative[ChiralityProjector];

ChiralityProjector[1] = DiracGamma[6];
ChiralityProjector[-1] = DiracGamma[7];

ChiralityProjector /:
   MakeBoxes[ChiralityProjector[1], TraditionalForm] :=
    SubscriptBox["\[Omega]", "+"];

ChiralityProjector /:
   MakeBoxes[ChiralityProjector[-1], TraditionalForm] :=
    SubscriptBox["\[Omega]", "-"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)


If[tarcerloadedflag===True, ToExpression["ToTFi"];
 Clear[tarcerloadedflag]];

If[$VersionNumber>3.4,
(* This loads all kind of packages; but it costs some time ... *)
(*
Calc[1];Tr[1];
*)

(*
ToExpression["ToTFi"];
*)
Unprotect[Variables];
Variables[s_List/;Length[Dimensions[s]]>1]:=Variables[Flatten[s]];
Protect[Variables];
];

If[($Notebooks =!= True) && (Global`$FeynCalcStartupMessages =!= False),
   $PrePrint = MakeContext["FeynCalcForm"];
   WriteString["stdout",
   "$PrePrint is set to FeynCalcForm. Use FI and FC to change the display format.\n"],
   If[($Notebooks =!= True), $PrePrint = MakeContext["FeynCalcForm"]];
  ];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

savethisdir=Directory[];
SetDirectory[HighEnergyPhysics`FeynCalc`Private`feyncalchepdir];

If[Global`$LoadPhi===True,
   If[$Notebooks===True,
      CellPrint[Cell[TextData[{"Loading PHI "}],
                  "Text"]],
      Print["Loading PHI "]
   ];
   If[StringMatchQ[$OperatingSystem, "*MacOS*"],
   Get[$PathnameSeparator<>"Phi"<>$PathnameSeparator<>"Phi.m"],
   Get["Phi"<>$PathnameSeparator<>"Phi.m"]]
];

If[Global`$LoadFeynArts===True,
   If[$Notebooks===True,
      CellPrint[Cell[TextData[{"Loading FeynArts "}],
                  "Text"]],
      Print["Loading FeynArts "]
   ];
   If[Get["FeynArts.m"]===$Failed,
     If[$Notebooks===True,
	CellPrint[Cell[TextData[{
	  "FeynArts not found. Please put the files in\n", 
	  HighEnergyPhysics`FeynCalc`$FeynCalcDirectory,
	  "\n", "and reload FeynCalc",
	  "\n","FeynArts can be downloaded from ",
	  ButtonBox["www.feynarts.de", ButtonData:>{
	   URL[ "http://www.feynarts.de"], None},
	  ButtonStyle->"Hyperlink", ButtonNote->"http://www.feynarts.de"]}
	 ],"Text"]],
       WriteString["stdout",
          "FeynArts not found. Please put the files in ", 
	  HighEnergyPhysics`FeynCalc`$FeynCalcDirectory,
	  ". FeynArts can be downloaded from www.feynarts.de\n"];
     ];
  ];
];

SetDirectory[savethisdir];
Clear[savethisdir];
