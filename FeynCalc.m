(* :Title: FeynCalc *)

(* :Version: 4.2.0 *)

(* :Author: Rolf Mertig  (rolfm@mertig.com) *)

(* :Summary: Tools and Tables *)

(* :Terms of use: GPL, see
                  http://www.feyncalc.org/licence.txt *)

(* :Mathematica Version 3.0 or higher *)

(* :History:

	Version 1.0 written 1991 by Rolf Mertig.
	Version 3.0 includes typesetting features of Mathematica 3.0
   Version 3.0.1.1 includes two bug-fixes for OneLoop
   Version 4.0 : 2000, reorganized for open-source and extensibility
   Version 4.2.0 : 2002, small bug fixes, more reorganization,
                   inclusion of help system, PHI and FeynArts
                   by Frederik Orellana, fjob@cabocomm.dk *)


(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)
(* ************************************************************************ *)
(* Init stuff *)
(* ************************************************************************ *)
(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)

System`MyBeginPackage[a_,b___] :=
((*What is this? F.Orellana.23/2-2003*)(*NoPrint["MB ", a];*)
 Hold[BeginPackage][a,b]//ReleaseHold);

System`MyEndPackage[] :=
((*NoPrint["EE ", Context[]]; *)EndPackage[]);

HighEnergyPhysics`FeynCalc`$FeynCalcVersion = "4.2.0";

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
            StringMatchQ[#, "HighEnergyPhysics`fcloops`*"] ||
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
    (*Current directory is "HighEnergyPhysics"*)
    (HighEnergyPhysics`FeynCalc`$FeynCalcDirectory =
    FileNames["FeynCalc.m",{Directory[]}]) != {},

    HighEnergyPhysics`FeynCalc`$FeynCalcDirectory = Directory[];
If[MemberQ[$Path,Evaluate[ParentDirectory[Directory[]]]]!=True,
$Path=Append[$Path,ParentDirectory[Directory[]]]],

    (*Installation in current directory*)
    (HighEnergyPhysics`FeynCalc`$FeynCalcDirectory =
    FileNames["HighEnergyPhysics",{Directory[]}]) != {},

    HighEnergyPhysics`FeynCalc`$FeynCalcDirectory =
    HighEnergyPhysics`FeynCalc`$FeynCalcDirectory[[1]];
If[MemberQ[$Path,Evaluate[ParentDirectory[
HighEnergyPhysics`FeynCalc`$FeynCalcDirectory]]]!=True,
$Path=Append[$Path,ParentDirectory[HighEnergyPhysics`FeynCalc`$FeynCalcDirectory]]],

   (*Installation in ~/.Mathematica*)
   (HighEnergyPhysics`FeynCalc`$FeynCalcDirectory =
   Join[FileNames[$HomeDirectory <> $PathnameSeparator <>
    ".Mathematica" <> $PathnameSeparator <> "*" <> $PathnameSeparator <>
    "AddOns" <> $PathnameSeparator <> "Applications" <> $PathnameSeparator <>
    "HighEnergyPhysics"],
    FileNames[$HomeDirectory <> $PathnameSeparator <>
    ".Mathematica" <> $PathnameSeparator <> "Applications" <> $PathnameSeparator <>
    "HighEnergyPhysics"]]) != {} &&
   FileNames[Sort[HighEnergyPhysics`FeynCalc`$FeynCalcDirectory][[-1]]<>
   $PathnameSeparator <> "FeynCalc.m"] != {},

    (*We take the most recent installation*)
    HighEnergyPhysics`FeynCalc`$FeynCalcDirectory =
    Sort[HighEnergyPhysics`FeynCalc`$FeynCalcDirectory][[-1]],

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
  {"Tarcer", "tarcer", "Phi",
   "FeynArts", "GraphInfo", "Models", "ShapeData",
   "documentation", "Documentation",
   "CVS", "cvs", ".AppleDouble"};

HighEnergyPhysics`FeynCalc`Private`configfile= "FCConfig.m";


SetDirectory[HighEnergyPhysics`FeynCalc`$FeynCalcDirectory];
If[
FileNames[HighEnergyPhysics`FeynCalc`Private`configfile] =!= {},
 Get@HighEnergyPhysics`FeynCalc`Private`configfile;
  ];
ResetDirectory[];

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
   Remove@@{ToExpression["Tr"]};
   (*And, well, System`MonomialList is gone;
    we construct a replacement for use in Collect3.
    F.Orellana, 17/9-2002*)
    System`MonomialList =
    Internal`FromDistributedTermsList[
      Internal`DistributedTermsList[#1, Sequence@@Drop[{##}, 1]],
    List]&,
Scan[ {Remove @@ Names["Global`"<>#], ToExpression["System`"<>#]}&,
  { "CommonDefaultFormatTypes" }];
  ];


savethisdir=Directory[];
 HighEnergyPhysics`FeynCalc`Private`feyncalchepdir =
HighEnergyPhysics`FeynCalc`$FeynCalcDirectory;
SetDirectory[HighEnergyPhysics`FeynCalc`Private`feyncalchepdir];


If[Global`$LoadTARCER ===True,

SetDirectory["Tarcer"];
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
                  "Tarcer"}, "TARCER.nb"]], ButtonStyle -> "Hyperlink",
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

(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)
(* ************************************************************************ *)
(* Main context - usage definitions *)
(* ************************************************************************ *)
(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`"];

CheckContext::"usage"=
"CheckContext[string] yields True if the packaged associated with \
string is already loaded, and False otherwise.";

If[$VersionNumber<4.0,
FeynCalc::"usage"=
"For installation notes visit www.feyncalc.org\n
For a list of availabe objects type $FeynCalcStuff, \
which contains a list of all functions and options in StringForm. \
You can get on-line information by ?function, (e.g.; ?Contract). Eventually \
you have to first enter the command function once (e.g., type Contract \
and hit Return), which loads the necessary libraries, and the \
?function (?Contract) will work.\n\n
There are several useful functions for short input, type $FCS for a list of \
short commands. Then type, e.g., ?GA.\n \n
To get rid of the start-up messages put the line \n
$FeynCalcStartupMessages = False; \n
 into your init.m or the HighEnergyPhysics/FCConfig.m file.",
FeynCalc::"usage"=
"For installation notes visit www.feyncalc.org\n
For a list of availabe objects type $FeynCalcStuff, \
which contains a list of all functions and options in StringForm. \
You can get on-line information by ?function, e.g., ?Contract.\n
There are several useful functions for short input, type $FCS for a list of \
short commands. Then type, e.g., ?GA.\n \n
To get rid of the start-up messages put the line \n
$FeynCalcStartupMessages = False; \n
 into your init.m or the HighEnergyPhysics/FeynCalcConfig.m file."
];

Li2::"usage"="Li2 is an abbreviation for the dilog function, i.e., Li2 = PolyLog[2,#]&.";
Li3::"usage"="Li3 is an abbreviation for the trilog function, i.e., Li3 = PolyLog[3,#]&.";

FI::"usage"=
"FI changes the output format to InputForm. \
This is useful to see the internal representation of FeynCalc \
objects. To change back to FeynCalcForm use FC.";

FC::"usage"=
"FC changes the output format to FeynCalcForm. \
To change to InputForm use FI.";

MakeContext::"usage"=
"MakeContext[string] constructs the context path of string. \
MakeContext is invoked at startup of FeynCalc. \
MakeContext[a, b] construct the context path of b defined \
in context of a.";

SPC::"usage"=
"SPC is an abbreviation for ScalarProductCancel.";

SPL::"usage"=
"SPL is an abbreviation for SimplifyPolyLog.";

SubContext::"usage"=
"SubContext[fun] gives the sub-directory (context) in \
HighEnergyPhysics.";

$AL::"usage"=
"$AL is the head of dummy indices which may be introduced by \
Uncontract.";

$Color::"usage"=
"$Color is False by default. If set to True, some special variables \
will be colored.";

$Covariant::"usage" =
"The boolean setting of $Covariant determines whether \
lorentz indices are displayed as lower indices (True) or as \
upper ones (False).";

$FeynCalcStuff::"usage"=
"$FeynCalcStuff is the list of availabe stuff in FeynCalc.";

$FeynCalcVersion::"usage"=
"$FeynCalcVersion is a string that represents the version of FeynCalc.";

$FCS::"usage"="$FCS is a list of functions with a short name. \
E.g. GA[nu] can be used instead of DiracGamma[nu]." ;

$FCT::"usage"="If $FCT is set to True special typesetting rules for \
built-in functions (like Dot) are changed.";

$FortranContinuationCharacter::"usage"="$FortranContinuationCharacter \
is the continuation character used in Write2.";

$Gauge::"usage"=
"$Gauge(= 1/xi) is a constant specifying the gauge fixing parameter of QED \
in Lorentz gauge.  The usual choice is Feynman gauge, $Gauge=1. \
Notice that $Gauge is used by some functions, the option Gauge by others.";

$Gauge/:
MakeBoxes[$Gauge,TraditionalForm]:=
MakeBoxes[StyleForm["\[Lambda]",FontSlant->"Italic"]];

$BreitMaison::"usage"=
"The Breitenlohner-Maison gamma5 scheme is currently not supported by \
the Dirac algebra functions. Use Tracer if you need it.";

(*
  "The setting of $BreitMaison determines whether the Breitenlohner-
        Maison scheme is applied. If $BreitMaison=True, the so-called
        naive gamma5 prescription is used, i.e. gamma5 anticommutes in
        all dimensions.  The default is False. The setting should
        be chosen in the file FeynCalc.m BEFORE loading the package.
        Reversion during a session is not possible.";
*)

(*$Kreimer::"usage"=
"Experimental setup of the Kreimer-scheme for Gamma5. Better don't use it.";*)

$Larin::"usage"=
"If set to True, the Larin-Gorishny-Atkyampo-DelBurgo-scheme for \
gamma5 in D-dimensions is used, i.e. before evaluating traces \
(but after moving gamma5 anticommuting in all dimensions to the \
right of the Dirac string) a product  gamma[mu].gamma5 is \
substituted to  -I/6 Eps[mu,al,be,si] gamma[al,be,si], \
where all indices live in D-dimensions now. \
Especially the Levic-Civita tensor is taken to be \
D-dimensional, i.e., contraction of two Eps's results in D's. \
This has (FOR ONE AXIAL-VECTOR-CURRENT ONLY, it is not so clear \
if this scheme also works for more than one fermion line \
involving gamma5) the same effect as the \
Breitenlohner-Maison-'t Hooft-Veltman scheme.";

$LimitTo4::"usage"=
"$LimitTo4 is a global variable with default setting True. \
If set to False no limit Dimension -> 4 is \
performed after tensor integral decomposition.";

$LorentzIndices::"usage"=
"$LorentzIndices is a global variable. If set to True the dimension \
of LorentzIndex is displayed as an index.";

$MemoryAvailable::"usage"=
"$MemoryAvailable is  a global variable which is set to an integer \
n, where n is the available amount of main memory in MB. \
The default is 128. It should be increased if possible. \
The higher $MemoryAvailable can be, the more intermediate \
steps do not have to be repeated by FeynCalc.";

$MIntegrate::"usage"=
"$MIntegrate is a global list of integrations done by Mathematica \
inside OPEIntDelta.";

$NonComm::"usage"=
"$NonComm contains a list of all non-commutative heads present.";

$MU::"usage"=
"$MU is the head of dummy indices which may be introduced by \
Chisholm (and evtl. Contract and DiracReduce).";

$OPEWard::"usage"=
   "$OPEWard is experimental.";

$PairBrackets::"usage" =
"$PairBrackets determines whether brackets are drawn around \
scalar products in the notebook interface.";

$SpinorMinimal::"usage"=
"$SpinorMinimal is a global switch for an additional simplification \
attempt in DiracSimplify for more than one Spinor-line. \
The default is False, since otherwise it costs too much time.";

$VeryVerbose::"usage"=
"$VeryVerbose is a global variable with default setting 0. \
If set to 1, 2, ..., less and more intermediate comments and informations \
are displayed during calculations.";

$West::"usage"=
"If $West is set to True (which is the default), \
traces involving more than 4 Dirac matrices \
and gamma5 are calculated recursively according to formula (A.5) from \
Comp. Phys. Comm 77 (1993) 286-298, which is based on the Breitenlohner \
Maison gamma5 - scheme.";

$AchmedRoss::"usage"= "experimental";

$Abbreviations::"usage"=
"$Abbreviations is a list of string substitution rules used when \
generating names for storing intermediate results. \
It is used by OneLoop and PaVeReduce.\
The elements of the list should be of the form \"name\" -> \"abbreviation\".";

$Abbreviations = {", "->"","^"->"","{"->"", "/" -> "",
                  "Subscript"->"su","SmallVariable"->"sma",
                  "}"->"", "["->"", "]"->"", "*" -> "", " " -> "" ,
		  "\n" -> "", "\r" -> ""};

(* Added 23/2-2003. F.Orellana. *)
$Multiplications::"usage" =
    "$Multiplications is a set functions which should be treated as
(commutative or non-commutative) multiplications by FieldDerivative.";

$DistributiveFunctions::"usage" =
    "$DistributiveFunctions is a set of functions over which FieldDerivative
should be distributed.";

$Containers::"usage" =
    "$FieldContainers is a set of heads over which FieldDerivative should
distribute, in the following sense: Let c be a member of $Containers. Then
FieldDerivative[c[f, g, h][x], x, {mu}] ->
c[FieldDerivative[f[x], x, {mu}], FieldDerivative[f[x], x, {mu}],
FieldDerivative[f[x], x, {mu}]].";

TBox::"usage"="TBox[a, b, ...] produces a RowBox[{a,b, ...}] where \
a,b, ... are boxed in TraditionalForm.";

Tbox::"usage"="TBox[a, b, ...]  produces a RowBox[{a,b, ...}] where \
a,b, ... are boxed in TraditionalForm.";

OptionsSelect::"usage"= "OptionsSelect[function,opts] returns the option settings of opts \
accepted by function.  When an option occurs several times in opts, the first \
setting is selected";

(* DOT moved into main context. 32/2-2003. F.Orellana.
   The reason for this is the following: In order to use
   DOT instead of Dot consistenly everywhere, we need to be
   able to do e.g. f[DOT[a_,b_]]:=g[a,b]. Because DOT is immediately
   set to something else (Dot), this works only if DOT is in
   $ContextPath at the moment this definition is evaluated.
   In the context of the packages, $ContextPath is typically
   {"System`", "HighEnergyPhysics`package`", "HighEnergyPhysics`FeynCalc`"}. *)

DOT::"usage" =
"DOT[a, b, ...] is the FeynCalc function for non-commutative \
multiplication. By default it is set to the Mathematica Dot \
functions. By setting  \n
DOT=. \n
this can be disabled. \
Note that then non-commutative products should to be entered \
like DOT[ DiracMatrix[mu], m + DiracSlash[p], DiracMatrix[mu] ], \
etc.";


(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)
(* ************************************************************************ *)
(* Functions in main context and declaration of packages *)
(* ************************************************************************ *)
(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)


(* ------------------------------------------------------------------ *)
Begin["`Private`"]

(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)


$AchmedRoss = False;
$BreitMaison =  False;
(* SetAttributes[$BreitMaison, Locked];*)
$Color       = False;
$Covariant = False;

$FCS = {"FAD", "FV", "FVD", "GA", "GA5", "GS",
          "GSD", "LC", "LCD", "MT","MTD", "SD", "SOD",
          "SP", "SPC", "SPD", "SPL", "FCI", "FCE", "FI",
          "FC", "GGV", "GP", "QGV", "QO", "FDr", "CDr"
         };

$FCT  = False;
$FortranContinuationCharacter = "&";
(*If[!ValueQ[$Kreimer],  $Kreimer = False];*)
$Larin   = False;
$LimitTo4 = True;
$LorentzIndices = False;
$MemoryAvailable = 256;
$OPEWard = False;
$PairBrackets = False;
$MIntegrate = {};
$SpinorMinimal = False;
If[!ValueQ[$VeryVerbose],  $VeryVerbose   = 0];

$West = True;

DOT = Dot;

$Multiplications = {Times, DOT};
$DistributiveFunctions = {Conjugate, Transpose};
$Containers = {};

FI := (Format[LineBreak[_]]:= ""; $PrePrint=InputForm;);
FC := (Format[LineBreak[_]]:= "\n";
       (If[!$Notebooks, $PrePrint= MakeContext["FeynCalcForm"],
               Unset[$PrePrint]]);
      );

Li3 = PolyLog[3,#]&;
Li2 = PolyLog[2,#]&;

SPC := ToExpression["ScalarProductCancel"];
SPL := ToExpression["SimplifyPolyLog"];

  CheckContext[{x__String}] := MemberQ[$Packages,
   StringJoin@@Flatten[{"HighEnergyPhysics`",
   Map[SubContext[#] <> # <>"`"&,{x}]}]];

  CheckContext[x_String] := MemberQ[$Packages,
   StringJoin["HighEnergyPhysics`", SubContext[x] <> x, "`"]];

OptionsSelect[function_, opts___] :=
  Select[(Cases[{opts}, _Rule,
          Infinity] //. ({a___, b_ -> c_, d___, b_ -> e_, f___} -> {a, b -> c,
               d, f})), (!
          FreeQ[#, (Options[function] /. (a_ -> b_) -> a -> _ /.
                List -> Alternatives)]) &];

If[!ValueQ[$NonComm], $NonComm = {}];

Unprotect[ToBoxes];


 MyNeeds[x_String] := ( If[!MemberQ[$Packages, x], Needs[x]]);

  (* This is for the grouping of files in subdirectories in
     HighEnergyPhysics, like:  general, fcloops, fctools, fctables, qcd
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

DPS[x__] := DPS[x] = DeclarePackage[x];

fcDeclarePackge[{x_, y_List}] := fcDeclarePackge[x, y];
fcDeclarePackge[{x_, y_ /;(Head[y]=!=List),z___}] :=
  fcDeclarePackge[x, {Last[Flatten[{x}]], y, z}];

fcDeclarePackge[x_, y_] :=
If[!CheckContext[x],
  DPS[StringJoin["HighEnergyPhysics`", SubContext[x], x, "`"], y]
];


fcDeclarePackge[x_String] := fcDeclarePackge[x, x];

(*WHY?? - Commented out. F.Orellana. 7/9-2002*)
(*DeclarePackage["HighEnergyPhysics`fcloops`OneLoop`",{"OneLoopSum"}];*)


(* Multiple objects in the same context *)

multifunpack=
{
{"Contract","Contract2", "Contract3"},
{"DiracSlash", "SL"},
{"DiracSimplify", "ChisholmSpinor", "DiracCanonical", "InsideDiracTrace"},
{"DotSimplify", "DotSimplifyRelations"},
{"FermionSpinSum", "SpinorCollect"},
{"FeynAmpDenominator", "FD"},
{"FeynAmpDenominatorSimplify", "FDS"},
{"FeynCalcForm", "FCF"},
{"FeynCalcInternal", "FCI"},
{"FeynCalcExternal", "FCE"},
{"FeynCalc2FORM", "FORMEpilog", "FORMProlog", "TraceDimension"},
{"FieldStrength", "IndexPosition"},
{"FORM2FeynCalc", "Vectors"},
{"GluonGhostVertex", "GGV"},
{"GluonPropagator", "GP"},
{"GhostPropagator", "GHP"},
{"GluonVertex", "GV"},
{"Isolate", "IsolatePrint", "IsolateSplit"},
{"OneLoop", "CancelQP", "CombineGraphs", "DenominatorOrder", "FinalFunction", "ExtraVariables",
"OneLoopSum", "Prefactor", "SelectGraphs", "ReduceGamma", "ReduceToScalars", "SmallVariables",
"StandardMatrixElement"},
{"PropagatorDenominator", "PD"},
{"QuarkGluonVertex", "QGV"},
{"QuarkPropagator", "QP"},
{"SquareAmplitude", "EnergyMomentumConservation", "SpinSumExternalMomentum", "SelectedGraphs"},
{"Twist2GluonOperator", "GO"},
{"Twist2QuarkOperator", "QO"},
{"Write2", "FUNCTION", "PostFortranFile", "PreFortranFile"},
{"DoPolarizationSums", "PolarizationUncontract", "EpsUncontract"},
{"ILimit", "FunctionLimits"}
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

   ResetDirectory[];

   declarepackagelist = Join[declarepackagelist, multifunpack];
   $FeynCalcStuff = Union[Flatten[declarepackagelist]];

TBox[] = "\[Null]";
TBox[a_] := ToBoxes[a, TraditionalForm];
Tbox[]    = "\[Null]";
Tbox[a_] := totr[a];

TBox[a_,b__] := RowBox @ Map[(ToBoxes @@ {#, TraditionalForm})&, {a, b}];

totr[Subscript[y_,in__Integer]] := SubscriptBox[totr[y],RowBox[{in}]];

totr[y_Symbol] := If[FormatValues[Evaluate[y]] === {},
                     ToString[y],
                     ToBoxes[y, TraditionalForm], y];

totr[y_String] := y;
totr[y_] := ToBoxes[y, TraditionalForm] /; Head[y]=!=Symbol;


(* somehow \[NoBreak] does not really work, but it does
the spacing "right" ...*)

Tbox[a_,b__] :=
(RowBox @ (Insert[
  Map[totr, {a,b}], "\[NoBreak]",
    Array[{#}&,Length[{a,b}]-1,2]]));

Unprotect[Dot];
Dot /:
MakeBoxes[Dot[a__], TraditionalForm] := (
ClearAttributes[Times, Orderless];
embo = MakeBoxes[Times[a], TraditionalForm];
SetAttributes[Times, Orderless];
embo ) /; $FCT === True;


End[];
MyEndPackage[];


Map[HighEnergyPhysics`FeynCalc`Private`fcDeclarePackge,
    HighEnergyPhysics`FeynCalc`Private`declarepackagelist];

(* take care of SubContext of the multifunpack values *)
(* (avoid Global` symbols, F.Orellana, 14/9-2002) *)

HighEnergyPhysics`FeynCalc`Private`tab =
(
Table[ Map[ HighEnergyPhysics`FeynCalc`Private`setsubcontext[#,
           HighEnergyPhysics`FeynCalc`Private`multifunpack[[i,1]] ]&,
        Rest[ HighEnergyPhysics`FeynCalc`Private`multifunpack[[i]] ] ],
       {i, Length[HighEnergyPhysics`FeynCalc`Private`multifunpack]}
     ] /. HighEnergyPhysics`FeynCalc`Private`setsubcontext[a_String,b_String] :>
            {Hold[Set][Hold[SubContext][a], Hold[SubContext][b]],
            (*Hold[Set][Hold[MakeContext][a], Hold[MakeContext][b]]*)
            (*Bug fix, F.Orellana, 14/9-2002*)
            Hold[Set][Hold[MakeContext][a], Hold[MakeContext][b, a]]}
) ;


{Hold[Set][Hold[SubContext][a], Hold[SubContext][b]],
             Hold[Set][Hold[MakeContext][a], Hold[MakeContext][b]]}

ReleaseHold[HighEnergyPhysics`FeynCalc`Private`tab];

(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)

(* ------------------------------------------------------------------ *)

(* Print startup message *)

If[Global`$FeynCalcStartupMessages =!= False ,
If[$Notebooks===True,
   CellPrint[Cell[TextData[{StyleBox[ "FeynCalc" , FontWeight-> "Bold"], " ",
    $FeynCalcVersion,
     "\nFor help, type ?FeynCalc,\nuse the built-in ",
     ButtonBox["help system",
       ButtonFunction -> (FrontEndExecute[FrontEnd`HelpBrowserLookup["AddOns", #]] &),
       ButtonData:>{ "Short Overview", "intro"},
       ButtonStyle->"AddOnsLink",
       ButtonNote->"Open the help browser"],
     "\nor visit ",
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

];
Clear[feversion];



(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)
(* ************************************************************************ *)
(* Subpackages in subcontexts of HighEnergyPhysics`FeynCalc` *)
(* ************************************************************************ *)
(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)


(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Abbreviation *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Abbreviation`",
             "HighEnergyPhysics`FeynCalc`"];

Abbreviation::"usage"=
"Abbreviation[name] gives a shortname for name (in HoldForm). \
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

(* :Summary: Anti5 anticommutes gamma5's right or left *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Anti5`",
             "HighEnergyPhysics`FeynCalc`"];

Anti5::"usage" =
"Anti5[exp] anticommutes all gamma5 one time to the right. \
Anti5[exp, n] anticommutes all gamma5 n times to the right. \
Anti5[exp, -n] anticommutes all gamma5 n times to the left.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[DiracGamma, FeynCalcInternal, MemSet];

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

AntiCommutator::"usage"=
"AntiCommutator[x, y] = c  defines the anti-commutator of the \
non-commuting objects x and y. \
Settings of AntiCommutator (e.g.AntiCommutator[a,b]=c) \
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



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: AntiQuarkField *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`AntiQuarkField`",
             "HighEnergyPhysics`FeynCalc`"];

AntiQuarkField::"usage" =
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

Bracket::"usage"= "Bracket is an option for Convolute.";

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


CA::"usage"=
"CA is one of the Casimir operator eigenvalues of SU(N); CA = N";

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

CF::"usage"=
"CF is one of the Casimir operator eigenvalues of SU(N); CF = (N^2-1)/(2 N)";

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

Cases2::"usage"=
"Cases2[expr, f] is equivalent to \
Cases[{expr}, HoldPattern[f[___]], Infinity]//Union. \
Cases2[expr, f1, f2, ...] or \
Cases2[expr, {f1, f2, ...}] is equivalent to \
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

(* :Title: ChiralityProjector *)

(* :Author: Rolf Mertig *)


(* :Summary: left and right handed projectors *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`ChiralityProjector`",
               "HighEnergyPhysics`FeynCalc`"];

ChiralityProjector::"usage" =
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
If[$VeryVerbose > 0,WriteString["stdout", "ChiralityProjector | \n "]];
Null



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

Collecting::"usage" =
"Collecting is an option of Contract2, ScalarProductCancel, SquareAmplitude, \
Series2, TID and related functions. Setting it to True will trigger \
some kind of collecting of the result.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Collecting | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Commutator *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Commutator`",
             "HighEnergyPhysics`FeynCalc`"];

Commutator::"usage"=
"Commutator[x, y] = c  defines the commutator between the non-commuting \
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


Commutator/: MakeBoxes[Commutator[a_, b_],
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

CommutatorExplicit::"usage"=
"CommutatorExplicit[exp] substitutes any Commutator and AntiCommutator \
in exp by their definitions.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[
AntiCommutator,
Commutator
];

CommutatorExplicit[exp_] := exp /.
   {Commutator :> ((DOT[#1, #2] - DOT[#2, #1])&),
    AntiCommutator :> ((DOT[#1, #2] + DOT[#2, #1])&)
   };

End[]; MyEndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "CommutatorExplicit | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ComplexIndex *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: The head for complex conjugated indices *)

(* ------------------------------------------------------------------------ *)

(*

MyBeginPackage["HighEnergyPhysics`FeynCalc`ComplexIndex`",
             "HighEnergyPhysics`FeynCalc`"];

ComplexIndex::"usage"=
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

*)



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: CounterT*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`CounterT`",
             "HighEnergyPhysics`FeynCalc`"];

CounterT::"usage"= "CounterT is a factor used by GluonPropagator and \
QuarkPropagator when CounterTerm is set to All.";

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

CouplingConstant::"usage" =
"CouplingConstant is an option for several Feynman rule functions and \
for CovariantD and FieldStrength.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "CouplingConstant | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DataType *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: DataType is just a data type *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DataType`",
             "HighEnergyPhysics`FeynCalc`"];

DataType::"usage"=
"DataType[exp, type] = True   defines the object exp to have datatype type. \
DataType[exp1, exp2, ..., type] defines the objects exp1, exp2, ... to \
have datatype type. \
The default setting is DataType[__, _]:=False. \
To assign a certain data type, do e.g.: \
DataType[x, PositiveInteger] = True.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"]


MakeContext[NonCommFreeQ, NonCommQ, SelectFree];
noncommutative := noncommutative = MakeContext["NonCommutative"];

DataType[_] := soso /; Message[DataType::argrx, DataType, 1, "2 or more"];
DataType[] := soso /; Message[DataType::argrx, DataType, 0, "2 or more"];

(* Listability of DataType[x,y,z,type]=bol *)

DataType /: HoldPattern[Set[DataType[a_, b__,type_], bool_]] :=
            Map[set[dt[#, type], bool]&, {a, b}] /. {set:>Set,dt:>DataType};

DataType[a_, b__, type_] := Flatten[{DataType[a, type], DataType[b, type]}];

(* Special rules for NonCommutative *)
(* Setting DataType[x,NonCommutative]=True or DataType[x,NonCommutative]=False
   updates $NonComm and NonCommFreeQ *)

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
           (*Let's update NonCommQ also. F.Orellana, 11/9-2002*)
            nnt = (RuleDelayed @@ {HoldPattern @@ {ncq[exp]}, True}
                  ) /. ncq -> NonCommQ;
            set[downvalues[NonCommQ],Prepend[
                SelectFree[DownValues@@{NonCommQ}, exp], nnt
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
           (*Let's update NonCommQ also. F.Orellana, 11/9-2002*)
            nnt = (RuleDelayed @@ {HoldPattern @@ {ncq[exp]}, False}
                  ) /. ncq -> NonCommQ;
            set[downvalues[NonCommQ],Prepend[
                SelectFree[DownValues@@{NonCommQ}, exp], nnt
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

DeclareNonCommutative::"usage" =
"DeclareNonCommutative[a, b, ...] declares a,b, ... to be \
noncommutative, i.e., DataType[a,b, ...,  NonCommutative] is set to \
True.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

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


(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DeltaFunction *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  Dirac-delta function  (just a name) *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DeltaFunction`",
             "HighEnergyPhysics`FeynCalc`"];

DeltaFunction::"usage"= "DeltaFunction is the Dirac delta-function.";

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

(* :Title: DeltaFunctionPrime *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  Dirac-delta function derivative (just a name) *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DeltaFunctionPrime`",
             "HighEnergyPhysics`FeynCalc`"];

DeltaFunctionPrime::"usage"=
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

DeltaFunctionDoublePrime::"usage"=
"DeltaFunctionDoublePrime denotes the second derivative of the \
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



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Dimension *)

(* :Author: Rolf Mertig *)


(* :Summary: Dimension is an option for several functions *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Dimension`",
               "HighEnergyPhysics`FeynCalc`"];

Dimension::"usage" =
"Dimension is an option for DiracMatrix, DiracSlash, FourVector, \
LeviCivita, MetricTensor, SetMandelstam, OneLoop and ScalarProduct. \
The default setting is sometimes 4, sometimes D. \
The setting should always be 4, a symbol (D, n, ...), or \
(D-4), (n-4), ... .";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Dimension | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DimensionalReduction *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DimensionalReduction`",
             "HighEnergyPhysics`FeynCalc`"];

DimensionalReduction::"usage"= "DimensionalReduction is an option \
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

DiracBasis::"usage" =
"DiracBasis[any] is a head which is wrapped around Dirac structures \
(and the 1) as a result of the function DiracReduce. \
Eventually you want to substitute DiracBasis by Identity (or \
set: DiracBasis[1] = S; DiracBasis[DiracMatrix[mu]] = P; etc.).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracBasis | \n "]];
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

DiracGamma::"usage" =
"DiracGamma[x, dim] is the way all Dirac \
matrices and slashes are represented (in the internal representation). \
Use DiracMatrix (or GA, GAD) and DiracSlash (or GS, GSD) \
for manual (short) input. \
DiraGamma[x, 4] simplifies to DiracGamma[x].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ LorentzIndex, ExplicitLorentzIndex, Momentum,
             DeclareNonCommutative, DiracGammaT];

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
DiracGamma[Momentum[x_,dix___], Momentum[y_,diy___]] := DOT[
  DiracGamma[Momentum[x,dix], dix], DiracGamma[Momentum[y,diy], diy]];
DiracGamma[Momentum[x_,dix___], Momentum[y_,diy___], z__] := DOT[
  DiracGamma[Momentum[x,dix], dix], DiracGamma[Momentum[y,diy], diy],
DiracGamma[z]];

DiracGamma[LorentzIndex[x_,dix___], LorentzIndex[y_,diy___]] := DOT[
  DiracGamma[LorentzIndex[x,dix], dix],
    DiracGamma[LorentzIndex[y,diy], diy]];
DiracGamma[LorentzIndex[x_,dix___], LorentzIndex[y_,diy___], z__] := DOT[
  DiracGamma[LorentzIndex[x,dix], dix],
  DiracGamma[LorentzIndex[y,diy], diy], DiracGamma[z]];

DiracGamma[LorentzIndex[x_,dix___], Momentum[y_,diy___]] := DOT[
  DiracGamma[LorentzIndex[x,dix], dix], DiracGamma[Momentum[y,diy], diy]];
DiracGamma[LorentzIndex[x_,dix___], Momentum[y_,diy___], z__] := DOT[
  DiracGamma[LorentzIndex[x,dix], dix], DiracGamma[Momentum[y,diy], diy],
   DiracGamma[z]];

DiracGamma[Momentum[x_,dix___], LorentzIndex[y_,diy___]] := DOT[
  DiracGamma[Momentum[x,dix], dix], DiracGamma[LorentzIndex[y,diy], diy]];
DiracGamma[Momentum[x_,dix___], LorentzIndex[y_,diy___], z__] := DOT[
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
   ) /; $BreitMaison === True && (lo === LorentzIndex || lo === ExplicitLorentzIndex);

DiracGamma /:
  MakeBoxes[ DiracGamma[lo_[in_], ru___Rule], TraditionalForm ] :=
   (SuperscriptBox["\[Gamma]", Tbox[lo[in]]]
   ) /; $BreitMaison === False && (lo === LorentzIndex || lo === ExplicitLorentzIndex);

DiracGamma /:
  MakeBoxes[ DiracGamma[lo_[in_,d_Symbol], _Symbol,
           ru___Rule], TraditionalForm ] :=
   (SuperscriptBox["\[Gamma]", Tbox[lo[in,d]]]
   ) /; (lo === LorentzIndex || lo === ExplicitLorentzIndex);

DiracGamma /:
  MakeBoxes[ DiracGamma[lo_[in_, d_Symbol-4], d_Symbol-4,
             ru___Rule], TraditionalForm
           ] :=
      SuperscriptBox[RowBox[{OverscriptBox["\[Gamma]","^"]}], Tbox[in]
                    ] /; (lo === LorentzIndex || lo === ExplicitLorentzIndex);

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

DiracGammaCombine::"usage"=
"DiracGammaCombine[exp] is (nearly) the inverse operation to \
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

DiracGammaExpand::"usage"=
"DiracGammaExpand[exp] expands all DiracGamma[Momentum[a+b+..]] in \
exp into (DiracGamma[Momentum[a]] + DiracGamma[Momentum[b]] + ...).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ DiracGamma, MomentumExpand, Momentum];

(* Catch DiracGamma[Momentum[a] + Momentum [b] +...].
   F.Orellana. 26/2-2003 *)
(* Redundant I think.*)
(*extraDiracRule = DiracGamma[b : HoldPattern[
      Plus[(___*Momentum[__] | Momentum[__]) ..]], dim___]  :>
      (DiracGamma[#, dim]& /@ b);*)

DiracGammaExpand[x_] :=
If[FreeQ[x, DiracGamma], MakeContext["FeynCalcInternal"][x], x
  ] /. DiracGamma -> gaev /. gaevlin -> DiracGamma (*/. extraDiracRule*);
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

DiracGammaT::"usage" =
"DiracGammaT[x] denotes the transpose of DiracGamma. \
Transpose[DiracGammaT[x]] gives DiracGamma[x]. \
Note that x must have Head LorentzIndex or Momentum.";


(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ DeclareNonCommutative, DiracGamma, LorentzIndex, Momentum];

DeclareNonCommutative[DiracGammaT];

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


DiracMatrix::"usage" =
"DiracMatrix[m] denotes a Dirac gamma matrix with Lorentz index m. \
DiracMatrix[m1, m2, ..] is a product of gamma matrices with Lorentz \
indices m1, m2, etc. DiracMatrix[5] is gamma5.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ DeclareNonCommutative, Dimension, DiracGamma, LorentzIndex,
             ExplicitLorentzIndex];

fci  := fci = MakeContext["FeynCalcInternal"];

Options[DiracMatrix] = {Dimension -> 4, fci -> True};

DeclareNonCommutative[DiracMatrix];

DiracMatrix[a_Integer] := DiracGamma[a];

(* 12/1-2002. Comment by F.Orellana:
   Don't know why Rolf provided this alternative input method (below).
   Think it's better to use DiracMatrix[a,b,c,...],
   which will be translated by FCI anyway as soon as
   Contract, DiracSimplify, ... is applied.
   With this alternative input method, integers are wrapped
   in ExplicitLorentzIndex, prohibiting DiracSimplify from working
   (could of course easily be fixed). *)

DiracMatrix[DOT[a_,b__], opt___Rule] := Map[DiracGamma[LorentzIndex[#,
 Dimension /. {opt} /. Options[DiracMatrix]],
 Dimension /. {opt} /. Options[DiracMatrix]]&, DOT[a,b]];

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
   MakeBoxes[DiracMatrix[x_,y___,z_,___Rule],
             TraditionalForm
            ] := RowBox @ Map[
                 SuperscriptBox["\[Gamma]",
                                MakeBoxes[#, TraditionalForm]
                               ]&,
                              {x,y,z}
                             ] /; Head[z]=!=Rule;
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

DiracOrder::"usage"=
"DiracOrder[expr] orders the Dirac matrices in expr alphabetically. \
DiracOrder[expr, orderlist] orders the Dirac matrices in expr according \
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
des  := des                 = MakeContext["DiracTrick"];


dotLin[z_] := dotsimplify[z(*/.Dot -> DOT*), expanding -> False];

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
                         } /. DOT -> des /. des -> DOT;
(* change here in Expand : 24.5.93 *)
    diraccanres = Expand[dotLin[ diraccanres ], diracgamma
                        ] /. pair -> sCO /. sCO->pair;
    diraccanres] ];

DiracOrder[x__] := diracord@@fci[{x}];

diracord[x_]              := FixedPoint[diraccanonical, x, 42];
diracord[x_,y___,z_]      := FixedPoint[diraccanonical,
                              DOT[x,y,z], 42]/;Head[z]=!=List;
diracord[x_,y__,ord_List] := diracord[DOT[x,y],ord];

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
            } /. DOT -> des /. des -> DOT, {diracordi,1,Length[ord]}
      ];
      (Expand[dotLin[diracordres], diracgamma])/.pair->sCO/.sCO->pair]];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracOrder | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracSimpCombine *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DiracSimpCombine`",
             "HighEnergyPhysics`FeynCalc`"];

(* ------------------------------------------------------------------------ *)

DiracSimpCombine::"usage"=
"DiracSimpCombine is an option for DiracSimplify. If set to \
True, sums of DiracGamma's will be merged as much as \
possible in DiracGamma[ .. + .. + ]'s.";

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracSimpCombine | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracSlash *)

(* :Author: Rolf Mertig *)


(* :Summary: DiracSlash  is a Feynman slash *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DiracSlash`",
             "HighEnergyPhysics`FeynCalc`"];

DiracSlash::"usage" =
"DiracSlash[p] is the contraction FourVector[p, mu]*DiracSlash[mu]. \
A product of those can be entered in the form DiracSlash[p1, p2, ..]."

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ Dimension];

fci := fci = MakeContext["FeynCalcInternal"];

MakeContext[ DeclareNonCommutative, DiracGamma, Momentum];

DeclareNonCommutative[DiracSlash];

Options[DiracSlash] = {Dimension -> 4, fci -> True};


DiracSlash[DOT[a_,b__] opt___Rule] := Map[DiracGamma[LorentzIndex[#,
 Dimension /. {opt} /. Options[DiracSlash]],
 Dimension /. {opt} /. Options[DiracSlash]]&, DOT[a,b]];

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

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DiracSigma`",
             "HighEnergyPhysics`FeynCalc`"];

DiracSigma::"usage" =
"DiracSigma[a, b] stands for I/2*(a . b - b . a) in 4 dimensions. \
a and b must have Head DiracGamma, DiracMatrix or DiracSlash. \
Only antisymmetry is implemented.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ DiracGamma, DiracMatrix, DiracSlash];

If[FreeQ[$NonComm, DiracSigma] && Head[$NonComm]===List,
   AppendTo[$NonComm, DiracSigma]];

(* by definition *)
DiracSigma[DOT[a_,b_]] := DiracSigma[a,b];
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

(* :Summary: substitute DiracSigma in terms of DiracGamma's *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DiracSigmaExplicit`",
             "HighEnergyPhysics`FeynCalc`"];

DiracSigmaExplicit::"usage" =
"DiracSigmaExplicit[exp] inserts in exp the definition of \
DiracSigma. DiracSigmaExplict is also an option of \
DiracSimplify.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

fci := fci  = MakeContext["FeynCalcInternal"];
MakeContext[ DiracGamma, DiracMatrix, DiracSigma, DiracSlash];

dirsigex[a_DiracGamma, b_DiracGamma] := dirsigex[a,b] =
I/2 (DOT[a, b] - DOT[b, a]);

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

(* :Summary: Dirac spinors *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DiracSpinor`",
               "HighEnergyPhysics`FeynCalc`"];

DiracSpinor::"usage" =
"DiracSpinor[p, m, ind] is a Dirac spinor for a fermion with momentum p \
and mass m and indices ind.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[DeclareNonCommutative];

DeclareNonCommutative[DiracSpinor];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracSigmaExplicit | \n "]];
Null



(* :Title: DiracSubstitute67 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Dirac chirality projection *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DiracSubstitute67`",
             "HighEnergyPhysics`FeynCalc`"];

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

DiracSubstitute67::"usage"=
"DiracSubstitute67 is an option for DiracSimplify. If set to \
True the chirality-projectors DiracGamma[6] and DiracGamma[7] are \
substituted by their definitions.";

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracSubstitute67 | \n "]];
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

DiracTraceEvaluate::"usage" =
"DiracTraceEvaluate is an option for DiracTrace and Tr.
If set to False, DiracTrace remains unevaluated.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracTraceEvaluate | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Divideout *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Divideout is an option for several functions *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Divideout`",
             "HighEnergyPhysics`FeynCalc`"];

Divideout::"usage" =
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

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`DotPower`",
             "HighEnergyPhysics`FeynCalc`"];

DotPower::"usage" =
"DotPower is an option for DotSimplify. It determines whether
non-commutative powers are represented by successive multiplication
or by Power.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DotPower | \n "]];
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

DummyIndex::"usage" =
"DummyIndex is an option of CovariantD specifying an index to use as \
dummy summation index. If set to Automatic, unique indices are generated."

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DummyIndex | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Eps *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Eps is the head of Levi-Civita tensors *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Eps`",
             "HighEnergyPhysics`FeynCalc`"];

Eps::"usage" =
"Eps[a, b, c, d] represents the totally antisymmetric epsilon
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
(*Added ExplicitLorentzIndex. 21/9-2002. F.Orellana*)
exlor   := exlor = MakeContext["ExplicitLorentzIndex"];
mom   := mom = MakeContext["Momentum"];

Options[Eps] = {dimension -> 4};

Eps[a__Symbol, ru___Rule] := (Signature[{a}] (Eps @@ Sort[{a}])) /;
                             Signature[{a}] =!= 1;

Eps[a__?(MatchQ[#,_Integer|exlor[_Integer]]&), ru___Rule] := Signature[{a}];

Eps[a___, n1_. (lor|exlor)[mu_,___], b___, n2_. (lor|exlor)[mu_,___],c___ ] := 0 /;
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


(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: EpsContract *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Contraction of Eps'es *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`EpsContract`",
             "HighEnergyPhysics`FeynCalc`"];

EpsContract::"usage"=
"EpsContract is an option of Contract specifying whether Levi-Civita
tensors Eps[...] will be contracted, i.e., products
of two Eps are replaced via the determinant formula.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "EpsContract | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: EpsDiscard *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`EpsDiscard`",
             "HighEnergyPhysics`FeynCalc`"];


EpsDiscard::"usage"=
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

(* :Title: Epsilon *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Epsilon is the epsilon in dimensional regularization.
             For QCD  n = 4 + Epsilon 
*) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`Epsilon`",
             "HighEnergyPhysics`FeynCalc`"];

Epsilon::"usage" = 
"Epsilon is (D-4), where D is the number of space-time dimensions. Epsilon \
stands for a small positive number.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[PositiveNumber];

DataType[Epsilon, PositiveNumber] = True;

Unprotect[Greater];
Greater[Re[Epsilon],-4]=True;
Greater[Re[Epsilon],-3]=True;
Greater[Re[Epsilon],-2]=True;
Greater[Re[Epsilon],-1]=True;
Greater[Re[Epsilon],0]=True;

   MakeBoxes[Epsilon^n_Integer?Negative, TraditionalForm] := 
             FractionBox[1,TBox[Epsilon^(-n)]];
   MakeBoxes[Epsilon^(-1),TraditionalForm] := 
             FractionBox[1,TBox[Epsilon]];
   Epsilon /:
   MakeBoxes[Epsilon, TraditionalForm] :=
    TagBox["\[CurlyEpsilon]", TraditionalForm]

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Epsilon | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: EpsilonOrder *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: EpsilonOrder *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`EpsilonOrder`",
             "HighEnergyPhysics`FeynCalc`"];

EpsilonOrder::"usage" =
"EpsilonOrder is an option of OPEIntDelta and RHI. The setting
determines the order n (Epsilon^n) which should be kept.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "EpsilonOrder | \n "]];
Null



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

ExpandScalarProduct::"usage" =
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
(* well, not used, so commented out. F.Orellana, 8/11-2002. *)
(*FRH = FixedPoint[ReleaseHold, #]&;*)

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

(* Catch Pair[LorentzIndex[mu], Momentum[a] + Momentum [b] +...].
   F.Orellana. 26/2-2003 *)
extraMomRule = pair[lorentzindex[a__],
               b : Plus[(___*momentum[__] | momentum[__]),
                        (___*momentum[__] | momentum[__]) ...]]  :>
               (pair[lorentzindex[a], #]& /@ b);

pairexpand1[x_]:=  x /. pair->scevdoit /. extraMomRule;


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



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Expanding *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Expanding *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Expanding`",
             "HighEnergyPhysics`FeynCalc`"];

Expanding::"usage" =
"Expanding is an option for DotSimplify, Calc, Contract, DiracSimplify, SUNSimplify, etc.
As option for Contract it specifies whether expansion w.r.t.
LorentzIndex is done BEFORE contraction. \n
If set to False in DiracSimplify or SUNSimplify,
only a limited set of simplifications
(multiplicative linearity etc.) is
performed. For DotSimplity, it determines
whether noncommutative expansion is done.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Expanding | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Explicit *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 11 March '98 at 19:08 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Explicit *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`Explicit`",
             "HighEnergyPhysics`FeynCalc`"];

Explicit::"usage" = 
"Explicit is an option for FieldStrength, GluonVertex,
SUNF, CovariantFieldDerivative, Twist2GluonOperator and others functions. 
If set to True the full form of the operator is inserted. 
Explicit[exp] inserts explicit expressions of FieldStrength, 
GluonVertex and Twist2GluonOperator in exp. SUNF's are replaced 
by SUNTrace objects.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[ExpandScalarProduct];

FieldStrength := FieldStrength = MakeContext["FieldStrength"];

Explicit[y_] := Block[{gv, t2g, fi, r = y}, 
        If[
           CheckContext["GluonVertex"]
           ,
           gv[x__]  := gv[x] = 
           Expand[ExpandScalarProduct[
           MakeContext["GluonVertex"][x, Explicit->True]]];
           r = r /. MakeContext["GluonVertex"] -> gv
          ];
        If[
           CheckContext["Twist2GluonOperator"],
           t2g[x__] := t2g[x] = 
           MakeContext["Twist2GluonOperator"][x, Explicit->True];
           r = r /. MakeContext["Twist2GluonOperator"] -> t2g 
          ];
        If[
           CheckContext["FieldStrength"]
           ,
           fi[x__] := 
           MakeContext["FieldStrength"][x, Explicit->True];
           r = r /. MakeContext["FieldStrength"] -> fi 
          ];
                   r];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Explicit | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ExplicitLorentzIndex *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created ? *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Lorentz indices of integers *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`ExplicitLorentzIndex`",
             "HighEnergyPhysics`FeynCalc`"];

ExplicitLorentzIndex::"usage"=
"ExplicitLorentzIndex[ind] is an explicit Lorentz index, i.e., ind is
an integer.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

(*Commented out 18/6-2002 by F.Orellana. Don't know why it was there - it causes infinite recursion*)
(*MakeContext[ExplicitLorentzIndex];*)

SetAttributes[ExplicitLorentzIndex, Constant ];

ExplicitLorentzIndex /:
   MakeBoxes[ ExplicitLorentzIndex[p_, in___], TraditionalForm
            ] := p;

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ExplicitLorentzIndex | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ExplicitSUNIndex *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 30 October '98 at 12:01 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Head for SUN-Indices *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`ExplicitSUNIndex`",
             "HighEnergyPhysics`FeynCalc`"];

ExplicitSUNIndex::"usage"=
"ExplicitSUNIndex[ind] is a specific SU(N) index, i.e., 
ind is an integer.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

SetAttributes[ExplicitSUNIndex, {Constant, Flat, OneIdentity}];

SUNIndex = MakeContext["SUNIndex"];

ExplicitSUNIndex/:
SUNIndex[i_ExplicitSUNIndex]:= ExplicitSUNIndex[i];

   ExplicitSUNIndex /:
   MakeBoxes[ ExplicitSUNIndex[p_], TraditionalForm
            ] := p;

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ExplicitSUNIndex | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ExtraFactor *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: ExtraFactor *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`ExtraFactor`",
             "HighEnergyPhysics`FeynCalc`"];

ExtraFactor::"usage"=
"ExtraFactor is an option for SquareAmplitude and FermionSpinSum.
The setting ExtraFactor -> fa  multiplies the whole amplitude with the
factor fa before squaring.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ExtraFactor | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FAD*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FAD`",
             "HighEnergyPhysics`FeynCalc`"];

FAD::"usage"= "FAD[q, q-p, ...] denotes 1/(q^2 (q-p)^2 ...).
FAD[{q1,m}, {q1-p,m}, q2, ...] is
1/( (q1^2 - m^2) ( (q1-p)^2 - m^2 ) q2^2 ... ).
(Translation into FeynCalc internal form is performed by
FeynCalcInternal.)";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[Dimension];

Options[FAD] = {Dimension -> D};



ff[{y_,z_}] := SequenceForm["[",y^2, "-", z^2,"]"];

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

FeynCalcForm::"usage"=
"FeynCalcForm[expr] changes the printed output to a an easy to read
form. Whether the result of FeynCalcForm[expr] is displayed
or not, depends on the setting of $PrePrint.
$PrePrint = FeynCalcForm forces displaying everything
after applying FeynCalcForm. In order to change to the normal
(internal) Mathematica OutputForm, do: ($PrePrint=.).";

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
PlusDistribution := PlusDistribution =
                                MakeContext["PlusDistribution"];
RHO          := RHO           = MakeContext["RHO"];
RHI          := RHI           = MakeContext["RHI"];
scalarproduct:= scalarproduct = MakeContext["ScalarProduct"];
Sn           := Sn            = MakeContext["Sn"];
spinor       := spinor        = MakeContext["Spinor"];
sundelta     := sundelta      = MakeContext["SUNDelta"];
sund         := sund          = MakeContext["SUND"];
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
   If[$PrePrint === FeynCalcForm,
      If[MemberQ[{TraditionalForm, StandardForm, InputForm},
                  "Output" /. (
                  cdf /.
                  Options[$FrontEnd, "CommonDefaultFormatTypes"])
                ]
         ,
         Unset[$PrePrint]; (*Print["UNSET"]; *)x
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
   (*Format[fcdot2[a_,b__]] := Infix[fcdot2[a,b], " ", 320];*)
   (*Not an allowed syntax in mma 4.1. F.Orellana*)
     Format[fcdot2[a_,b__]] := Infix[fcdot2[a,b], " "];
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
         DOT:>fcdot /.
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
               DOT["v"[-p/.momentum->iDentity], a],
             fcdot[spinor[p_, 0, ___], a__]  :>
               DOT["u"[p/.momentum->iDentity], a],
             fcdot[a__,spinor[-p_, 0, ___] ] :>
               DOT["v"[-p/.momentum->iDentity], a],
             fcdot[a__, spinor[p_, 0, ___]]  :>
               DOT[a, "u"[p/.momentum->iDentity]]
            }, {}
           ]/.
         If[CC["Spinor"],
            {
             fcdot[spinor[-p_, mas_, _], a__] :>
               DOT["v"[-p/.momentum->iDentity,mas], a],
             fcdot[spinor[p_, mas_, _], a__]  :>
               DOT["u"[p/.momentum->iDentity,mas], a],
             fcdot[a__,spinor[-p_, mas_, _] ] :>
               DOT[a, "v"[-p/.momentum->iDentity,mas]],
             fcdot[a__, spinor[p_, mas_, _]]  :>
               DOT[a, "u"[p/.momentum->iDentity,mas]]
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
        fcdot:>fcdot2/. (*fcdot2 -> DOT /.*)
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

(* :Title: FinalSubstitutions *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: option for several functions *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FinalSubstitutions`",
             "HighEnergyPhysics`FeynCalc`"];

FinalSubstitutions::"usage" =
"FinalSubstitutions is an option for OneLoop, OneLoopSum,
Write2, FeynCalcExternal and FeynCalcInternal. All substitutions indicated hereby are done at the
end of the calculation.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FinalSubstitutions | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FORM *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: FORM *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FORM`",
             "HighEnergyPhysics`FeynCalc`"];

FORM::"usage" =
"FORM is a bolean option telling FeynCalc whether or not to use FORM for
evaluation. If set to True a FORM file is generated and run from Mathematica
and the result read back in. Currently, only RHI has this option and it is
required to be on a UNIX system and have R. Hamberg's FORM-program installed
correctly.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FORM | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FV *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FV`",
             "HighEnergyPhysics`FeynCalc`"];

FV::"usage"= "FV[p,mu] is a fourvector and is transformed into
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



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FVD *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FVD`",
             "HighEnergyPhysics`FeynCalc`"];

FVD::"usage"= "FVD[p,mu] is a fourvector and is
transformed into Pair[Momentum[p,D], LorentzIndex[mu,D]]
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

FactorFull::"usage"=
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

FactorTime::"usage"=
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

Factoring::"usage" = "Factoring is an option for Collect2, Contract,
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

Factorout::"usage" = "Factorout is an option for OPEInt and OPEIntegrate.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Factorout | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCIntegrate *)

(* :Author: Frederik Orellana *)

(* ------------------------------------------------------------------------ *)
(* :History: created 17 April 2001 at 13:52 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  Feynman integration *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FCIntegrate`",
             "HighEnergyPhysics`FeynCalc`"];

FCIntegrate::"usage"=
"FCIntegrate is an option of certain Feynman integral related functions. \
It determines which integration function is used to evaluate analytic \
integrals. Possible settings include Integrate, NIntegrate,
(DOT[Integratedx@@#2, #1] &).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FCIntegrate | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCNIntegrate *)

(* :Author: Frederik Orellana *)

(* ------------------------------------------------------------------------ *)
(* :History: created 17 April 2001 at 13:52 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  Feynman numerical integration *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FCNIntegrate`",
             "HighEnergyPhysics`FeynCalc`"];

FCNIntegrate::"usage"=
"FCNIntegrate is an option of certain Feynman integral related functions \
which may return output containing both integrals that can be evaluated \
and integrals that can only be evaluated numerically. \
It then determines which integration function is used to evaluate numeric \
integrals. Possible settings include NIntegrate, (0*#1)&, \
(DOT[Integratedx@@#2, #1] &).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FCNIntegrate | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FeynAmp *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Feynman amplitudes *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FeynAmp`",
             "HighEnergyPhysics`FeynCalc`"];

FeynAmp::"usage"=
"FeynAmp[q, amp] denotes a Feynman amplitude.
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

(* :Title: FeynAmpDenominator *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 16 August '97 at 18:22 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Propagators *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FeynAmpDenominator`",
             "HighEnergyPhysics`FeynCalc`"];

FeynAmpDenominator::"usage" =
"FeynAmpDenominator[ PropagatorDenominator[ ... ],
PropagatorDenominator[ ... ], ... ] represents
the inverse denominators of the propagators, i.e. FeynAmpDenominator[x]
is 1/x .";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ FAD, FeynCalcInternal, Momentum];

FeynAmpDenominator[ar__List] := FeynAmpDenominator[ar] =
FeynCalcInternal[FAD[ar]];

    MakeBoxes[f_. FeynAmpDenominator[a__], TraditionalForm
             ] := (MakeBoxes[#,TraditionalForm]&)@@{f/ Apply[DOT,
                   Map[( (#[[1]]/.Momentum[aa_,___]:>aa)^2 -
                          #[[2]]^2 )&, {a}
                      ]
                                  ]}

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FeynAmpDenominator | \n "]];
Null



(* :Title: FeynAmpList *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created ? *)
(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FeynAmpList`",
             "HighEnergyPhysics`FeynCalc`"];

FeynAmpList::"usage"=
"FeynAmpList[info][FeynAmp[...], FeynAmp[...], ...] is a head of a list of
Feynman amplitudes.";

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FeynAmpList | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FeynmanParameterNames *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FeynmanParameterNames`",
             "HighEnergyPhysics`FeynCalc`"];

FeynmanParameterNames::"usage"=
"FeynmanParameterNames is an option for FeynmanParametrize and \
FeynmanParametrize.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FeynmanParameterNames | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FourVector *)

(* :Author: Rolf Mertig *)


(* :Summary: FourVector *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`FourVector`",
               "HighEnergyPhysics`FeynCalc`"];

FourVector::"usage" =
"FourVector[p, mu] is the four Dimensional vector p with Lorentz index m.
A vector with space-time Dimension d is obtained by supplying the option
Dimension->d."

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

fci := fci = MakeContext["FeynCalcInternal"];

MakeContext[ Dimension, LorentzIndex, Momentum, Pair];

Options[FourVector]  = {Dimension -> 4, fci -> True};

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

FreeIndex::"usage"=
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

FreeQ2::"usage" =
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

(* :Title: GA *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 8 December '98 at 12:57 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`GA`",
             "HighEnergyPhysics`FeynCalc`"];

GA::"usage"=
"GA[mu] can be used as input for gamma_mu and is
transformed into DiracMatrix[mu] by FeynCalcInternal.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext["DeclareNonCommutative"][GA];

GA[DOT[x_,y__]] := Map[GA,DOT[x,y]];
GA[x_, y__] := DOT @@ Map[GA,{x,y}];

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

GA5::"usage"=
"GA5 is equivalent to DiracGamma[5] and denotes gamma5.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[DiracGamma];

GA5 = DiracGamma[5];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "GA5 | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: GAD *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`GAD`",
             "HighEnergyPhysics`FeynCalc`"];

GAD::"usage"=
"GAD[mu] can be used as input for a D-dimensional gamma_mu and is
transformed into DiracMatrix[mu, Dimension->D] by FeynCalcInternal.";



(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

GAD[DOT[x_,y__]] := Map[GAD, DOT[x,y]];
GAD[x_, y__] := DOT @@ Map[GAD,{x,y}];

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

(* :Title: GS *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 8 December '98 at 12:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`GS`",
             "HighEnergyPhysics`FeynCalc`"];

GS::"usage"=
"GS[p] is transformed into DiracSlash[p] by FeynCalcInternal.
GS[p,q, ...] is equivalent to GS[p].GS[q]. ...";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext["DeclareNonCommutative"][GS];

GS[DOT[x_,y__]] := Map[GS,DOT[x,y]];

GS[x_, y__] := DOT @@ Map[GS,{x,y}];

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

GSD::"usage"=
"GSD[p] is transformed into DiracSlash[p,Dimension->D] by FeynCalcInternal.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext["DeclareNonCommutative"][GSD];

GSD[DOT[x_,y__]] := Map[GSD, DOT[x,y]];
GSD[x_, y__] := DOT @@ Map[GSD, {x, y}];

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

(* :Title: GammaExpand *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`GammaExpand`",
             "HighEnergyPhysics`FeynCalc`"];

GammaExpand::"usage"= "GammaExpand[exp] rewrites
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

Gauge::"usage" =
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

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created ? *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Gauge field *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`GaugeField`",
             "HighEnergyPhysics`FeynCalc`"];

GaugeField::"usage" =
"GaugeField is a name of a gauge field.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

   GaugeField /: MakeBoxes[GaugeField, TraditionalForm] := "A";

End[]; MyEndPackage[];
If[$VeryVerbose > 0,WriteString["stdout", "GaugeField | \n "]];
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

GaugeXi::"usage"= "GaugeXi is a head for gauge parameters.";

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

(* :Title: GluonField *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created ? *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Gluon field *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`GluonField`",
             "HighEnergyPhysics`FeynCalc`"];

GluonField::"usage" =
"GluonField is a name of a gauge field.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

   GluonField /: MakeBoxes[GluonField, TraditionalForm] := "A";

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "GluonField | \n "]];
Null



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


GrassmannParity::"usage" =  "GrassmannParity is a data type.
E.g. DataType[F, GrassmannParity] = 1 declares F to be of
bosonic type and DataType[F, GrassmannParity] = -1 of fermionic
one.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"]


End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "GrassmannParity | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Gstrong *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: The strong coupling constant *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Gstrong`",
             "HighEnergyPhysics`FeynCalc`"];

Gstrong::"usage" =
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

(* :Title: IFPD *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Inverse propagator *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`IFPD`",
             "HighEnergyPhysics`FeynCalc`"];

IFPD::"usage" =
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

(* :Title: IncludePair *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: IncludePair *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`IncludePair`",
             "HighEnergyPhysics`FeynCalc`"];

IncludePair::"usage" =
"IncludePair is an option for FC2RHI, FC2TLI and FeynAmpDenominatorSimplify.
Possible settings are True and False.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "IncludePair | \n "]];
Null


(******* Seems not to be in use. F.Orellana, 8/9-2002 ********)
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Indices *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

(*MyBeginPackage["HighEnergyPhysics`FeynCalc`Indices`",
             "HighEnergyPhysics`FeynCalc`"];

Indices::"usage"= "Indices is an option for FORM2FeynCalc. Its default
setting is Automatic. It may be set to a list, if the FORM-file does
not contain a I(ndices) statement.";*)

(* ------------------------------------------------------------------------ *)

(*Begin["`Private`"];

End[]; MyEndPackage[];*)
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(*If[$VeryVerbose > 0,WriteString["stdout", "Indices | \n "]];
Null*)



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: InitialFunction *)

(* :Author: Frederik Orellana *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 3 August 2000 at 22:36 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`InitialFunction`",
             "HighEnergyPhysics`FeynCalc`"];

InitialFunction::"usage"= 
"InitialFunction is an option of FeynRule the setting of which is applied to \
the first argument of FeynRule before anything else";

EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

If[$VeryVerbose > 0,WriteString["stdout", "InitialFunction | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: InitialSubstitutions *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: option for several functions *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`InitialSubstitutions`",
             "HighEnergyPhysics`FeynCalc`"];

InitialSubstitutions::"usage" =
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

(* :Title: IntegralTable *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`IntegralTable`",
             "HighEnergyPhysics`FeynCalc`"];

IntegralTable::"usage"=
"IntegralTable is an option of OneLoopSimplify, TwoLoopSimplify and
FeynAmpDenominatorSimplify.
It may be set to a list of the form :
{FCIntegral[ ... ] :> bla, ...}.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "IntegralTable| \n "]];
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

Integratedx::"usage"=
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

IntermediateSubstitutions::"usage" =
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

(* :Title: IsolateNames *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: option for several functions *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`IsolateNames`",
             "HighEnergyPhysics`FeynCalc`"];

IsolateNames::"usage" =
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

(* :Title: KK *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: KK *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`KK`",
             "HighEnergyPhysics`FeynCalc`"];

KK::"usage" =
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

(* :Title: KeepOnly *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`KeepOnly`",
             "HighEnergyPhysics`FeynCalc`"];

KeepOnly::"usage"=
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



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: LC *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`LC`",
             "HighEnergyPhysics`FeynCalc`"];

LC::"usage"=
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

LCD::"usage"=
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

(* :Title: LeftPartialD *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 11 March '98 at 22:04 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: partial derivative *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`LeftPartialD`",
             "HighEnergyPhysics`FeynCalc`"];

LeftPartialD::"usage"=
"LeftPartialD[mu] denotes partial_mu, acting to the left.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ Commutator, DeclareNonCommutative,
    FreeQ2, LorentzIndex, Momentum, OPEDelta, RightPartialD];
(*Bug fix, 30/1-2003. F.Orellana*)
DeclareNonCommutative[LeftPartialD];
(* ******************************************************************** *)

LeftPartialD[xx__] := LeftPartialD @@ (LorentzIndex /@ {xx}) /;
		 FreeQ2[{xx}, {LorentzIndex, Momentum, OPEDelta, RowBox,
			       Pattern, Blank}] && (Union[{xx}]=!={1});

LeftPartialD[(1)..] = 1;
LeftPartialD[c:OPEDelta..] := LeftPartialD @@ (Momentum /@ {c});
LeftPartialD[x_LorentzIndex, y__LorentzIndex] :=
          DOT @@ Map[LeftPartialD, {x, y}];
LeftPartialD[x_Momentum, y__Momentum] := DOT @@ Map[LeftPartialD, {x, y}];

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

LeftRightPartialD::"usage"=
"LeftRightPartialD[mu] denotes partial_mu, acting to the left and
right. ExplicitPartialD[LeftRightPartialD[mu]] gives
1/2 (RightPartialD[mu] - LeftPartialD[mu]).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[DeclareNonCommutative,
            FreeQ2, Momentum, LorentzIndex, OPEDelta];

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

LeftRightPartialD2::"usage"=
"LeftRightPartialD2[mu] denotes partial_mu, acting to the left and
right. ExplicitPartialD[LeftRightPartialD2[mu]] gives
(RightPartialD[mu] + LeftPartialD[mu]).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[DeclareNonCommutative,
       FreeQ2, Momentum, LorentzIndex, OPEDelta];

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

(* :Title: LeviCivita *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: LeviCivita *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`LeviCivita`",
             "HighEnergyPhysics`FeynCalc`"];

LeviCivita::"usage" =
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
             ];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "LeviCivita | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: LeviCivitaSign *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Option  for DiracTrace and Tr *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`LeviCivitaSign`",
             "HighEnergyPhysics`FeynCalc`"];

LeviCivitaSign::"usage" =
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

Loop::"usage"= "Loop is an option indicating the number of (virtual) loops.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Loop | \n "]];
Null



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

LorentzIndex::"usage"= "LorentzIndex is the head of Lorentz indices.
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



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Lower *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Lower`",
             "HighEnergyPhysics`FeynCalc`"];

Lower::"usage"= "Lower may be used inside LorentzIndex to indicate an
covariant LorentzIndex.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Lower | \n "]];
Null



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

MT::"usage"=
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

MTD::"usage"=
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



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Mandelstam *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Option for several functions *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Mandelstam`",
             "HighEnergyPhysics`FeynCalc`"];

Mandelstam::"usage" =
"Mandelstam is an option for DiracTrace, OneLoop, OneLoopSum, Tr
and TrickMandelstam.  A typical setting is
Mandelstam -> {s, t, u, m1^2 + m2^2 + m3^2 + m4^2},
which stands for  s + t + u = m1^2 + m2^2 + m3^2 +  m4^2.
If other than four-particle processes are calculated the
setting should be: Mandelstam -> {}.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Mandelstam | \n "]];
Null



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

MemSet::"usage" =
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

MemSet[x_,y_, ops___Rule] :=
If[((MemoryAvailable/.{ops} /. Options[MemSet]) -
      MemoryInUse[]/1000000.) <1.,
    y, 
    Set[x, y]
  ];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "MemSet | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: MemoryAvailable *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: MemoryAvailable *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`MemoryAvailable`",
             "HighEnergyPhysics`FeynCalc`"];

MemoryAvailable::"usage" =
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

(* :Title: MetricTensor *)

(* :Author: Rolf Mertig *)


(* :Summary: *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`MetricTensor`",
               "HighEnergyPhysics`FeynCalc`"];

MetricTensor::"usage"=
"MetricTensor[mu, nu] is the metric tensor in 4 dimensions.
The metric tensor in d dimensions is obtained by supplying the
option Dimension->d.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

fci:= fci = MakeContext["FeynCalcInternal"];

MakeContext[Dimension, LorentzIndex, Pair];

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
If[$VeryVerbose > 0,WriteString["stdout", "MetricTensor | \n "]];
Null



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

Momentum::"usage"=
"Momentum is the head of a four momentum (p).
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

(* :Title: MomentumCombine *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: MomentumCombine *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`MomentumCombine`",
             "HighEnergyPhysics`FeynCalc`"];

MomentumCombine::"usage" =
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

MomentumCombine2::"usage" =
"MomentumCombine2[expr]  is the inverse operation to
MomentumExpand and ExpandScalarProduct.
MomentumCombine2 combines also
FourVectors.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ FreeQ2, LorentzIndex, Momentum, MomentumExpand, Pair];

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

MomentumExpand::"usage" =
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

MomentumExpand[x_] := x /. Momentum -> fourvecev;

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "MomentumExpand | \n "]];
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

NegativeInteger::"usage" =  "NegativeInteger is a data type.
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

Nf::"usage" =
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

(* :Title: NonCommFreeQ *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: test for non-commutativity *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`NonCommFreeQ`",
             "HighEnergyPhysics`FeynCalc`"];

NonCommFreeQ::"usage" =
"NonCommFreeQ[exp] yields True if exp contains no non-commutative objects \
(i.e. those objects which are listed in $NonComm) or only non-commutative \
objects inside DiracTrace's or SUNTrace's.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ DiracTrace, FreeQ2, SUNTrace, MemSet];

(* Have traces treated as commutating objects. F.Orellana, 11/9-2002. *)
excludeTraces = a : (DiracTrace |  SUNTrace)[__] :>
                (a /. (Rule[#, ToString[#]] & /@
                {DiracTrace, SUNTrace, Sequence @@ $NonComm}));

NonCommFreeQ[x_?NumberQ]   := True;
NonCommFreeQ[x_]           := MemSet[NonCommFreeQ[x],
                              FreeQ2[x /. excludeTraces, $NonComm]];

(*Comment: Because of this MemSet, NonCommFreeQ is updated when setting a new
  DataType[x,NonCommutative]=True or DataType[x,NonCommutative]=False.
  Rolf implemented it this way to gain speed I suppose. F.Orellana*)

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

NonCommQ::"usage" =
"NonCommQ[exp] yields True if exp contains non-commutative objects \
(i.e. those objects which are listed in $NonComm) not inside \
DiracTrace's or SUNTrace's.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ DiracTrace, FreeQ2, SUNTrace, MemSet];

(* Have traces treated as commutating objects. F.Orellana, 11/9-2002. *)
excludeTraces = a : (DiracTrace |  SUNTrace)[__] :>
                (a /. (Rule[#, ToString[#]] & /@
                {DiracTrace, SUNTrace, Sequence @@ $NonComm}));

NonCommQ[x_?NumberQ]   := False; (*True*)(*Should be False?? 11/9-2002 , F.Orellana.*)
NonCommQ[x_]           := MemSet[NonCommQ[x],
                          FreeQ2[x /. excludeTraces, $NonComm]=!=True];
                       (*The =!= above put in 11/9-2002, F.Orellana.*)

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "NonCommQ | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: NonCommutative *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: NonCommutative *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`NonCommutative`",
             "HighEnergyPhysics`FeynCalc`"];

NonCommutative::"usage"=
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

NumberOfMetricTensors::"usage"=
"NumberOfMetricTensors is an option of Tdec.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "NumberOfMetricTensors | \n "]];
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

OPE::"usage"= "OPE is used internally in OPE1Loop.";

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

PaVeOrderList::"usage"=
"PaVeOrderList is an option for PaVeOrder and PaVeReduce,
specifying in which order the arguments of D0 are to be permuted.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PaVeOrderList | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Pair *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 20 December '98 at 23:59 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: The head of four-vectors, metric tensor and
             scalar products. *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Pair`",
             "HighEnergyPhysics`FeynCalc`"];

Pair::"usage"=
"Pair[a , b] is a special pairing used in the internal
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
(HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex|
HighEnergyPhysics`FeynCalc`ExplicitLorentzIndex`ExplicitLorentzIndex)[a_,___],
(HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex|
HighEnergyPhysics`FeynCalc`ExplicitLorentzIndex`ExplicitLorentzIndex)[b_,___] ],
             TraditionalForm
            ] := SuperscriptBox["g",Tbox[a,b] ];

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
      (HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex|
      HighEnergyPhysics`FeynCalc`ExplicitLorentzIndex`ExplicitLorentzIndex)[a__],
      HighEnergyPhysics`FeynCalc`Momentum`Momentum[
      HighEnergyPhysics`FeynCalc`Polarization`Polarization[
                              b_,Complex[0,1]],___]
                 ], TraditionalForm
            ] := RowBox[{
        SubscriptBox["\[CurlyEpsilon]",
                     Tbox[LorentzIndex[a]]],
                     "(",Tbox[b],")"}];

Pair /:
   MakeBoxes[Pair[
      (HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex|
      HighEnergyPhysics`FeynCalc`ExplicitLorentzIndex`ExplicitLorentzIndex)[a__],
      HighEnergyPhysics`FeynCalc`Momentum`Momentum[
      HighEnergyPhysics`FeynCalc`Polarization`Polarization[
                              b_,Complex[0,-1]],___]
                 ], TraditionalForm
            ] := RowBox[{
        SubsuperscriptBox["\[CurlyEpsilon]",
                          Tbox[LorentzIndex[a]], "*"
                          ], "(", Tbox[b], ")"
                        }
                       ];

Pair /:
   MakeBoxes[Pair[
              (HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex|
      HighEnergyPhysics`FeynCalc`ExplicitLorentzIndex`ExplicitLorentzIndex)[a__],
              HighEnergyPhysics`FeynCalc`Momentum`Momentum[
                   b_Subscripted, di___]
                 ], TraditionalForm
            ] := SubsuperscriptBox[Tbox[b[[1,0]]],
                                   Tbox@@b[[1]],
                                    Tbox[LorentzIndex[a]]];

Pair /:
   MakeBoxes[Pair[
              (HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex|
      HighEnergyPhysics`FeynCalc`ExplicitLorentzIndex`ExplicitLorentzIndex)[a__],
              HighEnergyPhysics`FeynCalc`Momentum`Momentum[
                   b_Subscript,di___]
                 ], TraditionalForm
            ] := SubsuperscriptBox[Tbox[b[[1]]], Tbox@@Rest[b],
                                    Tbox[LorentzIndex[a]]];

Pair /:
   MakeBoxes[Pair[
              (HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex|
      HighEnergyPhysics`FeynCalc`ExplicitLorentzIndex`ExplicitLorentzIndex)[a__],
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



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PairCollect *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: PairCollect is an option for several functions *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`PairCollect`",
             "HighEnergyPhysics`FeynCalc`"];

PairCollect::"usage" =
"PairCollect is an option for DiracTrace specifying if
the result is collected with respect to Pair's.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PairCollect | \n "]];
Null



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

PairContract::"usage" =
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
   DOT[A___, HoldPattern[PairContract[lor_[z_,___],x_]], B___,
                 m_. f_[a__], c___ ] :=
 DOT[A,B,(m f[a]/.LorentzIndex[z,___]->x),c]/;
    ((!FreeQ[f[a], LorentzIndex[z,___]]) && (lor === LorentzIndex));

PairContract/: DOT[A___, m_. f_[a__], B___,
                   HoldPattern[PairContract[lor_[z_,___],x_]], c___ ] :=
 DOT[A.(m f[a]/.LorentzIndex[z,___]->x),B,c]/;
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

PairContract2::"usage" =
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

PairContract3::"usage" =
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

PartialD::"usage"=
"PartialD[mu] denotes partial_mu. PartialD[x, mu] denotes d/d x^mu.
The first one acts on QuantumField[f], the second on QuantumField[f][x],
where f is some field name and x is a space-time variable.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[FreeQ2, LorentzIndex, Momentum, OPEDelta,
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

PartialD /:
   MakeBoxes[PartialD[x_ ^n_], TraditionalForm] :=
    SubsuperscriptBox["\[PartialD]", Tbox[x], Tbox[n]
                     ] /; Head[x] === Momentum;

PartialD /:
   MakeBoxes[ PartialD[x_], TraditionalForm] :=
    SubscriptBox["\[PartialD]", ToBoxes[x,TraditionalForm]];

PartialD /:
   MakeBoxes[ PartialD[x_, HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex[mu__]], TraditionalForm] :=
    RowBox[{"\[PartialD]", "/", "\[PartialD]",
            SuperscriptBox[ToBoxes[x,TraditionalForm], ToBoxes[LorentzIndex[mu],TraditionalForm]]
            }];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PartialD | \n "]];
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

PauliSigma::"usage" =
"PauliSigma denotes the vector of the 3 Pauli matrices.
PauliSigma[1], PauliSigma[2], PauliSigma[3] give the
explicit Pauli matrices. PauliSigma[] yields
{PauliSigma[1], PauliSigma[2], PauliSigma[3]}.";

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



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

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

PlusDistribution::"usage"=
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

(* :Title: Polarization *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 30 January '99 at 21:45 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Head for polarization vectors *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Polarization`",
             "HighEnergyPhysics`FeynCalc`"];

Polarization::"usage"=
"Polarization[k] = Polarization[k, I] represents a
polarization momentum with (incoming) momentum k.
A slashed polarization vector (e1(k) slash) has to be entered
as DiracSlash[Polarization[k]].
The internal representation for a polarization vector e1
corresponding to a boson with four momentum k is:
Momentum[ Polarization[ k, I ] ].
With this notation transversality of polarization vectors is
provided, i.e.  Pair[ Momentum[k],
Momentum[ Polarization[k, I] ] ] yields 0.
Polarization[k,-I] denotes the complex conjugate polarization.\n
Polarization is also an option.
The setting 0 denotes the unpolarized and 1 the polarized case.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[Pair];

(* by convention *)
Polarization[k_] /;FreeQ[k,Blank|BlankSequence|BlankNullSequence] :=
  Polarization[k] = Polarization[k, I];

Polarization[k_] /;FreeQ[k,Blank|BlankSequence|BlankNullSequence] :=
  Polarization[k] = Polarization[k, I];

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


PolarizationVector::"usage" =
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
(PolarizationVector[x]=polVec[x] )/; FreeQ[{x}, Pattern] &&
(*Hack to keep unevaluated when given "FeynArts arguments". F.Orellana, 29/3-2003*)
  (Length[{x}]===2 ||
  (*FA uses particle name (which is alway not AtomQ) as first argument*)
  AtomQ[{x}[[1]]] || 
  Head[{x}[[-1]]===sunindex]);

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


PositiveInteger::"usage" =  "PositiveInteger is a data type.
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


PositiveNumber::"usage" =  "PositiveNumber is a data type.
E.g. DataType[Epsilon, PositiveNumber] = True (by default). ";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"]

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PositiveNumber | \n "]];
Null



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

PropagatorDenominator::"usage" =
"PropagatorDenominator[Momentum[q], m] is a factor of the denominator of a
propagator.  If q is supposed to be D-dimensional enter:
PropagatorDenominator[Momentum[q, D], m].  What is meant is
1/(q^2-m^2).
PropagatorDenominator[p] evaluates to PropagatorDenominator[p,0].";

PD::"usage" =
"PD is an abbreviation for PropagatorDenominator.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[FreeQ2];

PropagatorDenominator[a_ /; FreeQ2[a, {BlankNullSequence,Pattern}]
                     ] := PropagatorDenominator[a, 0];

PropagatorDenominator/:
   MakeBoxes[PropagatorDenominator[a_, 0], TraditionalForm
            ] := ToBoxes[1/a^2, TraditionalForm];

   MakeBoxes[f_. PropagatorDenominator[a_, b_/;b=!=0], TraditionalForm
            ] := ToBoxes[f/(a^2-b^2), TraditionalForm];

PD = PropagatorDenominator;

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

QuantumField::"usage"=
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
                   (lo === LorentzIndex || lo === ExplicitLorentzIndex);

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
 (lori___HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex|
 lori___HighEnergyPhysics`FeynCalc`ExplicitLorentzIndex`ExplicitLorentzIndex),
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
 (lori___HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex|
 lori___HighEnergyPhysics`FeynCalc`ExplicitLorentzIndex`ExplicitLorentzIndex),
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


(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: QuarkField *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: quark field *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`QuarkField`",
             "HighEnergyPhysics`FeynCalc`"];

QuarkField::"usage" =
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

QuarkMass::"usage"= "QuarkMass is an option of Amplitude.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "QuarkMass | \n "]];
Null



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

Rename::"usage" =
"Rename is an option for Contract. If set to True,
dummy indices in Eps are renamed, using $MU[i].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Rename | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: RightPartialD *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 17 October '97 at 15:40 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: partial derivative *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`RightPartialD`",
             "HighEnergyPhysics`FeynCalc`"];

RightPartialD::"usage"=
"RightPartialD[mu] denotes partial_mu, acting to the right.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

FreeQ2       = MakeContext["FreeQ2"];
LorentzIndex = MakeContext["LorentzIndex"];
Momentum     = MakeContext["Momentum"];
OPEDelta     = MakeContext["OPEDelta"];

DeclareNonCommutative = MakeContext["DeclareNonCommutative"];
DeclareNonCommutative[RightPartialD];

(* ******************************************************************** *)

RightPartialD[xx__] := RightPartialD @@ (LorentzIndex /@ {xx}) /; 
		 FreeQ2[{xx}, {LorentzIndex, Momentum, OPEDelta, RowBox, 
			       Pattern, Blank}] && (Union[{xx}]=!={1});

RightPartialD[(1)..] = 1;
RightPartialD[c:OPEDelta..] := RightPartialD @@ (Momentum /@ {c});
RightPartialD[x_LorentzIndex, y__LorentzIndex] := 
          DOT @@ Map[RightPartialD, {x, y}];
RightPartialD[x_Momentum, y__Momentum] := DOT @@ Map[RightPartialD, {x, y}];

(*
RightPartialD[Momentum[OPEDelta]^n_Integer?Positive] :=
       DOT @@ Map[RightPartialD, Table[Momentum[OPEDelta],{n}]];
*)
   RightPartialD /:
   MakeBoxes[RightPartialD[x_ ^n_],TraditionalForm] :=
    SubsuperscriptBox[RowBox[{
      OverscriptBox["\[PartialD]", "\[RightArrow]"]}], Tbox[" ",x],Tbox[n]
                     ] /; Head[x] === Momentum;

   RightPartialD /:
   MakeBoxes[RightPartialD[x_] ,TraditionalForm] :=
    SubscriptBox[RowBox[{
      OverscriptBox["\[PartialD]", "\[RightArrow]"]}], Tbox[x]];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "RightPartialD | \n "]];
Null


(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ScaleMu *)

(* :Author: Frederik Orellana *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 7 September 2002 at 19:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Dimensional regularization scale *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`ScaleMu`",
             "HighEnergyPhysics`FeynCalc`"];

ScaleMu::"usage"= "ScaleMu is the mass scale used for dimensional \
regularization of loop integrals";


(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

ScaleMu /:
MakeBoxes[ScaleMu, TraditionalForm] := "\[Mu]";

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ScaleMu | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SD *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Kronecker delta for SU(N) *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SD`",
             "HighEnergyPhysics`FeynCalc`"];

SD::"usage"=
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

(* :Title: SO *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 8 February '98 at 1:17 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SO`",
             "HighEnergyPhysics`FeynCalc`"];

SO::"usage"=
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

(* :Title: SOD *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 8 February '98 at 1:16 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SOD`",
             "HighEnergyPhysics`FeynCalc`"];

SOD::"usage"= "SOD[q] stands for the D-dimensional scalar product of
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

(* :Title: SP *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SP`",
             "HighEnergyPhysics`FeynCalc`"];

SP::"usage"= "SP[p,q] is the four-dimensional scalar product of p with q.
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

(* :Title: SPD *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SPD`",
             "HighEnergyPhysics`FeynCalc`"];

SPD::"usage"= "SPD[p, q] is the D-dimensional scalar product of p with q.
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

SUND::"usage"=
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


SUNDelta::"usage"=
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
If[$VeryVerbose > 0,WriteString["stdout", "SUNDelta | \n "]];
Null



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

SUNDeltaContract::"usage"=
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
             ( y[z] /. i -> j ) /; (*Added SumOver stuff. F.Orellana. 20/8-2002*)
               FreeQ[y[z], _HighEnergyPhysics`FeynArts`SumOver] &&
               !FreeQ[y[z]//Hold, i] &&
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

SUNF::"usage"=
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
(* Four arguments are now allowed. SMQCD.mod uses that. F.Orellana, 20/8-2002 *)
HoldPattern[SUNF[a___, x_, b___, x_, c___, ___Rule]] := 0 /;
         (Head[x] === sunindex) && FreeQ[x, Pattern] &&
          Length[{a,x,b,x,c}] == 3;
HoldPattern[SUNF[a___, x_, y_, b___, ___Rule]] := -SUNF[a, y, x, b] /;
FreeQ[{a,x,y,b}, Pattern] && Length[{a,x,y,b}] === 3 &&
(!OrderedQ[{x, y}]) && Head[x] === sunindex && Head[y] === sunindex;

SUNF[i_,j_,k_,Explicit -> False] := SUNF[i,j,k];
HoldPattern[SUNF[i_,j_,k_,op___Rule|op___List]]:= 2 I (suntrace[ fci[sunt[i,k,j]] ] -
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
If[$VeryVerbose > 0,WriteString["stdout", "SUNF | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SUNFJacobi *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SUNFJacobi`",
             "HighEnergyPhysics`FeynCalc`"];


SUNFJacobi::"usage"="SUNFJacobi is an option for SUNSimplify, indicating
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

SUNFToTraces::"usage"=
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

SUNIndex::"usage"=
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

SUNIndexRename::"usage"= "SUNIndexRename is an option of SUNSimplify. If set to
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

SUNN::"usage" =
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

SUNNToCACF::"usage"= "SUNNToCACF is an option of SUNSimplify. If set to
True, the Casimir operator eigenvalues CA (=N) and CF (=(N^2-1)/(2 N))
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


SUNT::"usage"=
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
If[$VeryVerbose > 0,WriteString["stdout", "SUNT | \n "]];
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

ScalarProduct::"usage" =
"ScalarProduct[p, q] is the input for scalar product.
ScalarProduct[p] is equivalent to ScalarProduct[p, p].
Expansion of sums of momenta in ScalarProduct is done with
ExpandScalarProduct. Scalar products may be set, e.g.
ScalarProduct[a, b] = m^2; but a and b may not contain sums.
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

ScalarProductExpand::"usage" =
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

(* :Title: SmallDelta *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: SmallDelta is some small delta *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`SmallDelta`",
             "HighEnergyPhysics`FeynCalc`"];

SmallDelta::"usage" = "SmallDelta denotes some small positive number.";

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

SmallEpsilon::"usage" = "SmallEpsilon denotes some small
positive number.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

(*
SmallEpsilon /: SmallEpsilon _ = 0;
SmallEpsilon /: SmallEpsilon^_Integer?Positive = 0;
*)

SmallEpsilon /:
MakeBoxes[SmallEpsilon, TraditionalForm] := "\[Epsilon]";

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

SmallVariable::"usage" =
"SmallVariable[me] is a small (negligible) variable.
This means any mass with head SmallVariable be neglected if it
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



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Spinor *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Spinor denotes spinors *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Spinor`",
             "HighEnergyPhysics`FeynCalc`"];

Spinor::"usage" = "Spinor[p, m] represents a Dirac spinor.
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

SpinorU::"usage" = "SpinorU[p, m] denotes a u-spinor.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[DeclareNonCommutative];
DeclareNonCommutative[SpinorU];

   SpinorU /:
    MakeBoxes[SpinorU[p_], TraditionalForm] := Tbox["u","(",p,")"];
   SpinorU /:
    MakeBoxes[SpinorU[p_,m_,___], TraditionalForm] :=
    Tbox["u","(",p,",",m,")"];
   SpinorU /:
    MakeBoxes[SpinorU[p_,0,___], TraditionalForm] := Tbox["u","(",p,")"];

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

SpinorUBar::"usage" = "SpinorUBar[p, m] denotes a ubar-spinor.";

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
   SpinorUBar /:
   MakeBoxes[SpinorUBar[p_,0,___], TraditionalForm] :=
   Tbox[OverBar["u"],"(",p,")"];

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

SpinorV::"usage" = "SpinorV[p, m] denotes a v-spinor.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

DeclareNonCommutative = MakeContext["DeclareNonCommutative"];
DeclareNonCommutative[SpinorV];

   SpinorV /:
   MakeBoxes[SpinorV[p__], TraditionalForm] := Tbox["v","(",p,")"];
   SpinorV /:
   MakeBoxes[SpinorV[p_,m_,___], TraditionalForm] :=
   Tbox["v","(",p,",",m,")"];
   SpinorV /:
   MakeBoxes[SpinorV[p_,0,___], TraditionalForm] := Tbox["v","(",p,")"];

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

SpinorVBar::"usage" = "SpinorVBar[p, m] denotes a vbar-spinor.";

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
   SpinorVBar /:
   MakeBoxes[SpinorVBar[p_,0,___], TraditionalForm] :=
   Tbox[OverBar["v"],"(",p,")"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SpinorVBar | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SpinPolarizationSum *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Option for several functions *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`SpinPolarizationSum`",
             "HighEnergyPhysics`FeynCalc`"];

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

SpinPolarizationSum::"usage"=
"SpinPolarizationSum is an option for SquareAmplitude and
FermionSpinSum. The set (pure) function acts on the usual spin sum.";

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SpinPolarizationSum | \n "]];
Null



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

TraceOfOne::"usage" =
"TraceOfOne is an option for Tr and DiracTrace.
Its setting determines the value of the unit trace.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "TraceOfOne | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Trick *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 28 January '98 at 15:27 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Trick does non-commutative expansion and simple contractions *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Trick`",
             "HighEnergyPhysics`FeynCalc`"];

Trick::"usage" =
"Trick[exp] uses Contract, DotSimplify and SUNDeltaContract.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[
DotSimplify, EpsContract,
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

UnDeclareNonCommutative::"usage" =
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
          False]&, Flatten[{b}]
     ]; Null);

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "UnDeclareNonCommutative | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Upper *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`FeynCalc`Upper`",
             "HighEnergyPhysics`FeynCalc`"];

Upper::"usage"= "Upper may be used inside LorentzIndex to indicate an
contravariant LorentzIndex.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Upper | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: WriteOut *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: WriteOut is an option for several functions *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`WriteOut`",
             "HighEnergyPhysics`FeynCalc`"];

WriteOut::"usage" = 
"WriteOut is an option for OneLoop and SquareAmplitude. 
If set to True, the result of
OneLoop will be written to a file called \"name.res\", where name
is the first argument of OneLoop.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "WriteOut | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: WriteOutPaVe *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`WriteOutPaVe`",
             "HighEnergyPhysics`FeynCalc`"];

WriteOutPaVe::"usage"=
"WriteOutPaVe is an option for PaVeReduce and OneLoopSum. \
If set to a string, the results of all Passarino-Veltman PaVe's are stored in \
files with names generated from this string and the arguments of PaVe.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "WriteOutPaVe | \n "]];
Null



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ZeroMomentumInsertion *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`ZeroMomentumInsertion`",
             "HighEnergyPhysics`FeynCalc`"];

ZeroMomentumInsertion::"usage"= 
"ZeroMomentumInsertion is an option of FeynRule, Twist2GluonOperator and
Twist2QuarkOperator.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ZeroMomentumInsertion | \n "]];
Null



(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)
(* ************************************************************************ *)
(* Finish and print startup message *)
(* ************************************************************************ *)
(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)

Protect[Dot];

If[tarcerloadedflag===True, ToExpression["ToTFi"];
 Clear[tarcerloadedflag]];

If[$VersionNumber>3.4,
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
