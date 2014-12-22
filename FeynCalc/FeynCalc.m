(* ::Package:: *)

(* :Title: FeynCalc *)
If[$VersionNumber>5.3,
Remove[PartialD];
(*Off[PartialD::shdw];
Off[HighEnergyPhysics`FeynCalc`CoreObjects`PartialD::shdw];
*)
];

(* :Version: 9.0.0.0 *)

(* :Authors: Rolf Mertig  (rolfm@gluonvision.com)
             Frederik Orellana (frederik@orellana.dk)
*)

(* :Summary: Tools and Tables *)

(* :Terms of use: GPL, see
                  http://www.feyncalc.org/licence.txt *)

(* :Mathematica Version 6 - 9 *)

(* :History:

   Version 1.0 written 1991 by Rolf Mertig.
   Version 3.0 includes typesetting features of Mathematica 3.0
   Version 3.0.1.1 includes two bug-fixes for OneLoop
   Version 4.0 : 2000, reorganized for open-source and extensibility
   Version 4.2.0 : 2002, small bug fixes, more reorganization,
                   inclusion of help system, PHI and FeynArts
                   by Frederik Orellana, frederik@orellana.dk
   Version 5.0.0b: 2003, bug fixes, adjustments for M5.0 more reorganization,
   Version 5.1.0: 2006, bug fixes, updates for mma 5.2 and new FeynArts
   Version 6.0.0: 2007, bug fixes, updates for mma 6.0 and new FeynArts
   Version 7.0.0: 2009/2010, bug fixes, updates for mma 7.0 and new FeynArts
   Version 8.0.0: 2010,  minimal updates for Mathematica 8.0, added a patched o FeynArts 3.4
   Version 8.0.0: 2011,  fixed some bugs reported by ibedir
   Version 8.0.0beta3: 2011,  fixed a bug in OneLoop, changed Uncontract, TID
   Version 8.0.1: 2011,  fixed a problem in DiracTrace
   Version 8.0.2: 2011,  fixed more problems, working on documentation
   Version 8.0.3: 2011,  added ClearAttributes[FeynAmpDenominator,Orderless], added Momentum in DiracSimplify
   Version 8.1.0: 2012,  fixed DiracTrick, improved SUNSimplify, DiracEquation, fixed Hyperlinks in FeynCalc8.nb, fixed Tarcer .mx loading
   Version 8.1.0.1: 2012, updated PHI to work with FeynArts-3.7, which can now be kept in a subdir
   Version 8.2.0: 2012, added FeynArts 3.7 - unpatched. On first load it will be patched automatically.
   Version 9.0.0: 2014, added FeynArts 3.9 - unpatched. On first load it will be patched automatically. Fixed reported bugs.

*)


(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)
(* ************************************************************************ *)
(* Init stuff *)
(* ************************************************************************ *)
(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)
(* this is fine *)

HighEnergyPhysics`FeynCalc`$FeynCalcVersion = "9.0.0";

(* ------------------------------------------------------------------------ *)
(* Clear all definitions.
   Added 21/9-2000 by F.Orellana to ease debugging *)
(* ------------------------------------------------------------------------ *)

(*Set defaults here, not in the config file*)
If[!ValueQ[Global`$FeynCalcStartupMessages], Global`$FeynCalcStartupMessages = True];

(*FeynCalcCellPrint=CellPrint;*)
System`Private`cellmargs = CellMargins->{{Inherited,Inherited},{1,0}};
System`FeynCalcCellPrint[ce_Cell] :=
CellPrint[Append[ce(*/."Text"->"SmallText"*), System`Private`cellmargs]] /;
Global`$FeynCalcStartupMessages =!= False;
System`FeynCalcPrint[x__] :=
  If[Global`$FeynCalcStartupMessages =!= False, Print[x]];

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

Off[ClearAll::wrsym];
  ClearAll /@ fcallsymbols;
On[ClearAll::wrsym];

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

(* Change, Sept. 9th 2003, RM :
Use (like JLink) the System`Private`FindFile function which returns
the directory where this (FeynCalc.m) file is located on the file system.

FeynCalc can be loaded now from everywhere.
E.g.:
<</tmp/HighEnergyPhysics/FeynCalc.m


Set

HighEnergyPhysics`FeynCalc`$FeynCalcDirectory = /mydir/HighEnergyPhysics

in FCConfig.m if a different installation should be used.

*)

If[!ValueQ[HighEnergyPhysics`FeynCalc`$FeynCalcDirectory],
Block[{thisdir = Directory[]},
       HighEnergyPhysics`FeynCalc`$FeynCalcDirectory =
       SetDirectory@DirectoryName[System`Private`FindFile[$Input]];
(* Need to do a SetDirectory in order to get the full path name under Linux.*)
       SetDirectory@thisdir
     ]
];

HighEnergyPhysics`FeynCalc`Private`configfile= "FCConfig.m";

SetDirectory[HighEnergyPhysics`FeynCalc`$FeynCalcDirectory];
If[
FileNames[HighEnergyPhysics`FeynCalc`Private`configfile] =!= {},
 Get@HighEnergyPhysics`FeynCalc`Private`configfile;
  ];
ResetDirectory[];

If[$Notebooks ===True,
   FeynCalcCellPrint[Cell[TextData[{"Loading FeynCalc from " <>
   HighEnergyPhysics`FeynCalc`$FeynCalcDirectory }],
                  "Text"]],
FeynCalcPrint["Loading FeynCalc from ", HighEnergyPhysics`FeynCalc`$FeynCalcDirectory
     ]
  ];

If[!MemberQ[$Path,Evaluate[ParentDirectory[
    HighEnergyPhysics`FeynCalc`$FeynCalcDirectory]]],
    AppendTo[$Path, ParentDirectory[
    HighEnergyPhysics`FeynCalc`$FeynCalcDirectory]]
];

If[FileNames["*",{HighEnergyPhysics`FeynCalc`$FeynCalcDirectory}] == {},
   Print["Could not find a FeynCalc installation. Quitting the Mathematica kernel."];
   Quit[]; Exit[];
];


(* ------------------------------------------------------------------------ *)

(* Maybe this is not a good idea. It is probably better to positively
declare explicitly those directories which are Package declared
*)
  HighEnergyPhysics`FeynCalc`$ExcludeAutomaticDeclarePackageDirectories=
  {"Tarcer", "tarcer", "Phi",
   "FeynArts", "GraphInfo", "Models", "ShapeData",
   "documentation", "Documentation","core",
   "CVS", "cvs", ".AppleDouble"};

If[($VersionNumber < 3.0),
   Print["You need Mathematica 3.0 to run FeynCalc 3.0.
          Quitting the Mathematica kernel."];
   Quit[]; Exit[];
  ];

If[3.4 < $VersionNumber < 6, (* RM change 1/4/2009*)
   (*And, well, System`MonomialList is gone;
    we construct a replacement for use in Collect3.
    F.Orellana, 17/9-2002*)
    System`MonomialList =
    Internal`FromDistributedTermsList[
      Internal`DistributedTermsList[#1, Sequence@@Drop[{##, CoefficientDomain-> RationalFunctions}, 1]],
    List]&];

If[$VersionNumber >= 7, (* RM change 1/4/2009*)
	Scan[ {Remove @@ Names["Global`"<>#], ToExpression["System`"<>#]}&,
  { "CommonDefaultFormatTypes" }];
];

savethisdir=Directory[];
 HighEnergyPhysics`FeynCalc`Private`feyncalchepdir =
HighEnergyPhysics`FeynCalc`$FeynCalcDirectory;
SetDirectory[HighEnergyPhysics`FeynCalc`Private`feyncalchepdir];

(*Set defaults here, not in the config file*)
If[!ValueQ[Global`$LoadTARCER], Global`$LoadTARCER = True];

If[StringQ[Global`$LoadTARCER] || (Global`$LoadTARCER === True ),

If[StringQ[ Global`$LoadTARCER ],
   HighEnergyPhysics`FeynCalc`Private`tarcerfilenames =
   {Global`$LoadTARCER},
HighEnergyPhysics`FeynCalc`Private`tarcerfilenames =
FileNames["tarcer"<> StringReplace[$System,{"-"->"","Microsoft"->"","("->"",")"->""," "->""}] <>"*.mx",
    ToFileName[{HighEnergyPhysics`FeynCalc`$FeynCalcDirectory,
"Tarcer"}],IgnoreCase->True]
];

If[HighEnergyPhysics`FeynCalc`Private`tarcerfilenames=!={},

tarcerloadedflag = True;
If[Global`$FeynCalcStartupMessages=!=False,
If[$Notebooks ===True,
   FeynCalcCellPrint[Cell[TextData[{"Loading TARCER ",
HighEnergyPhysics`FeynCalc`Private`tarcerfilenames//Last}],
                  "Text"]],
   FeynCalcPrint["Loading TARCER ",
HighEnergyPhysics`FeynCalc`Private`tarcerfilenames//Last]
  ];
  ];
Get[Last[HighEnergyPhysics`FeynCalc`Private`tarcerfilenames]];
Clear[HighEnergyPhysics`FeynCalc`Private`tarcerfilenames];,

If[$Notebooks ===True,
txt = {"WARNING! No TARCER*.mx file found. Please evaluate the notebook ",
        ButtonBox["TARCER.nb",
          ButtonFunction.NotebookOpen1[
              ToFileName[{HighEnergyPhysics`FeynCalc`$FeynCalcDirectory,
                  "Tarcer"}, "TARCER.nb"]], ButtonStyle -> "Hyperlink",
          ButtonNote -> "Open the notebook TARCER.nb"],
        " or get one of the preprocessed files at ",
        ButtonBox["www.feyncalc.org/tarcer",
          ButtonData :> {URL["http://www.feyncalc.org/tarcer/"], None},
          ButtonStyle -> "Hyperlink",
          ButtonNote -> "http://www.feyncalc.org/tarcer/"]} /.
      Dot -> RuleDelayed /. NotebookOpen1 -> NotebookOpen; CellPrint[
  Cell[TextData[txt], "Text"]];Clear[txt,NotebookOpen1];,
   Print["WARNING! No TARCER*.mx file found. Please evaluate \
the notebook TARCER.nb or get one of the preprocessed files at \
http://www.feyncalc.org/tarcer/"];
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

BeginPackage["HighEnergyPhysics`FeynCalc`"];

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
You can get on-line information by ?function, e.g. ?Contract.\n
There are several useful functions for short input, type $FCS for a list of \
short commands. Then type, e.g., ?GA.\n\n
To enable/disable start-up messages, put the line\n
$FeynCalcStartupMessages = True;\n
or\n
$FeynCalcStartupMessages = False;\n
 into your \"init.m\" file or into your \"FCConfig.m\" file."
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

MakeFeynCalcPrivateContext::"usage"=
"MakeFeynCalcPrivateContext[val] constructs
HighEnergyPhysics`FeynCalc`Private`val.";


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

(*$FortranFormatDoublePrecision::"usage"="If set to True
FortranForm[2.] will give 2D0 and FortranForm[a/100] will give 1.D-2*a";*)

$Gauge::"usage"=
"$Gauge(= 1/xi) is a constant specifying the gauge fixing parameter of QED \
in Lorentz gauge.  The usual choice is Feynman gauge, $Gauge=1. \
Notice that $Gauge is used by some functions, the option Gauge by others.";

$Gauge/:
MakeBoxes[$Gauge,TraditionalForm]:=
MakeBoxes[StyleForm["\[Lambda]",FontSlant->"Italic"]];

$IndexPrefix::"usage"=
"$IndexPrefix is a list of prefixes for default Lorentz and color indices
used by GluonPropagator and similar functions.";

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

$LeviCivitaSign::"usage"=
"$LeviCivitaSign is by default set to -1 which corresponds to the convention
Tr[LeviCivita[a,b,c,d,5]] = -4*I*Eps[a,b,c,d].
Setting $LeviCivitaSign=I  will switch to the FORM-convention.";

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
The default is 1024. It should be increased if possible. \
The higher $MemoryAvailable can be, the more intermediate \
steps do not have to be repeated by FeynCalc.";

$NonComm::"usage"=
"$NonComm contains a list of all non-commutative heads present.";

$MU::"usage"=
"$MU is the head of dummy indices which may be introduced by \
Chisholm, Contract, DiracSimplify, FermionSpinSum and various \
QCD functions. By default it is unset, but can be set to anything.";

$OPEWard::"usage"=
   "$OPEWard is experimental.";

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

FCPrint::"usage" =
"FCPrint[level, x] outputs Print[x] if the value of $VeryVerbose
is larger than level.";

UseWriteString::"usage" =
"UseWriteString is an option for FCPrint. If set to True,
the expression is printed via WriteString instead of Print.";

WriteStringOutput::"usage" =
"UseWriteStringOutput an option for FCPrint. It specifies, to which
stream WriteString should output the expression";


(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)
(* ************************************************************************ *)
(* Functions in main context and declaration of packages *)
(* ************************************************************************ *)
(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)


(* ------------------------------------------------------------------ *)
Begin["`Private`"]

(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)

$IndexPrefix={"li","ci"};

$AchmedRoss = False;
$BreitMaison =  False;
(* SetAttributes[$BreitMaison, Locked];*)
$Color       = False;
$Covariant = False;

$FCS = {"FAD", "FV", "FVD", "FVE", "GA","GAD", "GAE" , "GA5", "GS",
          "GSD", "GSE", "LC", "LCD", "MT","MTD","MTE", "SD", "SDF", "SOD",
          "SP", "SPC", "SPD", "SPE", "SPL", "FCI", "FCE", "FI",
          "FC", "GGV", "GP", "QGV", "QO", "FDr", "CDr"
         };


$FCT  = False;
(*If[!ValueQ[$Kreimer],  $Kreimer = False];*)
$Larin   = False;
$LimitTo4 = True;
$LorentzIndices = False;
(* RM20120113: changed this *)
$MemoryAvailable = 4096;
$OPEWard = False;
If[!ValueQ[$VeryVerbose],  $VeryVerbose   = 0];

If[!ValueQ[$West],
$West = True;
];

DOT = Dot;

$Multiplications = {Times, DOT};
$DistributiveFunctions = {Conjugate, Transpose};
$Containers = {};


Options[FCPrint] = {
    UseWriteString -> False,
    WriteStringOutput ->"stdout"
}

FCPrint[level_, x__, OptionsPattern[]] :=
    If[ $VeryVerbose >= level,
        If[ OptionValue[UseWriteString],
            WriteString[OptionValue[WriteStringOutput],x],
            Print[x]
        ]
    ];
SetAttributes[FCPrint, HoldAll];

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

(*The implementation is a bit expensive; should be improved...*)
OptionsSelect[function_, opts___] :=
  Select[(Cases[{opts}, _Rule|_RuleDelayed, Infinity] //.
  {{a___, b_ -> c_, d___, b_ -> e_, f___} -> {a, b -> c, d, f},
   {a___, b_ :> c_, d___, b_ :> e_, f___} -> {a, b :> c, d, f}}),
  (!FreeQ[#, (Options[function] /.
             {((a_ -> b_) | (a_ :> b_)) -> ((a -> _) | (a :> _))} /.
              List -> Alternatives)])&];

If[!ValueQ[$NonComm], $NonComm = {}];

Unprotect[ToBoxes];


 MyNeeds[x_String] := ( If[!MemberQ[$Packages, x], Needs[x]]);

(*
load everything in core which is, by definition, not in the core sub-Context
*)

  (* This is for the grouping of files in subdirectories in
     HighEnergyPhysics, like:  general, fcloops, fctools, fctables, qcd
  *)
  SubContext[_String] := "FeynCalc`";

SetAttributes[MakeContext, HoldAll];

  MakeContext[x_String, y_String] :=
    If[SubContext[x] === "FeynCalc`",
    ToExpression["HighEnergyPhysics`FeynCalc`"<> x <>"`" <> y],
    (MyNeeds["HighEnergyPhysics`"<> SubContext[x] <> x <>"`"];
     ToExpression["HighEnergyPhysics`"<>SubContext[x]<>x<>"`"<>y]
     (*<>x<>"`" added 6/8-2000, F.Orellana*)
    )];

  MakeFeynCalcPrivateContext[x_String] := MakeFeynCalcPrivateContext[x] =
ToExpression["HighEnergyPhysics`FeynCalc`Private`"<>x];



SetAttributes[MakeContext, HoldAll];

SetAttributes[SetDel, HoldAll];
SetDel[a_, b_] := (a := a = b);

  MakeContext[x_Symbol] := MakeContext[x] =
          With[{s=ToString[x]}, SetDel[x, Apply[MakeContext, {s}]]];

  MakeContext[x___Symbol] := Map[MakeContext, {x}];

  (*OLD*)
  MakeContext[x_String] := MakeContext[x] = (If[SubContext[x] =!= "FeynCalc`",
             MyNeeds["HighEnergyPhysics`"<> SubContext[x] <> x <>"`"]];
             ToExpression["HighEnergyPhysics`"<>SubContext[x] <>
                        x <> "`" <> x]);

  (*NEW*) (*Don't see what this is good for. It gives loading problems
  and the old stuff seems to work fine. F.Orellana, 30/3-2006*)
  (*MakeContext[x_String] := MakeContext[x] =
  Block[{},
    If[!FreeQ[Rest/@multifunpack, x],
      (* take care of SubContext of the multifunpack values.
         F.Orellana,  28/3-2005 *)
      Evaluate[ToString[Context[MakeContext[Evaluate[
         multifunpack[[ Position[multifunpack, x] [[1,1]], 1]] ]]<>
         "`" <> x]]],
      If[SubContext[x] =!= "FeynCalc`",
        MyNeeds["HighEnergyPhysics`"<> SubContext[x] <> x <>"`"]];
        ToExpression["HighEnergyPhysics`"<>SubContext[x] <>
           x <> "`" <> x]
    ]
  ];*)

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
{"Contract", "Contract2", "Contract3", "Rename"},
{"DiracSimplify", "ChisholmSpinor", "DiracCanonical", "InsideDiracTrace", "$SpinorMinimal", "DiracSimpCombine", "DiracSubstitute67"},
{"DotSimplify", "DotSimplifyRelations", "DotPower"},
{"FermionSpinSum", "SpinorCollect"},
{"FeynAmpDenominatorSimplify", "FDS"},
{"FeynCalcForm", "FCF"},
{"FeynCalcInternal", "FCI"},
{"FeynCalcExternal", "FCE"},
{"FeynCalc2FORM", "FORMEpilog", "FORMProlog", "TraceDimension"},
{"FeynCalcToLaTeX", "F2L"},
{"FieldStrength", "IndexPosition"},
{"FORM2FeynCalc", "Vectors"},
{"GluonGhostVertex", "GGV"},
{"GluonPropagator", "GP"},
{"GhostPropagator", "GHP"},
{"GluonVertex", "GV"},
{"Isolate", "IsolatePrint", "IsolateSplit"},
{"OneLoop", "CancelQP", "CombineGraphs", "DenominatorOrder", "FinalFunction", "ExtraVariables",
"OneLoopSum", "Prefactor", "SelectGraphs", "ReduceGamma", "ReduceToScalars", "SmallVariables",
"StandardMatrixElement", "SetStandardMatrixElements"},
(*{"PropagatorDenominator", "PD"},*)
{"QuarkGluonVertex", "QGV"},
{"QuarkPropagator", "QP"},
{"SquareAmplitude", "EnergyMomentumConservation", "SpinSumExternalMomentum", "SelectedGraphs"},
{"Twist2GluonOperator", "GO"},
{"Twist2QuarkOperator", "QO"},
{"Write2", "FUNCTION", "PostFortranFile", "PreFortranFile", "$FortranContinuationCharacter"},
{"DoPolarizationSums", "PolarizationUncontract", "EpsUncontract"},
{"ILimit", "FunctionLimits"},
{"FieldDerivative", "FDr"},
{"CovariantFieldDerivative", "CDr"},
{"CheckDB", "ForceSave", "NoSave"},
{"OPEIntegrateDelta", "$MIntegrate"},
(*{"Pair", "$PairBrackets"},*)
{"Convolute", "Bracket"},
{"CovariantD", "DummyIndex"},
{"Factor2", "FactorFull"},
{"RHI", "FORM"},
{"FeynRule", "InitialFunction"},
{"Tdec", "NumberOfMetricTensors"},
{"SUNSimplify", "SUNFJacobi", "SUNIndexRename"}
};


feyncalcdir =  HighEnergyPhysics`FeynCalc`$FeynCalcDirectory;

SetDirectory[feyncalcdir];

(* get all the directories (like general, qcd, fctools, fctables *)
(* this is not a good idea, since it will also get files in, e.g., fcexamples ...
hepdirs = Select[FileNames[], FileType[#]===Directory&];
*)
(* I think the only clean way is to list the directories explicitly here ..., or? *)
hepdirs = { "fcdevel", "fcloops", "fctables", "fctools", "general",  "qcd", "fctests" };

(* fix for Mac OS *)
If[StringMatchQ[$OperatingSystem, "MacOS"],
  hepdirs = If[StringMatchQ[#, ":*"], StringDrop[#, 1], #] & /@ hepdirs];

(* Commented out for the moment ...
hepdirs = Complement[ hepdirs,
      HighEnergyPhysics`FeynCalc`$ExcludeAutomaticDeclarePackageDirectories];
*)

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

Tbox[a_,b__] := RowBox @ Map[totr, {a,b}];

(*
Tbox[a_,b__] :=
(RowBox @ (Insert[
  Map[totr, {a,b}], "\[NoBreak]",
    Array[{#}&,Length[{a,b}]-1,2]]));
*)

Unprotect[Dot];
Dot /:
MakeBoxes[Dot[a__], TraditionalForm] := (
ClearAttributes[Times, Orderless];
embo = MakeBoxes[Times[a], TraditionalForm];
SetAttributes[Times, Orderless];
embo ) /; $FCT === True;


l[w_Integer] := l[w] = Block[{pre},
     If[!MatchQ[pre = $IndexPrefix,{_String,_String}],
        pre = {ToString[Unique["l"]], ToString[Unique["c"]]} ];
     ToExpression[pre[[1]]<>ToString[w]]
];

c[w_Integer] := c[w] = Block[{pre},
     If[!MatchQ[pre = $IndexPrefix,{_String,_String}],
        pre = {ToString[Unique["l"]], ToString[Unique["c"]]} ];
     ToExpression[pre[[2]]<>ToString[w]]
];


End[];
EndPackage[];

Map[HighEnergyPhysics`FeynCalc`Private`fcDeclarePackge,
    HighEnergyPhysics`FeynCalc`Private`declarepackagelist];

SetDirectory[ToFileName[{HighEnergyPhysics`FeynCalc`$FeynCalcDirectory,"core"}]];
(* need to do this first, otherwise $NonComm does not get built correctly *)
Get["DataType.m"];
Get["DeclareNonCommutative.m"];
corefiles = FileNames["*.m"];
Get/@corefiles;
ResetDirectory[];

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

(*OLD: uncommented*)
(*{Hold[Set][Hold[SubContext][a], Hold[SubContext][b]],
             Hold[Set][Hold[MakeContext][a], Hold[MakeContext][b]]}*)

ReleaseHold[HighEnergyPhysics`FeynCalc`Private`tab];

(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)

(* ------------------------------------------------------------------ *)

(* Print startup message *)

If[Global`$FeynCalcStartupMessages =!= False ,
If[$Notebooks===True,
  With[{fcrefnb = FileNameJoin[ {$FeynCalcDirectory, "Documentation", "English", "FeynCalcRef.nb"}]},
   FeynCalcCellPrint[Cell[TextData[{StyleBox[ "FeynCalc" , FontWeight-> "Bold"], " ",
    $FeynCalcVersion,
     ". For help, type ?FeynCalc, use the ",
     ButtonBox["help browser",
       (*ButtonFunction :> NotebookOpen[fcrefnb],*)
       ButtonData:>{ "Short Overview", "intro"},
       ButtonStyle->"AddOnsLink",
       ButtonNote->"FeynCalc"],
     " or visit ",
     ButtonBox["www.feyncalc.org", ButtonData:>{
      URL[ "http://www.feyncalc.org/"], None},
     ButtonStyle->"Hyperlink", ButtonNote->"http://www.feyncalc.org/"]}
    ],"Text"]]
  ]
,
  WriteString["stdout", "FeynCalc " <> $FeynCalcVersion ,
              " Type ?FeynCalc for help or visit http://www.feyncalc.org/", "\n"];
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
(* RM: change 2010-01-10 *)
SetOptions[$FrontEndSession, "CommonDefaultFormatTypes"->{"Output" -> TraditionalForm}]
,
Null

];
Clear[feversion];



(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)
(* ************************************************************************ *)
(* Finish and print startup message *)
(* ************************************************************************ *)
(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)

Protect[Dot];

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

(*Default*)
(* CHANGE2007RM *)
(* CHANGE2014VS *)
If[Global`$LoadFeynArts =!= False,
If[ValueQ[HighEnergyPhysics`FeynCalc`$FeynArtsDirectory]=!=True,
   HighEnergyPhysics`FeynCalc`$FeynArtsDirectory = Automatic];

If[HighEnergyPhysics`FeynCalc`$FeynArtsDirectory === Automatic,
Off[General::cdir];
  If[(search = FileNames["FeynArts.m", {$FeynCalcDirectory, $FeynCalcDirectory <> $PathnameSeparator <> "*"}])=!={},
If[Length[search]>1,CellPrint[Cell[TextData[{
    "Found multiple FeynArts directories in ", $FeynCalcDirectory, ":\n",
      TableForm[DirectoryName/@search],"\n\n","Please use HighEnergyPhysics`FeynCalc`$FeynArtsDirectory \n"," to specify the directory FeynArts should be loaded from and reload FeynCalc"}
   ],"Text"]]; Global`$LoadFeynArts = False;]
, Global`$LoadFeynArts = False; CellPrint[Cell[TextData[{
    "FeynArts not found. Please install FeynArts, e.g., in\n",
          If[$VersionNumber >= 4.2, $UserAddOnsDirectory,
             ToFileName[{$TopDirectory,"AddOns","Applications"}]],
    "\n", "and reload FeynCalc",
    "\n","FeynArts can be downloaded from ",
    ButtonBox["www.feynarts.de", ButtonData:>{
     URL[ "http://www.feynarts.de/"], None},
    ButtonStyle->"Hyperlink", ButtonNote->"http://www.feynarts.de/"]}
   ],"Text"]]];
On[General::cdir];
  If[Length[search]>0&&StringQ[search[[1]]], HighEnergyPhysics`FeynCalc`$FeynArtsDirectory = DirectoryName[search[[1]]]]];
];

(*Set defaults here, not in the config file*)
If[!ValueQ[Global`$LoadPhi], Global`$LoadPhi = True];
If[!ValueQ[Global`$LoadFeynArts], Global`$LoadFeynArts = True];

If[Global`$LoadPhi===True,
   If[$Notebooks===True,
      FeynCalcCellPrint[Cell[TextData[{"Loading PHI "}],
                  "Text"]],
      FeynCalcPrint["Loading PHI "]
   ];
   If[StringMatchQ[$OperatingSystem, "MacOS"],
   Get[$PathnameSeparator<>"Phi"<>$PathnameSeparator<>"Phi.m"],
   Get["Phi"<>$PathnameSeparator<>"Phi.m"]]
];

If[ Global`$LoadFeynArts===True,
   If[$Notebooks===True,
      FeynCalcCellPrint[Cell[TextData[{
     "Loading FeynArts, see ", ButtonBox["www.feynarts.de", ButtonData:>{
     URL[ "http://www.feynarts.de/"], None},
    ButtonStyle->"Hyperlink", ButtonNote->"http://www.feynarts.de/"]," for documentation"}],
                  "Text"]],
      FeynCalcPrint["Loading FeynArts, see www.feynarts.de for documentation"]
   ];
(* loading *)
Block[{Print},
If[HighEnergyPhysics`FeynCalc`$FeynArtsDirectory === Automatic,
   loadfa = Get["FeynArts`"];
   If[loadfa=!=$Failed,
   HighEnergyPhysics`FeynCalc`$FeynArtsDirectory = HighEnergyPhysics`FeynArts`$FeynArtsDir],
   $Path = Drop[$Path,Flatten[Position[$Path,s_String /; StringMatchQ[s, "*FeynArts*"]]]];
   $Path = Join[{ToString[HighEnergyPhysics`FeynCalc`$FeynArtsDirectory]},$Path];
   loadfa =  Get["FeynArts`"];
]];
If[loadfa =!=$Failed,
Block[{faversion},
faversion = FindList[ToFileName[{HighEnergyPhysics`FeynCalc`$FeynArtsDirectory},"FeynArts.m"],
"$FeynArts = "] /. {s_String} :> StringDrop[s,12];

If[$Notebooks===True,
   FeynCalcCellPrint[Cell[TextData[{
   "FeynArts " <> faversion <> " patched for use with FeynCalc"}], "Text"]],
   FeynCalcPrint["FeynArts " <>
   faversion <> " patched for use with FeynCalc"
        ] /; Global`$FeynCalcStartupMessages =!= False
]];
];

   If[loadfa === $Failed,
     If[$Notebooks===True,
  CellPrint[Cell[TextData[{
    "FeynArts not found. Please install FeynArts, e.g., in\n",
          If[$VersionNumber >= 4.2, $UserAddOnsDirectory,
             ToFileName[{$TopDirectory,"AddOns","Applications"}]],
    "\n", "and reload FeynCalc",
    "\n","FeynArts can be downloaded from ",
    ButtonBox["www.feynarts.de", ButtonData:>{
     URL[ "http://www.feynarts.de/"], None},
    ButtonStyle->"Hyperlink", ButtonNote->"http://www.feynarts.de/"]}
   ],"Text"]],
       WriteString["stdout",
          "FeynArts not found. Please install FeynArts, e.g., in\n ",
          If[$VersionNumber >= 4.2, $UserAddOnsDirectory,
             ToFileName[{$TopDirectory,"AddOns","Applications"}]
            ],
          "FeynArts not found. Please put the files in ",
    HighEnergyPhysics`FeynCalc`$FeynArtsDirectory,
    ". FeynArts can be downloaded from www.feynarts.de\n"];
     ],
(* This only removes FeynArts formatting rules which interfer with FeynCalc formatting rules *)
Remove[HighEnergyPhysics`FeynArts`SetForm];
(* RM20110830: added ClearAttributes[FeynAmpDenominator, Orderless];  *)
ClearAttributes[FeynAmpDenominator, Orderless];
  ];
];

$LeviCivitaSign=-1;


(* From Mathematica 4.0 onwards there is  "Tr" functions;
   Overload Tr to use TR
*)
If[$VersionNumber>3.4,
   Unprotect@@{ToExpression["Tr"]};
   Tr[x__]:=HighEnergyPhysics`fctools`TR`TR[x] /;
       !FreeQ[{x}, DiracGamma | DiracMatrix | DiracSlash | GA | GAD | GAE | GS | GSD | GSE | Pair];
   Tr::usage="FeynCalc extension: Tr[list] finds the trace of the matrix or tensor list. Tr[list, f] finds a
   generalized trace, combining terms with f instead of Plus. Tr[list, f, n]
   goes down to level n in list. \n \n
Tr[ expression ] calculates the DiracTrace, i.e.,  TR[ expression ],
if any of DiracGamma, DiracSlash, GA, GAD, GAE, GS, GSD or GAE are present in expression.";

Tr/:Options[Tr]:=Options[HighEnergyPhysics`fctools`TR`TR];

   Protect@@{ToExpression["Tr"]};
  ];

(* Added by Rolf Mertig 20101209*)
If[$VersionNumer>=7., Unprotect[Graph]; Remove[Graph]];


SetDirectory[savethisdir];
Clear[savethisdir];
