(*
Definitions for the package Utilities
*)

(*
Usage
*)


MandelstamReduce::"usage" = 
    "MandelstamReduce[amp,opts] gives the amplitude amp with all momenta \
variables given by the option MomentumVariablesString substituted by \
Mandelstam variables MandelstamS, MandelstamT and MandelstamU.  When the \
option OnMassShell is set to True, the masses specified in the option Masses \
are used.  The substituted Mandelstam variables depend on the value of the \
option MomentaSumLeft.  E.g. For MomentaSumLeft set to All, the following \
convention is used: s=(p1+p2)^2, t=(p2+p3)^2, u=(p1+p3)^2 with all particles \
incoming";

MandelstamS::"usage" = "MandelstamS is the Mandelstam s-variable";

MandelstamT::"usage" = "MandelstamT is the Mandelstam t-variable";

MandelstamU::"usage" = "MandelstamU is the Mandelstam u-variable";

MandelstamCancel::"usage" = 
    "MandelstamCancel is an option for MandelstamReduce.  When set to one of \
MandelstamS, MandelstamT or MandelstamU, this variable is cancelled using the \
Mandelstam relation.  The cancellation is disabled by setting \
MandelstamCancel to None.  Default value : MandelstamU";

$LorentzIndicesCounter::"usage" = 
    "$LorentzIndicesCounter is a variable numbering the indices supplied by \
LorentzIndicesSupply.  To start at 1 simply set $LorentzIndicesCounter = 0";

LorentzIndicesSupply::"usage" = 
    "LorentzIndicesSupply replaces ScalarProduct's and Pair's of momenta and \
gamma matrices with products with written out Lorentz indices";

MomentaApart::"usage" = 
    "MomentaApart separates Pair[LorentzIndex[mu],Momentum[a+b+...]] into \
Pair[LorentzIndex[mu],a]+Pair[LorentzIndex[mu],b]+...";

FourPoint::"usage" = 
    "FourPoint[q,exp] expresses a rank four tensor integral (exp is the \
integrand, q is the four-momentum to be integrated with respect to) in terms \
of lower rank PaVe's according to the formula on page 22 in the FeynCalc1.0 \
manual.  Any lower rank subexpressions resulting from the expansion of exp or \
already in exp are passed to OneLoop, and FourPoint will accept the options \
of OneLoop and pass them on";

GammaSort::"usage" = 
    "GammaSort[exp] returns the expression with DOT products of gamma \
matrices sorted according to the ordering function specified";

Gamma5AntiCommute::"usage" = 
    "Gamma5AntiCommute is an option of GammaSort specifying whether or not \
the gamma-5 matrix should be taken to anticommute with the other gamma \
matrices.  Default value : False";

OrderingFunction::"usage" = 
    "OrderingFunction is an option of GammaSort specifying which function \
should be used for the ordering of the gamma matrices.  Default value : \
OrderedQ";

DiscardMomenta::"usage" = 
    "DiscardMomenta is an option for DiscardOrders.  When set to True, any \
ocurring momentum will be considered an expansion quantity, and higher powers \
will be discarded.  Default value : True";

DiscardOrders::"usage" = 
    "DiscardOrders[m,opts] discards products with the total order of momenta \
and/or any other quantities from $ExpansionQuantities higher than the \
PerturbationOrder specified in opts or Options[DiscardOrders]";

FCToTracer::"usage" = 
    "FCToTracer[exp] translates the expression exp involving unevaluated \
FeynCalc DiracTrace's to Tracer notation with the Tracer line indices set to \
variable names determined by the setting of the option TracerIndicesString";

$TracerIndicesCounter::"usage" = 
    "$TracerIndicesCounter is a variable used by FCToTracer for generating \
fermion line indices";

TracerIndicesString::"usage" = 
    "TracerIndicesString is an option of FCToTracer specifying the index used \
for the fermion line index of Tracer.  Default value : \"l\"";

TracerToFC::"usage" = 
    "TracerToFC[exp] translates the expression exp traced with Tracer to \
FeynCalc notation";

CheckF::"usage" = 
    "CheckF[exp,fil] does the following:  Checks if the setting of the option Directory \
is a valid directory name and if fil is a valid file name.  \
Checks if file exists.  If it does, Get's \
fil and returns the loaded expressions.  If fil does not exist, evaluates exp, saves it \
to fil and returns the evaluated exp.  NOTICE : If fil ends with \".Gen\" or \".Mod\", \
the setting of Directory is ignored and fil is saved in the \"CouplingVectors\" \
subdirectory of \"phi\".  If fil ends with \".Fac\", \
the setting of Directory is ignored and fil is saved in the \"Factors\" \
subdirectory of \"phi\". If fil is a file name with full path, the setting of Directory \
is also ignored";

ForceSave::"usage" = 
    "ForceSave is an option of CheckF. Setting it to True forces the first argument to \
be evaluated and saved, no matter if the file specified by the second argument exists \
or not.  Default value : False";

NoSave::"usage" = 
    "NoSave is an option of CheckF. If set to True, no results will be saved to disk. \
It is there to allow \
evaluating notebooks using CheckF without having to worry about overwriting old results \
(SetOptions[CheckF,NoSave->True]).  Default value : False";


Begin["`Private`"];

(*
Boxes
*)


MandelstamS /: MakeBoxes[MandelstamS, TraditionalForm] := 
    MakeBoxes[StyleForm["s", FontSlant -> "Italic"]][[1]];
MandelstamT /: MakeBoxes[MandelstamT, TraditionalForm] := 
    MakeBoxes[StyleForm["t", FontSlant -> "Italic"]][[1]];
MandelstamU /: MakeBoxes[MandelstamU, TraditionalForm] := 
    MakeBoxes[StyleForm["u", FontSlant -> "Italic"]][[1]];

(*
Errors
*)

CheckF::nostring = 
    "`1` is not a string.  Please give the file name as a string";
CheckF::baddir = 
    "`1` is not a valid directory.  Please set the option Directory \
correctly";

End[];
