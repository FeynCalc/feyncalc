(*
Definitions for the package Channels
*)

(*
Usage
*)

MassArguments::"usage" = 
    "MassArguments is an option for AmplitudeProjection relevant when \
OnMassShell is set to True, specifying the optional extra arguments supplied \
to UParticleMass.  Default value : {RenormalizationState[0]}";

FieldProjection::"usage" = 
    "FieldProjection[IsoVector[QuantumField[Particle[p]]],opts] returns the \
field specified by  the option Channel as a linear combination of the \
iso-spin components of QuantumField[Particle[p]]";

AmplitudeProjection::"usage" = 
    "AmplitudeProjection[amp,opts], where amp[i1,i2,...] is a function of the \
isospin indices i1, i2, ... of the external particles, returns the amplitude \
of the channel specified by the option Channel.  AmplitudeProjection uses the \
rules specified in $IsoSpinProjectionRules.  NOTICE:  For the particles under \
consideration, Phi must know their anti-particles.  That is, e.g. \
Conjugate[PionPlus] is PionMinus";

$IsoSpinProjectionRules::"usage" = 
    "$IsoSpinProjectionRules is a set of rules used by FieldProjection and \
AmplitudeProjection to project out in channels.  Changing the default setting \
of this quantity should be done with care.  Notice that the setting of \

$IsoSpinProjectionRules is related to the values of \
WriteOutUmatrices[UGeneratorMatrix[i,opts]], where i is an integer.  The \
default set of rules is not very large, but more rules in the same syntax may \
easily be added";

Channel::"usage" = 
    "Channel is an option of FieldProjection and AmplitudeProjection, \
specifying which channel the field or amplitude should be projected out in.  \
For FieldProjection, the possible settings are listed in $UParticles.  For \
AmplitudeProjection, the possible settings are \
{{p1,p2,...}->{pp1,pp2,...},{i,...}}, where  p1,.. and pp1,... are generic \
particles from $UParticles like Pion etc. and i is the iso-spin, or simply \
{{p1,p2,...}->{pp1,pp2,...}}, where p1,.. and pp1,... now have to be \
iso-eigenstates like PionPlus etc..  Default value : PionPlus for \
FieldProjection and {{Pion,Pion}->{Pion,Pion},{2}} for AmplitudeProjection";

USum::"usage" = 
    "USum is a summation that works like Sum.  It may be faster that Sum for \
multiple summations where each summation reduces the number of terms.  This \
is typically the case in isospin indices summations";

USumHeld::"usage" = 
    "USumHeld is a symbol substituted for USum when the option HoldSums of \
SUNReduce is set to True";

SUNReduce::"usage" = 
    "SUNReduce[a] finds SU(n) objects, simplifies using SU(2) or SU(3) rules \
and sums over pairs of indices not in $ConstantIsoIndices.  Indices are \
summed over only if they have head SUNIndex.  NOTICE: With the option \
SummationForm set to ExplicitSums, large expressions that can be expanded to \
a sum, should be expanded before applying SUNReduce (this will reduce \
computation time dramatically)";

SUDFSymmetrize::"usage" = 
    "SUDFSymmetrize[a] renames factors multiplying SUND[i,j,k], \
SU3D[i,j,k], SUNF[i,j,k], SU2F[i,j,k] or SU3F[i,j,k] in an attempt to \
reduce a";

IsoFunction::"usage" = 
    "IsoFunction is a head recognized by SUNReduce, so that for e.g. \
IsoFunction[f][SUNIndex[i]*SUNDelta[SUNIndex[i],SUNIndex[j]] ocurring in an \
expression will imply a sum over SUNIndex[i]";

SummationForm::"usage" = 
    "SummationForm is an option for SUNReduce specifying whether the sums should \
be performed by 'brute force' (ExplicitSums) or symbolically (ImplicitSums).  \
Default value : ImplicitSums for SUNReduce, ExplicitSums for \
AmplitudeProjection";

ImplicitSums::"usage" = 
    "ImplicitSums is one of the two possible settings of the option \
SummationForm of SUNReduce";

ExplicitSums::"usage" = 
    "ExplicitSums is one of the two possible settings of the option \
SummationForm of SUNReduce";

HoldSums::"usage" = 
    "HoldSums is an option for SUNReduce and AnmplitudeProjection relevant \
when the option SummationForm is set to ExplicitSums.  When set to True, the \
isospin summations are not performed and USum is substituted with USumHeld.  \
Default value : True for SUNReduce, False for AnmplitudeProjection";

IndicesCleanup::"usage" = 
    "IndicesCleanup[expr] renames dummy indices in expr in a systematic way \
in order to get cancellations and a simpler expression. The expression expr \
should be in Phi notation, that is, involving the products NM and/or Times, \
not Dot or NonCommutativeMultiply.  NOTICE : IndicesCleanup will not work \
properly when the indices are nested more than one level down in factors (\
the only exception being NM[UTrace[NM[a]],UTrace[NM[b]]] with a and b having \
isospin dependence).";

ExtendedCleanup::"usage" = 
    "ExtendedCleanup is an option for IndicesCleanup.  When set to True, \
contractions within a mixing of Times and NM will also be renamed, so that in \
a mixed product there will be no multiple pairs of the same index.  When set \
to False,  there will be multiple pairs of the same index in mixed products.  \
This will give wrong results with e.g. FeynRule.  Default value : True";

FCleanup::"usage" = 
    "FCleanup is an option for IndicesCleanup.  When set to True, special \
attention is given to renaming the indices of SU2F, SU3F and SUNF in order to \
get cancellations.  Default value : False";

IsoDummy::"usage" = 
    "IsoDummy is a head used by IndicesCleanup for temporary renaming of \
indices";

NM1::"usage" = 
    "NM1 is a head used by IndicesCleanup for temporary renaming of Times";

NM2::"usage" = 
    "NM2 is a head used by IndicesCleanup for temporary renaming of DOT";
    
IsoExternDummy::"usage" = 
    "IsoExternDummy is a head used by IndicesCleanup for temporary renaming \
of indices";

IsoInternDummy::"usage" = 
    "IsoInternDummy is a head used by IndicesCleanup for temporary renaming \
of indices";

LorentzDummy::"usage" = 
    "LorentzDummy is a head used by IndicesCleanup for temporary renaming of \
indices";

LorentzExternDummy::"usage" = 
    "LorentzExternDummy is a head used by IndicesCleanup for temporary \
renaming of indices";

LorentzInternDummy::"usage" = 
    "LorentzInternDummy is a head used by IndicesCleanup for temporary \
renaming of indices";

DerivativeExternDummy::"usage" = 
    "DerivativeExternDummy is a head used by IndicesCleanup for temporary \
renaming of indices";

DerivativeInternDummy::"usage" = 
    "DerivativeInternDummy is a head used by IndicesCleanup for temporary \
renaming of indices";

IsoDummys::"usage" = 
    "IsoDummys is an option for IndicesCleanup.  It should be a list of two \
three strings.  Default value : {\"j\",\"k\",\"l\"}";

LorentzDummys::"usage" = 
    "LorentzDummys is an option for IndicesCleanup.  It should be a list of \
five strings.  Default value : {\"\[Xi]\",\"\[Rho]\",\"\[Sigma]\",\"\[Tau]\",\
\"\[Omega]\"}";

Begin["`Private`"];

(*
Boxes
*)

sumstart[{_, ss_, _, _}] := ss;
sumstart[{_, ss_, _}] := ss;
sumstart[{_, ss_}] := 1;
sumstart[{_}] := 1;
sumvar[{ss_, __}] := MakeBoxes[TraditionalForm[ss]];
sumvar[{_}] := "";
sv[bb__, rr_] := sumvar[{bb}[[rr]]];
sumeq[{_}] := "";
sumeq[{ss_, __}] := "=";
USumHeld /:
     MakeBoxes[USumHeld[a__, b : _List ..], 
	  TraditionalForm] :=
   
    RowBox[Join[
        Table[UnderoverscriptBox["\[CapitalSigma]", 
            RowBox[{sv[b, rep], sumeq[{b}[[rep]]], 
                sumstart[{b}[[rep]]]}], {b}[[rep]][[-1]]], {rep, 
            Length[{b}]}], {MakeBoxes[TraditionalForm[a]]}]];

End[];
