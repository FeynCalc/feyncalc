#!/usr/bin/perl
#
# Perl script to patch FeynArts to work with Phi and FeynCalc.
# 
# Phi version 1.2, Frederik Orellana 2/8-2000.
#
#

################################################################################

# First issue a warning and ask for confirmation

print "\nThis will modify all the FeynArts *.m files.
Please make sure you have a backup.\n
Do you want to continue (y/n)?\n\n";

$confirm = <STDIN>;

if ($confirm =~ "y" || $confirm =~ "yes") {

# The list of files to be modified

@FAfiles=("FeynArts.m","SetUp.m","FeynArts/Analytic.m",
"FeynArts/Graphics.m","FeynArts/Initialize.m","FeynArts/Insert.m",
"FeynArts/Topology.m","FeynArts/Utilities.m");

# The list of names to be changed

@FAnames=("Loop","Indices","Global`PolarizationVector","FeynAmp",
"PropagatorDenominator","FeynAmpDenominator","GaugeXi","NonCommutative",
"Global`DiracSpinor","Global`DiracTrace");

# The list of names to be substituted in

@FCnames=(
"FALoop",
"FAIndices",
"Global`FAPolarizationVector",
"FAFeynAmp",
"HighEnergyPhysics`FeynCalc`PropagatorDenominator`PropagatorDenominator",
"HighEnergyPhysics`FeynCalc`FeynAmpDenominator`FeynAmpDenominator",
"HighEnergyPhysics`FeynCalc`GaugeXi`GaugeXi",
"FANonCommutative",
"HighEnergyPhysics`FeynCalc`DiracSpinor`DiracSpinor",
"HighEnergyPhysics`FeynCalc`DiracTrace`DiracTrace"
);

# Get the base directory

print "\nEnter the path to the directory containing FeynArts.m: \n\n";

$input_dir = <STDIN>;

$input_dir =~ s/\n//g;

print "\n";

# Check if FeynArts.m can be found and has not already been patched

open(DATA,"$input_dir/FeynArts.m") || die "Cannot find FeynArts, please try again and give the correct path\n";

while (<DATA>){

    if (m/patched for use with FeynCalc by Frederik Orellana/){
        die "This copy of FeynArts is already patched!\n\n"
    }

}

close(DATA);

################################################################################

# Include the Phi particle patterns

print "\nAdding \$UParticleHeads to F | S | V | U | SV in FeynArts.m.

   >Please check that this is actually done. If not, do it manually.\n";

open(DATA,"$input_dir/FeynArts.m") || die "Cannot open file for reading!\n";

@lines = ();

while (eof(DATA) <= 0){

    $add = <DATA>;

    $add =~ s/F \| S \| V \| U \| SV/F \| S \| V \| U (**)\| SV \| HighEnergyPhysics`Phi`Objects`\$UParticleHeads/;

    push(@lines,$add);

}

close(DATA);

open(DATA,">$input_dir/FeynArts.m") || die "Cannot open file for writing!\n";
for $ii (@lines) {print DATA "$ii";}
close(DATA);

################################################################################

print "\nAdding \$FermionHeads to | F | U and F in FeynArts/Analytic.m.

   >Please check that this is actually done. If not, do it manually.\n\n";

open(DATA,"$input_dir/FeynArts/Analytic.m") || die "Cannot open file for reading!\n";

@lines = ();

while (eof(DATA) <= 0){

    $add = <DATA>;

    $add =~ s/F \| U/F (**)\| U \| HighEnergyPhysics`Phi`Objects`\$FermionHeads/;

    $add =~ s/F]/F \| HighEnergyPhysics`Phi`Objects`\$FermionHeads]/;

    $add =~ s/Global`DiracSpinor\[ mom_, mass_, ___ \] \:\= Global`Spinor\[mom, mass\];/(*Global`DiracSpinor[ mom_, mass_, ___ ] := Global`Spinor[mom, mass];*)/;

    $add =~ s/Attributes\[\s?FeynAmpDenominator\s?\]\s?\=\s?\{Orderless\}//;

    push(@lines,$add);

}

close(DATA);

open(DATA,">$input_dir/FeynArts/Analytic.m") || die "Cannot open file for writing!\n";
for $ii (@lines) {print DATA "$ii";}
close(DATA);

################################################################################

# Make it known that the FA code has been patched
# and replace / with $PathnameSeparator

print "Replacing \"/\" with \"\$PathnameSeparator\" and setting \$FeynArtsDir\n\n";

open(DATA,"$input_dir/FeynArts.m") || die "Cannot open file for reading!\n";

@lines = ();

while (eof(DATA) <= 0){

    $add = <DATA>;

    $add =~ s/Have fun!/Have fun!!

Patch FApatch (August 1 2000) applied for
compatibility with Phi and FeynCalc/;

    $add =~ s/Print\[\"last revision:(.*)\"\]/Print\["last revision: $1" \];
Print\["patched for use with FeynCalc by Frederik Orellana"\];

(* To avoid error messages on reload *)
If[NumberQ[HighEnergyPhysics`FeynArts`\$FeynArts],
ClearAll[HighEnergyPhysics`FeynArts`Greek, HighEnergyPhysics`FeynArts`UCGreek],
Remove[HighEnergyPhysics`FeynArts`\$FeynArts]];/;

    $add =~ s/\"(.*)\/(.*)\"/\"$1\" \<\> \$PathnameSeparator \<\> \"$2\"/;

    $add =~ s/\$Platform = Environment\[\"HOSTTYPE\"\]/\$Platform = Environment[\"HOSTTYPE\"]

If[ValueQ[HighEnergyPhysics\`FeynCalc\`\$FeynCalcDirectory],
\$FeynArtsDir = HighEnergyPhysics\`FeynCalc\`\$FeynCalcDirectory<>
\$PathnameSeparator,
Remove[HighEnergyPhysics\`FeynCalc\`\$FeynCalcDirectory]]/;

    push(@lines,$add);

}

close(DATA);

open(DATA,">$input_dir/FeynArts.m") || die "Cannot open file for writing!\n";
for $ii (@lines) {print DATA "$ii";}
close(DATA);

################################################################################

# The files loop

$fi=0;

foreach (@FAfiles) {

    # Read in the data file

    print "patching $input_dir/@FAfiles[$fi] \n";

    open(DATA,"$input_dir/@FAfiles[$fi]") || die "Cannot open file for reading!\n\n";

    @lines = ();

    while (eof(DATA) <= 0){

        $add = <DATA>;

        # The names loop

        $na=0;

        foreach (@FAnames) {

        $add =~ s/$FAnames[$na]/$FCnames[$na]/g;

        ++$na};

        # Have formatting only for TraditionalForm
	
	$add =~ s/Format\[(.*)\]\s?\:\=/Format[$1, TraditionalForm] :=/g;
	$add =~ s/Format\[(.*)\]\s?\=/Format[$1, TraditionalForm] =/g;

        # Clean up unwanted replacements

        $add =~ s/FALoopNr/LoopNr/g;
        $add =~ s/FALoopPD/LoopPD/g;
        $add =~ s/SetFALoop/SetLoop/g;
        $add =~ s/KinematicFAIndices/KinematicIndices/g;
        $add =~ s/CreateFAFeynAmp/CreateFeynAmp/g;
        $add =~ s/FADiracFASpinor/FADiracSpinor/g;
        $add =~ s/FAFA/FA/g;
        $add =~ s/Cases\[p, HighEnergyPhysics\`FeynCalc\`PropagatorDenominator\`PropagatorDenominator\[__\]\]/Cases[p, HoldPattern[HighEnergyPhysics`FeynCalc`PropagatorDenominator`PropagatorDenominator[__]]]/g;
	$add =~ s/FAHighEnergyPhysics\`FeynCalc\`FeynAmpDenominator\`FeynAmpDenominator/HighEnergyPhysics`FeynCalc`FeynAmpDenominator`FeynAmpDenominator/g;

        push(@lines,$add);

    }

    close(DATA);

    # Update the data file

    open(DATA,">$input_dir/@FAfiles[$fi]") || die "Cannot open file for writing!\n";
    for $ii (@lines) {print DATA "$ii";}
    close(DATA);

    ++$fi
}


################################################################################

open(DATA,"$input_dir/SetUp.m") || die "Cannot open file for reading!\n";

@lines = ();

while (eof(DATA) <= 0){

    $add = <DATA>;

    $add =~ s/\"(.*)\/(.*)\"/\"$1\" \<\> \$PathnameSeparator \<\> \"$2\"/;

    $add =~ s/\$Verbose = 2/\$Verbose := HighEnergyPhysics`FeynCalc`\$VeryVerbose/;

    push(@lines,$add);

}

close(DATA);

open(DATA,">$input_dir/SetUp.m") || die "Cannot open file for writing!\n";
for $ii (@lines) {print DATA "$ii";}
close(DATA);

################################################################################

print "\nMaking small fixes in FeynArts/Analytic.m.\n";

open(DATA,"$input_dir/FeynArts/Analytic.m") || die "Cannot open file for reading!\n";

@lines = ();

while (eof(DATA) <= 0){

    $add = <DATA>;

    # Small bug fix to avoid annoying error messages

    $add =~ s/SequenceForm\[StringTake\[ToString\[type\], 3\], i\]/SequenceForm[StringTake[ToString[type], Min[3,StringLength[ToString[type]]]], i]/;

    push(@lines,$add);

}

close(DATA);

open(DATA,">$input_dir/FeynArts/Analytic.m") || die "Cannot open file for writing!\n";
for $ii (@lines) {print DATA "$ii";}
close(DATA);

################################################################################

print "\nChanging FeynArts/Insert.m.to allow one-vertices\n";

open(DATA,"$input_dir/FeynArts/Insert.m") || die "Cannot open file for reading!\n";

@lines = ();

while (eof(DATA) <= 0){

    $add = <DATA>;

    # Small fix to allow one-vertices

    $add =~ s/DeleteCases\[Take\[\#\, 2\]\, Vertex\[1\]\[_\]\]\&\/\@ top\,/(DeleteCases[Take[\#, 2], Vertex[1][_]] \& \/\@ (top \/. 
          p : Propagator[Internal][___, Vertex[1][_], ___] :> (p \/. 
                Vertex[1] -> Vertex[vertexone]))) \/. vertexone -> 1,/;

    push(@lines,$add);

}

close(DATA);

open(DATA,">$input_dir/FeynArts/Insert.m") || die "Cannot open file for writing!\n";
for $ii (@lines) {print DATA "$ii";}
close(DATA);

################################################################################

print "\nChanging FeynArts/Utilities.m.to allow one-vertices\n";

open(DATA,"$input_dir/FeynArts/Utilities.m") || die "Cannot open file for reading!\n";

@lines = ();

while (eof(DATA) <= 0){

    $add = <DATA>;

    # Small fix to allow one-vertices

    $add =~ s/Union\[ Cases\[top\, Vertex\[n__\]\[_\] \/; \{n\} \=\!\= \{1\}\, \{2\}\] \]/Union[ Join[Cases[Cases[top,Propagator[Internal][__]], Vertex[n__][_], Infinity],Cases[top, Vertex[n__][_] \/; {n} =!= {1}, {2}]] ]/;

    push(@lines,$add);

}

close(DATA);

open(DATA,">$input_dir/FeynArts/Utilities.m") || die "Cannot open file for writing!\n";
for $ii (@lines) {print DATA "$ii";}
close(DATA);

################################################################################

print "\nMaking small fixes in FeynArts/Graphics.m.\n\n";

open(DATA,"$input_dir/FeynArts/Graphics.m") || die "Cannot open file for reading!\n";

@lines = ();

while (eof(DATA) <= 0){

    $add = <DATA>;

    # Display symbols properly in notebooks

    $add =~ s/\, \$Notebooks = False/(*, \$Notebooks = False*)/;

    # Small bug fix to avoid annoying error messages

    $add =~ s/ShortHand\[ type_ \] \:\= StringTake\[ ToString\[type\]\, 3 \]/ShortHand[ type_ ] := StringTake[ ToString[type],Min[3,StringLength[ToString[type]]] ]/;

    # Use TraditionalForm formatting

    $add =~ s/SymbolChar\[ c_ \] \:\= FontForm\[c\, \{\"Symbol\"\, \#\}\]\&/(*SymbolChar[ c_ ] := FontForm[c, {"Symbol", #}]&*)/;

    # Use Mathematica symbols with TeXToPS

    $add =~ s/TextChar\[ c_ \] \:\= FontForm\[c\, \{TextFont\, \#\}\]\&/(*TextChar[ c_ ] := FontForm[c, {TextFont, #}]&*)

form=If[AtomQ[\$FrontEnd],StandardForm,
("Output" \/. (CommonDefaultFormatTypes \/. Options[\$FrontEnd]))];

Global`sq \/: MakeBoxes[Global`sq[], ___] := "";
    
SymbolChar[ c_ ] := StyleForm[form[c], FontFamily->TextFont, FontSize->#]&

TextChar[ c_ ] := StyleForm[form[ToExpression[c]\/.Null->""], FontFamily->TextFont, FontSize->#]&/;

    # Use font Times instead of Helvetica

    $add =~ s/TextFont = "Helvetica"/TextFont = "Times"/;

    # Use Mathematica symbols for arrows

    $add =~ s/End\[\]/

(*Below are the codes for arrows used by Mathematica*)

TeXToPS\[ \"\\\\leftrightarrow\" \] := SymbolChar\["\\[LeftRightArrow\]"\];
TeXToPS\[ \"\\\\leftarrow\" \] := SymbolChar\["\\[LeftArrow\]"\];
TeXToPS\[ \"\\\\rightarrow\" \] := SymbolChar\["\\[RightArrow\]"\];
TeXToPS\[ \"\\\\to\" \] := SymbolChar\["\\[RightArrow\]"\];

End\[\]/;

    push(@lines,$add);

}

close(DATA);

open(DATA,">$input_dir/FeynArts/Graphics.m") || die "Cannot open file for writing!\n";
for $ii (@lines) {print DATA "$ii";}
close(DATA);

}

else{print "\nOK. No files have been changed\n\n";}
