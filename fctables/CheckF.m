(* :Title: CheckF *)

(* :Author: Frederik Orellana *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 October 2003 at 21:52 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctables`CheckF`",
             "HighEnergyPhysics`FeynCalc`"];

CheckF::"usage" = 
    "CheckF[exp,fil] does the following:  Checks if the setting of the option Directory \
is a valid directory name and if fil is a valid file name. \
Checks if file exists. If it does, Gets and evaluates fil. \
If fil does not exist, evaluates exp and saves it to fil. \
Saving and evaluating can be further controlled with the options ForceSave and NoSave. \
If the option Check is set to False the return value is what is evaluated (see above). \
If Check is set to True the return value is True or False depending on \
whether the evaluation of exp agrees with what is loaded from fil or fil does not \
exist. \
Default value of Check : False. \
NOTICE : If fil ends with \".Gen\" or \".Mod\", \
the setting of Directory is ignored and fil is saved in the \"CouplingVectors\" \
subdirectory of \"Phi\". If fil ends with \".Fac\", \
the setting of Directory is ignored and fil is saved in the \"Factors\" \
subdirectory of \"Phi\". If fil is a file name with full path, the setting of Directory \
is also ignored.";

ForceSave::"usage" = 
    "ForceSave is an option of CheckF. Setting it to True forces the first argument to \
be evaluated even if the file specified by the second argument exists.  \
The expression is also saved if setting NoSave is set to False.  Default value : False.";

NoSave::"usage" = 
    "NoSave is an option of CheckF. If set to True, no results will ever be saved to disk. \
It is there to allow evaluating notebooks using CheckF without having to worry about \
overwriting old results (SetOptions[CheckF,NoSave->True]).  Default value : False.";

CheckF::"nostring" = 
    "`1` is not a string.  Please give the file name as a string.";
    
CheckF::"baddir" = 
    "`1` is not a valid directory.  Please set the option Directory \
correctly.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

(* ------------------------------------------------------------------------ *)

Options[CheckF] = {Directory -> ToFileName[{$FeynCalcDirectory}, "fcdb"],
    ForceSave -> False, NoSave -> False, Check -> False};

(* ------------------------------------------------------------------------ *)

VerbosePrint[n_Integer,s__]:=If[$VeryVerbose>=n,Print[s]];

eliminateDoubles[s_String] :=
    Block[{str},
      str = FixedPoint[
          StringReplace[#,
              Evaluate[$PathnameSeparator <> $PathnameSeparator] ->
                Evaluate[$PathnameSeparator]] &, s];
      If[StringMatchQ[str, "*" <> $PathnameSeparator], StringDrop[str, -1],
        str]];


SetAttributes[CheckF, HoldFirst];

CheckF[ex_, fi_, opts : ((_Rule | {___Rule}) ...)] :=
    Block[{dir, file, finex, fs, ns, checkok=True},

      If[StringQ[fi] =!= True, Message[CheckF::nostring, fi];
        Return[ex]];

      Which[
         StringMatchQ[fi,"*.Gen"]===True||StringMatchQ[fi,"*.Mod"]===True,
           dir = eliminateDoubles[HighEnergyPhysics`FeynCalc`$FeynCalcDirectory <>
	       $PathnameSeparator <>
	       "Phi" <> $PathnameSeparator <> "CouplingVectors"],
          StringMatchQ[fi,"*.Fac"]===True || StringMatchQ[fi,"*.Mass"]===True,
           dir = eliminateDoubles[HighEnergyPhysics`FeynCalc`$FeynCalcDirectory <>
	       $PathnameSeparator <>
	       "Phi" <> $PathnameSeparator <> "Factors"],
	      True,
           dir = (Directory /. Flatten[{opts}] /. Options[CheckF])
      ];

      Which[

        (*File name given with full path*)
        DirectoryName[fi] =!= "",
        If[FileType[DirectoryName[fi]] === Directory, file = fi],

        (*Directory specified ok*)
        FileType[dir] === Directory,
        file = eliminateDoubles[dir <> $PathnameSeparator <> fi],

        (*Directory specified not ok, try Directory[]*)

        FileType[eliminateDoubles[Directory[] <> $PathnameSeparator <> dir]] ===
           Directory,
        file = eliminateDoubles[
              Directory[] <> $PathnameSeparator <> dir <> $PathnameSeparator <>
                 fi,

        True, (Message[CheckF::baddir, dir]; Return[ex])];

        ];

      VerbosePrint[1, "Using file name " <> file];

      fs=(ForceSave/.Flatten[{opts}]/.Options[CheckF]);
      ns=(NoSave/.Flatten[{opts}]/.Options[CheckF]);
      ch=(Check/.Flatten[{opts}]/.Options[CheckF]);

      If[FileType[file] === None || fs === True,
      If[FileType[file] === None,
        VerbosePrint[1, "File does not exist, evaluating"],
        If[fs,VerbosePrint[1, "File exists, force evaluating"]]];
        finex = Evaluate[ReleaseHold[ex]];
        If[ns,
          VerbosePrint[1, "NoSave set to True, will evaluate but not save"],
          VerbosePrint[1, "Saving"];
          Put[finex, file]],
      VerbosePrint[1, "File exists, loading"];
        finex = Get[file];
        If[ch,
          VerbosePrint[1, "File exists, comparing"];
          finex = (Expand[Evaluate[ReleaseHold[ex]] - finex] === 0)]
      ];

      finex

      ];

End[]; EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "CheckF | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
