(* :Title: CheckDB *)

(* :Author: Frederik Orellana *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 October 2003 at 21:52 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

CheckDB::usage =
"CheckDB[exp, fil] saves (with Put) or retrieves (with Get) exp from a file
fil. It checks if the setting of the option Directory is a valid directory
name and if fil is a valid file name and does exist. If it does, Get[fil] is
executed. If fil does not exist, exp gets evaluated and saved to fil.

Saving and evaluating can be further controlled with the options ForceSave and
NoSave. If the option Check is set to False the return value is what is
evaluated (see above). If Check is set to True the return value is True or
False depending on whether the evaluation of exp agrees with what is loaded
from fil or fil does not exist.

Default value of Check : False. If fil ends with \".Gen\" or \".Mod\", the
setting of Directory is ignored and fil is  saved in the \"CouplingVectors\" 
subdirectory of \"Phi\". If fil ends with  \".Fac\", the setting of Directory
is  ignored and fil is saved in the \"Factors\" subdirectory of \"Phi\".

If fil is a file name with full path, the setting of Directory is also
ignored.";

ForceSave::usage =
"ForceSave is an option of CheckDB. Setting it to True forces the first
argument to be evaluated even if the file specified by the second argument
exists.  The expression is also saved if NoSave is set to False. Default
value: False.";

NoSave::usage =
"NoSave is an option of CheckDB. If set to True, no results will ever be saved
to disk. It is there to allow evaluating notebooks using CheckDB without
having to worry about overwriting old results
(SetOptions[CheckDB,NoSave->True]).  Default value: False.";

CheckDB::"nostring" =
"`1` is not a string.  Please give the file name as a string.";

CheckDB::"baddir" =
"`1` is not a valid directory.  Please set the option Directory correctly.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`CheckDB`Private`"]

(* ------------------------------------------------------------------------ *)

Options[CheckDB] = {Directory :> ToFileName[{$FeynCalcDirectory}, "Database"],
		ForceSave -> False, NoSave -> False, Check -> False};

(* ------------------------------------------------------------------------ *)


eliminateDoubles[s_String] :=
		Block[{str},
			str = FixedPoint[
					StringReplace[#,
							Evaluate[$PathnameSeparator <> $PathnameSeparator] ->
								Evaluate[$PathnameSeparator]] &, s];
			If[StringMatchQ[str, "*" <> $PathnameSeparator], StringDrop[str, -1],
				str]];


SetAttributes[CheckDB, HoldFirst];

CheckDB[ex_, fi_, opts : ((_Rule | {___Rule}) ...)] :=
		Block[{ch, dir, file, finex, fs, ns, checkok=True},

			If[StringQ[fi] =!= True, Message[CheckDB::nostring, fi];
				Return[ex]];

			Which[
				StringMatchQ[fi,"*.Gen"]===True||StringMatchQ[fi,"*.Mod"]===True,
					dir = eliminateDoubles[$FeynCalcDirectory <>
				$PathnameSeparator <>
				"Phi" <> $PathnameSeparator <> "CouplingVectors"],
					StringMatchQ[fi,"*.Fac"]===True || StringMatchQ[fi,"*.Mass"]===True,
					dir = eliminateDoubles[$FeynCalcDirectory <>
				$PathnameSeparator <>
				"Phi" <> $PathnameSeparator <> "Factors"],
				True,
					dir = (Directory /. Flatten[{opts}] /. Options[CheckDB])
			];

			Which[

				(*File name given with full path*)
				DirectoryName[fi] =!= "",
				If[FileType[DirectoryName[fi]] === Directory, file = fi],

				(*Directory specified ok*)
				FileType[dir] === Directory,
				file = eliminateDoubles[dir <> $PathnameSeparator <> fi],

				(*Directory specified not ok, try Directory[]*)
				FileType[eliminateDoubles[Directory[] <> $PathnameSeparator <> dir]] === Directory,
				file = eliminateDoubles[  Directory[] <> $PathnameSeparator <> dir <> $PathnameSeparator <> fi],

				True,
				(Message[CheckDB::baddir, dir];
				Return[ex]);

				];

			FCPrint[1, "Using file name " <> file];

			fs=(ForceSave/.Flatten[{opts}]/.Options[CheckDB]);
			ns=(NoSave/.Flatten[{opts}]/.Options[CheckDB]);
			ch=(Check/.Flatten[{opts}]/.Options[CheckDB]);

			If[(FileType[file] === None || fs === True) && ch === False,

				If[FileType[file] === None,
					FCPrint[1, "File does not exist, evaluating"],
					If[fs,FCPrint[1, "File exists, force evaluating"]]
				];
				finex = Evaluate[ReleaseHold[ex]];
				If[ns,
					FCPrint[1, "NoSave set to True, will evaluate but not save"],
					FCPrint[1, "Saving"];
					Put[finex, file]
				],

				If[FileType[file] === None && fs =!= True,
					FCPrint[1, "File does not exist, cannot load"];
					finex = ex,
					FCPrint[1, "File exists, loading"];
					finex = Get[file];
					If[ch,
						FCPrint[1, "File exists, comparing"];
						finex = (Expand[Evaluate[ReleaseHold[ex]] - finex] === 0)
					]
				];

			];

			finex

			];

FCPrint[1,"CheckDB.m loaded."];
End[]
