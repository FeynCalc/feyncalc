(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: install															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Installs FeynCalc and FeynArts *)

(* ------------------------------------------------------------------------ *)

InstallFeynCalc::notcomp =
"Your Mathematica version is too old. FeynCalc requires at least Mathematica 8. Installation aborted!";
InstallFeynCalc::failed =
"Download of `1` failed. Installation aborted!";

AutoOverwriteFeynCalcDirectory::usage="AutoOverwriteFeynCalcDirectory is an option of InstallFeynCalc. If \
set to True, the existing FeynCalc directory will be deleted without any further notice. The default
value None means that the user will be asked by a dialog. False means that the directory will be overwritten.";

AutoDisableInsufficientVersionWarning::usage="AutoDisableInsufficientVersionWarning is an option of InstallFeynCalc. If \
set to True, warning messages for notebooks that were created with a newer Mathematica version will be silently disabled. \
This is needed to use FeynCalc documentation in Mathematica 8 and 9, since otherwise the warning message will appear every \
time one opens a help page for a FeynCalc function. The default value None means that the user will be asked by a dialog. \
False means that the warning will not be disabled.";

FeynArtsMirrorLink::usage="FeynArtsMirrorLink is an option of InstallFeynCalc. It specifies the url \
to the mirror repository of FeynArts. This repository is maintained by FeynCalc developers and tries to follow \
the development of FeynArts using git. It is also used to install FeynArts with InstallFeynCalc.";

AutoInstallFeynArts::usage="AutoInstallFeynArts is an option of InstallFeynCalc. If \
set to True, FeynArts will be installed automatically.";

FeynCalcDevelopmentVersionLink::usage="FeynCalcDevelopmentVersionLink is an option of InstallFeynCalc. It specifies the url \
to the main repository of FeynCalc. This repository is used to install the development version of FeynCalc.";

FeynCalcStableVersionLink::usage="FeynCalcStableVersionLink is an option of InstallFeynCalc. It specifies the url \
to the latest stable release of FeynCalc.";

InstallFeynCalcDevelopmentVersion::usage="InstallFeynCalcDevelopmentVersion is an option of InstallFeynCalc. If \
set to True, the installer will download the latest development version of FeynCalc from the git repository. \
Otherwise it will install the latest stable version.";

InstallFeynCalcTo::usage="InstallFeynCalcTo is an option of InstallFeynCalc. It specifies, the full path \
to the directory where FeynCalc will be installed.";

InstallFeynArtsTo::usage="InstallFeynArtsTo is an option of InstallFeynArts. It specifies, the full path \
to the directory where FeynArts will be installed.";

If[  $VersionNumber == 8,
(*To use FetchURL in MMA8 we need to load URLTools first *)
Needs["Utilities`URLTools`"];
];

Options[InstallFeynCalc]={
	AutoDisableInsufficientVersionWarning->None,
	AutoInstallFeynArts->None,
	AutoOverwriteFeynCalcDirectory->None,
	FeynCalcDevelopmentVersionLink->"https://github.com/FeynCalc/feyncalc/archive/master.zip",
	FeynCalcStableVersionLink->"https://github.com/FeynCalc/feyncalc/archive/hotfix-stable.zip",
	InstallFeynCalcDevelopmentVersion->False,
	InstallFeynCalcTo->FileNameJoin[{$UserBaseDirectory, "Applications","FeynCalc"}]
};

Options[InstallFeynArts]={
	FeynArtsMirrorLink->"https://github.com/FeynCalc/feynarts-mirror/archive/master.zip",
	InstallFeynArtsTo->FileNameJoin[{$UserBaseDirectory, "Applications","FeynCalc","FeynArts"}]
};

InstallFeynArts[OptionsPattern[]]:=
	Module[{tmpzip,fazip,FCGetUrl,unzipDir,faDir},
		(* Install FeynArts	*)

		faDir=OptionValue[InstallFeynArtsTo];
		fazip = OptionValue[FeynArtsMirrorLink];

		If[$VersionNumber == 8,
			(*To use FetchURL in MMA8 we need to load URLTools first *)
			FCGetUrl[x_]:= Utilities`URLTools`FetchURL[x],
			FCGetUrl[x_]:= URLSave[x,CreateTemporary[]]
		];


		WriteString["stdout", "Downloading FeynArts from ", fazip," ..."];
		tmpzip=FCGetUrl[fazip];
		unzipDir= tmpzip<>".dir";
		WriteString["stdout", "done! \n"];

		(* Extract to the content	*)
		WriteString["stdout", "FeynArts zip file was saved to ", tmpzip,".\n"];
		WriteString["stdout", "Extracting FeynArts zip file to ", faDir, " ..."];
		ExtractArchive[tmpzip, unzipDir];
		WriteString["stdout", "done! \n"];

		(* Move the files to the final destination	*)
		WriteString["stdout", "Copying FeynArts to ", faDir, " ..."];
		CopyDirectory[FileNameJoin[{unzipDir,"feynarts-mirror-master"}],faDir];
		WriteString["stdout", "done! \n"];

		(* Delete the downloaded file	*)
		Quiet@DeleteFile[tmpzip];

		(* Delete the extracted archive *)
		Quiet@DeleteDirectory[unzipDir, DeleteContents -> True];

	];


InstallFeynCalc[OptionsPattern[]]:=
	Module[{	unzipDir, tmpzip, gitzip, packageName, packageDir,
				strDisableWarning,strFeynArts,FCGetUrl,
				strOverwriteFCdit, faInstalled, zipDir},

	If[OptionValue[InstallFeynCalcDevelopmentVersion],
		gitzip = OptionValue[FeynCalcDevelopmentVersionLink];
		zipDir = "feyncalc-master",
		gitzip = OptionValue[FeynCalcStableVersionLink];
		zipDir = "feyncalc-hotfix-stable"
	];
	faInstalled=False;

	packageName = "FeynCalc";
	packageDir = OptionValue[InstallFeynCalcTo];

strDisableWarning="To make the documentation work, we need to disable the warning that appears \
when you open a notebook that was created with a newer Mathematica version. Otherwise this \
warning will pop up every time you use the Documentation Center to read info on FeynCalc functions \
in Mathematica 8 and 9. This setting is harmless and can be always undone via \
\"SetOptions[$FrontEnd, MessageOptions -> {\"InsufficientVersionWarning\" -> True}]\". Should we do this now?";

strFeynArts="Do you want to install FeynArts from "<> OptionValue[InstallFeynArts,FeynArtsMirrorLink] <> "? FeynArts is a Feynman diagram \
generator which is currently developed by Thomas Hahn (www.feynarts.de). It is not a part of FeynCalc but it \
can be used together with FeynCalc after some patching. The patched version will be located in the directory \"FeynArts\"
inside your FeynCalc installation.";

strOverwriteFCdit="Looks like FeynCalc is already installed. Do you want to replace the content \
of " <> packageDir <> " with the downloaded version of FeynCalc? If you are using any custom configuration \
files or add-ons that are located in that directory, please backup them in advance.";

	If[$VersionNumber < 8,
		Message[InstallFeynCalc::notcomp];
		Abort[]
	];

	If[$VersionNumber == 8,
		(*To use FetchURL in MMA8 we need to load URLTools first *)
		FCGetUrl[x_]:= Utilities`URLTools`FetchURL[x],
		FCGetUrl[x_]:= URLSave[x,CreateTemporary[]]
	];


	(* If the package directory already exists, ask the user about overwriting *)
	If[ DirectoryQ[packageDir],

		If[ OptionValue[AutoOverwriteFeynCalcDirectory],

			Quiet@DeleteDirectory[packageDir, DeleteContents -> True],

			Null,
			If[ ChoiceDialog[strOverwriteFCdit,{"Yes, overwrite the " <> packageName <>" directory"->True,
				"No! I need to do a backup first."->False}],
				Quiet@DeleteDirectory[packageDir, DeleteContents -> True],
				Abort[]
			]
		]
	];

	(* Download FeynCalc tarball	*)
	WriteString["stdout", "Downloading FeynCalc from ", gitzip," ..."];
	tmpzip=FCGetUrl[gitzip];
	unzipDir= tmpzip<>".dir";
	WriteString["stdout", "done! \n"];

	(* Extract to the content	*)
	WriteString["stdout", "FeynCalc zip file was saved to ", tmpzip,".\n"];
	WriteString["stdout", "Extracting FeynCalc zip file to ", unzipDir, " ..."];
	ExtractArchive[tmpzip, unzipDir];
	WriteString["stdout", "done! \n"];

	(* Delete the downloaded file	*)
	Quiet@DeleteFile[tmpzip];

	(* Move the files to the final destination	*)
	WriteString["stdout", "Copying "<>packageName<>" to ", packageDir, " ..."];
	CopyDirectory[FileNameJoin[{unzipDir,zipDir,"FeynCalc"}],packageDir];
	WriteString["stdout", "done! \n"];
	(* Delete the extracted archive *)
	Quiet@DeleteDirectory[unzipDir, DeleteContents -> True];

	(* Activate the documentation	*)
	WriteString["stdout", "Setting up the help system... "];
	RenameDirectory[FileNameJoin[{packageDir,"DocOutput"}],FileNameJoin[{packageDir,"Documentation"}]];
	Quiet@DeleteDirectory[FileNameJoin[{packageDir,"DocSource"}], DeleteContents -> True];

	If[ OptionValue[AutoDisableInsufficientVersionWarning],

		SetOptions[$FrontEnd, MessageOptions -> {"InsufficientVersionWarning" -> False}],

		Null,
		If[ ChoiceDialog[strDisableWarning],
			SetOptions[$FrontEnd, MessageOptions -> {"InsufficientVersionWarning" -> False}]
		]
	];


	WriteString["stdout", "done! \n"];

	If[ OptionValue[AutoInstallFeynArts],

		faInstalled=True;
		InstallFeynArts[InstallFeynArtsTo->FileNameJoin[{packageDir,"FeynArts"}]],
		Null,
		If[ ChoiceDialog[strFeynArts],
			faInstalled=True;
			InstallFeynArts[InstallFeynArtsTo->FileNameJoin[{packageDir,"FeynArts"}]]
		]
	];


	WriteString["stdout", "\nInstallation complete! Loading FeynCalc... \n"];

	If[	faInstalled,
		Global`$LoadFeynArts=True;
	];
	Get["FeynCalc`"];

];
