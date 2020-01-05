(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: install															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary:  Installs FeynCalc and FeynArts *)

(* ------------------------------------------------------------------------ *)

InstallFeynCalc::notcomp =
"Your Mathematica version is too old. FeynCalc requires at least Mathematica 8. Installation aborted!";

InstallFeynCalc::failed =
"Download of `1` failed. Installation aborted!";

InstallFeynCalcQuiet::usage="InstallFeynCalcQuiet is the silent mode of installing FeynCalc, where the \
installer does not ask you any questions but silently overwrites any existing FeynCalc installation and \
modifies Mathematica's options accordingly. FeynArts is not installed. The main purpose of this mode is \
to facilitate the installation of FeynCalc on Mathematica Online.";

AutoEnableTraditionalForm::usage="AutoEnableTraditionalForm is an option of InstallFeynCalc. If \
set to True, the format type of new output cells will be set to TraditionalForm. False means that \
the current value will not be changed.";

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

$PathToFCArc::usage="$PathToFCArc specifies where the installer should look for the zipped FeynCalc version. \
If the value is not empty, the installer will use the specified file instead of downloading it from the official \
website."

$PathToFAArc::usage="$PathToFAArc specifies where the installer should look for the FeynArts tarball. \
If the value is not empty, the installer will use the specified file instead of downloading it from the official \
website."

If[ !ValueQ[$PathToFCArc],
	$PathToFCArc = ""
];

If[ !ValueQ[$PathToFAArc],
	$PathToFAArc = ""
];

If[  $VersionNumber == 8,
(*To use FetchURL in MMA8 we need to load URLTools first *)
Needs["Utilities`URLTools`"];
];

Options[InstallFeynCalc]={
	AutoDisableInsufficientVersionWarning-> None,
	AutoEnableTraditionalForm -> None,
	AutoInstallFeynArts-> None,
	AutoOverwriteFeynCalcDirectory-> None,
	FeynCalcDevelopmentVersionLink->"https://github.com/FeynCalc/feyncalc/archive/master.zip",
	FeynCalcStableVersionLink->"https://github.com/FeynCalc/feyncalc/archive/hotfix-stable.zip",
	InstallFeynCalcDevelopmentVersion->False,
	InstallFeynCalcTo->FileNameJoin[{$UserBaseDirectory, "Applications","FeynCalc"}]
};

Options[InstallFeynCalcQuiet]=
	Options[InstallFeynCalc];

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

		(* Download FeynArts tarball	*)
		If[ $PathToFAArc=!="",
			tmpzip = $PathToFAArc;
			WriteString["stdout", "Installing FeynArts from ", tmpzip," ..."],
			WriteString["stdout", "Downloading FeynArts from ", fazip," ..."];
			tmpzip=FCGetUrl[fazip];
		];
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
		If[ $PathToFAArc==="",
			Quiet@DeleteFile[tmpzip];
		];

		(* Delete the extracted archive *)
		Quiet@DeleteDirectory[unzipDir, DeleteContents -> True];

	];

InstallFeynCalcQuiet[]:=
	InstallFeynCalc[
		AutoDisableInsufficientVersionWarning-> True,
		AutoEnableTraditionalForm -> True,
		AutoInstallFeynArts-> False,
		AutoOverwriteFeynCalcDirectory-> True
	];

InstallFeynCalc[OptionsPattern[]]:=
	Module[{	unzipDir, tmpzip, gitzip, packageName, packageDir, fullPath,
				strDisableWarning,strFeynArts,FCGetUrl, configFileProlog,
				strOverwriteFCdit, faInstalled, zipDir, strEnableTraditionalForm,
				useTraditionalForm, configFile},

	If[OptionValue[InstallFeynCalcDevelopmentVersion],
		gitzip = OptionValue[FeynCalcDevelopmentVersionLink],
		gitzip = OptionValue[FeynCalcStableVersionLink]
	];

	faInstalled=False;
	useTraditionalForm=False;

	packageName = "FeynCalc";
	packageDir = OptionValue[InstallFeynCalcTo];

strDisableWarning="To make the documentation work, we need to disable the warning that appears \
when you open a notebook that was created with a newer Mathematica version. Otherwise this \
warning will pop up every time you use the Documentation Center to read info on FeynCalc functions \
in Mathematica 8 and 9. This setting is harmless and can be always undone via \
\"SetOptions[$FrontEnd, MessageOptions -> {\"InsufficientVersionWarning\" -> True}]\". Should we do this now?";

strEnableTraditionalForm="FeynCalc makes an extensive use of Mathematica's typesetting capabilities to \
format the output in a nice and easily readable manner. However, the built-in typesetting is available \
only if the format type of new output cells is set to TraditionalForm. The default value is StandardForm. \
Do you want to allow FeynCalc to change the default output format to TraditionalForm whenever it is loaded? \
This will only affect the current FeynCalc front end session and will not influence any subsequent Mathematica \
sessions, i.e. the changes are not persistent.";

strFeynArts="Do you want to install FeynArts from "<> OptionValue[InstallFeynArts,FeynArtsMirrorLink] <> "? FeynArts is a Feynman diagram \
generator which is currently developed by Thomas Hahn (www.feynarts.de). It is not a part of FeynCalc but it \
can be used together with FeynCalc after some patching. The patched version will be located in the directory \"FeynArts\"
inside your FeynCalc installation.";

strOverwriteFCdit="Looks like FeynCalc is already installed. Do you want to replace the content \
of " <> packageDir <> " with the downloaded version of FeynCalc? If you are using any custom configuration \
files or add-ons that are located in that directory, please backup them in advance.";

configFileProlog ="(*Here you can put some commands and settings to be evaluated on every start of FeynCalc. \n
This allows you to customize your FeynCalc installation to fit your needs best.*)";

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
				"No, I need to do a backup first. Abort installation."->False}, WindowFloating->True, WindowTitle->"Existing FeynCalc Installation detected"],
				Quiet@DeleteDirectory[packageDir, DeleteContents -> True],
				Abort[]
			]
		]
	];

	(* Download FeynCalc tarball	*)
	If[ $PathToFCArc=!="",
		tmpzip = $PathToFCArc;
		WriteString["stdout", "Installing FeynCalc from ", tmpzip," ..."],
		WriteString["stdout", "Downloading FeynCalc from ", gitzip," ..."];
		tmpzip=FCGetUrl[gitzip];
	];

	If[tmpzip===$Failed,
		WriteString["stdout", "\nFailed to download FeynCalc. Please check your interent connection.\nInstallation aborted!"];
		Abort[],

		unzipDir= tmpzip<>".dir";
		WriteString["stdout", "done! \n"];
	];

	(* Extract to the content	*)
	WriteString["stdout", "FeynCalc zip file was saved to ", tmpzip,".\n"];
	WriteString["stdout", "Extracting FeynCalc zip file to ", unzipDir, " ..."];

	If[	ExtractArchive[tmpzip, unzipDir]===$Failed,
		WriteString["stdout", "\nFailed to extract the FeynCalc zip. The file might be corrupted.\nInstallation aborted!"];
		Abort[],
		WriteString["stdout", "done! \n"];
		(* Delete the downloaded file	*)
		If[ $PathToFCArc==="",
			Quiet@DeleteFile[tmpzip];
		]
	];

	WriteString["stdout", "Recognizing the directory structure..."];
	zipDir = FileNames["FeynCalc.m", unzipDir, Infinity];
	If[ Length[zipDir]===1,
		fullPath = DirectoryName[zipDir[[1]]];
		zipDir = Last[FileNameSplit[DirectoryName[zipDir[[1]]]]];
		WriteString["stdout", "done! \n"],
		WriteString["stdout", "\nFailed to recognize the directory structure of the downloaded zip file. \nInstallation aborted!"];
		Abort[]
	];

	(* Move the files to the final destination	*)
	WriteString["stdout", "Copying "<>packageName<>" to ", packageDir, " ..."];

	If[	CopyDirectory[fullPath,packageDir]===$Failed,
		WriteString["stdout", "\nFailed to copy "  <>fullPath<>" to ", packageDir <>". \nInstallation aborted!"];
		Abort[],
		WriteString["stdout", "done! \n"];
		(* Delete the extracted archive *)
		Quiet@DeleteDirectory[unzipDir, DeleteContents -> True];
	];

	(* Activate the documentation	*)
	WriteString["stdout", "Setting up the help system ... "];
	RenameDirectory[FileNameJoin[{packageDir,"DocOutput"}],FileNameJoin[{packageDir,"Documentation"}]];
	Quiet@DeleteDirectory[FileNameJoin[{packageDir,"DocSource"}], DeleteContents -> True];

	If[ OptionValue[AutoDisableInsufficientVersionWarning],

		SetOptions[$FrontEnd, MessageOptions -> {"InsufficientVersionWarning" -> False}],

		Null,
		If[ ChoiceDialog[strDisableWarning, WindowFloating->True, WindowTitle->"Documentation system"],
			SetOptions[$FrontEnd, MessageOptions -> {"InsufficientVersionWarning" -> False}]
		]
	];

	(* Activate TraditionalForm	*)
	WriteString["stdout", "Setting up the format type of new output cells ... "];
	If[ OptionValue[AutoEnableTraditionalForm],

		useTraditionalForm = True,
		Null,
		If[ ChoiceDialog[strEnableTraditionalForm, WindowFloating->True, WindowTitle->"TraditionalForm output"],
			useTraditionalForm = True
		]
	];

	WriteString["stdout", "done! \n"];

	(* To have the documentation available immediately after installing FeynCalc (following the advice of Szabolcs Horv'at) *)
	RebuildPacletData[];

	(* Generate FCConfig.m	*)
	WriteString["stdout", "Creating the configuration file ... "];
	configFile = StringJoin[configFileProlog, "\n\n(* Activate TraditionalForm output for each FeynCalc session *) \n$FCTraditionalFormOutput="<>ToString[useTraditionalForm]<>";"];
	Export[FileNameJoin[{packageDir,"FCConfig.m"}], configFile, "Text"];
	WriteString["stdout", "done! \n"];

	If[ OptionValue[AutoInstallFeynArts],

		faInstalled=True;
		InstallFeynArts[InstallFeynArtsTo->FileNameJoin[{packageDir,"FeynArts"}]],
		Null,
		If[ ChoiceDialog[strFeynArts, WindowFloating->True, WindowTitle->"Install FeynArts"],
			faInstalled=True;
			InstallFeynArts[InstallFeynArtsTo->FileNameJoin[{packageDir,"FeynArts"}]]
		]
	];


	WriteString["stdout", "\nInstallation complete! Loading FeynCalc ... \n"];

	If[	faInstalled,
		If[	InstallFeynCalcDevelopmentVersion,
			Global`$LoadAddOns={"FeynArtsLoader"},
			Global`$LoadFeynArts=True
		]
	];
	Get["FeynCalc`"];

];
