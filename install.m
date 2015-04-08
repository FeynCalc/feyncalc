 (* Author: Rolf Mertig, GluonVision GmbH *)

(* This is an installation file to be called once from within a Mathematica Kernel or FrontEnd like this:

 Import["http://www.feyncalc.org/install.m"]

By default the installation will be done to FileNameJoin[{$UserBaseDirectory,"Applications"}] ,
however,this can be changed by uncommenting e.g.:

$installdirectory = FileNameJoin[{$BaseDirectory,"Applications"}] ];

*)


If[ ("AllowInternetUse" /. SystemInformation["Network"]) === False,
    Print["You have configured Mathematica not to access the internet. Too bad.
	Please check the \"Allow Mathematica to use the Internet\" box in the
    Help \[FilledRightTriangle] Internet Connectivity dialog. Exiting now."];
    Quit[]
];

$ZipFile = "http://www.feyncalc.org/download/fclatest.zip";

If[ !ValueQ[$installdirectory],
    $installdirectory = FileNameJoin[{$UserBaseDirectory,"Applications"}]
];

(* Executing

Import["http://www.feyncalc.org/download/FeynCalcInstall.m"]
    will automatically download the latest version of Feyncalc and
    unzip fclatest.zip to
    FileNameJoin[{$UserBaseDirectory, "Applications"}]

*) BeginPackage["Unzip`",{"JLink`"}] (* Exported symbols added here with SymbolName::usage *)


CopyRemote::usage = "CopyRemote[url, localfilename] copies a file from
an http location to localfilename.";

Unzip::usage = "Unzip[file] unzips file.";

URLFileByteSize::usage = "gives the remote file size in Byte."

Verbose::usage = "Verbose is an option to Unzip."

Begin["`Private`"]

InstallJava[];

Options[Unzip]  =  {Verbose -> True};

Unzip[zipfilein_String?FileExistsQ, dir_: Directory[], OptionsPattern[]] :=
    JavaBlock[
    Module[ {enum, saveEntry, startdir, zf, buf, zipfile, comments, targets, target, len, dirs},
        zipfile = If[ DirectoryName[zipfilein] === "",
                      FileNameJoin[{Directory[],zipfilein}],
                      zipfilein
                  ];
        buf = JavaNew["[B", 10000]; (* ] *)
        If[ startdir =!= dir,
            If[ !DirectoryQ[dir],
                mkdirs[dir]
            ];
            SetDirectory[dir]
        ];
        saveEntry[zipfi_, zipentry_] :=
            JavaBlock[
             Block[ {bos, fi, fos, numRead, stream, outStream, fromcharcode, topdirle},
                 fi = zipentry[getName[]];
                 If[ zipentry[isDirectory[]],
                     mkdirs[FileNameJoin[{dir, fi}]],
                     stream = JavaNew["java.io.BufferedInputStream",
                       zipfi[getInputStream[zipentry]]];
                     outStream =
                      JavaNew["java.io.BufferedOutputStream",
                       JavaNew["java.io.FileOutputStream", FileNameJoin[{dir, fi}]]];
                     While[(numRead = stream[read[buf]]) > 0, outStream[write[buf, 0, numRead]]];
                     stream[close[]];
                     outStream[close[]];
                 ]
             ]];
        zf = JavaNew["java.util.zip.ZipFile", zipfile];
        len = zf[size[]];
        enum = zf[entries[]];
        comments = OptionValue[Verbose] /. Options[Unzip];
        targets = Table[enum[nextElement[]],{len}];
        dirs = Function[x, If[ !DirectoryQ[x],
                               CreateDirectory[x, CreateIntermediateDirectories -> True]
                           ]] /@ (Union[DirectoryName[#[getName[]]]& /@ targets]/."":>Sequence[]);
        Do[
               If[ comments,
                   Print[StringJoin["extracting: ", FileNameJoin[{dir, StringReplace[target[getName[]], "/" -> $PathnameSeparator]}]]]
               ];
               saveEntry[zf, target],
           {target, targets}
        ];
        zf @ close[];
        dir
    ]];


(*
Example usage:
CopyRemote["http://www.mertig.com/mathdepot/buttons/ButtonTools.nb",
ToFileName[{$UserAddOnsDirectory,"SystemFiles","FrontEnd","Palettes"},
"ButtonTools.nb"]]
*)

(* You need JLink 2.0 or higher.
 this code is based on the GetRemote example in the JLink
 documentation *)

Options[CopyRemote] = {ProxyHost :> None, ProxyPort :> None};

CopyRemote[url_String /; StringMatchQ[url, "http://*.*", IgnoreCase-> True],
localfile_:Automatic, opts___?OptionQ] :=
    (
    Needs["JLink`"];
    JLink`JavaBlock[
        Module[ {u, stream, numRead, outFile, buf, prxyHost, prxyPort},
            {prxyHost, prxyPort} = {ProxyHost, ProxyPort} /.
            Flatten[{opts}] /. Options[CopyRemote];
            JLink`InstallJava[];
            If[ StringQ[prxyHost],
                (* Set properties to force use of proxy. *)
                JLink`SetInternetProxy[prxyHost, prxyPort]
            ];
            u = JLink`JavaNew["java.net.URL", url];
            (* This is where the error will show up if the URL is not valid.
               A Java exception will be thrown during openStream, which
               causes the method to return $Failed.
            *)
            stream = u@openStream[];
            If[ stream === $Failed,
                Return[$Failed]
            ];
            buf = JLink`JavaNew["[B", 5000];
            (* 5000 is an arbitrary buffer size *)
            If[ StringQ[localfile],
                outFile = OpenWrite[localfile, DOSTextFormat -> False],
                outFile = OpenTemporary[DOSTextFormat->False];
            ];
            While[(numRead = stream@read[buf]) > 0,
             WriteString[outFile, FromCharacterCode[If[ # < 0,
                                                        #+256,
                                                        #
                                                    ]& /@ Take[JLink`Val[buf], numRead]]]
            ];
            stream@close[];
            Close[outFile]
        (* Close returns the filename *)
        ]
    ] );



URLFileByteSize[link_String] :=
    URLFileByteSize[link] =
    Module[ {url, urlcon, len},
        url = JavaNew["java.net.URL", link];
        urlcon = url@openConnection[];
        len = urlcon@getContentLength[];
        urlcon@getInputStream[]@close[];
        len
    ];

End[]

EndPackage[]

Module[ {ziplocal, fcfilesize},
    If[ !DirectoryQ[$installdirectory],
        CreateDirectory[$installdirectory]
    ];
    Print["downloading ", $ZipFile,"   please wait "];
    ziplocal = FileNameJoin[{ $installdirectory, FileNameTake @ $ZipFile}];

    (* get rid of previous download *)
    If[ FileExistsQ[ziplocal],
        DeleteFile@ziplocal
    ];
    fcfilesize = Unzip`URLFileByteSize[$ZipFile];
    If[ (Head[$FrontEnd]===System`FrontEndObject)  && (Global`$FCProgressDisplay =!= False),
        PrintTemporary @  (* this way it does not get saved which is good *)
        Dynamic@Row[{"Downloading ", Round[fcfilesize/1024^2]," MB from ",
        If[ StringQ[Setting@#],
            #,
            " "
        ] &@$ZipFile, " ",
        ProgressIndicator[
         Quiet[If[ ! NumberQ[#],
                   0,
                   #
               ] &@( Refresh[FileByteCount[ziplocal],
                     UpdateInterval -> .01]/fcfilesize )]],
        " ", If[ ! NumberQ[Setting@#],
                 0,
                 #
             ] &@
         Refresh[FileByteCount[ziplocal]/1024.^2, UpdateInterval -> .02],
        " MByte"
        }],
        Print["Downloading ", Round[fcfilesize/1024^2]," MB from ", $ZipFile]
    ];
    CopyRemote[$ZipFile, ziplocal];
    Print["Downloading done, installing FeynCalc to ", Style[$installdirectory, FontWeight -> "Bold"]];
    Unzip[ziplocal, $installdirectory, Verbose -> False];
    Print["installation of FeynCalc ready."];
    Print["loading FeynCalc "];

    (*
    $LoadFeynArts=True; $LoadPhi = True; $LoadTARCER = False;
    *)
    (* for the moment: do not care about patching FeynArts *)
    If[ !ValueQ[$LoadFeynArts],
        $LoadFeynArts = False
    ];
    If[ !ValueQ[$LoadPhi],
        $LoadPhi = False
    ];
    If[ !ValueQ[$LoadTARCER],
        $LoadTARCER = False
    ];

	(* Clear the help index *)
	helpIndexFiles = FileNames[FileNameJoin[{$UserBaseDirectory,
		"FrontEnd", "*", "*",   "SystemFiles", "FrontEnd", "TextResources",   "HelpBrowserSetup.pbf"}]];
	DeleteFile /@ helpIndexFiles;

    (* check if FeynCalc is installed. If not, install it *)
    Which [
           FindFile["FeynCalc`"] =!= $Failed, (* for FC 9 *)
           Needs["FeynCalc`"],
           FindFile["FeynCalc`"] =!= $Failed,
           Needs["FeynCalc`"],
           True,
           Print["Installation of FeynCalc failed!, please try again."]
    ];
];
