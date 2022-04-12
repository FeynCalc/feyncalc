(* ::Package:: *)

$FeynCalcStartupMessages=False;
<<FeynCalc`


QuitAbort[]:=
If[$FrontEnd===Null,
	Quit[],
	Abort[]
];


fcSymbols=Names["FeynCalc`*"];
docFiles=FileBaseName/@FileNames["*.m",FileNameJoin[{$FeynCalcDirectory,"Documentation","Mathematica"}],Infinity];


in=Import[FileNameJoin[{$FeynCalcDirectory,"Documentation","Markdown","Extra","FeynCalc.md"}],"Text"];
str=StringCases[in,"- "~~Shortest[x__]~~" - "/;!StringFreeQ[x,"["]:>x];
overviewSymbols=StringReplace[Union[Flatten[StringCases[#,"["~~Shortest[x__]~~"]":>x]&/@StringSplit[str,","]]],"\\"->""];


If[!DirectoryQ[outputDir],
	Print["ERROR! The directory ", outputDir, " does not exist!" ];
	QuitAbort[]
];


getTitlesFirst[str_]:=
Block[{tmp,res},
tmp=StringCases[str,"- "~~Shortest[x__]~~"\n"/;!StringFreeQ[x,"["]:>x];
res=StringReplace[DeleteDuplicates[Flatten[StringCases[#,"("~~Shortest[x__]~~".md)":>x]&/@StringSplit[tmp,","]]],"\\"->""];
res
]


getTitles[str_]:=
Block[{tmp,res},
tmp=StringCases[str,"- "~~Shortest[x__]~~" - "/;!StringFreeQ[x,"["]:>x];
res=StringReplace[DeleteDuplicates[Flatten[StringCases[#,"["~~Shortest[x__]~~"]":>x]&/@StringSplit[tmp,","]]],"\\"->""];
res
]


raw=Rest[StringSplit[in,"##"]];
out=StringSplit[#,"\n",2]&/@raw;


final=Join[{{StringTrim[out[[1]][[1]]],getTitlesFirst[out[[1]][[2]]]}},
{StringTrim[#[[1]]],getTitles[StringTrim[#[[2]]]]}&/@out[[2;;]]];


finalStr=StringRiffle[Flatten[Map[{"\n\\chapter{"<>#[[1]]<>"}\n",Function[x,{"\\subfile{pages/"<>x<>".tex}"}]/@#[[2]]}&,final]],"\n"];
finalStr=StringReplace[finalStr,{"/$"->"/Dollar"}];


finalStr


Print["Saving the output to", FileNameJoin[{outputDir,"includes.tex"}]];
WriteString[FileNameJoin[{outputDir,"includes.tex"}],finalStr];






