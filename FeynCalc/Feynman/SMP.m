(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SMP																*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary: Some model parameters											*)

(* ------------------------------------------------------------------------ *)

SMP::usage= "SMP[\"par\"] displays a symbol for the model parameter par. \
Typical parameters are masses, coupling constants, mixing angles etc. \
Parameters that are complex, like CKM matrix element, have an I as an additional parameter,
i.e. SMP[\"V_ud\",I] and SMP[\"V_ud\",-I]. \n
SMP[] shows the list of available parameters.";

Gstrong::usage =
"Gstrong is a shortcut for SMP[\"g_s\"].";

AlphaStrong::usage =
"AlphaStrong is a shortcut for SMP[\"alpha_s\"].";

AlphaFS::usage =
"AlphaFS is a shortcut for SMP[\"alpha_fs\"].";

Begin["`Package`"]
End[]

Begin["`SMP`Private`"]

(*	Particle masses	*)

SMP /:
	MakeBoxes[SMP["m_e"], TraditionalForm] :=
		SubscriptBox["m", "e"];

SMP /:
	MakeBoxes[SMP["m_mu"], TraditionalForm] :=
		SubscriptBox["m", "\[Mu]"];
SMP /:
	MakeBoxes[SMP["m_tau"], TraditionalForm] :=
		SubscriptBox["m", "\[Tau]"];

SMP /:
	MakeBoxes[SMP["m_u"], TraditionalForm] :=
		SubscriptBox["m", "u"];

SMP /:
	MakeBoxes[SMP["m_d"], TraditionalForm] :=
		SubscriptBox["m", "d"];

SMP /:
	MakeBoxes[SMP["m_c"], TraditionalForm] :=
		SubscriptBox["m", "c"];

SMP /:
	MakeBoxes[SMP["m_s"], TraditionalForm] :=
		SubscriptBox["m", "s"];

SMP /:
	MakeBoxes[SMP["m_t"], TraditionalForm] :=
		SubscriptBox["m", "t"];

SMP /:
	MakeBoxes[SMP["m_b"], TraditionalForm] :=
		SubscriptBox["m", "b"];

SMP /:
	MakeBoxes[SMP["m_W"], TraditionalForm] :=
		SubscriptBox["m", "W"];

SMP /:
	MakeBoxes[SMP["m_Z"], TraditionalForm] :=
		SubscriptBox["m", "Z"];

SMP /:
	MakeBoxes[SMP["m_H"], TraditionalForm] :=
		SubscriptBox["m", "H"];

SMP /:
	MakeBoxes[SMP["m_q"], TraditionalForm] :=
		SubscriptBox["m", "q"];

SMP /:
	MakeBoxes[SMP["m_Q"], TraditionalForm] :=
		SubscriptBox["m", "Q"];

SMP /:
	MakeBoxes[SMP["m_qu"], TraditionalForm] :=
		SubscriptBox["m", SubscriptBox["q","u"]];

SMP /:
	MakeBoxes[SMP["m_qd"], TraditionalForm] :=
		SubscriptBox["m", SubscriptBox["q","d"]];

SMP /:
	MakeBoxes[SMP["m_pi"], TraditionalForm] :=
		SubscriptBox["m", "\[Pi]"];

SMP /:
	MakeBoxes[SMP["m_l"], TraditionalForm] :=
		SubscriptBox["m", "l"];

(* Coupling constans and mixing angles *)

SMP /:
	MakeBoxes[SMP["g_s"], TraditionalForm] :=
		SubscriptBox["g", "s"];

SMP /:
	MakeBoxes[SMP["e"], TraditionalForm] :=
		ToBoxes["e"];

SMP /:
	MakeBoxes[SMP["Q_u"], TraditionalForm] :=
		SubscriptBox["Q", "u"];

SMP /:
	MakeBoxes[SMP["Q_d"], TraditionalForm] :=
		SubscriptBox["Q", "d"];

SMP /:
	MakeBoxes[SMP["G_F"], TraditionalForm] :=
		SubscriptBox["G", "F"];

SMP /:
	MakeBoxes[SMP["g_W"], TraditionalForm] :=
		SubscriptBox["g", "W"];

SMP /:
	MakeBoxes[SMP["g'_W"], TraditionalForm] :=
		SubscriptBox["g'", "W"];

SMP /:
	MakeBoxes[SMP["cos_W"], TraditionalForm] :=
		RowBox[{"cos(", SubscriptBox["\[Theta]", "W"], ")"}];

SMP /:
	MakeBoxes[SMP["sin_W"], TraditionalForm] :=
		RowBox[{"sin(", SubscriptBox["\[Theta]", "W"], ")"}];

SMP /:
	MakeBoxes[SMP["theta_W"], TraditionalForm] :=
		SubscriptBox["\[Theta]", "W"];

SMP /:
	MakeBoxes[SMP["cos_C"],	TraditionalForm] :=
		RowBox[{"cos(", SubscriptBox["\[Theta]", "C"], ")"}];

SMP /:
	MakeBoxes[SMP["sin_C"], TraditionalForm] :=
		RowBox[{"sin(", SubscriptBox["\[Theta]", "C"], ")"}];

SMP /:
	MakeBoxes[SMP["theta_C"], TraditionalForm] :=
		SubscriptBox["\[Theta]", "C"];

SMP /:
	MakeBoxes[SMP["alpha_fs"], TraditionalForm] :=
		"\[Alpha]";

SMP /:
	MakeBoxes[SMP["alpha_s"], TraditionalForm] :=
		SubscriptBox["\[Alpha]", "s"];

(* CKM matrix *)

SMP /:
	MakeBoxes[SMP["V_ud", c_Complex], TraditionalForm] /;c === Complex[0, 1] :=
		SubscriptBox["V", "ud"];

SMP /:
	MakeBoxes[SMP["V_ud", c_Complex], TraditionalForm] /; c === Complex[0, -1] :=
		SubsuperscriptBox["V", "ud", "*"];

SMP /:
	MakeBoxes[SMP["V_us", c_Complex], TraditionalForm] /; c === Complex[0, 1] :=
		SubscriptBox["V", "us"];

SMP /:
	MakeBoxes[SMP["V_us", c_Complex], TraditionalForm] /; c === Complex[0, -1] :=
		SubsuperscriptBox["V", "us", "*"];

SMP /:
	MakeBoxes[SMP["V_ub", c_Complex], TraditionalForm] /; c === Complex[0, 1] :=
		SubscriptBox["V", "ub"];

SMP /:
	MakeBoxes[SMP["V_ub", c_Complex], TraditionalForm] /; c === Complex[0, -1] :=
		SubsuperscriptBox["V", "ub", "*"];

SMP /:
	MakeBoxes[SMP["V_cd", c_Complex], TraditionalForm] /; c === Complex[0, 1] :=
		SubscriptBox["V", "cd"];

SMP /:
	MakeBoxes[SMP["V_cd", c_Complex], TraditionalForm] /; c === Complex[0, -1] :=
		SubsuperscriptBox["V", "cd", "*"];

SMP /:
	MakeBoxes[SMP["V_cs", c_Complex], TraditionalForm] /; c === Complex[0, 1] :=
		SubscriptBox["V", "cs"];

SMP /:
	MakeBoxes[SMP["V_cs", c_Complex], TraditionalForm] /; c === Complex[0, -1] :=
		SubsuperscriptBox["V", "cs", "*"];

SMP /:
	MakeBoxes[SMP["V_cb", c_Complex], TraditionalForm] /; c === Complex[0, 1] :=
		SubscriptBox["V", "cb"];

SMP /:
	MakeBoxes[SMP["V_cb", c_Complex], TraditionalForm] /; c === Complex[0, -1] :=
		SubsuperscriptBox["V", "cb", "*"];

SMP /:
	MakeBoxes[SMP["V_td", c_Complex], TraditionalForm] /; c === Complex[0, 1] :=
		SubscriptBox["V", "td"];

SMP /:
	MakeBoxes[SMP["V_td", c_Complex], TraditionalForm] /; c === Complex[0, -1] :=
		SubsuperscriptBox["V", "td", "*"];

SMP /:
	MakeBoxes[SMP["V_ts", c_Complex], TraditionalForm] /; c === Complex[0, 1] :=
		SubscriptBox["V", "ts"];

SMP /:
	MakeBoxes[SMP["V_ts", c_Complex], TraditionalForm] /; c === Complex[0, -1] :=
		SubsuperscriptBox["V", "ts", "*"];

SMP /:
	MakeBoxes[SMP["V_tb", c_Complex], TraditionalForm] /; c === Complex[0, 1] :=
		SubscriptBox["V", "tb"];

SMP /:
	MakeBoxes[SMP["V_tb", c_Complex], TraditionalForm] /; c === Complex[0, -1] :=
		SubsuperscriptBox["V", "tb", "*"];

(* Generic renormalization constants *)
SMP /: MakeBoxes[SMP["dPsi"], TraditionalForm] :=
	ToBoxes["\[Delta]\[CapitalPsi]"];

SMP /: MakeBoxes[SMP["dA"], TraditionalForm] :=
	ToBoxes["\[Delta]A"];

SMP /: MakeBoxes[SMP["dm"], TraditionalForm] :=
	ToBoxes["\[Delta]m"];

SMP /: MakeBoxes[SMP["de"], TraditionalForm] :=
	ToBoxes["\[Delta]e"];

SMP /: MakeBoxes[SMP["du"], TraditionalForm] :=
	ToBoxes["\[Delta]m"];

SMP /: MakeBoxes[SMP["dxi"], TraditionalForm] :=
	ToBoxes["\[Delta]\[Xi]"];

SMP /: MakeBoxes[SMP["Z_Psi"], TraditionalForm] :=
	SubscriptBox["Z", "\[CapitalPsi]"];

SMP /: MakeBoxes[SMP["Z_A"], TraditionalForm] :=
	SubscriptBox["Z", "A"];

SMP /: MakeBoxes[SMP["Z_m"], TraditionalForm] :=
	SubscriptBox["Z", "m"];

SMP /: MakeBoxes[SMP["Z_u"], TraditionalForm] :=
	SubscriptBox["Z", "u"];

SMP /: MakeBoxes[SMP["Z_xi"], TraditionalForm] :=
	SubscriptBox["Z", "\[Xi]"];

SMP /: MakeBoxes[SMP["Z_e"], TraditionalForm] :=
	SubscriptBox["Z", "e"];

SMP /: MakeBoxes[SMP["dZ_Psi"], TraditionalForm] :=
	SubscriptBox["\[Delta]Z", "\[CapitalPsi]"];

SMP /: MakeBoxes[SMP["dZ_A"], TraditionalForm] :=
	SubscriptBox["\[Delta]Z", "A"];

SMP /: MakeBoxes[SMP["dZ_m"], TraditionalForm] :=
	SubscriptBox["\[Delta]Z", "m"];

SMP /: MakeBoxes[SMP["dZ_e"], TraditionalForm] :=
	SubscriptBox["\[Delta]Z", "e"];

SMP /: MakeBoxes[SMP["dZ_u"], TraditionalForm] :=
	SubscriptBox["\[Delta]Z", "u"];

SMP /: MakeBoxes[SMP["dZ_xi"], TraditionalForm] :=
	SubscriptBox["\[Delta]Z", "xi"];

(* MS renormalization constants *)

SMP /: MakeBoxes[SMP["dPsi^MS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["dPsi"], TraditionalForm], "MS"];

SMP /: MakeBoxes[SMP["dA^MS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["dA"], TraditionalForm], "MS"];

SMP /: MakeBoxes[SMP["dm^MS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["dm"], TraditionalForm], "MS"];

SMP /: MakeBoxes[SMP["de^MS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["de"], TraditionalForm], "MS"];

SMP /: MakeBoxes[SMP["du^MS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["du"], TraditionalForm], "MS"];

SMP /: MakeBoxes[SMP["dxi^MS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["dxi"], TraditionalForm], "MS"];

SMP /: MakeBoxes[SMP["Z_Psi^MS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["Z_Psi"], TraditionalForm], "MS"];

SMP /: MakeBoxes[SMP["Z_A^MS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["Z_A"], TraditionalForm], "MS"];

SMP /: MakeBoxes[SMP["Z_m^MS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["Z_m"], TraditionalForm], "MS"];

SMP /: MakeBoxes[SMP["Z_u^MS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["Z_u"], TraditionalForm], "MS"];

SMP /: MakeBoxes[SMP["Z_xi^MS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["Z_xi"], TraditionalForm], "MS"];

SMP /: MakeBoxes[SMP["Z_e^MS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["Z_e"], TraditionalForm], "MS"];

SMP /: MakeBoxes[SMP["dZ_Psi^MS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["dZ_Psi"], TraditionalForm], "MS"];

SMP /: MakeBoxes[SMP["dZ_A^MS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["dZ_A"], TraditionalForm], "MS"];

SMP /: MakeBoxes[SMP["dZ_m^MS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["dZ_m"], TraditionalForm], "MS"];

SMP /: MakeBoxes[SMP["dZ_e^MS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["dZ_e"], TraditionalForm], "MS"];

SMP /: MakeBoxes[SMP["dZ_u^MS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["dZ_u"], TraditionalForm], "MS"];

SMP /: MakeBoxes[SMP["dZ_xi^MS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["dZ_xi"], TraditionalForm], "MS"];

(* MSbar renormalization constants *)

SMP /: MakeBoxes[SMP["dPsi^MSbar"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["dPsi"], TraditionalForm], OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["dA^MSbar"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["dA"], TraditionalForm], OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["dm^MSbar"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["dm"], TraditionalForm], OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["de^MSbar"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["de"], TraditionalForm], OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["du^MSbar"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["du"], TraditionalForm], OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["dxi^MSbar"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["dxi"], TraditionalForm], OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["Z_Psi^MSbar"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["Z_Psi"], TraditionalForm], OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["Z_A^MSbar"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["Z_A"], TraditionalForm], OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["Z_m^MSbar"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["Z_m"], TraditionalForm], OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["Z_u^MSbar"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["Z_u"], TraditionalForm], OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["Z_xi^MSbar"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["Z_xi"], TraditionalForm], OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["Z_e^MSbar"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["Z_e"], TraditionalForm], OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["dZ_Psi^MSbar"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["dZ_Psi"], TraditionalForm], OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["dZ_A^MSbar"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["dZ_A"], TraditionalForm], OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["dZ_m^MSbar"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["dZ_m"], TraditionalForm], OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["dZ_e^MSbar"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["dZ_e"], TraditionalForm], OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["dZ_u^MSbar"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["dZ_u"], TraditionalForm], OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["dZ_xi^MSbar"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["dZ_xi"], TraditionalForm], OverscriptBox["MS", "\[LongDash]"]];

(* OS renormalization constants *)

SMP /: MakeBoxes[SMP["dPsi^OS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["dPsi"], TraditionalForm], "OS"];

SMP /: MakeBoxes[SMP["dA^OS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["dA"], TraditionalForm], "OS"];

SMP /: MakeBoxes[SMP["dm^OS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["dm"], TraditionalForm], "OS"];

SMP /: MakeBoxes[SMP["de^OS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["de"], TraditionalForm], "OS"];

SMP /: MakeBoxes[SMP["du^OS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["du"], TraditionalForm], "OS"];

SMP /: MakeBoxes[SMP["dxi^OS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["dxi"], TraditionalForm], "OS"];

SMP /: MakeBoxes[SMP["Z_Psi^OS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["Z_Psi"], TraditionalForm], "OS"];

SMP /: MakeBoxes[SMP["Z_A^OS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["Z_A"], TraditionalForm], "OS"];

SMP /: MakeBoxes[SMP["Z_m^OS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["Z_m"], TraditionalForm], "OS"];

SMP /: MakeBoxes[SMP["Z_u^OS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["Z_u"], TraditionalForm], "OS"];

SMP /: MakeBoxes[SMP["Z_xi^OS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["Z_xi"], TraditionalForm], "OS"];

SMP /: MakeBoxes[SMP["Z_e^OS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["Z_e"], TraditionalForm], "OS"];

SMP /: MakeBoxes[SMP["dZ_Psi^OS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["dZ_Psi"], TraditionalForm], "OS"];

SMP /: MakeBoxes[SMP["dZ_A^OS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["dZ_A"], TraditionalForm], "OS"];

SMP /: MakeBoxes[SMP["dZ_m^OS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["dZ_m"], TraditionalForm], "OS"];

SMP /: MakeBoxes[SMP["dZ_e^OS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["dZ_e"], TraditionalForm], "OS"];

SMP /: MakeBoxes[SMP["dZ_u^OS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["dZ_u"], TraditionalForm], "OS"];

SMP /: MakeBoxes[SMP["dZ_xi^OS"], TraditionalForm] :=
	SuperscriptBox[ToBoxes[SMP["dZ_xi"], TraditionalForm], "OS"];

(* Epsilon related stuff *)
SMP /:
	MakeBoxes[SMP["Delta"], TraditionalForm] :=
		ToBoxes["\[CapitalDelta]"];

SMP /:
	MakeBoxes[SMP["Delta_UV"], TraditionalForm] :=
		SubscriptBox["\[CapitalDelta]", "UV"];

SMP /:
	MakeBoxes[SMP["Delta_IR"], TraditionalForm] :=
		SubscriptBox["\[CapitalDelta]", "IR"];


SMP[]=
	Block[{li},
		li={
			"m_e","m_mu","m_tau",
			"m_u","m_d","m_c",
			"m_s","m_t","m_b",
			"m_H","m_W","m_Z",
			"m_q","m_Q","m_qu","m_qd",
			"m_l","m_pi",
			"g_s","e","Q_u","Q_d",
			"G_F","g_W","g'_W",
			"cos_W","sin_W","theta_W",
			"cos_C","sin_C","theta_C",
			"alpha_fs","alpha_s",
			"dPsi", "dA", "dm", "de", "dxi",
			"Z_Psi", "Z_A", "Z_m", "Z_u", "Z_xi", "Z_e",
			"dZ_Psi", "dZ_A", "dZ_m", "dZ_e", "dZ_u", "dZ_xi",
			"dPsi^MS", "dA^MS", "dm^MS", "de^MS", "dxi^MS",
			"Z_Psi^MS", "Z_A^MS", "Z_m^MS", "Z_u^MS", "Z_xi^MS", "Z_e^MS",
			"dZ_Psi^MS", "dZ_A^MS", "dZ_m^MS", "dZ_e^MS", "dZ_u^MS", "dZ_xi^MS",
			"dPsi^MSbar", "dA^MSbar", "dm^MSbar", "de^MSbar", "dxi^MSbar",
			"Z_Psi^MSbar", "Z_A^MSbar", "Z_m^MSbar", "Z_u^MSbar", "Z_xi^MSbar", "Z_e^MSbar",
			"dZ_Psi^MSbar", "dZ_A^MSbar", "dZ_m^MSbar", "dZ_e^MSbar", "dZ_u^MSbar", "dZ_xi^MSbar",
			"dPsi^OS", "dA^OS", "dm^OS", "de^OS", "dxi^OS",
			"Z_Psi^OS", "Z_A^OS", "Z_m^OS", "Z_u^OS", "Z_xi^OS", "Z_e^OS",
			"dZ_Psi^OS", "dZ_A^OS", "dZ_m^OS", "dZ_e^OS", "dZ_u^OS", "dZ_xi^OS",
			"dPsi","dA","dm",
			"Z_Psi","Z_A","Z_m","Z_xi", "Z_e",
			"dZ_Psi","dZ_A","dZ_m","dZ_e",
			{"V_ud",I},{"V_ud",-I},
			{"V_us",I},{"V_us",-I},
			{"V_ub",I},{"V_ub",-I},
			{"V_cd",I},{"V_cd",-I},
			{"V_cs",I},{"V_cs",-I},
			{"V_cb",I},{"V_cb",-I},
			{"V_td",I},{"V_td",-I},
			{"V_ts",I},{"V_ts",-I},
			{"V_tb",I},{"V_tb",-I}
		};
		{SMP /@ li, li} // Transpose
	];


SMP[{x__}]:=
	SMP[x];

Gstrong:=
	SMP["g_s"];

AlphaStrong:=
	SMP["alpha_s"];

AlphaFS:=
	SMP["alpha_fs"];

SetAttributes[SMP, Protected]

FCPrint[1,"SMP.m loaded."];
End[]
