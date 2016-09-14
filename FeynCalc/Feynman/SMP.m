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
		"e";

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

SMP[]={
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
{"V_ud",I},{"V_ud",-I},
{"V_us",I},{"V_us",-I},
{"V_ub",I},{"V_ub",-I},
{"V_cd",I},{"V_cd",-I},
{"V_cs",I},{"V_cs",-I},
{"V_cb",I},{"V_cb",-I},
{"V_td",I},{"V_td",-I},
{"V_ts",I},{"V_ts",-I},
{"V_tb",I},{"V_tb",-I}
}

SMP[{x__}]:=
	SMP[x];

Gstrong:=
	SMP["g_s"];

AlphaStrong:=
	SMP["alpha_s"];

AlphaFS:=
	SMP["alpha_fs"];

FCPrint[1,"SMP.m loaded."];
End[]
