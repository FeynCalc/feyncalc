(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SMP																*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: Some model parameters											*)

(* ------------------------------------------------------------------------ *)

SMP::usage=
"SMP[par] displays a symbol for the model parameter par. Typical parameters are
masses, coupling constants, mixing angles etc.

Parameters that are complex, like a CKM matrix element, have an I as an
additional argument, e.g. SMP[\"V_ud\", I] and  SMP[\"V_ud\", -I].

SMP[] shows the list of all available parameters.";

Begin["`Package`"]
End[]

Begin["`SMP`Private`"]

DataType[SMP, FCVariable] = True;

(*	Number of flavors	*)

SMP /:
	MakeBoxes[SMP["N_F"], TraditionalForm] :=
		SubscriptBox["N", "F"];

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
	MakeBoxes[SMP["g"], TraditionalForm] :=
		ToBoxes["g"];

SMP /:
	MakeBoxes[SMP["e"], TraditionalForm] :=
		ToBoxes["e"];

SMP /:
	MakeBoxes[SMP["e_Q"], TraditionalForm] :=
		SubscriptBox["e", "Q"];

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

SMP /:
	MakeBoxes[SMP["s_12"], TraditionalForm] :=
		SubscriptBox["s", "12"];

SMP /:
	MakeBoxes[SMP["s_13"], TraditionalForm] :=
		SubscriptBox["s", "13"];

SMP /:
	MakeBoxes[SMP["s_23"], TraditionalForm] :=
		SubscriptBox["s", "23"];

SMP /:
	MakeBoxes[SMP["c_12"], TraditionalForm] :=
		SubscriptBox["c", "12"];

SMP /:
	MakeBoxes[SMP["c_13"], TraditionalForm] :=
		SubscriptBox["c", "13"];

SMP /:
	MakeBoxes[SMP["c_23"], TraditionalForm] :=
		SubscriptBox["c", "23"];

(* Generic renormalization constants *)
SMP /: MakeBoxes[SMP["d_psi"], TraditionalForm] :=
	SubscriptBox["\[Delta]","\[Psi]"];

SMP /: MakeBoxes[SMP["d_A"], TraditionalForm] :=
	SubscriptBox["\[Delta]","A"];

SMP /: MakeBoxes[SMP["d_m"], TraditionalForm] :=
	SubscriptBox["\[Delta]","m"];

SMP /: MakeBoxes[SMP["d_e"], TraditionalForm] :=
	SubscriptBox["\[Delta]","e"];

SMP /: MakeBoxes[SMP["d_g"], TraditionalForm] :=
	SubscriptBox["\[Delta]","g"];

SMP /: MakeBoxes[SMP["d_u"], TraditionalForm] :=
	SubscriptBox["\[Delta]","u"];

SMP /: MakeBoxes[SMP["d_xi"], TraditionalForm] :=
	SubscriptBox["\[Delta]","\[Xi]"];

SMP /: MakeBoxes[SMP["d_phi"], TraditionalForm] :=
	SubscriptBox["\[Delta]","\[Phi]"];

SMP /: MakeBoxes[SMP["Z_psi"], TraditionalForm] :=
	SubscriptBox["Z", "\[Psi]"];

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

SMP /: MakeBoxes[SMP["Z_g"], TraditionalForm] :=
	SubscriptBox["Z", "g"];

SMP /: MakeBoxes[SMP["Z_phi"], TraditionalForm] :=
	SubscriptBox["Z", "\[Phi]"];

SMP /: MakeBoxes[SMP["dZ_psi"], TraditionalForm] :=
	SubscriptBox["\[Delta]Z", "\[Psi]"];

SMP /: MakeBoxes[SMP["dZ_A"], TraditionalForm] :=
	SubscriptBox["\[Delta]Z", "A"];

SMP /: MakeBoxes[SMP["dZ_m"], TraditionalForm] :=
	SubscriptBox["\[Delta]Z", "m"];

SMP /: MakeBoxes[SMP["dZ_e"], TraditionalForm] :=
	SubscriptBox["\[Delta]Z", "e"];

SMP /: MakeBoxes[SMP["dZ_g"], TraditionalForm] :=
	SubscriptBox["\[Delta]Z", "g"];

SMP /: MakeBoxes[SMP["dZ_u"], TraditionalForm] :=
	SubscriptBox["\[Delta]Z", "u"];

SMP /: MakeBoxes[SMP["dZ_xi"], TraditionalForm] :=
	SubscriptBox["\[Delta]Z", "\[Xi]"];

SMP /: MakeBoxes[SMP["dZ_phi"], TraditionalForm] :=
	SubscriptBox["\[Delta]Z", "\[Phi]"];

(* MS renormalization constants *)

SMP /: MakeBoxes[SMP["d_psi^MS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]","\[Psi]", "MS"];

SMP /: MakeBoxes[SMP["d_A^MS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]","A", "MS"];

SMP /: MakeBoxes[SMP["d_m^MS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]","m", "MS"];

SMP /: MakeBoxes[SMP["d_e^MS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]","e", "MS"];

SMP /: MakeBoxes[SMP["d_g^MS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]","g", "MS"];

SMP /: MakeBoxes[SMP["d_u^MS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]","u", "MS"];

SMP /: MakeBoxes[SMP["d_xi^MS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]","\[Xi]", "MS"];

SMP /: MakeBoxes[SMP["d_phi^MS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]","\[Phi]", "MS"];

SMP /: MakeBoxes[SMP["Z_psi^MS"], TraditionalForm] :=
	SubsuperscriptBox["Z", "\[Psi]", "MS"];

SMP /: MakeBoxes[SMP["Z_A^MS"], TraditionalForm] :=
	SubsuperscriptBox["Z", "A", "MS"];

SMP /: MakeBoxes[SMP["Z_m^MS"], TraditionalForm] :=
	SubsuperscriptBox["Z", "m", "MS"];

SMP /: MakeBoxes[SMP["Z_u^MS"], TraditionalForm] :=
	SubsuperscriptBox["Z", "u", "MS"];

SMP /: MakeBoxes[SMP["Z_xi^MS"], TraditionalForm] :=
	SubsuperscriptBox["Z", "\[Xi]", "MS"];

SMP /: MakeBoxes[SMP["Z_e^MS"], TraditionalForm] :=
	SubsuperscriptBox["Z", "e", "MS"];

SMP /: MakeBoxes[SMP["Z_g^MS"], TraditionalForm] :=
	SubsuperscriptBox["Z", "g", "MS"];

	SMP /: MakeBoxes[SMP["Z_phi^MS"], TraditionalForm] :=
	SubsuperscriptBox["Z", "\[Phi]", "MS"];

SMP /: MakeBoxes[SMP["dZ_psi^MS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]Z", "\[Psi]", "MS"];

SMP /: MakeBoxes[SMP["dZ_A^MS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]Z", "A","MS"];

SMP /: MakeBoxes[SMP["dZ_m^MS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]Z", "m","MS"];

SMP /: MakeBoxes[SMP["dZ_e^MS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]Z", "e","MS"];

SMP /: MakeBoxes[SMP["dZ_g^MS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]Z", "g","MS"];

SMP /: MakeBoxes[SMP["dZ_u^MS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]Z", "u","MS"];

SMP /: MakeBoxes[SMP["dZ_xi^MS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]Z", "\[Xi]","MS"];

SMP /: MakeBoxes[SMP["dZ_phi^MS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]Z", "\[Phi]","MS"];

(* MSbar renormalization constants *)

SMP /: MakeBoxes[SMP["d_psi^MSbar"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]","\[Psi]", OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["d_psi^MSbar"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]","\[Psi]", OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["d_A^MSbar"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]","A", OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["d_m^MSbar"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]","m", OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["d_e^MSbar"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]","e", OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["d_g^MSbar"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]","g", OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["d_u^MSbar"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]","u", OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["d_xi^MSbar"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]","\[Xi]", OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["d_phi^MSbar"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]","\[Phi]", OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["Z_psi^MSbar"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]Z", "\[Psi]", OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["Z_A^MSbar"], TraditionalForm] :=
	SubsuperscriptBox["Z", "A", OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["Z_m^MSbar"], TraditionalForm] :=
	SubsuperscriptBox["Z", "m", OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["Z_u^MSbar"], TraditionalForm] :=
	SubsuperscriptBox["Z", "u", OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["Z_xi^MSbar"], TraditionalForm] :=
	SubsuperscriptBox["Z", "\[Xi]", OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["Z_e^MSbar"], TraditionalForm] :=
	SubsuperscriptBox["Z", "e", OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["Z_g^MSbar"], TraditionalForm] :=
	SubsuperscriptBox["Z", "g", OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["Z_phi^MSbar"], TraditionalForm] :=
	SubsuperscriptBox["Z", "\[Phi]", OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["dZ_psi^MSbar"], TraditionalForm] :=
	SubsuperscriptBox["Z", "\[Psi]", OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["dZ_A^MSbar"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]Z", "A",OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["dZ_m^MSbar"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]Z", "m",OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["dZ_e^MSbar"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]Z", "e",OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["dZ_g^MSbar"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]Z", "g",OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["dZ_u^MSbar"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]Z", "u",OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["dZ_xi^MSbar"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]Z", "\[Xi]",OverscriptBox["MS", "\[LongDash]"]];

SMP /: MakeBoxes[SMP["dZ_phi^MSbar"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]Z", "\[Phi]",OverscriptBox["MS", "\[LongDash]"]];

(* OS renormalization constants *)

SMP /: MakeBoxes[SMP["d_psi^OS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]","\[Psi]", "OS"];

SMP /: MakeBoxes[SMP["d_A^OS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]","A", "OS"];

SMP /: MakeBoxes[SMP["d_m^OS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]","m", "OS"];

SMP /: MakeBoxes[SMP["d_e^OS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]","e", "OS"];

SMP /: MakeBoxes[SMP["d_g^OS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]","g", "OS"];

SMP /: MakeBoxes[SMP["d_u^OS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]","u", "OS"];

SMP /: MakeBoxes[SMP["d_xi^OS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]","\[Xi]", "OS"];

SMP /: MakeBoxes[SMP["d_phi^OS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]","\[Phi]", "OS"];

SMP /: MakeBoxes[SMP["Z_psi^OS"], TraditionalForm] :=
	SubsuperscriptBox["Z", "\[Psi]", "OS"];

SMP /: MakeBoxes[SMP["Z_A^OS"], TraditionalForm] :=
	SubsuperscriptBox["Z", "A", "OS"];

SMP /: MakeBoxes[SMP["Z_m^OS"], TraditionalForm] :=
	SubsuperscriptBox["Z", "m", "OS"];

SMP /: MakeBoxes[SMP["Z_u^OS"], TraditionalForm] :=
	SubsuperscriptBox["Z", "u", "OS"];

SMP /: MakeBoxes[SMP["Z_xi^OS"], TraditionalForm] :=
	SubsuperscriptBox["Z", "\[Xi]", "OS"];

SMP /: MakeBoxes[SMP["Z_e^OS"], TraditionalForm] :=
	SubsuperscriptBox["Z", "e", "OS"];

SMP /: MakeBoxes[SMP["Z_g^OS"], TraditionalForm] :=
	SubsuperscriptBox["Z", "g", "OS"];

SMP /: MakeBoxes[SMP["Z_phi^OS"], TraditionalForm] :=
	SubsuperscriptBox["Z", "\[Phi]", "OS"];

SMP /: MakeBoxes[SMP["dZ_psi^OS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]Z", "\[Psi]", "OS"];

SMP /: MakeBoxes[SMP["dZ_A^OS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]Z", "A","OS"];

SMP /: MakeBoxes[SMP["dZ_m^OS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]Z", "m","OS"];

SMP /: MakeBoxes[SMP["dZ_e^OS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]Z", "e","OS"];

SMP /: MakeBoxes[SMP["dZ_g^OS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]Z", "g","OS"];

SMP /: MakeBoxes[SMP["dZ_u^OS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]Z", "u","OS"];

SMP /: MakeBoxes[SMP["dZ_xi^OS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]Z", "\[Xi]","OS"];

SMP /: MakeBoxes[SMP["dZ_phi^OS"], TraditionalForm] :=
	SubsuperscriptBox["\[Delta]Z", "\[Phi]","OS"];

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
			"N_F",
			"m_e","m_mu","m_tau",
			"m_u","m_d","m_c",
			"m_s","m_t","m_b",
			"m_H","m_W","m_Z",
			"m_q","m_Q","m_qu","m_qd",
			"m_l","m_pi",
			"g", "g_s","e","e_Q","Q_u","Q_d",
			"G_F","g_W","g'_W",
			"cos_W","sin_W","theta_W",
			"cos_C","sin_C","theta_C",
			"alpha_fs","alpha_s",
			"d_psi", "d_phi", "d_A", "d_m", "d_u", "d_xi", "d_e", "d_g",
			"Z_psi", "Z_phi", "Z_A", "Z_m", "Z_u", "Z_xi", "Z_e", "Z_g",
			"dZ_psi", "dZ_phi", "dZ_A", "dZ_m", "dZ_u", "dZ_xi", "dZ_e", "dZ_g",

			"d_psi^MS", "d_phi^MS", "d_A^MS", "d_m^MS", "d_u^MS", "d_xi^MS", "d_e^MS", "d_g^MS",
			"Z_psi^MS", "Z_phi^MS", "Z_A^MS", "Z_m^MS", "Z_u^MS", "Z_xi^MS", "Z_e^MS", "Z_g^MS",
			"dZ_psi^MS", "dZ_phi^MS", "dZ_A^MS", "dZ_m^MS", "dZ_u^MS", "dZ_xi^MS", "dZ_e^MS", "dZ_g^MS",

			"d_psi^MSbar", "d_phi^MSbar", "d_A^MSbar", "d_m^MSbar", "d_u^MSbar", "d_xi^MSbar", "d_e^MSbar", "d_g^MSbar",
			"Z_psi^MSbar", "Z_phi^MSbar", "Z_A^MSbar", "Z_m^MSbar", "Z_u^MSbar", "Z_xi^MSbar", "Z_e^MSbar", "Z_g^MSbar",
			"dZ_psi^MSbar", "dZ_phi^MSbar", "dZ_A^MSbar", "dZ_m^MSbar", "dZ_u^MSbar", "dZ_xi^MSbar", "dZ_e^MSbar", "dZ_g^MSbar",

			"d_psi^OS", "d_phi^OS", "d_A^OS", "d_m^OS", "d_u^OS", "d_xi^OS", "d_e^OS", "d_g^OS",
			"Z_psi^OS", "Z_phi^OS", "Z_A^OS", "Z_m^OS", "Z_u^OS", "Z_xi^OS", "Z_e^OS", "Z_g^OS",
			"dZ_psi^OS", "dZ_phi^OS", "dZ_A^OS", "dZ_m^OS", "dZ_u^OS", "dZ_xi^OS", "dZ_e^OS", "dZ_g^OS",

			{"V_ud",I},{"V_ud",-I},
			{"V_us",I},{"V_us",-I},
			{"V_ub",I},{"V_ub",-I},
			{"V_cd",I},{"V_cd",-I},
			{"V_cs",I},{"V_cs",-I},
			{"V_cb",I},{"V_cb",-I},
			{"V_td",I},{"V_td",-I},
			{"V_ts",I},{"V_ts",-I},
			{"V_tb",I},{"V_tb",-I},
			"s_12", "s_13", "s_23",
			"c_12", "c_13", "c_23"
		};
		{SMP /@ li, li} // Transpose
	];

SMP /:
	Conjugate[SMP[a_, Complex[0,1]]] := SMP[{a, -I}];

SMP /:
	Conjugate[SMP[a_, Complex[0,-1]]] := SMP[{a, I}];

SMP[{x__}]:=
	SMP[x];

SetAttributes[SMP, Protected];

FCPrint[1,"SMP.m loaded."];
End[]
