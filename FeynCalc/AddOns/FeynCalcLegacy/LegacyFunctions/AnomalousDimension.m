(* :Title: AnomalousDimension*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 23 December '98 at 0:28 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

AnomalousDimension::usage=
"AnomalousDimension[name] is a database of anomalous dimensions of twist 2
operators.

 AnomalousDimension[\"gnsqg0\"] yields the non-singlet one-loop contribution
to the anomalous dimension $\\gamma_{S,qg}^{(0),m}$ in the MS-bar scheme etc.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`AnomalousDimension`Private`"]

Options[AnomalousDimension] = {Polarization -> 1, Simplify->FullSimplify};

AnomalousDimension[es_, opt___Rule] := AnomalousDimension[es, OPEm, opt];

AnomalousDimension[es_, m_Symbol, opt___Rule] :=
Block[{pol, s, s1, s2, s1t, s2t, s12,s21,s12t,d3,s3t,s3,
			S1,S2,S12,S12t,S21,S3,S3t,S1t,S2t,ST,sim,simfun,n=m},
simfun = Simplify /. {opt} /. Options[AnomalousDimension];
If[simfun === FullSimplify,

	sim[n^4+2n^3+2n^2+5n+2] = (2 + n)*(1 + 2*n + n^3);
	sim[n^5+n^4-4n^3+3n^2-7n-2] = -2 + n*(-7 + n*(3 + n*(-4 + n + n^2)));
	sim[5n^5+5n^4-2n^3-n^2-5n-2] =  (-1 + n)*(2 + n*(7 + n*(8 + 5*n*(2 + n))));
	sim[11n^2+22n+12] =  12 + 11*n*(2 + n);
	sim[76n^5+271n^4+254n^3+41n^2+72n+36]=
				36 + n*(72 + n*(41 + n*(254 + n*(271 + 76*n))));
	sim[9n^5+30n^4+16n^3-31n^2-32n-4]=
		(2 + n)*(-2 + n*(-15 + n*(-8 + 3*n*(4 + 3*n))));
	sim[67n^4+134n^3+67n^2+144n+72 ] = 72 + n*(144 + 67*n*(1 + n)^2);
	sim[48n^6+144n^5+469n^4+698n^3+7n^2+258n+144] = 144 +
			n*(258 + n*(7 + n*(698 + n*(469 + 48*n*(3 + n)))));
	sim[3n^4+6n^3+16n^2+13n-3] =  -3 + n*(1 + n)*(13 + 3*n*(1 + n));
	sim[n^6+3n^5+5n^4+n^3-8n^2+2n+4] =
			4 + n*(2 + n*(-8 + n*(1 + n*(5 + n*(3 + n)))));
	sim[z_] := FullSimplify[z]
	];
s = ToString[es];
s1  =  SumS[1, m-1]; S1  =  SumS[1, m];
s2  =  SumS[2, m-1]; S2  =  SumS[2, m];
s12 =  SumS[1,2, m-1]; S12 =  SumS[1,2, m];
s12t=  SumT[1,2, m-1]; S12t=  SumT[1,2, m];
s21 =  SumS[2,1, m-1]; S21 =  SumS[2,1, m];
s3  =  SumS[3, m-1]; S3  =  SumS[3, m];
s3t =  SumT[3, m-1]; S3t =  SumT[3, m];
s1t =  SumT[1, m-1]; S1t =  SumT[1, m];
s2t =  SumT[2, m-1]; S2t =  SumT[2, m];
s3t =  SumT[3, m-1]; S3t =  SumT[3, m];
SP2 =  SumP[2, m/2];
SP3 =  SumP[3, m/2];
ST  = SumT[m];
pol =  Polarization /. {opt} /. Options[AnomalousDimension];
If[pol === 1,
Which[s=== "gnsqq0", CF (8 s1+4/m+4/(m+1)-6)
										,
				s=== "gsqg0", Tf (8/m - 16/(m+1))
										,
				s=== "gsgq0", CF (4/(m+1) - 8/m)
										,
				s=== "gsgg0", CA (8 s1 - 8/m + 16/(m+1)-22/3) + 8/3 Tf
										,
				s=== "gpsqq1", CF Tf 16 (2/(m+1)^3+3/(m+1)^2+1/(m+1)+2/m^3-
																	1/m^2-1/m)
										,
				s=== "GPSQQ1", CF Tf 16 sim[n^4+2n^3+2n^2+5n+2]/n^3/(n+1)^3
										,
				s=== "gnsqq1", -(CF*Nf*(-2/3-8/(3*m^2)-8/(9*m)-8/(3*(1+m)^2)+
												88/(9*(1+m))+(80*s1)/9- (16*s2)/3)+
												CA*CF*(17/3+44/(3*m^2)+212/(9*m)-16/(1+m)^3-
												4/(3*(1+m)^2)-748/(9*(1+m))-(536*s1)/9+
												(88*s2)/3-16*s3-
												(16*s2t)/m-(16*s2t)/(1+m)+
												16*s3t-32*s12t)+
												CF^2*(3+8/m^3-40/m+40/(1+m)^3+16/(1+m)^2+
												40/(1+m)+(16*s1)/m^2+ (16*s1)/(1+m)^2-24*s2+
												(16*s2)/m+(16*s2)/(1+m)+ 32*s12+32*s21+
												(32*s2t)/m+(32*s2t)/(1+m)- 32*s3t+64*s12t)
												)
										,
				s=== "gsqg1", CA Tf 16 (-s1^2/m+2s1^2/(m+1)-2s1/m^2 +
											4 s1/(m+1)^2 - s2/m + 2 s2/(m+1) - 2 s2t/m +
											4 s2t/(m+1)-4/m+3/(m+1)-3/m^2+
											8/(m+1)^2+2/m^3+12/(m+1)^3
																) +
											8CF Tf (2 s1^2/m-4s1^2/(m+1)-2s2/m+4s2/(m+1)+
											14/m-19/(m+1)-1/m^2-8/(m+1)^2-2/m^3+4/(m+1)^3
															)
										,
				s=== "GSQG1", CA Tf 16 ( (n-1)/n/(n+1) (-S2+SP2+S1^2)-
											4/n/(n+1)^2 S1 - sim[n^5+n^4-4n^3+3n^2-7n-2]/
											n^3/(n+1)^3
																) +
											CF Tf 8 ( 2(n-1)/n/(n+1)(S2-S1^2)+
																4(n-1)/n^2/(n+1) S1 -
																sim[5n^5+5n^4-2n^3-n^2-5n-2
																		]/n^3/(n+1)^3
															)
										,
				s=== "gsgq1", CA CF 8 (-2s1^2/m+s1^2/(m+1)+16s1/3/m-5s1/3/(m+1)+
											2 s2/m-s2/(m+1)+4s2t/m-2s2t/(m+1)-56/9/m-
											20/9/(m+1)+28/3/m^2-38/3/(m+1)^2-4/m^3-6/(m+1)^3
															) +
											CF^2 4 (4s1^2/m-2s1^2/(m+1)-8s1/m+2s1/(m+1) +
											8s1/m^2-4s1/(m+1)^2+4s2/m-2s2/(m+1)+
											15/m-6/(m+1)-12/m^2+3/(m+1)^2+4/m^3-2/(m+1)^3
															) +
											CF Tf 32 (-2s1/3/m+s1/3/(m+1) + 7/9/m -
											2/9/(m+1)-2/3/m^2+1/3/(m+1)^2
																)
										,
				s=== "GSGQ1", CA CF 8 ((n+2)/n/(n+1)(-S2+SP2-S1^2)+
											sim[11n^2+22n+12]/3/n^2/(n+1) S1-
											sim[76n^5+271n^4+254n^3+41n^2+72n+36
													]/9/n^3/(n+1)^3
															) +
											CF^2 4 (2(n+2)/n/(n+1)(S2+S1^2)-
											2sim[3n^2+7n+2]/n/(n+1)^2 S1 +
											sim[9n^5+30n^4+16n^3-31n^2-32n-4]/n^3/(n+1)^3
															) +
											CF Tf 32 (-(n+2)/3/n/(n+1)S1+
															sim[5n^2+12n+4]/9/n/(n+1)^2
														)
										,
				s=== "gsgg1", CA^2 4 (134/9 s1+8s1/m^2-16s1/(m+1)^2+
											+8s2/m-16s2/(m+1)+4s3-8s12-8s21+8s2t/m-
											16 s2t/(m+1)+4s3t-8s12t-107/9/m+241/9/(m+1)+
											58/3/m^2-86/3/(m+1)^2-8/m^3-48/(m+1)^3-16/3
															) +
											CA Tf 32 (-5s1/9+14/9/m-19/9/(m+1)-1/3/m^2-
																1/3/(m+1)^2+1/3
																) +
											CF Tf 8 (-10/(m+1)+2/(m+1)^2+4/(m+1)^3+1+
																10/m-10/m^2+4/m^3
															)
										,
				s=== "GSGG1", CA^2 4 (-SP3-4S1 SP2+8ST+8/n/(n+1) SP2+
											2 sim[67n^4+134n^3+67n^2+144n+72
														]/9/n^2/(n+1)^2 S1 -
											sim[48n^6+144n^5+469n^4+698n^3+7n^2+258n+144
													]/9/n^3/(n+1)^3
															) +
											CA Tf 32 (-5/9S1+sim[3n^4+6n^3+16n^2+13n-3
																					]/9/n^2/(n+1)^2
																) +
											CF Tf 8 sim[n^6+3n^5+5n^4+n^3-8n^2+2n+4
																	]/n^3/(n+1)^3
],
		"NOTYET"
]];

FCPrint[1,"AnomalousDimension.m loaded"];
End[]
