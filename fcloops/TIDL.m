(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: TIDL *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on ? *)
(* ------------------------------------------------------------------------ *)

(* :Summary: TIDL *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fcloops`TIDL`",
             "HighEnergyPhysics`FeynCalc`"];

TIDL::"usage" = "TIDL[{q,mu}, {p}];
TIDL[{{qi, mu}, {qj, nu}, ...}}, {p1, p2, ...}] or
TIDL[exp, {{qi, mu}, {qj, nu}, ...}}, {p1, p2, ...}] .";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   
MakeContext[ 
ChangeDimension,
Dimension, ExpandScalarProduct, LorentzIndex,  Momentum, Pair,
               SP,MT,FV ];

Options[TIDL] = {Dimension -> D};

TIDL[{a_/;Head[a]=!=List, b_/;Head[b]=!=List},c__] := TIDL[{{a,b}},c];

TIDL[a_List, b_List, opt___Rule] := 
 Block[{en, rr},
       en = Dimension /. {opt} /. Options[TIDL];
        rr = tidl[a, b, en] /. tidlist;
        If[!FreeQ[rr, tidl],
           rr = rr /. tidl[aa_,_, en] :>
           Apply[Times,
                 Map[Pair[Momentum[#[[1]],en],LorentzIndex[#[[2]],en]]&,
             aa]]
          ];
    rr];

tidlist = {
(* Amu *)
tidl[{{q_,mu_}},{},n_] :> 0
,
(* Amunu *)
tidl[{{q1_,mu_},{q2_,nu_}},{},n_] :>
   Pair[LorentzIndex[mu, n], LorentzIndex[nu, n]]/n *
    Pair[Momentum[q1, n], Momentum[q2, n]]
,
(* Amunurho *)
tidl[{{q_,mu_},{q_,nu_},{q_,rho_}},{},n_] :> 0
,
tidl[{{q_,mu_},{q_,nu_},{q_,rho_},{q_,si_}},{},n_] :>
((MT[mu, si]*MT[nu, rho] + MT[mu, rho]*MT[nu, si] + 
       MT[mu, nu]*MT[rho, si])*SP[q, q]^2)/(n*(2 + n))
,
tidl[{{q1_,mu_},{q2_,nu_},{q3_,rho_},{q4_,si_}},{},n_] :>
(* Amunurhosi *)
Block[
{X1 -> n, X2 -> SP[q1, q2], X3 -> SP[q1, q3], X4 -> SP[q1, q4],
  X5 -> SP[q2, q3], X6 -> SP[q2, q4], X7 -> SP[q3, q4]},
ChangeDimension[
 -(((X4*X5 + X1*X4*X5 - X3*X6 - X2*X7)*MT[mu, si]*MT[nu, rho])/
    ((1 - X1)*X1*(2 + X1))) + ((1 + X1)*(X4*X5 - X3*X6 - X1*X3*X6 + X2*X7)*
    MT[mu, rho]*MT[nu, si])/(X1*(2 + X1)*(1 - X1^2)) +
  ((1 + X1)*(X4*X5 + X3*X6 - X2*X7 - X1*X2*X7)*MT[mu, nu]*MT[rho, si])/
   (X1*(2 + X1)*(1 - X1^2)), n ]
],
(* Bmu *)
tidl[{{q_,mu_}},{p_},n_] :> 
(
   (Pair[LorentzIndex[mu, n], Momentum[p, n]]*
     Pair[Momentum[p, n], Momentum[q, n]])/
   Pair[Momentum[p, n], Momentum[p, n]]
) /;  Pair[Momentum[p, n], Momentum[p, n]] =!= 0
,
(* Bmunu *)
tidl[{{q1_,mu_},{q2_,nu_}},{p_},n_] :>
(
(Pair[LorentzIndex[mu, n], LorentzIndex[nu, n]]*
   (Pair[Momentum[p, n], Momentum[q1, n]]*Pair[Momentum[p, n],
      Momentum[q2, n]] - Pair[Momentum[p, n], Momentum[p, n]]*
     Pair[Momentum[q1, n], Momentum[q2, n]]))/
  ((1 - D)*Pair[Momentum[p, n], Momentum[p, n]]) -
 (Pair[LorentzIndex[mu, n], Momentum[p, n]]*Pair[LorentzIndex[nu, n],
    Momentum[p, n]]*(D*Pair[Momentum[p, n], Momentum[q1, n]]*
     Pair[Momentum[p, n], Momentum[q2, n]] -
    Pair[Momentum[p, n], Momentum[p, n]]*Pair[Momentum[q1, n],
      Momentum[q2, n]]))/((1 - D)*Pair[Momentum[p, n], Momentum[p, n]]^2)
) /; Pair[Momentum[p, n], Momentum[p, n]] =!= 0
,
(* Bmunurho *)
tidl[{{q_,mu_},{q_,nu_},{q_,rho_}},{p_},n_] :>
ChangeDimension[
  -((FV[p, mu]*FV[p, nu]*FV[p, rho]*SP[p, q]*
((2 + n)*SP[p, q]^2 - 3*SP[p, p]*SP[q, q]))/((1 - n)*SP[p, p]^3)) + 
   (FV[p, rho]*MT[mu, nu]*SP[p, q]*(SP[p, q]^2 - SP[p, p]*SP[q, q]))/
    ((1 - n)*SP[p, p]^2) + (FV[p, nu]*MT[mu, rho]*SP[p, q]*
      (SP[p, q]^2 - SP[p, p]*SP[q, q]))/((1 - n)*SP[p, p]^2) + 
   (FV[p, mu]*MT[nu, rho]*SP[p, q]*(SP[p, q]^2 - SP[p, p]*SP[q, q]))/
    ((1 - n)*SP[p, p]^2),n]  /; 
Pair[Momentum[p, n], Momentum[p, n]] =!= 0
,
(* Bmunurhosigma *)
tidl[{{q_,mu_},{q_,nu_},{q_,rho_},{q_,si_}},{p_},n_] :>
ChangeDimension[
(SP[p, p]*(-SP[p, q]^2 + SP[p, p]*SP[q, q])*
     (FV[p, nu]*FV[p, si]*MT[mu, rho]*
        ((2 + n)*SP[p, q]^2 - SP[p, p]*SP[q, q]) + 
       FV[p, rho]*(FV[p, si]*MT[mu, nu] + FV[p, nu]*MT[mu, si])*
        ((2 + n)*SP[p, q]^2 - SP[p, p]*SP[q, q]) + 
       (MT[mu, si]*MT[nu, rho] + MT[mu, rho]*MT[nu, si] + 
   MT[mu, nu]*MT[rho, si])*SP[p, p]*(-SP[p, q]^2 + SP[p, p]*SP[q, q])) + 
FV[p, mu]*(-((FV[p, si]*MT[nu, rho] + FV[p, rho]*MT[nu, si])*SP[p, p]*
          (SP[p, q]^2 - SP[p, p]*SP[q, q])*
          ((2 + n)*SP[p, q]^2 - SP[p, p]*SP[q, q])) - 
       FV[p, nu]*(MT[rho, si]*SP[p, p]*(SP[p, q]^2 - SP[p, p]*SP[q, q])*
           ((2 + n)*SP[p, q]^2 - SP[p, p]*SP[q, q]) + 
          FV[p, rho]*FV[p, si]*(-((2 + n)*(4 + n)*SP[p, q]^4) + 
           6*(2 + n)*SP[p, p]*SP[p, q]^2*SP[q, q] - 3*SP[p, p]^2*SP[q, q]^2)
          )))/((-1 + n^2)*SP[p, p]^4)
             ,n] /; Pair[Momentum[p, n], Momentum[p, n]] =!= 0
,
(* Cmu *)
tidl[{{qi_,al_}},{p_,k_},n_] :>
(
  (Pair[LorentzIndex[al, n], Momentum[p, n]]*
       Pair[Momentum[k, n], Momentum[p, n]]*
       Pair[Momentum[k, n], Momentum[qi, n]] - 
      Pair[LorentzIndex[al, n], Momentum[p, n]]*
       Pair[Momentum[k, n], Momentum[k, n]]*
       Pair[Momentum[p, n], Momentum[qi, n]])/ 
(Pair[Momentum[k, n], Momentum[p, n]]^2 - 
      Pair[Momentum[k, n], Momentum[k, n]]*
       Pair[Momentum[p, n], Momentum[p, n]]) + 
   (-(Pair[LorentzIndex[al, n], Momentum[k, n]]*
         Pair[Momentum[k, n], Momentum[qi, n]]*
         Pair[Momentum[p, n], Momentum[p, n]]) + 
      Pair[LorentzIndex[al, n], Momentum[k, n]]*
       Pair[Momentum[k, n], Momentum[p, n]]*
       Pair[Momentum[p, n], Momentum[qi, n]])/
    (Pair[Momentum[k, n], Momentum[p, n]]^2 - 
      Pair[Momentum[k, n], Momentum[k, n]]*
       Pair[Momentum[p, n], Momentum[p, n]])
) /; Expand[ExpandScalarProduct[
    (Pair[Momentum[k, n], Momentum[p, n]]^2 -
      Pair[Momentum[k, n], Momentum[k, n]]*
       Pair[Momentum[p, n], Momentum[p, n]])
           ] ] =!= 0
,
(* Cmunu *)
tidl[{{qi_,al_},{qj_,be_}},{p_,z_},dD_] :>
Block[
{a,b,c,d,e,f,g,i,j,k,l,m,n},
a = Pair[LorentzIndex[al, dD], LorentzIndex[be, dD]];
b = Pair[LorentzIndex[al, dD], Momentum[z, dD]];
c = Pair[LorentzIndex[al, dD], Momentum[p, dD]];
d = Pair[LorentzIndex[be, dD], Momentum[z, dD]];
e = Pair[LorentzIndex[be, dD], Momentum[p, dD]];
f = Pair[Momentum[z, dD], Momentum[z, dD]];
g = Pair[Momentum[z, dD], Momentum[p, dD]];
h = Pair[Momentum[z, dD], Momentum[qi, dD]];
i = Pair[Momentum[z, dD], Momentum[qj, dD]];
k = Pair[Momentum[p, dD], Momentum[p, dD]];
l = Pair[Momentum[p, dD], Momentum[qi, dD]];
m = Pair[Momentum[p, dD], Momentum[qj, dD]];
n = Pair[Momentum[qi, dD], Momentum[qj, dD]];
(a*(h*i*k - g*i*l - g*h*m + f*l*m + g^2*n - f*k*n))/
((-2 + dD)*(g^2 - f*k)) +
(c*e*(-2*g^2*h*i + dD*g^2*h*i + f*h*i*k + f*g*i*l - dD*f*g*i*l + f*g*h*m -
dD*f*g*h*m - f^2*l*m + dD*f^2*l*m + f*g^2*n - f^2*k*n))/
((-2 + dD)*(g^2 - f*k)^2) + (c*d*
(g*h*i*k - dD*g*h*i*k + g^2*i*l - 2*f*i*k*l + dD*f*i*k*l - g^2*h*m +
dD*g^2*h*m + f*g*l*m - dD*f*g*l*m - g^3*n + f*g*k*n))/
((-2 + dD)*(g^2 - f*k)^2) + (b*e*
(g*h*i*k - dD*g*h*i*k - g^2*i*l + dD*g^2*i*l + g^2*h*m - 2*f*h*k*m +
dD*f*h*k*m + f*g*l*m - dD*f*g*l*m - g^3*n + f*g*k*n))/
((-2 + dD)*(g^2 - f*k)^2) + (b*d*
(-(h*i*k^2) + dD*h*i*k^2 + g*i*k*l - dD*g*i*k*l + g*h*k*m - 
dD*g*h*k*m -
2*g^2*l*m + dD*g^2*l*m + f*k*l*m + g^2*k*n - f*k^2*n))/
((-2 + dD)*(g^2 - f*k)^2)
] /; Expand[ExpandScalarProduct[
    (Pair[Momentum[z, n], Momentum[p, n]]^2 -
      Pair[Momentum[z, n], Momentum[z, n]]*
       Pair[Momentum[p, n], Momentum[p, n]])
           ] ] =!= 0
,
(* Cmunurho *)
tidl[{{qi_, mu_}, {qj_, nu_}, {qk_, rho_}}, {p1_, p2_},z_
    ] :>
Block[{
a,b,c,d,e,f,g,h,i,k,l,m,n,o,p,q,r,s,t,u,v },
a=Pair[LorentzIndex[mu,z],LorentzIndex[nu,z]];
b=Pair[LorentzIndex[mu,z],LorentzIndex[rho,z]];
c=Pair[LorentzIndex[mu,z],Momentum[p1,z]];
d=Pair[LorentzIndex[mu,z],Momentum[p2,z]];
e=Pair[LorentzIndex[nu,z],LorentzIndex[rho,z]];
f=Pair[LorentzIndex[nu,z],Momentum[p1,z]];
g=Pair[LorentzIndex[nu,z],Momentum[p2,z]];
h=Pair[LorentzIndex[rho,z],Momentum[p1,z]];
i=Pair[LorentzIndex[rho,z],Momentum[p2,z]];
k=Pair[Momentum[p1,z],Momentum[p1,z]];
l=Pair[Momentum[p1,z],Momentum[p2,z]];
m=Pair[Momentum[p1,z],Momentum[qi,z]];
n=Pair[Momentum[p1,z],Momentum[qj,z]];
o=Pair[Momentum[p1,z],Momentum[qk,z]];
p=Pair[Momentum[p2,z],Momentum[p2,z]];
q=Pair[Momentum[p2,z],Momentum[qi,z]];
r=Pair[Momentum[p2,z],Momentum[qj,z]];
s=Pair[Momentum[p2,z],Momentum[qk,z]];
t=Pair[Momentum[qi,z],Momentum[qj,z]];
u=Pair[Momentum[qi,z],Momentum[qk,z]];
v=Pair[Momentum[qj,z],Momentum[qk,z]];
(a*i*(l*o-k*s)*(m*n*p-l*n*q-l*m*r+k*q*r+l^2*t-k*p*t))/
((-2+z)*(l^2-k*p)^2)+(a*h*(-(o*p)+l*s)*
(m*n*p-l*n*q-l*m*r+k*q*r+l^2*t-k*p*t))/
((-2+z)*(l^2-k*p)^2)+(b*g*(l*n-k*r)*
(m*o*p-l*o*q-l*m*s+k*q*s+l^2*u-k*p*u))/
((-2+z)*(l^2-k*p)^2)+(b*f*(-(n*p)+l*r)*
(m*o*p-l*o*q-l*m*s+k*q*s+l^2*u-k*p*u))/
((-2+z)*(l^2-k*p)^2)+(d*e*(l*m-k*q)*
(n*o*p-l*o*r-l*n*s+k*r*s+l^2*v-k*p*v))/
((-2+z)*(l^2-k*p)^2)+(c*e*(-(m*p)+l*q)*
(n*o*p-l*o*r-l*n*s+k*r*s+l^2*v-k*p*v))/
((-2+z)*(l^2-k*p)^2)+(d*g*i*
(-2*l^3*m*n*o+z*l^3*m*n*o+3*k*l*m*n*o*p-z*k*l^2*n*o*q-
k^2*n*o*p*q-z*k*l^2*m*o*r-k^2*m*o*p*r+k^2*l*o*q*r+
z*k^2*l*o*q*r-z*k*l^2*m*n*s-k^2*m*n*p*s+k^2*l*n*q*s+
z*k^2*l*n*q*s+k^2*l*m*r*s+z*k^2*l*m*r*s-k^3*q*r*s-
z*k^3*q*r*s+k*l^3*o*t-k^2*l*o*p*t-k^2*l^2*s*t+k^3*p*s*t+
k*l^3*n*u-k^2*l*n*p*u-k^2*l^2*r*u+k^3*p*r*u+k*l^3*m*v-
k^2*l*m*p*v-k^2*l^2*q*v+k^3*p*q*v))/((-2+z)*(l^2-k*p)^3)+
(d*g*h*(-(z*l^2*m*n*o*p)-k*m*n*o*p^2+l^3*n*o*q+z*k*l*n*o*p*q+
l^3*m*o*r+z*k*l*m*o*p*r-2*k*l^2*o*q*r+k^2*o*p*q*r-
z*k^2*o*p*q*r+z*l^3*m*n*s+k*l*m*n*p*s-k*l^2*n*q*s-
z*k*l^2*n*q*s-k*l^2*m*r*s-z*k*l^2*m*r*s+k^2*l*q*r*s+
z*k^2*l*q*r*s-k*l^2*o*p*t+k^2*o*p^2*t+k*l^3*s*t-
k^2*l*p*s*t-l^4*n*u+k*l^2*n*p*u+k*l^3*r*u-k^2*l*p*r*u-
l^4*m*v+k*l^2*m*p*v+k*l^3*q*v-k^2*l*p*q*v))/
((-2+z)*(l^2-k*p)^3)+(d*f*i*
(-(z*l^2*m*n*o*p)-k*m*n*o*p^2+l^3*n*o*q+z*k*l*n*o*p*q+
z*l^3*m*o*r+k*l*m*o*p*r-k*l^2*o*q*r-z*k*l^2*o*q*r+
l^3*m*n*s+z*k*l*m*n*p*s-2*k*l^2*n*q*s+k^2*n*p*q*s-
z*k^2*n*p*q*s-k*l^2*m*r*s-z*k*l^2*m*r*s+k^2*l*q*r*s+
z*k^2*l*q*r*s-l^4*o*t+k*l^2*o*p*t+k*l^3*s*t-k^2*l*p*s*t-
k*l^2*n*p*u+k^2*n*p^2*u+k*l^3*r*u-k^2*l*p*r*u-l^4*m*v+
k*l^2*m*p*v+k*l^3*q*v-k^2*l*p*q*v))/((-2+z)*(l^2-k*p)^3)+
(c*g*i*(-(z*l^2*m*n*o*p)-k*m*n*o*p^2+z*l^3*n*o*q+k*l*n*o*p*q+
l^3*m*o*r+z*k*l*m*o*p*r-k*l^2*o*q*r-z*k*l^2*o*q*r+
l^3*m*n*s+z*k*l*m*n*p*s-k*l^2*n*q*s-z*k*l^2*n*q*s-
2*k*l^2*m*r*s+k^2*m*p*r*s-z*k^2*m*p*r*s+k^2*l*q*r*s+
z*k^2*l*q*r*s-l^4*o*t+k*l^2*o*p*t+k*l^3*s*t-k^2*l*p*s*t-
l^4*n*u+k*l^2*n*p*u+k*l^3*r*u-k^2*l*p*r*u-k*l^2*m*p*v+
k^2*m*p^2*v+k*l^3*q*v-k^2*l*p*q*v))/((-2+z)*(l^2-k*p)^3)+
(c*f*i*(l*m*n*o*p^2+z*l*m*n*o*p^2-l^2*n*o*p*q-z*l^2*n*o*p*q-
l^2*m*o*p*r-z*l^2*m*o*p*r+z*l^3*o*q*r+k*l*o*p*q*r-
2*l^2*m*n*p*s+k*m*n*p^2*s-z*k*m*n*p^2*s+l^3*n*q*s+
z*k*l*n*p*q*s+l^3*m*r*s+z*k*l*m*p*r*s-z*k*l^2*q*r*s-
k^2*p*q*r*s+l^3*o*p*t-k*l*o*p^2*t-k*l^2*p*s*t+k^2*p^2*s*t+
l^3*n*p*u-k*l*n*p^2*u-l^4*r*u+k*l^2*p*r*u+l^3*m*p*v-
k*l*m*p^2*v-l^4*q*v+k*l^2*p*q*v))/((-2+z)*(l^2-k*p)^3)+
(c*g*h*(l*m*n*o*p^2+z*l*m*n*o*p^2-l^2*n*o*p*q-z*l^2*n*o*p*q-
2*l^2*m*o*p*r+k*m*o*p^2*r-z*k*m*o*p^2*r+l^3*o*q*r+
z*k*l*o*p*q*r-l^2*m*n*p*s-z*l^2*m*n*p*s+z*l^3*n*q*s+
k*l*n*p*q*s+l^3*m*r*s+z*k*l*m*p*r*s-z*k*l^2*q*r*s-
k^2*p*q*r*s+l^3*o*p*t-k*l*o*p^2*t-l^4*s*t+k*l^2*p*s*t+
l^3*n*p*u-k*l*n*p^2*u-k*l^2*p*r*u+k^2*p^2*r*u+l^3*m*p*v-
k*l*m*p^2*v-l^4*q*v+k*l^2*p*q*v))/((-2+z)*(l^2-k*p)^3)+
(d*f*h*(l*m*n*o*p^2+z*l*m*n*o*p^2-2*l^2*n*o*p*q+k*n*o*p^2*q-
z*k*n*o*p^2*q-l^2*m*o*p*r-z*l^2*m*o*p*r+l^3*o*q*r+
z*k*l*o*p*q*r-l^2*m*n*p*s-z*l^2*m*n*p*s+l^3*n*q*s+
z*k*l*n*p*q*s+z*l^3*m*r*s+k*l*m*p*r*s-z*k*l^2*q*r*s-
k^2*p*q*r*s+l^3*o*p*t-k*l*o*p^2*t-l^4*s*t+k*l^2*p*s*t+
l^3*n*p*u-k*l*n*p^2*u-l^4*r*u+k*l^2*p*r*u+l^3*m*p*v-
k*l*m*p^2*v-k*l^2*p*q*v+k^2*p^2*q*v))/((-2+z)*(l^2-k*p)^3)+
(c*f*h*(-(m*n*o*p^3)-z*m*n*o*p^3+l*n*o*p^2*q+z*l*n*o*p^2*q+
l*m*o*p^2*r+z*l*m*o*p^2*r-z*l^2*o*p*q*r-k*o*p^2*q*r+
l*m*n*p^2*s+z*l*m*n*p^2*s-z*l^2*n*p*q*s-k*n*p^2*q*s-
z*l^2*m*p*r*s-k*m*p^2*r*s-2*l^3*q*r*s+z*l^3*q*r*s+
3*k*l*p*q*r*s-l^2*o*p^2*t+k*o*p^3*t+l^3*p*s*t-k*l*p^2*s*t-
l^2*n*p^2*u+k*n*p^3*u+l^3*p*r*u-k*l*p^2*r*u-l^2*m*p^2*v+
k*m*p^3*v+l^3*p*q*v-k*l*p^2*q*v))/((-2+z)*(l^2-k*p)^3)
] /; Expand[ExpandScalarProduct[
    (Pair[Momentum[p1, n], Momentum[p2, n]]^2 -
      Pair[Momentum[p1, n], Momentum[p1, n]]*
       Pair[Momentum[p2, n], Momentum[p2, n]])
           ] ] =!= 0
,
(* Cmunurhosi *)
tidl[{{q_, mu_}, {q_, nu_}, {q_, rho_}, {q_, si_}}, {p1_, p2_},n_
    ] :>
Block[{L,id=Identity},
L[1] = Pair[LorentzIndex[mu, n], LorentzIndex[si, n]];
L[2] = Pair[LorentzIndex[nu, n], LorentzIndex[rho, n]];
L[3] = Pair[Momentum[p1, n], Momentum[p2, n]];
L[4] = Pair[Momentum[p1, n], Momentum[p1, n]];
L[5] = Pair[Momentum[p2, n], Momentum[p2, n]];
L[6] = Pair[Momentum[p1, n], Momentum[q, n]];
L[7] = Pair[Momentum[p2, n], Momentum[q, n]];
L[8] = Pair[Momentum[q, n], Momentum[q, n]];
L[9] = Pair[LorentzIndex[mu, n], LorentzIndex[rho, n]];
L[10] = Pair[LorentzIndex[nu, n], LorentzIndex[si, n]];
L[11] = Pair[LorentzIndex[mu, n], LorentzIndex[nu, n]];
L[12] = Pair[LorentzIndex[rho, n], LorentzIndex[si, n]];
L[13] = Pair[LorentzIndex[mu, n], Momentum[p2, n]];
L[14] = Pair[LorentzIndex[nu, n], Momentum[p2, n]];
L[15] = Pair[LorentzIndex[rho, n], Momentum[p2, n]];
L[16] = Pair[LorentzIndex[si, n], Momentum[p2, n]];
L[17] = Pair[LorentzIndex[nu, n], Momentum[p1, n]];
L[18] = Pair[LorentzIndex[mu, n], Momentum[p1, n]];
L[19] = Pair[LorentzIndex[rho, n], Momentum[p1, n]];
L[20] = Pair[LorentzIndex[si, n], Momentum[p1, n]];
L[21] = -2 + n;
L[22] = id[L[3]]^2 - id[L[4]]*id[L[5]];
L[23] = ( id[L[5]]*id[L[6]]^2 - 
  2*id[L[3]]*id[L[6]]*id[L[7]] + 
  id[L[4]]*id[L[7]]^2 + id[L[3]]^2*id[L[8]] - 
  id[L[4]]*id[L[5]]*id[L[8]]
       );
L[24] = ( n*id[L[3]]^2*id[L[6]]^2 + 
  id[L[4]]*id[L[5]]*id[L[6]]^2 - 
  2*id[L[3]]*id[L[4]]*id[L[6]]*id[L[7]] - 
  2*n*id[L[3]]*id[L[4]]*id[L[6]]*id[L[7]] + 
  id[L[4]]^2*id[L[7]]^2 + n*id[L[4]]^2*id[L[7]]^2 + 
  id[L[3]]^2*id[L[4]]*id[L[8]] - 
  id[L[4]]^2*id[L[5]]*id[L[8]]
       );
L[25] = ( -2*n*id[L[3]]^4*id[L[6]]^4 + 
  n^2*id[L[3]]^4*id[L[6]]^4 + 
  6*n*id[L[3]]^2*id[L[4]]*id[L[5]]*id[L[6]]^4 + 
  3*id[L[4]]^2*id[L[5]]^2*id[L[6]]^4 - 
  4*n*id[L[3]]^3*id[L[4]]*id[L[6]]^3*id[L[7]] - 
  4*n^2*id[L[3]]^3*id[L[4]]*id[L[6]]^3*id[L[7]] - 
  12*id[L[3]]*id[L[4]]^2*id[L[5]]*id[L[6]]^3*
   id[L[7]] - 12*n*id[L[3]]*id[L[4]]^2*id[L[5]]*
   id[L[6]]^3*id[L[7]] + 
  12*id[L[3]]^2*id[L[4]]^2*id[L[6]]^2*id[L[7]]^2 + 
  18*n*id[L[3]]^2*id[L[4]]^2*id[L[6]]^2*id[L[7]]^2 + 
  6*n^2*id[L[3]]^2*id[L[4]]^2*id[L[6]]^2*id[L[7]]^2 + 
  6*id[L[4]]^3*id[L[5]]*id[L[6]]^2*id[L[7]]^2 + 
  6*n*id[L[4]]^3*id[L[5]]*id[L[6]]^2*id[L[7]]^2 - 
  12*id[L[3]]*id[L[4]]^3*id[L[6]]*id[L[7]]^3 - 
  16*n*id[L[3]]*id[L[4]]^3*id[L[6]]*id[L[7]]^3 - 
  4*n^2*id[L[3]]*id[L[4]]^3*id[L[6]]*id[L[7]]^3 + 
  3*id[L[4]]^4*id[L[7]]^4 + 
  4*n*id[L[4]]^4*id[L[7]]^4 + 
  n^2*id[L[4]]^4*id[L[7]]^4 + 
  6*n*id[L[3]]^4*id[L[4]]*id[L[6]]^2*id[L[8]] + 
  6*id[L[3]]^2*id[L[4]]^2*id[L[5]]*id[L[6]]^2*
   id[L[8]] - 6*n*id[L[3]]^2*id[L[4]]^2*id[L[5]]*
   id[L[6]]^2*id[L[8]] - 
  6*id[L[4]]^3*id[L[5]]^2*id[L[6]]^2*id[L[8]] - 
  12*id[L[3]]^3*id[L[4]]^2*id[L[6]]*id[L[7]]*
   id[L[8]] - 12*n*id[L[3]]^3*id[L[4]]^2*id[L[6]]*
   id[L[7]]*id[L[8]] + 
  12*id[L[3]]*id[L[4]]^3*id[L[5]]*id[L[6]]*
   id[L[7]]*id[L[8]] + 
  12*n*id[L[3]]*id[L[4]]^3*id[L[5]]*id[L[6]]*
   id[L[7]]*id[L[8]] + 
  6*id[L[3]]^2*id[L[4]]^3*id[L[7]]^2*id[L[8]] + 
  6*n*id[L[3]]^2*id[L[4]]^3*id[L[7]]^2*id[L[8]] - 
  6*id[L[4]]^4*id[L[5]]*id[L[7]]^2*id[L[8]] - 
  6*n*id[L[4]]^4*id[L[5]]*id[L[7]]^2*id[L[8]] + 
  3*id[L[3]]^4*id[L[4]]^2*id[L[8]]^2 - 
  6*id[L[3]]^2*id[L[4]]^3*id[L[5]]*id[L[8]]^2 + 
  3*id[L[4]]^4*id[L[5]]^2*id[L[8]]^2
       );
L[26] = ( -(id[L[3]]*id[L[5]]*id[L[6]]^2) - 
  n*id[L[3]]*id[L[5]]*id[L[6]]^2 + 
  2*id[L[3]]^2*id[L[6]]*id[L[7]] + 
  n*id[L[3]]^2*id[L[6]]*id[L[7]] + 
  n*id[L[4]]*id[L[5]]*id[L[6]]*id[L[7]] - 
  id[L[3]]*id[L[4]]*id[L[7]]^2 - 
  n*id[L[3]]*id[L[4]]*id[L[7]]^2 - 
  id[L[3]]^3*id[L[8]] + 
  id[L[3]]*id[L[4]]*id[L[5]]*id[L[8]]
       );
L[27] = ( -(n*id[L[3]]^3*id[L[5]]*id[L[6]]^4) - 
  n^2*id[L[3]]^3*id[L[5]]*id[L[6]]^4 - 
  3*id[L[3]]*id[L[4]]*id[L[5]]^2*id[L[6]]^4 - 
  3*n*id[L[3]]*id[L[4]]*id[L[5]]^2*id[L[6]]^4 + 
  4*n*id[L[3]]^4*id[L[6]]^3*id[L[7]] + 
  n^2*id[L[3]]^4*id[L[6]]^3*id[L[7]] + 
  12*id[L[3]]^2*id[L[4]]*id[L[5]]*id[L[6]]^3*
   id[L[7]] + 9*n*id[L[3]]^2*id[L[4]]*id[L[5]]*
   id[L[6]]^3*id[L[7]] + 
  3*n^2*id[L[3]]^2*id[L[4]]*id[L[5]]*id[L[6]]^3*
   id[L[7]] + 3*n*id[L[4]]^2*id[L[5]]^2*id[L[6]]^3*
   id[L[7]] - 12*id[L[3]]^3*id[L[4]]*id[L[6]]^2*
   id[L[7]]^2 - 15*n*id[L[3]]^3*id[L[4]]*id[L[6]]^2*
   id[L[7]]^2 - 3*n^2*id[L[3]]^3*id[L[4]]*id[L[6]]^2*
   id[L[7]]^2 - 6*id[L[3]]*id[L[4]]^2*id[L[5]]*
   id[L[6]]^2*id[L[7]]^2 - 
  9*n*id[L[3]]*id[L[4]]^2*id[L[5]]*id[L[6]]^2*
   id[L[7]]^2 - 3*n^2*id[L[3]]*id[L[4]]^2*id[L[5]]*
   id[L[6]]^2*id[L[7]]^2 + 
  12*id[L[3]]^2*id[L[4]]^2*id[L[6]]*id[L[7]]^3 + 
  15*n*id[L[3]]^2*id[L[4]]^2*id[L[6]]*id[L[7]]^3 + 
  3*n^2*id[L[3]]^2*id[L[4]]^2*id[L[6]]*id[L[7]]^3 + 
  n*id[L[4]]^3*id[L[5]]*id[L[6]]*id[L[7]]^3 + 
  n^2*id[L[4]]^3*id[L[5]]*id[L[6]]*id[L[7]]^3 - 
  3*id[L[3]]*id[L[4]]^3*id[L[7]]^4 - 
  4*n*id[L[3]]*id[L[4]]^3*id[L[7]]^4 - 
  n^2*id[L[3]]*id[L[4]]^3*id[L[7]]^4 - 
  3*n*id[L[3]]^5*id[L[6]]^2*id[L[8]] - 
  6*id[L[3]]^3*id[L[4]]*id[L[5]]*id[L[6]]^2*
   id[L[8]] + 6*id[L[3]]*id[L[4]]^2*id[L[5]]^2*
   id[L[6]]^2*id[L[8]] + 
  3*n*id[L[3]]*id[L[4]]^2*id[L[5]]^2*id[L[6]]^2*
   id[L[8]] + 12*id[L[3]]^4*id[L[4]]*id[L[6]]*
   id[L[7]]*id[L[8]] + 
  9*n*id[L[3]]^4*id[L[4]]*id[L[6]]*id[L[7]]*
   id[L[8]] - 12*id[L[3]]^2*id[L[4]]^2*id[L[5]]*
   id[L[6]]*id[L[7]]*id[L[8]] - 
  6*n*id[L[3]]^2*id[L[4]]^2*id[L[5]]*id[L[6]]*
   id[L[7]]*id[L[8]] - 
  3*n*id[L[4]]^3*id[L[5]]^2*id[L[6]]*id[L[7]]*
   id[L[8]] - 6*id[L[3]]^3*id[L[4]]^2*id[L[7]]^2*
   id[L[8]] - 6*n*id[L[3]]^3*id[L[4]]^2*id[L[7]]^2*
   id[L[8]] + 6*id[L[3]]*id[L[4]]^3*id[L[5]]*
   id[L[7]]^2*id[L[8]] + 
  6*n*id[L[3]]*id[L[4]]^3*id[L[5]]*id[L[7]]^2*
   id[L[8]] - 3*id[L[3]]^5*id[L[4]]*id[L[8]]^2 + 
  6*id[L[3]]^3*id[L[4]]^2*id[L[5]]*id[L[8]]^2 - 
  3*id[L[3]]*id[L[4]]^3*id[L[5]]^2*id[L[8]]^2
       );
L[28] = ( id[L[5]]^2*id[L[6]]^2 + n*id[L[5]]^2*id[L[6]]^2 - 
  2*id[L[3]]*id[L[5]]*id[L[6]]*id[L[7]] - 
  2*n*id[L[3]]*id[L[5]]*id[L[6]]*id[L[7]] + 
  n*id[L[3]]^2*id[L[7]]^2 + 
  id[L[4]]*id[L[5]]*id[L[7]]^2 + 
  id[L[3]]^2*id[L[5]]*id[L[8]] - 
  id[L[4]]*id[L[5]]^2*id[L[8]]
       );
L[29] = ( 2*id[L[3]]^2*id[L[5]]^2*id[L[6]]^4 + 
  3*n*id[L[3]]^2*id[L[5]]^2*id[L[6]]^4 + 
  n^2*id[L[3]]^2*id[L[5]]^2*id[L[6]]^4 + 
  id[L[4]]*id[L[5]]^3*id[L[6]]^4 + 
  n*id[L[4]]*id[L[5]]^3*id[L[6]]^4 - 
  8*id[L[3]]^3*id[L[5]]*id[L[6]]^3*id[L[7]] - 
  10*n*id[L[3]]^3*id[L[5]]*id[L[6]]^3*id[L[7]] - 
  2*n^2*id[L[3]]^3*id[L[5]]*id[L[6]]^3*id[L[7]] - 
  4*id[L[3]]*id[L[4]]*id[L[5]]^2*id[L[6]]^3*
   id[L[7]] - 6*n*id[L[3]]*id[L[4]]*id[L[5]]^2*
   id[L[6]]^3*id[L[7]] - 
  2*n^2*id[L[3]]*id[L[4]]*id[L[5]]^2*id[L[6]]^3*
   id[L[7]] + 8*id[L[3]]^4*id[L[6]]^2*id[L[7]]^2 + 
  6*n*id[L[3]]^4*id[L[6]]^2*id[L[7]]^2 + 
  n^2*id[L[3]]^4*id[L[6]]^2*id[L[7]]^2 + 
  8*id[L[3]]^2*id[L[4]]*id[L[5]]*id[L[6]]^2*
   id[L[7]]^2 + 18*n*id[L[3]]^2*id[L[4]]*id[L[5]]*
   id[L[6]]^2*id[L[7]]^2 + 
  4*n^2*id[L[3]]^2*id[L[4]]*id[L[5]]*id[L[6]]^2*
   id[L[7]]^2 + 2*id[L[4]]^2*id[L[5]]^2*id[L[6]]^2*
   id[L[7]]^2 + n^2*id[L[4]]^2*id[L[5]]^2*id[L[6]]^2*
   id[L[7]]^2 - 8*id[L[3]]^3*id[L[4]]*id[L[6]]*
   id[L[7]]^3 - 10*n*id[L[3]]^3*id[L[4]]*id[L[6]]*
   id[L[7]]^3 - 2*n^2*id[L[3]]^3*id[L[4]]*id[L[6]]*
   id[L[7]]^3 - 4*id[L[3]]*id[L[4]]^2*id[L[5]]*
   id[L[6]]*id[L[7]]^3 - 
  6*n*id[L[3]]*id[L[4]]^2*id[L[5]]*id[L[6]]*
   id[L[7]]^3 - 2*n^2*id[L[3]]*id[L[4]]^2*id[L[5]]*
   id[L[6]]*id[L[7]]^3 + 
  2*id[L[3]]^2*id[L[4]]^2*id[L[7]]^4 + 
  3*n*id[L[3]]^2*id[L[4]]^2*id[L[7]]^4 + 
  n^2*id[L[3]]^2*id[L[4]]^2*id[L[7]]^4 + 
  id[L[4]]^3*id[L[5]]*id[L[7]]^4 + 
  n*id[L[4]]^3*id[L[5]]*id[L[7]]^4 + 
  4*id[L[3]]^4*id[L[5]]*id[L[6]]^2*id[L[8]] + 
  5*n*id[L[3]]^4*id[L[5]]*id[L[6]]^2*id[L[8]] - 
  2*id[L[3]]^2*id[L[4]]*id[L[5]]^2*id[L[6]]^2*
   id[L[8]] - 4*n*id[L[3]]^2*id[L[4]]*id[L[5]]^2*
   id[L[6]]^2*id[L[8]] - 
  2*id[L[4]]^2*id[L[5]]^3*id[L[6]]^2*id[L[8]] - 
  n*id[L[4]]^2*id[L[5]]^3*id[L[6]]^2*id[L[8]] - 
  8*id[L[3]]^5*id[L[6]]*id[L[7]]*id[L[8]] - 
  4*n*id[L[3]]^5*id[L[6]]*id[L[7]]*id[L[8]] + 
  4*id[L[3]]^3*id[L[4]]*id[L[5]]*id[L[6]]*
   id[L[7]]*id[L[8]] - 
  4*n*id[L[3]]^3*id[L[4]]*id[L[5]]*id[L[6]]*
   id[L[7]]*id[L[8]] + 
  4*id[L[3]]*id[L[4]]^2*id[L[5]]^2*id[L[6]]*
   id[L[7]]*id[L[8]] + 
  8*n*id[L[3]]*id[L[4]]^2*id[L[5]]^2*id[L[6]]*
   id[L[7]]*id[L[8]] + 
  4*id[L[3]]^4*id[L[4]]*id[L[7]]^2*id[L[8]] + 
  5*n*id[L[3]]^4*id[L[4]]*id[L[7]]^2*id[L[8]] - 
  2*id[L[3]]^2*id[L[4]]^2*id[L[5]]*id[L[7]]^2*
   id[L[8]] - 4*n*id[L[3]]^2*id[L[4]]^2*id[L[5]]*
   id[L[7]]^2*id[L[8]] - 
  2*id[L[4]]^3*id[L[5]]^2*id[L[7]]^2*id[L[8]] - 
  n*id[L[4]]^3*id[L[5]]^2*id[L[7]]^2*id[L[8]] + 
  2*id[L[3]]^6*id[L[8]]^2 - 
  3*id[L[3]]^4*id[L[4]]*id[L[5]]*id[L[8]]^2 + 
  id[L[4]]^3*id[L[5]]^3*id[L[8]]^2
       );
L[30] = ( -3*id[L[3]]*id[L[5]]^3*id[L[6]]^4 - 
  4*n*id[L[3]]*id[L[5]]^3*id[L[6]]^4 - 
  n^2*id[L[3]]*id[L[5]]^3*id[L[6]]^4 + 
  12*id[L[3]]^2*id[L[5]]^2*id[L[6]]^3*id[L[7]] + 
  15*n*id[L[3]]^2*id[L[5]]^2*id[L[6]]^3*id[L[7]] + 
  3*n^2*id[L[3]]^2*id[L[5]]^2*id[L[6]]^3*id[L[7]] + 
  n*id[L[4]]*id[L[5]]^3*id[L[6]]^3*id[L[7]] + 
  n^2*id[L[4]]*id[L[5]]^3*id[L[6]]^3*id[L[7]] - 
  12*id[L[3]]^3*id[L[5]]*id[L[6]]^2*id[L[7]]^2 - 
  15*n*id[L[3]]^3*id[L[5]]*id[L[6]]^2*id[L[7]]^2 - 
  3*n^2*id[L[3]]^3*id[L[5]]*id[L[6]]^2*id[L[7]]^2 - 
  6*id[L[3]]*id[L[4]]*id[L[5]]^2*id[L[6]]^2*
   id[L[7]]^2 - 9*n*id[L[3]]*id[L[4]]*id[L[5]]^2*
   id[L[6]]^2*id[L[7]]^2 - 
  3*n^2*id[L[3]]*id[L[4]]*id[L[5]]^2*id[L[6]]^2*
   id[L[7]]^2 + 4*n*id[L[3]]^4*id[L[6]]*id[L[7]]^3 + 
  n^2*id[L[3]]^4*id[L[6]]*id[L[7]]^3 + 
  12*id[L[3]]^2*id[L[4]]*id[L[5]]*id[L[6]]*
   id[L[7]]^3 + 9*n*id[L[3]]^2*id[L[4]]*id[L[5]]*
   id[L[6]]*id[L[7]]^3 + 
  3*n^2*id[L[3]]^2*id[L[4]]*id[L[5]]*id[L[6]]*
   id[L[7]]^3 + 3*n*id[L[4]]^2*id[L[5]]^2*id[L[6]]*
   id[L[7]]^3 - n*id[L[3]]^3*id[L[4]]*id[L[7]]^4 - 
  n^2*id[L[3]]^3*id[L[4]]*id[L[7]]^4 - 
  3*id[L[3]]*id[L[4]]^2*id[L[5]]*id[L[7]]^4 - 
  3*n*id[L[3]]*id[L[4]]^2*id[L[5]]*id[L[7]]^4 - 
  6*id[L[3]]^3*id[L[5]]^2*id[L[6]]^2*id[L[8]] - 
  6*n*id[L[3]]^3*id[L[5]]^2*id[L[6]]^2*id[L[8]] + 
  6*id[L[3]]*id[L[4]]*id[L[5]]^3*id[L[6]]^2*
   id[L[8]] + 6*n*id[L[3]]*id[L[4]]*id[L[5]]^3*
   id[L[6]]^2*id[L[8]] + 
  12*id[L[3]]^4*id[L[5]]*id[L[6]]*id[L[7]]*
   id[L[8]] + 9*n*id[L[3]]^4*id[L[5]]*id[L[6]]*
   id[L[7]]*id[L[8]] - 
  12*id[L[3]]^2*id[L[4]]*id[L[5]]^2*id[L[6]]*
   id[L[7]]*id[L[8]] - 
  6*n*id[L[3]]^2*id[L[4]]*id[L[5]]^2*id[L[6]]*
   id[L[7]]*id[L[8]] - 
  3*n*id[L[4]]^2*id[L[5]]^3*id[L[6]]*id[L[7]]*
   id[L[8]] - 3*n*id[L[3]]^5*id[L[7]]^2*id[L[8]] - 
  6*id[L[3]]^3*id[L[4]]*id[L[5]]*id[L[7]]^2*
   id[L[8]] + 6*id[L[3]]*id[L[4]]^2*id[L[5]]^2*
   id[L[7]]^2*id[L[8]] + 
  3*n*id[L[3]]*id[L[4]]^2*id[L[5]]^2*id[L[7]]^2*
   id[L[8]] - 3*id[L[3]]^5*id[L[5]]*id[L[8]]^2 + 
  6*id[L[3]]^3*id[L[4]]*id[L[5]]^2*id[L[8]]^2 - 
  3*id[L[3]]*id[L[4]]^2*id[L[5]]^3*id[L[8]]^2
       );
L[31] = ( 3*id[L[5]]^4*id[L[6]]^4 + 4*n*id[L[5]]^4*id[L[6]]^4 + 
  n^2*id[L[5]]^4*id[L[6]]^4 - 
  12*id[L[3]]*id[L[5]]^3*id[L[6]]^3*id[L[7]] - 
  16*n*id[L[3]]*id[L[5]]^3*id[L[6]]^3*id[L[7]] - 
  4*n^2*id[L[3]]*id[L[5]]^3*id[L[6]]^3*id[L[7]] + 
  12*id[L[3]]^2*id[L[5]]^2*id[L[6]]^2*id[L[7]]^2 + 
  18*n*id[L[3]]^2*id[L[5]]^2*id[L[6]]^2*id[L[7]]^2 + 
  6*n^2*id[L[3]]^2*id[L[5]]^2*id[L[6]]^2*id[L[7]]^2 + 
  6*id[L[4]]*id[L[5]]^3*id[L[6]]^2*id[L[7]]^2 + 
  6*n*id[L[4]]*id[L[5]]^3*id[L[6]]^2*id[L[7]]^2 - 
  4*n*id[L[3]]^3*id[L[5]]*id[L[6]]*id[L[7]]^3 - 
  4*n^2*id[L[3]]^3*id[L[5]]*id[L[6]]*id[L[7]]^3 - 
  12*id[L[3]]*id[L[4]]*id[L[5]]^2*id[L[6]]*
   id[L[7]]^3 - 12*n*id[L[3]]*id[L[4]]*id[L[5]]^2*
   id[L[6]]*id[L[7]]^3 - 2*n*id[L[3]]^4*id[L[7]]^4 + 
  n^2*id[L[3]]^4*id[L[7]]^4 + 
  6*n*id[L[3]]^2*id[L[4]]*id[L[5]]*id[L[7]]^4 + 
  3*id[L[4]]^2*id[L[5]]^2*id[L[7]]^4 + 
  6*id[L[3]]^2*id[L[5]]^3*id[L[6]]^2*id[L[8]] + 
  6*n*id[L[3]]^2*id[L[5]]^3*id[L[6]]^2*id[L[8]] - 
  6*id[L[4]]*id[L[5]]^4*id[L[6]]^2*id[L[8]] - 
  6*n*id[L[4]]*id[L[5]]^4*id[L[6]]^2*id[L[8]] - 
  12*id[L[3]]^3*id[L[5]]^2*id[L[6]]*id[L[7]]*
   id[L[8]] - 12*n*id[L[3]]^3*id[L[5]]^2*id[L[6]]*
   id[L[7]]*id[L[8]] + 
  12*id[L[3]]*id[L[4]]*id[L[5]]^3*id[L[6]]*
   id[L[7]]*id[L[8]] + 
  12*n*id[L[3]]*id[L[4]]*id[L[5]]^3*id[L[6]]*
   id[L[7]]*id[L[8]] + 
  6*n*id[L[3]]^4*id[L[5]]*id[L[7]]^2*id[L[8]] + 
  6*id[L[3]]^2*id[L[4]]*id[L[5]]^2*id[L[7]]^2*
   id[L[8]] - 6*n*id[L[3]]^2*id[L[4]]*id[L[5]]^2*
   id[L[7]]^2*id[L[8]] - 
  6*id[L[4]]^2*id[L[5]]^3*id[L[7]]^2*id[L[8]] + 
  3*id[L[3]]^4*id[L[5]]^2*id[L[8]]^2 - 
  6*id[L[3]]^2*id[L[4]]*id[L[5]]^3*id[L[8]]^2 + 
  3*id[L[4]]^2*id[L[5]]^4*id[L[8]]^2
       );
L[32] = ( (id[L[1]]*id[L[2]]*id[L[23]]^2)/
   (n*id[L[21]]*id[L[22]]^2) + 
  (id[L[9]]*id[L[10]]*id[L[23]]^2)/
   (n*id[L[21]]*id[L[22]]^2) + 
  (id[L[11]]*id[L[12]]*id[L[23]]^2)/
   (n*id[L[21]]*id[L[22]]^2) + 
  (id[L[12]]*id[L[13]]*id[L[14]]*id[L[23]]*
     id[L[24]])/(n*id[L[21]]*id[L[22]]^3) + 
  (id[L[10]]*id[L[13]]*id[L[15]]*id[L[23]]*
     id[L[24]])/(n*id[L[21]]*id[L[22]]^3) + 
  (id[L[1]]*id[L[14]]*id[L[15]]*id[L[23]]*
     id[L[24]])/(n*id[L[21]]*id[L[22]]^3) + 
  (id[L[2]]*id[L[13]]*id[L[16]]*id[L[23]]*
     id[L[24]])/(n*id[L[21]]*id[L[22]]^3) + 
  (id[L[9]]*id[L[14]]*id[L[16]]*id[L[23]]*
     id[L[24]])/(n*id[L[21]]*id[L[22]]^3) + 
  (id[L[11]]*id[L[15]]*id[L[16]]*id[L[23]]*
     id[L[24]])/(n*id[L[21]]*id[L[22]]^3) + 
  (id[L[13]]*id[L[14]]*id[L[15]]*id[L[16]]*
     id[L[25]])/(n*id[L[21]]*id[L[22]]^4) + 
  (id[L[12]]*id[L[13]]*id[L[17]]*id[L[23]]*
     id[L[26]])/(n*id[L[21]]*id[L[22]]^3) + 
  (id[L[1]]*id[L[15]]*id[L[17]]*id[L[23]]*
     id[L[26]])/(n*id[L[21]]*id[L[22]]^3) + 
  (id[L[9]]*id[L[16]]*id[L[17]]*id[L[23]]*
     id[L[26]])/(n*id[L[21]]*id[L[22]]^3) + 
  (id[L[12]]*id[L[14]]*id[L[18]]*id[L[23]]*
     id[L[26]])/(n*id[L[21]]*id[L[22]]^3) + 
  (id[L[10]]*id[L[15]]*id[L[18]]*id[L[23]]*
     id[L[26]])/(n*id[L[21]]*id[L[22]]^3) + 
  (id[L[2]]*id[L[16]]*id[L[18]]*id[L[23]]*
     id[L[26]])/(n*id[L[21]]*id[L[22]]^3) + 
  (id[L[10]]*id[L[13]]*id[L[19]]*id[L[23]]*
     id[L[26]])/(n*id[L[21]]*id[L[22]]^3) + 
  (id[L[1]]*id[L[14]]*id[L[19]]*id[L[23]]*
     id[L[26]])/(n*id[L[21]]*id[L[22]]^3) + 
  (id[L[11]]*id[L[16]]*id[L[19]]*id[L[23]]*
     id[L[26]])/(n*id[L[21]]*id[L[22]]^3) + 
  (id[L[2]]*id[L[13]]*id[L[20]]*id[L[23]]*
     id[L[26]])/(n*id[L[21]]*id[L[22]]^3) + 
  (id[L[9]]*id[L[14]]*id[L[20]]*id[L[23]]*
     id[L[26]])/(n*id[L[21]]*id[L[22]]^3) + 
  (id[L[11]]*id[L[15]]*id[L[20]]*id[L[23]]*
     id[L[26]])/(n*id[L[21]]*id[L[22]]^3) + 
  (id[L[13]]*id[L[15]]*id[L[16]]*id[L[17]]*
     id[L[27]])/(n*id[L[21]]*id[L[22]]^4) + 
  (id[L[14]]*id[L[15]]*id[L[16]]*id[L[18]]*
     id[L[27]])/(n*id[L[21]]*id[L[22]]^4) + 
  (id[L[13]]*id[L[14]]*id[L[16]]*id[L[19]]*
     id[L[27]])/(n*id[L[21]]*id[L[22]]^4) + 
  (id[L[13]]*id[L[14]]*id[L[15]]*id[L[20]]*
     id[L[27]])/(n*id[L[21]]*id[L[22]]^4) + 
  (id[L[12]]*id[L[17]]*id[L[18]]*id[L[23]]*
     id[L[28]])/(n*id[L[21]]*id[L[22]]^3) + 
  (id[L[1]]*id[L[17]]*id[L[19]]*id[L[23]]*
     id[L[28]])/(n*id[L[21]]*id[L[22]]^3) + 
  (id[L[10]]*id[L[18]]*id[L[19]]*id[L[23]]*
     id[L[28]])/(n*id[L[21]]*id[L[22]]^3) + 
  (id[L[9]]*id[L[17]]*id[L[20]]*id[L[23]]*
     id[L[28]])/(n*id[L[21]]*id[L[22]]^3) + 
  (id[L[2]]*id[L[18]]*id[L[20]]*id[L[23]]*
     id[L[28]])/(n*id[L[21]]*id[L[22]]^3) + 
  (id[L[11]]*id[L[19]]*id[L[20]]*id[L[23]]*
     id[L[28]])/(n*id[L[21]]*id[L[22]]^3) + 
  (id[L[15]]*id[L[16]]*id[L[17]]*id[L[18]]*
     id[L[29]])/(n*id[L[21]]*id[L[22]]^4) + 
  (id[L[13]]*id[L[16]]*id[L[17]]*id[L[19]]*
     id[L[29]])/(n*id[L[21]]*id[L[22]]^4) + 
  (id[L[14]]*id[L[16]]*id[L[18]]*id[L[19]]*
     id[L[29]])/(n*id[L[21]]*id[L[22]]^4) + 
  (id[L[13]]*id[L[15]]*id[L[17]]*id[L[20]]*
     id[L[29]])/(n*id[L[21]]*id[L[22]]^4) + 
  (id[L[14]]*id[L[15]]*id[L[18]]*id[L[20]]*
     id[L[29]])/(n*id[L[21]]*id[L[22]]^4) + 
  (id[L[13]]*id[L[14]]*id[L[19]]*id[L[20]]*
     id[L[29]])/(n*id[L[21]]*id[L[22]]^4) + 
  (id[L[16]]*id[L[17]]*id[L[18]]*id[L[19]]*
     id[L[30]])/(n*id[L[21]]*id[L[22]]^4) + 
  (id[L[15]]*id[L[17]]*id[L[18]]*id[L[20]]*
     id[L[30]])/(n*id[L[21]]*id[L[22]]^4) + 
  (id[L[13]]*id[L[17]]*id[L[19]]*id[L[20]]*
     id[L[30]])/(n*id[L[21]]*id[L[22]]^4) + 
  (id[L[14]]*id[L[18]]*id[L[19]]*id[L[20]]*
     id[L[30]])/(n*id[L[21]]*id[L[22]]^4) + 
  (id[L[17]]*id[L[18]]*id[L[19]]*id[L[20]]*
     id[L[31]])/(n*id[L[21]]*id[L[22]]^4)
       )] /; Expand[ExpandScalarProduct[
    (Pair[Momentum[p1, n], Momentum[p2, n]]^2 -
      Pair[Momentum[p1, n], Momentum[p1, n]]*
       Pair[Momentum[p2, n], Momentum[p2, n]])
           ] ] =!= 0
,
(* Cmunurhosidelta *)
tidl[{{q_, mu_}, {q_, nu_}, {q_, rho_}, {q_, si_}, {q_,de_}}, 
      {p1_, p2_},n_
    ] :> Block[{
x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, 
x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, 
x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42
},
x1=Pair[LorentzIndex[de,n],Momentum[p2,n]];
x2=Pair[LorentzIndex[mu,n],LorentzIndex[si,n]];
x3=Pair[LorentzIndex[nu,n],LorentzIndex[rho,n]];
x4=Pair[Momentum[p1,n],Momentum[p2,n]];
x5=Pair[Momentum[p1,n],Momentum[p1,n]];
x6=Pair[Momentum[p2,n],Momentum[p2,n]];
x7=Pair[Momentum[p1,n],Momentum[q,n]];
x8=Pair[Momentum[p2,n],Momentum[q,n]];
x9=Pair[Momentum[q,n],Momentum[q,n]];
x10=Pair[LorentzIndex[de,n],LorentzIndex[si,n]];
x11=Pair[LorentzIndex[mu,n],Momentum[p2,n]];
x12=Pair[LorentzIndex[mu,n],LorentzIndex[rho,n]];
x13=Pair[LorentzIndex[nu,n],LorentzIndex[si,n]];
x14=Pair[LorentzIndex[de,n],LorentzIndex[rho,n]];
x15=Pair[LorentzIndex[nu,n],Momentum[p2,n]];
x16=Pair[LorentzIndex[mu,n],LorentzIndex[nu,n]];
x17=Pair[LorentzIndex[rho,n],LorentzIndex[si,n]];
x18=Pair[LorentzIndex[de,n],LorentzIndex[nu,n]];
x19=Pair[LorentzIndex[de,n],LorentzIndex[mu,n]];
x20=Pair[LorentzIndex[rho,n],Momentum[p2,n]];
x21=Pair[LorentzIndex[si,n],Momentum[p2,n]];
x22=Pair[LorentzIndex[de,n],Momentum[p1,n]];
x23=Pair[LorentzIndex[mu,n],Momentum[p1,n]];
x24=Pair[LorentzIndex[nu,n],Momentum[p1,n]];
x25=Pair[LorentzIndex[rho,n],Momentum[p1,n]];
x26=Pair[LorentzIndex[si,n],Momentum[p1,n]];
x27=2-n;
x28=x4^2-x5*x6;x29=x4*x7-x5*x8;
x30=x6*x7^2-2*x4*x7*x8+x5*x8^2+x4^2*x9-x5*x6*x9;
x31=n*x4^2*x7^2+3*x5*x6*x7^2-6*x4*x5*x7*x8-2*n*x4*x5*x7*x8+
3*x5^2*x8^2+n*x5^2*x8^2+3*x4^2*x5*x9-3*x5^2*x6*x9;
x32=2*n*x4^4*x7^4-n^2*x4^4*x7^4-10*n*x4^2*x5*x6*x7^4-
15*x5^2*x6^2*x7^4+12*n*x4^3*x5*x7^3*x8+4*n^2*x4^3*x5*x7^3*x8+
60*x4*x5^2*x6*x7^3*x8+20*n*x4*x5^2*x6*x7^3*x8-
60*x4^2*x5^2*x7^2*x8^2-38*n*x4^2*x5^2*x7^2*x8^2-
6*n^2*x4^2*x5^2*x7^2*x8^2-30*x5^3*x6*x7^2*x8^2-
10*n*x5^3*x6*x7^2*x8^2+60*x4*x5^3*x7*x8^3+32*n*x4*x5^3*x7*x8^3+
4*n^2*x4*x5^3*x7*x8^3-15*x5^4*x8^4-8*n*x5^4*x8^4-n^2*x5^4*x8^4-
10*n*x4^4*x5*x7^2*x9-30*x4^2*x5^2*x6*x7^2*x9+
10*n*x4^2*x5^2*x6*x7^2*x9+30*x5^3*x6^2*x7^2*x9+
60*x4^3*x5^2*x7*x8*x9+20*n*x4^3*x5^2*x7*x8*x9-
60*x4*x5^3*x6*x7*x8*x9-20*n*x4*x5^3*x6*x7*x8*x9-
30*x4^2*x5^3*x8^2*x9-10*n*x4^2*x5^3*x8^2*x9+30*x5^4*x6*x8^2*x9+
10*n*x5^4*x6*x8^2*x9-15*x4^4*x5^2*x9^2+30*x4^2*x5^3*x6*x9^2-
15*x5^4*x6^2*x9^2;x33=x6*x7-x4*x8;
x34=2*x4^2*x6*x7^3+n*x4^2*x6*x7^3+x5*x6^2*x7^3-4*x4^3*x7^2*x8-
n*x4^3*x7^2*x8-5*x4*x5*x6*x7^2*x8-2*n*x4*x5*x6*x7^2*x8+
8*x4^2*x5*x7*x8^2+2*n*x4^2*x5*x7*x8^2+x5^2*x6*x7*x8^2+
n*x5^2*x6*x7*x8^2-3*x4*x5^2*x8^3-n*x4*x5^2*x8^3+2*x4^4*x7*x9-
x4^2*x5*x6*x7*x9-x5^2*x6^2*x7*x9-3*x4^3*x5*x8*x9+3*x4*x5^2*x6*x8*x9\
;x35=2*n*x4^4*x6*x7^5+n^2*x4^4*x6*x7^5+12*x4^2*x5*x6^2*x7^5+
6*n*x4^2*x5*x6^2*x7^5+3*x5^2*x6^3*x7^5-6*n*x4^5*x7^4*x8-
n^2*x4^5*x7^4*x8-48*x4^3*x5*x6*x7^4*x8-22*n*x4^3*x5*x6*x7^4*x8-
4*n^2*x4^3*x5*x6*x7^4*x8-27*x4*x5^2*x6^2*x7^4*x8-
12*n*x4*x5^2*x6^2*x7^4*x8+48*x4^4*x5*x7^3*x8^2+
32*n*x4^4*x5*x7^3*x8^2+4*n^2*x4^4*x5*x7^3*x8^2+
96*x4^2*x5^2*x6*x7^3*x8^2+42*n*x4^2*x5^2*x6*x7^3*x8^2+
6*n^2*x4^2*x5^2*x6*x7^3*x8^2+6*x5^3*x6^2*x7^3*x8^2+
6*n*x5^3*x6^2*x7^3*x8^2-108*x4^3*x5^2*x7^2*x8^3-
54*n*x4^3*x5^2*x7^2*x8^3-6*n^2*x4^3*x5^2*x7^2*x8^3-
42*x4*x5^3*x6*x7^2*x8^3-26*n*x4*x5^3*x6*x7^2*x8^3-
4*n^2*x4*x5^3*x6*x7^2*x8^3+72*x4^2*x5^3*x7*x8^4+
36*n*x4^2*x5^3*x7*x8^4+4*n^2*x4^2*x5^3*x7*x8^4+3*x5^4*x6*x7*x8^4+
4*n*x5^4*x6*x7*x8^4+n^2*x5^4*x6*x7*x8^4-15*x4*x5^4*x8^5-
8*n*x4*x5^4*x8^5-n^2*x4*x5^4*x8^5+4*n*x4^6*x7^3*x9+
24*x4^4*x5*x6*x7^3*x9+2*n*x4^4*x5*x6*x7^3*x9-
18*x4^2*x5^2*x6^2*x7^3*x9-6*n*x4^2*x5^2*x6^2*x7^3*x9-
6*x5^3*x6^3*x7^3*x9-48*x4^5*x5*x7^2*x8*x9-18*n*x4^5*x5*x7^2*x8*x9+
6*x4^3*x5^2*x6*x7^2*x8*x9+6*n*x4^3*x5^2*x6*x7^2*x8*x9+
42*x4*x5^3*x6^2*x7^2*x8*x9+12*n*x4*x5^3*x6^2*x7^2*x8*x9+
84*x4^4*x5^2*x7*x8^2*x9+24*n*x4^4*x5^2*x7*x8^2*x9-
78*x4^2*x5^3*x6*x7*x8^2*x9-18*n*x4^2*x5^3*x6*x7*x8^2*x9-
6*x5^4*x6^2*x7*x8^2*x9-6*n*x5^4*x6^2*x7*x8^2*x9-
30*x4^3*x5^3*x8^3*x9-10*n*x4^3*x5^3*x8^3*x9+30*x4*x5^4*x6*x8^3*x9+
10*n*x4*x5^4*x6*x8^3*x9+12*x4^6*x5*x7*x9^2-21*x4^4*x5^2*x6*x7*x9^2+
6*x4^2*x5^3*x6^2*x7*x9^2+3*x5^4*x6^3*x7*x9^2-15*x4^5*x5^2*x8*x9^2+
30*x4^3*x5^3*x6*x8*x9^2-15*x4*x5^4*x6^2*x8*x9^2;
x36=3*x4*x6^2*x7^3+n*x4*x6^2*x7^3-8*x4^2*x6*x7^2*x8-
2*n*x4^2*x6*x7^2*x8-x5*x6^2*x7^2*x8-n*x5*x6^2*x7^2*x8+
4*x4^3*x7*x8^2+n*x4^3*x7*x8^2+5*x4*x5*x6*x7*x8^2+
2*n*x4*x5*x6*x7*x8^2-2*x4^2*x5*x8^3-n*x4^2*x5*x8^3-x5^2*x6*x8^3+
3*x4^3*x6*x7*x9-3*x4*x5*x6^2*x7*x9-2*x4^4*x8*x9+x4^2*x5*x6*x8*x9+
x5^2*x6^2*x8*x9;x37=6*x4^3*x6^2*x7^5+5*n*x4^3*x6^2*x7^5+
n^2*x4^3*x6^2*x7^5+9*x4*x5*x6^3*x7^5+3*n*x4*x5*x6^3*x7^5-
24*x4^4*x6*x7^4*x8-16*n*x4^4*x6*x7^4*x8-2*n^2*x4^4*x6*x7^4*x8-
48*x4^2*x5*x6^2*x7^4*x8-21*n*x4^2*x5*x6^2*x7^4*x8-
3*n^2*x4^2*x5*x6^2*x7^4*x8-3*x5^2*x6^3*x7^4*x8-
3*n*x5^2*x6^3*x7^4*x8+24*x4^5*x7^3*x8^2+10*n*x4^5*x7^3*x8^2+
n^2*x4^5*x7^3*x8^2+96*x4^3*x5*x6*x7^3*x8^2+
52*n*x4^3*x5*x6*x7^3*x8^2+6*n^2*x4^3*x5*x6*x7^3*x8^2+
30*x4*x5^2*x6^2*x7^3*x8^2+18*n*x4*x5^2*x6^2*x7^3*x8^2+
3*n^2*x4*x5^2*x6^2*x7^3*x8^2-72*x4^4*x5*x7^2*x8^3-
30*n*x4^4*x5*x7^2*x8^3-3*n^2*x4^4*x5*x7^2*x8^3-
72*x4^2*x5^2*x6*x7^2*x8^3-48*n*x4^2*x5^2*x6*x7^2*x8^3-
6*n^2*x4^2*x5^2*x6*x7^2*x8^3-6*x5^3*x6^2*x7^2*x8^3-
2*n*x5^3*x6^2*x7^2*x8^3-n^2*x5^3*x6^2*x7^2*x8^3+
54*x4^3*x5^2*x7*x8^4+27*n*x4^3*x5^2*x7*x8^4+
3*n^2*x4^3*x5^2*x7*x8^4+21*x4*x5^3*x6*x7*x8^4+
13*n*x4*x5^3*x6*x7*x8^4+2*n^2*x4*x5^3*x6*x7*x8^4-12*x4^2*x5^3*x8^5-
7*n*x4^2*x5^3*x8^5-n^2*x4^2*x5^3*x8^5-3*x5^4*x6*x8^5-
n*x5^4*x6*x8^5+12*x4^5*x6*x7^3*x9+7*n*x4^5*x6*x7^3*x9+
6*x4^3*x5*x6^2*x7^3*x9-4*n*x4^3*x5*x6^2*x7^3*x9-
18*x4*x5^2*x6^3*x7^3*x9-3*n*x4*x5^2*x6^3*x7^3*x9-
24*x4^6*x7^2*x8*x9-6*n*x4^6*x7^2*x8*x9-36*x4^4*x5*x6*x7^2*x8*x9-
15*n*x4^4*x5*x6*x7^2*x8*x9+54*x4^2*x5^2*x6^2*x7^2*x8*x9+
18*n*x4^2*x5^2*x6^2*x7^2*x8*x9+6*x5^3*x6^3*x7^2*x8*x9+
3*n*x5^3*x6^3*x7^2*x8*x9+60*x4^5*x5*x7*x8^2*x9+
15*n*x4^5*x5*x7*x8^2*x9-30*x4^3*x5^2*x6*x7*x8^2*x9-
30*x4*x5^3*x6^2*x7*x8^2*x9-15*n*x4*x5^3*x6^2*x7*x8^2*x9-
24*x4^4*x5^2*x8^3*x9-9*n*x4^4*x5^2*x8^3*x9+18*x4^2*x5^3*x6*x8^3*x9+
8*n*x4^2*x5^3*x6*x8^3*x9+6*x5^4*x6^2*x8^3*x9+n*x5^4*x6^2*x8^3*x9+
6*x4^7*x7*x9^2-3*x4^5*x5*x6*x7*x9^2-12*x4^3*x5^2*x6^2*x7*x9^2+
9*x4*x5^3*x6^3*x7*x9^2-12*x4^6*x5*x8*x9^2+21*x4^4*x5^2*x6*x8*x9^2-
6*x4^2*x5^3*x6^2*x8*x9^2-3*x5^4*x6^3*x8*x9^2;
x38=3*x6^2*x7^2+n*x6^2*x7^2-6*x4*x6*x7*x8-2*n*x4*x6*x7*x8+
n*x4^2*x8^2+3*x5*x6*x8^2+3*x4^2*x6*x9-3*x5*x6^2*x9;
x39=12*x4^2*x6^3*x7^5+7*n*x4^2*x6^3*x7^5+n^2*x4^2*x6^3*x7^5+
3*x5*x6^4*x7^5+n*x5*x6^4*x7^5-54*x4^3*x6^2*x7^4*x8-
27*n*x4^3*x6^2*x7^4*x8-3*n^2*x4^3*x6^2*x7^4*x8-
21*x4*x5*x6^3*x7^4*x8-13*n*x4*x5*x6^3*x7^4*x8-
2*n^2*x4*x5*x6^3*x7^4*x8+72*x4^4*x6*x7^3*x8^2+
30*n*x4^4*x6*x7^3*x8^2+3*n^2*x4^4*x6*x7^3*x8^2+
72*x4^2*x5*x6^2*x7^3*x8^2+48*n*x4^2*x5*x6^2*x7^3*x8^2+
6*n^2*x4^2*x5*x6^2*x7^3*x8^2+6*x5^2*x6^3*x7^3*x8^2+
2*n*x5^2*x6^3*x7^3*x8^2+n^2*x5^2*x6^3*x7^3*x8^2-24*x4^5*x7^2*x8^3-
10*n*x4^5*x7^2*x8^3-n^2*x4^5*x7^2*x8^3-96*x4^3*x5*x6*x7^2*x8^3-
52*n*x4^3*x5*x6*x7^2*x8^3-6*n^2*x4^3*x5*x6*x7^2*x8^3-
30*x4*x5^2*x6^2*x7^2*x8^3-18*n*x4*x5^2*x6^2*x7^2*x8^3-
3*n^2*x4*x5^2*x6^2*x7^2*x8^3+24*x4^4*x5*x7*x8^4+
16*n*x4^4*x5*x7*x8^4+2*n^2*x4^4*x5*x7*x8^4+48*x4^2*x5^2*x6*x7*x8^4+
21*n*x4^2*x5^2*x6*x7*x8^4+3*n^2*x4^2*x5^2*x6*x7*x8^4+
3*x5^3*x6^2*x7*x8^4+3*n*x5^3*x6^2*x7*x8^4-6*x4^3*x5^2*x8^5-
5*n*x4^3*x5^2*x8^5-n^2*x4^3*x5^2*x8^5-9*x4*x5^3*x6*x8^5-
3*n*x4*x5^3*x6*x8^5+24*x4^4*x6^2*x7^3*x9+9*n*x4^4*x6^2*x7^3*x9-
18*x4^2*x5*x6^3*x7^3*x9-8*n*x4^2*x5*x6^3*x7^3*x9-
6*x5^2*x6^4*x7^3*x9-n*x5^2*x6^4*x7^3*x9-60*x4^5*x6*x7^2*x8*x9-
15*n*x4^5*x6*x7^2*x8*x9+30*x4^3*x5*x6^2*x7^2*x8*x9+
30*x4*x5^2*x6^3*x7^2*x8*x9+15*n*x4*x5^2*x6^3*x7^2*x8*x9+
24*x4^6*x7*x8^2*x9+6*n*x4^6*x7*x8^2*x9+36*x4^4*x5*x6*x7*x8^2*x9+
15*n*x4^4*x5*x6*x7*x8^2*x9-54*x4^2*x5^2*x6^2*x7*x8^2*x9-
18*n*x4^2*x5^2*x6^2*x7*x8^2*x9-6*x5^3*x6^3*x7*x8^2*x9-
3*n*x5^3*x6^3*x7*x8^2*x9-12*x4^5*x5*x8^3*x9-7*n*x4^5*x5*x8^3*x9-
6*x4^3*x5^2*x6*x8^3*x9+4*n*x4^3*x5^2*x6*x8^3*x9+
18*x4*x5^3*x6^2*x8^3*x9+3*n*x4*x5^3*x6^2*x8^3*x9+
12*x4^6*x6*x7*x9^2-21*x4^4*x5*x6^2*x7*x9^2+
6*x4^2*x5^2*x6^3*x7*x9^2+3*x5^3*x6^4*x7*x9^2-6*x4^7*x8*x9^2+
3*x4^5*x5*x6*x8*x9^2+12*x4^3*x5^2*x6^2*x8*x9^2-9*x4*x5^3*x6^3*x8*x9^2\
;x40=15*x4*x6^4*x7^5+8*n*x4*x6^4*x7^5+n^2*x4*x6^4*x7^5-
72*x4^2*x6^3*x7^4*x8-36*n*x4^2*x6^3*x7^4*x8-
4*n^2*x4^2*x6^3*x7^4*x8-3*x5*x6^4*x7^4*x8-4*n*x5*x6^4*x7^4*x8-
n^2*x5*x6^4*x7^4*x8+108*x4^3*x6^2*x7^3*x8^2+
54*n*x4^3*x6^2*x7^3*x8^2+6*n^2*x4^3*x6^2*x7^3*x8^2+
42*x4*x5*x6^3*x7^3*x8^2+26*n*x4*x5*x6^3*x7^3*x8^2+
4*n^2*x4*x5*x6^3*x7^3*x8^2-48*x4^4*x6*x7^2*x8^3-
32*n*x4^4*x6*x7^2*x8^3-4*n^2*x4^4*x6*x7^2*x8^3-
96*x4^2*x5*x6^2*x7^2*x8^3-42*n*x4^2*x5*x6^2*x7^2*x8^3-
6*n^2*x4^2*x5*x6^2*x7^2*x8^3-6*x5^2*x6^3*x7^2*x8^3-
6*n*x5^2*x6^3*x7^2*x8^3+6*n*x4^5*x7*x8^4+n^2*x4^5*x7*x8^4+
48*x4^3*x5*x6*x7*x8^4+22*n*x4^3*x5*x6*x7*x8^4+
4*n^2*x4^3*x5*x6*x7*x8^4+27*x4*x5^2*x6^2*x7*x8^4+
12*n*x4*x5^2*x6^2*x7*x8^4-2*n*x4^4*x5*x8^5-n^2*x4^4*x5*x8^5-
12*x4^2*x5^2*x6*x8^5-6*n*x4^2*x5^2*x6*x8^5-3*x5^3*x6^2*x8^5+
30*x4^3*x6^3*x7^3*x9+10*n*x4^3*x6^3*x7^3*x9-30*x4*x5*x6^4*x7^3*x9-
10*n*x4*x5*x6^4*x7^3*x9-84*x4^4*x6^2*x7^2*x8*x9-
24*n*x4^4*x6^2*x7^2*x8*x9+78*x4^2*x5*x6^3*x7^2*x8*x9+
18*n*x4^2*x5*x6^3*x7^2*x8*x9+6*x5^2*x6^4*x7^2*x8*x9+
6*n*x5^2*x6^4*x7^2*x8*x9+48*x4^5*x6*x7*x8^2*x9+
18*n*x4^5*x6*x7*x8^2*x9-6*x4^3*x5*x6^2*x7*x8^2*x9-
6*n*x4^3*x5*x6^2*x7*x8^2*x9-42*x4*x5^2*x6^3*x7*x8^2*x9-
12*n*x4*x5^2*x6^3*x7*x8^2*x9-4*n*x4^6*x8^3*x9-
24*x4^4*x5*x6*x8^3*x9-2*n*x4^4*x5*x6*x8^3*x9+
18*x4^2*x5^2*x6^2*x8^3*x9+6*n*x4^2*x5^2*x6^2*x8^3*x9+
6*x5^3*x6^3*x8^3*x9+15*x4^5*x6^2*x7*x9^2-30*x4^3*x5*x6^3*x7*x9^2+
15*x4*x5^2*x6^4*x7*x9^2-12*x4^6*x6*x8*x9^2+21*x4^4*x5*x6^2*x8*x9^2-
6*x4^2*x5^2*x6^3*x8*x9^2-3*x5^3*x6^4*x8*x9^2;
x41=15*x6^4*x7^4+8*n*x6^4*x7^4+n^2*x6^4*x7^4-60*x4*x6^3*x7^3*x8-
32*n*x4*x6^3*x7^3*x8-4*n^2*x4*x6^3*x7^3*x8+60*x4^2*x6^2*x7^2*x8^2+
38*n*x4^2*x6^2*x7^2*x8^2+6*n^2*x4^2*x6^2*x7^2*x8^2+
30*x5*x6^3*x7^2*x8^2+10*n*x5*x6^3*x7^2*x8^2-12*n*x4^3*x6*x7*x8^3-
4*n^2*x4^3*x6*x7*x8^3-60*x4*x5*x6^2*x7*x8^3-
20*n*x4*x5*x6^2*x7*x8^3-2*n*x4^4*x8^4+n^2*x4^4*x8^4+
10*n*x4^2*x5*x6*x8^4+15*x5^2*x6^2*x8^4+30*x4^2*x6^3*x7^2*x9+
10*n*x4^2*x6^3*x7^2*x9-30*x5*x6^4*x7^2*x9-10*n*x5*x6^4*x7^2*x9-
60*x4^3*x6^2*x7*x8*x9-20*n*x4^3*x6^2*x7*x8*x9+
60*x4*x5*x6^3*x7*x8*x9+20*n*x4*x5*x6^3*x7*x8*x9+
10*n*x4^4*x6*x8^2*x9+30*x4^2*x5*x6^2*x8^2*x9-
10*n*x4^2*x5*x6^2*x8^2*x9-30*x5^2*x6^3*x8^2*x9+15*x4^4*x6^2*x9^2-
30*x4^2*x5*x6^3*x9^2+15*x5^2*x6^4*x9^2;
x42=-((x1*x12*x13*x29*x30^2)/(n*x27*x28^3))-
(x11*x13*x14*x29*x30^2)/(n*x27*x28^3)-
(x10*x12*x15*x29*x30^2)/(n*x27*x28^3)-
(x1*x16*x17*x29*x30^2)/(n*x27*x28^3)-
(x11*x17*x18*x29*x30^2)/(n*x27*x28^3)-
(x15*x17*x19*x29*x30^2)/(n*x27*x28^3)-
(x14*x15*x2*x29*x30^2)/(n*x27*x28^3)-
(x10*x16*x20*x29*x30^2)/(n*x27*x28^3)-
(x13*x19*x20*x29*x30^2)/(n*x27*x28^3)-
(x18*x2*x20*x29*x30^2)/(n*x27*x28^3)-
(x14*x16*x21*x29*x30^2)/(n*x27*x28^3)-
(x12*x18*x21*x29*x30^2)/(n*x27*x28^3)-
(x10*x11*x29*x3*x30^2)/(n*x27*x28^3)-
(x1*x2*x29*x3*x30^2)/(n*x27*x28^3)-
(x19*x21*x29*x3*x30^2)/(n*x27*x28^3)-
(x1*x11*x15*x17*x29*x30*x31)/(n*x27*x28^4)-
(x1*x11*x13*x20*x29*x30*x31)/(n*x27*x28^4)-
(x10*x11*x15*x20*x29*x30*x31)/(n*x27*x28^4)-
(x1*x15*x2*x20*x29*x30*x31)/(n*x27*x28^4)-
(x1*x12*x15*x21*x29*x30*x31)/(n*x27*x28^4)-
(x11*x14*x15*x21*x29*x30*x31)/(n*x27*x28^4)-
(x1*x16*x20*x21*x29*x30*x31)/(n*x27*x28^4)-
(x11*x18*x20*x21*x29*x30*x31)/(n*x27*x28^4)-
(x15*x19*x20*x21*x29*x30*x31)/(n*x27*x28^4)-
(x1*x11*x21*x29*x3*x30*x31)/(n*x27*x28^4)+
(x1*x11*x15*x20*x21*x29*x32)/(n*x27*x28^5)+
(x12*x13*x22*x30^2*x33)/(n*x27*x28^3)+
(x16*x17*x22*x30^2*x33)/(n*x27*x28^3)+
(x13*x14*x23*x30^2*x33)/(n*x27*x28^3)+
(x17*x18*x23*x30^2*x33)/(n*x27*x28^3)+
(x10*x12*x24*x30^2*x33)/(n*x27*x28^3)+
(x17*x19*x24*x30^2*x33)/(n*x27*x28^3)+
(x14*x2*x24*x30^2*x33)/(n*x27*x28^3)+
(x10*x16*x25*x30^2*x33)/(n*x27*x28^3)+
(x13*x19*x25*x30^2*x33)/(n*x27*x28^3)+
(x18*x2*x25*x30^2*x33)/(n*x27*x28^3)+
(x14*x16*x26*x30^2*x33)/(n*x27*x28^3)+
(x12*x18*x26*x30^2*x33)/(n*x27*x28^3)+
(x2*x22*x3*x30^2*x33)/(n*x27*x28^3)+
(x10*x23*x3*x30^2*x33)/(n*x27*x28^3)+
(x19*x26*x3*x30^2*x33)/(n*x27*x28^3)+
(x11*x15*x17*x22*x30*x34)/(n*x27*x28^4)+
(x11*x13*x20*x22*x30*x34)/(n*x27*x28^4)+
(x15*x2*x20*x22*x30*x34)/(n*x27*x28^4)+
(x12*x15*x21*x22*x30*x34)/(n*x27*x28^4)+
(x16*x20*x21*x22*x30*x34)/(n*x27*x28^4)+
(x1*x15*x17*x23*x30*x34)/(n*x27*x28^4)+
(x1*x13*x20*x23*x30*x34)/(n*x27*x28^4)+
(x10*x15*x20*x23*x30*x34)/(n*x27*x28^4)+
(x14*x15*x21*x23*x30*x34)/(n*x27*x28^4)+
(x18*x20*x21*x23*x30*x34)/(n*x27*x28^4)+
(x1*x11*x17*x24*x30*x34)/(n*x27*x28^4)+
(x10*x11*x20*x24*x30*x34)/(n*x27*x28^4)+
(x1*x2*x20*x24*x30*x34)/(n*x27*x28^4)+
(x1*x12*x21*x24*x30*x34)/(n*x27*x28^4)+
(x11*x14*x21*x24*x30*x34)/(n*x27*x28^4)+
(x19*x20*x21*x24*x30*x34)/(n*x27*x28^4)+
(x1*x11*x13*x25*x30*x34)/(n*x27*x28^4)+
(x10*x11*x15*x25*x30*x34)/(n*x27*x28^4)+
(x1*x15*x2*x25*x30*x34)/(n*x27*x28^4)+
(x1*x16*x21*x25*x30*x34)/(n*x27*x28^4)+
(x11*x18*x21*x25*x30*x34)/(n*x27*x28^4)+
(x15*x19*x21*x25*x30*x34)/(n*x27*x28^4)+
(x1*x12*x15*x26*x30*x34)/(n*x27*x28^4)+
(x11*x14*x15*x26*x30*x34)/(n*x27*x28^4)+
(x1*x16*x20*x26*x30*x34)/(n*x27*x28^4)+
(x11*x18*x20*x26*x30*x34)/(n*x27*x28^4)+
(x15*x19*x20*x26*x30*x34)/(n*x27*x28^4)+
(x11*x21*x22*x3*x30*x34)/(n*x27*x28^4)+
(x1*x21*x23*x3*x30*x34)/(n*x27*x28^4)+
(x1*x11*x26*x3*x30*x34)/(n*x27*x28^4)+
(x11*x15*x20*x21*x22*x35)/(n*x27*x28^5)+
(x1*x15*x20*x21*x23*x35)/(n*x27*x28^5)+
(x1*x11*x20*x21*x24*x35)/(n*x27*x28^5)+
(x1*x11*x15*x21*x25*x35)/(n*x27*x28^5)+
(x1*x11*x15*x20*x26*x35)/(n*x27*x28^5)-
(x15*x17*x22*x23*x30*x36)/(n*x27*x28^4)-
(x13*x20*x22*x23*x30*x36)/(n*x27*x28^4)-
(x11*x17*x22*x24*x30*x36)/(n*x27*x28^4)-
(x2*x20*x22*x24*x30*x36)/(n*x27*x28^4)-
(x12*x21*x22*x24*x30*x36)/(n*x27*x28^4)-
(x1*x17*x23*x24*x30*x36)/(n*x27*x28^4)-
(x10*x20*x23*x24*x30*x36)/(n*x27*x28^4)-
(x14*x21*x23*x24*x30*x36)/(n*x27*x28^4)-
(x11*x13*x22*x25*x30*x36)/(n*x27*x28^4)-
(x15*x2*x22*x25*x30*x36)/(n*x27*x28^4)-
(x16*x21*x22*x25*x30*x36)/(n*x27*x28^4)-
(x1*x13*x23*x25*x30*x36)/(n*x27*x28^4)-
(x10*x15*x23*x25*x30*x36)/(n*x27*x28^4)-
(x18*x21*x23*x25*x30*x36)/(n*x27*x28^4)-
(x10*x11*x24*x25*x30*x36)/(n*x27*x28^4)-
(x1*x2*x24*x25*x30*x36)/(n*x27*x28^4)-
(x19*x21*x24*x25*x30*x36)/(n*x27*x28^4)-
(x12*x15*x22*x26*x30*x36)/(n*x27*x28^4)-
(x16*x20*x22*x26*x30*x36)/(n*x27*x28^4)-
(x14*x15*x23*x26*x30*x36)/(n*x27*x28^4)-
(x18*x20*x23*x26*x30*x36)/(n*x27*x28^4)-
(x1*x12*x24*x26*x30*x36)/(n*x27*x28^4)-
(x11*x14*x24*x26*x30*x36)/(n*x27*x28^4)-
(x19*x20*x24*x26*x30*x36)/(n*x27*x28^4)-
(x1*x16*x25*x26*x30*x36)/(n*x27*x28^4)-
(x11*x18*x25*x26*x30*x36)/(n*x27*x28^4)-
(x15*x19*x25*x26*x30*x36)/(n*x27*x28^4)-
(x21*x22*x23*x3*x30*x36)/(n*x27*x28^4)-
(x11*x22*x26*x3*x30*x36)/(n*x27*x28^4)-
(x1*x23*x26*x3*x30*x36)/(n*x27*x28^4)-
(x15*x20*x21*x22*x23*x37)/(n*x27*x28^5)-
(x11*x20*x21*x22*x24*x37)/(n*x27*x28^5)-
(x1*x20*x21*x23*x24*x37)/(n*x27*x28^5)-
(x11*x15*x21*x22*x25*x37)/(n*x27*x28^5)-
(x1*x15*x21*x23*x25*x37)/(n*x27*x28^5)-
(x1*x11*x21*x24*x25*x37)/(n*x27*x28^5)-
(x11*x15*x20*x22*x26*x37)/(n*x27*x28^5)-
(x1*x15*x20*x23*x26*x37)/(n*x27*x28^5)-
(x1*x11*x20*x24*x26*x37)/(n*x27*x28^5)-
(x1*x11*x15*x25*x26*x37)/(n*x27*x28^5)+
(x17*x22*x23*x24*x30*x33*x38)/(n*x27*x28^4)+
(x13*x22*x23*x25*x30*x33*x38)/(n*x27*x28^4)+
(x2*x22*x24*x25*x30*x33*x38)/(n*x27*x28^4)+
(x10*x23*x24*x25*x30*x33*x38)/(n*x27*x28^4)+
(x12*x22*x24*x26*x30*x33*x38)/(n*x27*x28^4)+
(x14*x23*x24*x26*x30*x33*x38)/(n*x27*x28^4)+
(x16*x22*x25*x26*x30*x33*x38)/(n*x27*x28^4)+
(x18*x23*x25*x26*x30*x33*x38)/(n*x27*x28^4)+
(x19*x24*x25*x26*x30*x33*x38)/(n*x27*x28^4)+
(x22*x23*x26*x3*x30*x33*x38)/(n*x27*x28^4)+
(x20*x21*x22*x23*x24*x39)/(n*x27*x28^5)+
(x15*x21*x22*x23*x25*x39)/(n*x27*x28^5)+
(x11*x21*x22*x24*x25*x39)/(n*x27*x28^5)+
(x1*x21*x23*x24*x25*x39)/(n*x27*x28^5)+
(x15*x20*x22*x23*x26*x39)/(n*x27*x28^5)+
(x11*x20*x22*x24*x26*x39)/(n*x27*x28^5)+
(x1*x20*x23*x24*x26*x39)/(n*x27*x28^5)+
(x11*x15*x22*x25*x26*x39)/(n*x27*x28^5)+
(x1*x15*x23*x25*x26*x39)/(n*x27*x28^5)+
(x1*x11*x24*x25*x26*x39)/(n*x27*x28^5)-
(x21*x22*x23*x24*x25*x40)/(n*x27*x28^5)-
(x20*x22*x23*x24*x26*x40)/(n*x27*x28^5)-
(x15*x22*x23*x25*x26*x40)/(n*x27*x28^5)-
(x11*x22*x24*x25*x26*x40)/(n*x27*x28^5)-
(x1*x23*x24*x25*x26*x40)/(n*x27*x28^5)+
(x22*x23*x24*x25*x26*x33*x41)/(n*x27*x28^5)
]/; Expand[ExpandScalarProduct[
    (Pair[Momentum[p1, n], Momentum[p2, n]]^2 -
      Pair[Momentum[p1, n], Momentum[p1, n]]*
       Pair[Momentum[p2, n], Momentum[p2, n]])
           ] ] =!= 0
,
(* Dmu *)
tidl[{{q_,mu_}},{p1_,p2_,p3_},n_] :>
 Block[{x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,
x17,x18,x19,x20,x21,x22},
x1 = ExpandScalarProduct[LorentzIndex[mu, n], Momentum[p1, n]];
x2 = ExpandScalarProduct[LorentzIndex[mu, n], Momentum[p2, n]];
x3 = ExpandScalarProduct[LorentzIndex[mu, n], Momentum[p3, n]];
x4 = ExpandScalarProduct[Momentum[p1, n], Momentum[p1, n]];
x5 = ExpandScalarProduct[Momentum[p1, n], Momentum[p2, n]];
x6 = ExpandScalarProduct[Momentum[p1, n], Momentum[p3, n]];
x7 = ExpandScalarProduct[Momentum[p1, n], Momentum[q, n]];
x8 = ExpandScalarProduct[Momentum[p2, n], Momentum[p2, n]];
x9 = ExpandScalarProduct[Momentum[p2, n], Momentum[p3, n]];
x10 = ExpandScalarProduct[Momentum[p2, n], Momentum[q, n]];
x11 = ExpandScalarProduct[Momentum[p3, n], Momentum[p3, n]];
x12 = ExpandScalarProduct[Momentum[p3, n], Momentum[q, n]];
x13 = Expand[x5^2 - x4*x8];
x14 = Expand[x5*x6 - x4*x9];
x15 = Expand[-(x11*x4) + x6^2];
x16 = Expand[x6*x8 - x5*x9];
x17 = Expand[-(x11*x5) + x6*x9];
x18 = Expand[-(x11*x8) + x9^2];
x19 = Expand[-(x10*(-(x11*x4) + x6^2)) + x12*(x5*x6 - x4*x9) + 
x7*(-(x11*x5) + x6*x9)];
x20 = Expand[x12*(x5^2 - x4*x8) - x10*(x5*x6 - x4*x9) + x7*(x6*x8 - x5*x9)];
x21 = Expand[x12*(x6*x8 - x5*x9) - x10*(-(x11*x5) + x6*x9) + 
x7*(-(x11*x8) + x9^2)];
x22 = Factor[x11*x5^2 - x11*x4*x8 + x6^2*x8 - 2*x5*x6*x9 + x4*x9^2];
-((x19*x2)/x22) + (x1*x21)/x22 + (x20*x3)/x22
]
,
(* Dmunu *)
tidl[{{q_,mu_}, {q_,nu_}},{p1_,p2_,p3_},n_] :>
Block[{x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,
x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27,x28,x29,
x30,x31,x32,x33,x34,x35,x36,x37,x38,x39,x40,x41,x42,x43,x44,x45},
x1=ExpandScalarProduct[LorentzIndex[mu,n],LorentzIndex[nu,n]];
x2=ExpandScalarProduct[LorentzIndex[mu,n],Momentum[p1,n]];
x3=ExpandScalarProduct[LorentzIndex[mu,n],Momentum[p2,n]];
x4=ExpandScalarProduct[LorentzIndex[mu,n],Momentum[p3,n]];
x5=ExpandScalarProduct[LorentzIndex[nu,n],Momentum[p1,n]];
x6=ExpandScalarProduct[LorentzIndex[nu,n],Momentum[p2,n]];
x7=ExpandScalarProduct[LorentzIndex[nu,n],Momentum[p3,n]];
x8=ExpandScalarProduct[Momentum[p1,n],Momentum[p1,n]];
x9=ExpandScalarProduct[Momentum[p1,n],Momentum[p2,n]];
x10=ExpandScalarProduct[Momentum[p1,n],Momentum[p3,n]];
x11=ExpandScalarProduct[Momentum[p1,n],Momentum[q,n]];
x12=ExpandScalarProduct[Momentum[p2,n],Momentum[p2,n]];
x13=ExpandScalarProduct[Momentum[p2,n],Momentum[p3,n]];
x14=ExpandScalarProduct[Momentum[p2,n],Momentum[q,n]];
x15=ExpandScalarProduct[Momentum[p3,n],Momentum[p3,n]];
x16=ExpandScalarProduct[Momentum[p3,n],Momentum[q,n]];
x17=ExpandScalarProduct[Momentum[q,n],Momentum[q,n]];
x18=Expand[2-n];
x19=Expand[3-n];
x20=Expand[x10^2-x15*x8];
x21=Expand[x10*x12-x13*x9];
x22=Expand[x10*x13-x15*x9];
x23=Expand[x13^2-x12*x15];
x24=Expand[-(x13*x8)+x10*x9];
x25=Expand[-(x12*x8)+x9^2];
x26=Factor[x10^2*x12+x13^2*x8-x12*x15*x8-2*x10*x13*x9+x15*x9^2];
x27=Expand[
x11^2*(x13^2-x12*x15)+x14^2*(x10^2-x15*x8)-
2*x14*x16*(-(x13*x8)+x10*x9)+2*x11*x16*(x10*x12-x13*x9)-
2*x11*x14*(x10*x13-x15*x9)+x16^2*(-(x12*x8)+x9^2)-
x17*(x10^2*x12+x13^2*x8-x12*x15*x8-2*x10*x13*x9+x15*x9^2)
];
x28=Expand[
(2-n)*x14^2*(x10^2-x15*x8)^2-
2*(2-n)*x14*x16*(x10^2-x15*x8)*(-(x13*x8)+x10*x9)-
2*(2-n)*x11*x14*(x10^2-x15*x8)*(x10*x13-x15*x9)+
x17*(x10^2-x15*x8)*(x10^2*x12+x13^2*x8-x12*x15*x8-2*x10*x13*x9+
x15*x9^2)-2*x11*x16*(x10^3*x12+3*x10*x13^2*x8-n*x10*x13^2*x8-
x10*x12*x15*x8-4*x10^2*x13*x9+n*x10^2*x13*x9-2*x13*x15*x8*x9+
n*x13*x15*x8*x9+3*x10*x15*x9^2-n*x10*x15*x9^2)+
x11^2*(2*x10^2*x13^2-n*x10^2*x13^2+x10^2*x12*x15+x13^2*x15*x8-
x12*x15^2*x8-6*x10*x13*x15*x9+2*n*x10*x13*x15*x9+3*x15^2*x9^2-
n*x15^2*x9^2)+x16^2*(x10^2*x12*x8+3*x13^2*x8^2-n*x13^2*x8^2-
x12*x15*x8^2-6*x10*x13*x8*x9+2*n*x10*x13*x8*x9+2*x10^2*x9^2-
n*x10^2*x9^2+x15*x8*x9^2)
];
x29=Expand[
(2-n)*x11^2*(x13^2-x12*x15)*(x10*x13-x15*x9)+
(2-n)*x14^2*(x10^2-x15*x8)*(x10*x13-x15*x9)+
x17*(x10*x13-x15*x9)*(x10^2*x12+x13^2*x8-x12*x15*x8-2*x10*x13*x9+
x15*x9^2)-x14*x16*(3*x10^3*x12-n*x10^3*x12-x10*x13^2*x8+
n*x10*x13^2*x8-3*x10*x12*x15*x8+n*x10*x12*x15*x8-2*x10^2*x13*x9+
4*x13*x15*x8*x9-2*n*x13*x15*x8*x9-x10*x15*x9^2+n*x10*x15*x9^2)+
x11*x16*(x10^2*x12*x13-n*x10^2*x12*x13-3*x13^3*x8+n*x13^3*x8+
3*x12*x13*x15*x8-n*x12*x13*x15*x8+2*x10*x13^2*x9-
4*x10*x12*x15*x9+2*n*x10*x12*x15*x9+x13*x15*x9^2-n*x13*x15*x9^2)-
x11*x14*(4*x10^2*x13^2-2*n*x10^2*x13^2-3*x10^2*x12*x15+
n*x10^2*x12*x15-3*x13^2*x15*x8+n*x13^2*x15*x8+3*x12*x15^2*x8-
n*x12*x15^2*x8-2*x10*x13*x15*x9+2*n*x10*x13*x15*x9+x15^2*x9^2-
n*x15^2*x9^2)+x16^2*(-2*x10*x12*x13*x8+n*x10*x12*x13*x8+
3*x10^2*x12*x9-n*x10^2*x12*x9+3*x13^2*x8*x9-n*x13^2*x8*x9-
x12*x15*x8*x9-4*x10*x13*x9^2+n*x10*x13*x9^2+x15*x9^3)
];
x30=Expand[
(2-n)*x11^2*(x13^2-x12*x15)*(x10*x12-x13*x9)+
(2-n)*x16^2*(x10*x12-x13*x9)*(-(x12*x8)+x9^2)+
x17*(x10*x12-x13*x9)*(x10^2*x12+x13^2*x8-x12*x15*x8-2*x10*x13*x9+
x15*x9^2)-x14^2*(x10^3*x12+3*x10*x13^2*x8-n*x10*x13^2*x8-
x10*x12*x15*x8-4*x10^2*x13*x9+n*x10^2*x13*x9-2*x13*x15*x8*x9+
n*x13*x15*x8*x9+3*x10*x15*x9^2-n*x10*x15*x9^2)+
x11*x16*(x10^2*x12^2-n*x10^2*x12^2-3*x12*x13^2*x8+n*x12*x13^2*x8+
3*x12^2*x15*x8-n*x12^2*x15*x8-2*x10*x12*x13*x9+
2*n*x10*x12*x13*x9+4*x13^2*x9^2-2*n*x13^2*x9^2-3*x12*x15*x9^2+
n*x12*x15*x9^2)-x11*x14*
(x10^2*x12*x13-n*x10^2*x12*x13-3*x13^3*x8+n*x13^3*x8+
3*x12*x13*x15*x8-n*x12*x13*x15*x8+2*x10*x13^2*x9-
4*x10*x12*x15*x9+2*n*x10*x12*x15*x9+x13*x15*x9^2-n*x13*x15*x9^2)-
x14*x16*(-4*x10*x12*x13*x8+2*n*x10*x12*x13*x8+x10^2*x12*x9-
n*x10^2*x12*x9+x13^2*x8*x9-n*x13^2*x8*x9+3*x12*x15*x8*x9-
n*x12*x15*x8*x9+2*x10*x13*x9^2-3*x15*x9^3+n*x15*x9^3)
];
x31=Expand[
(2-n)*x11^2*(x13^2-x12*x15)^2+
2*(2-n)*x11*(x13^2-x12*x15)*x16*(x10*x12-x13*x9)-
2*(2-n)*x11*x14*(x13^2-x12*x15)*(x10*x13-x15*x9)+
(x13^2-x12*x15)*x17*(x10^2*x12+x13^2*x8-x12*x15*x8-2*x10*x13*x9+
x15*x9^2)+x16^2*(3*x10^2*x12^2-n*x10^2*x12^2+x12*x13^2*x8-
x12^2*x15*x8-6*x10*x12*x13*x9+2*n*x10*x12*x13*x9+2*x13^2*x9^2-
n*x13^2*x9^2+x12*x15*x9^2)-
2*x14*x16*(3*x10^2*x12*x13-n*x10^2*x12*x13+x13^3*x8-x12*x13*x15*x8-
4*x10*x13^2*x9+n*x10*x13^2*x9-2*x10*x12*x15*x9+n*x10*x12*x15*x9+
3*x13*x15*x9^2-n*x13*x15*x9^2)+
x14^2*(2*x10^2*x13^2-n*x10^2*x13^2+x10^2*x12*x15+x13^2*x15*x8-
x12*x15^2*x8-6*x10*x13*x15*x9+2*n*x10*x13*x15*x9+3*x15^2*x9^2-
n*x15^2*x9^2)
];
x32=Expand[
(2-n)*x14^2*(x10^2-x15*x8)*(-(x13*x8)+x10*x9)+
(2-n)*x16^2*(-(x13*x8)+x10*x9)*(-(x12*x8)+x9^2)+
x17*(-(x13*x8)+x10*x9)*(x10^2*x12+x13^2*x8-x12*x15*x8-
2*x10*x13*x9+x15*x9^2)-
x11*x14*(3*x10^3*x12-n*x10^3*x12-x10*x13^2*x8+n*x10*x13^2*x8-
3*x10*x12*x15*x8+n*x10*x12*x15*x8-2*x10^2*x13*x9+
4*x13*x15*x8*x9-2*n*x13*x15*x8*x9-x10*x15*x9^2+n*x10*x15*x9^2)+
x11^2*(3*x10^2*x12*x13-n*x10^2*x12*x13+x13^3*x8-x12*x13*x15*x8-
4*x10*x13^2*x9+n*x10*x13^2*x9-2*x10*x12*x15*x9+n*x10*x12*x15*x9+
3*x13*x15*x9^2-n*x13*x15*x9^2)-
x14*x16*(-3*x10^2*x12*x8+n*x10^2*x12*x8+x13^2*x8^2-n*x13^2*x8^2+
3*x12*x15*x8^2-n*x12*x15*x8^2-2*x10*x13*x8*x9+2*n*x10*x13*x8*x9+
4*x10^2*x9^2-2*n*x10^2*x9^2-3*x15*x8*x9^2+n*x15*x8*x9^2)+
x11*x16*(-4*x10*x12*x13*x8+2*n*x10*x12*x13*x8+x10^2*x12*x9-
n*x10^2*x12*x9+x13^2*x8*x9-n*x13^2*x8*x9+3*x12*x15*x8*x9-
n*x12*x15*x8*x9+2*x10*x13*x9^2-3*x15*x9^3+n*x15*x9^3)
];
x33=Expand[
-2*(2-n)*x14*x16*(-(x13*x8)+x10*x9)*(-(x12*x8)+x9^2)+
2*(2-n)*x11*x16*(x10*x12-x13*x9)*(-(x12*x8)+x9^2)+
(2-n)*x16^2*(-(x12*x8)+x9^2)^2+
x17*(-(x12*x8)+x9^2)*(x10^2*x12+x13^2*x8-x12*x15*x8-2*x10*x13*x9+
x15*x9^2)+x11^2*(3*x10^2*x12^2-n*x10^2*x12^2+x12*x13^2*x8-
x12^2*x15*x8-6*x10*x12*x13*x9+2*n*x10*x12*x13*x9+2*x13^2*x9^2-
n*x13^2*x9^2+x12*x15*x9^2)+
x14^2*(x10^2*x12*x8+3*x13^2*x8^2-n*x13^2*x8^2-x12*x15*x8^2-
6*x10*x13*x8*x9+2*n*x10*x13*x8*x9+2*x10^2*x9^2-n*x10^2*x9^2+
x15*x8*x9^2)-2*x11*x14*(-2*x10*x12*x13*x8+n*x10*x12*x13*x8+
3*x10^2*x12*x9-n*x10^2*x12*x9+3*x13^2*x8*x9-n*x13^2*x8*x9-
x12*x15*x8*x9-4*x10*x13*x9^2+n*x10*x13*x9^2+x15*x9^3)
];
x34=Expand[
3*x10^2*x12^2-n*x10^2*x12^2+x12*x13^2*x8-x12^2*x15*x8-
6*x10*x12*x13*x9+2*n*x10*x12*x13*x9+2*x13^2*x9^2-n*x13^2*x9^2+
x12*x15*x9^2
];
x35=Expand[
2*x10^2*x13^2-n*x10^2*x13^2+x10^2*x12*x15+x13^2*x15*x8-x12*x15^2*x8-
6*x10*x13*x15*x9+2*n*x10*x13*x15*x9+3*x15^2*x9^2-n*x15^2*x9^2
];
x36=Expand[
x10^2*x12*x8+3*x13^2*x8^2-n*x13^2*x8^2-x12*x15*x8^2-6*x10*x13*x8*x9+
2*n*x10*x13*x8*x9+2*x10^2*x9^2-n*x10^2*x9^2+x15*x8*x9^2
];
x37=Expand[
x10^3*x12+3*x10*x13^2*x8-n*x10*x13^2*x8-x10*x12*x15*x8-
4*x10^2*x13*x9+n*x10^2*x13*x9-2*x13*x15*x8*x9+n*x13*x15*x8*x9+
3*x10*x15*x9^2-n*x10*x15*x9^2
];
x38=Expand[
3*x10^2*x12*x13-n*x10^2*x12*x13+x13^3*x8-x12*x13*x15*x8-
4*x10*x13^2*x9+n*x10*x13^2*x9-2*x10*x12*x15*x9+n*x10*x12*x15*x9+
3*x13*x15*x9^2-n*x13*x15*x9^2
];
x39=Expand[
-2*x10*x12*x13*x8+n*x10*x12*x13*x8+3*x10^2*x12*x9-n*x10^2*x12*x9+
3*x13^2*x8*x9-n*x13^2*x8*x9-x12*x15*x8*x9-4*x10*x13*x9^2+
n*x10*x13*x9^2+x15*x9^3
];
x40=Expand[
3*x10^3*x12-n*x10^3*x12-x10*x13^2*x8+n*x10*x13^2*x8-
3*x10*x12*x15*x8+n*x10*x12*x15*x8-2*x10^2*x13*x9+4*x13*x15*x8*x9-
2*n*x13*x15*x8*x9-x10*x15*x9^2+n*x10*x15*x9^2
];
x41=Expand[
x10^2*x12*x13-n*x10^2*x12*x13-3*x13^3*x8+n*x13^3*x8+
3*x12*x13*x15*x8-n*x12*x13*x15*x8+2*x10*x13^2*x9-4*x10*x12*x15*x9+
2*n*x10*x12*x15*x9+x13*x15*x9^2-n*x13*x15*x9^2
];
x42=Expand[
-4*x10*x12*x13*x8+2*n*x10*x12*x13*x8+x10^2*x12*x9-n*x10^2*x12*x9+
x13^2*x8*x9-n*x13^2*x8*x9+3*x12*x15*x8*x9-n*x12*x15*x8*x9+
2*x10*x13*x9^2-3*x15*x9^3+n*x15*x9^3
];
x43=Expand[
x10^2*x12^2-n*x10^2*x12^2-3*x12*x13^2*x8+n*x12*x13^2*x8+
3*x12^2*x15*x8-n*x12^2*x15*x8-2*x10*x12*x13*x9+2*n*x10*x12*x13*x9+
4*x13^2*x9^2-2*n*x13^2*x9^2-3*x12*x15*x9^2+n*x12*x15*x9^2
];
x44=Expand[
4*x10^2*x13^2-2*n*x10^2*x13^2-3*x10^2*x12*x15+n*x10^2*x12*x15-
3*x13^2*x15*x8+n*x13^2*x15*x8+3*x12*x15^2*x8-n*x12*x15^2*x8-
2*x10*x13*x15*x9+2*n*x10*x13*x15*x9+x15^2*x9^2-n*x15^2*x9^2
];
x45=Expand[
-3*x10^2*x12*x8+n*x10^2*x12*x8+x13^2*x8^2-n*x13^2*x8^2+
3*x12*x15*x8^2-n*x12*x15*x8^2-2*x10*x13*x8*x9+2*n*x10*x13*x8*x9+
4*x10^2*x9^2-2*n*x10^2*x9^2-3*x15*x8*x9^2+n*x15*x8*x9^2
];
(x1*x27)/(x19*x26)-(x29*x3*x5)/(x19*x26^2)+(x2*x31*x5)/(x19*x26^2)+
(x30*x4*x5)/(x19*x26^2)-(x2*x29*x6)/(x19*x26^2)+
(x28*x3*x6)/(x19*x26^2)-(x32*x4*x6)/(x19*x26^2)+
(x2*x30*x7)/(x19*x26^2)-(x3*x32*x7)/(x19*x26^2)+(x33*x4*x7)/(x19*x26^2)
]
,
(* Dmunurho *)
tidl[{{q_,mu_}, {q_,nu_},{q_,rho_}},{p1_,p2_,p3_},n_] :>
Block[{x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,
x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27,x28,x29,x30,x31,x32,x33,
x34,x35,x36,x37,x38,x39,x40,x41,x42,x43,x44,x45,x46,x47,x48,x49,x50,
x51,x52,x53,x54,x55,x56,x57,x58,x59,x60,x61,x62,x63,x64,x65,x66,x67,
x68,x69,x70,x71,x72,x73,x74,x75,x76,x77,x78,x79,x80,x81,x82,x83,x84,
x85,x86,x87,x88,x89,x90,x91,x92,x93},
x1=Pair[LorentzIndex[mu,n],LorentzIndex[nu,n]];
x2=Pair[LorentzIndex[mu,n],LorentzIndex[rho,n]];
x3=ExpandScalarProduct[LorentzIndex[mu,n],Momentum[p1,n]];
x4=ExpandScalarProduct[LorentzIndex[mu,n],Momentum[p2,n]];
x5=ExpandScalarProduct[LorentzIndex[mu,n],Momentum[p3,n]];
x6=ExpandScalarProduct[LorentzIndex[nu,n],LorentzIndex[rho,n]];
x7=ExpandScalarProduct[LorentzIndex[nu,n],Momentum[p1,n]];
x8=ExpandScalarProduct[LorentzIndex[nu,n],Momentum[p2,n]];
x9=ExpandScalarProduct[LorentzIndex[nu,n],Momentum[p3,n]];
x10=ExpandScalarProduct[LorentzIndex[rho,n],Momentum[p1,n]];
x11=ExpandScalarProduct[LorentzIndex[rho,n],Momentum[p2,n]];
x12=ExpandScalarProduct[LorentzIndex[rho,n],Momentum[p3,n]];
x13=ExpandScalarProduct[Momentum[p1,n],Momentum[p1,n]];
x14=ExpandScalarProduct[Momentum[p1,n],Momentum[p2,n]];
x15=ExpandScalarProduct[Momentum[p1,n],Momentum[p3,n]];
x16=ExpandScalarProduct[Momentum[p1,n],Momentum[q,n]];
x17=ExpandScalarProduct[Momentum[p2,n],Momentum[p2,n]];
x18=ExpandScalarProduct[Momentum[p2,n],Momentum[p3,n]];
x19=ExpandScalarProduct[Momentum[p2,n],Momentum[q,n]];
x20=ExpandScalarProduct[Momentum[p3,n],Momentum[p3,n]];
x21=ExpandScalarProduct[Momentum[p3,n],Momentum[q,n]];
x22=Pair[Momentum[q,n],Momentum[q,n]];
x23=Expand[-3+n];
x24=Expand[x14^2-x13*x17];
x25=Expand[x14*x15-x13*x18];
x26=Expand[x15^2-x13*x20];
x27=Expand[x15*x17-x14*x18];
x28=Expand[x15*x18-x14*x20];
x29=Expand[x18^2-x17*x20];
x30=Expand[
x19*(x15^2-x13*x20)-x16*(x15*x18-x14*x20)-(x14*x15-x13*x18)*x21
];
x31=Expand[
x16*(x15*x17-x14*x18)-(x14*x15-x13*x18)*x19+(x14^2-x13*x17)*x21
];
x32=Expand[
-(x19*(x15*x18-x14*x20))+x16*(x18^2-x17*x20)+(x15*x17-x14*x18)*x21
];
x33=Factor[
x15^2*x17-2*x14*x15*x18+x13*x18^2+x14^2*x20-x13*x17*x20
];
x34=Expand[
3*x14^2*x15^2-x13*x15^2*x17-4*x13*x14*x15*x18+2*x13^2*x18^2-
x13*x14^2*x20+x13^2*x17*x20
];
x35=Expand[
2*x14*x15^2*x17-x14^2*x15*x18-3*x13*x15*x17*x18+2*x13*x14*x18^2-
x14^3*x20+x13*x14*x17*x20
];
x36=Expand[
x15^3*x17+x14*x15^2*x18-2*x13*x15*x18^2-2*x14^2*x15*x20-
x13*x15*x17*x20+3*x13*x14*x18*x20
];
x37=Expand[
2*x15^2*x17^2-4*x14*x15*x17*x18+3*x14^2*x18^2-x13*x17*x18^2-
x14^2*x17*x20+x13*x17^2*x20
];
x38=Expand[
2*x15^2*x17*x18-x14*x15*x18^2-x13*x18^3-3*x14*x15*x17*x20+
2*x14^2*x18*x20+x13*x17*x18*x20
];
x39=Expand[
3*x15^2*x18^2-x15^2*x17*x20-4*x14*x15*x18*x20-x13*x18^2*x20+
2*x14^2*x20^2+x13*x17*x20^2
];
x40=Expand[
-(x19^2*(x15^2-x13*x20))+2*x16*x19*(x15*x18-x14*x20)-
x16^2*(x18^2-x17*x20)-2*x16*(x15*x17-x14*x18)*x21+
2*(x14*x15-x13*x18)*x19*x21-(x14^2-x13*x17)*x21^2+
(x15^2*x17-2*x14*x15*x18+x13*x18^2+x14^2*x20-x13*x17*x20)*x22
];
x41=Expand[
n*x19^2*(x15^2-x13*x20)^2-2*n*x16*x19*(x15^2-x13*x20)*
(x15*x18-x14*x20)+x16^2*
(n*x15^2*x18^2-3*x15^2*x17*x20+6*x14*x15*x18*x20-
2*n*x14*x15*x18*x20-3*x13*x18^2*x20-3*x14^2*x20^2+n*x14^2*x20^2+
3*x13*x17*x20^2)-2*n*(x14*x15-x13*x18)*x19*(x15^2-x13*x20)*x21+
2*x16*(3*x15^3*x17-6*x14*x15^2*x18+n*x14*x15^2*x18+
3*x13*x15*x18^2-n*x13*x15*x18^2+3*x14^2*x15*x20-
n*x14^2*x15*x20-3*x13*x15*x17*x20+n*x13*x14*x18*x20)*x21+
(n*x14^2*x15^2-3*x13*x15^2*x17+6*x13*x14*x15*x18-
2*n*x13*x14*x15*x18-3*x13^2*x18^2+n*x13^2*x18^2-
3*x13*x14^2*x20+3*x13^2*x17*x20)*x21^2-
3*(x15^2-x13*x20)*(x15^2*x17-2*x14*x15*x18+x13*x18^2+x14^2*x20-
x13*x17*x20)*x22
];
x42=Expand[
-2*n*x16*x19*(x15*x18-x14*x20)*(x18^2-x17*x20)+
n*x16^2*(x18^2-x17*x20)^2+
x19^2*(n*x15^2*x18^2-3*x15^2*x17*x20+6*x14*x15*x18*x20-
2*n*x14*x15*x18*x20-3*x13*x18^2*x20-3*x14^2*x20^2+n*x14^2*x20^2+
3*x13*x17*x20^2)+2*n*x16*(x15*x17-x14*x18)*(x18^2-x17*x20)*x21+
2*x19*(3*x15^2*x17*x18-n*x15^2*x17*x18-6*x14*x15*x18^2+
n*x14*x15*x18^2+3*x13*x18^3+n*x14*x15*x17*x20+3*x14^2*x18*x20-
n*x14^2*x18*x20-3*x13*x17*x18*x20)*x21-
(3*x15^2*x17^2-n*x15^2*x17^2-6*x14*x15*x17*x18+
2*n*x14*x15*x17*x18-n*x14^2*x18^2+3*x13*x17*x18^2+
3*x14^2*x17*x20-3*x13*x17^2*x20)*x21^2-
3*(x18^2-x17*x20)*(x15^2*x17-2*x14*x15*x18+x13*x18^2+x14^2*x20-
x13*x17*x20)*x22
];
x43=Expand[
x19^2*(n*x14^2*x15^2-3*x13*x15^2*x17+6*x13*x14*x15*x18-
2*n*x13*x14*x15*x18-3*x13^2*x18^2+n*x13^2*x18^2-3*x13*x14^2*x20+
3*x13^2*x17*x20)+2*x16*x19*
(3*x14*x15^2*x17-n*x14*x15^2*x17-6*x14^2*x15*x18+n*x14^2*x15*x18+
n*x13*x15*x17*x18+3*x13*x14*x18^2-n*x13*x14*x18^2+3*x14^3*x20-
3*x13*x14*x17*x20)-x16^2*
(3*x15^2*x17^2-n*x15^2*x17^2-6*x14*x15*x17*x18+
2*n*x14*x15*x17*x18-n*x14^2*x18^2+3*x13*x17*x18^2+
3*x14^2*x17*x20-3*x13*x17^2*x20)+
2*n*x16*(x14^2-x13*x17)*(x15*x17-x14*x18)*x21-
2*n*(x14^2-x13*x17)*(x14*x15-x13*x18)*x19*x21+
n*(x14^2-x13*x17)^2*x21^2-
3*(x14^2-x13*x17)*(x15^2*x17-2*x14*x15*x18+x13*x18^2+x14^2*x20-
x13*x17*x20)*x22
];
x44=Expand[
n*x14^2*x15^2-3*x13*x15^2*x17+6*x13*x14*x15*x18-2*n*x13*x14*x15*x18-
3*x13^2*x18^2+n*x13^2*x18^2-3*x13*x14^2*x20+3*x13^2*x17*x20
];
x45=Expand[
n*x14^2*x15^2-x13*x15^2*x17+2*x13*x14*x15*x18-2*n*x13*x14*x15*x18-
x13^2*x18^2+n*x13^2*x18^2-x13*x14^2*x20+x13^2*x17*x20
];
x46=Expand[
x15^2*x17^2-n*x15^2*x17^2-2*x14*x15*x17*x18+2*n*x14*x15*x17*x18-
n*x14^2*x18^2+x13*x17*x18^2+x14^2*x17*x20-x13*x17^2*x20
];
x47=Expand[
3*x15^2*x17^2-n*x15^2*x17^2-6*x14*x15*x17*x18+2*n*x14*x15*x17*x18-
n*x14^2*x18^2+3*x13*x17*x18^2+3*x14^2*x17*x20-3*x13*x17^2*x20
];
x48=Expand[
n*x15^2*x18^2-3*x15^2*x17*x20+6*x14*x15*x18*x20-2*n*x14*x15*x18*x20-
3*x13*x18^2*x20-3*x14^2*x20^2+n*x14^2*x20^2+3*x13*x17*x20^2
];
x49=Expand[
n*x15^2*x18^2-x15^2*x17*x20+2*x14*x15*x18*x20-2*n*x14*x15*x18*x20-
x13*x18^2*x20-x14^2*x20^2+n*x14^2*x20^2+x13*x17*x20^2
];
x50=Expand[
x14*x15^2*x17-n*x14*x15^2*x17-2*x14^2*x15*x18+n*x14^2*x15*x18+
n*x13*x15*x17*x18+x13*x14*x18^2-n*x13*x14*x18^2+x14^3*x20-
x13*x14*x17*x20
];
x51=Expand[
3*x14*x15^2*x17-n*x14*x15^2*x17-6*x14^2*x15*x18+n*x14^2*x15*x18+
n*x13*x15*x17*x18+3*x13*x14*x18^2-n*x13*x14*x18^2+3*x14^3*x20-
3*x13*x14*x17*x20
];
x52=Expand[
x15^3*x17-2*x14*x15^2*x18+n*x14*x15^2*x18+x13*x15*x18^2-
n*x13*x15*x18^2+x14^2*x15*x20-n*x14^2*x15*x20-x13*x15*x17*x20+
n*x13*x14*x18*x20
];
x53=Expand[
3*x15^3*x17-6*x14*x15^2*x18+n*x14*x15^2*x18+3*x13*x15*x18^2-
n*x13*x15*x18^2+3*x14^2*x15*x20-n*x14^2*x15*x20-3*x13*x15*x17*x20+
n*x13*x14*x18*x20
];
x54=Expand[
x15^2*x17*x18-n*x15^2*x17*x18-2*x14*x15*x18^2+n*x14*x15*x18^2+
x13*x18^3+n*x14*x15*x17*x20+x14^2*x18*x20-n*x14^2*x18*x20-
x13*x17*x18*x20
];
x55=Expand[
3*x15^2*x17*x18-n*x15^2*x17*x18-6*x14*x15*x18^2+n*x14*x15*x18^2+
3*x13*x18^3+n*x14*x15*x17*x20+3*x14^2*x18*x20-n*x14^2*x18*x20-
3*x13*x17*x18*x20
];
x56=Expand[
3*n*x14^2*x15^2+x13*x15^2*x17-2*n*x13*x15^2*x17-2*x13*x14*x15*x18-
2*n*x13*x14*x15*x18+x13^2*x18^2+n*x13^2*x18^2+x13*x14^2*x20-
2*n*x13*x14^2*x20-x13^2*x17*x20+2*n*x13^2*x17*x20
];
x57=Expand[
3*n*x14^2*x15^2+2*x13*x15^2*x17-n*x13*x15^2*x17-4*x13*x14*x15*x18-
4*n*x13*x14*x15*x18+2*x13^2*x18^2+2*n*x13^2*x18^2+2*x13*x14^2*x20-
n*x13*x14^2*x20-2*x13^2*x17*x20+n*x13^2*x17*x20
];
x58=Expand[
2*x14*x15^2*x17+2*n*x14*x15^2*x17-4*x14^2*x15*x18-n*x14^2*x15*x18-
3*n*x13*x15*x17*x18+2*x13*x14*x18^2+2*n*x13*x14*x18^2+2*x14^3*x20-
n*x14^3*x20-2*x13*x14*x17*x20+n*x13*x14*x17*x20
];
x59=Expand[
2*x15^3*x17-n*x15^3*x17-4*x14*x15^2*x18-n*x14*x15^2*x18+
2*x13*x15*x18^2+2*n*x13*x15*x18^2+2*x14^2*x15*x20+2*n*x14^2*x15*x20-
2*x13*x15*x17*x20+n*x13*x15*x17*x20-3*n*x13*x14*x18*x20
];
x60=Expand[
x15^2*x17^2+n*x15^2*x17^2-2*x14*x15*x17*x18-2*n*x14*x15*x17*x18+
3*n*x14^2*x18^2+x13*x17*x18^2-2*n*x13*x17*x18^2+x14^2*x17*x20-
2*n*x14^2*x17*x20-x13*x17^2*x20+2*n*x13*x17^2*x20
];
x61=Expand[
2*x15^2*x17^2+2*n*x15^2*x17^2-4*x14*x15*x17*x18-4*n*x14*x15*x17*x18+
3*n*x14^2*x18^2+2*x13*x17*x18^2-n*x13*x17*x18^2+2*x14^2*x17*x20-
n*x14^2*x17*x20-2*x13*x17^2*x20+n*x13*x17^2*x20
];
x62=Expand[
2*x15^2*x17*x18+2*n*x15^2*x17*x18-4*x14*x15*x18^2-n*x14*x15*x18^2+
2*x13*x18^3-n*x13*x18^3-3*n*x14*x15*x17*x20+2*x14^2*x18*x20+
2*n*x14^2*x18*x20-2*x13*x17*x18*x20+n*x13*x17*x18*x20
];
x63=Expand[
3*n*x15^2*x18^2+x15^2*x17*x20-2*n*x15^2*x17*x20-2*x14*x15*x18*x20-
2*n*x14*x15*x18*x20+x13*x18^2*x20-2*n*x13*x18^2*x20+x14^2*x20^2+
n*x14^2*x20^2-x13*x17*x20^2+2*n*x13*x17*x20^2
];
x64=Expand[
3*n*x15^2*x18^2+2*x15^2*x17*x20-n*x15^2*x17*x20-4*x14*x15*x18*x20-
4*n*x14*x15*x18*x20+2*x13*x18^2*x20-n*x13*x18^2*x20+2*x14^2*x20^2+
2*n*x14^2*x20^2-2*x13*x17*x20^2+n*x13*x17*x20^2
];
x65=Expand[
-(n*(x14*x15-x13*x18)*x19^3*(x15^2-x13*x20)^2)-
x16*x19^2*(x15^2-x13*x20)*(2*x15^3*x17-n*x15^3*x17-4*x14*x15^2*x18-
n*x14*x15^2*x18+2*x13*x15*x18^2+2*n*x13*x15*x18^2+
2*x14^2*x15*x20+2*n*x14^2*x15*x20-2*x13*x15*x17*x20+
n*x13*x15*x17*x20-3*n*x13*x14*x18*x20)+
x16^2*x19*(4*x15^4*x17*x18-2*n*x15^4*x17*x18-8*x14*x15^3*x18^2+
n*x14*x15^3*x18^2+4*x13*x15^2*x18^3+n*x13*x15^2*x18^3-
x14*x15^3*x17*x20+2*n*x14*x15^3*x17*x20+6*x14^2*x15^2*x18*x20-
7*x13*x15^2*x17*x18*x20+2*n*x13*x15^2*x17*x18*x20+
5*x13*x14*x15*x18^2*x20-4*n*x13*x14*x15*x18^2*x20-
3*x13^2*x18^3*x20-x14^3*x15*x20^2-n*x14^3*x15*x20^2+
x13*x14*x15*x17*x20^2-2*n*x13*x14*x15*x17*x20^2-
3*x13*x14^2*x18*x20^2+3*n*x13*x14^2*x18*x20^2+3*x13^2*x17*x18*x20^2)\
-x16^3*(2*x15^3*x17*x18^2-n*x15^3*x17*x18^2-4*x14*x15^2*x18^3+
n*x14*x15^2*x18^3+2*x13*x15*x18^4+x15^3*x17^2*x20-
5*x14*x15^2*x17*x18*x20+2*n*x14*x15^2*x17*x18*x20+
8*x14^2*x15*x18^2*x20-2*n*x14^2*x15*x18^2*x20-
x13*x15*x17*x18^2*x20-3*x13*x14*x18^3*x20+x14^2*x15*x17*x20^2-
n*x14^2*x15*x17*x20^2-x13*x15*x17^2*x20^2-3*x14^3*x18*x20^2+
n*x14^3*x18*x20^2+3*x13*x14*x17*x18*x20^2)+
x19^2*(x15^2-x13*x20)*(3*n*x14^2*x15^2+2*x13*x15^2*x17-
n*x13*x15^2*x17-4*x13*x14*x15*x18-4*n*x13*x14*x15*x18+
2*x13^2*x18^2+2*n*x13^2*x18^2+2*x13*x14^2*x20-n*x13*x14^2*x20-
2*x13^2*x17*x20+n*x13^2*x17*x20)*x21-
2*x16*x19*(x14*x15^4*x17+n*x14*x15^4*x17-2*x14^2*x15^3*x18+
n*x14^2*x15^3*x18+x13*x15^3*x17*x18-2*n*x13*x15^3*x17*x18-
x13*x14*x15^2*x18^2-n*x13*x14*x15^2*x18^2+x13^2*x15*x18^3+
n*x13^2*x15*x18^3+x14^3*x15^2*x20-2*n*x14^3*x15^2*x20-
3*x13*x14*x15^2*x17*x20+5*x13*x14^2*x15*x18*x20+
2*n*x13*x14^2*x15*x18*x20-x13^2*x15*x17*x18*x20+
2*n*x13^2*x15*x17*x18*x20-2*x13^2*x14*x18^2*x20-
2*n*x13^2*x14*x18^2*x20-2*x13*x14^3*x20^2+n*x13*x14^3*x20^2+
2*x13^2*x14*x17*x20^2-n*x13^2*x14*x17*x20^2)*x21+
x16^2*(2*x15^4*x17^2-6*x14*x15^3*x17*x18+2*n*x14*x15^3*x17*x18+
4*x14^2*x15^2*x18^2-n*x14^2*x15^2*x18^2+4*x13*x15^2*x17*x18^2-
3*n*x13*x15^2*x17*x18^2-6*x13*x14*x15*x18^3+
2*n*x13*x14*x15*x18^3+2*x13^2*x18^4-x14^2*x15^2*x17*x20-
2*n*x14^2*x15^2*x17*x20-x13*x15^2*x17^2*x20+4*x14^3*x15*x18*x20+
4*n*x13*x14*x15*x17*x18*x20-x13*x14^2*x18^2*x20-
2*n*x13*x14^2*x18^2*x20-x13^2*x17*x18^2*x20-3*x14^4*x20^2+
n*x14^4*x20^2+4*x13*x14^2*x17*x20^2-n*x13*x14^2*x17*x20^2-
x13^2*x17^2*x20^2)*x21-(x14*x15-x13*x18)*x19*
(3*n*x14^2*x15^2+x13*x15^2*x17-2*n*x13*x15^2*x17-
2*x13*x14*x15*x18-2*n*x13*x14*x15*x18+x13^2*x18^2+
n*x13^2*x18^2+x13*x14^2*x20-2*n*x13*x14^2*x20-x13^2*x17*x20+
2*n*x13^2*x17*x20)*x21^2+
x16*(4*x14^2*x15^3*x17+n*x14^2*x15^3*x17-3*x13*x15^3*x17^2-
8*x14^3*x15^2*x18+n*x14^3*x15^2*x18+5*x13*x14*x15^2*x17*x18-
4*n*x13*x14*x15^2*x17*x18+6*x13*x14^2*x15*x18^2-
3*x13^2*x15*x17*x18^2+3*n*x13^2*x15*x17*x18^2-x13^2*x14*x18^3-
n*x13^2*x14*x18^3+4*x14^4*x15*x20-2*n*x14^4*x15*x20-
7*x13*x14^2*x15*x17*x20+2*n*x13*x14^2*x15*x17*x20+
3*x13^2*x15*x17^2*x20-x13*x14^3*x18*x20+2*n*x13*x14^3*x18*x20+
x13^2*x14*x17*x18*x20-2*n*x13^2*x14*x17*x18*x20)*x21^2+
(x14^2-x13*x17)*(n*x14^2*x15^2-x13*x15^2*x17+2*x13*x14*x15*x18-
2*n*x13*x14*x15*x18-x13^2*x18^2+n*x13^2*x18^2-x13*x14^2*x20+
x13^2*x17*x20)*x21^3+3*(x14*x15-x13*x18)*x19*(x15^2-x13*x20)*
(x15^2*x17-2*x14*x15*x18+x13*x18^2+x14^2*x20-x13*x17*x20)*x22-
x16*(x15^2*x17-2*x14*x15*x18+x13*x18^2+x14^2*x20-x13*x17*x20)*
(x15^3*x17+x14*x15^2*x18-2*x13*x15*x18^2-2*x14^2*x15*x20-
x13*x15*x17*x20+3*x13*x14*x18*x20)*x22-
(x15^2*x17-2*x14*x15*x18+x13*x18^2+x14^2*x20-x13*x17*x20)*
(3*x14^2*x15^2-x13*x15^2*x17-4*x13*x14*x15*x18+2*x13^2*x18^2-
x13*x14^2*x20+x13^2*x17*x20)*x21*x22
];
x66=Expand[
-(n*x19^3*(x15^2-x13*x20)^2*(x15*x18-x14*x20))+
x16^3*(x18^2-x17*x20)*(n*x15^2*x18^2-x15^2*x17*x20+
2*x14*x15*x18*x20-2*n*x14*x15*x18*x20-x13*x18^2*x20-x14^2*x20^2+
n*x14^2*x20^2+x13*x17*x20^2)+
x16*x19^2*(x15^2-x13*x20)*(3*n*x15^2*x18^2+2*x15^2*x17*x20-
n*x15^2*x17*x20-4*x14*x15*x18*x20-4*n*x14*x15*x18*x20+
2*x13*x18^2*x20-n*x13*x18^2*x20+2*x14^2*x20^2+2*n*x14^2*x20^2-
2*x13*x17*x20^2+n*x13*x17*x20^2)-
x16^2*x19*(x15*x18-x14*x20)*
(3*n*x15^2*x18^2+x15^2*x17*x20-2*n*x15^2*x17*x20-
2*x14*x15*x18*x20-2*n*x14*x15*x18*x20+x13*x18^2*x20-
2*n*x13*x18^2*x20+x14^2*x20^2+n*x14^2*x20^2-x13*x17*x20^2+
2*n*x13*x17*x20^2)-x19^2*(x15^2-x13*x20)*
(2*x15^3*x17-n*x15^3*x17-4*x14*x15^2*x18-n*x14*x15^2*x18+
2*x13*x15*x18^2+2*n*x13*x15*x18^2+2*x14^2*x15*x20+
2*n*x14^2*x15*x20-2*x13*x15*x17*x20+n*x13*x15*x17*x20-
3*n*x13*x14*x18*x20)*x21-
2*x16*x19*(x15^4*x17*x18+n*x15^4*x17*x18-2*x14*x15^3*x18^2+
n*x14*x15^3*x18^2+x13*x15^2*x18^3-2*n*x13*x15^2*x18^3+
x14*x15^3*x17*x20-2*n*x14*x15^3*x17*x20-x14^2*x15^2*x18*x20-
n*x14^2*x15^2*x18*x20-3*x13*x15^2*x17*x18*x20+
5*x13*x14*x15*x18^2*x20+2*n*x13*x14*x15*x18^2*x20-
2*x13^2*x18^3*x20+n*x13^2*x18^3*x20+x14^3*x15*x20^2+
n*x14^3*x15*x20^2-x13*x14*x15*x17*x20^2+
2*n*x13*x14*x15*x17*x20^2-2*x13*x14^2*x18*x20^2-
2*n*x13*x14^2*x18*x20^2+2*x13^2*x17*x18*x20^2-n*x13^2*x17*x18*x20^2
)*x21+x16^2*(4*x15^3*x17*x18^2+n*x15^3*x17*x18^2-
8*x14*x15^2*x18^3+n*x14*x15^2*x18^3+4*x13*x15*x18^4-
2*n*x13*x15*x18^4-3*x15^3*x17^2*x20+5*x14*x15^2*x17*x18*x20-
4*n*x14*x15^2*x17*x18*x20+6*x14^2*x15*x18^2*x20-
7*x13*x15*x17*x18^2*x20+2*n*x13*x15*x17*x18^2*x20-
x13*x14*x18^3*x20+2*n*x13*x14*x18^3*x20-3*x14^2*x15*x17*x20^2+
3*n*x14^2*x15*x17*x20^2+3*x13*x15*x17^2*x20^2-x14^3*x18*x20^2-
n*x14^3*x18*x20^2+x13*x14*x17*x18*x20^2-2*n*x13*x14*x17*x18*x20^2)*
x21+x19*(4*x14*x15^4*x17-2*n*x14*x15^4*x17-8*x14^2*x15^3*x18+
n*x14^2*x15^3*x18-x13*x15^3*x17*x18+2*n*x13*x15^3*x17*x18+
6*x13*x14*x15^2*x18^2-x13^2*x15*x18^3-n*x13^2*x15*x18^3+
4*x14^3*x15^2*x20+n*x14^3*x15^2*x20-7*x13*x14*x15^2*x17*x20+
2*n*x13*x14*x15^2*x17*x20+5*x13*x14^2*x15*x18*x20-
4*n*x13*x14^2*x15*x18*x20+x13^2*x15*x17*x18*x20-
2*n*x13^2*x15*x17*x18*x20-3*x13^2*x14*x18^2*x20+
3*n*x13^2*x14*x18^2*x20-3*x13*x14^3*x20^2+3*x13^2*x14*x17*x20^2)*
x21^2+x16*(2*x15^4*x17^2-6*x14*x15^3*x17*x18+
2*n*x14*x15^3*x17*x18+4*x14^2*x15^2*x18^2-n*x14^2*x15^2*x18^2-
x13*x15^2*x17*x18^2-2*n*x13*x15^2*x17*x18^2+4*x13*x14*x15*x18^3-
3*x13^2*x18^4+n*x13^2*x18^4+4*x14^2*x15^2*x17*x20-
3*n*x14^2*x15^2*x17*x20-x13*x15^2*x17^2*x20-6*x14^3*x15*x18*x20+
2*n*x14^3*x15*x18*x20+4*n*x13*x14*x15*x17*x18*x20-
x13*x14^2*x18^2*x20-2*n*x13*x14^2*x18^2*x20+
4*x13^2*x17*x18^2*x20-n*x13^2*x17*x18^2*x20+2*x14^4*x20^2-
x13*x14^2*x17*x20^2-x13^2*x17^2*x20^2)*x21^2-
(2*x14^2*x15^3*x17-n*x14^2*x15^3*x17+x13*x15^3*x17^2-
4*x14^3*x15^2*x18+n*x14^3*x15^2*x18-5*x13*x14*x15^2*x17*x18+
2*n*x13*x14*x15^2*x17*x18+8*x13*x14^2*x15*x18^2-
2*n*x13*x14^2*x15*x18^2+x13^2*x15*x17*x18^2-
n*x13^2*x15*x17*x18^2-3*x13^2*x14*x18^3+n*x13^2*x14*x18^3+
2*x14^4*x15*x20-x13*x14^2*x15*x17*x20-x13^2*x15*x17^2*x20-
3*x13*x14^3*x18*x20+3*x13^2*x14*x17*x18*x20)*x21^3+
3*x19*(x15^2-x13*x20)*(x15*x18-x14*x20)*
(x15^2*x17-2*x14*x15*x18+x13*x18^2+x14^2*x20-x13*x17*x20)*x22-
x16*(x15^2*x17-2*x14*x15*x18+x13*x18^2+x14^2*x20-x13*x17*x20)*
(3*x15^2*x18^2-x15^2*x17*x20-4*x14*x15*x18*x20-x13*x18^2*x20+
2*x14^2*x20^2+x13*x17*x20^2)*x22-
(x15^2*x17-2*x14*x15*x18+x13*x18^2+x14^2*x20-x13*x17*x20)*
(x15^3*x17+x14*x15^2*x18-2*x13*x15*x18^2-2*x14^2*x15*x20-
x13*x15*x17*x20+3*x13*x14*x18*x20)*x21*x22
];
x67=Expand[
n*x16^3*(x15*x17-x14*x18)*(x18^2-x17*x20)^2-
x16^2*x19*(x18^2-x17*x20)*(2*x15^2*x17*x18+2*n*x15^2*x17*x18-
4*x14*x15*x18^2-n*x14*x15*x18^2+2*x13*x18^3-n*x13*x18^3-
3*n*x14*x15*x17*x20+2*x14^2*x18*x20+2*n*x14^2*x18*x20-
2*x13*x17*x18*x20+n*x13*x17*x18*x20)-
x19^3*(2*x15^4*x17*x18-4*x14*x15^3*x18^2+n*x14*x15^3*x18^2+
2*x13*x15^2*x18^3-n*x13*x15^2*x18^3-3*x14*x15^3*x17*x20+
8*x14^2*x15^2*x18*x20-2*n*x14^2*x15^2*x18*x20-
x13*x15^2*x17*x18*x20-5*x13*x14*x15*x18^2*x20+
2*n*x13*x14*x15*x18^2*x20+x13^2*x18^3*x20-3*x14^3*x15*x20^2+
n*x14^3*x15*x20^2+3*x13*x14*x15*x17*x20^2+x13*x14^2*x18*x20^2-
n*x13*x14^2*x18*x20^2-x13^2*x17*x18*x20^2)+
x16*x19^2*(4*x15^3*x17*x18^2+n*x15^3*x17*x18^2-8*x14*x15^2*x18^3+
n*x14*x15^2*x18^3+4*x13*x15*x18^4-2*n*x13*x15*x18^4-
3*x15^3*x17^2*x20+5*x14*x15^2*x17*x18*x20-
4*n*x14*x15^2*x17*x18*x20+6*x14^2*x15*x18^2*x20-
7*x13*x15*x17*x18^2*x20+2*n*x13*x15*x17*x18^2*x20-
x13*x14*x18^3*x20+2*n*x13*x14*x18^3*x20-3*x14^2*x15*x17*x20^2+
3*n*x14^2*x15*x17*x20^2+3*x13*x15*x17^2*x20^2-x14^3*x18*x20^2-
n*x14^3*x18*x20^2+x13*x14*x17*x18*x20^2-2*n*x13*x14*x17*x18*x20^2)+
x16^2*(x18^2-x17*x20)*(2*x15^2*x17^2+2*n*x15^2*x17^2-
4*x14*x15*x17*x18-4*n*x14*x15*x17*x18+3*n*x14^2*x18^2+
2*x13*x17*x18^2-n*x13*x17*x18^2+2*x14^2*x17*x20-
n*x14^2*x17*x20-2*x13*x17^2*x20+n*x13*x17^2*x20)*x21+
x19^2*(2*x15^4*x17^2-6*x14*x15^3*x17*x18+2*n*x14*x15^3*x17*x18+
4*x14^2*x15^2*x18^2-n*x14^2*x15^2*x18^2+4*x13*x15^2*x17*x18^2-
3*n*x13*x15^2*x17*x18^2-6*x13*x14*x15*x18^3+
2*n*x13*x14*x15*x18^3+2*x13^2*x18^4-x14^2*x15^2*x17*x20-
2*n*x14^2*x15^2*x17*x20-x13*x15^2*x17^2*x20+4*x14^3*x15*x18*x20+
4*n*x13*x14*x15*x17*x18*x20-x13*x14^2*x18^2*x20-
2*n*x13*x14^2*x18^2*x20-x13^2*x17*x18^2*x20-3*x14^4*x20^2+
n*x14^4*x20^2+4*x13*x14^2*x17*x20^2-n*x13*x14^2*x17*x20^2-
x13^2*x17^2*x20^2)*x21-2*x16*x19*
(x15^3*x17^2*x18+n*x15^3*x17^2*x18-x14*x15^2*x17*x18^2-
n*x14*x15^2*x17*x18^2-2*x14^2*x15*x18^3+n*x14^2*x15*x18^3+
x13*x15*x17*x18^3-2*n*x13*x15*x17*x18^3+x13*x14*x18^4+
n*x13*x14*x18^4-2*x14*x15^2*x17^2*x20-2*n*x14*x15^2*x17^2*x20+
5*x14^2*x15*x17*x18*x20+2*n*x14^2*x15*x17*x18*x20-
x13*x15*x17^2*x18*x20+2*n*x13*x15*x17^2*x18*x20+x14^3*x18^2*x20-
2*n*x14^3*x18^2*x20-3*x13*x14*x17*x18^2*x20-2*x14^3*x17*x20^2+
n*x14^3*x17*x20^2+2*x13*x14*x17^2*x20^2-n*x13*x14*x17^2*x20^2)*x21+
x16*(x15*x17-x14*x18)*(x15^2*x17^2+n*x15^2*x17^2-
2*x14*x15*x17*x18-2*n*x14*x15*x17*x18+3*n*x14^2*x18^2+
x13*x17*x18^2-2*n*x13*x17*x18^2+x14^2*x17*x20-
2*n*x14^2*x17*x20-x13*x17^2*x20+2*n*x13*x17^2*x20)*x21^2-
x19*(x14*x15^3*x17^2+n*x14*x15^3*x17^2-6*x14^2*x15^2*x17*x18+
3*x13*x15^2*x17^2*x18-3*n*x13*x15^2*x17^2*x18+8*x14^3*x15*x18^2-
n*x14^3*x15*x18^2-5*x13*x14*x15*x17*x18^2+
4*n*x13*x14*x15*x17*x18^2-4*x13*x14^2*x18^3-n*x13*x14^2*x18^3+
3*x13^2*x17*x18^3+x14^3*x15*x17*x20-2*n*x14^3*x15*x17*x20-
x13*x14*x15*x17^2*x20+2*n*x13*x14*x15*x17^2*x20-4*x14^4*x18*x20+
2*n*x14^4*x18*x20+7*x13*x14^2*x17*x18*x20-
2*n*x13*x14^2*x17*x18*x20-3*x13^2*x17^2*x18*x20)*x21^2-
(x14^2-x13*x17)*(x15^2*x17^2-n*x15^2*x17^2-2*x14*x15*x17*x18+
2*n*x14*x15*x17*x18-n*x14^2*x18^2+x13*x17*x18^2+x14^2*x17*x20-
x13*x17^2*x20)*x21^3-3*x16*(x15*x17-x14*x18)*(x18^2-x17*x20)*
(x15^2*x17-2*x14*x15*x18+x13*x18^2+x14^2*x20-x13*x17*x20)*x22+
x19*(x15^2*x17-2*x14*x15*x18+x13*x18^2+x14^2*x20-x13*x17*x20)*
(2*x15^2*x17*x18-x14*x15*x18^2-x13*x18^3-3*x14*x15*x17*x20+
2*x14^2*x18*x20+x13*x17*x18*x20)*x22-
(x15^2*x17-2*x14*x15*x18+x13*x18^2+x14^2*x20-x13*x17*x20)*
(2*x15^2*x17^2-4*x14*x15*x17*x18+3*x14^2*x18^2-x13*x17*x18^2-
x14^2*x17*x20+x13*x17^2*x20)*x21*x22
];
x68=Expand[
-(n*x16^3*(x15*x18-x14*x20)*(x18^2-x17*x20)^2)+
x19^3*(x15^2-x13*x20)*(n*x15^2*x18^2-x15^2*x17*x20+
2*x14*x15*x18*x20-2*n*x14*x15*x18*x20-x13*x18^2*x20-x14^2*x20^2+
n*x14^2*x20^2+x13*x17*x20^2)+
x16^2*x19*(x18^2-x17*x20)*(3*n*x15^2*x18^2+2*x15^2*x17*x20-
n*x15^2*x17*x20-4*x14*x15*x18*x20-4*n*x14*x15*x18*x20+
2*x13*x18^2*x20-n*x13*x18^2*x20+2*x14^2*x20^2+2*n*x14^2*x20^2-
2*x13*x17*x20^2+n*x13*x17*x20^2)-
x16*x19^2*(x15*x18-x14*x20)*
(3*n*x15^2*x18^2+x15^2*x17*x20-2*n*x15^2*x17*x20-
2*x14*x15*x18*x20-2*n*x14*x15*x18*x20+x13*x18^2*x20-
2*n*x13*x18^2*x20+x14^2*x20^2+n*x14^2*x20^2-x13*x17*x20^2+
2*n*x13*x17*x20^2)-x16^2*(x18^2-x17*x20)*
(2*x15^2*x17*x18+2*n*x15^2*x17*x18-4*x14*x15*x18^2-
n*x14*x15*x18^2+2*x13*x18^3-n*x13*x18^3-3*n*x14*x15*x17*x20+
2*x14^2*x18*x20+2*n*x14^2*x18*x20-2*x13*x17*x18*x20+
n*x13*x17*x18*x20)*x21+x19^2*
(4*x15^4*x17*x18-2*n*x15^4*x17*x18-8*x14*x15^3*x18^2+
n*x14*x15^3*x18^2+4*x13*x15^2*x18^3+n*x13*x15^2*x18^3-
x14*x15^3*x17*x20+2*n*x14*x15^3*x17*x20+6*x14^2*x15^2*x18*x20-
7*x13*x15^2*x17*x18*x20+2*n*x13*x15^2*x17*x18*x20+
5*x13*x14*x15*x18^2*x20-4*n*x13*x14*x15*x18^2*x20-
3*x13^2*x18^3*x20-x14^3*x15*x20^2-n*x14^3*x15*x20^2+
x13*x14*x15*x17*x20^2-2*n*x13*x14*x15*x17*x20^2-
3*x13*x14^2*x18*x20^2+3*n*x13*x14^2*x18*x20^2+3*x13^2*x17*x18*x20^2
)*x21-2*x16*x19*(x15^3*x17*x18^2-2*n*x15^3*x17*x18^2-
2*x14*x15^2*x18^3+n*x14*x15^2*x18^3+x13*x15*x18^4+
n*x13*x15*x18^4-2*x15^3*x17^2*x20+n*x15^3*x17^2*x20+
5*x14*x15^2*x17*x18*x20+2*n*x14*x15^2*x17*x18*x20-
x14^2*x15*x18^2*x20-n*x14^2*x15*x18^2*x20-
3*x13*x15*x17*x18^2*x20+x13*x14*x18^3*x20-2*n*x13*x14*x18^3*x20-
2*x14^2*x15*x17*x20^2-2*n*x14^2*x15*x17*x20^2+
2*x13*x15*x17^2*x20^2-n*x13*x15*x17^2*x20^2+x14^3*x18*x20^2+
n*x14^3*x18*x20^2-x13*x14*x17*x18*x20^2+2*n*x13*x14*x17*x18*x20^2)*
x21-x19*(3*x15^4*x17^2-n*x15^4*x17^2-4*x14*x15^3*x17*x18-
4*x14^2*x15^2*x18^2+n*x14^2*x15^2*x18^2+x13*x15^2*x17*x18^2+
2*n*x13*x15^2*x17*x18^2+6*x13*x14*x15*x18^3-
2*n*x13*x14*x15*x18^3-2*x13^2*x18^4+x14^2*x15^2*x17*x20+
2*n*x14^2*x15^2*x17*x20-4*x13*x15^2*x17^2*x20+
n*x13*x15^2*x17^2*x20+6*x14^3*x15*x18*x20-2*n*x14^3*x15*x18*x20-
4*n*x13*x14*x15*x17*x18*x20-4*x13*x14^2*x18^2*x20+
3*n*x13*x14^2*x18^2*x20+x13^2*x17*x18^2*x20-2*x14^4*x20^2+
x13*x14^2*x17*x20^2+x13^2*x17^2*x20^2)*x21^2-
x16*(x15^3*x17^2*x18+n*x15^3*x17^2*x18-6*x14*x15^2*x17*x18^2+
8*x14^2*x15*x18^3-n*x14^2*x15*x18^3+x13*x15*x17*x18^3-
2*n*x13*x15*x17*x18^3-4*x13*x14*x18^4+2*n*x13*x14*x18^4+
3*x14*x15^2*x17^2*x20-3*n*x14*x15^2*x17^2*x20-
5*x14^2*x15*x17*x18*x20+4*n*x14^2*x15*x17*x18*x20-
x13*x15*x17^2*x18*x20+2*n*x13*x15*x17^2*x18*x20-
4*x14^3*x18^2*x20-n*x14^3*x18^2*x20+7*x13*x14*x17*x18^2*x20-
2*n*x13*x14*x17*x18^2*x20+3*x14^3*x17*x20^2-3*x13*x14*x17^2*x20^2)*
x21^2+(3*x14*x15^3*x17^2-n*x14*x15^3*x17^2-8*x14^2*x15^2*x17*x18+
2*n*x14^2*x15^2*x17*x18-x13*x15^2*x17^2*x18+
n*x13*x15^2*x17^2*x18+4*x14^3*x15*x18^2-n*x14^3*x15*x18^2+
5*x13*x14*x15*x17*x18^2-2*n*x13*x14*x15*x17*x18^2-
2*x13*x14^2*x18^3+n*x13*x14^2*x18^3-x13^2*x17*x18^3+
3*x14^3*x15*x17*x20-3*x13*x14*x15*x17^2*x20-2*x14^4*x18*x20+
x13*x14^2*x17*x18*x20+x13^2*x17^2*x18*x20)*x21^3+
3*x16*(x15*x18-x14*x20)*(x18^2-x17*x20)*
(x15^2*x17-2*x14*x15*x18+x13*x18^2+x14^2*x20-x13*x17*x20)*x22-
x19*(x15^2*x17-2*x14*x15*x18+x13*x18^2+x14^2*x20-x13*x17*x20)*
(3*x15^2*x18^2-x15^2*x17*x20-4*x14*x15*x18*x20-x13*x18^2*x20+
2*x14^2*x20^2+x13*x17*x20^2)*x22+
(x15^2*x17-2*x14*x15*x18+x13*x18^2+x14^2*x20-x13*x17*x20)*
(2*x15^2*x17*x18-x14*x15*x18^2-x13*x18^3-3*x14*x15*x17*x20+
2*x14^2*x18*x20+x13*x17*x18*x20)*x21*x22
];
x69=Expand[
x19^3*(x15^2-x13*x20)*(n*x14^2*x15^2-x13*x15^2*x17+2*x13*x14*x15*x18-
2*n*x13*x14*x15*x18-x13^2*x18^2+n*x13^2*x18^2-x13*x14^2*x20+
x13^2*x17*x20)+x16*x19^2*
(4*x14*x15^4*x17-2*n*x14*x15^4*x17-8*x14^2*x15^3*x18+
n*x14^2*x15^3*x18-x13*x15^3*x17*x18+2*n*x13*x15^3*x17*x18+
6*x13*x14*x15^2*x18^2-x13^2*x15*x18^3-n*x13^2*x15*x18^3+
4*x14^3*x15^2*x20+n*x14^3*x15^2*x20-7*x13*x14*x15^2*x17*x20+
2*n*x13*x14*x15^2*x17*x20+5*x13*x14^2*x15*x18*x20-
4*n*x13*x14^2*x15*x18*x20+x13^2*x15*x17*x18*x20-
2*n*x13^2*x15*x17*x18*x20-3*x13^2*x14*x18^2*x20+
3*n*x13^2*x14*x18^2*x20-3*x13*x14^3*x20^2+3*x13^2*x14*x17*x20^2)-
x16^2*x19*(3*x15^4*x17^2-n*x15^4*x17^2-4*x14*x15^3*x17*x18-
4*x14^2*x15^2*x18^2+n*x14^2*x15^2*x18^2+x13*x15^2*x17*x18^2+
2*n*x13*x15^2*x17*x18^2+6*x13*x14*x15*x18^3-2*n*x13*x14*x15*x18^3-
2*x13^2*x18^4+x14^2*x15^2*x17*x20+2*n*x14^2*x15^2*x17*x20-
4*x13*x15^2*x17^2*x20+n*x13*x15^2*x17^2*x20+6*x14^3*x15*x18*x20-
2*n*x14^3*x15*x18*x20-4*n*x13*x14*x15*x17*x18*x20-
4*x13*x14^2*x18^2*x20+3*n*x13*x14^2*x18^2*x20+x13^2*x17*x18^2*x20-
2*x14^4*x20^2+x13*x14^2*x17*x20^2+x13^2*x17^2*x20^2)+
x16^3*(3*x15^3*x17^2*x18-n*x15^3*x17^2*x18-8*x14*x15^2*x17*x18^2+
2*n*x14*x15^2*x17*x18^2+4*x14^2*x15*x18^3-n*x14^2*x15*x18^3+
3*x13*x15*x17*x18^3-2*x13*x14*x18^4-x14*x15^2*x17^2*x20+
n*x14*x15^2*x17^2*x20+5*x14^2*x15*x17*x18*x20-
2*n*x14^2*x15*x17*x18*x20-3*x13*x15*x17^2*x18*x20-
2*x14^3*x18^2*x20+n*x14^3*x18^2*x20+x13*x14*x17*x18^2*x20-
x14^3*x17*x20^2+x13*x14*x17^2*x20^2)-
(x14*x15-x13*x18)*x19^2*(3*n*x14^2*x15^2+x13*x15^2*x17-
2*n*x13*x15^2*x17-2*x13*x14*x15*x18-2*n*x13*x14*x15*x18+
x13^2*x18^2+n*x13^2*x18^2+x13*x14^2*x20-2*n*x13*x14^2*x20-
x13^2*x17*x20+2*n*x13^2*x17*x20)*x21-
2*x16*x19*(x14^2*x15^3*x17-2*n*x14^2*x15^3*x17-2*x13*x15^3*x17^2+
n*x13*x15^3*x17^2-2*x14^3*x15^2*x18+n*x14^3*x15^2*x18+
5*x13*x14*x15^2*x17*x18+2*n*x13*x14*x15^2*x17*x18-
x13*x14^2*x15*x18^2-n*x13*x14^2*x15*x18^2-2*x13^2*x15*x17*x18^2-
2*n*x13^2*x15*x17*x18^2+x13^2*x14*x18^3+n*x13^2*x14*x18^3+
x14^4*x15*x20+n*x14^4*x15*x20-3*x13*x14^2*x15*x17*x20+
2*x13^2*x15*x17^2*x20-n*x13^2*x15*x17^2*x20+x13*x14^3*x18*x20-
2*n*x13*x14^3*x18*x20-x13^2*x14*x17*x18*x20+
2*n*x13^2*x14*x17*x18*x20)*x21-
x16^2*(x14*x15^3*x17^2+n*x14*x15^3*x17^2-6*x14^2*x15^2*x17*x18+
3*x13*x15^2*x17^2*x18-3*n*x13*x15^2*x17^2*x18+8*x14^3*x15*x18^2-
n*x14^3*x15*x18^2-5*x13*x14*x15*x17*x18^2+
4*n*x13*x14*x15*x17*x18^2-4*x13*x14^2*x18^3-n*x13*x14^2*x18^3+
3*x13^2*x17*x18^3+x14^3*x15*x17*x20-2*n*x14^3*x15*x17*x20-
x13*x14*x15*x17^2*x20+2*n*x13*x14*x15*x17^2*x20-4*x14^4*x18*x20+
2*n*x14^4*x18*x20+7*x13*x14^2*x17*x18*x20-
2*n*x13*x14^2*x17*x18*x20-3*x13^2*x17^2*x18*x20)*x21+
(x14^2-x13*x17)*x19*(3*n*x14^2*x15^2+2*x13*x15^2*x17-
n*x13*x15^2*x17-4*x13*x14*x15*x18-4*n*x13*x14*x15*x18+
2*x13^2*x18^2+2*n*x13^2*x18^2+2*x13*x14^2*x20-n*x13*x14^2*x20-
2*x13^2*x17*x20+n*x13^2*x17*x20)*x21^2-
x16*(x14^2-x13*x17)*(2*x14*x15^2*x17+2*n*x14*x15^2*x17-
4*x14^2*x15*x18-n*x14^2*x15*x18-3*n*x13*x15*x17*x18+
2*x13*x14*x18^2+2*n*x13*x14*x18^2+2*x14^3*x20-n*x14^3*x20-
2*x13*x14*x17*x20+n*x13*x14*x17*x20)*x21^2-
n*(x14^2-x13*x17)^2*(x14*x15-x13*x18)*x21^3-
x19*(x15^2*x17-2*x14*x15*x18+x13*x18^2+x14^2*x20-x13*x17*x20)*
(3*x14^2*x15^2-x13*x15^2*x17-4*x13*x14*x15*x18+2*x13^2*x18^2-
x13*x14^2*x20+x13^2*x17*x20)*x22+
x16*(x15^2*x17-2*x14*x15*x18+x13*x18^2+x14^2*x20-x13*x17*x20)*
(2*x14*x15^2*x17-x14^2*x15*x18-3*x13*x15*x17*x18+2*x13*x14*x18^2-
x14^3*x20+x13*x14*x17*x20)*x22+
3*(x14^2-x13*x17)*(x14*x15-x13*x18)*
(x15^2*x17-2*x14*x15*x18+x13*x18^2+x14^2*x20-x13*x17*x20)*x21*x22
];
x70=Expand[
-(x16^3*(x18^2-x17*x20)*(x15^2*x17^2-n*x15^2*x17^2-2*x14*x15*x17*x18+
2*n*x14*x15*x17*x18-n*x14^2*x18^2+x13*x17*x18^2+x14^2*x17*x20-
x13*x17^2*x20))-x19^3*
(2*x14*x15^4*x17-4*x14^2*x15^3*x18+n*x14^2*x15^3*x18-
3*x13*x15^3*x17*x18+8*x13*x14*x15^2*x18^2-2*n*x13*x14*x15^2*x18^2-
3*x13^2*x15*x18^3+n*x13^2*x15*x18^3+2*x14^3*x15^2*x20-
n*x14^3*x15^2*x20-x13*x14*x15^2*x17*x20-5*x13*x14^2*x15*x18*x20+
2*n*x13*x14^2*x15*x18*x20+3*x13^2*x15*x17*x18*x20+
x13^2*x14*x18^2*x20-n*x13^2*x14*x18^2*x20+x13*x14^3*x20^2-
x13^2*x14*x17*x20^2)+x16*x19^2*
(2*x15^4*x17^2-6*x14*x15^3*x17*x18+2*n*x14*x15^3*x17*x18+
4*x14^2*x15^2*x18^2-n*x14^2*x15^2*x18^2-x13*x15^2*x17*x18^2-
2*n*x13*x15^2*x17*x18^2+4*x13*x14*x15*x18^3-3*x13^2*x18^4+
n*x13^2*x18^4+4*x14^2*x15^2*x17*x20-3*n*x14^2*x15^2*x17*x20-
x13*x15^2*x17^2*x20-6*x14^3*x15*x18*x20+2*n*x14^3*x15*x18*x20+
4*n*x13*x14*x15*x17*x18*x20-x13*x14^2*x18^2*x20-
2*n*x13*x14^2*x18^2*x20+4*x13^2*x17*x18^2*x20-
n*x13^2*x17*x18^2*x20+2*x14^4*x20^2-x13*x14^2*x17*x20^2-
x13^2*x17^2*x20^2)-x16^2*x19*
(x15^3*x17^2*x18+n*x15^3*x17^2*x18-6*x14*x15^2*x17*x18^2+
8*x14^2*x15*x18^3-n*x14^2*x15*x18^3+x13*x15*x17*x18^3-
2*n*x13*x15*x17*x18^3-4*x13*x14*x18^4+2*n*x13*x14*x18^4+
3*x14*x15^2*x17^2*x20-3*n*x14*x15^2*x17^2*x20-
5*x14^2*x15*x17*x18*x20+4*n*x14^2*x15*x17*x18*x20-
x13*x15*x17^2*x18*x20+2*n*x13*x15*x17^2*x18*x20-4*x14^3*x18^2*x20-
n*x14^3*x18^2*x20+7*x13*x14*x17*x18^2*x20-
2*n*x13*x14*x17*x18^2*x20+3*x14^3*x17*x20^2-3*x13*x14*x17^2*x20^2)+
x16^2*(x15*x17-x14*x18)*(x15^2*x17^2+n*x15^2*x17^2-
2*x14*x15*x17*x18-2*n*x14*x15*x17*x18+3*n*x14^2*x18^2+
x13*x17*x18^2-2*n*x13*x17*x18^2+x14^2*x17*x20-
2*n*x14^2*x17*x20-x13*x17^2*x20+2*n*x13*x17^2*x20)*x21+
x19^2*(4*x14^2*x15^3*x17+n*x14^2*x15^3*x17-3*x13*x15^3*x17^2-
8*x14^3*x15^2*x18+n*x14^3*x15^2*x18+5*x13*x14*x15^2*x17*x18-
4*n*x13*x14*x15^2*x17*x18+6*x13*x14^2*x15*x18^2-
3*x13^2*x15*x17*x18^2+3*n*x13^2*x15*x17*x18^2-x13^2*x14*x18^3-
n*x13^2*x14*x18^3+4*x14^4*x15*x20-2*n*x14^4*x15*x20-
7*x13*x14^2*x15*x17*x20+2*n*x13*x14^2*x15*x17*x20+
3*x13^2*x15*x17^2*x20-x13*x14^3*x18*x20+2*n*x13*x14^3*x18*x20+
x13^2*x14*x17*x18*x20-2*n*x13^2*x14*x17*x18*x20)*x21-
2*x16*x19*(x14*x15^3*x17^2+n*x14*x15^3*x17^2-x14^2*x15^2*x17*x18-
n*x14^2*x15^2*x17*x18-2*x13*x15^2*x17^2*x18-
2*n*x13*x15^2*x17^2*x18-2*x14^3*x15*x18^2+n*x14^3*x15*x18^2+
5*x13*x14*x15*x17*x18^2+2*n*x13*x14*x15*x17*x18^2+
x13*x14^2*x18^3-2*n*x13*x14^2*x18^3-2*x13^2*x17*x18^3+
n*x13^2*x17*x18^3+x14^3*x15*x17*x20-2*n*x14^3*x15*x17*x20-
x13*x14*x15*x17^2*x20+2*n*x13*x14*x15*x17^2*x20+x14^4*x18*x20+
n*x14^4*x18*x20-3*x13*x14^2*x17*x18*x20+2*x13^2*x17^2*x18*x20-
n*x13^2*x17^2*x18*x20)*x21-
(x14^2-x13*x17)*x19*(2*x14*x15^2*x17+2*n*x14*x15^2*x17-
4*x14^2*x15*x18-n*x14^2*x15*x18-3*n*x13*x15*x17*x18+
2*x13*x14*x18^2+2*n*x13*x14*x18^2+2*x14^3*x20-n*x14^3*x20-
2*x13*x14*x17*x20+n*x13*x14*x17*x20)*x21^2+
x16*(x14^2-x13*x17)*(2*x15^2*x17^2+2*n*x15^2*x17^2-
4*x14*x15*x17*x18-4*n*x14*x15*x17*x18+3*n*x14^2*x18^2+
2*x13*x17*x18^2-n*x13*x17*x18^2+2*x14^2*x17*x20-
n*x14^2*x17*x20-2*x13*x17^2*x20+n*x13*x17^2*x20)*x21^2+
n*(x14^2-x13*x17)^2*(x15*x17-x14*x18)*x21^3+
x19*(x15^2*x17-2*x14*x15*x18+x13*x18^2+x14^2*x20-x13*x17*x20)*
(2*x14*x15^2*x17-x14^2*x15*x18-3*x13*x15*x17*x18+2*x13*x14*x18^2-
x14^3*x20+x13*x14*x17*x20)*x22-
x16*(x15^2*x17-2*x14*x15*x18+x13*x18^2+x14^2*x20-x13*x17*x20)*
(2*x15^2*x17^2-4*x14*x15*x17*x18+3*x14^2*x18^2-x13*x17*x18^2-
x14^2*x17*x20+x13*x17^2*x20)*x22-
3*(x14^2-x13*x17)*(x15*x17-x14*x18)*
(x15^2*x17-2*x14*x15*x18+x13*x18^2+x14^2*x20-x13*x17*x20)*x21*x22
];
x71=Expand[
x19^3*(x15^2-x13*x20)*(x15^3*x17-2*x14*x15^2*x18+n*x14*x15^2*x18+
x13*x15*x18^2-n*x13*x15*x18^2+x14^2*x15*x20-n*x14^2*x15*x20-
x13*x15*x17*x20+n*x13*x14*x18*x20)+
x16^3*(x18^2-x17*x20)*(x15^2*x17*x18-n*x15^2*x17*x18-
2*x14*x15*x18^2+n*x14*x15*x18^2+x13*x18^3+n*x14*x15*x17*x20+
x14^2*x18*x20-n*x14^2*x18*x20-x13*x17*x18*x20)-
x16*x19^2*(x15^4*x17*x18+n*x15^4*x17*x18-2*x14*x15^3*x18^2+
n*x14*x15^3*x18^2+x13*x15^2*x18^3-2*n*x13*x15^2*x18^3+
x14*x15^3*x17*x20-2*n*x14*x15^3*x17*x20-x14^2*x15^2*x18*x20-
n*x14^2*x15^2*x18*x20-3*x13*x15^2*x17*x18*x20+
5*x13*x14*x15*x18^2*x20+2*n*x13*x14*x15*x18^2*x20-
2*x13^2*x18^3*x20+n*x13^2*x18^3*x20+x14^3*x15*x20^2+
n*x14^3*x15*x20^2-x13*x14*x15*x17*x20^2+2*n*x13*x14*x15*x17*x20^2-
2*x13*x14^2*x18*x20^2-2*n*x13*x14^2*x18*x20^2+
2*x13^2*x17*x18*x20^2-n*x13^2*x17*x18*x20^2)-
x16^2*x19*(x15^3*x17*x18^2-2*n*x15^3*x17*x18^2-2*x14*x15^2*x18^3+
n*x14*x15^2*x18^3+x13*x15*x18^4+n*x13*x15*x18^4-
2*x15^3*x17^2*x20+n*x15^3*x17^2*x20+5*x14*x15^2*x17*x18*x20+
2*n*x14*x15^2*x17*x18*x20-x14^2*x15*x18^2*x20-
n*x14^2*x15*x18^2*x20-3*x13*x15*x17*x18^2*x20+x13*x14*x18^3*x20-
2*n*x13*x14*x18^3*x20-2*x14^2*x15*x17*x20^2-
2*n*x14^2*x15*x17*x20^2+2*x13*x15*x17^2*x20^2-
n*x13*x15*x17^2*x20^2+x14^3*x18*x20^2+n*x14^3*x18*x20^2-
x13*x14*x17*x18*x20^2+2*n*x13*x14*x17*x18*x20^2)-
x19^2*(x14*x15^4*x17+n*x14*x15^4*x17-2*x14^2*x15^3*x18+
n*x14^2*x15^3*x18+x13*x15^3*x17*x18-2*n*x13*x15^3*x17*x18-
x13*x14*x15^2*x18^2-n*x13*x14*x15^2*x18^2+x13^2*x15*x18^3+
n*x13^2*x15*x18^3+x14^3*x15^2*x20-2*n*x14^3*x15^2*x20-
3*x13*x14*x15^2*x17*x20+5*x13*x14^2*x15*x18*x20+
2*n*x13*x14^2*x15*x18*x20-x13^2*x15*x17*x18*x20+
2*n*x13^2*x15*x17*x18*x20-2*x13^2*x14*x18^2*x20-
2*n*x13^2*x14*x18^2*x20-2*x13*x14^3*x20^2+n*x13*x14^3*x20^2+
2*x13^2*x14*x17*x20^2-n*x13^2*x14*x17*x20^2)*x21-
x16*x19*(x15^4*x17^2-n*x15^4*x17^2-8*x14*x15^3*x17*x18+
12*x14^2*x15^2*x18^2-2*n*x14^2*x15^2*x18^2+2*x13*x15^2*x17*x18^2+
4*n*x13*x15^2*x17*x18^2-8*x13*x14*x15*x18^3+x13^2*x18^4-
n*x13^2*x18^4+2*x14^2*x15^2*x17*x20+4*n*x14^2*x15^2*x17*x20+
2*x13*x15^2*x17^2*x20-8*x14^3*x15*x18*x20-
8*n*x13*x14*x15*x17*x18*x20+2*x13*x14^2*x18^2*x20+
4*n*x13*x14^2*x18^2*x20+2*x13^2*x17*x18^2*x20+x14^4*x20^2-
n*x14^4*x20^2+2*x13*x14^2*x17*x20^2-3*x13^2*x17^2*x20^2+
n*x13^2*x17^2*x20^2)*x21-
x16^2*(x15^3*x17^2*x18+n*x15^3*x17^2*x18-x14*x15^2*x17*x18^2-
n*x14*x15^2*x17*x18^2-2*x14^2*x15*x18^3+n*x14^2*x15*x18^3+
x13*x15*x17*x18^3-2*n*x13*x15*x17*x18^3+x13*x14*x18^4+
n*x13*x14*x18^4-2*x14*x15^2*x17^2*x20-2*n*x14*x15^2*x17^2*x20+
5*x14^2*x15*x17*x18*x20+2*n*x14^2*x15*x17*x18*x20-
x13*x15*x17^2*x18*x20+2*n*x13*x15*x17^2*x18*x20+x14^3*x18^2*x20-
2*n*x14^3*x18^2*x20-3*x13*x14*x17*x18^2*x20-2*x14^3*x17*x20^2+
n*x14^3*x17*x20^2+2*x13*x14*x17^2*x20^2-n*x13*x14*x17^2*x20^2)*x21-
x19*(x14^2*x15^3*x17-2*n*x14^2*x15^3*x17-2*x13*x15^3*x17^2+
n*x13*x15^3*x17^2-2*x14^3*x15^2*x18+n*x14^3*x15^2*x18+
5*x13*x14*x15^2*x17*x18+2*n*x13*x14*x15^2*x17*x18-
x13*x14^2*x15*x18^2-n*x13*x14^2*x15*x18^2-2*x13^2*x15*x17*x18^2-
2*n*x13^2*x15*x17*x18^2+x13^2*x14*x18^3+n*x13^2*x14*x18^3+
x14^4*x15*x20+n*x14^4*x15*x20-3*x13*x14^2*x15*x17*x20+
2*x13^2*x15*x17^2*x20-n*x13^2*x15*x17^2*x20+x13*x14^3*x18*x20-
2*n*x13*x14^3*x18*x20-x13^2*x14*x17*x18*x20+
2*n*x13^2*x14*x17*x18*x20)*x21^2-
x16*(x14*x15^3*x17^2+n*x14*x15^3*x17^2-x14^2*x15^2*x17*x18-
n*x14^2*x15^2*x17*x18-2*x13*x15^2*x17^2*x18-
2*n*x13*x15^2*x17^2*x18-2*x14^3*x15*x18^2+n*x14^3*x15*x18^2+
5*x13*x14*x15*x17*x18^2+2*n*x13*x14*x15*x17*x18^2+
x13*x14^2*x18^3-2*n*x13*x14^2*x18^3-2*x13^2*x17*x18^3+
n*x13^2*x17*x18^3+x14^3*x15*x17*x20-2*n*x14^3*x15*x17*x20-
x13*x14*x15*x17^2*x20+2*n*x13*x14*x15*x17^2*x20+x14^4*x18*x20+
n*x14^4*x18*x20-3*x13*x14^2*x17*x18*x20+2*x13^2*x17^2*x18*x20-
n*x13^2*x17^2*x18*x20)*x21^2+
(x14^2-x13*x17)*(x14*x15^2*x17-n*x14*x15^2*x17-2*x14^2*x15*x18+
n*x14^2*x15*x18+n*x13*x15*x17*x18+x13*x14*x18^2-
n*x13*x14*x18^2+x14^3*x20-x13*x14*x17*x20)*x21^3-
x19*(x15^2*x17-2*x14*x15*x18+x13*x18^2+x14^2*x20-x13*x17*x20)*
(x15^3*x17+x14*x15^2*x18-2*x13*x15*x18^2-2*x14^2*x15*x20-
x13*x15*x17*x20+3*x13*x14*x18*x20)*x22+
x16*(x15^2*x17-2*x14*x15*x18+x13*x18^2+x14^2*x20-x13*x17*x20)*
(2*x15^2*x17*x18-x14*x15*x18^2-x13*x18^3-3*x14*x15*x17*x20+
2*x14^2*x18*x20+x13*x17*x18*x20)*x22+
(x15^2*x17-2*x14*x15*x18+x13*x18^2+x14^2*x20-x13*x17*x20)*
(2*x14*x15^2*x17-x14^2*x15*x18-3*x13*x15*x17*x18+2*x13*x14*x18^2-
x14^3*x20+x13*x14*x17*x20)*x21*x22
];
x72=Expand[
2*x14^2*x15^3*x17-n*x14^2*x15^3*x17+x13*x15^3*x17^2-4*x14^3*x15^2*x18+
n*x14^3*x15^2*x18-5*x13*x14*x15^2*x17*x18+2*n*x13*x14*x15^2*x17*x18+
8*x13*x14^2*x15*x18^2-2*n*x13*x14^2*x15*x18^2+x13^2*x15*x17*x18^2-
n*x13^2*x15*x17*x18^2-3*x13^2*x14*x18^3+n*x13^2*x14*x18^3+
2*x14^4*x15*x20-x13*x14^2*x15*x17*x20-x13^2*x15*x17^2*x20-
3*x13*x14^3*x18*x20+3*x13^2*x14*x17*x18*x20
];
x73=Expand[
2*x14*x15^4*x17-4*x14^2*x15^3*x18+n*x14^2*x15^3*x18-
3*x13*x15^3*x17*x18+8*x13*x14*x15^2*x18^2-2*n*x13*x14*x15^2*x18^2-
3*x13^2*x15*x18^3+n*x13^2*x15*x18^3+2*x14^3*x15^2*x20-
n*x14^3*x15^2*x20-x13*x14*x15^2*x17*x20-5*x13*x14^2*x15*x18*x20+
2*n*x13*x14^2*x15*x18*x20+3*x13^2*x15*x17*x18*x20+x13^2*x14*x18^2*x20-
n*x13^2*x14*x18^2*x20+x13*x14^3*x20^2-x13^2*x14*x17*x20^2
];
x74=Expand[
3*x14*x15^3*x17^2-n*x14*x15^3*x17^2-8*x14^2*x15^2*x17*x18+
2*n*x14^2*x15^2*x17*x18-x13*x15^2*x17^2*x18+n*x13*x15^2*x17^2*x18+
4*x14^3*x15*x18^2-n*x14^3*x15*x18^2+5*x13*x14*x15*x17*x18^2-
2*n*x13*x14*x15*x17*x18^2-2*x13*x14^2*x18^3+n*x13*x14^2*x18^3-
x13^2*x17*x18^3+3*x14^3*x15*x17*x20-3*x13*x14*x15*x17^2*x20-
2*x14^4*x18*x20+x13*x14^2*x17*x18*x20+x13^2*x17^2*x18*x20
];
x75=Expand[
2*x15^4*x17*x18-4*x14*x15^3*x18^2+n*x14*x15^3*x18^2+2*x13*x15^2*x18^3-
n*x13*x15^2*x18^3-3*x14*x15^3*x17*x20+8*x14^2*x15^2*x18*x20-
2*n*x14^2*x15^2*x18*x20-x13*x15^2*x17*x18*x20-5*x13*x14*x15*x18^2*x20+
2*n*x13*x14*x15*x18^2*x20+x13^2*x18^3*x20-3*x14^3*x15*x20^2+
n*x14^3*x15*x20^2+3*x13*x14*x15*x17*x20^2+x13*x14^2*x18*x20^2-
n*x13*x14^2*x18*x20^2-x13^2*x17*x18*x20^2
];
x76=Expand[
3*x15^3*x17^2*x18-n*x15^3*x17^2*x18-8*x14*x15^2*x17*x18^2+
2*n*x14*x15^2*x17*x18^2+4*x14^2*x15*x18^3-n*x14^2*x15*x18^3+
3*x13*x15*x17*x18^3-2*x13*x14*x18^4-x14*x15^2*x17^2*x20+
n*x14*x15^2*x17^2*x20+5*x14^2*x15*x17*x18*x20-
2*n*x14^2*x15*x17*x18*x20-3*x13*x15*x17^2*x18*x20-2*x14^3*x18^2*x20+
n*x14^3*x18^2*x20+x13*x14*x17*x18^2*x20-x14^3*x17*x20^2+
x13*x14*x17^2*x20^2
];
x77=Expand[
2*x15^3*x17*x18^2-n*x15^3*x17*x18^2-4*x14*x15^2*x18^3+
n*x14*x15^2*x18^3+2*x13*x15*x18^4+x15^3*x17^2*x20-
5*x14*x15^2*x17*x18*x20+2*n*x14*x15^2*x17*x18*x20+
8*x14^2*x15*x18^2*x20-2*n*x14^2*x15*x18^2*x20-x13*x15*x17*x18^2*x20-
3*x13*x14*x18^3*x20+x14^2*x15*x17*x20^2-n*x14^2*x15*x17*x20^2-
x13*x15*x17^2*x20^2-3*x14^3*x18*x20^2+n*x14^3*x18*x20^2+
3*x13*x14*x17*x18*x20^2
];
x78=Expand[
4*x14^2*x15^3*x17+n*x14^2*x15^3*x17-3*x13*x15^3*x17^2-
8*x14^3*x15^2*x18+n*x14^3*x15^2*x18+5*x13*x14*x15^2*x17*x18-
4*n*x13*x14*x15^2*x17*x18+6*x13*x14^2*x15*x18^2-3*x13^2*x15*x17*x18^2+
3*n*x13^2*x15*x17*x18^2-x13^2*x14*x18^3-n*x13^2*x14*x18^3+
4*x14^4*x15*x20-2*n*x14^4*x15*x20-7*x13*x14^2*x15*x17*x20+
2*n*x13*x14^2*x15*x17*x20+3*x13^2*x15*x17^2*x20-x13*x14^3*x18*x20+
2*n*x13*x14^3*x18*x20+x13^2*x14*x17*x18*x20-2*n*x13^2*x14*x17*x18*x20
];
x79=Expand[
4*x14*x15^4*x17-2*n*x14*x15^4*x17-8*x14^2*x15^3*x18+n*x14^2*x15^3*x18-
x13*x15^3*x17*x18+2*n*x13*x15^3*x17*x18+6*x13*x14*x15^2*x18^2-
x13^2*x15*x18^3-n*x13^2*x15*x18^3+4*x14^3*x15^2*x20+
n*x14^3*x15^2*x20-7*x13*x14*x15^2*x17*x20+2*n*x13*x14*x15^2*x17*x20+
5*x13*x14^2*x15*x18*x20-4*n*x13*x14^2*x15*x18*x20+
x13^2*x15*x17*x18*x20-2*n*x13^2*x15*x17*x18*x20-3*x13^2*x14*x18^2*x20+
3*n*x13^2*x14*x18^2*x20-3*x13*x14^3*x20^2+3*x13^2*x14*x17*x20^2
];
x80=Expand[
x14*x15^3*x17^2+n*x14*x15^3*x17^2-6*x14^2*x15^2*x17*x18+
3*x13*x15^2*x17^2*x18-3*n*x13*x15^2*x17^2*x18+8*x14^3*x15*x18^2-
n*x14^3*x15*x18^2-5*x13*x14*x15*x17*x18^2+4*n*x13*x14*x15*x17*x18^2-
4*x13*x14^2*x18^3-n*x13*x14^2*x18^3+3*x13^2*x17*x18^3+
x14^3*x15*x17*x20-2*n*x14^3*x15*x17*x20-x13*x14*x15*x17^2*x20+
2*n*x13*x14*x15*x17^2*x20-4*x14^4*x18*x20+2*n*x14^4*x18*x20+
7*x13*x14^2*x17*x18*x20-2*n*x13*x14^2*x17*x18*x20-3*x13^2*x17^2*x18*x20
];
x81=Expand[
4*x15^4*x17*x18-2*n*x15^4*x17*x18-8*x14*x15^3*x18^2+n*x14*x15^3*x18^2+
4*x13*x15^2*x18^3+n*x13*x15^2*x18^3-x14*x15^3*x17*x20+
2*n*x14*x15^3*x17*x20+6*x14^2*x15^2*x18*x20-7*x13*x15^2*x17*x18*x20+
2*n*x13*x15^2*x17*x18*x20+5*x13*x14*x15*x18^2*x20-
4*n*x13*x14*x15*x18^2*x20-3*x13^2*x18^3*x20-x14^3*x15*x20^2-
n*x14^3*x15*x20^2+x13*x14*x15*x17*x20^2-2*n*x13*x14*x15*x17*x20^2-
3*x13*x14^2*x18*x20^2+3*n*x13*x14^2*x18*x20^2+3*x13^2*x17*x18*x20^2
];
x82=Expand[
x15^3*x17^2*x18+n*x15^3*x17^2*x18-6*x14*x15^2*x17*x18^2+
8*x14^2*x15*x18^3-n*x14^2*x15*x18^3+x13*x15*x17*x18^3-
2*n*x13*x15*x17*x18^3-4*x13*x14*x18^4+2*n*x13*x14*x18^4+
3*x14*x15^2*x17^2*x20-3*n*x14*x15^2*x17^2*x20-5*x14^2*x15*x17*x18*x20+
4*n*x14^2*x15*x17*x18*x20-x13*x15*x17^2*x18*x20+
2*n*x13*x15*x17^2*x18*x20-4*x14^3*x18^2*x20-n*x14^3*x18^2*x20+
7*x13*x14*x17*x18^2*x20-2*n*x13*x14*x17*x18^2*x20+3*x14^3*x17*x20^2-
3*x13*x14*x17^2*x20^2
];
x83=Expand[
4*x15^3*x17*x18^2+n*x15^3*x17*x18^2-8*x14*x15^2*x18^3+
n*x14*x15^2*x18^3+4*x13*x15*x18^4-2*n*x13*x15*x18^4-
3*x15^3*x17^2*x20+5*x14*x15^2*x17*x18*x20-4*n*x14*x15^2*x17*x18*x20+
6*x14^2*x15*x18^2*x20-7*x13*x15*x17*x18^2*x20+
2*n*x13*x15*x17*x18^2*x20-x13*x14*x18^3*x20+2*n*x13*x14*x18^3*x20-
3*x14^2*x15*x17*x20^2+3*n*x14^2*x15*x17*x20^2+3*x13*x15*x17^2*x20^2-
x14^3*x18*x20^2-n*x14^3*x18*x20^2+x13*x14*x17*x18*x20^2-
2*n*x13*x14*x17*x18*x20^2
];
x84=Expand[
x14^2*x15^3*x17-2*n*x14^2*x15^3*x17-2*x13*x15^3*x17^2+
n*x13*x15^3*x17^2-2*x14^3*x15^2*x18+n*x14^3*x15^2*x18+
5*x13*x14*x15^2*x17*x18+2*n*x13*x14*x15^2*x17*x18-x13*x14^2*x15*x18^2-
n*x13*x14^2*x15*x18^2-2*x13^2*x15*x17*x18^2-2*n*x13^2*x15*x17*x18^2+
x13^2*x14*x18^3+n*x13^2*x14*x18^3+x14^4*x15*x20+n*x14^4*x15*x20-
3*x13*x14^2*x15*x17*x20+2*x13^2*x15*x17^2*x20-n*x13^2*x15*x17^2*x20+
x13*x14^3*x18*x20-2*n*x13*x14^3*x18*x20-x13^2*x14*x17*x18*x20+
2*n*x13^2*x14*x17*x18*x20
];
x85=Expand[
x14*x15^4*x17+n*x14*x15^4*x17-2*x14^2*x15^3*x18+n*x14^2*x15^3*x18+
x13*x15^3*x17*x18-2*n*x13*x15^3*x17*x18-x13*x14*x15^2*x18^2-
n*x13*x14*x15^2*x18^2+x13^2*x15*x18^3+n*x13^2*x15*x18^3+
x14^3*x15^2*x20-2*n*x14^3*x15^2*x20-3*x13*x14*x15^2*x17*x20+
5*x13*x14^2*x15*x18*x20+2*n*x13*x14^2*x15*x18*x20-
x13^2*x15*x17*x18*x20+2*n*x13^2*x15*x17*x18*x20-2*x13^2*x14*x18^2*x20-
2*n*x13^2*x14*x18^2*x20-2*x13*x14^3*x20^2+n*x13*x14^3*x20^2+
2*x13^2*x14*x17*x20^2-n*x13^2*x14*x17*x20^2
];
x86=Expand[
x14*x15^3*x17^2+n*x14*x15^3*x17^2-x14^2*x15^2*x17*x18-
n*x14^2*x15^2*x17*x18-2*x13*x15^2*x17^2*x18-2*n*x13*x15^2*x17^2*x18-
2*x14^3*x15*x18^2+n*x14^3*x15*x18^2+5*x13*x14*x15*x17*x18^2+
2*n*x13*x14*x15*x17*x18^2+x13*x14^2*x18^3-2*n*x13*x14^2*x18^3-
2*x13^2*x17*x18^3+n*x13^2*x17*x18^3+x14^3*x15*x17*x20-
2*n*x14^3*x15*x17*x20-x13*x14*x15*x17^2*x20+2*n*x13*x14*x15*x17^2*x20+
x14^4*x18*x20+n*x14^4*x18*x20-3*x13*x14^2*x17*x18*x20+
2*x13^2*x17^2*x18*x20-n*x13^2*x17^2*x18*x20
];
x87=Expand[
x15^4*x17^2-n*x15^4*x17^2-8*x14*x15^3*x17*x18+12*x14^2*x15^2*x18^2-
2*n*x14^2*x15^2*x18^2+2*x13*x15^2*x17*x18^2+4*n*x13*x15^2*x17*x18^2-
8*x13*x14*x15*x18^3+x13^2*x18^4-n*x13^2*x18^4+2*x14^2*x15^2*x17*x20+
4*n*x14^2*x15^2*x17*x20+2*x13*x15^2*x17^2*x20-8*x14^3*x15*x18*x20-
8*n*x13*x14*x15*x17*x18*x20+2*x13*x14^2*x18^2*x20+
4*n*x13*x14^2*x18^2*x20+2*x13^2*x17*x18^2*x20+x14^4*x20^2-
n*x14^4*x20^2+2*x13*x14^2*x17*x20^2-3*x13^2*x17^2*x20^2+
n*x13^2*x17^2*x20^2
];
x88=Expand[
2*x15^4*x17^2-6*x14*x15^3*x17*x18+2*n*x14*x15^3*x17*x18+
4*x14^2*x15^2*x18^2-n*x14^2*x15^2*x18^2-x13*x15^2*x17*x18^2-
2*n*x13*x15^2*x17*x18^2+4*x13*x14*x15*x18^3-3*x13^2*x18^4+
n*x13^2*x18^4+4*x14^2*x15^2*x17*x20-3*n*x14^2*x15^2*x17*x20-
x13*x15^2*x17^2*x20-6*x14^3*x15*x18*x20+2*n*x14^3*x15*x18*x20+
4*n*x13*x14*x15*x17*x18*x20-x13*x14^2*x18^2*x20-
2*n*x13*x14^2*x18^2*x20+4*x13^2*x17*x18^2*x20-n*x13^2*x17*x18^2*x20+
2*x14^4*x20^2-x13*x14^2*x17*x20^2-x13^2*x17^2*x20^2
];
x89=Expand[
2*x15^4*x17^2-6*x14*x15^3*x17*x18+2*n*x14*x15^3*x17*x18+
4*x14^2*x15^2*x18^2-n*x14^2*x15^2*x18^2+4*x13*x15^2*x17*x18^2-
3*n*x13*x15^2*x17*x18^2-6*x13*x14*x15*x18^3+2*n*x13*x14*x15*x18^3+
2*x13^2*x18^4-x14^2*x15^2*x17*x20-2*n*x14^2*x15^2*x17*x20-
x13*x15^2*x17^2*x20+4*x14^3*x15*x18*x20+4*n*x13*x14*x15*x17*x18*x20-
x13*x14^2*x18^2*x20-2*n*x13*x14^2*x18^2*x20-x13^2*x17*x18^2*x20-
3*x14^4*x20^2+n*x14^4*x20^2+4*x13*x14^2*x17*x20^2-
n*x13*x14^2*x17*x20^2-x13^2*x17^2*x20^2
];
x90=Expand[
3*x15^4*x17^2-n*x15^4*x17^2-4*x14*x15^3*x17*x18-4*x14^2*x15^2*x18^2+
n*x14^2*x15^2*x18^2+x13*x15^2*x17*x18^2+2*n*x13*x15^2*x17*x18^2+
6*x13*x14*x15*x18^3-2*n*x13*x14*x15*x18^3-2*x13^2*x18^4+
x14^2*x15^2*x17*x20+2*n*x14^2*x15^2*x17*x20-4*x13*x15^2*x17^2*x20+
n*x13*x15^2*x17^2*x20+6*x14^3*x15*x18*x20-2*n*x14^3*x15*x18*x20-
4*n*x13*x14*x15*x17*x18*x20-4*x13*x14^2*x18^2*x20+
3*n*x13*x14^2*x18^2*x20+x13^2*x17*x18^2*x20-2*x14^4*x20^2+
x13*x14^2*x17*x20^2+x13^2*x17^2*x20^2
];
x91=Expand[
x15^4*x17*x18+n*x15^4*x17*x18-2*x14*x15^3*x18^2+n*x14*x15^3*x18^2+
x13*x15^2*x18^3-2*n*x13*x15^2*x18^3+x14*x15^3*x17*x20-
2*n*x14*x15^3*x17*x20-x14^2*x15^2*x18*x20-n*x14^2*x15^2*x18*x20-
3*x13*x15^2*x17*x18*x20+5*x13*x14*x15*x18^2*x20+
2*n*x13*x14*x15*x18^2*x20-2*x13^2*x18^3*x20+n*x13^2*x18^3*x20+
x14^3*x15*x20^2+n*x14^3*x15*x20^2-x13*x14*x15*x17*x20^2+
2*n*x13*x14*x15*x17*x20^2-2*x13*x14^2*x18*x20^2-
2*n*x13*x14^2*x18*x20^2+2*x13^2*x17*x18*x20^2-n*x13^2*x17*x18*x20^2
];
x92=Expand[
x15^3*x17^2*x18+n*x15^3*x17^2*x18-x14*x15^2*x17*x18^2-
n*x14*x15^2*x17*x18^2-2*x14^2*x15*x18^3+n*x14^2*x15*x18^3+
x13*x15*x17*x18^3-2*n*x13*x15*x17*x18^3+x13*x14*x18^4+
n*x13*x14*x18^4-2*x14*x15^2*x17^2*x20-2*n*x14*x15^2*x17^2*x20+
5*x14^2*x15*x17*x18*x20+2*n*x14^2*x15*x17*x18*x20-
x13*x15*x17^2*x18*x20+2*n*x13*x15*x17^2*x18*x20+x14^3*x18^2*x20-
2*n*x14^3*x18^2*x20-3*x13*x14*x17*x18^2*x20-2*x14^3*x17*x20^2+
n*x14^3*x17*x20^2+2*x13*x14*x17^2*x20^2-n*x13*x14*x17^2*x20^2
];
x93=Expand[
x15^3*x17*x18^2-2*n*x15^3*x17*x18^2-2*x14*x15^2*x18^3+
n*x14*x15^2*x18^3+x13*x15*x18^4+n*x13*x15*x18^4-2*x15^3*x17^2*x20+
n*x15^3*x17^2*x20+5*x14*x15^2*x17*x18*x20+2*n*x14*x15^2*x17*x18*x20-
x14^2*x15*x18^2*x20-n*x14^2*x15*x18^2*x20-3*x13*x15*x17*x18^2*x20+
x13*x14*x18^3*x20-2*n*x13*x14*x18^3*x20-2*x14^2*x15*x17*x20^2-
2*n*x14^2*x15*x17*x20^2+2*x13*x15*x17^2*x20^2-n*x13*x15*x17^2*x20^2+
x14^3*x18*x20^2+n*x14^3*x18*x20^2-x13*x14*x17*x18*x20^2+
2*n*x13*x14*x17*x18*x20^2
];
(x1*x11*x30*x40)/(x23*x33^2)+(x1*x12*x31*x40)/(x23*x33^2)+
(x1*x10*x32*x40)/(x23*x33^2)+(x3*x32*x40*x6)/(x23*x33^2)+
(x30*x4*x40*x6)/(x23*x33^2)+(x31*x40*x5*x6)/(x23*x33^2)+
(x2*x32*x40*x7)/(x23*x33^2)+(x10*x3*x32*x42*x7)/(x23*x33^3)+
(x11*x4*x66*x7)/(x23*x33^3)+(x12*x3*x67*x7)/(x23*x33^3)+
(x10*x5*x67*x7)/(x23*x33^3)+(x11*x3*x68*x7)/(x23*x33^3)+
(x10*x4*x68*x7)/(x23*x33^3)+(x12*x5*x7*x70)/(x23*x33^3)+
(x12*x4*x7*x71)/(x23*x33^3)+(x11*x5*x7*x71)/(x23*x33^3)+
(x2*x30*x40*x8)/(x23*x33^2)+(x11*x30*x4*x41*x8)/(x23*x33^3)+
(x12*x4*x65*x8)/(x23*x33^3)+(x11*x5*x65*x8)/(x23*x33^3)+
(x11*x3*x66*x8)/(x23*x33^3)+(x10*x4*x66*x8)/(x23*x33^3)+
(x10*x3*x68*x8)/(x23*x33^3)+(x12*x5*x69*x8)/(x23*x33^3)+
(x12*x3*x71*x8)/(x23*x33^3)+(x10*x5*x71*x8)/(x23*x33^3)+
(x2*x31*x40*x9)/(x23*x33^2)+(x12*x31*x43*x5*x9)/(x23*x33^3)+
(x11*x4*x65*x9)/(x23*x33^3)+(x10*x3*x67*x9)/(x23*x33^3)+
(x12*x4*x69*x9)/(x23*x33^3)+(x11*x5*x69*x9)/(x23*x33^3)+
(x12*x3*x70*x9)/(x23*x33^3)+(x10*x5*x70*x9)/(x23*x33^3)+
(x11*x3*x71*x9)/(x23*x33^3)+(x10*x4*x71*x9)/(x23*x33^3)
],
(* Dmunurhosigma *)
tidl[{{q_,mu_}, {q_,nu_},{q_,rho_}, {q_,si_}},{p1_,p2_,p3_},n_] :>
Block[{X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,spe},
spe[a_,b_] := spe[a,b] = Expand[ExpandScalarProduct[
  Pair[Momentum[a,n],Momentum[b,n]]]];
ChangeDimension[
(
((3*X4^4*X6^2*X8^4*X9^2-12*X3*X4^3*X6*X7*X8^4*X9^2-
12*X3^3*X4*X7*X8^4*X9^3+3*X3^4*X8^4*X9^4+
3*X2^2*X8^4*X9^2*(X7^2-X6*X9)^2-
6*X2*X8^4*X9^2*(X4^2*X6-2*X3*X4*X7+X3^2*X9)*(-X7^2+X6*X9)+
6*X3^2*X4^2*X8^4*X9^2*(2*X7^2+X6*X9)+
X1^2*(X10*X4*X6-X10*X3*X7+X5*X7^2-X4*X7*X8-X5*X6*X9+
X3*X8*X9)^4+3*X10^4*X6^2*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2-
12*X10^3*X6*X7*X8*(X4^2*X6-2*X3*X4*X7+X3^2*X9+
X2*(X7^2-X6*X9))^2+
3*X11^2*(X7^2-X6*X9)^2*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2-
6*X11*X8^2*X9*(-X7^2+X6*X9)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2-
12*X10*X7*X8*(X8^2*X9+X11*(X7^2-X6*X9))*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2+
6*X10^2*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2*
(X11*X6*(X7^2-X6*X9)+X8^2*(2*X7^2+X6*X9))-
2*X1*(X10*X4*X6-X10*X3*X7+X5*X7^2-X4*X7*X8-X5*X6*X9+
X3*X8*X9)^2*(-(X7^2*(X5*X7-X4*X8)^2)-
2*X10*X7*(2*X4^2*X6*X8+X7^2*(-(X3*X5)+3*X2*X8)+
X4*X7*(X5*X6-5*X3*X8))+
(2*X5^2*X6*X7^2-2*X5*X7*(X4*X6+X3*X7)*X8+
(3*X4^2*X6-4*X3*X4*X7+3*X2*X7^2)*X8^2)*X9+
2*X10*(X4*X6*(X5*X6-X3*X8)+
X7*(-(X3*X5*X6)-2*X3^2*X8+3*X2*X6*X8))*X9-
(X5^2*X6^2-2*X3*X5*X6*X8+(-2*X3^2+3*X2*X6)*X8^2)*X9^2+
3*X11*(X7^2-X6*X9)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))+
X10^2*(2*X4^2*X6^2-4*X3*X4*X6*X7+3*X2*X6*(X7^2-X6*X9)+
X3^2*(-X7^2+3*X6*X9))))*FV[p1,mu]*FV[p1,nu]*FV[p1,rho]*
FV[p1,si])/
((1-X1)*(3-X1)*(X4^2*X6-2*X3*X4*X7+X2*X7^2+X3^2*X9-X2*X6*X9)^
4)+((3*X4^4*X5^4*X6^2*X9^2-12*X3*X4^3*X5^4*X6*X7*X9^2-
12*X3^3*X4*X5^4*X7*X9^3+3*X3^4*X5^4*X9^4+
3*X2^2*X5^4*X9^2*(X7^2-X6*X9)^2-
6*X2*X5^4*X9^2*(X4^2*X6-2*X3*X4*X7+X3^2*X9)*(-X7^2+X6*X9)+
6*X3^2*X4^2*X5^4*X9^2*(2*X7^2+X6*X9)+
X1^2*(X10*X3*X4-X10*X2*X7+X4*X5*X7-X4^2*X8-X3*X5*X9+
X2*X8*X9)^4+3*X10^4*X2^2*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2-
12*X10^3*X2*X4*X5*(X4^2*X6-2*X3*X4*X7+X3^2*X9+
X2*(X7^2-X6*X9))^2+
3*X11^2*(X4^2-X2*X9)^2*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2-
6*X11*X5^2*X9*(-X4^2+X2*X9)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2-
12*X10*X4*X5*(X5^2*X9+X11*(X4^2-X2*X9))*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2+
6*X10^2*(X11*X2*(X4^2-X2*X9)+X5^2*(2*X4^2+X2*X9))*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2+
2*X1*(X10*X3*X4-X10*X2*X7+X4*X5*X7-X4^2*X8-X3*X5*X9+
X2*X8*X9)^2*(X4^2*(X5*X7-X4*X8)^2-
(X5^2*(3*X4^2*X6-4*X3*X4*X7+3*X2*X7^2)-
2*X4*X5*(X3*X4+X2*X7)*X8+2*X2*X4^2*X8^2)*X9+
(X5^2*(-2*X3^2+3*X2*X6)-2*X2*X3*X5*X8+X2^2*X8^2)*X9^2-
3*X11*(X4^2-X2*X9)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))+
X10^2*(4*X2*X3*X4*X7+X3^2*(X4^2-3*X2*X9)+
X2*(-3*X4^2*X6-2*X2*X7^2+3*X2*X6*X9))+
2*X10*(X4^3*(3*X5*X6-X3*X8)+X4^2*(-5*X3*X5*X7+X2*X7*X8)+
X2*X7*(X3*X5-X2*X8)*X9+
X4*(2*X3^2*X5*X9+X2*(2*X5*X7^2-3*X5*X6*X9+X3*X8*X9)))))*
FV[p2,mu]*FV[p2,nu]*FV[p2,rho]*FV[p2,si])/
((1-X1)*(3-X1)*(X4^2*X6-2*X3*X4*X7+X2*X7^2+X3^2*X9-X2*X6*X9)^
4)+((-3*X4^4*X5*X6^2*X8^3*X9^2+12*X3*X4^3*X5*X6*X7*X8^3*X9^2+
12*X3^3*X4*X5*X7*X8^3*X9^3-3*X3^4*X5*X8^3*X9^4-
3*X2^2*X5*X8^3*X9^2*(X7^2-X6*X9)^2+
6*X2*X5*X8^3*X9^2*(X4^2*X6-2*X3*X4*X7+X3^2*X9)*
(-X7^2+X6*X9)-6*X3^2*X4^2*X5*X8^3*X9^2*(2*X7^2+X6*X9)+
X1^2*(X10*X3*X4-X10*X2*X7+X4*X5*X7-X4^2*X8-X3*X5*X9+
X2*X8*X9)*(-(X10*X4*X6)+X10*X3*X7-X5*X7^2+X4*X7*X8+
X5*X6*X9-X3*X8*X9)^3-
3*X10^4*X3*X6*(X4^2*X6-2*X3*X4*X7+X3^2*X9+
X2*(X7^2-X6*X9))^2+
3*X10^3*(X5*X6*X7+X4*X6*X8+2*X3*X7*X8)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2-
3*X11^2*(X4*X7-X3*X9)*(X7^2-X6*X9)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2+
3*X11*X8*X9*(-(X7*(X5*X7+X4*X8))+(X5*X6+X3*X8)*X9)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2-
3*X10*(-(X11*X7^2*(X5*X7+3*X4*X8))-
X8^2*(3*X5*X7+X4*X8)*X9+
X11*(X5*X6*X7+X4*X6*X8+2*X3*X7*X8)*X9)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2-
3*X10^2*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2*
(X8*(2*X5*X7^2+2*X4*X7*X8+X5*X6*X9+X3*X8*X9)+
X11*(X4*X6*X7+X3*(X7^2-2*X6*X9)))+
X1*(X10*X4*X6-X10*X3*X7+X5*X7^2-X4*X7*X8-X5*X6*X9+
X3*X8*X9)*(-2*X4*X7^2*(X5*X7-X4*X8)^3+
(X5*X7-X4*X8)*(2*X5^2*X7^2*(2*X4*X6+X3*X7)-
X5*X7*(X4^2*X6+12*X3*X4*X7-X2*X7^2)*X8+
X4*(3*X4^2*X6-2*X3*X4*X7+5*X2*X7^2)*X8^2)*X9-
(2*X5^3*X6*X7*(X4*X6+2*X3*X7)+
X5^2*(X4^2*X6^2-14*X3*X4*X6*X7+
(-7*X3^2+2*X2*X6)*X7^2)*X8+
2*X5*(3*X3^2*X4*X7+2*X2*X4*X6*X7+
2*X3*(X4^2*X6+X2*X7^2))*X8^2-
(X4^2*(-X3^2+6*X2*X6)-2*X2*X3*X4*X7+3*X2^2*X7^2)*X8^3
)*X9^2+(2*X3*X5^3*X6^2+X5^2*X6*(-7*X3^2+X2*X6)*X8+
2*X3*X5*(X3^2+2*X2*X6)*X8^2-X2*(-X3^2+3*X2*X6)*X8^3)*
X9^3+X10^2*(2*(-2*X4^4*X6^2*X8+
X2*X7^4*(X3*X5+3*X2*X8)+
X4^3*X6*X7*(-(X5*X6)+3*X3*X8)+
X4*X7^3*(-5*X3^2*X5+2*X2*X5*X6-7*X2*X3*X8)+
X4^2*X7^2*(3*X3*X5*X6+5*X3^2*X8-2*X2*X6*X8))+
(X3^2-X2*X6)*(-7*X4^2*X6*X8+
X7^2*(8*X3*X5+3*X2*X8)+4*X4*X7*(X5*X6-2*X3*X8))*X9\
+3*(X3^2-X2*X6)*(-2*X3*X5*X6+X3^2*X8+X2*X6*X8)*X9^2)+
3*X11*(2*X4*X7^2*(X5*X7-X4*X8)+
(-2*X5*X7*(X4*X6+X3*X7)+
(X4^2*X6+2*X3*X4*X7+X2*X7^2)*X8)*X9+
(2*X3*X5*X6-(X3^2+X2*X6)*X8)*X9^2)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))+
X10^3*(-(X3^2*X7*(11*X4^2*X6+X2*X7^2))-3*X3^4*X7*X9+
X3^3*X4*(4*X7^2+6*X6*X9)+
2*X3*X4*X6*(2*X4^2*X6+4*X2*X7^2-3*X2*X6*X9)+
X2*X6*X7*(-(X4^2*X6)-3*X2*X7^2+3*X2*X6*X9))+
X10*(-(X7*(X5*X7-X4*X8)*
(X2*X5*X7^3+5*X4^3*X6*X8+
X4*X7^2*(-8*X3*X5+11*X2*X8)+
X4^2*X7*(7*X5*X6-16*X3*X8)))+
(2*X4^3*X6*X8*(X5*X6+2*X3*X8)+
X7^3*(X5^2*(-7*X3^2+2*X2*X6)+8*X2*X3*X5*X8-
9*X2^2*X8^2)+
2*X4*X7^2*(-(X3*X5^2*X6)-X3^2*X5*X8+
6*X2*X5*X6*X8+5*X2*X3*X8^2)+
X4^2*X7*(7*X5^2*X6^2-20*X3*X5*X6*X8+9*X3^2*X8^2-
14*X2*X6*X8^2))*X9-
(X5^2*X6*(6*X3*X4*X6-7*X3^2*X7+X2*X6*X7)+
2*X5*(X4*X6*(-7*X3^2+X2*X6)+
2*X3*(X3^2+2*X2*X6)*X7)*X8+
(2*X3^3*X4+4*X2*X3*X4*X6+3*X2*X3^2*X7-
9*X2^2*X6*X7)*X8^2)*X9^2+
3*X11*(X4^2*X6*X7-X2*X7^3-2*X3*X4*X6*X9+
(X3^2+X2*X6)*X7*X9)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9)))))*
(FV[p1,nu]*FV[p1,rho]*FV[p1,si]*FV[p2,mu]+
FV[p1,mu]*FV[p1,rho]*FV[p1,si]*FV[p2,nu]+
FV[p1,mu]*FV[p1,nu]*FV[p1,si]*FV[p2,rho]+
FV[p1,mu]*FV[p1,nu]*FV[p1,rho]*FV[p2,si]))/
((1-X1)*(3-X1)*(X4^2*X6-2*X3*X4*X7+X2*X7^2+X3^2*X9-X2*X6*X9)^
4)+((3*X4^4*X5^2*X6^2*X8^2*X9^2-
12*X3*X4^3*X5^2*X6*X7*X8^2*X9^2-12*X3^3*X4*X5^2*X7*X8^2*X9^3+
3*X3^4*X5^2*X8^2*X9^4+3*X2^2*X5^2*X8^2*X9^2*(X7^2-X6*X9)^2-
6*X2*X5^2*X8^2*X9^2*(X4^2*X6-2*X3*X4*X7+X3^2*X9)*
(-X7^2+X6*X9)+6*X3^2*X4^2*X5^2*X8^2*X9^2*(2*X7^2+X6*X9)+
X1^2*(X10*X3*X4-X10*X2*X7+X4*X5*X7-X4^2*X8-X3*X5*X9+
X2*X8*X9)^2*(X10*X4*X6-X10*X3*X7+X5*X7^2-X4*X7*X8-
X5*X6*X9+X3*X8*X9)^2+
X10^4*(2*X3^2+X2*X6)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2-
2*X10^3*(X4*X5*X6+2*X3*X5*X7+2*X3*X4*X8+X2*X7*X8)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2-
2*X10*(3*X11*X4*X7*(X5*X7+X4*X8)+
3*X5*X8*(X5*X7+X4*X8)*X9-
X11*(X4*X5*X6+2*X3*X5*X7+2*X3*X4*X8+X2*X7*X8)*X9)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2-
X11*X9*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2*
(X8^2*(-X4^2+X2*X9)+X5^2*(-X7^2+X6*X9)+
X5*(-4*X4*X7*X8+4*X3*X8*X9))+
X10^2*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2*
(X8^2*(2*X4^2+X2*X9)+4*X5*X8*(2*X4*X7+X3*X9)+
X5^2*(2*X7^2+X6*X9)+
X11*(X4^2*X6+4*X3*X4*X7-4*X3^2*X9+X2*(X7^2-2*X6*X9)))-
X11^2*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2*
(4*X3*X4*X7*X9+X4^2*(-3*X7^2+X6*X9)+
X9*(-2*X3^2*X9+X2*(X7^2-X6*X9)))+
X1*(2*X4^2*X7^2*(X5*X7-X4*X8)^4-
(X5*X7-X4*X8)^2*(X5^2*X7^2*
(5*X4^2*X6+2*X3*X4*X7+X2*X7^2)-
16*X3*X4^2*X5*X7^2*X8+
X4^2*(X4^2*X6+2*X3*X4*X7+5*X2*X7^2)*X8^2)*X9+
(X5*X7-X4*X8)*(X5^3*X7*
(4*X4^2*X6^2+4*X3*X4*X6*X7+(X3^2+3*X2*X6)*X7^2)-
X5^2*(-2*X4^3*X6^2+18*X3*X4^2*X6*X7+
X4*(17*X3^2+X2*X6)*X7^2+2*X2*X3*X7^3)*X8+
X5*(2*X3*X4^3*X6+X4^2*(17*X3^2+X2*X6)*X7+
18*X2*X3*X4*X7^2-2*X2^2*X7^3)*X8^2-
X4*(X4^2*(X3^2+3*X2*X6)+4*X2*X3*X4*X7+4*X2^2*X7^2)*
X8^3)*X9^2-(X5^4*X6*
(X4^2*X6^2+2*X3*X4*X6*X7+(2*X3^2+3*X2*X6)*X7^2)-
2*X5^3*(X3*X4^2*X6^2+X4*X6*(9*X3^2+X2*X6)*X7+
X3*(3*X3^2+2*X2*X6)*X7^2)*X8-
2*X5^2*(X4^2*X6*(-5*X3^2+2*X2*X6)-
2*X3*X4*(4*X3^2+5*X2*X6)*X7+
X2*(-5*X3^2+2*X2*X6)*X7^2)*X8^2-
2*X5*(X3*X4^2*(3*X3^2+2*X2*X6)+
X2*X4*(9*X3^2+X2*X6)*X7+X2^2*X3*X7^2)*X8^3+
X2*(X4^2*(2*X3^2+3*X2*X6)+2*X2*X3*X4*X7+X2^2*X7^2)*
X8^4)*X9^3+(X5^4*X6^2*(X3^2+X2*X6)-
2*X3*X5^3*X6*(3*X3^2+X2*X6)*X8-
2*X5^2*(-2*X3^4-5*X2*X3^2*X6+X2^2*X6^2)*X8^2-
2*X2*X3*X5*(3*X3^2+X2*X6)*X8^3+X2^2*(X3^2+X2*X6)*X8^4
)*X9^4+X10^4*(-3*X3^2*X4^4*X6^2-X2*X4^4*X6^3+10*X3^3*X4^3*X6*X7+
6*X2*X3*X4^3*X6^2*X7-6*X3^4*X4^2*X7^2-
18*X2*X3^2*X4^2*X6*X7^2+10*X2*X3^3*X4*X7^3+
6*X2^2*X3*X4*X6*X7^3-3*X2^2*X3^2*X7^4-X2^3*X6*X7^4+
(-X3^2+X2*X6)*
(X4^2*X6*(5*X3^2+X2*X6)-4*X3*X4*(X3^2+2*X2*X6)*X7+
X2*(5*X3^2+X2*X6)*X7^2)*X9)+
X11*(-6*X4^2*X7^2*(X5*X7-X4*X8)^2+
(X5*X7-X4*X8)*
(X5*X7*(7*X4^2*X6+10*X3*X4*X7+X2*X7^2)-
X4*(X4^2*X6+10*X3*X4*X7+7*X2*X7^2)*X8)*X9-
(X5^2*(X4^2*X6^2+10*X3*X4*X6*X7+
(5*X3^2+2*X2*X6)*X7^2)-
4*X5*(3*X3^2*X4*X7+2*X2*X4*X6*X7+
2*X3*(X4^2*X6+X2*X7^2))*X8+
(X4^2*(5*X3^2+2*X2*X6)+10*X2*X3*X4*X7+
X2^2*X7^2)*X8^2)*X9^2+
(X5^2*X6*(5*X3^2+X2*X6)-4*X3*X5*(X3^2+2*X2*X6)*X8+
X2*(5*X3^2+X2*X6)*X8^2)*X9^3)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))+
2*X10^3*(X2^2*X7^5*(3*X3*X5+X2*X8)+
X4^5*X6^2*(X5*X6+3*X3*X8)-
3*X3*X4^4*X6*X7*(X5*X6+3*X3*X8)-
X2*X3*X7^3*(-5*X3^2*X5+7*X2*X5*X6-2*X2*X3*X8)*X9+
(-X3^2+X2*X6)*X7*
(2*X3*X5*(X3^2+2*X2*X6)-X2*(5*X3^2+X2*X6)*X8)*X9^2+
X4^3*(X7^2*(3*X3^2*X5*X6-3*X2*X5*X6^2+2*X3^3*X8+
6*X2*X3*X6*X8)+
X3*X6*(2*X3*X5*X6+5*X3^2*X8-7*X2*X6*X8)*X9)+
X4^2*X7*(X7^2*(2*X3^3*X5+6*X2*X3*X5*X6+3*X2*X3^2*X8-
3*X2^2*X6*X8)+
(3*X3*X5*X6*(-3*X3^2+X2*X6)+
(X3^4+4*X2*X3^2*X6+X2^2*X6^2)*X8)*X9)+
X4*(-3*X2*X3*X7^4*(3*X3*X5+X2*X8)+
X7^2*(X5*(X3^4+4*X2*X3^2*X6+X2^2*X6^2)+
3*X2*X3*(-3*X3^2+X2*X6)*X8)*X9+
(-X3^2+X2*X6)*
(-(X5*X6*(5*X3^2+X2*X6))+2*X3*(X3^2+2*X2*X6)*X8)*
X9^2))+2*X10*
(X4*X7*(X5*X7-X4*X8)^2*
(X2*X5*X7^3+X4^3*X6*X8+X4*X7^2*(-6*X3*X5+5*X2*X8)+
X4^2*X7*(5*X5*X6-6*X3*X8))+
(X5*X7-X4*X8)*
(X2*X5*X7^4*(X3*X5+2*X2*X8)+
X4^4*X6*X8*(-2*X5*X6-X3*X8)+
3*X3*X4^2*X7^2*(X5^2*X6-X2*X8^2)+
X4*X7^3*(7*X3^2*X5^2-4*X2*X5^2*X6-
16*X2*X3*X5*X8+7*X2^2*X8^2)+
X4^3*X7*(-7*X5^2*X6^2+16*X3*X5*X6*X8+
(-7*X3^2+4*X2*X6)*X8^2))*X9+
(X7^3*(-(X3*X5^3*(3*X3^2+2*X2*X6))-
2*X2*X5^2*(-5*X3^2+2*X2*X6)*X8-
3*X2^2*X3*X5*X8^2+2*X2^3*X8^3)+
X4*X7^2*(5*X5^3*X6*(-X3^2+X2*X6)+
7*X3*X5^2*(X3^2+2*X2*X6)*X8-
X2*X5*(17*X3^2+7*X2*X6)*X8^2+3*X2^2*X3*X8^3)+
X4^2*X7*(3*X3*X5^3*X6^2-
X5^2*X6*(17*X3^2+7*X2*X6)*X8+
7*X3*X5*(X3^2+2*X2*X6)*X8^2+
5*X2*(-X3^2+X2*X6)*X8^3)+
X4^3*(2*X5^3*X6^3-3*X3*X5^2*X6^2*X8-
2*X5*X6*(-5*X3^2+2*X2*X6)*X8^2-
X3*(3*X3^2+2*X2*X6)*X8^3))*X9^2-
(4*X3^4*X5*X8*(X5*X7+X4*X8)+
2*X2*X6*(-(X4*X5*X6)+X2*X7*X8)*
(-(X5^2*X6)+X2*X8^2)+
X2*X3*X6*(-(X5^3*X6*X7)-3*X4*X5^2*X6*X8-
3*X2*X5*X7*X8^2-X2*X4*X8^3)-
3*X3^3*(X5^3*X6*X7+3*X4*X5^2*X6*X8+
3*X2*X5*X7*X8^2+X2*X4*X8^3)+
2*X3^2*(X4*X5^3*X6^2+5*X2*X5^2*X6*X7*X8+
5*X2*X4*X5*X6*X8^2+X2^2*X7*X8^3))*X9^3+
X11*(-(X4^2*X6)+2*X3*X4*X7-X2*X7^2-X3^2*X9+
X2*X6*X9)*(3*X4*X7*(X4^2*X6-X2*X7^2)*(X5*X7-X4*X8)-
(X2*X7^3*(-4*X3*X5+X2*X8)+
X4^3*X6*(X5*X6-4*X3*X8)+
X4*X7^2*(-(X5*(X3^2+2*X2*X6))+6*X2*X3*X8)+
X4^2*(6*X3*X5*X6*X7-(X3^2+2*X2*X6)*X7*X8))*X9+
(-2*X3^3*(X5*X7+X4*X8)-4*X2*X3*X6*(X5*X7+X4*X8)+
5*X3^2*(X4*X5*X6+X2*X7*X8)+
X2*X6*(X4*X5*X6+X2*X7*X8))*X9^2))+
X10^2*(3*(X5*X7-X4*X8)*
(-(X2^2*X5*X7^5)+X4^5*X6^2*X8+
X2*X4*X7^4*(2*X3*X5-3*X2*X8)+
X4^4*X6*X7*(3*X5*X6-2*X3*X8)+
2*X4^2*X7^3*(2*X3^2*X5-X2*X5*X6+3*X2*X3*X8)+
2*X4^3*X7^2*(-3*X3*X5*X6-2*X3^2*X8+X2*X6*X8))-
(X2*X7^4*(6*X3^2*X5^2-7*X2*X5^2*X6+2*X2*X3*X5*X8+
5*X2^2*X8^2)+
2*X4*X7^3*(-(X3*X5^2*(-9*X3^2+4*X2*X6))-
X2*X5*(13*X3^2+3*X2*X6)*X8-X2^2*X3*X8^2)+
X4^4*X6*(5*X5^2*X6^2+2*X3*X5*X6*X8+6*X3^2*X8^2-
7*X2*X6*X8^2)+
2*X4^2*X7^2*
(-(X5^2*X6*(5*X3^2+X2*X6))+
2*X3*X5*(-4*X3^2+19*X2*X6)*X8-
X2*(5*X3^2+X2*X6)*X8^2)-
2*X4^3*X7*(X3*X5^2*X6^2+
X5*X6*(13*X3^2+3*X2*X6)*X8+
X3*(-9*X3^2+4*X2*X6)*X8^2))*X9+
(X5^2*(2*X3^3*X4*X6*X7-14*X2*X3*X4*X6^2*X7+
9*X3^4*X7^2+X2*X6^2*(4*X4^2*X6-5*X2*X7^2)+
2*X3^2*X6*(X4^2*X6+X2*X7^2))+
2*X3*X5*(-2*X3^3*X4*X7+14*X2*X3*X4*X6*X7-
11*X3^2*(X4^2*X6+X2*X7^2)+
5*X2*X6*(X4^2*X6+X2*X7^2))*X8-
(X4^2*(-9*X3^4-2*X2*X3^2*X6+5*X2^2*X6^2)+
2*X2*X3*X4*(-X3^2+7*X2*X6)*X7-
2*X2^2*(X3^2+2*X2*X6)*X7^2)*X8^2)*X9^2+
(-X3^2+X2*X6)*
(X5^2*X6*(5*X3^2+X2*X6)-4*X3*X5*(X3^2+2*X2*X6)*X8+
X2*(5*X3^2+X2*X6)*X8^2)*X9^3-
X11*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))*
(X4^4*X6^2+2*X3*X4^3*X6*X7+
X4^2*(-2*(X3^2+2*X2*X6)*X7^2-
X6*(5*X3^2+X2*X6)*X9)+
X2*X7^2*(-5*X3^2*X9+X2*(X7^2-X6*X9))+
2*X3*X4*X7*(2*X3^2*X9+X2*(X7^2+4*X6*X9))))))*
(FV[p1,rho]*FV[p1,si]*FV[p2,mu]*FV[p2,nu]+
FV[p1,nu]*FV[p1,si]*FV[p2,mu]*FV[p2,rho]+
FV[p1,mu]*FV[p1,si]*FV[p2,nu]*FV[p2,rho]+
FV[p1,nu]*FV[p1,rho]*FV[p2,mu]*FV[p2,si]+
FV[p1,mu]*FV[p1,rho]*FV[p2,nu]*FV[p2,si]+
FV[p1,mu]*FV[p1,nu]*FV[p2,rho]*FV[p2,si]))/
((1-X1)*(3-X1)*(X4^2*X6-2*X3*X4*X7+X2*X7^2+X3^2*X9-X2*X6*X9)^
4)+((-3*X4^4*X5^3*X6^2*X8*X9^2+12*X3*X4^3*X5^3*X6*X7*X8*X9^2+
12*X3^3*X4*X5^3*X7*X8*X9^3-3*X3^4*X5^3*X8*X9^4-
3*X2^2*X5^3*X8*X9^2*(X7^2-X6*X9)^2+
6*X2*X5^3*X8*X9^2*(X4^2*X6-2*X3*X4*X7+X3^2*X9)*
(-X7^2+X6*X9)-6*X3^2*X4^2*X5^3*X8*X9^2*(2*X7^2+X6*X9)+
X1^2*(X10*X3*X4-X10*X2*X7+X4*X5*X7-X4^2*X8-X3*X5*X9+
X2*X8*X9)^3*(-(X10*X4*X6)+X10*X3*X7-X5*X7^2+X4*X7*X8+
X5*X6*X9-X3*X8*X9)-
3*X10^4*X2*X3*(X4^2*X6-2*X3*X4*X7+X3^2*X9+
X2*(X7^2-X6*X9))^2+
3*X10^3*(2*X3*X4*X5+X2*X5*X7+X2*X4*X8)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2-
3*X11^2*(X4^2-X2*X9)*(X4*X7-X3*X9)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2+
3*X11*X5*X9*(-(X4*(X5*X7+X4*X8))+(X3*X5+X2*X8)*X9)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2+
3*X10*(X11*X4^2*(3*X5*X7+X4*X8)+X5^2*(X5*X7+3*X4*X8)*X9-
X11*(2*X3*X4*X5+X2*X5*X7+X2*X4*X8)*X9)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2-
3*X10^2*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2*
(X5*(2*X4*X5*X7+2*X4^2*X8+X3*X5*X9+X2*X8*X9)+
X11*(X2*X4*X7+X3*(X4^2-2*X2*X9)))+
X1*(X10*X3*X4-X10*X2*X7+X4*X5*X7-X4^2*X8-X3*X5*X9+
X2*X8*X9)*(-2*X4^2*X7*(X5*X7-X4*X8)^3+
(X5*X7-X4*X8)*(X5^2*X7*
(5*X4^2*X6-2*X3*X4*X7+3*X2*X7^2)+
X4*X5*(X4^2*X6-12*X3*X4*X7-X2*X7^2)*X8+
2*X4^2*(X3*X4+2*X2*X7)*X8^2)*X9-
(X5^3*(3*X4^2*X6^2-2*X3*X4*X6*X7+
(-X3^2+6*X2*X6)*X7^2)-
2*X5^2*(2*X3*X4^2*X6+3*X3^2*X4*X7+2*X2*X4*X6*X7+
2*X2*X3*X7^2)*X8-
X5*(X4^2*(-7*X3^2+2*X2*X6)-14*X2*X3*X4*X7+
X2^2*X7^2)*X8^2-2*X2*X4*(2*X3*X4+X2*X7)*X8^3)*X9^2+
(X5^3*X6*(-X3^2+3*X2*X6)-2*X3*X5^2*(X3^2+2*X2*X6)*X8-
X2*X5*(-7*X3^2+X2*X6)*X8^2-2*X2^2*X3*X8^3)*X9^3+
X10^2*(2*(2*X2^2*X5*X7^4+X2*X4*X7^3*(-3*X3*X5+X2*X8)-
X4^4*X6*(3*X5*X6+X3*X8)+
X4^2*X7^2*(-5*X3^2*X5+2*X2*X5*X6-3*X2*X3*X8)+
X4^3*X7*(7*X3*X5*X6+5*X3^2*X8-2*X2*X6*X8))+
(-X3^2+X2*X6)*
(-7*X2*X5*X7^2+4*X4*X7*(-2*X3*X5+X2*X8)+
X4^2*(3*X5*X6+8*X3*X8))*X9-
3*(X3^2-X2*X6)*(X3^2*X5+X2*X5*X6-2*X2*X3*X8)*X9^2)-
3*X11*(-2*X4^2*X7*(X5*X7-X4*X8)+
(X5*(X4^2*X6+2*X3*X4*X7+X2*X7^2)-
2*X4*(X3*X4+X2*X7)*X8)*X9-
(X3^2*X5+X2*X5*X6-2*X2*X3*X8)*X9^2)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))+
X10^3*(X3^2*(X4^3*X6+11*X2*X4*X7^2)+3*X3^4*X4*X9-
2*X3^3*X7*(2*X4^2+3*X2*X9)+
2*X2*X3*X7*(-4*X4^2*X6-2*X2*X7^2+3*X2*X6*X9)+
X2*X4*X6*(3*X4^2*X6+X2*(X7^2-3*X6*X9)))+
X10*(X4*(-(X5*X7)+X4*X8)*
(5*X2*X5*X7^3+X4^3*X6*X8+
X4*X7^2*(-16*X3*X5+7*X2*X8)+
X4^2*X7*(11*X5*X6-8*X3*X8))+
(-2*X2*X5*X7^3*(2*X3*X5+X2*X8)+
X4*X7^2*(X5^2*(-9*X3^2+14*X2*X6)+20*X2*X3*X5*X8-
7*X2^2*X8^2)+
2*X4^2*X7*(-5*X3*X5^2*X6+X3^2*X5*X8-
6*X2*X5*X6*X8+X2*X3*X8^2)+
X4^3*(9*X5^2*X6^2-8*X3*X5*X6*X8+7*X3^2*X8^2-
2*X2*X6*X8^2))*X9-
(X5^2*(3*X4*X6*(-X3^2+3*X2*X6)-
2*X3*(X3^2+2*X2*X6)*X7)-
2*X5*(2*X3*X4*(X3^2+2*X2*X6)+
X2*(-7*X3^2+X2*X6)*X7)*X8-
X2*(-7*X3^2*X4+X2*X4*X6+6*X2*X3*X7)*X8^2)*X9^2+
3*X11*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))*
(X4^3*X6+2*X2*X3*X7*X9-
X4*(X2*X7^2+X3^2*X9+X2*X6*X9)))))*
(FV[p1,si]*FV[p2,mu]*FV[p2,nu]*FV[p2,rho]+
FV[p1,rho]*FV[p2,mu]*FV[p2,nu]*FV[p2,si]+
FV[p1,nu]*FV[p2,mu]*FV[p2,rho]*FV[p2,si]+
FV[p1,mu]*FV[p2,nu]*FV[p2,rho]*FV[p2,si]))/
((1-X1)*(3-X1)*(X4^2*X6-2*X3*X4*X7+X2*X7^2+X3^2*X9-X2*X6*X9)^
4)+((9*X4^4*X5^4*X6^4-36*X3*X4^3*X5^3*X6^3*(X5*X7+X4*X8)+
X1^3*(X10*X3^2-X10*X2*X6+X4*X5*X6-X3*X5*X7-X3*X4*X8+
X2*X7*X8)^4+36*X3^6*X5^2*X8^2*X9^2+
9*X2^4*X8^4*(X7^2-X6*X9)^2-
36*X3^5*X5^2*X8*X9*(4*X4*X7*X8+X5*X6*X9)-
36*X3^3*X4*X5^2*X6*(4*X4*X7*X8*(X5*X7+X4*X8)+
X5*X6*(X5*X7+2*X4*X8)*X9)+
9*X11^2*(X3^2-X2*X6)^2*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2-
18*X11*(X3^2-X2*X6)*(-(X5^2*X6)+2*X3*X5*X8-X2*X8^2)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2-
18*X2*X5*(-(X5*X6)+2*X3*X8)*(X4^2*X6-2*X3*X4*X7+X3^2*X9)*
(X8^2*(X4^2*X6-2*X3*X4*X7+X3^2*X9)+
X5^2*X6*(X7^2-X6*X9)+2*X3*X5*X8*(-X7^2+X6*X9))+
18*X2^3*X8^2*(X7^2-X6*X9)*
(X8^2*(X4^2*X6-2*X3*X4*X7+X3^2*X9)+
X5^2*X6*(X7^2-X6*X9)+2*X3*X5*X8*(-X7^2+X6*X9))+
9*X2^2*(X8^4*(X4^2*X6-2*X3*X4*X7+X3^2*X9)^2+
4*X5^2*(X4*X6-X3*X7)^2*X8^2*(X7^2-X6*X9)+
X5^4*X6^2*(X7^2-X6*X9)^2-
4*X3*X5^3*X6*X8*(X7^2-X6*X9)^2+
8*X3*X5*X8^3*(X4^2*X6-2*X3*X4*X7+X3^2*X9)*(-X7^2+X6*X9))\
+18*X3^2*X4^2*X5^2*X6^2*(8*X4*X5*X7*X8+2*X4^2*X8^2+
X5^2*(2*X7^2+X6*X9))+
9*X3^4*X5^2*(16*X4*X5*X6*X7*X8*X9+X5^2*X6^2*X9^2+
8*X4^2*X8^2*(2*X7^2+X6*X9))+
X1^2*(X10*X3^2-X10*X2*X6+X4*X5*X6-X3*X5*X7-X3*X4*X8+
X2*X7*X8)^2*(-(X4^2*X5^2*X6^2)+5*X10^2*(X3^2-X2*X6)^2+
2*X3*X4*X5^2*X6*X7+5*X3^2*X5^2*X7^2-6*X2*X5^2*X6*X7^2+
2*X3*X4^2*X5*X6*X8-14*X3^2*X4*X5*X7*X8+
10*X2*X4*X5*X6*X7*X8+2*X2*X3*X5*X7^2*X8+5*X3^2*X4^2*X8^2-
6*X2*X4^2*X6*X8^2+2*X2*X3*X4*X7*X8^2-X2^2*X7^2*X8^2-
10*X10*(X3^2-X2*X6)*
(-(X4*X5*X6)+X3*X5*X7+X3*X4*X8-X2*X7*X8)+
6*(-X3^2+X2*X6)*(X5^2*X6-2*X3*X5*X8+X2*X8^2)*X9-
6*X11*(X3^2-X2*X6)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9)))+
3*X1*(-3*X4^4*X5^4*X6^4+2*X10^4*(X3^2-X2*X6)^4+
12*X3*X4^3*X5^4*X6^3*X7-14*X3^2*X4^2*X5^4*X6^2*X7^2-
4*X2*X4^2*X5^4*X6^3*X7^2+4*X3^3*X4*X5^4*X6*X7^3+
8*X2*X3*X4*X5^4*X6^2*X7^3+2*X3^4*X5^4*X7^4-
6*X2*X3^2*X5^4*X6*X7^4+X2^2*X5^4*X6^2*X7^4+
12*X3*X4^4*X5^3*X6^3*X8-44*X3^2*X4^3*X5^3*X6^2*X7*X8-
4*X2*X4^3*X5^3*X6^3*X7*X8+44*X3^3*X4^2*X5^3*X6*X7^2*X8+
28*X2*X3*X4^2*X5^3*X6^2*X7^2*X8-16*X3^4*X4*X5^3*X7^3*X8-
20*X2*X3^2*X4*X5^3*X6*X7^3*X8-12*X2^2*X4*X5^3*X6^2*X7^3*X8+
4*X2*X3^3*X5^3*X7^4*X8+8*X2^2*X3*X5^3*X6*X7^4*X8-
14*X3^2*X4^4*X5^2*X6^2*X8^2-4*X2*X4^4*X5^2*X6^3*X8^2+
44*X3^3*X4^3*X5^2*X6*X7*X8^2+
28*X2*X3*X4^3*X5^2*X6^2*X7*X8^2-20*X3^4*X4^2*X5^2*X7^2*X8^2-
92*X2*X3^2*X4^2*X5^2*X6*X7^2*X8^2+
4*X2^2*X4^2*X5^2*X6^2*X7^2*X8^2+
44*X2*X3^3*X4*X5^2*X7^3*X8^2+
28*X2^2*X3*X4*X5^2*X6*X7^3*X8^2-14*X2^2*X3^2*X5^2*X7^4*X8^2-
4*X2^3*X5^2*X6*X7^4*X8^2+4*X3^3*X4^4*X5*X6*X8^3+
8*X2*X3*X4^4*X5*X6^2*X8^3-16*X3^4*X4^3*X5*X7*X8^3-
20*X2*X3^2*X4^3*X5*X6*X7*X8^3-12*X2^2*X4^3*X5*X6^2*X7*X8^3+
44*X2*X3^3*X4^2*X5*X7^2*X8^3+
28*X2^2*X3*X4^2*X5*X6*X7^2*X8^3-
44*X2^2*X3^2*X4*X5*X7^3*X8^3-4*X2^3*X4*X5*X6*X7^3*X8^3+
12*X2^3*X3*X5*X7^4*X8^3+2*X3^4*X4^4*X8^4-
6*X2*X3^2*X4^4*X6*X8^4+X2^2*X4^4*X6^2*X8^4+
4*X2*X3^3*X4^3*X7*X8^4+8*X2^2*X3*X4^3*X6*X7*X8^4-
14*X2^2*X3^2*X4^2*X7^2*X8^4-4*X2^3*X4^2*X6*X7^2*X8^4+
12*X2^3*X3*X4*X7^3*X8^4-3*X2^4*X7^4*X8^4-
8*X10^3*(X3^2-X2*X6)^3*
(-(X4*X5*X6)+X3*X5*X7+X3*X4*X8-X2*X7*X8)+
2*(-X3^2+X2*X6)*(X5^2*X6-2*X3*X5*X8+X2*X8^2)*
(-(X5^2*(-2*X4^2*X6^2+4*X3*X4*X6*X7+
(-3*X3^2+X2*X6)*X7^2))+
2*X5*(X3^2*X4*X7+3*X2*X4*X6*X7-
2*X3*(X4^2*X6+X2*X7^2))*X8-
(X4^2*(-3*X3^2+X2*X6)+4*X2*X3*X4*X7-2*X2^2*X7^2)*X8^2
)*X9+(X3^2-X2*X6)^2*(X5^2*X6-2*X3*X5*X8+X2*X8^2)^2*X9^2+
2*X11*(-X3^2+X2*X6)*
(-(X7^2*(X5^2*(-3*X3^2+X2*X6)+4*X2*X3*X5*X8-
2*X2^2*X8^2))+
X4^2*(2*X5^2*X6^2-4*X3*X5*X6*X8+
(3*X3^2-X2*X6)*X8^2)+
2*X4*X7*(X3^2*X5*X8+3*X2*X5*X6*X8-
2*X3*(X5^2*X6+X2*X8^2))+
(-X3^2+X2*X6)*(X5^2*X6-2*X3*X5*X8+X2*X8^2)*X9)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))+
X11^2*(X3^2-X2*X6)^2*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2+
6*X10^2*(X3^2-X2*X6)^2*
(X4^2*X5^2*X6^2-2*X3*X4*X5^2*X6*X7+2*X3^2*X5^2*X7^2-
X2*X5^2*X6*X7^2-2*X3*X4^2*X5*X6*X8+
4*X2*X4*X5*X6*X7*X8-2*X2*X3*X5*X7^2*X8+
2*X3^2*X4^2*X8^2-X2*X4^2*X6*X8^2-2*X2*X3*X4*X7*X8^2+
X2^2*X7^2*X8^2+
(-X3^2+X2*X6)*(X5^2*X6-2*X3*X5*X8+X2*X8^2)*X9-
X11*(X3^2-X2*X6)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9)))+
4*X10*(-X3^2+X2*X6)*
(X4*X5*X6-X3*X5*X7-X3*X4*X8+X2*X7*X8)*
(X7^2*(X5^2*(-2*X3^2+3*X2*X6)-2*X2*X3*X5*X8+X2^2*X8^2)-
2*X4*X7*(X3*X5^2*X6-4*X3^2*X5*X8+2*X2*X5*X6*X8+
X2*X3*X8^2)+
X4^2*(X5^2*X6^2-2*X3*X5*X6*X8+
(-2*X3^2+3*X2*X6)*X8^2)-
3*(-X3^2+X2*X6)*(X5^2*X6-2*X3*X5*X8+X2*X8^2)*X9+
3*X11*(X3^2-X2*X6)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9)))))*
FV[p3,mu]*FV[p3,nu]*FV[p3,rho]*FV[p3,si])/
((1-X1)*(9-X1^2)*(X4^2*X6-2*X3*X4*X7+X2*X7^2+X3^2*X9-
X2*X6*X9)^4)+((3*X4^4*X5*X6^2*X7*X8^3*X9-
3*X4^5*X6^2*X8^4*X9+12*X3*X4^3*X6*X7*X8^3*(-(X5*X7)+X4*X8)*
X9+12*X3^3*X4*X7*X8^3*(-(X5*X7)+X4*X8)*X9^2+
3*X3^4*X8^3*(X5*X7-X4*X8)*X9^3+
3*X2^2*X8^3*(X5*X7-X4*X8)*X9*(X7^2-X6*X9)^2+
6*X2*X8^3*(-(X5*X7)+X4*X8)*X9*(X4^2*X6-2*X3*X4*X7+X3^2*X9)*
(-X7^2+X6*X9)+6*X3^2*X4^2*X8^3*(X5*X7-X4*X8)*X9*
(2*X7^2+X6*X9)+X1^2*
(-(X10*X3^2)+X10*X2*X6-X4*X5*X6+X3*X5*X7+X3*X4*X8-
X2*X7*X8)*(-(X10*X4*X6)+X10*X3*X7-X5*X7^2+X4*X7*X8+
X5*X6*X9-X3*X8*X9)^3-
3*X10^3*X6*(X5*X6-X3*X8)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2+
3*X10^2*(X11*X6*(X4*X6-X3*X7)+
X8*(3*X5*X6*X7-(X4*X6+2*X3*X7)*X8))*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2-
3*X11^2*(X4*X6-X3*X7)*(-X7^2+X6*X9)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2-
3*X11*X8*(-(X5*X7^3)+X4*X7^2*X8+X5*X6*X7*X9-
2*X4*X6*X8*X9+X3*X7*X8*X9)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2-
3*X10*(X7*(2*X8^2*(X5*X7-X4*X8)+
X11*(X5*X6*X7+2*X4*X6*X8-3*X3*X7*X8))-
(X5*X6-X3*X8)*(X11*X6-X8^2)*X9)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2+
X1*(X10*X4*X6-X10*X3*X7+X5*X7^2-X4*X7*X8-X5*X6*X9+
X3*X8*X9)*(-(X7*(X5*X7-X4*X8)^2*
(-2*X4*X5*X6*X7+2*X3*X5*X7^2+3*X4^2*X6*X8-
4*X3*X4*X7*X8+X2*X7^2*X8))-
(4*X5^3*X6*X7^2*(X4*X6-X3*X7)-
X5^2*X7*(7*X4^2*X6^2-2*X3*X4*X6*X7+
(-7*X3^2+2*X2*X6)*X7^2)*X8+
2*X5*(3*X4^3*X6^2-2*X3*X4^2*X6*X7+
X4*(-3*X3^2+4*X2*X6)*X7^2-2*X2*X3*X7^3)*X8^2-
(6*X3*X4^3*X6-11*X3^2*X4^2*X7+8*X2*X3*X4*X7^2-
3*X2^2*X7^3)*X8^3)*X9+
(2*X5^3*X6^2*(X4*X6-X3*X7)-
X5^2*X6*(6*X3*X4*X6-7*X3^2*X7+X2*X6*X7)*X8-
2*X5*(X3^3*X7+X2*X6*(-3*X4*X6+2*X3*X7))*X8^2-
(-4*X3^3*X4+6*X2*X3*X4*X6+X2*X3^2*X7-3*X2^2*X6*X7)*
X8^3)*X9^2+X10^2*
(2*X4^3*X6^2*(X5*X6-X3*X8)+
3*X4^2*X6*X7*(-2*X3*X5*X6+3*X3^2*X8-X2*X6*X8)+
X7^3*(-6*X3^3*X5+4*X2*X3*X5*X6+11*X2*X3^2*X8-
9*X2^2*X6*X8)-
2*X4*X7^2*(-5*X3^2*X5*X6+2*X2*X5*X6^2+8*X3^3*X8-
5*X2*X3*X6*X8)+
4*X4*X6*(-X3^2+X2*X6)*(X5*X6-X3*X8)*X9+
(X3^2-X2*X6)*X7*(4*X3*X5*X6+5*X3^2*X8-9*X2*X6*X8)*X9)\
+X10^3*(-X3^2+X2*X6)*(X4^2*X6^2-2*X3*X4*X6*X7+
3*X2*X6*(X7^2-X6*X9)+X3^2*(-2*X7^2+3*X6*X9))+
3*X11*(X4^2*X6-2*X3*X4*X7+X2*X7^2+X3^2*X9-X2*X6*X9)*
(X4^2*X6*X7*X8+X7^3*(2*X3*X5-X2*X8)+
X7*(-2*X3*X5*X6+X3^2*X8+X2*X6*X8)*X9+
2*X4*X6*(-(X5*X7^2)+X5*X6*X9-X3*X8*X9))+
X10*(3*X4^4*X6^2*X8^2-4*X4^3*X6*X7*X8*(X5*X6+2*X3*X8)+
X7^4*(X5^2*(6*X3^2+X2*X6)-10*X2*X3*X5*X8+
6*X2^2*X8^2)-
X7^2*(X5^2*X6*(5*X3^2+2*X2*X6)-
2*X3*X5*(X3^2+6*X2*X6)*X8+X2*(4*X3^2+3*X2*X6)*X8^2
)*X9+(-X3^2+X2*X6)*(X5^2*X6^2-2*X3*X5*X6*X8+
(4*X3^2-3*X2*X6)*X8^2)*X9^2-
3*X11*(X4^2*X6-2*X3*X4*X7+X2*X7^2+X3^2*X9-X2*X6*X9)*
(X4^2*X6^2-2*X3*X4*X6*X7-(-2*X3^2+X2*X6)*X7^2+
X6*(-X3^2+X2*X6)*X9)+
X4^2*(X7^2*(7*X5^2*X6^2-2*X3*X5*X6*X8+
(10*X3^2+3*X2*X6)*X8^2)-7*X6*(X5*X6-X3*X8)^2*X9)\
+2*X4*X7*(X7^2*(-7*X3*X5^2*X6+4*X5*(X3^2+X2*X6)*X8-7*X2*X3*X8^2)+
(X5*X6-X3*X8)*(7*X3*X5*X6-3*X3^2*X8-4*X2*X6*X8)*X9))
))*(FV[p1,nu]*FV[p1,rho]*FV[p1,si]*FV[p3,mu]+
FV[p1,mu]*FV[p1,rho]*FV[p1,si]*FV[p3,nu]+
FV[p1,mu]*FV[p1,nu]*FV[p1,si]*FV[p3,rho]+
FV[p1,mu]*FV[p1,nu]*FV[p1,rho]*FV[p3,si]))/
((1-X1)*(3-X1)*(X4^2*X6-2*X3*X4*X7+X2*X7^2+X3^2*X9-X2*X6*X9)^
4)+((-3*X4^4*X5^2*X6^2*X7*X8^2*X9+3*X4^5*X5*X6^2*X8^3*X9+
12*X3*X4^3*X5*X6*X7*X8^2*(X5*X7-X4*X8)*X9+
12*X3^3*X4*X5*X7*X8^2*(X5*X7-X4*X8)*X9^2+
3*X3^4*X5*X8^2*(-(X5*X7)+X4*X8)*X9^3+
3*X2^2*X5*X8^2*(-(X5*X7)+X4*X8)*X9*(X7^2-X6*X9)^2+
6*X2*X5*X8^2*(X5*X7-X4*X8)*X9*(X4^2*X6-2*X3*X4*X7+X3^2*X9)*
(-X7^2+X6*X9)+6*X3^2*X4^2*X5*X8^2*(-(X5*X7)+X4*X8)*X9*
(2*X7^2+X6*X9)+X1^2*
(-(X10*X3^2)+X10*X2*X6-X4*X5*X6+X3*X5*X7+X3*X4*X8-
X2*X7*X8)*(X10*X3*X4-X10*X2*X7+X4*X5*X7-X4^2*X8-
X3*X5*X9+X2*X8*X9)*
(X10*X4*X6-X10*X3*X7+X5*X7^2-X4*X7*X8-X5*X6*X9+
X3*X8*X9)^2-X10^3*(-3*X3*X5*X6+2*X3^2*X8+X2*X6*X8)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2+
X10^2*(-3*X5^2*X6*X7+
X11*(-3*X3*X4*X6+2*X3^2*X7+X2*X6*X7)-
X5*(X4*X6+2*X3*X7)*X8+2*(2*X3*X4+X2*X7)*X8^2)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2-
X11^2*(2*X4^2*X6*X7-X3*X4*X7^2-X2*X7^3-3*X3*X4*X6*X9+
2*X3^2*X7*X9+X2*X6*X7*X9)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2+
X11*(-(X7*(X5*X7-X4*X8)*(X5*X7+2*X4*X8))+
(X5^2*X6*X7+X5*(-3*X4*X6+4*X3*X7)*X8+
(-3*X3*X4+X2*X7)*X8^2)*X9)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2+
X10*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2*
(X11*(2*X4^2*X6*X8-X7^2*(X3*X5+3*X2*X8)+
X4*(4*X5*X6*X7-2*X3*X7*X8)+
(-3*X3*X5*X6+2*X3^2*X8+X2*X6*X8)*X9)+
X8*(X8^2*(-2*X4^2-X2*X9)+X5*X8*(-2*X4*X7-X3*X9)+
X5^2*(4*X7^2+2*X6*X9)))+
X1*(X7*(X5*X7-X4*X8)^3*
(-(X4^2*X5*X6*X7)+X2*X5*X7^3+2*X4^3*X6*X8-
2*X3*X4^2*X7*X8)+
(X5*X7-X4*X8)*(-(X5^3*X7^2*
(-2*X4^2*X6^2-2*X3*X4*X6*X7+(X3^2+3*X2*X6)*X7^2))\
+2*X5^2*X7*(-(X4^3*X6^2)-6*X3*X4^2*X6*X7+
2*X4*(2*X3^2+X2*X6)*X7^2+X2*X3*X7^3)*X8+
X5*(3*X4^4*X6^2+X3*X4^3*X6*X7+
X4^2*(X3^2+8*X2*X6)*X7^2-15*X2*X3*X4*X7^3+
2*X2^2*X7^4)*X8^2-
X4*(3*X3*X4^3*X6+X4^2*(-4*X3^2+3*X2*X6)*X7-
X2*X3*X4*X7^2-X2^2*X7^3)*X8^3)*X9+
(X5^4*X6*X7*(-(X4^2*X6^2)-4*X3*X4*X6*X7+
(2*X3^2+3*X2*X6)*X7^2)-
X5^3*(X4^3*X6^3-16*X3*X4^2*X6^2*X7+
5*X2*X4*X6^2*X7^2+6*X3^3*X7^3+4*X2*X3*X6*X7^3)*X8-
X5^2*(3*X3*X4^3*X6^2+X4^2*X6*(17*X3^2+7*X2*X6)*X7-
7*X3*X4*(X3^2+2*X2*X6)*X7^2+
2*X2*(-5*X3^2+2*X2*X6)*X7^3)*X8^2+
X5*(3*X4^3*X6*(X3^2+2*X2*X6)+
2*X3*X4^2*(X3^2+X2*X6)*X7+
X2*X4*(-15*X3^2+4*X2*X6)*X7^2-2*X2^2*X3*X7^3)*X8^3\
-(-(X3^3*X4^3)-5*X2*X3^2*X4^2*X7-X2^3*X7^3+
X2*X3*X4*(6*X4^2*X6+X2*X7^2))*X8^4)*X9^2-
(X5^4*X6^2*(-2*X3*X4*X6+X3^2*X7+X2*X6*X7)-
X5^3*X6*(-9*X3^2*X4*X6+X2*X4*X6^2+6*X3^3*X7+
2*X2*X3*X6*X7)*X8-
X5^2*(3*X3*X4*X6*(3*X3^2+X2*X6)+
2*(-2*X3^4-5*X2*X3^2*X6+X2^2*X6^2)*X7)*X8^2+
X5*(X4*(2*X3^4+3*X2*X3^2*X6+3*X2^2*X6^2)-
2*X2*X3*(3*X3^2+X2*X6)*X7)*X8^3-
X2*(X3*X4*(-X3^2+3*X2*X6)-X2*(X3^2+X2*X6)*X7)*X8^4)*
X9^3+X10^3*(-2*X3*X4^4*X5*X6^3+2*X3^2*X4^3*X5*X6^2*X7+
6*X2*X4^3*X5*X6^3*X7+3*X3^3*X4^2*X5*X6*X7^2-
15*X2*X3*X4^2*X5*X6^2*X7^2-6*X3^4*X4*X5*X7^3+
16*X2*X3^2*X4*X5*X6*X7^3-2*X2^2*X4*X5*X6^2*X7^3-
X2*X3^3*X5*X7^4-X2^2*X3*X5*X6*X7^4+2*X2*X4^4*X6^3*X8+
2*X3^3*X4^3*X6*X7*X8-10*X2*X3*X4^3*X6^2*X7*X8+
4*X3^4*X4^2*X7^2*X8+X2*X3^2*X4^2*X6*X7^2*X8+
7*X2^2*X4^2*X6^2*X7^2*X8-8*X2*X3^3*X4*X7^3*X8+
5*X2^2*X3^2*X7^4*X8-3*X2^3*X6*X7^4*X8-
(X3^2-X2*X6)*(X3^3*X7*(-7*X5*X7+2*X4*X8)+
X3*X6*(-3*X4^2*X5*X6+4*X2*X5*X7^2-8*X2*X4*X7*X8)+
X3^2*(4*X4*X5*X6*X7+6*X4^2*X6*X8+X2*X7^2*X8)+
X2*X6*(2*X4*X5*X6*X7-3*X4^2*X6*X8+2*X2*X7^2*X8))*X9\
+(X3^2-X2*X6)^2*(-3*X3*X5*X6+2*X3^2*X8+X2*X6*X8)*X9^2)+
X11*(-(X4^2*X6)+2*X3*X4*X7-X2*X7^2-X3^2*X9+X2*X6*X9)*
(X7*(X5*X7-X4*X8)*
(X5*X7*(-5*X4^2*X6+4*X3*X4*X7+X2*X7^2)+
2*X4*(X4^2*X6+X3*X4*X7-2*X2*X7^2)*X8)+
(3*X4^3*X6*X8*(-(X5*X6)+X3*X8)+
X7^3*(-(X5^2*(5*X3^2+2*X2*X6))+8*X2*X3*X5*X8-
X2^2*X8^2)+
X4*X7^2*(2*X3*X5^2*X6+3*X3^2*X5*X8+
2*X2*X5*X6*X8-7*X2*X3*X8^2)+
X4^2*X7*(5*X5^2*X6^2-10*X3*X5*X6*X8+
(4*X3^2+X2*X6)*X8^2))*X9-
(X5^2*X6*(6*X3*X4*X6-5*X3^2*X7-X2*X6*X7)-
X5*(3*X4*X6*(3*X3^2+X2*X6)-
4*X3*(X3^2+2*X2*X6)*X7)*X8+
(3*X3*X4*(X3^2+X2*X6)-X2*(5*X3^2+X2*X6)*X7)*X8^2)*
X9^2)+X10^4*(-X3^2+X2*X6)*
(2*X3^4*X7*X9+X3^2*X6*X7*(4*X4^2-X2*X9)-
X3^3*X4*(2*X7^2+3*X6*X9)+
X3*X4*X6*(-(X4^2*X6)-X2*X7^2+3*X2*X6*X9)+
X2*X6*X7*(X2*X7^2-X6*(X4^2+X2*X9)))+
X10^2*(-3*X3*X4^5*X6^2*X8^2+
X2*X7^5*(X5^2*(3*X3^2+2*X2*X6)-4*X2*X3*X5*X8+
2*X2^2*X8^2)+
X4^4*X6*X7*(-4*X5^2*X6^2+14*X3*X5*X6*X8+6*X3^2*X8^2-
X2*X6*X8^2)-
X7^3*(X5^2*(9*X3^4-4*X2*X3^2*X6+4*X2^2*X6^2)-
2*X2*X3*X5*(8*X3^2+X2*X6)*X8-
X2^2*(-11*X3^2+2*X2*X6)*X8^2)*X9+
(-X3^2+X2*X6)*X7*
(X5^2*X6*(-5*X3^2+2*X2*X6)+
2*X3*X5*(2*X3^2+X2*X6)*X8-X2*(-X3^2+4*X2*X6)*X8^2
)*X9^2+X4^3*(-(X7^2*(-7*X3*X5^2*X6^2+
X5*X6*(23*X3^2+15*X2*X6)*X8+
X3*(12*X3^2-13*X2*X6)*X8^2))-
3*X6*(X5*X6-X3*X8)*
(-3*X3*X5*X6+2*X3^2*X8+X2*X6*X8)*X9)+
X4^2*X7*(X7^2*(X5^2*X6*(-7*X3^2+10*X2*X6)+
2*X3*X5*(7*X3^2+8*X2*X6)*X8-
X2*(-8*X3^2+11*X2*X6)*X8^2)-
(X5^2*X6^2*(17*X3^2+10*X2*X6)-
2*X3*X5*X6*(8*X3^2+19*X2*X6)*X8-
(6*X3^4-38*X2*X3^2*X6+5*X2^2*X6^2)*X8^2)*X9)+
X4*(-(X7^4*(X3*X5^2*(-6*X3^2+17*X2*X6)-
X2*X5*(-11*X3^2+9*X2*X6)*X8+2*X2^2*X3*X8^2))+
X7^2*(X3*X5^2*X6*(13*X3^2+14*X2*X6)-
X5*(11*X3^4+31*X2*X3^2*X6+12*X2^2*X6^2)*X8+
X2*X3*(13*X3^2+14*X2*X6)*X8^2)*X9-
3*(X3^2-X2*X6)*
(X3*X5^2*X6^2+X5*X6*(-3*X3^2+X2*X6)*X8+
X3^3*X8^2)*X9^2)+
X11*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))*
(2*X3^4*X7*X9+X3^3*X4*(2*X7^2-3*X6*X9)-
X3^2*X7*(4*X4^2*X6+4*X2*X7^2+X2*X6*X9)+
X3*X4*X6*(7*X2*X7^2+3*X6*(X4^2+X2*X9))+
X2*X6*X7*(-5*X4^2*X6+X2*(X7^2-X6*X9))))+
X10*(-((X5*X7-X4*X8)*
(2*X4^5*X6^2*X8^2+X2*X5*X7^5*(3*X3*X5+X2*X8)-
2*X4^4*X6*X7*X8*(X5*X6+X3*X8)+
X4*X7^4*(2*X3^2*X5^2-2*X2*X5^2*X6-
19*X2*X3*X5*X8+5*X2^2*X8^2)+
X4^2*X7^3*(-9*X3*X5^2*X6+
3*X5*(6*X3^2+5*X2*X6)*X8-8*X2*X3*X8^2)+
X4^3*X7^2*(6*X5^2*X6^2-13*X3*X5*X6*X8+
4*X3^2*X8^2-X2*X6*X8^2)))+
(3*X4^4*X6*X8^2*(3*X3*X5*X6-2*X3^2*X8-X2*X6*X8)+
X4^2*X7^2*X8*
(2*X5^2*X6*(20*X3^2+7*X2*X6)-
X3*X5*(23*X3^2+31*X2*X6)*X8-
X2*(-X3^2+X2*X6)*X8^2)+
2*X4*X5*X7^3*
(-(X5^2*X6*(7*X3^2+2*X2*X6))-
2*X3*X5*(-X3^2+X2*X6)*X8+
X2*(8*X3^2+X2*X6)*X8^2)+
X7^4*(X3*X5^3*(5*X3^2+7*X2*X6)-
X2*X5^2*(17*X3^2+X2*X6)*X8+9*X2^2*X3*X5*X8^2-
3*X2^3*X8^3)+
2*X4^3*X7*(3*X5^3*X6^3-18*X3*X5^2*X6^2*X8+
X5*X6*(8*X3^2+X2*X6)*X8^2+
X3*(X3^2+5*X2*X6)*X8^3))*X9-
(-(X5^3*X6*(-9*X3*X4^2*X6^2+
2*X4*X6*(8*X3^2+X2*X6)*X7-
X3*(4*X3^2+5*X2*X6)*X7^2))-
X5^2*(3*X4^2*X6^2*(8*X3^2+X2*X6)-
18*X3*X4*X6*(2*X3^2+X2*X6)*X7+
(2*X3^2+X2*X6)*(4*X3^2+5*X2*X6)*X7^2)*X8+
X5*(3*X3*X4^2*X6*(5*X3^2+4*X2*X6)-
2*X4*(7*X3^4+19*X2*X3^2*X6+X2^2*X6^2)*X7+
3*X2*X3*(5*X3^2+4*X2*X6)*X7^2)*X8^2-
X2*(9*X3^2*X4^2*X6-2*X3*X4*(5*X3^2+4*X2*X6)*X7+
X2*(7*X3^2+2*X2*X6)*X7^2)*X8^3)*X9^2+
(-X3^2+X2*X6)*
(-2*X2*X3^2*X8^3+X2*X6*X8*(-3*X5^2*X6+X2*X8^2)+
X3*X5*X6*(X5^2*X6+3*X2*X8^2))*X9^3-
X11*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))*
(2*X4^4*X6^2*X8-X2*X7^4*(5*X3*X5-X2*X8)+
2*X4^3*X6*X7*(-2*X5*X6+X3*X8)+2*X3^3*X4*X7*X8*X9+
X3*X7^2*(7*X3^2*X5+2*X2*X5*X6-9*X2*X3*X8)*X9+
(X3^2-X2*X6)*(-3*X3*X5*X6+2*X3^2*X8+X2*X6*X8)*
X9^2+8*X2*X4*X5*X6*X7*(X7^2-X6*X9)+
4*X2*X3*X4*X7*X8*(X7^2+4*X6*X9)-
2*X3^2*X4*X5*X7*(X7^2+5*X6*X9)+
X4^2*X6*(-8*X3^2*X8*X9+X2*X8*(-9*X7^2-X6*X9)+
3*X3*X5*(X7^2+3*X6*X9))))))*
(FV[p1,rho]*FV[p1,si]*FV[p2,nu]*FV[p3,mu]+
FV[p1,nu]*FV[p1,si]*FV[p2,rho]*FV[p3,mu]+
FV[p1,nu]*FV[p1,rho]*FV[p2,si]*FV[p3,mu]+
FV[p1,rho]*FV[p1,si]*FV[p2,mu]*FV[p3,nu]+
FV[p1,mu]*FV[p1,si]*FV[p2,rho]*FV[p3,nu]+
FV[p1,mu]*FV[p1,rho]*FV[p2,si]*FV[p3,nu]+
FV[p1,nu]*FV[p1,si]*FV[p2,mu]*FV[p3,rho]+
FV[p1,mu]*FV[p1,si]*FV[p2,nu]*FV[p3,rho]+
FV[p1,mu]*FV[p1,nu]*FV[p2,si]*FV[p3,rho]+
FV[p1,nu]*FV[p1,rho]*FV[p2,mu]*FV[p3,si]+
FV[p1,mu]*FV[p1,rho]*FV[p2,nu]*FV[p3,si]+
FV[p1,mu]*FV[p1,nu]*FV[p2,rho]*FV[p3,si]))/
((1-X1)*(3-X1)*(X4^2*X6-2*X3*X4*X7+X2*X7^2+X3^2*X9-X2*X6*X9)^
4)+((3*X4^4*X5^3*X6^2*X7*X8*X9-3*X4^5*X5^2*X6^2*X8^2*X9+
12*X3*X4^3*X5^2*X6*X7*X8*(-(X5*X7)+X4*X8)*X9+
12*X3^3*X4*X5^2*X7*X8*(-(X5*X7)+X4*X8)*X9^2+
3*X3^4*X5^2*X8*(X5*X7-X4*X8)*X9^3+
3*X2^2*X5^2*X8*(X5*X7-X4*X8)*X9*(X7^2-X6*X9)^2+
6*X2*X5^2*X8*(-(X5*X7)+X4*X8)*X9*
(X4^2*X6-2*X3*X4*X7+X3^2*X9)*(-X7^2+X6*X9)+
6*X3^2*X4^2*X5^2*X8*(X5*X7-X4*X8)*X9*(2*X7^2+X6*X9)+
X1^2*(X4*X5*X6+X10*(X3^2-X2*X6)-X3*X5*X7-X3*X4*X8+
X2*X7*X8)*(X10*X3*X4-X10*X2*X7+X4*X5*X7-X4^2*X8-
X3*X5*X9+X2*X8*X9)^2*
(X10*X4*X6-X10*X3*X7+X5*X7^2-X4*X7*X8-X5*X6*X9+
X3*X8*X9)-X10^3*(2*X3^2*X5+X2*X5*X6-3*X2*X3*X8)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2+
X10^2*(2*X5^2*(X4*X6+2*X3*X7)+
X11*(2*X3^2*X4+X2*X4*X6-3*X2*X3*X7)-
X5*(2*X3*X4+X2*X7)*X8-3*X2*X4*X8^2)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2+
X11^2*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2*
(X4^3*X6+X3*X4^2*X7+3*X2*X3*X7*X9+
X4*(-2*X2*X7^2-2*X3^2*X9-X2*X6*X9))-
X11*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2*
(X4^2*X5*X7*X8+X4^3*X8^2+3*X5*X7*(X3*X5+X2*X8)*X9-
X4*(4*X3*X5*X8*X9+X2*X8^2*X9+X5^2*(2*X7^2+X6*X9)))-
X10*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2*
(X5*(-2*X8^2*(2*X4^2+X2*X9)+X5*X8*(2*X4*X7+X3*X9)+
X5^2*(2*X7^2+X6*X9))+
X11*(2*X4*X7*(X3*X5-2*X2*X8)+X4^2*(3*X5*X6+X3*X8)-
2*X3^2*X5*X9+X2*(-2*X5*X7^2-X5*X6*X9+3*X3*X8*X9)))+
X1*(X4*(-(X5*X7)+X4*X8)^3*
(-2*X3*X4*X5*X7^2+2*X2*X5*X7^3+X4^3*X6*X8-X2*X4*X7^2*X8)\
-(X5*X7-X4*X8)*(-(X5^3*X7*
(-(X4^3*X6^2)-X3*X4^2*X6*X7+
X4*(-4*X3^2+3*X2*X6)*X7^2+3*X2*X3*X7^3))+
X5^2*(2*X4^4*X6^2-15*X3*X4^3*X6*X7+
X4^2*(X3^2+8*X2*X6)*X7^2+X2*X3*X4*X7^3+
3*X2^2*X7^4)*X8+
2*X4*X5*(X3*X4^3*X6+2*X4^2*(2*X3^2+X2*X6)*X7-
6*X2*X3*X4*X7^2-X2^2*X7^3)*X8^2-
X4^2*(X4^2*(X3^2+3*X2*X6)-2*X2*X3*X4*X7-2*X2^2*X7^2)*
X8^3)*X9+(-(X5^4*
(-(X4^3*X6^3)+X3*X4^2*X6^2*X7-5*X3^2*X4*X6*X7^2+
X3*(-X3^2+6*X2*X6)*X7^3))+
X5^3*(-2*X3*X4^3*X6^2+
X4^2*X6*(-15*X3^2+4*X2*X6)*X7+
2*X3*X4*(X3^2+X2*X6)*X7^2+
3*X2*(X3^2+2*X2*X6)*X7^3)*X8-
X5^2*(2*X4^3*X6*(-5*X3^2+2*X2*X6)-
7*X3*X4^2*(X3^2+2*X2*X6)*X7+
X2*X4*(17*X3^2+7*X2*X6)*X7^2+3*X2^2*X3*X7^3)*X8^2-
X5*(6*X3^3*X4^3+4*X2*X3*X4*(X4^2*X6-4*X2*X7^2)+
X2^2*X7*(5*X4^2*X6+X2*X7^2))*X8^3+
X2*X4*(X4^2*(2*X3^2+3*X2*X6)-4*X2*X3*X4*X7-
X2^2*X7^2)*X8^4)*X9^2-
(X5^4*X6*(X3^2*X4*X6+X2*X4*X6^2+X3^3*X7-
3*X2*X3*X6*X7)-
X5^3*(2*X3*X4*X6*(3*X3^2+X2*X6)-
(2*X3^4+3*X2*X3^2*X6+3*X2^2*X6^2)*X7)*X8-
X5^2*(2*X4*(-2*X3^4-5*X2*X3^2*X6+X2^2*X6^2)+
3*X2*X3*(3*X3^2+X2*X6)*X7)*X8^2-
X2*X5*(6*X3^3*X4+2*X2*X3*X4*X6-9*X2*X3^2*X7+
X2^2*X6*X7)*X8^3+
X2^2*(X3^2*X4+X2*X4*X6-2*X2*X3*X7)*X8^4)*X9^3+
X10^3*(5*X3^2*X4^4*X5*X6^2-3*X2*X4^4*X5*X6^3-
8*X3^3*X4^3*X5*X6*X7+4*X3^4*X4^2*X5*X7^2+
X2*X3^2*X4^2*X5*X6*X7^2+7*X2^2*X4^2*X5*X6^2*X7^2+
2*X2*X3^3*X4*X5*X7^3-10*X2^2*X3*X4*X5*X6*X7^3+
2*X2^3*X5*X6*X7^4-X3^3*X4^4*X6*X8-X2*X3*X4^4*X6^2*X8-
6*X3^4*X4^3*X7*X8+16*X2*X3^2*X4^3*X6*X7*X8-
2*X2^2*X4^3*X6^2*X7*X8+3*X2*X3^3*X4^2*X7^2*X8-
15*X2^2*X3*X4^2*X6*X7^2*X8+2*X2^2*X3^2*X4*X7^3*X8+
6*X2^3*X4*X6*X7^3*X8-2*X2^3*X3*X7^4*X8+
(-X3^2+X2*X6)*
(X3^3*X4*(2*X5*X7-7*X4*X8)+
X2*X6*(2*X4^2*X5*X6-3*X2*X5*X7^2+2*X2*X4*X7*X8)+
X3^2*(X4^2*X5*X6+6*X2*X5*X7^2+4*X2*X4*X7*X8)+
X2*X3*(-8*X4*X5*X6*X7+4*X4^2*X6*X8-3*X2*X7^2*X8))*X9\
+(X3^2-X2*X6)^2*(2*X3^2*X5+X2*X5*X6-3*X2*X3*X8)*X9^2)-
X11*(X4*(-(X5*X7)+X4*X8)*
(2*X5*X7*(-2*X4^2*X6+X3*X4*X7+X2*X7^2)+
X4*(X4^2*X6+4*X3*X4*X7-5*X2*X7^2)*X8)-
(3*X2*X5*X7^3*(-(X3*X5)+X2*X8)+
X4*X7^2*(-(X5^2*(4*X3^2+X2*X6))+10*X2*X3*X5*X8-
5*X2^2*X8^2)+
X4^2*X7*(7*X3*X5^2*X6-3*X3^2*X5*X8-
2*X2*X5*X6*X8-2*X2*X3*X8^2)+
X4^3*(X5^2*X6^2-8*X3*X5*X6*X8+
(5*X3^2+2*X2*X6)*X8^2))*X9+
(X5^2*(X4*X6*(5*X3^2+X2*X6)-3*X3*(X3^2+X2*X6)*X7)-
X5*(4*X3*X4*(X3^2+2*X2*X6)-
3*X2*(3*X3^2+X2*X6)*X7)*X8+
X2*(5*X3^2*X4+X2*X4*X6-6*X2*X3*X7)*X8^2)*X9^2)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))-
X10^4*(-X3^2+X2*X6)*
(2*X3^3*X4*(X4*X7-X3*X9)+
X2*(-(X4^3*X6^2)+X3*X4^2*X6*X7+3*X3^3*X7*X9+
X3^2*X4*(-4*X7^2+X6*X9))+
X2^2*(X3*X7*(X7^2-3*X6*X9)+X4*X6*(X7^2+X6*X9)))+
X10^2*(-3*X2^2*X3*X5^2*X7^5+
X4^4*X7*(-2*X3*X5^2*X6^2+X5*X6*(-11*X3^2+9*X2*X6)*X8+
X3*(6*X3^2-17*X2*X6)*X8^2)+
X4^5*X6*(2*X5^2*X6^2-4*X3*X5*X6*X8+
(3*X3^2+2*X2*X6)*X8^2)+
3*X2*X7^3*(X3*X5-X2*X8)*
(2*X3^2*X5+X2*X5*X6-3*X2*X3*X8)*X9-
3*(X3^2-X2*X6)*X7*
(X3^3*X5^2+X2*X5*(-3*X3^2+X2*X6)*X8+X2^2*X3*X8^2)*
X9^2+X4^2*X7*
(X7^2*(X3*X5^2*(-12*X3^2+13*X2*X6)-
X2*X5*(23*X3^2+15*X2*X6)*X8+7*X2^2*X3*X8^2)+
(X3*X5^2*X6*(13*X3^2+14*X2*X6)-
X5*(11*X3^4+31*X2*X3^2*X6+12*X2^2*X6^2)*X8+
X2*X3*(13*X3^2+14*X2*X6)*X8^2)*X9)+
X4^3*(X7^2*(-(X5^2*X6*(-8*X3^2+11*X2*X6))+
2*X3*X5*(7*X3^2+8*X2*X6)*X8+
X2*(-7*X3^2+10*X2*X6)*X8^2)+
(X5^2*X6^2*(-11*X3^2+2*X2*X6)+
2*X3*X5*X6*(8*X3^2+X2*X6)*X8-
(9*X3^4-4*X2*X3^2*X6+4*X2^2*X6^2)*X8^2)*X9)+
X4*(-(X2*X7^4*(X5^2*(-6*X3^2+X2*X6)-14*X2*X3*X5*X8+
4*X2^2*X8^2))+
X7^2*(X5^2*(6*X3^4-38*X2*X3^2*X6+5*X2^2*X6^2)+
2*X2*X3*X5*(8*X3^2+19*X2*X6)*X8-
X2^2*(17*X3^2+10*X2*X6)*X8^2)*X9-
(X3^2-X2*X6)*
(-(X5^2*X6*(-X3^2+4*X2*X6))+
2*X3*X5*(2*X3^2+X2*X6)*X8+
X2*(-5*X3^2+2*X2*X6)*X8^2)*X9^2)+
X11*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))*
(2*X3^4*X4*X9-
X3^2*X4*(4*X4^2*X6+4*X2*X7^2+X2*X6*X9)+
X3^3*(2*X4^2*X7-3*X2*X7*X9)+
X2*X3*X7*(7*X4^2*X6+3*X2*(X7^2+X6*X9))+
X2*X4*X6*(X4^2*X6-X2*(5*X7^2+X6*X9))))+
X10*((X5*X7-X4*X8)*
(2*X2^2*X5^2*X7^5-2*X2*X4*X5*X7^4*(X3*X5+X2*X8)+
X4^5*X6*X8*(X5*X6+3*X3*X8)+
X4^2*X7^3*(4*X3^2*X5^2-X2*X5^2*X6-13*X2*X3*X5*X8+
6*X2^2*X8^2)+
X4^3*X7^2*(-8*X3*X5^2*X6+3*X5*(6*X3^2+5*X2*X6)*X8-
9*X2*X3*X8^2)+
X4^4*X7*(5*X5^2*X6^2-19*X3*X5*X6*X8+2*X3^2*X8^2-
2*X2*X6*X8^2))-
(3*X2*X5^2*X7^4*(2*X3^2*X5+X2*X5*X6-3*X2*X3*X8)+
2*X4^3*X7*X8*
(-(X5^2*X6*(8*X3^2+X2*X6))+
2*X3*X5*(-X3^2+X2*X6)*X8+
X2*(7*X3^2+2*X2*X6)*X8^2)+
X4^2*X5*X7^2*
(X5^2*X6*(-X3^2+X2*X6)+
X3*X5*(23*X3^2+31*X2*X6)*X8-
2*X2*(20*X3^2+7*X2*X6)*X8^2)+
2*X4*X7^3*(-(X3*X5^3*(X3^2+5*X2*X6))-
X2*X5^2*(8*X3^2+X2*X6)*X8+18*X2^2*X3*X5*X8^2-
3*X2^3*X8^3)+
X4^4*(3*X5^3*X6^3-9*X3*X5^2*X6^2*X8+
X5*X6*(17*X3^2+X2*X6)*X8^2-
X3*(5*X3^2+7*X2*X6)*X8^3))*X9+
(X5^3*X6*(X4^2*X6*(7*X3^2+2*X2*X6)-
2*X3*X4*(5*X3^2+4*X2*X6)*X7+9*X2*X3^2*X7^2)-
X5^2*(3*X3*X4^2*X6*(5*X3^2+4*X2*X6)-
2*X4*(7*X3^4+19*X2*X3^2*X6+X2^2*X6^2)*X7+
3*X2*X3*(5*X3^2+4*X2*X6)*X7^2)*X8+
X5*(X4^2*(2*X3^2+X2*X6)*(4*X3^2+5*X2*X6)-
18*X2*X3*X4*(2*X3^2+X2*X6)*X7+
3*X2^2*(8*X3^2+X2*X6)*X7^2)*X8^2-
X2*(X3*X4^2*(4*X3^2+5*X2*X6)-
2*X2*X4*(8*X3^2+X2*X6)*X7+9*X2^2*X3*X7^2)*X8^3)*
X9^2+(-X3^2+X2*X6)*
(-2*X3^2*X5^3*X6+X2*X5*X6*(X5^2*X6-3*X2*X8^2)+
X2*X3*X8*(3*X5^2*X6+X2*X8^2))*X9^3-
X11*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))*
(2*X2^2*X5*X7^4+X4^4*X6*(X5*X6-5*X3*X8)+
2*X4^3*X7*(2*X3*X5*X6-X3^2*X8+4*X2*X6*X8)-
X2*X7^2*(8*X3^2*X5+X2*X5*X6-9*X2*X3*X8)*X9+
(X3^2-X2*X6)*(2*X3^2*X5+X2*X5*X6-3*X2*X3*X8)*X9^2+
X4^2*(X3^2*(-9*X5*X6+7*X3*X8)*X9+
X2*(-9*X5*X6*X7^2+X3*X8*(3*X7^2+2*X6*X9)))+
2*X4*X7*(X3^3*X5*X9-2*X2^2*X8*(X7^2+2*X6*X9)+
X2*X3*(-5*X3*X8*X9+X5*(X7^2+8*X6*X9)))))))*
(FV[p1,si]*FV[p2,nu]*FV[p2,rho]*FV[p3,mu]+
FV[p1,rho]*FV[p2,nu]*FV[p2,si]*FV[p3,mu]+
FV[p1,nu]*FV[p2,rho]*FV[p2,si]*FV[p3,mu]+
FV[p1,si]*FV[p2,mu]*FV[p2,rho]*FV[p3,nu]+
FV[p1,rho]*FV[p2,mu]*FV[p2,si]*FV[p3,nu]+
FV[p1,mu]*FV[p2,rho]*FV[p2,si]*FV[p3,nu]+
FV[p1,si]*FV[p2,mu]*FV[p2,nu]*FV[p3,rho]+
FV[p1,nu]*FV[p2,mu]*FV[p2,si]*FV[p3,rho]+
FV[p1,mu]*FV[p2,nu]*FV[p2,si]*FV[p3,rho]+
FV[p1,rho]*FV[p2,mu]*FV[p2,nu]*FV[p3,si]+
FV[p1,nu]*FV[p2,mu]*FV[p2,rho]*FV[p3,si]+
FV[p1,mu]*FV[p2,nu]*FV[p2,rho]*FV[p3,si]))/
((1-X1)*(3-X1)*(X4^2*X6-2*X3*X4*X7+X2*X7^2+X3^2*X9-X2*X6*X9)^
4)+((-3*X4^4*X5^4*X6^2*X7*X9+3*X4^5*X5^3*X6^2*X8*X9+
12*X3*X4^3*X5^3*X6*X7*(X5*X7-X4*X8)*X9+
12*X3^3*X4*X5^3*X7*(X5*X7-X4*X8)*X9^2+
3*X3^4*X5^3*(-(X5*X7)+X4*X8)*X9^3-
3*X2^2*X5^3*(X5*X7-X4*X8)*X9*(X7^2-X6*X9)^2+
6*X2*X5^3*(X5*X7-X4*X8)*X9*(X4^2*X6-2*X3*X4*X7+X3^2*X9)*
(-X7^2+X6*X9)+6*X3^2*X4^2*X5^3*(-(X5*X7)+X4*X8)*X9*
(2*X7^2+X6*X9)+X1^2*
(X4*X5*X6+X10*(X3^2-X2*X6)-X3*X5*X7-X3*X4*X8+X2*X7*X8)*
(-(X10*X3*X4)+X10*X2*X7-X4*X5*X7+X4^2*X8+X3*X5*X9-
X2*X8*X9)^3+3*X10^3*X2*(X3*X5-X2*X8)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2+
3*X10^2*(X11*X2*(-(X3*X4)+X2*X7)+
X5*(-2*X3*X4*X5-X2*X5*X7+3*X2*X4*X8))*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2-
3*X11^2*(X3*X4-X2*X7)*(X4^2-X2*X9)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2+
3*X10*(-(X4*(2*X5^2*(-(X5*X7)+X4*X8)+
X11*(-3*X3*X4*X5+2*X2*X5*X7+X2*X4*X8)))+
(X11*X2-X5^2)*(-(X3*X5)+X2*X8)*X9)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2-
3*X11*X5*(X4^2*X5*X7-X4^3*X8-2*X2*X5*X7*X9+
X4*(X3*X5+X2*X8)*X9)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2-
X1*(-(X10*X3*X4)+X10*X2*X7-X4*X5*X7+X4^2*X8+X3*X5*X9-
X2*X8*X9)*(X4*(X5*X7-X4*X8)^2*
(X5*(X4^2*X6-4*X3*X4*X7+3*X2*X7^2)+
2*X4*(X3*X4-X2*X7)*X8)+
(-(X5^3*(-3*X4^3*X6^2+8*X3*X4^2*X6*X7-11*X3^2*X4*X7^2+
6*X2*X3*X7^3))-
2*X5^2*(2*X3*X4^3*X6+3*X3^2*X4^2*X7-
4*X2*X4^2*X6*X7+2*X2*X3*X4*X7^2-3*X2^2*X7^3)*X8-
X4*X5*(X4^2*(-7*X3^2+2*X2*X6)-2*X2*X3*X4*X7+
7*X2^2*X7^2)*X8^2-4*X2*X4^2*(X3*X4-X2*X7)*X8^3)*X9-
(X5^3*(-(X3^2*X4*X6)+3*X2*X4*X6^2+4*X3^3*X7-
6*X2*X3*X6*X7)-
2*X5^2*(X3^3*X4+2*X2*X3*X4*X6-3*X2^2*X6*X7)*X8-
X2*X5*(-7*X3^2*X4+X2*X4*X6+6*X2*X3*X7)*X8^2-
2*X2^2*(X3*X4-X2*X7)*X8^3)*X9^2+
X10^3*(X3^2-X2*X6)*
(-2*X2*X3*X4*X7+X3^2*(-2*X4^2+3*X2*X9)+
X2*(3*X4^2*X6+X2*(X7^2-3*X6*X9)))+
X10^2*(-5*X3^4*X4*X5*X9+
X3^3*(2*X4^2*(8*X5*X7+3*X4*X8)-
4*X2*(X5*X7+X4*X8)*X9)+
X3^2*(-(X4*(11*X4^2*X5*X6+9*X2*X5*X7^2+
10*X2*X4*X7*X8))+
2*X2*(7*X4*X5*X6+2*X2*X7*X8)*X9)+
X2*(9*X4^3*X5*X6^2+4*X2*X4^2*X6*X7*X8+
3*X2*X4*X5*X6*(X7^2-3*X6*X9)-
2*X2^2*X7*X8*(X7^2+2*X6*X9))+
2*X2*X3*(-5*X4^2*X5*X6*X7-2*X4^3*X6*X8+
X2*X5*X7*(X7^2+2*X6*X9)+X2*X4*X8*(3*X7^2+2*X6*X9)))\
+X10*(-3*X2^2*X5^2*X7^4+4*X2*X4*X5*X7^3*(2*X3*X5+X2*X8)+
2*X4^3*X7*(7*X3*X5^2*X6-4*X5*(X3^2+X2*X6)*X8+
7*X2*X3*X8^2)+
X4^4*(-6*X5^2*X6^2+10*X3*X5*X6*X8-
(6*X3^2+X2*X6)*X8^2)+
7*X2*X7^2*(X3*X5-X2*X8)^2*X9-
2*X4*X7*(X3*X5-X2*X8)*
(3*X3^2*X5+4*X2*X5*X6-7*X2*X3*X8)*X9+
(X3^2-X2*X6)*(4*X3^2*X5^2-2*X2*X3*X5*X8+
X2*(-3*X5^2*X6+X2*X8^2))*X9^2+
X4^2*(-(X7^2*(X5^2*(10*X3^2+3*X2*X6)-2*X2*X3*X5*X8+
7*X2^2*X8^2))+
(X5^2*X6*(4*X3^2+3*X2*X6)-
2*X3*X5*(X3^2+6*X2*X6)*X8+
X2*(5*X3^2+2*X2*X6)*X8^2)*X9)-
3*X11*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))*
(2*X2*X3*X4*X7+X3^2*(-2*X4^2+X2*X9)+
X2*(X4^2*X6-X2*X7^2-X2*X6*X9)))+
3*X11*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))*
(2*X2*X4^2*X7*X8+X4^3*(X5*X6-2*X3*X8)+
2*X2*X7*(X3*X5-X2*X8)*X9-
X4*(X3^2*X5*X9+X2*(-2*X3*X8*X9+X5*(X7^2+X6*X9))))))*
(FV[p2,nu]*FV[p2,rho]*FV[p2,si]*FV[p3,mu]+
FV[p2,mu]*FV[p2,rho]*FV[p2,si]*FV[p3,nu]+
FV[p2,mu]*FV[p2,nu]*FV[p2,si]*FV[p3,rho]+
FV[p2,mu]*FV[p2,nu]*FV[p2,rho]*FV[p3,si]))/
((1-X1)*(3-X1)*(X4^2*X6-2*X3*X4*X7+X2*X7^2+X3^2*X9-X2*X6*X9)^
4)+((-4*X4^5*X5*X6^2*X7*X8^3+2*X4^6*X6^2*X8^4-
2*X3^5*X5*X8^3*X9^3+X2^3*X8^4*X9*(X7^2-X6*X9)^2+
X4^4*X5^2*X6^2*X8^2*(2*X7^2+X6*X9)+
X3^4*X8^2*X9^2*(2*(X5*X7+X4*X8)^2+X5^2*X6*X9)+
X1^2*(X10*X3^2-X10*X2*X6+X4*X5*X6-X3*X5*X7-X3*X4*X8+
X2*X7*X8)^2*(X10*X4*X6-X10*X3*X7+X5*X7^2-X4*X7*X8-
X5*X6*X9+X3*X8*X9)^2-
4*X3^3*X4*X8^2*X9*(2*X7*(X5^2*X7^2-X4*X5*X7*X8+X4^2*X8^2)+
X5*X6*(X5*X7+X4*X8)*X9)+
2*X3*X4^3*X6*X8^2*(-4*X7*(X5*X7-X4*X8)^2-
X5*X6*(2*X5*X7+X4*X8)*X9)+
X10^2*(3*X5^2*X6^2+X11*X6*(X3^2-X2*X6)-6*X3*X5*X6*X8+
(2*X3^2+X2*X6)*X8^2)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2-
2*X10*(2*X11*X5*X6*(X4*X6-X3*X7)-
X11*(2*X3*X4*X6-3*X3^2*X7+X2*X6*X7)*X8+
X8*(3*X5^2*X6*X7-2*X5*(X4*X6+2*X3*X7)*X8+
(2*X3*X4+X2*X7)*X8^2))*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2+
X11^2*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2*
(2*X4^2*X6^2-4*X3*X4*X6*X7+X3^2*(3*X7^2-X6*X9)+
X2*X6*(-X7^2+X6*X9))+
X2^2*X8^2*(X7^2-X6*X9)*
(2*X8^2*(X4*X7-X3*X9)^2-
2*X5*X8*(2*X4*X7+X3*X9)*(X7^2-X6*X9)+
X5^2*(X7^2-X6*X9)*(2*X7^2+X6*X9))+
2*X3^2*X4^2*X8^2*(-8*X4*X5*X7^3*X8+
2*X4^2*X8^2*(2*X7^2+X6*X9)+X5^2*(2*X7^2+X6*X9)^2)-
X11*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2*
(X5^2*X6*(-X7^2+X6*X9)+
X8^2*(4*X4^2*X6-4*X3*X4*X7-X2*X7^2-X3^2*X9+
2*X2*X6*X9)-2*X5*X8*(2*X4*X6*X7-3*X3*X7^2+X3*X6*X9))-
X2*X8^2*(X4^2*X6-2*X3*X4*X7+X3^2*X9)*
(4*X5*X8*(2*X4*X7+X3*X9)*(X7^2-X6*X9)-
2*X5^2*(X7^2-X6*X9)*(2*X7^2+X6*X9)+
X8^2*(2*X3*X4*X7*X9-X3^2*X9^2+X4^2*(-4*X7^2+3*X6*X9)))+
X1*(X7*(X5*X7-X4*X8)^2*
(-(X5^2*X7*(-(X4^2*X6^2)+2*X3*X4*X6*X7+
(-2*X3^2+X2*X6)*X7^2))+
2*X5*(-2*X4^3*X6^2+5*X3*X4^2*X6*X7-4*X3^2*X4*X7^2+
X2*X3*X7^3)*X8+
(4*X3*X4^3*X6-X4^2*(6*X3^2+5*X2*X6)*X7+
10*X2*X3*X4*X7^2-3*X2^2*X7^3)*X8^2)-
(-(X5^4*X6*X7^2*(-2*X4^2*X6^2+4*X3*X4*X6*X7+
(-5*X3^2+3*X2*X6)*X7^2))+
2*X5^3*X7*(-3*X4^3*X6^3+5*X3*X4^2*X6^2*X7+
X4*X6*(-3*X3^2+2*X2*X6)*X7^2+
X3*(-5*X3^2+4*X2*X6)*X7^3)*X8-
X5^2*(-5*X4^4*X6^3+2*X3*X4^3*X6^2*X7+
2*X4^2*X6*(5*X3^2+X2*X6)*X7^2+
2*X3*X4*(-9*X3^2+4*X2*X6)*X7^3+
X2*(-6*X3^2+7*X2*X6)*X7^4)*X8^2+
2*X4*X5*(-5*X3*X4^3*X6^2+
X4^2*X6*(9*X3^2+2*X2*X6)*X7-
X3*X4*(3*X3^2+4*X2*X6)*X7^2+
X2*(-6*X3^2+7*X2*X6)*X7^3)*X8^3+
(5*X3^2*X4^4*X6-2*X3*X4^3*(5*X3^2+2*X2*X6)*X7-
2*X2*X4^2*(-9*X3^2+2*X2*X6)*X7^2-
6*X2^2*X3*X4*X7^3+X2^3*X7^4)*X8^4)*X9+
(-(X5^4*X6^2*(-(X4^2*X6^2)+2*X3*X4*X6*X7+
(-4*X3^2+3*X2*X6)*X7^2))+
2*X5^3*X6*(-2*X3*X4^2*X6^2+X4*X6*(3*X3^2+X2*X6)*X7+
X3*(-7*X3^2+5*X2*X6)*X7^2)*X8+
X5^2*(2*X4^2*X6^2*(X3^2+2*X2*X6)-
2*X3*X4*X6*(-X3^2+7*X2*X6)*X7-
(-9*X3^4-2*X2*X3^2*X6+5*X2^2*X6^2)*X7^2)*X8^2-
2*X5*(2*X3*X4^2*X6*(-X3^2+2*X2*X6)-
X4*(-3*X3^4+3*X2*X3^2*X6+4*X2^2*X6^2)*X7-
X2*X3*(-3*X3^2+X2*X6)*X7^2)*X8^3+
(X3^2*X4^2*(-3*X3^2+4*X2*X6)-
2*X2*X3*X4*(-3*X3^2+4*X2*X6)*X7+X2^3*X6*X7^2)*X8^4)*
X9^2+(-X3^2+X2*X6)*(X5*X6-X3*X8)^2*
(X5^2*X6-2*X3*X5*X8+X2*X8^2)*X9^3+
2*X10^3*(-X3^2+X2*X6)*
(3*X4^3*X6^2*(-(X5*X6)+X3*X8)+
X4^2*X6*X7*(9*X3*X5*X6-7*X3^2*X8-2*X2*X6*X8)-
X7^3*(-4*X3^3*X5+X2*X3*X5*X6+5*X2*X3^2*X8-
2*X2^2*X6*X8)+
X4*X7^2*(-10*X3^2*X5*X6+X2*X5*X6^2+6*X3^3*X8+
3*X2*X3*X6*X8)-
X4*X6*(-X3^2+X2*X6)*(X5*X6-X3*X8)*X9+
(-X3^2+X2*X6)*X7*(X3*X5*X6+X3^2*X8-2*X2*X6*X8)*X9)+
X11*(X7*(4*X4^3*X6*X8*(X5*X6-X3*X8)+
X7^3*(X5^2*(-6*X3^2+X2*X6)+6*X2*X3*X5*X8-
X2^2*X8^2)-
2*X4*X7^2*(-5*X3*X5^2*X6+4*X2*X5*X6*X8+
X2*X3*X8^2)+
X4^2*X7*(-5*X5^2*X6^2-2*X3*X5*X6*X8+
(2*X3^2+5*X2*X6)*X8^2))+
(5*X4^2*X6*(X5*X6-X3*X8)^2+
2*X4*X7*(X5*X6-X3*X8)*
(-5*X3*X5*X6+X3^2*X8+4*X2*X6*X8)+
X7^2*(-(X5^2*X6*(-7*X3^2+2*X2*X6))-
2*X3*X5*(3*X3^2+2*X2*X6)*X8+
X2*(4*X3^2+X2*X6)*X8^2))*X9+
(-X3^2+X2*X6)*(X5*X6-X3*X8)^2*X9^2)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))+
X10^4*(X3^2-X2*X6)^2*
(X4^2*X6^2-2*X3*X4*X6*X7+X3^2*(2*X7^2-X6*X9)+
X2*X6*(-X7^2+X6*X9))+
2*X10*(2*X4^5*X6^2*X8^2*(X5*X6-X3*X8)+
X4^4*X6*X7*X8*(-4*X5^2*X6^2-2*X3*X5*X6*X8+
(X3^2+5*X2*X6)*X8^2)+
X7^5*(X3*X5^3*(-4*X3^2+X2*X6)+
X2*X5^2*(3*X3^2+2*X2*X6)*X8-3*X2^2*X3*X5*X8^2+
X2^3*X8^3)-
X7^3*(X3*X5^3*X6*(-5*X3^2+2*X2*X6)+
X5^2*(9*X3^4-4*X2*X3^2*X6+4*X2^2*X6^2)*X8-
3*X2*X3*X5*(2*X3^2+X2*X6)*X8^2+3*X2^2*X3^2*X8^3)*X9\
+(-X3^2+X2*X6)*X7*(X3*X5^3*X6^2+X5^2*X6*(-5*X3^2+2*X2*X6)*X8+
3*X3^3*X5*X8^2-X2^2*X6*X8^3)*X9^2+
X4^3*(-(X7^2*(-3*X5^3*X6^3-7*X3*X5^2*X6^2*X8+
X5*X6*(-8*X3^2+11*X2*X6)*X8^2+
X3*(-2*X3^2+9*X2*X6)*X8^3))+
X6*(X5*X6-X3*X8)*
(-3*X5^2*X6^2+6*X3*X5*X6*X8+
(-5*X3^2+2*X2*X6)*X8^2)*X9)+
X4^2*X7*(X7^2*(-9*X3*X5^3*X6^2+
X5^2*X6*(-7*X3^2+10*X2*X6)*X8+
X3*X5*(-12*X3^2+13*X2*X6)*X8^2+
X2*(3*X3^2+2*X2*X6)*X8^3)-
(-9*X3*X5^3*X6^3+X5^2*X6^2*(17*X3^2+10*X2*X6)*X8-
X3*X5*X6*(13*X3^2+14*X2*X6)*X8^2+
(9*X3^4-4*X2*X3^2*X6+4*X2^2*X6^2)*X8^3)*X9)+
X4*(-(X7^4*(X5^3*X6*(-10*X3^2+X2*X6)+
X3*X5^2*(-6*X3^2+17*X2*X6)*X8+
X2*X5*(-6*X3^2+X2*X6)*X8^2+3*X2^2*X3*X8^3))+
X7^2*(X5^3*X6^2*(-11*X3^2+2*X2*X6)+
X3*X5^2*X6*(13*X3^2+14*X2*X6)*X8+
X5*(6*X3^4-38*X2*X3^2*X6+5*X2^2*X6^2)*X8^2+
3*X2*X3*(2*X3^2+X2*X6)*X8^3)*X9+
(X3^2-X2*X6)*(-(X5*X6)+X3*X8)*
(-(X5^2*X6^2)+2*X3*X5*X6*X8+3*X3^2*X8^2-
4*X2*X6*X8^2)*X9^2)-
X11*(2*X4^3*X6^2*(X5*X6-X3*X8)-
2*X4*X6*X7^2*(-5*X3^2*X5+2*X2*X5*X6+3*X2*X3*X8)+
X4^2*X6*X7*(-6*X3*X5*X6+X3^2*X8+5*X2*X6*X8)+
X7^3*(-6*X3^3*X5+4*X2*X3*X5*X6+3*X2*X3^2*X8-
X2^2*X6*X8)+
4*X4*X6*(-X3^2+X2*X6)*(X5*X6-X3*X8)*X9-
(X3^2-X2*X6)*X7*(-4*X3*X5*X6+3*X3^2*X8+X2*X6*X8)*
X9)*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))
)+X10^2*(X4^4*X6^2*(4*X5^2*X6^2-8*X3*X5*X6*X8+9*X3^2*X8^2-
5*X2*X6*X8^2)+
2*X4^3*X6*X7*(-8*X3*X5^2*X6^2+
X5*X6*(7*X3^2+9*X2*X6)*X8+X3*(-9*X3^2+X2*X6)*X8^2)\
+X7^4*(-2*X5^2*(-6*X3^4+3*X2*X3^2*X6+X2^2*X6^2)+
2*X2*X3*X5*(-9*X3^2+5*X2*X6)*X8-
X2^2*(-9*X3^2+5*X2*X6)*X8^2)+
2*(-X3^2+X2*X6)*X7^2*
(X5^2*X6*(3*X3^2+2*X2*X6)-
X3*X5*(3*X3^2+7*X2*X6)*X8+X2*(3*X3^2+2*X2*X6)*X8^2
)*X9-(X3^2-X2*X6)^2*(2*X5^2*X6^2-4*X3*X5*X6*X8+
(3*X3^2-X2*X6)*X8^2)*X9^2+
2*X4^2*(X7^2*(-(X5^2*X6^2*(-17*X3^2+5*X2*X6))-
X3*X5*X6*(7*X3^2+17*X2*X6)*X8+
(2*X3^2+X2*X6)*(3*X3^2+X2*X6)*X8^2)+
X6*(-X3^2+X2*X6)*
(5*X5^2*X6^2-10*X3*X5*X6*X8+
(3*X3^2+2*X2*X6)*X8^2)*X9)+
2*X4*X7*(X7^2*(2*X3*X5^2*X6*(-9*X3^2+5*X2*X6)+
X5*(6*X3^4+13*X2*X3^2*X6-3*X2^2*X6^2)*X8+
X2*X3*(-9*X3^2+X2*X6)*X8^2)+
(X3^2-X2*X6)*
(10*X3*X5^2*X6^2-X5*X6*(17*X3^2+3*X2*X6)*X8+
X3*(3*X3^2+7*X2*X6)*X8^2)*X9)+
X11*(X3^2-X2*X6)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))*
(-5*X4^2*X6^2+10*X3*X4*X6*X7+X2*X6*(X7^2-X6*X9)+
X3^2*(-6*X7^2+X6*X9)))))*
(FV[p1,rho]*FV[p1,si]*FV[p3,mu]*FV[p3,nu]+
FV[p1,nu]*FV[p1,si]*FV[p3,mu]*FV[p3,rho]+
FV[p1,mu]*FV[p1,si]*FV[p3,nu]*FV[p3,rho]+
FV[p1,nu]*FV[p1,rho]*FV[p3,mu]*FV[p3,si]+
FV[p1,mu]*FV[p1,rho]*FV[p3,nu]*FV[p3,si]+
FV[p1,mu]*FV[p1,nu]*FV[p3,rho]*FV[p3,si]))/
((1-X1)*(3-X1)*(X4^2*X6-2*X3*X4*X7+X2*X7^2+X3^2*X9-X2*X6*X9)^
4)+((4*X4^5*X5^2*X6^2*X7*X8^2-2*X4^6*X5*X6^2*X8^3+
2*X3^5*X5^2*X8^2*X9^3+X4^4*X5^3*X6^2*X8*(-2*X7^2-X6*X9)-
X2^3*X5*X8^3*X9*(X7^2-X6*X9)^2+
X3^4*X5*X8*X9^2*(-2*(X5*X7+X4*X8)^2-X5^2*X6*X9)+
X1^2*(X10*X3^2-X10*X2*X6+X4*X5*X6-X3*X5*X7-X3*X4*X8+
X2*X7*X8)^2*(X10*X3*X4-X10*X2*X7+X4*X5*X7-X4^2*X8-
X3*X5*X9+X2*X8*X9)*
(-(X10*X4*X6)+X10*X3*X7-X5*X7^2+X4*X7*X8+X5*X6*X9-
X3*X8*X9)+4*X3^3*X4*X5*X8*X9*
(2*X7*(X5^2*X7^2-X4*X5*X7*X8+X4^2*X8^2)+
X5*X6*(X5*X7+X4*X8)*X9)+
2*X3*X4^3*X5*X6*X8*(4*X7*(X5*X7-X4*X8)^2+
X5*X6*(2*X5*X7+X4*X8)*X9)-
X10^2*(3*X3*X5^2*X6+X11*(X3^3-X2*X3*X6)-
2*X5*(2*X3^2+X2*X6)*X8+3*X2*X3*X8^2)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2-
X10*(-3*X5^3*X6*X7+X5^2*(X4*X6+2*X3*X7)*X8+
X5*(2*X3*X4+X2*X7)*X8^2-3*X2*X4*X8^3+
X11*(X3^2*(X5*X7+X4*X8)+3*X2*X6*(X5*X7+X4*X8)-
4*X3*(X4*X5*X6+X2*X7*X8)))*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2+
X11*(2*X5*X7^2*(X3*X5+X2*X8)+2*X4^2*X8*(X5*X6+X3*X8)+
X4*X7*(-3*X5^2*X6-2*X3*X5*X8-3*X2*X8^2)+
(X3*X5^2*X6-3*X3^2*X5*X8+X2*X5*X6*X8+X2*X3*X8^2)*X9)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2+
X2^2*X5*X8*(-X7^2+X6*X9)*
(2*X8^2*(X4*X7-X3*X9)^2-
2*X5*X8*(2*X4*X7+X3*X9)*(X7^2-X6*X9)+
X5^2*(X7^2-X6*X9)*(2*X7^2+X6*X9))-
2*X3^2*X4^2*X5*X8*(-8*X4*X5*X7^3*X8+
2*X4^2*X8^2*(2*X7^2+X6*X9)+X5^2*(2*X7^2+X6*X9)^2)-
X11^2*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2*
(-(X3^2*X4*X7)-3*X2*X4*X6*X7-X3^3*X9+
X3*(2*X4^2*X6+2*X2*X7^2+X2*X6*X9))+
X2*X5*X8*(X4^2*X6-2*X3*X4*X7+X3^2*X9)*
(4*X5*X8*(2*X4*X7+X3*X9)*(X7^2-X6*X9)-
2*X5^2*(X7^2-X6*X9)*(2*X7^2+X6*X9)+
X8^2*(2*X3*X4*X7*X9-X3^2*X9^2+X4^2*(-4*X7^2+3*X6*X9)))+
X1*((X5*X7-X4*X8)^2*
(X5^2*X7*(X4^3*X6^2-4*X3*X4^2*X6*X7+
X4*(2*X3^2+3*X2*X6)*X7^2-2*X2*X3*X7^3)+
2*X5*(X4^4*X6^2-3*X3*X4^3*X6*X7+4*X3^2*X4^2*X7^2-
3*X2*X3*X4*X7^3+X2^2*X7^4)*X8-
X4*(2*X3*X4^3*X6-X4^2*(2*X3^2+3*X2*X6)*X7+
4*X2*X3*X4*X7^2-X2^2*X7^3)*X8^2)-
(-(X5^4*X6*X7*(-(X4^3*X6^2)+5*X3*X4^2*X6*X7-
X4*(X3^2+6*X2*X6)*X7^2+3*X2*X3*X7^3))+
X5^3*(-2*X4^4*X6^3+10*X3*X4^3*X6^2*X7-
X4^2*X6*(X3^2+9*X2*X6)*X7^2-
2*X3*X4*(-4*X3^2+5*X2*X6)*X7^3+
X2*(X3^2+3*X2*X6)*X7^4)*X8-
X5^2*(-(X3*X4^4*X6^2)+X4^3*X6*(13*X3^2+3*X2*X6)*X7-
2*X3*X4^2*(-4*X3^2+19*X2*X6)*X7^2+
X2*X4*(13*X3^2+3*X2*X6)*X7^3-X2^2*X3*X7^4)*X8^2+
X5*(X4^4*X6*(X3^2+3*X2*X6)-
2*X3*X4^3*(-4*X3^2+5*X2*X6)*X7-
X2*X4^2*(X3^2+9*X2*X6)*X7^2+10*X2^2*X3*X4*X7^3-
2*X2^3*X7^4)*X8^3-
X2*X4*(3*X3*X4^3*X6-X4^2*(X3^2+6*X2*X6)*X7+
5*X2*X3*X4*X7^2-X2^2*X7^3)*X8^4)*X9-
(-(X5^4*X6*(-(X3*X4^2*X6^2)+X4*X6*(-X3^2+3*X2*X6)*X7-
X3^3*X7^2))+
X5^3*(X4^2*X6^2*(-7*X3^2+3*X2*X6)+
8*X2*X3*X4*X6^2*X7+X3^2*(-5*X3^2+X2*X6)*X7^2)*X8-
X3*X5^2*(X4^2*X6*(-11*X3^2+5*X2*X6)+
2*X3*X4*(-X3^2+7*X2*X6)*X7+
X2*(-11*X3^2+5*X2*X6)*X7^2)*X8^2+
X5*(X3^2*X4^2*(-5*X3^2+X2*X6)+8*X2^2*X3*X4*X6*X7+
X2^2*(-7*X3^2+3*X2*X6)*X7^2)*X8^3-
X2*(-(X3^3*X4^2)+X2*X4*(-X3^2+3*X2*X6)*X7-
X2^2*X3*X7^2)*X8^4)*X9^2+
(-X3^2+X2*X6)*(-(X3*X5)+X2*X8)*(X5*X6-X3*X8)*
(X5^2*X6-2*X3*X5*X8+X2*X8^2)*X9^3+
X10^4*(X3^2-X2*X6)^2*
(2*X2*X4*X6*X7+X3^3*X9+
X3*(-(X4^2*X6)-X2*X7^2-X2*X6*X9))-
X11*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))*
(2*X4^4*X6*X8*(X5*X6-X3*X8)+
X4*X7^3*(X5^2*(-2*X3^2+3*X2*X6)+2*X2*X3*X5*X8-
3*X2^2*X8^2)+
X4^3*X7*(-3*X5^2*X6^2+2*X3*X5*X6*X8+
(-2*X3^2+3*X2*X6)*X8^2)-
X4*X7*(X5^2*X6*(7*X3^2+3*X2*X6)-
4*X3*X5*(X3^2+4*X2*X6)*X8+
X2*(7*X3^2+3*X2*X6)*X8^2)*X9+
X4^2*(4*X7^2*(X3*X5^2*X6-2*X2*X5*X6*X8+X2*X3*X8^2)+
(X5*X6-X3*X8)*(5*X3*X5*X6-4*X3^2*X8-X2*X6*X8)*X9)\
+(X3*X5-X2*X8)*(-2*X2*X5*X7^4+
X7^2*(4*X3^2*X5+X2*X5*X6-5*X2*X3*X8)*X9+
(X3^2-X2*X6)*(-(X5*X6)+X3*X8)*X9^2))+
X10^3*(-X3^2+X2*X6)*
(3*X3^4*(X5*X7+X4*X8)*X9+
X3^2*(-9*X4^2*X5*X6*X7-5*X2*X5*X7^3-5*X4^3*X6*X8-
9*X2*X4*X7^2*X8-4*X2*X6*(X5*X7+X4*X8)*X9)+
2*X3^3*(X4*X7*(X5*X7+X4*X8)-(X4*X5*X6+X2*X7*X8)*X9)+
X2*X6*(-9*X4^2*X5*X6*X7-X4^3*X6*X8+
X2*X4*X8*(-9*X7^2+X6*X9)+X2*X5*X7*(-X7^2+X6*X9))+
2*X3*(3*X4^3*X5*X6^2+8*X2*X4^2*X6*X7*X8+
X2^2*X7*X8*(3*X7^2+X6*X9)+
X2*X4*X5*X6*(8*X7^2+X6*X9)))+
X10^2*(-4*X3*X4^4*X5^2*X6^3+7*X3^2*X4^3*X5^2*X6^2*X7+
9*X2*X4^3*X5^2*X6^3*X7-7*X3^3*X4^2*X5^2*X6*X7^2-
17*X2*X3*X4^2*X5^2*X6^2*X7^2+6*X3^4*X4*X5^2*X7^3+
13*X2*X3^2*X4*X5^2*X6*X7^3-3*X2^2*X4*X5^2*X6^2*X7^3-
9*X2*X3^3*X5^2*X7^4+5*X2^2*X3*X5^2*X6*X7^4+
8*X3^2*X4^4*X5*X6^2*X8+4*X3^3*X4^3*X5*X6*X7*X8-
36*X2*X3*X4^3*X5*X6^2*X7*X8-16*X3^4*X4^2*X5*X7^2*X8+
40*X2*X3^2*X4^2*X5*X6*X7^2*X8+
24*X2^2*X4^2*X5*X6^2*X7^2*X8+4*X2*X3^3*X4*X5*X7^3*X8-
36*X2^2*X3*X4*X5*X6*X7^3*X8+8*X2^2*X3^2*X5*X7^4*X8-
9*X3^3*X4^4*X6*X8^2+5*X2*X3*X4^4*X6^2*X8^2+
6*X3^4*X4^3*X7*X8^2+13*X2*X3^2*X4^3*X6*X7*X8^2-
3*X2^2*X4^3*X6^2*X7*X8^2-7*X2*X3^3*X4^2*X7^2*X8^2-
17*X2^2*X3*X4^2*X6*X7^2*X8^2+7*X2^2*X3^2*X4*X7^3*X8^2+
9*X2^3*X4*X6*X7^3*X8^2-4*X2^3*X3*X7^4*X8^2+
(X3^2-X2*X6)*(-17*X3^2*(X5*X7+X4*X8)*
(X4*X5*X6+X2*X7*X8)-
3*X2*X6*(X5*X7+X4*X8)*(X4*X5*X6+X2*X7*X8)+
X3^3*(3*X5^2*X7^2+22*X4*X5*X7*X8+3*X4^2*X8^2)+
X3*(X5^2*X6*(10*X4^2*X6+7*X2*X7^2)+
18*X2*X4*X5*X6*X7*X8+
X2*(7*X4^2*X6+10*X2*X7^2)*X8^2))*X9-
(X3^2-X2*X6)^2*
(X3^2*X5*X8+3*X2*X5*X6*X8-2*X3*(X5^2*X6+X2*X8^2))*
X9^2-X11*(X3^2-X2*X6)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))*
(4*X3^2*X4*X7+6*X2*X4*X6*X7+X3^3*X9+
X3*(-5*X4^2*X6-5*X2*X7^2-X2*X6*X9)))+
X10*(-(X2*X5^2*X7^5*(-7*X3^2*X5+3*X2*X5*X6+4*X2*X3*X8))+
X4^5*X6*X8^2*(-4*X3*X5*X6+7*X3^2*X8-3*X2*X6*X8)+
X4^4*X7*(-2*X5^3*X6^3+14*X3*X5^2*X6^2*X8+
X5*X6*(-11*X3^2+9*X2*X6)*X8^2-
2*X3*(3*X3^2+2*X2*X6)*X8^3)+
X7^3*(X5^3*(-X3^4-11*X2*X3^2*X6+6*X2^2*X6^2)+
2*X2*X3*X5^2*(8*X3^2+X2*X6)*X8-
3*X2^2*X5*(5*X3^2+X2*X6)*X8^2+6*X2^3*X3*X8^3)*X9-
(X3^2-X2*X6)*X7*
(-(X5^3*X6*(-X3^2+3*X2*X6))+
2*X3*X5^2*(2*X3^2+X2*X6)*X8+
3*X2*X5*(-3*X3^2+X2*X6)*X8^2+2*X2^2*X3*X8^3)*X9^2+
X4^2*X7*(X7^2*(3*X5^3*X6*(X3^2+X2*X6)+
2*X3*X5^2*(7*X3^2+8*X2*X6)*X8-
X2*X5*(23*X3^2+15*X2*X6)*X8^2+2*X2^2*X3*X8^3)-
(3*X5^3*X6^2*(5*X3^2+X2*X6)-
2*X3*X5^2*X6*(8*X3^2+19*X2*X6)*X8+
X5*(11*X3^4+31*X2*X3^2*X6+12*X2^2*X6^2)*X8^2-
2*X2*X3*(8*X3^2+X2*X6)*X8^3)*X9)+
X4^3*(X7^2*(2*X3*X5^3*X6^2-
X5^2*X6*(23*X3^2+15*X2*X6)*X8+
2*X3*X5*(7*X3^2+8*X2*X6)*X8^2+
3*X2*(X3^2+X2*X6)*X8^3)+
(6*X3*X5^3*X6^3-3*X5^2*X6^2*(5*X3^2+X2*X6)*X8+
2*X3*X5*X6*(8*X3^2+X2*X6)*X8^2+
(-X3^4-11*X2*X3^2*X6+6*X2^2*X6^2)*X8^3)*X9)+
X4*(-(X7^4*(2*X3*X5^3*(3*X3^2+2*X2*X6)-
X2*X5^2*(-11*X3^2+9*X2*X6)*X8-
14*X2^2*X3*X5*X8^2+2*X2^3*X8^3))+
X7^2*(2*X3*X5^3*X6*(8*X3^2+X2*X6)-
X5^2*(11*X3^4+31*X2*X3^2*X6+12*X2^2*X6^2)*X8+
2*X2*X3*X5*(8*X3^2+19*X2*X6)*X8^2-
3*X2^2*(5*X3^2+X2*X6)*X8^3)*X9+
(-X3^2+X2*X6)*
(2*X3*X5^3*X6^2+3*X5^2*X6*(-3*X3^2+X2*X6)*X8+
2*X3*X5*(2*X3^2+X2*X6)*X8^2-
X2*(-X3^2+3*X2*X6)*X8^3)*X9^2)+
X11*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))*
(5*X3^4*(X5*X7+X4*X8)*X9+
X3^2*(-3*X4^2*X5*X6*X7-7*X2*X5*X7^3-7*X4^3*X6*X8-
3*X2*X4*X7^2*X8-2*X2*X6*(X5*X7+X4*X8)*X9)+
3*X2*X6*(-3*X4^2*X5*X6*X7+X2*X5*X7^3+X4^3*X6*X8-
3*X2*X4*X7^2*X8-X2*X6*(X5*X7+X4*X8)*X9)+
2*X3^3*(X4*X7*(X5*X7+X4*X8)-
4*(X4*X5*X6+X2*X7*X8)*X9)+
2*X3*(2*X4^3*X5*X6^2+5*X2*X4^2*X6*X7*X8+
2*X2^2*X7*X8*(X7^2+2*X6*X9)+
X2*X4*X5*X6*(5*X7^2+4*X6*X9))))))*
(FV[p1,si]*FV[p2,rho]*FV[p3,mu]*FV[p3,nu]+
FV[p1,rho]*FV[p2,si]*FV[p3,mu]*FV[p3,nu]+
FV[p1,si]*FV[p2,nu]*FV[p3,mu]*FV[p3,rho]+
FV[p1,nu]*FV[p2,si]*FV[p3,mu]*FV[p3,rho]+
FV[p1,si]*FV[p2,mu]*FV[p3,nu]*FV[p3,rho]+
FV[p1,mu]*FV[p2,si]*FV[p3,nu]*FV[p3,rho]+
FV[p1,rho]*FV[p2,nu]*FV[p3,mu]*FV[p3,si]+
FV[p1,nu]*FV[p2,rho]*FV[p3,mu]*FV[p3,si]+
FV[p1,rho]*FV[p2,mu]*FV[p3,nu]*FV[p3,si]+
FV[p1,mu]*FV[p2,rho]*FV[p3,nu]*FV[p3,si]+
FV[p1,nu]*FV[p2,mu]*FV[p3,rho]*FV[p3,si]+
FV[p1,mu]*FV[p2,nu]*FV[p3,rho]*FV[p3,si]))/
((1-X1)*(3-X1)*(X4^2*X6-2*X3*X4*X7+X2*X7^2+X3^2*X9-X2*X6*X9)^
4)+((-4*X4^5*X5^3*X6^2*X7*X8+2*X4^6*X5^2*X6^2*X8^2-
2*X3^5*X5^3*X8*X9^3+X2^3*X5^2*X8^2*X9*(X7^2-X6*X9)^2+
X4^4*X5^4*X6^2*(2*X7^2+X6*X9)+
X3^4*X5^2*X9^2*(2*(X5*X7+X4*X8)^2+X5^2*X6*X9)+
X1^2*(X10*X3^2-X10*X2*X6+X4*X5*X6-X3*X5*X7-X3*X4*X8+
X2*X7*X8)^2*(X10*X3*X4-X10*X2*X7+X4*X5*X7-X4^2*X8-
X3*X5*X9+X2*X8*X9)^2-
4*X3^3*X4*X5^2*X9*(2*X7*(X5^2*X7^2-X4*X5*X7*X8+X4^2*X8^2)+
X5*X6*(X5*X7+X4*X8)*X9)+
2*X3*X4^3*X5^2*X6*(-4*X7*(X5*X7-X4*X8)^2-
X5*X6*(2*X5*X7+X4*X8)*X9)+
X10^2*(X11*X2*(X3^2-X2*X6)+X5^2*(2*X3^2+X2*X6)-
6*X2*X3*X5*X8+3*X2^2*X8^2)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2-
2*X10*(X5*(X5^2*(X4*X6+2*X3*X7)-2*X5*(2*X3*X4+X2*X7)*X8+
3*X2*X4*X8^2)+
X11*(3*X3^2*X4*X5-2*X2*X3*(X5*X7+X4*X8)+
X2*(-(X4*X5*X6)+2*X2*X7*X8)))*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2+
X2^2*X5^2*(X7^2-X6*X9)*
(2*X8^2*(X4*X7-X3*X9)^2-
2*X5*X8*(2*X4*X7+X3*X9)*(X7^2-X6*X9)+
X5^2*(X7^2-X6*X9)*(2*X7^2+X6*X9))+
X3^2*X4^2*X5^2*(-16*X4*X5*X7^3*X8+
4*X4^2*X8^2*(2*X7^2+X6*X9)+2*X5^2*(2*X7^2+X6*X9)^2)+
X2*X5^2*(X4^2*X6-2*X3*X4*X7+X3^2*X9)*
(-4*X5*X8*(2*X4*X7+X3*X9)*(X7^2-X6*X9)+
2*X5^2*(X7^2-X6*X9)*(2*X7^2+X6*X9)+
X8^2*(-2*X3*X4*X7*X9+X3^2*X9^2+X4^2*(4*X7^2-3*X6*X9)))-
X11^2*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2*
(4*X2*X3*X4*X7+X3^2*(-3*X4^2+X2*X9)+
X2*(X4^2*X6-X2*(2*X7^2+X6*X9)))+
X11*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2*
(4*X4*X5*X7*(X3*X5+X2*X8)+
X4^2*(X5^2*X6-6*X3*X5*X8+X2*X8^2)+X3^2*X5^2*X9-
X2^2*X8^2*X9+2*X2*X5*(X3*X8*X9-X5*(2*X7^2+X6*X9)))+
X1*(-(X4*(X5*X7-X4*X8)^2*
(X5^2*(3*X4^3*X6^2-10*X3*X4^2*X6*X7+
X4*(6*X3^2+5*X2*X6)*X7^2-4*X2*X3*X7^3)-
2*X5*(X3*X4^3*X6-4*X3^2*X4^2*X7+5*X2*X3*X4*X7^2-
2*X2^2*X7^3)*X8+
X4*(X4^2*(-2*X3^2+X2*X6)+2*X2*X3*X4*X7-X2^2*X7^2)*
X8^2))-(-(X5^4*
(-(X4^4*X6^3)+6*X3*X4^3*X6^2*X7+
2*X4^2*X6*(-9*X3^2+2*X2*X6)*X7^2+
2*X3*X4*(5*X3^2+2*X2*X6)*X7^3-5*X2*X3^2*X7^4))+
2*X5^3*X7*(X4^3*X6*(-6*X3^2+7*X2*X6)-
X3*X4^2*(3*X3^2+4*X2*X6)*X7+
X2*X4*(9*X3^2+2*X2*X6)*X7^2-5*X2^2*X3*X7^3)*X8-
X5^2*(X4^4*X6*(-6*X3^2+7*X2*X6)+
2*X3*X4^3*(-9*X3^2+4*X2*X6)*X7+
2*X2*X4^2*(5*X3^2+X2*X6)*X7^2+2*X2^2*X3*X4*X7^3-
5*X2^3*X7^4)*X8^2+
2*X4*X5*(X3*X4^3*(-5*X3^2+4*X2*X6)+
X2*X4^2*(-3*X3^2+2*X2*X6)*X7+5*X2^2*X3*X4*X7^2-
3*X2^3*X7^3)*X8^3-
X2*X4^2*(X4^2*(-5*X3^2+3*X2*X6)+4*X2*X3*X4*X7-
2*X2^2*X7^2)*X8^4)*X9+
(X5^4*(X2*X4^2*X6^3-2*X3*X4*X6*(-3*X3^2+4*X2*X6)*X7+
X3^2*(-3*X3^2+4*X2*X6)*X7^2)+
2*X5^3*(X3*X4^2*X6*(-3*X3^2+X2*X6)+
X4*(-3*X3^4+3*X2*X3^2*X6+4*X2^2*X6^2)*X7-
2*X2*X3*(-X3^2+2*X2*X6)*X7^2)*X8-
X5^2*(X4^2*(-9*X3^4-2*X2*X3^2*X6+5*X2^2*X6^2)+
2*X2*X3*X4*(-X3^2+7*X2*X6)*X7-
2*X2^2*(X3^2+2*X2*X6)*X7^2)*X8^2+
2*X2*X5*(X3*X4^2*(-7*X3^2+5*X2*X6)+
X2*X4*(3*X3^2+X2*X6)*X7-2*X2^2*X3*X7^2)*X8^3-
X2^2*(X4^2*(-4*X3^2+3*X2*X6)+2*X2*X3*X4*X7-
X2^2*X7^2)*X8^4)*X9^2+
(-X3^2+X2*X6)*(X3*X5-X2*X8)^2*
(X5^2*X6-2*X3*X5*X8+X2*X8^2)*X9^3+
2*X10^3*(-X3^2+X2*X6)*
(-5*X3^2*X4^3*X5*X6+2*X2*X4^3*X5*X6^2+6*X3^3*X4^2*X5*X7+
3*X2*X3*X4^2*X5*X6*X7-7*X2*X3^2*X4*X5*X7^2-
2*X2^2*X4*X5*X6*X7^2+3*X2^2*X3*X5*X7^3+4*X3^3*X4^3*X8-
X2*X3*X4^3*X6*X8-10*X2*X3^2*X4^2*X7*X8+
X2^2*X4^2*X6*X7*X8+9*X2^2*X3*X4*X7^2*X8-
3*X2^3*X7^3*X8-
(X3^2-X2*X6)*(X3^2*X4*X5+X2*X3*(X5*X7+X4*X8)+
X2*(-2*X4*X5*X6-X2*X7*X8))*X9)+
2*X10*(X4^4*X7*(-3*X3*X5^3*X6^2-
X5^2*X6*(-6*X3^2+X2*X6)*X8-
X3*X5*(-6*X3^2+17*X2*X6)*X8^2-
X2*(-10*X3^2+X2*X6)*X8^3)+
X4^5*(X5^3*X6^3-3*X3*X5^2*X6^2*X8+
X5*X6*(3*X3^2+2*X2*X6)*X8^2+
X3*(-4*X3^2+X2*X6)*X8^3)+
X4^2*X7*(-(X7^2*
(X3*X5^3*(-2*X3^2+9*X2*X6)+
X2*X5^2*(-8*X3^2+11*X2*X6)*X8-
7*X2^2*X3*X5*X8^2-3*X2^3*X8^3))+
(3*X3*X5^3*X6*(2*X3^2+X2*X6)+
X5^2*(6*X3^4-38*X2*X3^2*X6+5*X2^2*X6^2)*X8+
X2*X3*X5*(13*X3^2+14*X2*X6)*X8^2+
X2^2*(-11*X3^2+2*X2*X6)*X8^3)*X9)+
X4^3*(X7^2*(X5^3*X6*(3*X3^2+2*X2*X6)+
X3*X5^2*(-12*X3^2+13*X2*X6)*X8+
X2*X5*(-7*X3^2+10*X2*X6)*X8^2-9*X2^2*X3*X8^3)-
(3*X3^2*X5^3*X6^2-3*X3*X5^2*X6*(2*X3^2+X2*X6)*X8+
X5*(9*X3^4-4*X2*X3^2*X6+4*X2^2*X6^2)*X8^2+
X2*X3*(-5*X3^2+2*X2*X6)*X8^3)*X9)+
X7*(X3*X5-X2*X8)*
(-2*X2^2*X5^2*X7^4-
X2*X7^2*(X5^2*(-5*X3^2+2*X2*X6)+6*X2*X3*X5*X8-
3*X2^2*X8^2)*X9+
(X3^2-X2*X6)*
(3*X3^2*X5^2-4*X2*X5^2*X6+2*X2*X3*X5*X8-
X2^2*X8^2)*X9^2)+
X4*(X2*X5*X7^4*
(X5^2*(X3^2+5*X2*X6)-2*X2*X3*X5*X8-4*X2^2*X8^2)\
-X7^2*(X5^3*(9*X3^4-4*X2*X3^2*X6+4*X2^2*X6^2)-
X2*X3*X5^2*(13*X3^2+14*X2*X6)*X8+
X2^2*X5*(17*X3^2+10*X2*X6)*X8^2-9*X2^3*X3*X8^3)*
X9+(-X3^2+X2*X6)*
(-(X2*X5^3*X6^2)+3*X3^3*X5^2*X8+
X2*X5*(-5*X3^2+2*X2*X6)*X8^2+X2^2*X3*X8^3)*X9^2)\
+X11*(-3*X3^2*X4^3*X5*X6+X2*X4^3*X5*X6^2+6*X2*X3*X4^2*X5*X6*X7-
X2*X3^2*X4*X5*X7^2-5*X2^2*X4*X5*X6*X7^2+
2*X2^2*X3*X5*X7^3+6*X3^3*X4^3*X8-
4*X2*X3*X4^3*X6*X8-10*X2*X3^2*X4^2*X7*X8+
4*X2^2*X4^2*X6*X7*X8+6*X2^2*X3*X4*X7^2*X8-
2*X2^3*X7^3*X8+
(X3^2-X2*X6)*
(3*X3^2*X4*X5-4*X2*X3*(X5*X7+X4*X8)+
X2*(X4*X5*X6+4*X2*X7*X8))*X9)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9)))+
X10^4*(X3^2-X2*X6)^2*
(-2*X2*X3*X4*X7+X3^2*(2*X4^2-X2*X9)+
X2*(-(X4^2*X6)+X2*(X7^2+X6*X9)))+
X10^2*(9*X3^2*X4^4*X5^2*X6^2-5*X2*X4^4*X5^2*X6^3-
18*X3^3*X4^3*X5^2*X6*X7+2*X2*X3*X4^3*X5^2*X6^2*X7+
12*X3^4*X4^2*X5^2*X7^2+10*X2*X3^2*X4^2*X5^2*X6*X7^2+
2*X2^2*X4^2*X5^2*X6^2*X7^2-18*X2*X3^3*X4*X5^2*X7^3+
2*X2^2*X3*X4*X5^2*X6*X7^3+9*X2^2*X3^2*X5^2*X7^4-
5*X2^3*X5^2*X6*X7^4-18*X3^3*X4^4*X5*X6*X8+
10*X2*X3*X4^4*X5*X6^2*X8+12*X3^4*X4^3*X5*X7*X8+
26*X2*X3^2*X4^3*X5*X6*X7*X8-6*X2^2*X4^3*X5*X6^2*X7*X8-
14*X2*X3^3*X4^2*X5*X7^2*X8-
34*X2^2*X3*X4^2*X5*X6*X7^2*X8+
14*X2^2*X3^2*X4*X5*X7^3*X8+18*X2^3*X4*X5*X6*X7^3*X8-
8*X2^3*X3*X5*X7^4*X8+12*X3^4*X4^4*X8^2-
6*X2*X3^2*X4^4*X6*X8^2-2*X2^2*X4^4*X6^2*X8^2-
36*X2*X3^3*X4^3*X7*X8^2+20*X2^2*X3*X4^3*X6*X7*X8^2+
34*X2^2*X3^2*X4^2*X7^2*X8^2-10*X2^3*X4^2*X6*X7^2*X8^2-
16*X2^3*X3*X4*X7^3*X8^2+4*X2^4*X7^4*X8^2+
2*(-X3^2+X2*X6)*
(-3*X3^3*X4*X5*(X5*X7+X4*X8)-
X2*X3*(X5*X7+X4*X8)*(7*X4*X5*X6+10*X2*X7*X8)+
X3^2*(3*X5^2*(X4^2*X6+X2*X7^2)+
17*X2*X4*X5*X7*X8+3*X2*X4^2*X8^2)+
X2*(2*X5^2*X6*(X4^2*X6+X2*X7^2)+
3*X2*X4*X5*X6*X7*X8+
X2*(2*X4^2*X6+5*X2*X7^2)*X8^2))*X9+
(X3^2-X2*X6)^2*
(X5^2*(-3*X3^2+X2*X6)+4*X2*X3*X5*X8-2*X2^2*X8^2)*
X9^2+X11*(X3^2-X2*X6)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))*
(10*X2*X3*X4*X7+X3^2*(-6*X4^2+X2*X9)+
X2*(X4^2*X6-X2*(5*X7^2+X6*X9))))-
X11*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))*
(2*X4^3*X7*(X3*X5^2*X6+4*X2*X5*X6*X8-5*X2*X3*X8^2)+
X4^4*(X5^2*X6^2-6*X3*X5*X6*X8+(6*X3^2-X2*X6)*X8^2)+
(X3*X5-X2*X8)^2*X9*(X3^2*X9-X2*(5*X7^2+X6*X9))+
2*X4*X7*(X3*X5-X2*X8)*
(X3^2*X5*X9+X2*(-5*X3*X8*X9+2*X5*(X7^2+2*X6*X9)))+
X4^2*(6*X3^3*X5*X8*X9+2*X2*X3*X5*X8*(X7^2+2*X6*X9)+
X3^2*(-7*X2*X8^2*X9-2*X5^2*(X7^2+2*X6*X9))+
X2*(X5^2*X6*(-5*X7^2-X6*X9)+
X2*X8^2*(5*X7^2+2*X6*X9))))))*
(FV[p2,rho]*FV[p2,si]*FV[p3,mu]*FV[p3,nu]+
FV[p2,nu]*FV[p2,si]*FV[p3,mu]*FV[p3,rho]+
FV[p2,mu]*FV[p2,si]*FV[p3,nu]*FV[p3,rho]+
FV[p2,nu]*FV[p2,rho]*FV[p3,mu]*FV[p3,si]+
FV[p2,mu]*FV[p2,rho]*FV[p3,nu]*FV[p3,si]+
FV[p2,mu]*FV[p2,nu]*FV[p3,rho]*FV[p3,si]))/
((1-X1)*(3-X1)*(X4^2*X6-2*X3*X4*X7+X2*X7^2+X3^2*X9-X2*X6*X9)^
4)+((3*X4^4*X5^3*X6^3*X7*X8-3*X4^5*X5^2*X6^3*X8^2+
6*X3*X4^3*X5*X6^2*X8*(-(X5*X7)+X4*X8)*(2*X5*X7+X4*X8)+
6*X3^5*X5*X8^2*(-(X5*X7)+X4*X8)*X9^2+
3*X2^3*X8^3*(X5*X7-X4*X8)*(X7^2-X6*X9)^2-
6*X3^2*X4^2*X5*X6*X8*(-(X5*X7)+X4*X8)*
(2*X5*X7^2+4*X4*X7*X8+X5*X6*X9)+
3*X3^4*X5*X8*(X5*X7-X4*X8)*X9*(8*X4*X7*X8+X5*X6*X9)+
X1^2*(-(X10*X3^2)+X10*X2*X6-X4*X5*X6+X3*X5*X7+
X3*X4*X8-X2*X7*X8)^3*
(-(X10*X4*X6)+X10*X3*X7-X5*X7^2+X4*X7*X8+X5*X6*X9-
X3*X8*X9)-3*X11^2*(X3^2-X2*X6)*(-(X4*X6)+X3*X7)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2-
3*X10*(X5*X6-X3*X8)*
(X5^2*X6+X11*(X3^2-X2*X6)-2*X3*X5*X8+X2*X8^2)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2+
3*X11*(X5^2*X6*(X4*X6-X3*X7)-
X5*(2*X3*X4*X6-3*X3^2*X7+X2*X6*X7)*X8+
(-(X3^2*X4)+2*X2*X4*X6-X2*X3*X7)*X8^2)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2-
3*X2^2*X8*(-(X5*X7)+X4*X8)*(X7^2-X6*X9)*
(2*X8^2*(X4^2*X6-2*X3*X4*X7+X3^2*X9)+
X5^2*X6*(X7^2-X6*X9)+2*X3*X5*X8*(-X7^2+X6*X9))-
3*X2*X8*(-(X5*X7)+X4*X8)*(X4^2*X6-2*X3*X4*X7+X3^2*X9)*
(X8^2*(X4^2*X6-2*X3*X4*X7+X3^2*X9)+
2*X5^2*X6*(X7^2-X6*X9)+4*X3*X5*X8*(-X7^2+X6*X9))+
12*X3^3*X4*X5*X8*(-(X5*X7)+X4*X8)*
(X5*X6*X7*X9+X4*X8*(2*X7^2+X6*X9))-
X1*(X4*X5*X6+X10*(X3^2-X2*X6)-X3*X5*X7-X3*X4*X8+
X2*X7*X8)*(2*X10^3*(X3^2-X2*X6)^2*(-(X4*X6)+X3*X7)+
(X5*X7-X4*X8)*(X5^2*X7*
(X4^2*X6^2-2*X3*X4*X6*X7+(-2*X3^2+3*X2*X6)*X7^2)-
X5*(-3*X4^3*X6^2+11*X3*X4^2*X6*X7+
X4*(-14*X3^2+X2*X6)*X7^2+5*X2*X3*X7^3)*X8-
(3*X3*X4^3*X6-2*X4^2*(2*X3^2+3*X2*X6)*X7+
11*X2*X3*X4*X7^2-4*X2^2*X7^3)*X8^2)-
(X5^3*X6*(X4^2*X6^2-2*X3*X4*X6*X7+
(-5*X3^2+6*X2*X6)*X7^2)-
X5^2*(3*X3*X4^2*X6^2+2*X4*X6*(-5*X3^2+2*X2*X6)*X7+
X3*(-11*X3^2+14*X2*X6)*X7^2)*X8+
X5*(3*X3^2*X4^2*X6+2*X3*X4*(-7*X3^2+4*X2*X6)*X7+
X2*(-4*X3^2+7*X2*X6)*X7^2)*X8^2-
(X3^3*X4^2+2*X2*X4*(-4*X3^2+3*X2*X6)*X7+
X2^2*X3*X7^2)*X8^3)*X9+
3*(-X3^2+X2*X6)*(X5*X6-X3*X8)*
(X5^2*X6-2*X3*X5*X8+X2*X8^2)*X9^2+
X10^2*(-X3^2+X2*X6)*
(7*X4^2*X6*(X5*X6-X3*X8)+
X7^2*(6*X3^2*X5+X2*X5*X6-7*X2*X3*X8)+
2*X4*X7*(-7*X3*X5*X6+4*X3^2*X8+3*X2*X6*X8)-
(X3^2-X2*X6)*(-(X5*X6)+X3*X8)*X9)+
3*X11*(2*X4*X6*X7*(-(X3*X5)+X2*X8)+
X4^2*X6*(X5*X6-X3*X8)-
X7^2*(-2*X3^2*X5+X2*X5*X6+X2*X3*X8)+
(X3^2-X2*X6)*(-(X5*X6)+X3*X8)*X9)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))+
2*X10*(-(X4^3*X5^2*X6^3)+3*X3*X4^2*X5^2*X6^2*X7-
5*X3^2*X4*X5^2*X6*X7^2+2*X2*X4*X5^2*X6^2*X7^2+
3*X3^3*X5^2*X7^3-2*X2*X3*X5^2*X6*X7^3+
2*X3*X4^3*X5*X6^2*X8+X3^2*X4^2*X5*X6*X7*X8-
7*X2*X4^2*X5*X6^2*X7*X8-4*X3^3*X4*X5*X7^2*X8+
10*X2*X3*X4*X5*X6*X7^2*X8-X2*X3^2*X5*X7^3*X8-
X2^2*X5*X6*X7^3*X8-4*X3^2*X4^3*X6*X8^2+
3*X2*X4^3*X6^2*X8^2+5*X3^3*X4^2*X7*X8^2-
2*X2*X3*X4^2*X6*X7*X8^2-3*X2*X3^2*X4*X7^2*X8^2+
X2^2*X3*X7^3*X8^2-
(X3^2-X2*X6)*(-2*X5^2*X6*(X4*X6-X3*X7)+
X5*(4*X3*X4*X6-5*X3^2*X7+X2*X6*X7)*X8-
(-(X3^2*X4)+3*X2*X4*X6-2*X2*X3*X7)*X8^2)*X9+
3*X11*(X3^2-X2*X6)*(X4*X6-X3*X7)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9)))))*
(FV[p1,si]*FV[p3,mu]*FV[p3,nu]*FV[p3,rho]+
FV[p1,rho]*FV[p3,mu]*FV[p3,nu]*FV[p3,si]+
FV[p1,nu]*FV[p3,mu]*FV[p3,rho]*FV[p3,si]+
FV[p1,mu]*FV[p3,nu]*FV[p3,rho]*FV[p3,si]))/
((1-X1)*(3-X1)*(X4^2*X6-2*X3*X4*X7+X2*X7^2+X3^2*X9-X2*X6*X9)^
4)+((-81*X4^4*X5^4*X6^3*X7+81*X4^5*X5^3*X6^3*X8+
162*X3*X4^3*X5^2*X6^2*(X5*X7-X4*X8)*(2*X5*X7+X4*X8)+
162*X3^5*X5^2*X8*(X5*X7-X4*X8)*X9^2-
81*X2^3*X5*X8^2*(X5*X7-X4*X8)*(X7^2-X6*X9)^2+
162*X3^2*X4^2*X5^2*X6*(-(X5*X7)+X4*X8)*
(2*X5*X7^2+4*X4*X7*X8+X5*X6*X9)+
81*X3^4*X5^2*(-(X5*X7)+X4*X8)*X9*(8*X4*X7*X8+X5*X6*X9)+
X1^5*(X10*X3^2-X10*X2*X6+X4*X5*X6-X3*X5*X7-X3*X4*X8+
X2*X7*X8)^3*(X10*X3*X4-X10*X2*X7+X4*X5*X7-X4^2*X8-
X3*X5*X9+X2*X8*X9)-
81*X11^2*(X3^2-X2*X6)*(X3*X4-X2*X7)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2+
81*X10*(X3*X5-X2*X8)*
(X5^2*X6+X11*(X3^2-X2*X6)-2*X3*X5*X8+X2*X8^2)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2+
81*X11*(-(X5^2*(X3*X4*X6+X3^2*X7-2*X2*X6*X7))-
X5*(-3*X3^2*X4+X2*X4*X6+2*X2*X3*X7)*X8-
X2*(X3*X4-X2*X7)*X8^2)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2-
81*X2^2*X5*(X5*X7-X4*X8)*(X7^2-X6*X9)*
(2*X8^2*(X4^2*X6-2*X3*X4*X7+X3^2*X9)+
X5^2*X6*(X7^2-X6*X9)+2*X3*X5*X8*(-X7^2+X6*X9))-
81*X2*X5*(X5*X7-X4*X8)*(X4^2*X6-2*X3*X4*X7+X3^2*X9)*
(X8^2*(X4^2*X6-2*X3*X4*X7+X3^2*X9)+
2*X5^2*X6*(X7^2-X6*X9)+4*X3*X5*X8*(-X7^2+X6*X9))-
324*X3^3*X4*X5^2*(-(X5*X7)+X4*X8)*
(X5*X6*X7*X9+X4*X8*(2*X7^2+X6*X9))+
X1^4*(X4*X5*X6+X10*(X3^2-X2*X6)-X3*X5*X7-X3*X4*X8+
X2*X7*X8)*(5*X10^3*(X3^2-X2*X6)^2*(X3*X4-X2*X7)-
(X5*X7-X4*X8)*(X5^2*
(X4^3*X6^2-5*X3*X4^2*X6*X7+
X4*(X3^2+6*X2*X6)*X7^2-3*X2*X3*X7^3)+
X5*(X3*X4^3*X6+8*X3^2*X4^2*X7-7*X2*X4^2*X6*X7-
5*X2*X3*X4*X7^2+3*X2^2*X7^3)*X8+
X4*(X4^2*(-5*X3^2+3*X2*X6)+4*X2*X3*X4*X7-
2*X2^2*X7^2)*X8^2)-
2*(X5^3*(X3*X4^2*X6^2+X3^2*X4*X6*X7-3*X2*X4*X6^2*X7+
X3^3*X7^2)+
X5^2*(X4^2*X6*(-5*X3^2+2*X2*X6)+
2*X3*X4*(-2*X3^2+5*X2*X6)*X7-3*X2*X3^2*X7^2)*X8-
X5*(X3*X4^2*(-7*X3^2+4*X2*X6)+
X2*X4*(X3^2+5*X2*X6)*X7-3*X2^2*X3*X7^2)*X8^2+
X2*(X4^2*(-4*X3^2+3*X2*X6)+2*X2*X3*X4*X7-X2^2*X7^2)*
X8^3)*X9-3*(-X3^2+X2*X6)*(X3*X5-X2*X8)*
(X5^2*X6-2*X3*X5*X8+X2*X8^2)*X9^2-
3*X11*(X3*X4^2*X5*X6-2*X2*X4*X5*X6*X7+X2*X3*X5*X7^2-
2*X3^2*X4^2*X8+X2*X4^2*X6*X8+2*X2*X3*X4*X7*X8-
X2^2*X7^2*X8+(-X3^2+X2*X6)*(X3*X5-X2*X8)*X9)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))+
X10*(5*X3*X4^3*X5^2*X6^2-6*X3^2*X4^2*X5^2*X6*X7-
9*X2*X4^2*X5^2*X6^2*X7+7*X3^3*X4*X5^2*X7^2+
8*X2*X3*X4*X5^2*X6*X7^2-11*X2*X3^2*X5^2*X7^3+
6*X2^2*X5^2*X6*X7^3-14*X3^2*X4^3*X5*X6*X8+
4*X2*X4^3*X5*X6^2*X8-2*X3^3*X4^2*X5*X7*X8+
32*X2*X3*X4^2*X5*X6*X7*X8-4*X2*X3^2*X4*X5*X7^2*X8-
26*X2^2*X4*X5*X6*X7^2*X8+10*X2^2*X3*X5*X7^3*X8+
15*X3^3*X4^3*X8^2-10*X2*X3*X4^3*X6*X8^2-
25*X2*X3^2*X4^2*X7*X8^2+10*X2^2*X4^2*X6*X7*X8^2+
15*X2^2*X3*X4*X7^2*X8^2-5*X2^3*X7^3*X8^2+
2*(X3^2-X2*X6)*
(X5^2*(-5*X3*X4*X6+2*X3^2*X7+3*X2*X6*X7)+
2*X5*(4*X3^2*X4+X2*X4*X6-5*X2*X3*X7)*X8-
5*X2*(X3*X4-X2*X7)*X8^2)*X9-
6*X11*(X3^2-X2*X6)*(X3*X4-X2*X7)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9)))+
X10^2*(-X3^2+X2*X6)*
(-13*X3*(X4^2*X5*X6+X2*X5*X7^2+2*X2*X4*X7*X8)+
2*X3^3*X5*X9-2*X2*X3*X5*X6*X9+
X3^2*(11*X4*X5*X7+15*X4^2*X8-2*X2*X8*X9)+
X2*(15*X4*X5*X6*X7-2*X4^2*X6*X8+
X2*X8*(13*X7^2+2*X6*X9))))+
9*X1^2*(-5*X10^4*(X3^2-X2*X6)^3*(X3*X4-X2*X7)+
X5^4*X7*(2*X4^4*X6^3-10*X3*X4^3*X6^2*X7+
2*X4^2*X6*(5*X3^2+4*X2*X6)*X7^2-
X3*X4*(X3^2+13*X2*X6)*X7^3+X2*(3*X3^2+X2*X6)*X7^4)-
2*X5^3*(X4*X6-X3*X7)*
(X4^4*X6^2-3*X3*X4^3*X6*X7+
X4^2*(-8*X3^2+7*X2*X6)*X7^2+7*X2*X3*X4*X7^3-
4*X2^2*X7^4)*X8+
2*X5^2*(X3*X4^5*X6^2+X4^4*X6*(-13*X3^2+5*X2*X6)*X7+
X3*X4^3*(7*X3^2+9*X2*X6)*X7^2-
2*X2*X4^2*(X3^2+4*X2*X6)*X7^3-X2^2*X3*X4*X7^4+
2*X2^3*X7^5)*X8^2-
2*X4*X5*(X4^4*X6*(-3*X3^2+2*X2*X6)+
4*X3*X4^3*(-X3^2+X2*X6)*X7-
X2*X4^2*(-11*X3^2+5*X2*X6)*X7^2-8*X2^2*X3*X4*X7^3+
3*X2^3*X7^4)*X8^3+
X4^2*(X3*X4-X2*X7)*
(X4^2*(-5*X3^2+3*X2*X6)+4*X2*X3*X4*X7-2*X2^2*X7^2)*X8^4\
+2*(X5^4*(X3*X4^3*X6^3-X4^2*X6^2*(-X3^2+4*X2*X6)*X7+
X3*X4*X6*(-2*X3^2+5*X2*X6)*X7^2-
(X3^4-X2*X3^2*X6+X2^2*X6^2)*X7^3)+
X5^3*(X4*X6-X3*X7)*
(X4^2*X6*(-7*X3^2+3*X2*X6)+
X3*X4*(-7*X3^2+15*X2*X6)*X7-
2*X2*(X3^2+X2*X6)*X7^2)*X8-
X5^2*(2*X3*X4^3*X6*(-7*X3^2+4*X2*X6)+
X4^2*(7*X3^4+7*X2*X3^2*X6+4*X2^2*X6^2)*X7-
18*X2^2*X3*X4*X6*X7^2+X2^2*(5*X3^2+X2*X6)*X7^3)*
X8^2+X5*(X4^3*(-7*X3^4-X2*X3^2*X6+4*X2^2*X6^2)-
2*X2*X3*X4^2*(-7*X3^2+X2*X6)*X7-
X2^2*X4*(7*X3^2+5*X2*X6)*X7^2+4*X2^3*X3*X7^3)*X8^3\
-X2*(-(X3*X4)+X2*X7)*(4*X3^2*X4^2-2*X2*X3*X4*X7+
X2*(-3*X4^2*X6+X2*X7^2))*X8^4)*X9+
(-X3^2+X2*X6)*(X5^2*X6-2*X3*X5*X8+X2*X8^2)*
(X5^2*(3*X3*X4*X6-4*X3^2*X7+X2*X6*X7)-
2*X5*(X3^2*X4+2*X2*X4*X6-3*X2*X3*X7)*X8+
3*X2*(X3*X4-X2*X7)*X8^2)*X9^2+
X11^2*(X3^2-X2*X6)*(X3*X4-X2*X7)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2+
6*X10^2*(-X3^2+X2*X6)*
(3*X3*X4^3*X5^2*X6^2-5*X3^2*X4^2*X5^2*X6*X7-
4*X2*X4^2*X5^2*X6^2*X7+3*X3^3*X4*X5^2*X7^2+
6*X2*X3*X4*X5^2*X6*X7^2-4*X2*X3^2*X5^2*X7^3+
X2^2*X5^2*X6*X7^3-7*X3^2*X4^3*X5*X6*X8+
X2*X4^3*X5*X6^2*X8+4*X3^3*X4^2*X5*X7*X8+
14*X2*X3*X4^2*X5*X6*X7*X8-9*X2*X3^2*X4*X5*X7^2*X8-
9*X2^2*X4*X5*X6*X7^2*X8+6*X2^2*X3*X5*X7^3*X8+
5*X3^3*X4^3*X8^2-2*X2*X3*X4^3*X6*X8^2-
11*X2*X3^2*X4^2*X7*X8^2+2*X2^2*X4^2*X6*X7*X8^2+
9*X2^2*X3*X4*X7^2*X8^2-3*X2^3*X7^3*X8^2+
(X3^2-X2*X6)*
(X5^2*(-2*X3*X4*X6+X3^2*X7+X2*X6*X7)+
X5*(3*X3^2*X4+X2*X4*X6-4*X2*X3*X7)*X8-
2*X2*(X3*X4-X2*X7)*X8^2)*X9-
X11*(X3^2-X2*X6)*(X3*X4-X2*X7)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9)))+
2*X10^3*(X3^2-X2*X6)^2*
(X3^3*X5*X9-X3*
(9*(X4^2*X5*X6+X2*X5*X7^2+2*X2*X4*X7*X8)+
X2*X5*X6*X9)+
X3^2*(2*X4*(4*X5*X7+5*X4*X8)-X2*X8*X9)+
X2*(10*X4*X5*X6*X7-X4^2*X6*X8+X2*X8*(9*X7^2+X6*X9)))\
+2*X10*(-3*X3*X4^4*X5^3*X6^3+8*X3^2*X4^3*X5^3*X6^2*X7+
4*X2*X4^3*X5^3*X6^3*X7-11*X3^3*X4^2*X5^3*X6*X7^2-
7*X2*X3*X4^2*X5^3*X6^2*X7^2+4*X3^4*X4*X5^3*X7^3+
14*X2*X3^2*X4*X5^3*X6*X7^3-6*X2^2*X4*X5^3*X6^2*X7^3-
7*X2*X3^3*X5^3*X7^4+4*X2^2*X3*X5^3*X6*X7^4+
10*X3^2*X4^4*X5^2*X6^2*X8-X2*X4^4*X5^2*X6^3*X8-
10*X3^3*X4^3*X5^2*X6*X7*X8-
26*X2*X3*X4^3*X5^2*X6^2*X7*X8+
10*X3^4*X4^2*X5^2*X7^2*X8+
19*X2*X3^2*X4^2*X5^2*X6*X7^2*X8+
25*X2^2*X4^2*X5^2*X6^2*X7^2*X8-
16*X2*X3^3*X4*X5^2*X7^3*X8-
20*X2^2*X3*X4*X5^2*X6*X7^3*X8+
13*X2^2*X3^2*X5^2*X7^4*X8-4*X2^3*X5^2*X6*X7^4*X8-
15*X3^3*X4^4*X5*X6*X8^2+6*X2*X3*X4^4*X5*X6^2*X8^2+
48*X2*X3^2*X4^3*X5*X6*X7*X8^2-
12*X2^2*X4^3*X5*X6^2*X7*X8^2-
15*X2*X3^3*X4^2*X5*X7^2*X8^2-
39*X2^2*X3*X4^2*X5*X6*X7^2*X8^2+
18*X2^2*X3^2*X4*X5*X7^3*X8^2+
18*X2^3*X4*X5*X6*X7^3*X8^2-9*X2^3*X3*X5*X7^4*X8^2+
10*X3^4*X4^4*X8^3-9*X2*X3^2*X4^4*X6*X8^3+
2*X2^2*X4^4*X6^2*X8^3-22*X2*X3^3*X4^3*X7*X8^3+
10*X2^2*X3*X4^3*X6*X7*X8^3+23*X2^2*X3^2*X4^2*X7^2*X8^3-
5*X2^3*X4^2*X6*X7^2*X8^3-12*X2^3*X3*X4*X7^3*X8^3+
3*X2^4*X7^4*X8^3+
(X3^2-X2*X6)*
(X5^3*(5*X3*X4^2*X6^2-
2*X4*X6*(2*X3^2+3*X2*X6)*X7+
X3*(3*X3^2+2*X2*X6)*X7^2)+
X5^2*(X4^2*X6*(-16*X3^2+X2*X6)+
2*X3*X4*(X3^2+14*X2*X6)*X7-
X2*(13*X3^2+2*X2*X6)*X7^2)*X8-
3*X5*(-5*X3^3*X4^2+
2*X2*X4*(3*X3^2+2*X2*X6)*X7-5*X2^2*X3*X7^2)*
X8^2+X2*(X4^2*(-9*X3^2+4*X2*X6)+
10*X2*X3*X4*X7-5*X2^2*X7^2)*X8^3)*X9-
2*(X3^2-X2*X6)^2*(X3*X5-X2*X8)*
(X5^2*X6-2*X3*X5*X8+X2*X8^2)*X9^2-
2*X11*(X3^2-X2*X6)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))*
(X3^3*X5*X9+X3^2*(X4*X5*X7+3*X4^2*X8-X2*X8*X9)-
X3*(2*X4^2*X5*X6+4*X2*X4*X7*X8+
X2*X5*(2*X7^2+X6*X9))+
X2*(3*X4*X5*X6*X7-X4^2*X6*X8+X2*X8*(2*X7^2+X6*X9))
))+2*X11*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))*
(2*X3^4*X5^2*X7*X9+
X3^2*(-6*X4^3*X5*X6*X8-X4^2*X7*(2*X5^2*X6+7*X2*X8^2)+
2*X2*X4*X5*X8*(-4*X7^2+X6*X9)+
X2*X7*(X2*X8^2*X9+X5^2*(-X7^2-3*X6*X9)))+
X3^3*(6*X4^2*X5*X7*X8+3*X4^3*X8^2-2*X2*X5*X7*X8*X9-
X4*(X2*X8^2*X9+X5^2*(X7^2+X6*X9)))+
X2*(2*X4^3*X5*X6^2*X8+
X4^2*X6*X7*(-4*X5^2*X6+X2*X8^2)-
2*X2*X4*X5*X6*X8*(2*X7^2+X6*X9)-
X2*X7*(X5^2*X6*(X7^2-X6*X9)+
X2*X8^2*(2*X7^2+X6*X9)))+
X3*(6*X2*X4^2*X5*X6*X7*X8+X4^3*X6*(2*X5^2*X6-X2*X8^2)+
2*X2^2*X5*X7*X8*(2*X7^2+X6*X9)+
X2*X4*(X2*X8^2*(6*X7^2+X6*X9)+
X5^2*X6*(7*X7^2+X6*X9)))))+
3*X1^3*(X10^4*(-X3^2+X2*X6)^3*(X3*X4-X2*X7)+
(X5*X7-X4*X8)*(X5^3*
(-6*X4^4*X6^3+20*X3*X4^3*X6^2*X7-
4*X4^2*X6*(5*X3^2+X2*X6)*X7^2+
X3*X4*(7*X3^2+5*X2*X6)*X7^3+
X2*(-3*X3^2+X2*X6)*X7^4)-
X5^2*(-16*X3*X4^4*X6^2+
4*X4^3*X6*(10*X3^2+3*X2*X6)*X7-
X3*X4^2*(19*X3^2+41*X2*X6)*X7^2+
X2*X4*(19*X3^2+9*X2*X6)*X7^3-4*X2^2*X3*X7^4)*X8-
X5*(2*X4^4*X6*(6*X3^2+X2*X6)-
X3*X4^3*(21*X3^2+23*X2*X6)*X7+
X2*X4^2*(41*X3^2+7*X2*X6)*X7^2-
20*X2^2*X3*X4*X7^3+2*X2^3*X7^4)*X8^2+
X4*(X3*X4-X2*X7)*
(X4^2*(X3^2+3*X2*X6)-8*X2*X3*X4*X7+4*X2^2*X7^2)*X8^3
)+2*(X5^4*(2*X3*X4^3*X6^3+2*X4^2*X6^2*(-4*X3^2+X2*X6)*X7-
X3*X4*X6*(-7*X3^2+X2*X6)*X7^2-
(2*X3^4-X2*X3^2*X6+X2^2*X6^2)*X7^3)+
2*X5^3*(-2*X4^3*X6^2*(X3^2+X2*X6)+
3*X3*X4^2*X6*(3*X3^2+X2*X6)*X7+
2*X4*(-2*X3^4-5*X2*X3^2*X6+X2^2*X6^2)*X7^2+
X2*X3*(3*X3^2+X2*X6)*X7^3)*X8+
X5^2*(X3*X4^3*X6*(-X3^2+13*X2*X6)-
X4^2*(2*X3^2+X2*X6)*(5*X3^2+7*X2*X6)*X7+
9*X2*X3*X4*(3*X3^2+X2*X6)*X7^2-
X2^2*(11*X3^2+X2*X6)*X7^3)*X8^2-
2*X5*(X4^3*(-2*X3^4+5*X2*X3^2*X6+X2^2*X6^2)-
4*X2*X3*X4^2*(X3^2+2*X2*X6)*X7+
X2^2*X4*(11*X3^2+X2*X6)*X7^2-4*X2^3*X3*X7^3)*X8^3\
+X2*(X3*X4-X2*X7)*(X4^2*(-X3^2+3*X2*X6)-4*X2*X3*X4*X7+
2*X2^2*X7^2)*X8^4)*X9+
(-X3^2+X2*X6)*(X5^2*X6-2*X3*X5*X8+X2*X8^2)*
(X5^2*(-3*X3*X4*X6+2*X3^2*X7+X2*X6*X7)+
2*X5*(2*X3^2*X4+X2*X4*X6-3*X2*X3*X7)*X8-
3*X2*(X3*X4-X2*X7)*X8^2)*X9^2+
4*X10^3*(X3^2-X2*X6)^3*
(-(X4*X5*X7)+X4^2*X8+X3*X5*X9-X2*X8*X9)+
X11^2*(X3^2-X2*X6)*(X3*X4-X2*X7)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2+
6*X10^2*(X3^2-X2*X6)^2*
(-((X5*X7-X4*X8)*
(X2*X5*X7^2+X4*X7*(-3*X3*X5+X2*X8)+
X4^2*(2*X5*X6-X3*X8)))+
(X5^2*(X3*X4*X6-2*X3^2*X7+X2*X6*X7)-
2*X2*X5*(X4*X6-X3*X7)*X8+X2*(X3*X4-X2*X7)*X8^2)*
X9-X11*(X3*X4-X2*X7)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9)))+
2*X10*(-(X3*X4^4*X5^3*X6^3)-4*X3^2*X4^3*X5^3*X6^2*X7+
8*X2*X4^3*X5^3*X6^3*X7+16*X3^3*X4^2*X5^3*X6*X7^2-
22*X2*X3*X4^2*X5^3*X6^2*X7^2-10*X3^4*X4*X5^3*X7^3+
8*X2*X3^2*X4*X5^3*X6*X7^3+6*X2^2*X4*X5^3*X6^2*X7^3+
4*X2*X3^3*X5^3*X7^4-5*X2^2*X3*X5^3*X6*X7^4+
10*X3^2*X4^4*X5^2*X6^2*X8-7*X2*X4^4*X5^2*X6^3*X8-
16*X3^3*X4^3*X5^2*X6*X7*X8+
4*X2*X3*X4^3*X5^2*X6^2*X7*X8-2*X3^4*X4^2*X5^2*X7^2*X8+
16*X2*X3^2*X4^2*X5^2*X6*X7^2*X8+
4*X2^2*X4^2*X5^2*X6^2*X7^2*X8+
8*X2*X3^3*X4*X5^2*X7^3*X8-
20*X2^2*X3*X4*X5^2*X6*X7^3*X8-2*X2^2*X3^2*X5^2*X7^4*X8+
5*X2^3*X5^2*X6*X7^4*X8-12*X3^3*X4^4*X5*X6*X8^2+
9*X2*X3*X4^4*X5*X6^2*X8^2+18*X3^4*X4^3*X5*X7*X8^2-
6*X2^2*X4^3*X5*X6^2*X7*X8^2-
24*X2*X3^3*X4^2*X5*X7^2*X8^2+
6*X2^2*X3*X4^2*X5*X6*X7^2*X8^2+
12*X2^2*X3^2*X4*X5*X7^3*X8^2-3*X2^3*X3*X5*X7^4*X8^2+
2*X3^4*X4^4*X8^3-X2^2*X4^4*X6^2*X8^3-
8*X2*X3^3*X4^3*X7*X8^3+4*X2^2*X3*X4^3*X6*X7*X8^3+
8*X2^2*X3^2*X4^2*X7^2*X8^3-2*X2^3*X4^2*X6*X7^2*X8^3-
4*X2^3*X3*X4*X7^3*X8^3+X2^4*X7^4*X8^3-
2*(X3^2-X2*X6)*
(-(X5^3*(X3*X4^2*X6^2+X4*X6*(-5*X3^2+3*X2*X6)*X7+
X3*(3*X3^2-2*X2*X6)*X7^2))+
X5^2*(X4^2*X6*(-X3^2+4*X2*X6)-
2*X3*X4*(2*X3^2+X2*X6)*X7-
X2*(-5*X3^2+2*X2*X6)*X7^2)*X8-
3*X5*(X3*X4^2*(-X3^2+2*X2*X6)-
X2*X4*(X3^2+X2*X6)*X7+X2^2*X3*X7^2)*X8^2+
X2^2*(X4^2*X6-2*X3*X4*X7+X2*X7^2)*X8^3)*X9+
(X3^2-X2*X6)^2*(X3*X5-X2*X8)*
(X5^2*X6-2*X3*X5*X8+X2*X8^2)*X9^2+
X11*(X3^2-X2*X6)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))*
(X3^3*X5*X9+X3^2*(4*X4*X5*X7+6*X4^2*X8-X2*X8*X9)-
X3*(5*X4^2*X5*X6+10*X2*X4*X7*X8+
X2*X5*(5*X7^2+X6*X9))+
X2*(6*X4*X5*X6*X7-X4^2*X6*X8+X2*X8*(5*X7^2+X6*X9))
))-2*X11*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))*
(X3^4*X5*(X5*X7+3*X4*X8)*X9+
X3^3*(X4*X5^2*X7^2+3*X4^3*X8^2-
2*(X4*X5^2*X6+2*X2*X5*X7*X8+X2*X4*X8^2)*X9)-
X3^2*(3*X4^3*X5*X6*X8+X4^2*X7*(X5^2*X6+5*X2*X8^2)+
X2*X4*X5*X8*(X7^2+2*X6*X9)+
2*X2*X7*(X5^2*X7^2-X2*X8^2*X9))+
X2*(X4^3*X5*X6^2*X8+
2*X4^2*X6*X7*(-(X5^2*X6)+X2*X8^2)+
X2*X4*X5*X6*X8*(-5*X7^2-X6*X9)+
X2*X7*(X2*X8^2*(-X7^2-2*X6*X9)+
X5^2*X6*(X7^2-X6*X9)))+
X3*(6*X2*X4^2*X5*X6*X7*X8+X4^3*X6*(X5^2*X6-2*X2*X8^2)+
2*X2^2*X5*X7*X8*(X7^2+2*X6*X9)+
X2*X4*(2*X5^2*X6*(X7^2+X6*X9)+
X2*X8^2*(3*X7^2+2*X6*X9)))))+
27*X1*(-2*X10^4*(X3^2-X2*X6)^3*(X3*X4-X2*X7)+
(X5*X7-X4*X8)*(-(X5^3*
(-3*X4^4*X6^3+11*X3*X4^3*X6^2*X7-
X4^2*X6*(11*X3^2+4*X2*X6)*X7^2+
X3*X4*(4*X3^2+5*X2*X6)*X7^3+
X2*(-3*X3^2+X2*X6)*X7^4))+
X5^2*(-7*X3*X4^4*X6^2+X4^3*X6*(22*X3^2+3*X2*X6)*X7-
X3*X4^2*(10*X3^2+23*X2*X6)*X7^2+
X2*X4*(10*X3^2+9*X2*X6)*X7^3-4*X2^2*X3*X7^4)*X8+
X5*(X4^4*X6*(3*X3^2+2*X2*X6)-
X3*X4^3*(12*X3^2+5*X2*X6)*X7-
X2*X4^2*(-23*X3^2+2*X2*X6)*X7^2-
11*X2^2*X3*X4*X7^3+2*X2^3*X7^4)*X8^2+
X4*(X3*X4-X2*X7)*
(2*X3^2*X4^2-3*X2*X4^2*X6+2*X2*X3*X4*X7-X2^2*X7^2)*
X8^3)-(X5^4*
(X3*X4^3*X6^3+X4^2*X6^2*(-7*X3^2+4*X2*X6)*X7-
X3*X4*X6*(-5*X3^2+2*X2*X6)*X7^2-
(X3^4-2*X2*X3^2*X6+2*X2^2*X6^2)*X7^3)+
X5^3*(-(X4^3*X6^2*(-X3^2+5*X2*X6))-
6*X3*X4^2*X6*(-3*X3^2+X2*X6)*X7+
X4*(-7*X3^4-13*X2*X3^2*X6+8*X2^2*X6^2)*X7^2+
4*X2^2*X3*X6*X7^3)*X8+
X5^2*(X3*X4^3*X6*(-11*X3^2+17*X2*X6)-
X4^2*(11*X3^4+2*X2*X3^2*X6+5*X2^2*X6^2)*X7-
9*X2*X3*X4*(-3*X3^2+X2*X6)*X7^2-
2*X2^2*(2*X3^2+X2*X6)*X7^3)*X8^2-
X5*(X4^3*(-11*X3^4+11*X2*X3^2*X6+4*X2^2*X6^2)-
2*X2*X3*X4^2*(-X3^2+7*X2*X6)*X7-
X2^2*X4*(-17*X3^2+5*X2*X6)*X7^2-4*X2^3*X3*X7^3)*
X8^3+X2*(X3*X4-X2*X7)*
(X4^2*(-5*X3^2+6*X2*X6)-2*X2*X3*X4*X7+X2^2*X7^2)*
X8^4)*X9-(-X3^2+X2*X6)*
(X5^2*X6-2*X3*X5*X8+X2*X8^2)*
(X5^2*(-3*X3*X4*X6+2*X3^2*X7+X2*X6*X7)+
2*X5*(2*X3^2*X4+X2*X4*X6-3*X2*X3*X7)*X8-
3*X2*(X3*X4-X2*X7)*X8^2)*X9^2+
X11^2*(-X3^2+X2*X6)*(X3*X4-X2*X7)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2+
3*X10^2*(-X3^2+X2*X6)*
(3*X3*X4^3*X5^2*X6^2-7*X3^2*X4^2*X5^2*X6*X7-
2*X2*X4^2*X5^2*X6^2*X7+6*X3^3*X4*X5^2*X7^2+
3*X2*X3*X4*X5^2*X6*X7^2-5*X2*X3^2*X5^2*X7^3+
2*X2^2*X5^2*X6*X7^3-5*X3^2*X4^3*X5*X6*X8-
X2*X4^3*X5*X6^2*X8+2*X3^3*X4^2*X5*X7*X8+
16*X2*X3*X4^2*X5*X6*X7*X8-9*X2*X3^2*X4*X5*X7^2*X8-
9*X2^2*X4*X5*X6*X7^2*X8+6*X2^2*X3*X5*X7^3*X8+
4*X3^3*X4^3*X8^2-X2*X3*X4^3*X6*X8^2-
10*X2*X3^2*X4^2*X7*X8^2+X2^2*X4^2*X6*X7*X8^2+
9*X2^2*X3*X4*X7^2*X8^2-3*X2^3*X7^3*X8^2+
(-X3^2+X2*X6)*
(X5^2*(X3*X4*X6+X3^2*X7-2*X2*X6*X7)+
X5*(-3*X3^2*X4+X2*X4*X6+2*X2*X3*X7)*X8+
X2*(X3*X4-X2*X7)*X8^2)*X9-
2*X11*(X3^2-X2*X6)*(X3*X4-X2*X7)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9)))+
X10^3*(X3^2-X2*X6)^2*
(-9*X3*(X4^2*X5*X6+X2*X5*X7^2+2*X2*X4*X7*X8)-
X3^3*X5*X9+X2*X3*X5*X6*X9+
X3^2*(2*X4*(5*X5*X7+4*X4*X8)+X2*X8*X9)+
X2*(8*X4*X5*X6*X7+X4^2*X6*X8+X2*X8*(9*X7^2-X6*X9)))+
X10*(-(X3*X4^4*X5^3*X6^3)+8*X3^2*X4^3*X5^3*X6^2*X7-
4*X2*X4^3*X5^3*X6^3*X7-23*X3^3*X4^2*X5^3*X6*X7^2+
17*X2*X3*X4^2*X5^3*X6^2*X7^2+14*X3^4*X4*X5^3*X7^3+
2*X2*X3^2*X4*X5^3*X6*X7^3-12*X2^2*X4*X5^3*X6^2*X7^3-
11*X2*X3^3*X5^3*X7^4+10*X2^2*X3*X5^3*X6*X7^4-
2*X3^2*X4^4*X5^2*X6^2*X8+5*X2*X4^4*X5^2*X6^3*X8+
14*X3^3*X4^3*X5^2*X6*X7*X8-
26*X2*X3*X4^3*X5^2*X6^2*X7*X8+4*X3^4*X4^2*X5^2*X7^2*X8-
5*X2*X3^2*X4^2*X5^2*X6*X7^2*X8+
19*X2^2*X4^2*X5^2*X6^2*X7^2*X8-
16*X2*X3^3*X4*X5^2*X7^3*X8+4*X2^2*X3*X4*X5^2*X6*X7^3*X8+
13*X2^2*X3^2*X5^2*X7^4*X8-10*X2^3*X5^2*X6*X7^4*X8-
3*X3^3*X4^4*X5*X6*X8^2-18*X3^4*X4^3*X5*X7*X8^2+
36*X2*X3^2*X4^3*X5*X6*X7*X8^2-
6*X2^2*X4^3*X5*X6^2*X7*X8^2+
21*X2*X3^3*X4^2*X5*X7^2*X8^2-
39*X2^2*X3*X4^2*X5*X6*X7^2*X8^2-
6*X2^2*X3^2*X4*X5*X7^3*X8^2+18*X2^3*X4*X5*X6*X7^3*X8^2-
3*X2^3*X3*X5*X7^4*X8^2+8*X3^4*X4^4*X8^3-
9*X2*X3^2*X4^4*X6*X8^3+2*X2^2*X4^4*X6^2*X8^3-
14*X2*X3^3*X4^3*X7*X8^3+10*X2^2*X3*X4^3*X6*X7*X8^3+
11*X2^2*X3^2*X4^2*X7^2*X8^3-5*X2^3*X4^2*X6*X7^2*X8^3-
4*X2^3*X3*X4*X7^3*X8^3+X2^4*X7^4*X8^3+
(X3^2-X2*X6)*(-(X5^3*
(-5*X3*X4^2*X6^2+2*X4*X6*(-X3^2+6*X2*X6)*X7+
X3*(3*X3^2-8*X2*X6)*X7^2))+
X5^2*(X4^2*X6*(-22*X3^2+7*X2*X6)+
2*X3*X4*(X3^2+14*X2*X6)*X7-
X2*(7*X3^2+8*X2*X6)*X7^2)*X8-
3*X5*(X3*X4^2*(-7*X3^2+2*X2*X6)+
2*X2*X4*(4*X3^2+X2*X6)*X7-5*X2^2*X3*X7^2)*X8^2\
+X2*(X4^2*(-9*X3^2+4*X2*X6)+10*X2*X3*X4*X7-5*X2^2*X7^2)*X8^3)*X9-
2*(X3^2-X2*X6)^2*(X3*X5-X2*X8)*
(X5^2*X6-2*X3*X5*X8+X2*X8^2)*X9^2-
2*X11*(X3^2-X2*X6)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))*
(X3^3*X5*X9+X3^2*(4*X4*X5*X7+6*X4^2*X8-X2*X8*X9)-
X3*(5*X4^2*X5*X6+10*X2*X4*X7*X8+
X2*X5*(5*X7^2+X6*X9))+
X2*(6*X4*X5*X6*X7-X4^2*X6*X8+X2*X8*(5*X7^2+X6*X9)))
)+2*X11*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))*
(X3^4*X5*(X5*X7+3*X4*X8)*X9+
X3^3*(X4*X5^2*X7^2+3*X4^3*X8^2-
2*(X4*X5^2*X6+2*X2*X5*X7*X8+X2*X4*X8^2)*X9)-
X3^2*(3*X4^3*X5*X6*X8+X4^2*X7*(X5^2*X6+5*X2*X8^2)+
X2*X4*X5*X8*(X7^2+2*X6*X9)+
2*X2*X7*(X5^2*X7^2-X2*X8^2*X9))+
X2*(X4^3*X5*X6^2*X8+2*X4^2*X6*X7*(-(X5^2*X6)+X2*X8^2)+
X2*X4*X5*X6*X8*(-5*X7^2-X6*X9)+
X2*X7*(X2*X8^2*(-X7^2-2*X6*X9)+
X5^2*X6*(X7^2-X6*X9)))+
X3*(6*X2*X4^2*X5*X6*X7*X8+X4^3*X6*(X5^2*X6-2*X2*X8^2)+
2*X2^2*X5*X7*X8*(X7^2+2*X6*X9)+
X2*X4*(2*X5^2*X6*(X7^2+X6*X9)+
X2*X8^2*(3*X7^2+2*X6*X9))))))*
(FV[p2,si]*FV[p3,mu]*FV[p3,nu]*FV[p3,rho]+
FV[p2,rho]*FV[p3,mu]*FV[p3,nu]*FV[p3,si]+
FV[p2,nu]*FV[p3,mu]*FV[p3,rho]*FV[p3,si]+
FV[p2,mu]*FV[p3,nu]*FV[p3,rho]*FV[p3,si]))/
((1-X1)*(9-X1^2)^2*(X4^2*X6-2*X3*X4*X7+X2*X7^2+X3^2*X9-
X2*X6*X9)^4)+((-2*X4^3*X5*X6*X7*X8^3*X9+X4^4*X6*X8^4*X9+
2*X3^3*X5*X8^3*X9^3+X4^2*X5^2*X6*X8^2*X9*(X7^2-X6*X9)+
X2^2*X8^4*X9^2*(-X7^2+X6*X9)+
X10^4*X6*(-X3^2+X2*X6)*
(-(X4^2*X6)+2*X3*X4*X7-X2*X7^2-X3^2*X9+X2*X6*X9)+
2*X3*X4*X8^2*X9*(-(X7*(X5*X7-X4*X8)^2)+
X5*X6*(X5*X7+X4*X8)*X9)+
2*X10^3*(X5*X6*(X4*X6-X3*X7)-
(X3*X4*X6+X3^2*X7-2*X2*X6*X7)*X8)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))-
X11^2*(X7^2-X6*X9)*(X4^2*X6-2*X3*X4*X7+X3^2*X9+
X2*(X7^2-X6*X9))^2-
X3^2*X8^2*X9^2*(6*X4*X5*X7*X8-X4^2*X8^2+
X5^2*(-X7^2+X6*X9))+
X1*(X10*X4*X6-X10*X3*X7+X5*X7^2-X4*X7*X8-X5*X6*X9+
X3*X8*X9)^2*(X10^2*(-X3^2+X2*X6)-(X5*X7-X4*X8)^2+
X10*(-2*X4*X5*X6+2*X3*X5*X7+2*X3*X4*X8-2*X2*X7*X8)+
(X5^2*X6-2*X3*X5*X8+X2*X8^2)*X9+
X11*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9)))-
X10^2*(-(X4^2*X6)+2*X3*X4*X7-X2*X7^2-X3^2*X9+X2*X6*X9)*
(X5^2*X6*(X7^2-X6*X9)+
2*X5*X8*(-3*X4*X6*X7+2*X3*X7^2+X3*X6*X9)+
X11*(-(X4^2*X6^2)+2*X3*X4*X6*X7+X3^2*(X7^2-2*X6*X9)+
2*X2*X6*(-X7^2+X6*X9))+
X8^2*(X4^2*X6+4*X3*X4*X7+X3^2*X9-2*X2*(2*X7^2+X6*X9)))\
-X2*X8^2*X9*(2*X5*X8*(X4*X7-X3*X9)*(X7^2-X6*X9)-
X5^2*(X7^2-X6*X9)^2+
X8^2*(-2*X3*X4*X7*X9+X3^2*X9^2+X4^2*(-X7^2+2*X6*X9)))-
X11*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))*
(2*X5*X8*(X4*X7-X3*X9)*(X7^2-X6*X9)-
X5^2*(X7^2-X6*X9)^2+
X8^2*(-2*X3*X4*X7*X9+X4^2*(-X7^2+2*X6*X9)+
X9*(2*X2*X7^2+X3^2*X9-2*X2*X6*X9)))+
2*X10*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))*
(-(X7*X8*(X5*X7-X4*X8)^2)+
X8*(X5^2*X6*X7+X5*(X4*X6-3*X3*X7)*X8+
(-(X3*X4)+2*X2*X7)*X8^2)*X9+
X11*(X4^2*X6*X7*X8+
X4*(X5*X6*(X7^2-X6*X9)+X3*X8*(-3*X7^2+X6*X9))+
X7*(X3^2*X8*X9+2*X2*X8*(X7^2-X6*X9)+
X3*X5*(-X7^2+X6*X9)))))*
(FV[p1,rho]*FV[p1,si]*MT[mu,nu]+
FV[p1,nu]*FV[p1,si]*MT[mu,rho]+
FV[p1,nu]*FV[p1,rho]*MT[mu,si]+
FV[p1,mu]*FV[p1,si]*MT[nu,rho]+
FV[p1,mu]*FV[p1,rho]*MT[nu,si]+
FV[p1,mu]*FV[p1,nu]*MT[rho,si]))/
((1-X1)*(3-X1)*(X4^2*X6-2*X3*X4*X7+X2*X7^2+X3^2*X9-X2*X6*X9)^
3)+((2*X4^3*X5^2*X6*X7*X8^2*X9-X4^4*X5*X6*X8^3*X9-
2*X3^3*X5^2*X8^2*X9^3+X2^2*X5*X8^3*X9^2*(X7^2-X6*X9)+
X4^2*X5^3*X6*X8*X9*(-X7^2+X6*X9)+
X10^3*(-3*X3^2*(X5*X7+X4*X8)+X2*X6*(X5*X7+X4*X8)+
2*X3*(X4*X5*X6+X2*X7*X8))*
(-(X4^2*X6)+2*X3*X4*X7-X2*X7^2-X3^2*X9+X2*X6*X9)+
2*X3*X4*X5*X8*X9*(X7*(X5*X7-X4*X8)^2-
X5*X6*(X5*X7+X4*X8)*X9)-
X10^4*X3*(X3^2-X2*X6)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))+
X11*(-(X4*X7*(X5*X7-X4*X8)^2)+
(X5*X7^2*(X3*X5+X2*X8)+X4^2*X8*(X5*X6+X3*X8)+
X4*X7*(X5^2*X6-6*X3*X5*X8+X2*X8^2))*X9-
(X3*X5^2*X6-3*X3^2*X5*X8+X2*X5*X6*X8+X2*X3*X8^2)*X9^2)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))+
X11^2*(X4*X7-X3*X9)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2+
X3^2*X5*X8*X9^2*(6*X4*X5*X7*X8-X4^2*X8^2+
X5^2*(-X7^2+X6*X9))-
X1*(X10*X3*X4-X10*X2*X7+X4*X5*X7-X4^2*X8-X3*X5*X9+
X2*X8*X9)*(-(X10*X4*X6)+X10*X3*X7-X5*X7^2+X4*X7*X8+
X5*X6*X9-X3*X8*X9)*
(X10^2*(X3^2-X2*X6)+(X5*X7-X4*X8)^2+
2*X10*(X4*X5*X6-X3*X5*X7-X3*X4*X8+X2*X7*X8)-
(X5^2*X6-2*X3*X5*X8+X2*X8^2)*X9+
X11*(-(X4^2*X6)+2*X3*X4*X7-X2*X7^2-X3^2*X9+X2*X6*X9))-
X10*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))*
(-((X5*X7-X4*X8)^2*(X5*X7+X4*X8))+
(X5^3*X6*X7+X5^2*(3*X4*X6-4*X3*X7)*X8+
X5*(-4*X3*X4+3*X2*X7)*X8^2+X2*X4*X8^3)*X9+
X11*(3*X4^2*X5*X6*X7-4*X3*X4*X5*X7^2+X2*X5*X7^3+
X4^3*X6*X8-4*X3*X4^2*X7*X8+3*X2*X4*X7^2*X8-
(-3*X3^2*(X5*X7+X4*X8)+X2*X6*(X5*X7+X4*X8)+
2*X3*(X4*X5*X6+X2*X7*X8))*X9))+
X2*X5*X8*X9*(2*X5*X8*(X4*X7-X3*X9)*(X7^2-X6*X9)-
X5^2*(X7^2-X6*X9)^2+
X8^2*(-2*X3*X4*X7*X9+X3^2*X9^2+X4^2*(-X7^2+2*X6*X9)))+
X10^2*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))*
(-(X5*X7^2*(3*X3*X5-2*X2*X8))+X4^2*X8*(2*X5*X6-3*X3*X8)+
2*X4*X7*(X5^2*X6-X3*X5*X8+X2*X8^2)+
(X3*X5^2*X6-3*X3^2*X5*X8+X2*X5*X6*X8+X2*X3*X8^2)*X9+
X11*(-3*X3^2*X4*X7+X2*X4*X6*X7+2*X3^3*X9+
X3*(X4^2*X6+X2*(X7^2-2*X6*X9)))))*
(FV[p1,si]*FV[p2,rho]*MT[mu,nu]+
FV[p1,rho]*FV[p2,si]*MT[mu,nu]+
FV[p1,si]*FV[p2,nu]*MT[mu,rho]+
FV[p1,nu]*FV[p2,si]*MT[mu,rho]+
FV[p1,rho]*FV[p2,nu]*MT[mu,si]+
FV[p1,nu]*FV[p2,rho]*MT[mu,si]+
FV[p1,si]*FV[p2,mu]*MT[nu,rho]+
FV[p1,mu]*FV[p2,si]*MT[nu,rho]+
FV[p1,rho]*FV[p2,mu]*MT[nu,si]+
FV[p1,mu]*FV[p2,rho]*MT[nu,si]+
FV[p1,nu]*FV[p2,mu]*MT[rho,si]+
FV[p1,mu]*FV[p2,nu]*MT[rho,si]))/
((1-X1)*(3-X1)*(X4^2*X6-2*X3*X4*X7+X2*X7^2+X3^2*X9-X2*X6*X9)^
3)+((-54*X4^3*X5^3*X6*X7*X8*X9+27*X4^4*X5^2*X6*X8^2*X9+
54*X3^3*X5^3*X8*X9^3+27*X4^2*X5^4*X6*X9*(X7^2-X6*X9)+
27*X2^2*X5^2*X8^2*X9^2*(-X7^2+X6*X9)+
54*X3*X4*X5^2*X9*(-(X7*(X5*X7-X4*X8)^2)+
X5*X6*(X5*X7+X4*X8)*X9)+
27*X10^4*X2*(X3^2-X2*X6)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))-
54*X10^3*(X3^2*X4*X5+X2*X3*(X5*X7+X4*X8)+
X2*(-2*X4*X5*X6-X2*X7*X8))*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))+
27*X11*(X4^2*(X5*X7-X4*X8)^2-
2*(X5^2*(X4^2*X6-X3*X4*X7+X2*X7^2)-
X4*X5*(X3*X4+X2*X7)*X8+X2*X4^2*X8^2)*X9+
(X5^2*(-X3^2+2*X2*X6)-2*X2*X3*X5*X8+X2^2*X8^2)*X9^2)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))-
27*X11^2*(X4^2-X2*X9)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2+
27*X3^2*X5^2*X9^2*(-6*X4*X5*X7*X8+X4^2*X8^2+
X5^2*(X7^2-X6*X9))+
X1^4*(X10*X3*X4-X10*X2*X7+X4*X5*X7-X4^2*X8-X3*X5*X9+
X2*X8*X9)^2*(X10^2*(X3^2-X2*X6)+(X5*X7-X4*X8)^2+
2*X10*(X4*X5*X6-X3*X5*X7-X3*X4*X8+X2*X7*X8)-
(X5^2*X6-2*X3*X5*X8+X2*X8^2)*X9+
X11*(-(X4^2*X6)+2*X3*X4*X7-X2*X7^2-X3^2*X9+X2*X6*X9))-
27*X2*X5^2*X9*(2*X5*X8*(X4*X7-X3*X9)*(X7^2-X6*X9)-
X5^2*(X7^2-X6*X9)^2+
X8^2*(-2*X3*X4*X7*X9+X3^2*X9^2+X4^2*(-X7^2+2*X6*X9)))+
9*X1*(X10^2*(X3^2-X2*X6)+(X5*X7-X4*X8)^2+
2*X10*(X4*X5*X6-X3*X5*X7-X3*X4*X8+X2*X7*X8)-
(X5^2*X6-2*X3*X5*X8+X2*X8^2)*X9+
X11*(-(X4^2*X6)+2*X3*X4*X7-X2*X7^2-X3^2*X9+X2*X6*X9))*
(-3*X4^2*(X5*X7-X4*X8)^2-
2*X10*X4*(-2*X2*X5*X7^2+X4*X7*(X3*X5+3*X2*X8)+
X4^2*(X5*X6-3*X3*X8))+
(X5^2*(X4^2*X6+4*X3*X4*X7+X2*X7^2)-
6*X4*X5*(X3*X4+X2*X7)*X8+6*X2*X4^2*X8^2)*X9+
2*X10*(2*X3^2*X4*X5-3*X2*X3*(X5*X7+X4*X8)+
X2*(X4*X5*X6+3*X2*X7*X8))*X9-
(X5^2*(2*X3^2+X2*X6)-6*X2*X3*X5*X8+3*X2^2*X8^2)*X9^2+
X11*(X4^2-X2*X9)*
(X4^2*X6-2*X3*X4*X7+X2*X7^2+X3^2*X9-X2*X6*X9)+
X10^2*(4*X2*X3*X4*X7+X3^2*(-3*X4^2+X2*X9)+
X2*(X4^2*X6-2*X2*X7^2-X2*X6*X9)))-
X1^3*(X10^2*(X3^2-X2*X6)+(X5*X7-X4*X8)^2+
2*X10*(X4*X5*X6-X3*X5*X7-X3*X4*X8+X2*X7*X8)-
(X5^2*X6-2*X3*X5*X8+X2*X8^2)*X9+
X11*(-(X4^2*X6)+2*X3*X4*X7-X2*X7^2-X3^2*X9+X2*X6*X9))*
(-3*X4^2*(X5*X7-X4*X8)^2-
2*X10*X4*(-2*X2*X5*X7^2+X4*X7*(X3*X5+3*X2*X8)+
X4^2*(X5*X6-3*X3*X8))+
(X5^2*(X4^2*X6+4*X3*X4*X7+X2*X7^2)-
6*X4*X5*(X3*X4+X2*X7)*X8+6*X2*X4^2*X8^2)*X9+
2*X10*(2*X3^2*X4*X5-3*X2*X3*(X5*X7+X4*X8)+
X2*(X4*X5*X6+3*X2*X7*X8))*X9-
(X5^2*(2*X3^2+X2*X6)-6*X2*X3*X5*X8+3*X2^2*X8^2)*X9^2+
X11*(X4^2-X2*X9)*
(X4^2*X6-2*X3*X4*X7+X2*X7^2+X3^2*X9-X2*X6*X9)+
X10^2*(4*X2*X3*X4*X7+X3^2*(-3*X4^2+X2*X9)+
X2*(X4^2*X6-2*X2*X7^2-X2*X6*X9)))-
3*X1^2*(X10^2*(X3^2-X2*X6)+(X5*X7-X4*X8)^2+
2*X10*(X4*X5*X6-X3*X5*X7-X3*X4*X8+X2*X7*X8)-
(X5^2*X6-2*X3*X5*X8+X2*X8^2)*X9+
X11*(-(X4^2*X6)+2*X3*X4*X7-X2*X7^2-X3^2*X9+X2*X6*X9))*
(3*X4^2*(X5*X7-X4*X8)^2-
2*X10*X4*(4*X2*X5*X7^2+X4*X7*(-5*X3*X5-3*X2*X8)+
X4^2*(X5*X6+3*X3*X8))+
(X5^2*(X4^2*X6-8*X3*X4*X7+X2*X7^2)+
6*X4*X5*(X3*X4+X2*X7)*X8-6*X2*X4^2*X8^2)*X9+
2*X10*(-4*X3^2*X4*X5+3*X2*X3*(X5*X7+X4*X8)+
X2*(X4*X5*X6-3*X2*X7*X8))*X9-
(X5^2*(-4*X3^2+X2*X6)+6*X2*X3*X5*X8-3*X2^2*X8^2)*X9^2+
X11*(X4^2-X2*X9)*
(X4^2*X6-2*X3*X4*X7+X2*X7^2+X3^2*X9-X2*X6*X9)+
X10^2*(-8*X2*X3*X4*X7+X3^2*(3*X4^2+X2*X9)+
X2*(X4^2*X6+4*X2*X7^2-X2*X6*X9)))-
27*X10^2*(-(X4^2*X6)+2*X3*X4*X7-X2*X7^2-X3^2*X9+X2*X6*X9)*
(X2*X5^2*X7^2+2*X4*X5*X7*(2*X3*X5-3*X2*X8)+
X4^2*(-4*X5^2*X6+4*X3*X5*X8+X2*X8^2)+X3^2*X5^2*X9-
2*X2*X5^2*X6*X9+2*X2*X3*X5*X8*X9-X2^2*X8^2*X9+
X11*(2*X2*X3*X4*X7+X3^2*(X4^2-2*X2*X9)+
X2*(-2*X4^2*X6-X2*X7^2+2*X2*X6*X9)))+
54*X10*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))*
(-(X4*X5*(X5*X7-X4*X8)^2)+
X5*(X5*X7*(-(X3*X5)+X2*X8)+
X4*(2*X5^2*X6-3*X3*X5*X8+X2*X8^2))*X9+
X11*(X4^3*(2*X5*X6-X3*X8)+X4^2*(-3*X3*X5*X7+X2*X7*X8)+
X2*X7*(X3*X5-X2*X8)*X9+
X4*(X3^2*X5*X9+X2*(X3*X8*X9+X5*(X7^2-2*X6*X9))))))*
(FV[p2,rho]*FV[p2,si]*MT[mu,nu]+
FV[p2,nu]*FV[p2,si]*MT[mu,rho]+
FV[p2,nu]*FV[p2,rho]*MT[mu,si]+
FV[p2,mu]*FV[p2,si]*MT[nu,rho]+
FV[p2,mu]*FV[p2,rho]*MT[nu,si]+
FV[p2,mu]*FV[p2,nu]*MT[rho,si]))/
((1-X1)*(9-X1^2)^2*(X4^2*X6-2*X3*X4*X7+X2*X7^2+X3^2*X9-
X2*X6*X9)^3)+((3*X4^4*X5*X6*X7*X8^3-X4^5*X6*X8^4+
2*X3^3*X5*X8^2*(X5*X7-X4*X8)*X9^2+
X4^2*X5^3*X6*X7*X8*(X7^2-X6*X9)+
X2^2*X8^3*(-(X5*X7)+X4*X8)*X9*(X7^2-X6*X9)+
X4^3*X5^2*X6*X8^2*(-3*X7^2+X6*X9)+
2*X3*X4*X8*(-(X7*(X5*X7-X4*X8)^3)+
X5*X6*(X5*X7-X4*X8)*(X5*X7+X4*X8)*X9)+
X10^3*(X3^2-X2*X6)*(-(X5*X6)+X3*X8)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))-
X10^2*((-(X11*X3^2)+X11*X2*X6+2*X5^2*X6)*(X4*X6-X3*X7)+
X5*(-4*X3*X4*X6+X3^2*X7+3*X2*X6*X7)*X8-
(-3*X3^2*X4+X2*X4*X6+2*X2*X3*X7)*X8^2)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))+
X11*((-(X5*X7)+X4*X8)*
(2*X4^2*X6*X8+X7^2*(X3*X5+X2*X8)+
X4*X7*(-(X5*X6)-3*X3*X8))+
(-(X5^2*X6*(X4*X6-X3*X7))+
X5*(2*X3*X4*X6-3*X3^2*X7+X2*X6*X7)*X8+
(X3^2*X4-2*X2*X4*X6+X2*X3*X7)*X8^2)*X9)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))-
X11^2*(X4*X6-X3*X7)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2+
X3^2*X8*(-(X5*X7)+X4*X8)*X9*
(6*X4*X5*X7*X8-X4^2*X8^2+X5^2*(-X7^2+X6*X9))+
X1*(X4*X5*X6+X10*(X3^2-X2*X6)-X3*X5*X7-X3*X4*X8+
X2*X7*X8)*(-(X10*X4*X6)+X10*X3*X7-X5*X7^2+X4*X7*X8+
X5*X6*X9-X3*X8*X9)*
(X10^2*(X3^2-X2*X6)+(X5*X7-X4*X8)^2+
2*X10*(X4*X5*X6-X3*X5*X7-X3*X4*X8+X2*X7*X8)-
(X5^2*X6-2*X3*X5*X8+X2*X8^2)*X9+
X11*(-(X4^2*X6)+2*X3*X4*X7-X2*X7^2-X3^2*X9+X2*X6*X9))+
X10*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))*
(-((X5*X7-X4*X8)*(X5^2*X6*X7+X5*(-3*X4*X6+X3*X7)*X8+
(3*X3*X4-2*X2*X7)*X8^2))+
(X5*X6-X3*X8)*(X5^2*X6-2*X3*X5*X8+X2*X8^2)*X9+
X11*(3*X4^2*X6*(X5*X6-X3*X8)+
X7^2*(2*X3^2*X5+X2*X5*X6-3*X2*X3*X8)+
2*X4*X7*(-3*X3*X5*X6+2*X3^2*X8+X2*X6*X8)-
(X3^2-X2*X6)*(-(X5*X6)+X3*X8)*X9))-
X2*X8*(-(X5*X7)+X4*X8)*
(-2*X5*X8*(X4*X7-X3*X9)*(X7^2-X6*X9)+
X5^2*(X7^2-X6*X9)^2+
X8^2*(2*X3*X4*X7*X9-X3^2*X9^2+X4^2*(X7^2-2*X6*X9))))*
(FV[p1,si]*FV[p3,rho]*MT[mu,nu]+
FV[p1,rho]*FV[p3,si]*MT[mu,nu]+
FV[p1,si]*FV[p3,nu]*MT[mu,rho]+
FV[p1,nu]*FV[p3,si]*MT[mu,rho]+
FV[p1,rho]*FV[p3,nu]*MT[mu,si]+
FV[p1,nu]*FV[p3,rho]*MT[mu,si]+
FV[p1,si]*FV[p3,mu]*MT[nu,rho]+
FV[p1,mu]*FV[p3,si]*MT[nu,rho]+
FV[p1,rho]*FV[p3,mu]*MT[nu,si]+
FV[p1,mu]*FV[p3,rho]*MT[nu,si]+
FV[p1,nu]*FV[p3,mu]*MT[rho,si]+
FV[p1,mu]*FV[p3,nu]*MT[rho,si]))/
((1-X1)*(3-X1)*(X4^2*X6-2*X3*X4*X7+X2*X7^2+X3^2*X9-X2*X6*X9)^
3)+((-3*X4^4*X5^2*X6*X7*X8^2+X4^5*X5*X6*X8^3+
2*X3^3*X5^2*X8*(-(X5*X7)+X4*X8)*X9^2+
X2^2*X5*X8^2*(X5*X7-X4*X8)*X9*(X7^2-X6*X9)+
X4^3*X5^3*X6*X8*(3*X7^2-X6*X9)+
X4^2*X5^4*X6*X7*(-X7^2+X6*X9)+
2*X3*X4*X5*(X7*(X5*X7-X4*X8)^3+
X5*X6*(-(X5^2*X7^2)+X4^2*X8^2)*X9)+
X10^3*(X3^2-X2*X6)*(X3*X5-X2*X8)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))-
X10^2*(X11*(X3^2-X2*X6)*(X3*X4-X2*X7)-
X5^2*(2*X3*X4*X6-3*X3^2*X7+X2*X6*X7)+
X5*(X3^2*X4+3*X2*X4*X6-4*X2*X3*X7)*X8-
2*X2*(X3*X4-X2*X7)*X8^2)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))-
X11*((-(X5*X7)+X4*X8)*
(2*X2*X5*X7^2+X4*X7*(-3*X3*X5-X2*X8)+
X4^2*(X5*X6+X3*X8))-
(X5^2*(X3*X4*X6+X3^2*X7-2*X2*X6*X7)+
X5*(-3*X3^2*X4+X2*X4*X6+2*X2*X3*X7)*X8+
X2*(X3*X4-X2*X7)*X8^2)*X9)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))+
X11^2*(X3*X4-X2*X7)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2-
X3^2*X5*(X5*X7-X4*X8)*X9*
(-6*X4*X5*X7*X8+X4^2*X8^2+X5^2*(X7^2-X6*X9))+
X1*(X4*X5*X6+X10*(X3^2-X2*X6)-X3*X5*X7-X3*X4*X8+
X2*X7*X8)*(X10*X3*X4-X10*X2*X7+X4*X5*X7-X4^2*X8-
X3*X5*X9+X2*X8*X9)*
(X10^2*(X3^2-X2*X6)+(X5*X7-X4*X8)^2+
2*X10*(X4*X5*X6-X3*X5*X7-X3*X4*X8+X2*X7*X8)-
(X5^2*X6-2*X3*X5*X8+X2*X8^2)*X9+
X11*(-(X4^2*X6)+2*X3*X4*X7-X2*X7^2-X3^2*X9+X2*X6*X9))-
X10*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))*
(-((X5*X7-X4*X8)*(3*X5*X7*(X3*X5-X2*X8)+
X4*(-2*X5^2*X6+X3*X5*X8+X2*X8^2)))+
(X3*X5-X2*X8)*(X5^2*X6-2*X3*X5*X8+X2*X8^2)*X9+
X11*(3*X3*X4^2*X5*X6-4*X3^2*X4*X5*X7-2*X2*X4*X5*X6*X7+
3*X2*X3*X5*X7^2-2*X3^2*X4^2*X8-X2*X4^2*X6*X8+
6*X2*X3*X4*X7*X8-3*X2^2*X7^2*X8+
(X3^2-X2*X6)*(X3*X5-X2*X8)*X9))+
X2*X5*(-(X5*X7)+X4*X8)*
(-2*X5*X8*(X4*X7-X3*X9)*(X7^2-X6*X9)+
X5^2*(X7^2-X6*X9)^2+
X8^2*(2*X3*X4*X7*X9-X3^2*X9^2+X4^2*(X7^2-2*X6*X9))))*
(FV[p2,si]*FV[p3,rho]*MT[mu,nu]+
FV[p2,rho]*FV[p3,si]*MT[mu,nu]+
FV[p2,si]*FV[p3,nu]*MT[mu,rho]+
FV[p2,nu]*FV[p3,si]*MT[mu,rho]+
FV[p2,rho]*FV[p3,nu]*MT[mu,si]+
FV[p2,nu]*FV[p3,rho]*MT[mu,si]+
FV[p2,si]*FV[p3,mu]*MT[nu,rho]+
FV[p2,mu]*FV[p3,si]*MT[nu,rho]+
FV[p2,rho]*FV[p3,mu]*MT[nu,si]+
FV[p2,mu]*FV[p3,rho]*MT[nu,si]+
FV[p2,nu]*FV[p3,mu]*MT[rho,si]+
FV[p2,mu]*FV[p3,nu]*MT[rho,si]))/
((1-X1)*(3-X1)*(X4^2*X6-2*X3*X4*X7+X2*X7^2+X3^2*X9-X2*X6*X9)^
3)+((-6*X4^3*X5^3*X6^2*X7*X8+3*X4^4*X5^2*X6^2*X8^2-
12*X3^4*X5^2*X8^2*X9^2+3*X4^2*X5^4*X6^2*(X7^2-X6*X9)+
3*X2^3*X8^4*X9*(-X7^2+X6*X9)+
6*X3*X4*X5*X6*(-((X5*X7-X4*X8)^2*(X5*X7+X4*X8))+
X5^2*X6*(X5*X7+2*X4*X8)*X9)+
X3^2*X5*(12*X4*X7*X8*(X5*X7-X4*X8)^2+
3*X5*X6*(X5^2*X7^2-10*X4*X5*X7*X8-3*X4^2*X8^2)*X9-
3*X5^3*X6^2*X9^2)-
6*X3^3*X5*X8*X9*(-6*X4*X5*X7*X8+X4^2*X8^2+
X5^2*(X7^2-2*X6*X9))+
3*X10^2*(X3^2-X2*X6)*
(X5^2*X6+X11*(X3^2-X2*X6)-2*X3*X5*X8+X2*X8^2)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))+
6*X10*(X4*X5*X6-X3*X5*X7-X3*X4*X8+X2*X7*X8)*
(X5^2*X6+X11*(X3^2-X2*X6)-2*X3*X5*X8+X2*X8^2)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))-
3*X11*(X7^2*(X5^2*(-X3^2+2*X2*X6)-2*X2*X3*X5*X8+
X2^2*X8^2)-2*X4*X7*
(X3*X5^2*X6-3*X3^2*X5*X8+X2*X5*X6*X8+X2*X3*X8^2)+
X4^2*(X5^2*X6^2-2*X3*X5*X6*X8+(-X3^2+2*X2*X6)*X8^2)-
2*(-X3^2+X2*X6)*(X5^2*X6-2*X3*X5*X8+X2*X8^2)*X9)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))-
3*X11^2*(X3^2-X2*X6)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2+
X1*(X10^2*(-X3^2+X2*X6)-(X5*X7-X4*X8)^2+
X10*(-2*X4*X5*X6+2*X3*X5*X7+2*X3*X4*X8-2*X2*X7*X8)+
(X5^2*X6-2*X3*X5*X8+X2*X8^2)*X9+
X11*(X4^2*X6-2*X3*X4*X7+X2*X7^2+X3^2*X9-X2*X6*X9))*
(2*X4^2*X5^2*X6^2+3*X10^2*(X3^2-X2*X6)^2-
4*X3*X4*X5^2*X6*X7+3*X3^2*X5^2*X7^2-X2*X5^2*X6*X7^2-
4*X3*X4^2*X5*X6*X8+2*X3^2*X4*X5*X7*X8+
6*X2*X4*X5*X6*X7*X8-4*X2*X3*X5*X7^2*X8+3*X3^2*X4^2*X8^2-
X2*X4^2*X6*X8^2-4*X2*X3*X4*X7*X8^2+2*X2^2*X7^2*X8^2-
6*X10*(X3^2-X2*X6)*
(-(X4*X5*X6)+X3*X5*X7+X3*X4*X8-X2*X7*X8)+
(-X3^2+X2*X6)*(X5^2*X6-2*X3*X5*X8+X2*X8^2)*X9-
X11*(X3^2-X2*X6)*
(X4^2*X6-2*X3*X4*X7+X2*X7^2+X3^2*X9-X2*X6*X9))-
X1^2*(X10*X3^2-X10*X2*X6+X4*X5*X6-X3*X5*X7-X3*X4*X8+
X2*X7*X8)^2*(X10^2*(X3^2-X2*X6)+(X5*X7-X4*X8)^2+
2*X10*(X4*X5*X6-X3*X5*X7-X3*X4*X8+X2*X7*X8)-
(X5^2*X6-2*X3*X5*X8+X2*X8^2)*X9+
X11*(-(X4^2*X6)+2*X3*X4*X7-X2*X7^2-X3^2*X9+X2*X6*X9))-
3*X2^2*X8^2*(2*X5*X8*(X4*X7-2*X3*X9)*(X7^2-X6*X9)+
X5^2*(-X7^4+3*X6*X7^2*X9-2*X6^2*X9^2)+
X8^2*(-2*X3*X4*X7*X9+X3^2*X9^2+X4^2*(-X7^2+2*X6*X9)))+
3*X2*(X4^2*X8^4*(X4^2*X6-2*X3*X4*X7+X3^2*X9)+
X5^4*X6*(X7^2-X6*X9)^2-
2*X5^3*X8*(X7^2-X6*X9)*(X4*X6*X7+X3*(X7^2-2*X6*X9))+
X5^2*X8^2*(2*X3*X4*X7^3+X4^2*X6*(2*X7^2-3*X6*X9)+
X3^2*X9*(-3*X7^2+2*X6*X9))+
2*X5*X8^3*(-(X4^3*X6*X7)-5*X3^2*X4*X7*X9+2*X3^3*X9^2+
X3*X4^2*(X7^2+3*X6*X9))))*
(FV[p3,rho]*FV[p3,si]*MT[mu,nu]+
FV[p3,nu]*FV[p3,si]*MT[mu,rho]+
FV[p3,nu]*FV[p3,rho]*MT[mu,si]+
FV[p3,mu]*FV[p3,si]*MT[nu,rho]+
FV[p3,mu]*FV[p3,rho]*MT[nu,si]+
FV[p3,mu]*FV[p3,nu]*MT[rho,si]))/
((1-X1)*(9-X1^2)*(X4^2*X6-2*X3*X4*X7+X2*X7^2+X3^2*X9-
X2*X6*X9)^3)+((3*X10^4*(X3^2-X2*X6)^2-12*X4^3*X5*X7*X8^3+
3*X4^4*X8^4-12*X10^3*(X3^2-X2*X6)*
(-(X4*X5*X6)+X3*X5*X7+X3*X4*X8-X2*X7*X8)+
12*X3^2*X5^2*X8^2*X9^2+3*X2^2*X8^4*X9^2+
3*X5^4*(X7^2-X6*X9)^2+6*X4^2*X5^2*X8^2*(3*X7^2-X6*X9)+
12*X4*X5^3*X7*X8*(-X7^2+X6*X9)+
12*X3*X5*X8*X9*((X5*X7-X4*X8)^2-X5^2*X6*X9)+
6*X2*X8^2*X9*(-(X5*X7-X4*X8)^2+X5*(X5*X6-2*X3*X8)*X9)-
6*X11*((X5*X7-X4*X8)^2-(X5^2*X6-2*X3*X5*X8+X2*X8^2)*X9)*
(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))+
3*X11^2*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9))^2+
X1*(X10^2*(X3^2-X2*X6)+(X5*X7-X4*X8)^2+
2*X10*(X4*X5*X6-X3*X5*X7-X3*X4*X8+X2*X7*X8)-
(X5^2*X6-2*X3*X5*X8+X2*X8^2)*X9+
X11*(-(X4^2*X6)+2*X3*X4*X7-X2*X7^2-X3^2*X9+X2*X6*X9))^2\
-12*X10*(X4*X5*X6-X3*X5*X7-X3*X4*X8+X2*X7*X8)*
(-(X5*X7-X4*X8)^2+(X5^2*X6-2*X3*X5*X8+X2*X8^2)*X9+
X11*(X4^2*X6-2*X3*X4*X7+X3^2*X9+X2*(X7^2-X6*X9)))+
6*X10^2*(-(X7^2*(X5^2*(-3*X3^2+X2*X6)+4*X2*X3*X5*X8-
2*X2^2*X8^2))+
2*X4*X7*(-2*X3*X5^2*X6+X5*(X3^2+3*X2*X6)*X8-
2*X2*X3*X8^2)+X4^2*
(2*X5^2*X6^2-4*X3*X5*X6*X8+3*X3^2*X8^2-X2*X6*X8^2)+
(-X3^2+X2*X6)*(X5^2*X6-2*X3*X5*X8+X2*X8^2)*X9-
X11*(X3^2-X2*X6)*(X4^2*X6-2*X3*X4*X7+X3^2*X9+
X2*(X7^2-X6*X9))))*
(MT[mu,si]*MT[nu,rho]+MT[mu,rho]*MT[nu,si]+
MT[mu,nu]*MT[rho,si]))/
((1-X1)*(9-X1^2)*(X4^2*X6-2*X3*X4*X7+X2*X7^2+X3^2*X9-
X2*X6*X9)^2)
) /. Dispatch[
{X1->n,X2->spe[p1,p1],X3->spe[p1,p2],X4->spe[p1,p3],
X5->spe[p1,q],X6->spe[p2,p2],X7->spe[p2,p3],X8->spe[p2,q],
X9->spe[p3,p3],X10->spe[p3,q],X11->spe[q,q]}
             ]
                ,n
               ]
     ]
};

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "TIDL | \n "]];
Null
