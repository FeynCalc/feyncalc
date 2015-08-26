FVProjection is a simple toy add-on for FeynCalc 9.0 and above which shows
how one can write small or medium-size add-ons that enhance FeynCalc
with additional features.

The purpose of FVProjection is to decompose the given 4-vector x^mu
into transverse and longitudinal components with respect to some other
4-vector p. That is,

x^mu = x^mu_T + x^mu_L

with

x^mu_T = P_T^{mu nu} x_nu, P_T^{mu nu} = (g^{mu nu} - (p^mu p^nu)/p^2 )

x^mu_L = P_L^{mu nu} x_nu, P_L^{mu nu} = (p^mu p^nu)/p^2

The transverse projection is obtained from FVProjectionT[x,mu,p], for the
longitudinal projection use FVProjectionT[x,mu,p]. The projection tensors
can be acessed via FVProjectorT[mu,nu,p] and FVProjectorL[mu,nu,p].

