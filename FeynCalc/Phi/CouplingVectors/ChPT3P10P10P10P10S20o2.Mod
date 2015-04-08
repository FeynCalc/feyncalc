{{((I/36)*(3*SU3D[I1, k1, k2]*SU3D[I2, I5, k2]*SU3D[I3, I4, k1] +
	3*SU3D[I1, k1, k2]*SU3D[I2, I5, k1]*SU3D[I3, I4, k2] +
	6*SU3D[I1, I5, k1]*SU3D[I2, k1, k2]*SU3D[I3, I4, k2] +
	3*SU3D[I1, k1, k2]*SU3D[I2, I4, k2]*SU3D[I3, I5, k1] +
	3*SU3D[I1, k1, k2]*SU3D[I2, I4, k1]*SU3D[I3, I5, k2] +
	6*SU3D[I1, I4, k1]*SU3D[I2, k1, k2]*SU3D[I3, I5, k2] +
	6*SU3D[I1, I5, k1]*SU3D[I2, I4, k2]*SU3D[I3, k1, k2] +
	6*SU3D[I1, I4, k1]*SU3D[I2, I5, k2]*SU3D[I3, k1, k2] +
	3*SU3D[I1, k1, k2]*SU3D[I2, I3, k2]*SU3D[I4, I5, k1] +
	3*SU3D[I1, k1, k2]*SU3D[I2, I3, k1]*SU3D[I4, I5, k2] +
	6*SU3D[I1, I3, k1]*SU3D[I2, k1, k2]*SU3D[I4, I5, k2] +
	6*SU3D[I1, I2, k1]*SU3D[I3, k1, k2]*SU3D[I4, I5, k2] +
	6*SU3D[I1, I5, k1]*SU3D[I2, I3, k2]*SU3D[I4, k1, k2] +
	6*SU3D[I1, I3, k1]*SU3D[I2, I5, k2]*SU3D[I4, k1, k2] +
	6*SU3D[I1, I2, k1]*SU3D[I3, I5, k2]*SU3D[I4, k1, k2] +
	6*SU3D[I3, I4, k1]*SU3F[I1, k1, k2]*SU3F[I2, I5, k2] +
	6*SU3D[I3, I4, k1]*SU3F[I1, I5, k2]*SU3F[I2, k1, k2] +
	6*SU3D[I2, I4, k1]*SU3F[I1, k1, k2]*SU3F[I3, I5, k2] +
	6*SU3D[I1, I4, k1]*SU3F[I2, k1, k2]*SU3F[I3, I5, k2] +
	6*SU3D[I2, I4, k1]*SU3F[I1, I5, k2]*SU3F[I3, k1, k2] +
	6*SU3D[I1, I4, k1]*SU3F[I2, I5, k2]*SU3F[I3, k1, k2] +
	6*SU3D[I2, I3, k1]*SU3F[I1, k1, k2]*SU3F[I4, I5, k2] +
	6*SU3D[I1, I3, k1]*SU3F[I2, k1, k2]*SU3F[I4, I5, k2] +
	6*SU3D[I1, I2, k1]*SU3F[I3, k1, k2]*SU3F[I4, I5, k2] +
	6*SU3D[I2, I3, k1]*SU3F[I1, I5, k2]*SU3F[I4, k1, k2] +
	6*SU3D[I1, I3, k1]*SU3F[I2, I5, k2]*SU3F[I4, k1, k2] +
	6*SU3D[I1, I2, k1]*SU3F[I3, I5, k2]*SU3F[I4, k1, k2] +
	24*SU3D[I1, I4, k1]*SU3D[I2, I3, k1]*SUNDelta[0, I5] +
	24*SU3D[I1, I3, k1]*SU3D[I2, I4, k1]*SUNDelta[0, I5] +
	24*SU3D[I1, I2, k1]*SU3D[I3, I4, k1]*SUNDelta[0, I5] +
	8*SU3D[I3, I4, I5]*SUNDelta[I1, I2] + 8*SU3D[I2, I4, I5]*
	SUNDelta[I1, I3] + 8*SU3D[I2, I3, I5]*SUNDelta[I1, I4] +
	12*SU3D[I2, I3, I4]*SUNDelta[I1, I5] + 8*SU3D[I1, I4, I5]*
	SUNDelta[I2, I3] + 16*SUNDelta[0, I5]*SUNDelta[I1, I4]*
	SUNDelta[I2, I3] + 8*SU3D[I1, I3, I5]*SUNDelta[I2, I4] +
	16*SUNDelta[0, I5]*SUNDelta[I1, I3]*SUNDelta[I2, I4] +
	12*SU3D[I1, I3, I4]*SUNDelta[I2, I5] + 8*SU3D[I1, I2, I5]*
	SUNDelta[I3, I4] + 16*SUNDelta[0, I5]*SUNDelta[I1, I2]*
	SUNDelta[I3, I4] + 12*SU3D[I1, I2, I4]*SUNDelta[I3, I5] +
	12*SU3D[I1, I2, I3]*SUNDelta[I4, I5]))/
DecayConstant[PseudoScalar[1], RenormalizationState[0]]^2}}
