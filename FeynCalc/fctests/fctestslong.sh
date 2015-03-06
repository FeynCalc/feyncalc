#!/bin/bash

# This small bash script provides a nice way to check that
# FeynCalc is working properly using real-life examples.

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $DIR

#Tensor decompositions
math -nopromt -script ../fcexamples/VerifyTIDLOneLoop.m
math -nopromt -script ../fcexamples/VerifyTIDLTwoLoop.m
math -nopromt -script ../fcexamples/VerifyTIDLThreeLoop.m
math -nopromt -script ../fcexamples/VerifyTIDLFourLoop.m
