\name{SC19092-package}
\alias{SC19092-package}
\alias{SC19092}
\docType{package}
\title{
  Final Project of Course "Statistical Computing"
}
\description{
  This package contains two functions implementing two useful methods I learnt during this term, including examples. The solutions to all homework are also attached here for future purpose.
}
\details{
  For the solutions to homework, see vignette directly. The usage of two functions is summarized below.
  
  The first function cplm is designed for change point detection in linear model, that is, the point when the regression coefficients start to change. This method converts the problem into a variable selection problem and apply the idea of knockoff filter so that it can detect change points with both high power and guaranteed FDR control. The argument X is the design matrix and Y is the response vector. The argument target sets the desired FDR one wants to obtain. The argument c is the tuning parameter determining the length of intervals and is typically set to 1 or 2 from a Bayesian perspective. The argument changepoint is the oracle location of the change points. Since this function is used for simulation purpose, the oracle location is provided as contrast. It returns the FDR, modified FDR and power of two knockoff thresholds.
  
  The second function Inde_test aims at nonparametric test of independence between two random variables. The main idea of this test is to estimate the joint density and marginal density, respectively. And then test if the joint density is the product of two marginal density. The asymptotic distribution of the test statistic is used to compute p-value. The argument data is a matrix or dataframe with two columns. Then the independence of the two columns is tested. It returns the test statistic and p-value. 
}
\author{
Zhenghao Zeng

Maintainer: Zhenghao Zeng <zhzeng1998@outlook.com>
}
\references{
  See slides of course "Statistical Computing"
}
\keyword{ package }
