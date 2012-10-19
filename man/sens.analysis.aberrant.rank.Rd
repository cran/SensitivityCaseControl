\name{sens.analysis.aberrant.rank}
\alias{sens.analysis.aberrant.rank}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sensitivity analysis for aberrant rank test.
}
\description{
This function carries out sensitivity analysis for the aberrant rank test for matched case-control studies.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sens.analysis.aberrant.rank(cases.exposed, referents.exposed, case.rank, no.referents, Gamma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cases.exposed}{
Vector which contains the number of cases exposed in each matched set.
%%     ~~Describe \code{cases.exposed} here~~
}
  \item{referents.exposed}{
Vector which contains the number of referents (controls) exposed in each matched set.
%%     ~~Describe \code{referents.exposed} here~~
}
  \item{case.rank}{
Vector which contains the rank of the cases' extremeness of disease, where 1 corresponds to the least extreme disease and I=number of matched sets corresponds to the most extreme disease.
%%     ~~Describe \code{case.rank} here~~
}
  \item{no.referents}{
Vector which contains the number of referents (controls) in each matched set.
%%     ~~Describe \code{no.referents} here~~
}
  \item{Gamma}{
The sensitivity parameter Gamma at which the test is conducted.  
%%     ~~Describe \code{Gamma} here~~
}
}
\details{
The test being conducted is of the null hypothesis of no treatment effect vs. a one-sided alternative that exposure to the treatment increases the outcome.  The function outputs the lower and upper bounds on the p-values for testing the null hypothesis for all distributions of treatment assignments that are consistent with the  sensitivity parameter Gamma.  The test is rejected if the upper bound on the p-value is less than alpha.   The sensitivity parameter Gamma is the maximum amount by which two subjects matched for observed covariates may differ in their odds of exposure to the treatment.  The test statistic is the sum of the aberrant ranks of the cases in the matched sets where the case is exposed to treatment, where the aberrant ranks are the ranks of the cases in extremeness of disease (1 corresponds to the least extreme disease and I = number of matched sets corresponds to the most extreme disease).  See Small, Cheng, Halloran and Rosenbaum (2012) for further details.  
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{lower.bound.pval}{Lower bound on the p-value for all distributions of treatment assignment consistent with the sensitivity parameter Gamma.}
\item{upper.bound.pval}{Upper bound on the p-value for all distributions of treatment assignment consistent with the sensitivity parameter Gamma.}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Small, D.S., Cheng, J., Halloran, M.E. and Rosenbaum, P.R. (2012).  "Case Definition and Design Sensitivity."  Working paper.
%% ~put references to the literature/web site here ~
}
