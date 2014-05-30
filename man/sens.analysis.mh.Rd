\name{sens.analysis.mh}
\alias{sens.analysis.mh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sensitivity Analysis for Mantel-Haenszel test
}
\description{
Sensitivity analysis for the Mantel-Haenszel test of no treatment effect for matched case-control studies.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sens.analysis.mh(cases.exposed, referents.exposed, no.referents, Gamma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cases.exposed}{
Number of cases exposed to treatment in each matched set.
%%     ~~Describe \code{cases.exposed} here~~
}
  \item{referents.exposed}{
Number of referents (controls) exposed to treatment in each matched set.
%%     ~~Describe \code{referents.exposed} here~~
}
  \item{no.referents}{
Number of referents (controls) in each matched set.  
%%     ~~Describe \code{no.referents} here~~
}
  \item{Gamma}{
The sensitivity parameter Gamma at which the test is conducted.  
%%     ~~Describe \code{Gamma} here~~
}
}
\details{
The test being conducted is of the null hypothesis of no treatment effect vs. a one-sided alternative that exposure to the treatment increases the outcome.  The function outputs the lower and upper bounds on the p-values for testing the null hypothesis for all distributions of treatment assignments that are consistent with the sensitivity parameter Gamma.  The test is rejected if the upper bound on the p-value is less than alpha.   The sensitivity parameter Gamma is the maximum amount by which two subjects matched for observed covariates may differ in their odds of exposure to the treatment.  The test statistic is the Mantel-Haenszel test statistic, which is the sum of cases exposed to treatment.  
See Small, Cheng, Halloran and Rosenbaum (2012) for further details.  
%%  ~~ If necessary, more details than the description above ~~
}

%%  ~~ If necessary, more details than the description above ~~
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
Small, D.S., Cheng, J., Halloran, M.E. and Rosenbaum, P.R. (2013).  "Case Definition and Design Sensitivity."  Journal of the American Statistical Association, 108, 1457-1468.
%% ~put references to the literature/web site here ~
}
\examples{
# Data from narrow cases in Table 1 of Small, Cheng and Halloran, Rosenbaum
cases.exposed=c(rep(0,174+60+14+4+0),rep(1,34+19+6+1+0));
referents.exposed=c(rep(0,174),rep(1,60),rep(2,14),rep(3,4),rep(0,34),rep(1,19),rep(2,6),rep(3,1));
no.referents=rep(4,312);
# Sensitivity analysis for Mantel-Haenszel statistic at Gamma=1.2
sens.analysis.mh(cases.exposed,referents.exposed,no.referents,Gamma=1.2);
}
