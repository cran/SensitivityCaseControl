\name{adaptive.case.test}
\alias{adaptive.case.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Adaptive sensitivity analysis for case-control studies}
\description{
This function carries out sensitivity analysis for matched case-control studies using two Mantel-Haenszel tests with different case definitions.  The case in each matched set either meets a narrow case definition or only meets a more broadly defined case definition.  The function can accommodate designs in which cases are matched to variable numbers of controls (referents).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
adaptive.case.test(no.exposed.in.matched.set, narrowcase, case.exposed, size.matched.set, 
Gamma, narrowmultiplier = 1, alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{no.exposed.in.matched.set}{
Vector which gives the number of subjects exposed in each matched set.
%%     ~~Describe \code{no.exposed.in.matched.set} here~~
}
  \item{narrowcase}{
Vector which contains 1 or 0 according to whether the case in the matched set meets the narrow case definition (1 if meets the narrow case definition).
%%     ~~Describe \code{narrowcase} here~~
}
  \item{case.exposed}{
Vector which is 1 or 0 according to whether the case in the matched set was exposed to the treatment (1 if exposed to treatment)
%%     ~~Describe \code{case.exposed} here~~
}
  \item{size.matched.set}{
Vector which contains the number of subjects in each matched set.
%%     ~~Describe \code{size.matched.set} here~~
}
  \item{Gamma}{
The value of the sensitivity analysis parameter Gamma for which the test will be conducted.
%%     ~~Describe \code{Gamma} here~~
}
  \item{narrowmultiplier}{
The amount by which the narrow cases are multiplied in the second test statistic (see Details).  The default is 1.
%%     ~~Describe \code{narrowmultiplier} here~~
}
  \item{alpha}{
The significance level at which the test is conducted (default = .05).
%%     ~~Describe \code{alpha} here~~
}
}
\details{
The test being conducted is of the null hypothesis of no treatment effect vs. a one-sided alternative that exposure to the treatment increases the outcome.  The result of the test is whether or not the null hypothesis can be rejected for all distributions of treatment assignments that are consistent with the sensitivity parameter Gamma.  The sensitivity parameter Gamma is the maximum amount by which two subjects matched for observed covariates may differ in their odds of exposure to the treatment.  The test is rejected if either the Mantel-Haenszel statistic among the narrow cases is greater than a critical value or narrowmultiplier*Mantel Haenszel statistic among the narrow cases + Mantel-Haenszel statistic among the broad ases is greater than another critical value, where the critical values are determined so that the probability of a Type I error is at most alpha.  See Small, Cheng, Halloran and Rosenbaum (2013) for further details.  
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{alpha}{Significance level at which the test is conducted.}
\item{Gamma}{The value of the sensitivity parameter Gamma at which the test of the null hypothesis of no treatment effect is conducted.}
\item{testinfomat}{Matrix with information about the test.  The first column lists the definition of the two test statistics (one using only the narrow cases and the other using the narrow and marginal cases) that are being used in the adaptive test.  T1 is the Mantel-Haenszel statistic among the narrow cases and T2 is the Mantel-Haenszel test statistic among the broad (narrow + marginal) cases.  The second column lists the critical value for each test statistic.  The third column lists the observed values of these two test statsitics.  The null hypothesis is rejected if the observed value of either test statistic is greater than its critical value.}  
\item{overall.test.result}{"Accept H0" or "Reject H0" according to whether the null hypothesis of no treatment effect can be rejected for sensitivity parameter Gamma.}
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
# Adaptive test based on Table 4 in Small, Cheng, Halloran and Rosenbaum (2013)
no.exposed.in.matched.set=c(rep(0,229),rep(1,23),rep(1,51),rep(2,9),rep(0,373),rep(1,46),rep(1,50),
rep(2,13));
narrowcase=c(rep(1,312),rep(0,482));
case.exposed=c(rep(0,229),rep(0,23),rep(1,51),rep(1,9),rep(0,373),rep(0,46),rep(1,50),rep(1,13));
size.matched.set=rep(2,length(case.exposed))
# Test using the statistics T1 and T1+T2 where T1 is Mantel-Haenszel for narrow cases and T2 is 
# Mantel Haenszel for marginal cases
adaptive.case.test(no.exposed.in.matched.set,narrowcase,case.exposed,size.matched.set,Gamma=1.37,
alpha=.05,narrowmultiplier=1)
# Test using the statistics T1 and 2*T1+T2
adaptive.case.test(no.exposed.in.matched.set,narrowcase,case.exposed,size.matched.set,Gamma=1.37,
alpha=.05,narrowmultiplier=2)

# An example of an adaptive test in which narrow cases are matched to two controls and marginal 
# cases to one control
#      Narrow Cases                    Marginal Cases
# Referents Exposed  0   1   2         0     1   
# Case
# Not Exposed        40  10   6        50    20  
# Exposed            30  14   8        40    30
no.exposed.in.matched.set=c(rep(0,40),rep(1,10),rep(2,6),rep(1,30),rep(2,14),rep(3,8),rep(0,50),
rep(1,20),rep(1,40),rep(2,30))
narrowcase=c(rep(1,40+10+6+30+14+8),rep(0,50+20+40+30))
case.exposed=c(rep(0,40+10+6),rep(1,30+14+8),rep(0,50+20),rep(1,40+30))
size.matched.set=c(rep(3,40+10+6+30+14+8),rep(2,50+20+40+30))
adaptive.case.test(no.exposed.in.matched.set,narrowcase,case.exposed,size.matched.set,Gamma=1.2,
alpha=.05)




}
