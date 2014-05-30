\name{adaptive.noether.brown}
\alias{adaptive.noether.brown}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Adaptive test for observational studies that adapts between Noether's and Brown's test statistics
}
\description{
Sensitivity analysis for a matched pair cohort study that rejects the null hypothesis of no treatment effect if either Brown's statistic or Noether's statistic is greater than respective critical values.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
adaptive.noether.brown(y,Gamma,alpha=.05,lambda1=1/3,lambda2=2/3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
Vector which gives matched pair differences between treated and control unit response in each matched pair
%%     ~~Describe \code{no.exposed.in.matched.set} here~~
}
\item{Gamma}{
The value of the sensitivity analysis parameter Gamma for which the test will be conducted.
%%     ~~Describe \code{Gamma} here~~
}
  \item{alpha}{
The significance level at which the test is conducted (default = .05)
%%     ~~Describe \code{alpha} here~~
}
\item{lambda1}{The lambda1 parameter for defining Brown's and Noether's test statistics.  See details.
}
\item{lambda2}{The lambda2 parameter for defining Brown's and Noether's test statistics.  See details.
}
}
\details{
Let 0<=lambda1<=lambda2<=1.  Let B1 be the number of pairs for whom the treated unit in the pair has a greater outcome than the control unit and rank among the I pairs of the absolute value of the difference between the treated and control unit's outcomes is greater than or equal to (1 - lambda1)I, where I is the number of pairs, 
Also, let B2 be the number of pairs for whom the treated unit in the pair has a greater outcome than the control unit and the rank of the absolute value of the difference between the treated and control unit's outcomes is is less than or equal to (1-lambda1)I but greater than or equal to (1 - lambda2)I.  Noether (1973) proposed B1 as a test statistic, and Brown (1981) (and also Markowski and Hettmansperger (1982)) proposed T = 2B1 + B2 as a
test statistic.  The adaptive test combines these two test statistics.  See Rosenbaum (2012) for the details.   
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{alpha}{Significance level at which the test is conducted.}
\item{Gamma}{The value of the sensitivity parameter Gamma at which the test of the null hypothesis of no treatment effect is conducted.}
\item{testinfomat}{Matrix with information about the test.  The first column lists the critical values for Noether's and Brown's test statistics.  The second column lists the observed values of these two test statsitics.  The null hypothesis is rejected if the observed value of either test statistic is greater than its critical value.}  
\item{overall.test.result}{"Accept H0" or "Reject H0" according to whether the null hypothesis of no treatment effect can be rejected for sensitivity parameter Gamma.}
}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\references{
Rosenbaum, P.R. (2012).  "An Exact Adaptive Test with Superior Design Sensitivity in an Observational Study of Treatments for Ovarian Cancer."  Annals of Applied Statistics, 6, 83-105.  
%% ~put references to the literature/web site here ~
}
\examples{
# Data on volume of hippocampus in 15 sets of twins in which one twin is affected by schizophrenia
# and one twin is not (data from R.L. Suddath et al., "Anatomical Abnormalities in the Brains of 
# Monozygotic Twins Discordant for Schizophrenia," New England Journal of Medicine 322, 1990
# pp. 789-93.  Data taken from the Statistical Sleuth, F.L. Ramsey and D.W. Schafer
unaffected=c(1.94,1.44,1.56,1.58,2.06,1.66,1.75,1.77,1.78,1.92,1.25,1.93,2.04,1.62,2.08)
affected=c(1.27,16.3,1.47,1.39,1.93,1.26,1.71,1.67,1.28,1.85,1.02,1.34,2.02,1.59,1.97)
difference=unaffected-affected
adaptive.noether.brown(difference,Gamma=1.1)
# The null hypothesis of no treatment is rejected for Gamma=1.13 but not for Gamma=1.14
}
