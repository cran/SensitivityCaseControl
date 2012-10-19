\name{mhxct.weighted}
\alias{mhxct.weighted}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Distribution of Mantel-Haenszel test statistic.
}
\description{
Internal function that computes the distribution of m*Mantel-Haenszel statistic for sensitivity parameter Gamma.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mhxct.weighted(mi, Ji, gamma = 1, m = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mi}{
Number of exposed subjects in each matched set.
%%     ~~Describe \code{mi} here~~
}
  \item{Ji}{
Number of subjects in each matched set.
%%     ~~Describe \code{Ji} here~~
}
  \item{gamma}{
Sensitivity parameter.
%%     ~~Describe \code{gamma} here~~
}
  \item{m}{
Factor by which Mantel-Haenszel statistic should be multiplied (default is 1).  
%%     ~~Describe \code{m} here~~
}
}
\value{
Upper bound on the probability that m*Mantel-Haenszel statistic equals 0, 1,,...maximum value when the null hypothesis of no treatment effect is true and the sensitivity parameter is Gamma.  
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
