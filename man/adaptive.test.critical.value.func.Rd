\name{adaptive.test.critical.value.func}
\alias{adaptive.test.critical.value.func}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Critical values for adaptive test
}
\description{
This is an internal function which takes in a matrix that contains the error probabilities for different values of the test statistics T1.doublebar and T.doublebar  and finds the pair of critical values (k1,k) for T1.doublebar and T.doublebar which satisfy the following: (i) have a Type I error rate of at most alpha; (ii) among pairs of critical values that cannot be improved, the critical values are most equitable, dividing the chance of false rejection as equally as possible between the events T1.doublebar>=k1 and T.doublebar>=k.  
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
adaptive.test.critical.value.func(critmat, alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{critmat}{
Matrix that contains the error probabilities for different values of the test statistics T1.doublebar and T.doublebar that is produced by the function jointcrit
%%     ~~Describe \code{critmat} here~~
}
  \item{alpha}{
Significance level at which the test is conducted (alpha = 0.05).  
%%     ~~Describe \code{alpha} here~~
}
}
\value{
\item{t1}{Critical value k1 for the test statistic T1.doublebar}
\item{t1plust2}{Critical value k for the test statistic T.doublebar}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Small, D.S., Cheng, J., Halloran, M.E. and Rosenbaum, P.R. (2012).  "Case Definition and Design Sensitivity."  Working paper.  See Section 5.1 of the paper.
%% ~put references to the literature/web site here ~
}
