\name{jointcrit}
\alias{jointcrit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Joint critical values for adaptive test
}
\description{
Internal function.  Given the probability distribution of independent test statistics T1 and T2, the function finds the joint distribution of T1, T1+T2.  
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
jointcrit(p1, p2, alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p1}{
The probability distribution of the test statistic T1 taking on values 0, 1, 2,..., maximum value
%%     ~~Describe \code{p1} here~~
}
  \item{p2}{
The probability disribution of the test statistic T2 taking on values 0, 1, 2, ..., maximum value
%%     ~~Describe \code{p2} here~~
}
  \item{alpha}{
The significance level at which the test is conducted.
%%     ~~Describe \code{alpha} here~~
}
}
\value{
Matrix which contains for each possible value of t1 and t2, information about the probability of T1>=t1, T1+T2>=t1+t2.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
