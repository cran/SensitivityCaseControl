\name{gconv}
\alias{gconv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Distribution of the convolution of two discrete random variables that taken on nonnegative integer values.
}
\description{
Function finds the distribution for the convolution of two discrete random variables taking on nonnegative integer values.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gconv(g1, g2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g1}{
The probability that the first random variable takes on the values 0, 1, 2,...,maximum value.
%%     ~~Describe \code{g1} here~~
}
  \item{g2}{
The probability that the second random variable takes on the values 0, 1, 2,..., maximum value.
%%     ~~Describe \code{g2} here~~
}
}
\value{
The probability that the convolution of the two random variables takes on the values 0, 1, 2,...,maximum value.  
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Section 2.3 of Small, D.S., Cheng, J., Halloran, M.E. and Rosenbaum, P.R. (2013).  "Case Definition and Design Sensitivity."  Journal of the American Statistical Association, 108, 1457-1468.
%% ~put references to the literature/web site here ~
}
\examples{
## For Gamma=3, I=3 discordant pairs and the Mantel-Haenszel statistic with d_i=1 for all i
gconv(gconv(c(1/4,3/4),c(1/4,3/4)),c(1/4,3/4))
## returns Pr(Tbar=k|m,F) for k=0,1,2,3)
}
