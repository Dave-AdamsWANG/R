\name{generate.samples}
\alias{sample}
\title{Generate Samples}
\usage{
generate.samples(size = 100,p = 0.8 , q = 0.1)
}
\description{
'size' represents the size of the sample. 'p' and 'q' are the gene frequency of A and B respectively. The return value is a numeric vector of the populations of A, AB, B, O in order. This helps generate a random vector which may be used to demonstrate the usage of other functions even without experimental data.
}
\examples{
generate.samples()
}
\name{choose.p_i}
\alias{choose}
\title{Choose Probability!}
\usage{
choose.p_i(sample_n = c(4517,572,1325,3586))
}
\description{
The argument is a vector of the populations of A, AB, B, O in order. The return value is an estimation close the the real value of p and q in the form of a numeric vector, which may be used in MLE.
}
\examples{
choose.p_i()
}

\name{judgeOrder}
\alias{judge}
\title{Judge Final Information}
\usage{
judgeOrder(target = c("B","A"), W = c("A","B","O"),wt = c(1,1,0))
}
\description{
Judge orders of elements in a character vector and return the information with the greatest weight. When more then one piece of information with identical weight occurs, they will be in alphabetical order, and the return value will be a vector of char of length 1. 'target' is the target vector. 'W' contains all the possible characters. 'wt' contains the corresponding weight. This function will help the implementation of 'generate.samples_simulation' as well as in other necessary occasions.
}
\examples{
judgeOrder()
judgeOrder(target = c("B","A"), W = c("A","B","O"),wt = c(1,2,0))
}

\name{generate.samples_simulation}
\alias{simulation}
\title{Simulate the process of generating blood groups}
\usage{
generate.samples_simulation(size = 100,total = 2, W=c("A","B","O"),wt=c(1,1,0),wp=c(0.8,0.1,0.1))
}
\description{
This is function simuating the process of generating blood groups, and produces result indifferent from 'generate.samples', but it is more close to the real case. 'size' represents the size of the sample. 'total' is the total number of attributes in each individual. 'W' contains all the possibilities of the attributes. 'wt' and 'wp' are the corresponding weight and frequency. When total, W and wt are the default arguments, this function is identical to the simulation of blood groups. This function may apply to other occasions as well.
}
\examples{
generate.samples_simulation()
}

