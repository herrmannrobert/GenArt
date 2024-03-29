% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Functions.R
\name{dna_mutate}
\alias{dna_mutate}
\title{Mutate image DNA}
\usage{
dna_mutate(dna, degree = "soft", maxXY)
}
\arguments{
\item{dna}{matrix or character, untangled or tangled image DNA of any size.}

\item{degree}{numeric, number nucleotides to be modified per mutation, Default = 1.}
}
\description{
Function to mutate an image DNA.
}
\details{
It is recommended to use the softest mutation rate as given by default.
}
\examples{
dna <- dna_untangle(dna_in(rgb = FALSE))
test <- dna_mutate(dna, degree = 20)
pic_compare(dna, test)
}
