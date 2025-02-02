% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_megan.R
\name{parse_megan}
\alias{parse_megan}
\title{Parse MEGAN Output Files and Create a Phyloseq Object}
\usage{
parse_megan(file_path, to_phyloseq = FALSE)
}
\arguments{
\item{file_path}{A string containing the path to the MEGAN output file (XML format).}

\item{to_phyloseq}{Logical. If \code{TRUE}, converts the parsed data into a \code{phyloseq} object. Default is \code{FALSE}.}
}
\value{
A data frame with parsed taxonomic data or a \code{phyloseq} object if \code{to_phyloseq = TRUE}.
}
\description{
This function parses MEGAN output files, typically in XML format, for taxonomic and functional data.
It can create a \code{phyloseq} object for downstream analysis.
}
\details{
The function reads an XML file generated by MEGAN, extracts taxonomic assignments,
and summarizes the counts for each taxonomic rank. Optionally, it can create a \code{phyloseq} object.
}
