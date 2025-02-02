% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_substances.R
\name{compare_substances}
\alias{compare_substances}
\title{Compare Microbial Communities Between Substance Use Types}
\usage{
compare_substances(data, substance_col)
}
\arguments{
\item{data}{A data frame containing microbial abundance data. Must include columns for substance use type and microbial species.}

\item{substance_col}{The column name indicating substance use type (e.g., "SubstanceUseType").}
}
\value{
A list with three elements:
\itemize{
\item \code{summarized_data}: A data frame summarizing species counts by substance type.
\item \code{anova_results}: A data frame with statistical results (ANOVA + Tukey's HSD).
\item \code{visualization}: A ggplot object visualizing species abundance by substance type.
}
}
\description{
This function compares microbial communities across different types of substance use.
}
