% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/problem.R
\name{problem}
\alias{problem}
\title{Declare a problem}
\usage{
problem(
  type,
  expected,
  actual,
  ...,
  .class = c(paste0(type, "_problem"), "tblcheck_problem")
)
}
\arguments{
\item{type}{A character string, e.g. \code{column_values} or \code{table_rows}, that
describes the problem that was discovered.}

\item{expected, actual}{The expected and actual values. These should be
included when the value is a summary, e.g. \code{nrow(expected)} or
\code{length(actual)}. Be careful not to include large amounts of data.}

\item{...}{Additional elements to be included in the \code{problem} object.}

\item{.class}{The class of the problem. Typically, we expect the problem
class to be \verb{<type>_problem}, but if you are building custom classes you
may set these classes as desired.}
}
\value{
Returns a problem with class \verb{<type>_problem} and the base classes
\code{tblcheck_problem} and \code{gradethis_problem}.
}
\description{
Useful for constructing a small list to communicate the problem that was
discovered during checking.
}
\examples{
problem(
	type = "class",
	expected = "character",
	actual = "numeric",
	expected_length = 1,
	actual_length = 2
)

}
\seealso{
Other Problem functions: 
\code{\link{problem_grade}()},
\code{\link{problem_message}()},
\code{\link{problem_type}()}
}
\concept{Problem functions}
