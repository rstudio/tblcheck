% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grade.R
\name{tblcheck_grade}
\alias{tblcheck_grade}
\alias{tblcheck_grade.default}
\alias{tblcheck_grade.list}
\alias{tblcheck_grade.tblcheck_problem}
\title{Apply automatic grading to a problem object}
\usage{
tblcheck_grade(problem, max_diffs = 3, env = parent.frame(), ...)

\method{tblcheck_grade}{default}(problem, max_diffs = 3, env = parent.frame(), ...)

\method{tblcheck_grade}{list}(problem, max_diffs = 3, env = parent.frame(), ...)

\method{tblcheck_grade}{tblcheck_problem}(problem, max_diffs = 3, env = parent.frame(), ...)
}
\arguments{
\item{problem}{A problem generated by a \verb{tbl_check_*()} function.}

\item{max_diffs}{\verb{[numeric(1)]}\cr The maximum number of mismatched values to
display in an informative failure message.
Passed to \code{\link[=tbl_check_names]{tbl_check_names()}} to determine the number of mismatched column
names to display and the \code{n_values} argument of \code{\link[=tbl_check_column]{tbl_check_column()}} to
determine the number of mismatched column values to display.
Defaults to 3.}

\item{env}{The environment used for grading.}

\item{...}{
  Arguments passed on to \code{\link[gradethis:graded]{gradethis::fail}}
  \describe{
    \item{\code{hint}}{Include a code feedback hint with the failing message? This
argument only applies to \code{fail()} and \code{fail_if_equal()} and the message is
added using the default options of \code{\link[gradethis:code_feedback]{give_code_feedback()}} and
\code{\link[gradethis:code_feedback]{maybe_code_feedback()}}. The default value of \code{hint} can be set using
\code{\link[gradethis:gradethis_setup]{gradethis_setup()}} or the \code{gradethis.fail.hint} option.}
    \item{\code{encourage}}{Include a random encouraging phrase with
\code{\link[gradethis:praise]{random_encouragement()}}? The default value of \code{encourage} can be set
using \code{\link[gradethis:gradethis_setup]{gradethis_setup()}} or the \code{gradethis.fail.encourage} option.}
  }}
}
\value{
A \code{\link[gradethis:graded]{gradethis::fail()}} message or \code{NULL} invisibly.
}
\description{
Apply automatic grading to a problem object
}
\examples{
.result <- 1:10
.solution <- letters[1:10]
problem <- vec_check()
tblcheck_grade(problem)
}
