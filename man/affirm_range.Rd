% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/affirm_range.R
\name{affirm_range}
\alias{affirm_range}
\title{Affirm Range}
\usage{
affirm_range(
  data,
  label,
  column,
  range,
  boundaries = c(TRUE, TRUE),
  id = NA_integer_,
  priority = NA_integer_,
  data_frames = NA_character_,
  report_listing = NULL,
  data_action = NULL,
  error = getOption("affirm.error", default = FALSE)
)
}
\arguments{
\item{data}{a data frame}

\item{label}{a string used to describe the affirmation}

\item{column}{a single column to check values of}

\item{range}{vector of length two indicating the upper and lower bounds of the range.
The class of the \code{range} must be compatible with the \code{column}, e.g. if \code{column}
is numeric, \code{range} must also be numeric; if \code{column} is a date, range must be a
date; if \code{column} is an integer, \code{range} must be an integer, etc.}

\item{boundaries}{logical vector of length 2 indicating whether to include UB and LB in
the range check. Default is \code{c(TRUE, TRUE)}}

\item{id, priority, data_frames}{Optional additional information that will be passed to affirmation report.
\itemize{
\item \code{id} must be an integer, e.g. \code{id = 1L}
\item \code{priority} must be an integer, e.g. \code{priority = 1L}
\item \code{data_frames} string of data frame names used in affirmation, e.g. \code{data_frames = "RAND, DM"}
}}

\item{report_listing}{an expression selecting/filtering rows from \verb{data=} to return in the
issue listing report. The default is to return the result from \code{create_report_listing()},
which are the rows that do \emph{not} met in \verb{condition=}
and columns included in the \verb{condition=} expression along with any columns
set in \code{option('affirm.id_cols')}. The \code{'affirm.id_cols'} option must be a
character vector of column names, where columns will be selected with
\code{dplyr::select(any_of(getOption('affirm.id_cols')))}.}

\item{data_action}{this expression is executed at the end of the function call when supplied.
\itemize{
\item Default is NULL, and the passed data frame in \verb{data=} is returned unaltered.
\item Perhaps you'll need to remove problematic rows: \code{data_action = dplyr::filter(., !(!!condition))}
}}

\item{error}{Logical indicating whether to throw an error when condition is not met. Default is \code{FALSE}.}
}
\value{
data frame
}
\description{
A wrapper for \code{affirm_true()}.
The column, range, and boundaries arguments are used to construct the
\code{affirm_true(condition = column >= range[1] & column <= range[2])} argument.
}
\examples{
affirm_init(replace = TRUE)

dplyr::as_tibble(mtcars) |>
 affirm_range(
   label = "MPG is >0 and <=30",
   column = mpg,
   range = c(0, 30),
   boundaries = c(FALSE, TRUE)
 )

affirm_close()
}
\seealso{
Other Data Affirmations: 
\code{\link{affirm_false}()},
\code{\link{affirm_na}()},
\code{\link{affirm_no_dupes}()},
\code{\link{affirm_true}()},
\code{\link{affirm_values}()}
}
\concept{Data Affirmations}
