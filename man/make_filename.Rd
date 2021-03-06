% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{make_filename}
\alias{make_filename}
\title{Generate filename for motor vehicle crash year}
\usage{
make_filename(year)
}
\arguments{
\item{year}{A string or integer giving the year to use in the filename}
}
\value{
This function returns a string containing the filename for motor vehicle crashes in a particular year.
		An warning message is returned if the input value cannot be coerced to an integer value.
}
\description{
This function (make_filename) creates a string containing a filename.
The user input year is converted to an integer.
The year is then incorporated into a string with prefix "accident_" and suffix ".csv.bz2".
}
\examples{
\dontrun{
make_filename(2013)
make_filename("2013")
make_filename(year=2013)
}

}
