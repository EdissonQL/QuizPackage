% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.r
\name{fars_read}
\alias{fars_read}
\title{fars_read}
\usage{
fars_read(filename)
}
\arguments{
\item{filename}{A character string giving the file name (and full path) with data to be read}
}
\value{
if the file exists, the function returns a data frame with the data read
}
\description{
This function reads data from a file in csv format.
The function validates if the file exists but id doesn't show the progress when the data is
loaded from the file.
}
\examples{
\dontrun{
df <- fars_read("./data/data2015.csv")
}

}
