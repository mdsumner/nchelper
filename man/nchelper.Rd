\name{nchelper}
\alias{[.nchelper}
\alias{dim.nchelper}
\alias{names.nchelper}
\alias{nchelper}
\title{nchelper}
\usage{
  nchelper(file, varname = NULL)

  dim.nchelper(x)

  names.nchelper(x)

  [.nchelper(x, ..., drop = TRUE)
}
\arguments{
  \item{file}{NetCDF file}

  \item{varname}{NetCDF variable to access}
}
\value{
  nchelper object
}
\description{
  Helper functions to treat a NetCDF variable as an R array
}
\details{
  Create an object to represent a NetCDF variable,
  primarily to access slices from it with R's Extract
  syntax for arrays.
}
\examples{
f <- "E:\\\\DATA\\\\Reynolds\\\\sst.wkmean.1990-present.nc"
x <- nchelper(f, varname = "sst")
x[,2,3]
}

