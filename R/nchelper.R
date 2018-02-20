#' nchelper
#' 
#' Helper functions to treat a NetCDF variable as an R array
#'
#' Create an object to represent a NetCDF variable, primarily to access slices from it with R's Extract syntax for arrays.
#' @param file NetCDF file
#' @param varname NetCDF variable to access
#' @return nchelper object
#' @export
#' @importFrom RNetCDF open.nc file.inq.nc var.inq.nc dim.inq.nc var.get.nc
#' @examples
#' f <- system.file("extdata", "avhrr-only-v2.20180126.nc", package = "nchelper")
#' a <- nchelper(f, "sst")
#' plot(a[,360,,] * 0.01, ylim = c(20, 32), type = "l")
nchelper <- function(file, varname = NULL) {
    #if (!file.exists(file)) stop("File does not exist: ", file)

    nc <- try(RNetCDF::open.nc(file))
    on.exit(RNetCDF::close.nc(nc), add = TRUE)
    if (inherits(nc, "try-error")) stop("Cannot open source: ", file, nc)
    ##on.exit(close.nc(nc))

    fileinq <- file.inq.nc(nc)
    nvars <- fileinq$nvars
    if (is.null(varname)) {
        vars <- character(nvars)
        for (i in seq_len(nvars)) {
            vars[i] <- var.inq.nc(nc, i-1)$name
        }
        varname <- vars[1]
        warning("no varname specified, returning ", varname, "\n choose varname from ", paste(vars, collapse = ","))
    }
    varinq <- var.inq.nc(nc, varname)
    dims <- integer(length(varinq$dimids))
    for (i in seq_along(dims)) dims[i] <- dim.inq.nc(nc, varinq$dimids[i])$length
    structure(.Data = list(file = file, varinq = varinq, dims = dims), class = "nchelper")
}

#' dim
#' 
#' @param x `nchelper` object
#' @export
dim.nchelper <- function(x) {
    x$dims
}
#' names
#' @param x `nchelper`` object
#' @export
names.nchelper <- function(x) {
    x$varinq$name
}
#' Extract
#' 
#' @param x `nchelper` object
#' @param ... index arguments, see `base::Extract`
#' @param drop remove degenerate singleton dimensions, `TRUE` by default
#' @export
"[.nchelper" <- function(x, ..., drop = TRUE) {
    ncdims <- dim(x)
    ndims <- length(ncdims)
    indexlist <- match.call(expand.dots = FALSE)[["..."]]

    ## process indexes
    indexcount <- length(indexlist)
    if (!length(ncdims) == indexcount) stop("index position and number must match variable", names(x), paste0("[", paste(ncdims,collapse = ","), "]"))
    ## TODO: check sensible subsets:
    ## note that we can only handle indexes that are contiguous and increasing
    ## we now have respectable indexes
    starts <- counts <- integer(ndims)
    .processIndex <- function(index, dimsize) {
        if (is.language(index) & index == "") {
            index <- c(1, dimsize)
        } else {
            index <- eval(index)
            index <- c(index[1], length(index))
        }
        index
    }

    ## not monotonically increasing
    ## not within 1:n
    for (i in seq_along(indexlist)) indexlist[[i]] <- .processIndex(indexlist[[i]], ncdims[i])
##return(index)
    nc <- RNetCDF::open.nc(x$file)
    on.exit(RNetCDF::close.nc(nc), add = TRUE)
    d <- var.get.nc(nc, x$varinq$name, start = sapply(indexlist, "[", 1), count = sapply(indexlist, "[", 2))

    d
}


