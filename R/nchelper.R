#' NetCDF variables accessed like an R array
#'
#' Access NetCDF variables directly as if they are an R array in memory
#'
#' @name nchelper-package
#' @docType package
#' @author Michael D. Sumner
NULL


##' Helper functions to treat a NetCDF variable as an R array
##'
##' Create an object to represent a NetCDF variable, primarily to access slices from it with R's Extract syntax for arrays.
##' @title nchelper
##' @param file NetCDF file
##' @param varname NetCDF variable to access
##' @return nchelper object
##' @export
##' @examples
##' f <- "E:\\DATA\\Reynolds\\sst.wkmean.1990-present.nc"
##' x <- nchelper(f, varname = "sst")
##' x[,2,3]
nchelper <- function(file, varname = NULL) {
    if (!file.exists(file)) stop("File does not exist: ", file)

    nc <- try(open.nc(file))
    if (inherits(nc, "try-error")) stop("Cannot open file: ", file, nc)
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
    structure(.Data = list(file = file, varinq = varinq, dims = dims, con = nc), class = "nchelper")
}

##' @rdname nchelper
##' @export
dim.nchelper <- function(x) {
    x$dims
}
##' @rdname nchelper
##' @export
names.nchelper <- function(x) {
    x$varinq$name
}
##' @rdname nchelper
##' @export
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
    nc <- x$con
    d <- var.get.nc(nc, x$varinq$name, start = sapply(indexlist, "[", 1), count = sapply(indexlist, "[", 2))

    d
}




