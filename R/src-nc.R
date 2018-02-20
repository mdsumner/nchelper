#' @importFrom dplyr src
src_nc <- function(con, ...) {
  x <- src("nc") 
  x$con <- con
  x
}
#' @importFrom dplyr db_list_tables
db_list_tables.src_nc <- function(con) {
  ncmeta::nc_vars(con$con)$name
}
print.src_nc <- function(x, ...) {
  cat(sprintf("NetCDF source: %s\n  available variables:\n %s ", 
              x$con,
              paste(db_list_tables(nc), collapse = ", ")))  
}
#' @importFrom dplyr tbl
tbl.src_nc <- function(x, from, ...) {
  if (missing(from)) stop("need 'from' variable name")
  stopifnot(from %in% db_list_tables(x))
  a <- nchelper(x$con, from)
  dm <- dim(a)
  starts <- rep(1, length(dm))
  counts <- starts
  counts[1] <- 6
  tibble::as_tibble(setNames(list(RNetCDF::var.get.nc(RNetCDF::open.nc(x$con), from, start = starts, count = counts)), from))
}