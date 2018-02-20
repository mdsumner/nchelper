
tbl_nc <- function(variable) {
  tibble::as_tibble(setNames(list(RNetCDF::var.get.nc(RNetCDF::open.nc(ncsource), variable)), 
                             variable))
}
src_tbls.src_nc <- function(x) {
  ncmeta::nc_vars(x$ncsource)$name
}
print.src_nc <- function(x, ...) {
  print(x$name)
  print(src_tbls(x))
}
#' Title
#'
#' @param ncsource 
#'
#' @return
#' @export
#'
#' @examples
#' f <- system.file("extdata", "S2008001.L3m_DAY_CHL_chlor_a_9km.nc", package = "ncmeta")
#' src_nc(f)
src_nc <- function(ncsource) {
  structure(list(tbl_f = tbl_nc,
                 name = sprintf("<NetCDF: %s>", ncsource),
                 ncsource = ncsource),
            class = c("src_nc", "src")
  )
}


#' @importFrom dplyr tbl
# tbl.src_nc <- function(x, from, ...) {
#   if (missing(from)) stop("need 'from' variable name")
#   stopifnot(from %in% db_list_tables(x))
#   a <- nchelper(x$con, from)
#   dm <- dim(a)
#   starts <- rep(1, length(dm))
#   counts <- starts
#   counts[1] <- 6
#   tibble::as_tibble(setNames(list(RNetCDF::var.get.nc(RNetCDF::open.nc(x$con), from, start = starts, count = counts)), from))
# }