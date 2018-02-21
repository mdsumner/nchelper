simulate_nc <- function() {
  structure(list(), class = c("NetCDF", "DBIConnection"))
}



#' @importFrom dplyr tbl
#' @export
tbl.src_nc <- function(src, variable, nmax = 10) {
  nc_con <- RNetCDF::open.nc(src$ncsource)
  dims <- ncmeta::nc_dims(nc_con, variable)
  #var <- ncmeta::nc_var(nc_con, variable)
  starts <- rep(1L, nrow(dims))
  counts <- rep(1L, nrow(dims))
  counts[1L] <- min(c(nmax, as.integer(dims$length[1])))
  
  #  print(sprintf("returning %i rows of %i", counts[1L], as.integer(prod(dims$length))))
  df <-   tibble::as_tibble(setNames(list(as.vector(RNetCDF::var.get.nc(nc_con, variable, 
                                                                        start = starts, count = counts))), 
                                     variable))
  df
  #df_nc <- dbplyr::tbl_lazy(df, src = simulate_sqlite())
  #' df_sqlite %>% summarise(x = sd(x)) %>% show_query()
  out <- dplyr::make_tbl("lazy", ops = op_base(df, names(df)), src = src)
  class(out) <- c("tbl_ncdb", class(out))
  out
}
head.tbl_ncdb <- function(x, n = 6L) {
 
}
dim.tbl_ncdb <- function(x) {
  c(NA, length(op_vars(x$ops)))
}
#' @importFrom dplyr src_tbls
src_tbls.src_nc <- function(x) {
  ncmeta::nc_vars(x$ncsource)$name
}
#' @export
print.src_nc <- function(x, ...) {
  ## needs to be format.src_ncd

  print(sprintf("src: %s", x$name))
  print(sprintf("tbls: %s", paste0(src_tbls(x), collapse = ", ")))
  print(sprintf("origin: %s", dirname(x$ncsource)))
}
#' NetCDF virtual tables
#'
#' @param ncsource 
#'
#' @return a 'src_nc', a virtual-database for a NetCDF source
#' @export
#'
#' @examples
#' f <- system.file("extdata", "S2008001.L3m_DAY_CHL_chlor_a_9km.nc", package = "ncmeta")
#' nc <- src_nc(f)
#' \dontrun{
#' nc <- src_nc("http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdQSwind3day")
#' nc
#' }
src_nc <- function(ncsource) {
  structure(list(tbl_f = tbl.src_nc,
                 name = sprintf("<NetCDF: %s>", basename(ncsource)),
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