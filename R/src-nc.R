#' Tbl
#'
#' 
#' See \code{dplyr::\link[dplyr]{tbl}} for details.
#'
#' @name tbl
#' @rdname dplyr-verbs
#' @keywords internal
#' @importFrom dplyr tbl 
#' @export tbl
#' @importFrom dbplyr op_base
#' @importFrom tibble trunc_mat
#' @importFrom rlang .data
#' @export
#' @examples 
#' f <- system.file("extdata", "S2008001.L3m_DAY_CHL_chlor_a_9km.nc", package = "ncmeta")
#' src_nc(f) %>% tbl("lat")
#' src_nc(f) %>% tbl("palette")
#' 
#' f2 <- system.file("extdata", "avhrr-only-v2.20180126.nc", package = "nchelper")
#' tbl(src_nc(f2), "sst")
#' tbl(src_nc(f2), "zlev")
#' tbl(src_nc(f2), "time")
#' tbl(src_nc(f2), "lat")
#' tbl(src_nc(f2), "lon")
tbl.src_nc <- function(src, variable, ...) {
  varcompare <- variable
  grids <- ncmeta::nc_grids(src$ncsource)
  grid_vars <- dplyr::inner_join(dplyr::filter(grids, variable == varcompare) %>% 
                               dplyr::select(.data$grid), grids, "grid")
  dim_vars <- ncmeta::nc_axes(src$ncsource) %>% dplyr::filter(.data$variable == varcompare) %>% 
    dplyr::inner_join(ncmeta::nc_dims(src$ncsource), c("dimension" = "id"))
  all_vars <- c(dim_vars$name, grid_vars$variable)
  print(all_vars)
  out <- dplyr::make_tbl("lazy", ops = op_base(NULL, unique(all_vars), class= "ncdb"), src = src)
  class(out) <- c("tbl_ncdb", class(out))
  out
}
#' @export
as.data.frame.tbl_ncdb <- function(x, row.names = NULL, optional = NULL,
                                   ..., n = Inf) {
  collect(x, ..., n = n)
}
#' Collect
#'
#' 
#' See \code{utils::\link[utils]{head}} for details.
#'
#' @name head
#' @keywords internal
#' @export
#' @importFrom utils head
head.tbl_ncdb <- function(x, n = 6L, ...) {
  collect(x, ..., n = n)
}


#' Collect
#'
#' 
#' See \code{dplyr::\link[dplyr]{collect}} for details.
#'
#' @name collect
#' @rdname dplyr-verbs
#' @keywords internal
#' @export
#' @importFrom dplyr collect
#' @export collect
collect.tbl_ncdb <- function(x, ...,  n = Inf ) {
  read_nr_ncdb(x, nmax = n)
}

var_dim_get <- function(con, x) {
  tib <- tibble::tibble(variable = x$ops$vars)
  vars <- ncmeta::nc_vars(con) %>% dplyr::inner_join(tib, c("name" = "variable"))
  vars$dim_var <- vars$ndims == max(vars$ndims)
  vars
}
read_nr_ncdb <- function(x, nmax = -1, ...) {
  if (nmax < 1) nmax <- 1
#  if (nmax  < 1) return(tibble::
  nc_con <- RNetCDF::open.nc(x$src$ncsource)
  variables <- var_dim_get(nc_con, x)
  var_variables <- variables$name[variables$dim_var]
  dim_variables <- variables$name[!variables$dim_var]
  #browser()
  dims <- ncmeta::nc_axes(nc_con, var_variables[1L]) %>% dplyr::inner_join(ncmeta::nc_dims(nc_con), c("dimension" = "id"))
  starts <- rep(1L, nrow(dims))
  counts <- rep(1L, nrow(dims))
  counts[1L] <- min(c(nmax, as.integer(prod(dims$length))))
  df <- tibble::as_tibble(stats::setNames(purrr::map(var_variables, ~as.vector(RNetCDF::var.get.nc(nc_con, .x,
                                                       start = starts, count = counts))), 
           var_variables))
  ## TODO expand out the dim vars
  df[dim_variables] <- NA  
  df
}
#' @export
#' @importFrom dbplyr op_vars
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
#' @param ncsource file or online link NetCDF source
#'
#' @return a 'src_nc', a virtual-database for a NetCDF source
#' @export
#'
#' @examples
#' f <- system.file("extdata", "S2008001.L3m_DAY_CHL_chlor_a_9km.nc", package = "ncmeta")
#' nc <- src_nc(f)
#' print(nc)
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

