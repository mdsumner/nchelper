u <- "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/access/avhrr-only/201801/avhrr-only-v2.20180126.nc"
curl::curl_download(u, file.path("inst/extdata", basename(u)))

#f <- file.path("inst/extdata", basename(u))
#f <- system.file("extdata", "avhrr-only-v2.20180126.nc", package = "nchelper")
#a <- nchelper(f, "sst")
