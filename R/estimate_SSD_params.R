#' Estimate species size distribuion (SSD)
#'
#' This function estimates a Species size distribuion based on
#' parameters from asymptotic mass
#'
#' @param x A tibble or dataframe with Lmax, a and b
#' @return A tibble of meanlog and sdlog parameters of SSD
#' @export
estimate_SSD_params <- function(x, meanlog_slope = 0.781, meanlog_intercept =  -1.010, sdlog = 0.995){
  x %>%
    dplyr::mutate(Mmax = .data[["a"]]*(.data[["Lmax"]]^.data[["b"]])) %>%
    dplyr::mutate(meanlog = meanlog_intercept + (meanlog_slope*log(.data[["Mmax"]])),
                  sdlog = sdlog)
}
