#' Estimate species size distribuion (SSD)
#'
#' This function estimates a Species size distribuion based on
#' parameters from asymptotic mass
#'
#' @param x A tibble or dataframe with Lmax, a and b
#' @return A tibble of meanlog and sdlog parameters of SSD
#' @export
estimate_SSD_params <- function(x){
  x %>%
    dplyr::mutate(Mmax = a*(Lmax^b)) %>%
    dplyr::mutate(meanlog = -1.010 + (0.781*log(Mmax)),
                  sdlog = 0.995)
}
