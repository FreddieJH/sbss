#' Estimate species size distribution (SSD)
#'
#' This function estimates a species size distribution based on
#' parameters from the species asymptotic mass and length-weight relationship;
#' \eqn{W = a * L^b}.
#'
#'
#' @param df A tibble or dataframe with columns of asymptotic size, and length-weight estimates (a, b)
#' @param lmax_colname character, the name of the column containing the asymptotic length of the species
#' @param a_colname character, the name of the column containing the species length weight parameter a
#' @param b_colname character, the name of the column containing the species length weight parameter b
#' @param meanlog_eqn function, the relationship between asymptotic mass and meanlog
#' @param sdlog_eqn function, the relationship between asymptotic mass and sdlog
#'
#' @return A tibble of meanlog and sdlog parameters of SSD
#' @export
#'
#' @import dplyr
est_ssd <- function(df, lmax_colname = "lmax", a_colname = "a", b_colname = "b",
                    meanlog_eqn, sdlog_eqn){

  df %>%
    mutate(mmax = .data[[a_colname]]*(.data[[lmax_colname]]^.data[[b_colname]])) %>%
    mutate(meanlog = meanlog_eqn(mmax),
                  sdlog = sdlog_eqn(mmax))

}


