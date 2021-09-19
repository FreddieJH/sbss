#' @title Sum up species size distributions
#'
#' @description Weighted sum of log-normal species body size distributions
#'
#' @param x vector of quantiles
#' @param meanlog vector of lognormal meanlog parameters (mean)
#' @param sdlog vector of lognormal sdlog parameters (standard devaiation)
#' @param weight vector of weights (likely the relative abundance of the species)
#'
#' @return vector of probabilities
#'
#' @export
sum_SSD <- function(x, meanlog, sdlog, weight){

  totl <- rep(0, length(x))

  for (i in seq_along(meanlog)) {
    totl <- totl + weight[i]*stats::dlnorm(x       = x,
                                           meanlog = meanlog[i],
                                           sdlog   = sdlog[i])
  }

  return(totl)
}

#' @title Integrate community size distribution
#'
#' @description Integrates over the summed species
#'
#' @param sdlog table with
#' @param meanlog table with
#' @param rel_abun table with
#' @param min min limit of integration
#' @param max max limit of integration
#'
#' @return integral of function
#'
#' @export
integrate_CSD <- function(data, min, max, abundance_colname = "abundance") {
  stats::integrate(function(x) sum_SSD(x,
                                       sdlog   = data[["sdlog"]],
                                       meanlog = data[["meanlog"]],
                                       weight  = data[[abundance_colname]]),
                   lower = min,
                   upper = max)$value
}


#' @title Create log bin table
#'
#' @description Create an empty log bin table
#'
#' @param max numeric value of maximum size
#' @param logbase numeric value of log base to be used
#'
#' @return A tibble of log bins
#'
#' @export
#' @importFrom rlang .data
#' @import dplyr
create_empty_logbins <- function(max, logbase = exp(1)){

  max_bin <- max |> log(base = logbase) |> ceiling()

  data.frame(bin_num = 1:max_bin) %>%
    dplyr::mutate(
      bin_floor = logbase^.data[["bin_num"]],
      bin_ceiling = logbase^(.data[["bin_num"]] + 1),
      bin_mid = (.data[["bin_floor"]] + .data[["bin_ceiling"]]) / 2,
      bin_width = .data[["bin_ceiling"]] - .data[["bin_floor"]]
    )
}



#' @title Calculate the community size spectrum
#'
#' @description This function estimates a size spectrum based on
#'
#' @param data A tibble or dataframe with Species, rel_abun, meanlog, sdlog, Mmax, and a grouping variable (e.g. site)
#' @param group_var A string of the desired grouping variable
#'
#' @return A tibble of normalised and non-normalised abundance with body size (m)
#'
#' @export
#' @importFrom rlang .data
calc_CSS <- function(data, group_var, abundance_colname = "abundance") {


  data_trim <- data %>% tidyr::drop_na(.data[["meanlog"]], .data[["sdlog"]], .data[[abundance_colname]])

  if (nrow(data_trim) < nrow(data)) {

    print("Excluded species: ")

    data %>%
      dplyr::filter(!(.data[["Species"]] %in% data_trim$Species)) %>%
      dplyr::pull(.data[["Species"]]) %>%
      print()

  }

  data_trim %>%
    dplyr::group_by_at(group_var) %>%
    tidyr::nest() %>%
    dplyr::mutate(total_abun = purrr::map_dbl(data, ~sum(.[[abundance_colname]], na.rm = T))) %>%
    dplyr::mutate(max_size = purrr::map_dbl(data, ~max(.$Mmax, na.rm = T))) %>%
    dplyr::mutate(empty_table = list(create_empty_logbins(max(.data[["max_size"]], na.rm = T), logbase = 2))) %>%
    tidyr::unnest(cols = c(.data[["empty_table"]])) %>%
    dplyr::mutate(n_s_fit = purrr::pmap_dbl(.l = list(data = .data[["data"]],
                                                      min = .data[["bin_floor"]],
                                                      max = .data[["bin_ceiling"]]), integrate_CSD)) %>%
    dplyr::select(-data) %>%
    dplyr::mutate(pm_s = .data[["n_s_fit"]]/.data[["total_abun"]]) %>%
    dplyr::rename(m = .data[["bin_mid"]]) %>%
    dplyr::mutate(norm_density = .data[["n_s_fit"]]/.data[["bin_width"]]) %>%
    dplyr::mutate(density = .data[["n_s_fit"]]) %>%
    dplyr::select(.data[[group_var]], .data[["m"]], .data[["norm_density"]], .data[["density"]])


}
