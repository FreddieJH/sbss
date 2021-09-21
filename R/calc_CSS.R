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
sum_ssd <- function(x, meanlog, sdlog, weight){

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
#' @param df table with cols of sdlog, meanlog and relative abundance
#' @param min integer, min body size limit of integration
#' @param max integer, max body size limit of integration
#' @param abundance_colname character string, name of the column containing relative abundance
#'
#' @return integral of function
#'
#' @export
integrate_csd <- function(df, min, max, abundance_colname = "abundance") {
  stats::integrate(function(x) sum_ssd(x,
                                       sdlog   = df[["sdlog"]],
                                       meanlog = df[["meanlog"]],
                                       weight  = df[[abundance_colname]]),
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
#' @param df A tibble or dataframe with Species, rel_abun, meanlog, sdlog, Mmax, and a grouping variable (e.g. site)
#' @param abundance_colname character, name of the column containing abundance information
#' @param group_var A string of the desired grouping variable
#'
#' @return A tibble of normalised and non-normalised abundance with body size (m)
#'
#' @export
#' @importFrom rlang .data
#' @import dplyr
#' @importFrom purrr map_dbl
#' @importFrom purrr pmap_dbl
#' @import tidyr
calc_css <- function(df, group_var, abundance_colname = "abundance") {


  data_trim <- df %>% drop_na(.data[["meanlog"]], .data[["sdlog"]], .data[[abundance_colname]])

  if (nrow(data_trim) < nrow(df)) {

    print("Excluded species: ")

    df %>%
      filter(!(.data[["species_name"]] %in% data_trim$species_name)) %>%
      pull(.data[["species_name"]]) %>%
      print()

  }

  data_trim %>%
    group_by_at(group_var) %>%
    nest() %>%
    mutate(total_abun = map_dbl(data, ~sum(.[[abundance_colname]], na.rm = T))) %>%
    mutate(max_size = map_dbl(data, ~max(.$mmax, na.rm = T))) %>%
    mutate(empty_table = list(create_empty_logbins(max(.data[["max_size"]], na.rm = T), logbase = 2))) %>%
    unnest(cols = c(.data[["empty_table"]])) %>%
    mutate(n_s_fit = pmap_dbl(.l = list(df = .data[["data"]],
                                                      min = .data[["bin_floor"]],
                                                      max = .data[["bin_ceiling"]]), integrate_csd)) %>%
    select(-data) %>%
    mutate(pm_s = .data[["n_s_fit"]]/.data[["total_abun"]]) %>%
    rename(m = .data[["bin_mid"]]) %>%
    mutate(norm_density = .data[["n_s_fit"]]/.data[["bin_width"]]) %>%
    mutate(density = .data[["n_s_fit"]]) %>%
    select(.data[[group_var]], .data[["m"]], .data[["norm_density"]], .data[["density"]])


}
