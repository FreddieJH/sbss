
# Sum up species size distributions
sum_SSD <- function(x, meanlog_vec, sdlog_vec, weight_vec){
  totl <- rep(0, length(x))
  for(i in seq_along(meanlog_vec)){
    totl <- totl + weight_vec[i]*dlnorm(x       = x,
                                        meanlog = meanlog_vec[i],
                                        sdlog   = sdlog_vec[i])
  }
  return(totl)
}



# function to sum lnorms
integrate_CSD <- function(df, min, max) integrate(function(x) sum_SSD(x,
                                                                    sdlog_vec = df$sdlog,
                                                                    meanlog_vec = df$meanlog,
                                                                    weight_vec = df$rel_abun),
                                               lower = min,
                                               upper = max)$value


# create empty logbins to be used to fill in with estimated abundance
create_empty_logbins <- function(max, logbase = exp(1)){

  max_bin <- log(max, base = logbase) %>% ceiling()

  data.frame(bin_num = 1:max_bin) %>%
    dplyr::mutate(
      bin_floor = logbase^bin_num,
      bin_ceiling = logbase^(bin_num + 1),
      bin_mid = (bin_floor + bin_ceiling) / 2,
      bin_width = bin_ceiling - bin_floor
    )
}


#' Calculate the commuinity size spectrum
#'
#' This function estimates a size spectrum based on
#'
#' @param x A tibble or dataframe with Species, rel_abun, meanlog, sdlog, Mmax, and a grouping variable (e.g. site)
#' @return A tibble of normalised and non-normalised abundance with body size (m)
#' @export
calc_CSS <- function(x, group_var) {

  x.trim <- x %>% tidyr::drop_na(meanlog, sdlog, rel_abun)

  if(nrow(x.trim) < nrow(x)){

    print("Excluded species: ")

    x %>%
      dplyr::filter(!(x$Species %in% x.trim$Species)) %>%
      dplyr::pull(Species) %>%
      print()

  }

  x.trim %>%
    dplyr::group_by_at(group_var) %>%
    tidyr::nest() %>%
    dplyr::mutate(total_abun = purrr::map_dbl(data, ~sum(.$rel_abun, na.rm = T))) %>%
    dplyr::mutate(max_size = purrr::map_dbl(data, ~max(.$Mmax, na.rm = T))) %>%
    dplyr::mutate(empty_table = list(create_empty_logbins(max(max_size, na.rm = T), logbase = 2))) %>%
    tidyr::unnest(cols = c(empty_table)) %>%
    dplyr::mutate(n_s_fit = purrr::pmap_dbl(.l = list(data, bin_floor, bin_ceiling), integrate_CSD)) %>%
    dplyr::select(-data) %>%
    dplyr::mutate(pm_s = n_s_fit/total_abun) %>%
    dplyr::rename(m = bin_mid) %>%
    dplyr::mutate(norm_density = n_s_fit/bin_width) %>%
    dplyr::mutate(density = n_s_fit) %>%
    dplyr::select(group, m, norm_density, density)

}
