taxa <- load_taxalist()


my_spp <-
  load_taxalist() %>%
  head(6) %>%
  dplyr::select(Species)

lookup_lmax(my_spp)
lookup_lw(my_spp)

test <-
  load_taxalist() %>%
  dplyr::slice(sample(dplyr::n(), 6)) %>%
  dplyr::select(Species) %>%
  dplyr::mutate(rel_abun = runif(n = 6, 0, 1)) %>%
  dplyr::mutate(group = 1)

test %>%
  lookup_lmax() %>%
  lookup_lw() %>%
  dplyr::mutate(Mmax = a*(Lmax^b)) %>%
  dplyr::mutate(meanlog = -1.010 + (0.781*log(Mmax)),
                sdlog = 0.995)


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

test2 <-
  test %>%
  lookup_lmax() %>%
  lookup_lw() %>%
  estimate_SSD_params() %>%
  calc_CSS(group_var = "group")


test2 %>%
  ggplot2::ggplot() +
  ggplot2::aes(log2(m), log2(norm_density)) +
  ggplot2::geom_point()




plot_CSS(test2, normalised = F, logbase = 10)




test2$data[[1]]


  dplyr::select(-data) %>%
  dplyr::mutate(pm_s = n_s_fit/n_s) %>%
  dplyr::rename(m = bin_mid) %>%
  dplyr::mutate(norm_density = n_s_fit/bin_width) %>%
  dplyr::mutate(density = n_s_fit) %>%
  # removing size bins that are expected to contain >1 indiv per transect
  dplyr::left_join(nsurv) %>%
  dplyr::mutate(est_survey_abundance = n_s_fit*500*n_surveys) %>%
  dplyr::filter(est_survey_abundance > 1)




  plot_SSDs(dat1, max_y = 0.1)
  plot_SSDs(dat1, log = T, logbase = 2)
