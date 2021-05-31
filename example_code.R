taxa <- load_taxalist()


my_spp <-
  load_taxalist() %>%
  head(6) %>%
  dplyr::select(Species)

lookup_lmax(my_spp)
lookup_lw(my_spp)


my_spp %>%
  lookup_lmax() %>%
  lookup_lw() %>%
  dplyr::mutate(Mmax = a*(Lmax^b))
