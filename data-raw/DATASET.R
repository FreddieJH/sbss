## code to prepare `DATASET` dataset goes here

set.seed(1)
example_data <-
  tibble::tibble(Species =
                   sample(x =
                            sbss::load_taxalist() |>
                            dplyr::filter(Class %in% c("Actinopteri",
                                                       "Asteroidea",
                                                       "Gastropoda",
                                                       "Elasmobranchii",
                                                       "Malacostraca",
                                                       "Echinoidea")) |>
                            dplyr::pull(Species),
                          size = 20,
                          replace = F),
                 abundance = rpois(n = 20, lambda = 4),
                 site = "A"
  )

usethis::use_data(example_data, overwrite = TRUE)


# Internal data ================================================================

lmax_tbl <- load_lmax()
taxa_tbl <- load_taxalist()
lw_tbl <- load_lw()

usethis::use_data(lmax_tbl, taxa_tbl, lw_tbl, overwrite = TRUE)
