## code to prepare `DATASET` dataset goes here

set.seed(1)
example_species <-
  tibble::tibble(Species =
                   sample(x = sbss::load_taxalist() |> dplyr::pull(Species),
                          size = 20,
                          replace = F)
  )

usethis::use_data(example_species, overwrite = TRUE)


# Internal data ================================================================

lmax_tbl <- load_lmax()
taxa_tbl <- load_taxalist()
lw_tbl <- load_lw()

usethis::use_data(lmax_tbl, taxa_tbl, lw_tbl, overwrite = TRUE)
