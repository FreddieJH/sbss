## code to prepare `DATASET` dataset goes here

# all_spp <-
#   sbss::load_taxalist() %>%
#   # head(6) %>%
#   dplyr::pull(Species)
#
# set.seed(1)
# example_species <-
#   tibble::tibble(species =
#                    sample(x = all_spp,
#                           size = 20,
#                           replace = F)
#   )

usethis::use_data(example_species, overwrite = TRUE)
