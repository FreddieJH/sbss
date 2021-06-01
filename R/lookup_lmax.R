

#' Clean length-weight ests
#'
#' This function cleans length-weight estimates from sealifebase and fishbase.
#' If no appropriate length-weight estimate is available for a species, the mean
#' for a lower taxonomic level will be used
#'
#' @param spp_lmax_tab A tibble or dataframe with a column of species names ("species_name") and a column of lmax ("species_lmax")
#' @return A tibble of length-weight variables
#' @export
lookup_lmax <- function(spp_table) {

  group_lmax_est <- function(lmax_table, taxa_table, groupvar = "Species"){

    lmax_table %>%
      dplyr::left_join(taxa_table, by = "Species") %>%
      dplyr::group_by_at(groupvar)  %>%
      dplyr::summarise(Lmax = mean(Lmax, na.rm = T),
                .groups = "drop") %>%
      dplyr::rename(!!paste0(groupvar, "_Lmax") := Lmax)

  }

  lmax_table <- load_lmax()
  taxa_table <- load_taxalist()

  spp_table %>%
    dplyr::left_join(taxa_table) %>%
    dplyr::left_join(group_lmax_est(lmax_table, taxa_table, groupvar = "Species"), by = "Species") %>%
    dplyr::left_join(group_lmax_est(lmax_table, taxa_table, groupvar = "Genus"), by = "Genus") %>%
    dplyr::left_join(group_lmax_est(lmax_table, taxa_table, groupvar = "Order"), by = "Order") %>%
    dplyr::left_join(group_lmax_est(lmax_table, taxa_table, groupvar = "Class"), by = "Class") %>%
    dplyr::left_join(group_lmax_est(lmax_table, taxa_table, groupvar = "Phylum"), by = "Phylum") %>%
    dplyr::left_join(group_lmax_est(lmax_table, taxa_table, groupvar = "Kingdom"), by = "Kingdom") %>%
    dplyr::mutate(Lmax = dplyr::coalesce(Species_Lmax, Genus_Lmax, Order_Lmax, Class_Lmax, Phylum_Lmax, Kingdom_Lmax)) %>%
    dplyr::select(names(spp_table), Lmax)
}
