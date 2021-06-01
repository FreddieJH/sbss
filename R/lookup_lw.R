
#' Clean length-weight ests
#'
#' This function cleans length-weight estimates from sealifebase and fishbase.
#' If no appropriate length-weight estimate is available for a species, the mean
#' for a lower taxonomic level will be used
#'
#' @param spp_lmax_tab A tibble or dataframe with a column of species names ("species_name") and a column of lmax ("species_lmax")
#' @return A tibble of length-weight variables
#' @export
lookup_lw <- function(spp_table) {

    lw_table   <- load_lw()
    taxa_table <- load_taxalist()

    group_lw_est <- function(lw_table, taxa_table, groupvar = "Species"){

      lw_table %>%
        dplyr::left_join(taxa_table, by = "Species") %>%
        dplyr::group_by_at(groupvar)  %>%
        dplyr::summarise(a = mean(a, na.rm = T),
                         b = mean(b, na.rm = T),
                         .groups = "drop") %>%
        dplyr::rename(!!paste0(groupvar, "_a") := a,
                      !!paste0(groupvar, "_b") := b)

    }

    estimated_lw <-
      spp_table %>%
      dplyr::left_join(taxa_table) %>%
      dplyr::left_join(group_lw_est(lw_table, taxa_table, groupvar = "Species"), by = "Species") %>%
      dplyr::left_join(group_lw_est(lw_table, taxa_table, groupvar = "Genus"), by = "Genus") %>%
      dplyr::left_join(group_lw_est(lw_table, taxa_table, groupvar = "Order"), by = "Order") %>%
      dplyr::left_join(group_lw_est(lw_table, taxa_table, groupvar = "Class"), by = "Class") %>%
      dplyr::left_join(group_lw_est(lw_table, taxa_table, groupvar = "Phylum"), by = "Phylum") %>%
      dplyr::left_join(group_lw_est(lw_table, taxa_table, groupvar = "Kingdom"), by = "Kingdom") %>%
      dplyr::mutate(a = dplyr::coalesce(Species_a, Genus_a, Order_a, Class_a, Phylum_a, Kingdom_a),
                    b = dplyr::coalesce(Species_b, Genus_b, Order_b, Class_b, Phylum_b, Kingdom_b)) %>%
      dplyr::select(names(spp_table), a, b)

    spp_table %>%
      dplyr::left_join(estimated_lw) %>%
      return()

}
