

#' Clean length-weight ests
#'
#' This function cleans length-weight estimates from sealifebase and fishbase.
#' If no appropriate length-weight estimate is available for a species, the mean
#' for a lower taxonomic level will be used
#'
#' @param spp_table A tibble or dataframe with a column of species names ("species")
#' @param full_load logical, if TRUE will pull values from Fishbase and Sealifebase.
#'
#' @return A tibble of length-weight variables
#'
#' @export
#' @import rlang
#' @import dplyr
lookup_lmax <- function(spp_table, full_load = F) {

  group_lmax_est <- function(lmax_table, taxa_table, groupvar = "Species"){

    lmax_table %>%
      left_join(taxa_table, by = "Species") %>%
      group_by_at(groupvar)  %>%
      summarise(Lmax = mean(.data[["Lmax"]], na.rm = T),
                       .groups = "drop") %>%
      rename(!!paste0(groupvar, "_Lmax") := .data[["Lmax"]])

  }

  lmax_table <-
    if (full_load) {
      load_lmax()
    } else {
      sbss::lmax_tbl |> tidyr::drop_na(.data[["Lmax"]])
    }

  taxa_table <-
    if (full_load) {
      load_taxalist()
    } else {
      sbss::taxa_tbl
    }

  spp_table %>%
    left_join(taxa_table, by = "Species") %>%
    left_join(group_lmax_est(lmax_table, taxa_table, groupvar = "Species"), by = "Species") %>%
    left_join(group_lmax_est(lmax_table, taxa_table, groupvar = "Genus"),   by = "Genus") %>%
    left_join(group_lmax_est(lmax_table, taxa_table, groupvar = "Order"),   by = "Order") %>%
    left_join(group_lmax_est(lmax_table, taxa_table, groupvar = "Class"),   by = "Class") %>%
    left_join(group_lmax_est(lmax_table, taxa_table, groupvar = "Phylum"),  by = "Phylum") %>%
    left_join(group_lmax_est(lmax_table, taxa_table, groupvar = "Kingdom"), by = "Kingdom") %>%
    mutate(Lmax = coalesce(.data[["Species_Lmax"]],
                                         .data[["Genus_Lmax"]],
                                         .data[["Order_Lmax"]],
                                         .data[["Class_Lmax"]],
                                         .data[["Phylum_Lmax"]],
                                         .data[["Kingdom_Lmax"]])) %>%
    select(names(spp_table), .data[["Lmax"]])

}
