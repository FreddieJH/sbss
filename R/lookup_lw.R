
#' Clean length-weight ests
#'
#' This function cleans length-weight estimates from sealifebase and fishbase.
#' If no appropriate length-weight estimate is available for a species, the mean
#' for a lower taxonomic level will be used
#'
#' @param spp_table tibble or dataframe with a column of species names (species_name) and a column of lmax (species_lmax)
#' @param full_load logical, if TRUE will load from Fishbase and Sealifebase
#'
#' @return tibble of length-weight variables
#' @export
#' @import rlang
#' @import dplyr
lookup_lw <- function(spp_table, full_load = T) {

  lw_table <-
    if (full_load) {
      load_lw()
    } else {
      sbss::lw_tbl |> dplyr::filter(across(c(.data[["a"]], .data[["b"]]),
                                           .fns = Negate(is.na)))
    }

  taxa_table <-
    if (full_load) {
      load_taxalist()
    } else {
      sbss::taxa_tbl
    }

    group_lw_est <- function(lw_table, taxa_table, groupvar = "Species"){

      lw_table %>%
        left_join(taxa_table, by = "Species") %>%
        group_by_at(groupvar)  %>%
        summarise(a = mean(.data[["a"]], na.rm = T),
                         b = mean(.data[["b"]], na.rm = T),
                         .groups = "drop") %>%
        rename(!!paste0(groupvar, "_a") := .data[["a"]],
                      !!paste0(groupvar, "_b") := .data[["b"]])

    }

    estimated_lw <-
      spp_table %>%
      left_join(taxa_table, by = "Species") %>%
      left_join(group_lw_est(lw_table, taxa_table, groupvar = "Species"), by = "Species") %>%
      left_join(group_lw_est(lw_table, taxa_table, groupvar = "Genus"), by = "Genus") %>%
      left_join(group_lw_est(lw_table, taxa_table, groupvar = "Order"), by = "Order") %>%
      left_join(group_lw_est(lw_table, taxa_table, groupvar = "Class"), by = "Class") %>%
      left_join(group_lw_est(lw_table, taxa_table, groupvar = "Phylum"), by = "Phylum") %>%
      left_join(group_lw_est(lw_table, taxa_table, groupvar = "Kingdom"), by = "Kingdom") %>%
      mutate(a = coalesce(.data[["Species_a"]],
                          .data[["Genus_a"]],
                          .data[["Order_a"]],
                          .data[["Class_a"]],
                          .data[["Phylum_a"]],
                          .data[["Kingdom_a"]]),
             b = coalesce(.data[["Species_b"]],
                          .data[["Genus_b"]],
                          .data[["Order_b"]],
                          .data[["Class_b"]],
                          .data[["Phylum_b"]],
                          .data[["Kingdom_b"]])) %>%
      select(names(spp_table), .data[["a"]], .data[["b"]])

    spp_table %>%
      left_join(estimated_lw)

}
