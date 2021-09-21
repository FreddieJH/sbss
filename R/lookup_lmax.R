

#' Clean length-weight ests
#'
#' This function cleans length-weight estimates from sealifebase and fishbase.
#' If no appropriate length-weight estimate is available for a species, the mean
#' for a lower taxonomic level will be used.
#'
#' @param df dataframe or tibble, must contain a column of species names
#' @param spp_colname character, the name of the column containing the species names
#' @param full_load logical, if TRUE will pull values from Fishbase and Sealifebase
#'
#' @return A tibble of assymptotic length variables
#'
#' @export
#' @import rlang
#' @import dplyr
find_lmax <- function(df, spp_colname = "species_name", full_load = F) {

  if (spp_colname %in% names(df) == FALSE) stop("spp_colname must be the name of a column in the dataframe.")

  lmax_table <-
    if (full_load) {
      load_lmax()
    } else {
      sbss::lmax_tbl |> tidyr::drop_na(.data[["lmax"]])
    }

  taxa_table <-
    if (full_load) {
      load_taxalist()
    } else {
      sbss::taxa_tbl
    }

  group_lmax_est <- function(lmax_table, taxa_table, groupvar = "species_name"){

    lmax_table %>%
      left_join(taxa_table, by = "species_name") %>%
      group_by_at(groupvar)  %>%
      summarise(lmax = mean(.data[["lmax"]], na.rm = T),
                .groups = "drop") %>%
      rename(!!paste0(groupvar, "_lmax") := .data[["lmax"]])

  }

  df %>%
    mutate(species_name = .data[[spp_colname]]) |>
    left_join(taxa_table, by = "species_name") %>%
    left_join(group_lmax_est(lmax_table, taxa_table, groupvar = "species_name"), by = "species_name") %>%
    left_join(group_lmax_est(lmax_table, taxa_table, groupvar = "genus"),   by = "genus") %>%
    left_join(group_lmax_est(lmax_table, taxa_table, groupvar = "family"),   by = "family") %>%
    left_join(group_lmax_est(lmax_table, taxa_table, groupvar = "order"),   by = "order") %>%
    left_join(group_lmax_est(lmax_table, taxa_table, groupvar = "class"),   by = "class") %>%
    left_join(group_lmax_est(lmax_table, taxa_table, groupvar = "phylum"),  by = "phylum") %>%
    left_join(group_lmax_est(lmax_table, taxa_table, groupvar = "kingdom"), by = "kingdom") %>%
    mutate(lmax = coalesce(.data[["species_name_lmax"]],
                           .data[["genus_lmax"]],
                           .data[["family_lmax"]],
                           .data[["order_lmax"]],
                           .data[["class_lmax"]],
                           .data[["phylum_lmax"]],
                           .data[["kingdom_lmax"]])) %>%
    select(names(df), .data[["lmax"]])

}
