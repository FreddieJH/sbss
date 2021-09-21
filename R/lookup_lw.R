
#' Clean length-weight ests
#'
#' This function cleans length-weight estimates from sealifebase and fishbase.
#' If no appropriate length-weight estimate is available for a species, the mean
#' for a lower taxonomic level will be used
#'
#' @param df tibble or dataframe, must contain a column of species names
#' @param spp_colname character, the name of the column containing the species names
#' @param full_load logical, if TRUE will load from Fishbase and Sealifebase
#'
#' @return tibble of length-weight variables
#' @export
#' @import rlang
#' @import dplyr
find_lw <- function(df, spp_colname = "species_name", full_load = T) {

  if (spp_colname %in% names(df) == FALSE) stop("spp_colname must be the name of a column in the dataframe.")

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

  group_lw_est <- function(lw_table, taxa_table, groupvar = "species_name"){

    lw_table %>%
      left_join(taxa_table, by = "species_name") %>%
      group_by_at(groupvar)  %>%
      summarise(a = mean(.data[["a"]], na.rm = T),
                b = mean(.data[["b"]], na.rm = T),
                .groups = "drop") %>%
      rename(!!paste0(groupvar, "_a") := .data[["a"]],
             !!paste0(groupvar, "_b") := .data[["b"]])

  }


  df %>%
    mutate(species_name = .data[[spp_colname]]) |>
    left_join(taxa_table, by = "species_name") %>%
    left_join(group_lw_est(lw_table, taxa_table, groupvar = "species_name"), by = "species_name") %>%
    left_join(group_lw_est(lw_table, taxa_table, groupvar = "genus"), by = "genus") %>%
    left_join(group_lw_est(lw_table, taxa_table, groupvar = "order"), by = "order") %>%
    left_join(group_lw_est(lw_table, taxa_table, groupvar = "class"), by = "class") %>%
    left_join(group_lw_est(lw_table, taxa_table, groupvar = "phylum"), by = "phylum") %>%
    left_join(group_lw_est(lw_table, taxa_table, groupvar = "kingdom"), by = "kingdom") %>%
    mutate(a = coalesce(.data[["species_name_a"]],
                        .data[["genus_a"]],
                        .data[["order_a"]],
                        .data[["class_a"]],
                        .data[["phylum_a"]],
                        .data[["kingdom_a"]]),
           b = coalesce(.data[["species_name_b"]],
                        .data[["genus_b"]],
                        .data[["order_b"]],
                        .data[["class_b"]],
                        .data[["phylum_b"]],
                        .data[["kingdom_b"]])) %>%
    select(names(df), .data[["a"]], .data[["b"]])

}
