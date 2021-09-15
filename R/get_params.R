#' Obtain length-weight parameters from Fishbase and Sealifebase
#'
#' This function obtains length-weight parameters from fishbase and
#' sealifebase databases, utilising the rfishbase package.
#'
#' @return A tibble of length-weight parameters
#' @export
#' @import rlang
#' @import dplyr
#' @importFrom rfishbase length_weight
#' @import tibble
load_lw <- function(){


  slb <-
    length_weight(server = "sealifebase") %>%
    as_tibble() %>%
    select(.data[["Species"]],
                  .data[["a"]],
                  .data[["b"]])


  fb <-
    length_weight(server = "fishbase") %>%
    as_tibble() %>%
    select(.data[["Species"]],
                  .data[["a"]],
                  .data[["b"]])

  bind_rows(slb, fb) %>%
    as_tibble()

}


#' Load Sealifebase and Fishbase Taxa
#'
#' This function loads a tibble of Sealifebase and Fishbase taxa
#'
#' @return A tibble of taxa
#' @export
#' @import rlang
#' @import dplyr
#' @importFrom rfishbase load_taxa
load_taxalist <- function(){

  slb <-
    load_taxa(server = "sealifebase") |>
    as_tibble() |>
    select(.data[["Species"]],
                  .data[["Genus"]],
                  .data[["Family"]],
                  .data[["Order"]],
                  .data[["Class"]],
                  .data[["Phylum"]],
                  .data[["Kingdom"]])

  fb <-
    load_taxa(server = "fishbase") |>
    as_tibble() |>
    mutate(Phylum = "Chordata",
                  Kingdom = "Animalia") |>
    select(.data[["Species"]],
                  .data[["Genus"]],
                  .data[["Family"]],
                  .data[["Order"]],
                  .data[["Class"]],
                  .data[["Phylum"]],
                  .data[["Kingdom"]])

  bind_rows(slb, fb)

}

#' Load a Matrix
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @return A matrix of the infile
#' @export
#' @import rlang
#' @import dplyr
#' @importFrom rfishbase popchar
load_lmax <- function(){

  slb <-
    popchar(server = "sealifebase") |>
    mutate(Lmax = as.numeric(Lmax)) |>
    select(.data[["Species"]], .data[["Lmax"]])

  fb <-
    popchar(server = "fishbase") |>
    mutate(Lmax = as.numeric(.data[["Lmax"]])) |>
    select(.data[["Species"]], .data[["Lmax"]])

  bind_rows(slb, fb)

}


