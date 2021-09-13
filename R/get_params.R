#' Obtain length-weight parameters from Fishbase and Sealifebase
#'
#' This function obtains length-weight parameters from fishbase and
#' sealifebase databases, utilising the rfishbase package.
#'
#' @return A tibble of length-weight parameters
#' @export
load_lw <- function(){
  dplyr::bind_rows(
    rfishbase::length_weight(server = "sealifebase") %>%
      tibble::as_tibble() %>%
      dplyr::select(Species, a, b),
    rfishbase::length_weight(server = "fishbase") %>%
      tibble::as_tibble() %>%
      dplyr::select(Species, a, b)
  ) %>%
    tibble::as_tibble()
}


#' Load Sealifebase and Fishbase Taxa
#'
#' This function loads a tibble of Sealifebase and Fishbase taxa
#'
#' @return A tibble of taxa
#' @export
load_taxalist <- function(){
  dplyr::bind_rows(
    rfishbase::load_taxa(server = "sealifebase") %>%
      tibble::as_tibble() %>%
      dplyr::select(Species, Genus, Family, Order, Class, Phylum, Kingdom),
    rfishbase::load_taxa(server = "fishbase") %>%
      tibble::as_tibble() %>%
      dplyr::mutate(Phylum = "Chordata",
                    Kingdom = "Animalia") %>%
      dplyr::select(Species, Genus, Family, Order, Class, Phylum, Kingdom)
  )

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
load_lmax <- function(){

  slb <-
    rfishbase::popchar(server = "sealifebase") |>
    dplyr::mutate(Lmax = as.numeric(Lmax)) |>
    dplyr::select(species = Species,
           lmax = Lmax)

  fb <-
    rfishbase::popchar(server = "fishbase") |>
    dplyr::mutate(Lmax = as.numeric(Lmax)) |>
    dplyr::select(species = Species,
           lmax = Lmax)

  dplyr::bind_rows(slb, fb)

}
