#' Plot community size spectrum
#'
#' This function plots an abundance size spectrum, with the option for
#' normalised or non-normalised axes
#'
#' @param data Table of mass, normalised density, and density
#' @param normalised Logical option of normalised (TRUE) or non-normalised (FALSE)
#' @param logbase The chosen logbase for the plotting
#' @return A ggplot figure
#' @export
plot_css <- function(data, normalised = T, logbase = 2){

  data |>
    rowwise() |>
    dplyr::mutate(y = ifelse(normalised, .data[["norm_density"]], .data[["density"]])) %>%
    ggplot2::ggplot() +
    ggplot2::aes(log(.data[["m"]], base = logbase), log(.data[["y"]], base = logbase)) +
    ggplot2::geom_point() +
    {if (!normalised) ggplot2::ylab("log(abundance)")} +
    {if (normalised) ggplot2::ylab("log(normalised abundance)")} +
    ggplot2::xlab("log(mass)") +
    ggplot2::theme_bw(24) +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
                   strip.text.x = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = 14),
                   axis.title = ggplot2::element_text(size = 20),
                   legend.position = "bottom",
                   legend.text = ggplot2::element_text(size = 20),
                   legend.title = ggplot2::element_blank(),
                   panel.spacing = ggplot2::unit(0, "lines"),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_rect(color = "grey90"),
                   panel.background = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "black"))

}

