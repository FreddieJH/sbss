
#' Plot species size distributions
#'
#' This function plots species size distributions
#'
#' @param data Dataframe or tibble
#' @param log Logical argument for logged axes
#' @param logbase The chosen log base for the plotting
#' @param max_x Numeric value determining the limit of the x-axis
#' @param max_y Numeric value determining the limit of the y-axis
#' @return A ggplot figure
#' @export
plot_SSDs <- function(data, log = F, logbase = NA, max_x = 100, max_y = 0.5, legend = T){

  log_if <- function(x, log = F, logbase = 2) {
    if (log == TRUE) {
      log(x, base = logbase)
    } else {
      x
    }
  }

  max_x_logif <- ifelse(log, log(max_x, base = logbase), max_x)

  data %>%
    tidyr::drop_na(.data[["meanlog"]], .data[["sdlog"]]) %>%
    dplyr::mutate(m_ks = list(seq(1, max_x, by = 0.1))) %>%
    dplyr::mutate(p_m_ks = purrr::pmap(list(x = .data[["m_ks"]], meanlog = .data[["meanlog"]], sdlog = .data[["sdlog"]]), stats::dlnorm)) %>%
    tidyr::unnest(cols = c(.data[["m_ks"]], .data[["p_m_ks"]])) %>%
    dplyr::mutate(k = as.factor(.data[["Species"]])) %>%
    dplyr::mutate_at(.vars = dplyr::vars(.data[["m_ks"]], .data[["p_m_ks"]]), .funs = log_if, log = log, logbase = logbase) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[["m_ks"]],
                                 y = .data[["p_m_ks"]],
                                 col = .data[["Species"]])) +
    ggplot2::geom_line(size = 1.1) +
    ggplot2::ylab("Probability density") +
    ggplot2::xlab("Body mass") +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
                   strip.text.x = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = 14),
                   axis.title = ggplot2::element_text(size = 20),
                   legend.text = ggplot2::element_text(size = 20),
                   legend.title = ggplot2::element_blank(),
                   panel.spacing = ggplot2::unit(0, "lines"),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_rect(color = "grey90", fill = "transparent"),
                   panel.background = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "black"),
                   legend.key = ggplot2::element_blank()) +
    {if (legend == F) ggplot2::theme(legend.position = "none")} +
    ggplot2::xlim(NA, max_x_logif) +
    ggplot2::ylim(NA, max_y)

}


