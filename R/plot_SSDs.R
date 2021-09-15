
#' Plot species size distributions
#'
#' This function plots species size distributions
#'
#' @param data Dataframe or tibble
#' @param log Logical arument for logged axes
#' @param logbase The chosen logbase for the plotting
#' @param max_x Numeric value determining the limit of the x-axis
#' @param max_y Numeric value determining the limit of the y-axis
#' @return A ggplot figure
#' @export
plot_SSDs <- function(data, log = F, logbase = NA, max_x = 100, max_y = 0.5){

  log_if <- function(x, log = F, logbase = 2) {
    if (log == TRUE) {
      log(x, base = logbase)
    } else {
      x
    }
  }

  max_x_logif <- ifelse(log, log(max_x, base = logbase), max_x)

  data %>%
    tidyr::drop_na({{ "meanlog" }}, {{ "sdlog" }}) %>%
    dplyr::mutate(m_ks = list(seq(1, max_x, by = 0.1))) %>%
    dplyr::mutate(p_m_ks = purrr::pmap(list(x = {{ "m_ks" }}, meanlog = {{ "meanlog" }}, sdlog = {{ "sdlog" }}), stats::dlnorm)) %>%
    tidyr::unnest(cols = c({{ "m_ks" }}, {{ "p_m_ks" }})) %>%
    dplyr::mutate(k = as.factor({{ "Species" }})) %>%
    dplyr::mutate_at(.vars = dplyr::vars({{ "m_ks" }}, {{ "p_m_ks" }}), .funs = log_if, log = log, logbase = logbase) %>%
    ggplot2::ggplot(ggplot2::aes(x = {{ "m_ks" }},
                                 y = {{ "p_m_ks" }},
                                 col = {{ "Species" }})) +
    ggplot2::geom_line() +
    ggplot2::ylab("Probability density") +
    ggplot2::xlab("Body mass") +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
                   strip.text.x = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = 14),
                   legend.text = ggplot2::element_text(size = 20),
                   legend.title = ggplot2::element_blank(),
                   panel.spacing = ggplot2::unit(0, "lines"),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_rect(color = "grey90"),
                   panel.background = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "black")) +
    ggplot2::xlim(NA, max_x_logif) +
    ggplot2::ylim(NA, max_y)

}


