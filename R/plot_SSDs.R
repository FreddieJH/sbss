
#' Plot species size distributions
#'
#' This function plots species size distributions
#'
#' @param data dataframe or tibble, containing the columns `meanlog`, and `sdlog`.
#' @param log logical, if true axes will be logged
#' @param logbase double, log base for the plotting
#' @param max_x integer, determining the limit of the x-axis
#' @param max_y integer, value determining the limit of the y-axis
#' @param legend logical, if TRUE legend of species names will be included
#'
#' @return A ggplot figure
#' @export
#'
#' @import ggplot2
#' @import tidyr
#' @import dplyr
plot_ssd <- function(data, log = F, logbase = NA, max_x = 100, max_y = 0.5, legend = T){

  log_if <- function(x, log = F, logbase = 2) {
    if (log == TRUE) {
      log(x, base = logbase)
    } else {
      x
    }
  }

  max_x_logif <- ifelse(log, log(max_x, base = logbase), max_x)

  data %>%
    drop_na(.data[["meanlog"]], .data[["sdlog"]]) %>%
    mutate(m_ks = list(seq(1, max_x, by = 0.1))) %>%
    mutate(p_m_ks = purrr::pmap(list(x = .data[["m_ks"]], meanlog = .data[["meanlog"]], sdlog = .data[["sdlog"]]), stats::dlnorm)) %>%
    unnest(cols = c(.data[["m_ks"]], .data[["p_m_ks"]])) %>%
    mutate(k = as.factor(.data[["species_name"]])) %>%
    mutate_at(.vars = vars(.data[["m_ks"]], .data[["p_m_ks"]]), .funs = log_if, log = log, logbase = logbase) %>%
    ggplot(aes(x = .data[["m_ks"]],
                                 y = .data[["p_m_ks"]],
                                 col = .data[["species_name"]])) +
    geom_line(size = 1) +
    ylab("Probability density") +
    xlab("Body mass") +
    theme(strip.background = element_blank(),
                   strip.text.x = element_blank(),
                   axis.text = element_text(size = 14),
                   axis.title = element_text(size = 20),
                   legend.text = element_text(size = 20),
                   legend.title = element_blank(),
                   panel.spacing = unit(0, "lines"),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_rect(color = "grey90", fill = "transparent"),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   legend.key = element_blank()) +
    {if (legend == F) theme(legend.position = "none")} +
    xlim(NA, max_x_logif) +
    ylim(NA, max_y)

}


