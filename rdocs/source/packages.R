if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse, data.table,
  readxl, readr, ggcorrplot, cowplot,
  RColorBrewer, scales, nortest, xlsx
)
windowsFonts(Arial=windowsFont("sans"))

options(scipen=999)

# Definindo paleta de cores da Estat
cores_estat <- c(
  "#CA1D1F", "#F55D1C", "#FDC500", "#F55751", "#086C75", "#17B2A7", "#69B2A7", "#AB324A",
  "black", "black", "black", "black", "black", 
  "black", "black", "black", "pink", "black",
  "black", "black", "black", "black", "black", 
  "black", "black", "black", "black", "black", 
  "black", "black", "black", "black", "black", 
  "black", "black", "black", "black", "black", 
  "black", "black", "black", "black", "black", 
  "black", "black", "black", "black", "black", 
  "black", "black", "black", "black", "black")

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 10),
      axis.title.x = ggplot2::element_text(colour = "black", size = 10),
      axis.text = ggplot2::element_text(colour = "black", size = 10),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      text = element_text(family = "sans", size = 12),
      legend.position = "top",
      ...
    )
  return(
    list(
      theme,
      scale_fill_manual(values = cores_estat),
      scale_colour_manual(values = cores_estat),
      scale_y_continuous(
        labels = scales::number_format(decimal.mark = ',',
                                       #                                       accuracy = 0.01,
                                       big.mark = "."))
    )
  )
}

# Definindo função que retorna frequências relativas de um vetor
percent <- function(absolute, digits = 2) {
  return(round(100 * absolute / sum(absolute), digits))
}

# Definindo função que retorna banco de dados com frequências
# relativas e absolutas de uma variável categórica
vector_frequencies <- function(vector) {
  frequency <- vector %>%
    table() %>%
    as_tibble() %>%
    mutate(
      rel = n %>%
        percent() %>%
        paste("%", sep = "")
    )
  colnames(frequency) <- c("groups", "absolute", "relative")
  return(frequency)
}