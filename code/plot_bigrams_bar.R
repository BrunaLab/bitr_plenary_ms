barplot_words <- function() {
  library(tidyverse)
  library(here)
  library(cowplot)


  system_list <- read_csv(here("data", "data_ms", "system_list.csv"), col_names = TRUE) %>%
    filter(geo == TRUE)

  system_bigram <- system_list %>%
    rename(bigram = system)



  rankings_pub <- read_csv(here("data", "data_ms", "ranked_bigrams.csv"))
  

  rankings_pub <- rankings_pub %>%
    mutate(system = as.factor(system)) %>%
    group_by(cat) %>%
    mutate(rank_perc = rank(desc(perc), ties.method = "random"))


  trop_term <- rankings_pub %>%
    filter(cat == "tropical") %>%
    select(bigram, cat, rank_perc, perc, system) %>%
    mutate(original = bigram) %>%
    mutate(bigram = paste(bigram, " - ", rank_perc, " ", sep = "")) %>%
    mutate(bigram = fct_reorder(bigram, perc)) %>%
    filter(rank_perc <= cutoff)

  nontrop_term <- rankings_pub %>%
    filter(cat == "non-tropical") %>%
    select(bigram, cat, rank_perc, perc, system) %>%
    mutate(original = bigram) %>%
    mutate(bigram = paste(bigram, " - ", rank_perc, " ", sep = "")) %>%
    mutate(bigram = fct_reorder(bigram, perc)) %>%
    filter(rank_perc <= cutoff)

  # three keyword list - trop, nontrop, overlap -----------------------------
  


  axis_max <- max(
    (max(nontrop_term$perc)),
    (max(trop_term$perc))
  ) *
    1.06


  GeomLabel$default_aes$size

  update_geom_defaults("text", list(size = 2))
  # https://stackoverflow.com/questions/25061822/ggplot-geom-text-font-size-control

  breaks_vec <- seq(0, axis_max, by = .5)
  min_x <- -1.3
  max_x <- axis_max



  kw_trop_bar <- trop_term %>% # This trick update the factor levels
    ggplot(aes(x = bigram, y = perc, fill = factor(system))) +
    geom_bar(stat = "identity", color = "black", size = 0.2) +
    scale_fill_manual(values = c("white", "slateblue4")) +
    # scale_y_continuous(limits = c(min_x, 6), breaks = seq(0, 6, by = .5))+
    scale_y_continuous(limits = c(min_x, max_x), breaks = breaks_vec) +
    ylab("Articles with Keyword (%)") +
    # ggtitle("Tropics")+
    # ylim(0, 4)+
    # scale_fill_manual(values = c("white","navy"))+
    coord_flip() +
    geom_text(
      data = trop_term, aes(
        x = bigram, y = -.12,
        label = bigram,
        color = factor(system),
        # fontface = "bold"),
        fontface = ifelse(system == "Y", "bold", "plain"),
      ),
      hjust = "right", vjust = 0, nudge_x = 0, nudge_y = 0.08
    ) +
    scale_color_manual(values = c("black", "midnightblue")) +
    # scale_y_discrete(expand = c(0, 2),limits = c(0, 4)) +
    theme_classic() +
    theme(
      panel.border = element_blank(), panel.grid.major = element_blank(),
      axis.line.y = element_line(color = "black", size = 0.0, lineend = "square"),
      axis.line.x = element_line(color = "black", size = 0.5, lineend = "square"),
      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), # sets colors of axes
      plot.title = element_text(hjust = 0.8, vjust = -103.5, face = "bold", size = 22, color = "navy"), # Sets title size, style, location
      axis.title.x = element_text(colour = "black", size = 10, vjust = -3, hjust = 0.6), # sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
      axis.title.y = element_blank(),
      # axis.title.y = element_text(colour = "black", size = 10, angle = 0, vjust = 0.5), # sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
      legend.position = "none",
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(colour = "black", size = 8),
      axis.text.y = element_blank(),
      # axis.text.y = element_text(colour = "black", size = 10), # sets size and style of labels on axes
      plot.margin = unit(c(1, 0.4, .5, .4), "cm")
    )

  kw_trop_bar



  kw_notrop_bar <- nontrop_term %>% # This trick update the factor levels
    ggplot(aes(x = bigram, y = perc, fill = factor(system))) +
    geom_bar(stat = "identity", color = "black", size = 0.2) +
    scale_fill_manual(values = c("white", "slateblue4")) +
    # scale_y_continuous(limits = c(min_x, 6), breaks = seq(0, 6, by = .5))+
    scale_y_continuous(limits = c(min_x, max_x), breaks = breaks_vec) +
    # scale_y_continuous(limits = c(min_x, axis_max), n.breaks=10,minor_breaks=2)+
    ylab("Articles with Keyword (%)") +
    # ggtitle("Non-tropical")+
    # ylim(0, 4)+
    # scale_fill_manual(values = c("white","navy"))+
    coord_flip() +
    geom_text(
      data = nontrop_term, aes(
        x = bigram, y = -.12,
        label = bigram,
        color = factor(system),
        # size= 6,
        # fontface = "bold"),
        fontface = ifelse(system == "Y", "bold", "plain"),
      ),
      hjust = "right", vjust = 0, nudge_x = 0, nudge_y = 0.08
    ) +
    scale_color_manual(values = c("black", "midnightblue")) +
    # scale_y_discrete(expand = c(0, 2),limits = c(0, 4)) +
    theme_classic() +
    theme(
      panel.border = element_blank(), panel.grid.major = element_blank(),
      axis.line.y = element_line(color = "black", size = 0.0, lineend = "square"),
      axis.line.x = element_line(color = "black", size = 0.5, lineend = "square"),
      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), # sets colors of axes
      plot.title = element_text(hjust = 0.8, vjust = -103.5, face = "bold", size = 22, color = "navy"), # Sets title size, style, location
      axis.title.x = element_text(colour = "black", size = 10, vjust = -3, hjust = 0.6), # sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
      axis.title.y = element_blank(),
      # axis.title.y = element_text(colour = "black", size = 10, angle = 0, vjust = 0.5), # sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
      legend.position = "none",
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(colour = "black", size = 8),
      axis.text.y = element_blank(),
      # axis.text.y = element_text(colour = "black", size = 10), # sets size and style of labels on axes
      plot.margin = unit(c(1, 0.4, .5, .4), "cm")
    )

  kw_notrop_bar

  terms_fig <- plot_grid(kw_trop_bar, kw_notrop_bar,
    nrow = 1,
    # labels = "AUTO",
    labels = (c("a. 'Tropical' Articles", "b. 'Non-tropical' Articles")),
    label_size = 10,
    align = "v"
  )

  terms_fig


  # for KW
  ggsave("bigram_fig.jpeg",
    path = "./figures",
    dpi = 700,
    width = 10,
    height = 7,
    units = c("in")
  )
}
