theme_set(theme_void(base_size = 24, base_family = "Editorial New")) #Editorial New, Noto Serif
theme_update(legend.margin = margin(0, 0, 50, 0),
             legend.text = element_text(margin = margin(5, 15, 5, 0)),
             legend.position = "top",
             plot.title = element_text(hjust = .5, face = "bold", size = 40,
                                       lineheight = 1.1, margin = margin(t = 10, b = 25)),
             plot.subtitle = element_text(hjust = .5, color = "grey40", size = 15,
                                          margin = margin(t = -8, b = 18)),
             plot.title.position = "plot",
             plot.caption = element_text(hjust = .5, color = "grey30", 
                                         lineheight = 1.3,
                                         size = 18, margin = margin(20, 0, 5, 0)),
             plot.caption.position = "plot",
             plot.margin = margin(10, 0, 10, 0))

grid <- read_csv(here::here("data", "de_constituencies_grid.csv"))

set.seed(1234567890)
pal <- sample(ggsci::pal_simpsons()(16))

ggplot(grid, aes(col, row, color = bundesland_de, fill = bundesland_de)) +
  geom_tile(size = 3, color = "#212121") +
  geom_tile(size = .01) +
  coord_fixed() +
  scale_x_continuous(expand = c(.01, .01), limits = c(-.5, max(grid$col) + .5)) +
  scale_y_reverse(expand = c(.03, .03)) +
  scale_color_manual(values = pal, guide = "none") +
  scale_fill_manual(values = pal, guide = "none") + 
  theme(plot.margin = margin(15, 10, 15, 0)) -> g1


ggplot(grid, aes(col, row, color = bundesland_de, color = bundesland_de)) +
  geom_point(size = 11, shape = 15) +
  coord_fixed() +
  scale_x_continuous(expand = c(.01, .01), limits = c(-.5, max(grid$col) + .5)) +
  scale_y_reverse(expand = c(.03, .03)) +
  scale_color_manual(values = pal, name = NULL) +
  guides(color = guide_legend(nrow = 2)) +
  theme(plot.margin = margin(15, 10, 15, 0)) -> g2

ggplot(grid, aes(col, row, color = bundesland_de, color = bundesland_de)) +
  geom_point(
    aes(fill = bundesland_de), 
    size = 9, shape = 21, stroke = 2, alpha = .5, color = "transparent"
  ) +
  geom_point(
    aes(color = bundesland_de), 
    size = 9, shape = 21, stroke = 2, fill = "transparent"
  ) +
  coord_fixed() +
  scale_x_continuous(expand = c(.01, .01), limits = c(-.5, max(grid$col) + .5)) +
  scale_y_reverse(expand = c(.03, .03)) +
  scale_color_manual(values = pal, guide = "none") +
  scale_fill_manual(values = pal, guide = "none") +
  theme(plot.margin = margin(15, 10, 15, 0)) -> g3

g1 + g2 + g3 + plot_layout(nrow = 1, guides = "collect") + plot_annotation(title = "Tile Grid Map of the 299 Constituencies of Germany", caption = "Design: Cédric Scherer & Ansgar Wolsing")

ggsave(here::here("plots", "grid_laender_variants.pdf"), width = 25, height = 15.5, device = cairo_pdf)

pdf_convert(pdf = here::here("plots", "grid_laender_variants.pdf"), 
            filenames = here::here("plots", "grid_laender_variants.png"),
            format = "png", dpi = 500)