
data("quakes")
view(quakes)
install.packages("ggimage")

library(ggplot2)
library(ggimage)

ggplot(quakes, aes(x = long, y = lat)) +
  geom_point(aes(size = mag, colour = depth),
             shape = 21,
             fill = "firebrick1",
             alpha = 0.8,
             stroke = 2) +
  scale_size_continuous(range = c(5, 20)) +
  scale_color_gradient(low = "chocolate", high = "green") +
  labs(
    title = "SMH",
    x = "Sideways",
    y = "Up & Down",
    size = "Shaky Strength",
    color = "Shaky Depth"
  ) +
  theme(
    panel.background = element_rect(fill = "darkorchid1"),
    plot.background = element_rect(fill = "deeppink"),
    panel.grid.major = element_line(color = "cyan", linewidth = 2),
    panel.grid.minor = element_line(color = "red",linewidth = 1),
    plot.title = element_text(
      size = 24, face = "bold", color = "blue"
    ),
    axis.text = element_text(
      size = 14, color = "purple"
    ),
    axis.title = element_text(
      size = 16, face = "bold", color = "gold"
    ),
    legend.background = element_rect(fill = "yellow"),
    legend.position = "right"
  )


ggplot(quakes, aes(x = long, y = lat)) +
  geom_image(
    aes(size = mag),
    image = "~/desktop/Data_Course_MASON/images/quake.png"
  ) +
  scale_size_continuous(range = c(0.07, 0.15)) +
  labs(
    title = "Poor Fiji",
    x = "East/West",
    y = "North/South",
    size = "Strength"
  ) +
  theme(
    panel.background = element_rect(fill = "darkorchid1"),
    plot.background = element_rect(fill = "deeppink"),
    panel.grid.major = element_line(color = "red", linewidth = 2),
    panel.grid.minor = element_line(color = "red",linewidth = 1),
    plot.title = element_text(
      size = 24, face = "bold", color = "blue"
    ),
    axis.text = element_text(
      size = 14, color = "purple"
    ),
    axis.title = element_text(
      size = 16, face = "bold", color = "gold"
    ),
    legend.background = element_rect(fill = "yellow"),
    legend.position = "right"
  )