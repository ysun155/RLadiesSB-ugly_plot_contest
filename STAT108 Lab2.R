library(tidyverse)
data <- read_csv("data/use_this_data/holiday_sweaters-2020-12-15-clean.csv") %>%
  filter(hs_tf == "Yes") %>% 
  # separate colors column into rows
  separate_rows(colors, sep = c(", ")) %>% 
  group_by(sweater) %>% 
  mutate(sum = length(colors)) %>% 
  rownames_to_column("values")

ggplot(data = data, 
       mapping = aes(x = colors, y = sum, color = factor(lights))) + 
  geom_point(size = 20, alpha = 1/10, aes(color = factor(lights))) + 
  theme(
    plot.background = element_rect(fill = "red"),
    panel.background = element_rect(fill = 'lightblue', colour = 'green', size = 4),
    axis.title.x = element_text(face = "bold.italic", color = "blue"),
    axis.title.y = element_text(family = "mono", face = "bold", color = "blue"),
    legend.background = element_rect(fill = "blue")
  ) +
  labs(x = "colors",
       y = "total number of colors in the sweater")

ggsave(here::here("yijia's plot.pdf"), width = 7, height = 4, dpi = 150)
