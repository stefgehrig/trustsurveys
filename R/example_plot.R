library(tidyverse)
library(scales)
df <- read_csv("output/df_trust.csv")

p <- df %>% 
  filter(country.name == "Brazil" & survey != "GPS") %>% 
  mutate(plot_year = case_when(
    survey == "WVS" ~ floor((as.numeric(substr(years, 1, 4)) + as.numeric(substr(years, 6, 9)))) / 2,
    TRUE ~ as.numeric(substr(years, 1, 4))),
    label = ifelse(survey == "WVS", paste0(survey, " ", wave, "\n(", years, ")"), "")) %>% 
  ggplot(aes(x = plot_year, y = trust, col = survey)) + 
  geom_point(size = 2) + 
  geom_smooth(show.legend = FALSE, se = FALSE, method = "loess", formula = y ~ x, span = 1) + 
  geom_text(aes(label = label), vjust = -1/2, show.legend = FALSE, size = 3) + 
  scale_y_continuous(limits = c(0,0.125), breaks = seq(0,0.125,0.025), labels = percent) + 
  scale_x_continuous(limits = c(1990, 2020)) + 
  theme_classic(14) + 
  theme(legend.position = "bottom") + 
  labs(x = "Year", 
       y = expression(paste("Pr(",italic("'Most people can be trusted'"), ")")),
       col = "",
       subtitle = "Brazil") + 
  scale_color_manual(values = c("#66c2a5", "#8da0cb"))

ggsave("output/example_plot.png", p, width = 2400, height = 1500, dpi = 330, units = "px")