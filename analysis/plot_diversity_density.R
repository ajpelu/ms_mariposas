library(tidyverse)
library(patchwork)
library(readxl)
library(here)

# Read data
elev <- read_excel(here::here("rawdata/Tabla_variables_modelizacion_lepidopteros_ACTUALIZADO_2019.xls"), 1) %>%
  rename(code = Abreviatura) %>%
  dplyr::select(nombre = "Transecto", elev = "elevation") %>%
  filter(!nombre %in% c("SNEVADA", "RÁSTER"))

denraw <- read_csv(here::here("data/densidad_by_year.csv")) %>% inner_join(elev)
divraw <- read_csv(here::here("data/diversidad_by_year.csv")) %>% inner_join(elev)


# Filter year
yselected <- c(2014, 2015, 2016, 2017, 2019)

# Compute stats for dendidad
densidad_avg <- denraw %>%
  filter(year %in% yselected) %>%
  group_by(nombre, code, elev) %>%
  summarise(mean = mean(den, na.rm = TRUE),
            sd = sd(den, na.rm = TRUE),
            se = sd/sqrt(n()))

plot_density <- densidad_avg %>%
  ggplot(aes(x=elev, y = mean, label=code)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), colour="gray", alpha=.3) +
  # geom_smooth(method="loess", span=.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se)) +
  geom_point(size=2, shape=21, fill="white") +
  theme_bw() +
  theme(
    panel.grid = element_blank()
  ) +
  ylab("Density (n ind /100 m)") +
  xlab("Elevation") +
  geom_text_repel()

plot_density

diversidad_avg <- divraw %>%
  filter(year %in% yselected) %>%
  group_by(nombre, code, elev) %>%
  summarise(mean = mean(diversidad, na.rm = TRUE),
            sd = sd(diversidad, na.rm = TRUE),
            se = sd/sqrt(n()))

plot_diversity <- diversidad_avg %>%
  ggplot(aes(x=elev, y = mean, label = code)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), colour="gray", alpha=.3) +
  # geom_smooth() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se)) +
  geom_point(size=2, shape=21, fill="white") +
  theme_bw() +
  theme(
    panel.grid = element_blank()
  ) +
  ylab("Diversity (Shannon index)") +
  xlab("Elevation") +
  geom_text_repel()

plot_diversity


pdd <- plot_density / plot_diversity

ggsave(here::here("figs/density_diversity_seleccion.pdf"),
       device = "pdf",
       width = 4.5, height = 6)
pdd
dev.off()


ggsave(plot= pdd,
       here::here("figs/density_diversity_seleccion.png"),
       device = "png",
       dpi = 300,
       width = 4.5, height = 6)


