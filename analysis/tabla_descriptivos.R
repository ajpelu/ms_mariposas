# Tabla

library(tidyverse)
library(here)

# Read data
# Filter year
yselected <- c(2014, 2015, 2016, 2017, 2019)


elev <- read_excel(here::here("rawdata/Tabla_variables_modelizacion_lepidopteros_ACTUALIZADO_2019.xls"), 1) %>%
  rename(code = Abreviatura) %>%
  dplyr::select(nombre = "Transecto", elev = "elevation", length_transect = "Longitud transecto") %>%
  filter(!nombre %in% c("SNEVADA", "RÁSTER"))

denraw <- read_csv(here::here("data/densidad_by_year.csv")) %>% inner_join(elev)
divraw <- read_csv(here::here("data/diversidad_by_year.csv")) %>% inner_join(elev)
riqraw <- read_csv(here::here("data/riqueza_by_year.csv"))
richness <- read_csv(here::here("data/richness.csv"))


densidad_avg <- denraw %>%
  filter(year %in% yselected) %>%
  group_by(nombre, code, elev) %>%
  summarise(mean = mean(den, na.rm = TRUE),
            sd = sd(den, na.rm = TRUE),
            se = sd/sqrt(n()))

diversidad_avg <- divraw %>%
  filter(year %in% yselected) %>%
  group_by(nombre, code, elev) %>%
  summarise(mean = mean(diversidad, na.rm = TRUE),
            sd = sd(diversidad, na.rm = TRUE),
            se = sd/sqrt(n()))

abundancia <- denraw %>%
  filter(year %in% yselected) %>%
  group_by(nombre) %>%
  summarise(mean = round(mean(ntotal),2),
            sd = sd(ntotal),
            se = round(sd/sqrt(n()),2),
            n_ind_total = sum(ntotal))


descriptivos <- elev %>% inner_join(densidad_avg) %>%
  dplyr::select(-sd) %>%
  mutate(mean = round(mean, 2),
         se = round(se,2)) %>%
  unite("density", mean,se, sep = " ± ") %>%
  inner_join(diversidad_avg) %>%
  dplyr::select(-sd) %>%
  mutate(mean = round(mean, 2),
         se = round(se,2)) %>%
  unite("diversity", mean,se, sep = " ± ") %>%
  inner_join(richness) %>%
  dplyr::select(-n) %>%
  rename(richness = n_year_selected) %>%
  inner_join(abundancia) %>%
  dplyr::select(-sd) %>%
  unite("abundancia_media", mean,se, sep = " ± ") %>%
  dplyr::select(
    "Transect" = nombre,
    "Code" = code,
    "Elevation" = elev,
    "Length" = length_transect,
    "Abundance" = density,
    "Diversity (H’)" = diversity,
    "Richness" = richness,
    "Mean number of individuals" = abundancia_media,
    "Total number of individuals" = n_ind_total)


write_csv(descriptivos, here::here("data/tabla_descriptivos_yearselected.csv"))

