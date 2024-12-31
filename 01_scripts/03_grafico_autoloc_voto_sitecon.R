library(tidyverse)
library(haven)
library(janitor)
library(patchwork)

eseb_2018 <- read_rds(file = "02_relatorios/eseb2018.R")

g1 <- eseb_2018 |>
  mutate(
    autoloc_esqdir = as.numeric(zap_label(autoloc_esqdir)),
    vt_presidente_2t_2018 = if_else(
      vt_presidente_2t_2018 == 0, "Situação", "Oposição"
    )
  ) |>
  select(
    autoloc_esqdir, aval_sit_econ_br, vt_governador_sit_opo, vt_presidente_2t_2018
  ) |>
  drop_na() |>
  ggplot() +
  aes(x = autoloc_esqdir, colour = vt_presidente_2t_2018) +
  geom_density(alpha = 0.3) +
  facet_wrap(~aval_sit_econ_br) +
  theme_bw() +
  labs(
    colour = "Presidente 2º turno 2018",
    x = ""
  )

g2 <- eseb_2018 |>
  mutate(
    autoloc_esqdir = as.numeric(zap_label(autoloc_esqdir)),
    vt_governador_sit_opo = if_else(
      vt_governador_sit_opo == 0, "Situação", "Oposição"
    )
  ) |>
  select(
    autoloc_esqdir, aval_sit_econ_br, vt_governador_sit_opo, vt_presidente_2t_2018
  ) |>
  drop_na() |>
  ggplot() +
  aes(x = autoloc_esqdir, colour = vt_governador_sit_opo) +
  geom_density(alpha = 0.3) +
  facet_wrap(~aval_sit_econ_br) +
  theme_bw() +
  labs(
    colour = "Governador 2º turno 2018",
    x = "Autolocalização (esquerda/direita)"
  )

g1 / g2
