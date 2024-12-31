# 1. Pacotes --------------------------------------------------------------

library(tidyverse)
library(lme4)
library(stargazer)
library(broom.mixed)

# 2. Importação -----------------------------------------------------------

eseb_2018 <- read_rds(file = "02_relatorios/eseb2018.R")

# 3. Seleção de variáveis -------------------------------------------------

names(eseb_2018)

eseb_2018 <- eseb_2018 |>
  select(
    vt_presidente_2t_2018,
    vt_governador_sit_opo,
    aval_sit_econ_br,
    recebeu_bf,
    ascensao,
    vt_presidente_1t_2014,
    partido_nao_gosta,
    autoloc_esqdir,
    regiao,
    cap_ncap,
    cor_raca,
    religiao,
    sexo,
    escolaridade,
    idade,
    fx_renda_familiar
  )

# 4. Modelos --------------------------------------------------------------

## Modelo 1 ----
modelo1 <- glm(
  vt_presidente_2t_2018 ~ aval_sit_econ_br +
    recebeu_bf +
    ascensao +
    vt_presidente_1t_2014 +
    partido_nao_gosta +
    autoloc_esqdir +
    regiao +
    cap_ncap +
    cor_raca +
    religiao +
    sexo +
    escolaridade +
    idade +
    fx_renda_familiar,
  family = binomial,
  data = eseb_2018
)

summary(modelo1)

## Modelo 2 ----
modelo2 <- glmer(
  vt_governador_sit_opo ~ aval_sit_econ_br +
    recebeu_bf +
    ascensao +
    vt_presidente_1t_2014 +
    partido_nao_gosta +
    autoloc_esqdir +
    (1 | regiao) +
    cap_ncap +
    cor_raca +
    religiao +
    sexo +
    escolaridade +
    idade +
    fx_renda_familiar,
  family = binomial,
  data = eseb_2018
)

summary(modelo2)

## Checagem de pressupostos ----
performance::check_model(modelo1)
performance::check_model(modelo2)

## Tabelas ----
stargazer(modelo1, modelo2, type = "text")

## Gráficos ----

# Modelo 1
coeficientes1 <- broom::tidy(modelo1, conf.int = TRUE) %>%
  filter(term != "(Intercept)") # Exclui o intercepto para clareza

ggplot(coeficientes1, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_abline(slope = 0) +
  coord_flip() +
  labs(
    x = "",
    y = "Estimativas dos Coeficientes",
    title = ""
  ) +
  theme_classic()

# Modelo 2
coeficientes2 <- broom.mixed::tidy(modelo2, effects = "fixed", conf.int = TRUE) %>%
  filter(term != "(Intercept)")

ggplot(coeficientes2, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_abline(slope = 0) +
  coord_flip() +
  labs(
    x = "",
    y = "Estimativas dos Coeficientes",
    title = ""
  ) +
  theme_classic()


# Junta os dois

coeficientes1 <- coeficientes1 |>
  mutate(modelo = "Modelo 1")

coeficientes2 <- coeficientes2 |>
  mutate(modelo = "Modelo 2")

coeficientes <- bind_rows(coeficientes1, coeficientes2)

coeficientes <- coeficientes |>
  mutate(
    significativo = ifelse(conf.low > 0 | conf.high < 0, "Sim", "Não")
  )

ggplot(
  coeficientes,
  aes(
    x = reorder(term, estimate),
    y = estimate,
    colour = significativo
  )
) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_abline(slope = 0) +
  coord_flip() +
  labs(
    x = "",
    y = "Estimativas dos Coeficientes",
    title = ""
  ) +
  theme_classic() +
  scale_colour_manual(values = c("grey", "black"), guide = "none") +
  facet_wrap(~modelo)
