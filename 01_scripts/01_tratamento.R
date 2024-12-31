# 1. Pacotes --------------------------------------------------------------

library(tidyverse)
library(haven)
library(readxl)
library(janitor)
library(sandwich)
library(lmtest)
library(skimr)
library(lme4)
library(stargazer)

# 2. Importação -----------------------------------------------------------

eseb_2018 <- read_sav("00_dados/04622/04622.sav")
governadores <- read_xlsx("00_dados/governadores.xlsx")

# 3. Seleção de variáveis -------------------------------------------------

eseb_2018 <- clean_names(eseb_2018)

glimpse(eseb_2018)

eseb_2018 <- eseb_2018 |>
  select(
    # Variaveis para filtro
    votou_1t_2018 = q12p1_a,
    votou_2t_2018 = q12p2_a,
    votou_1t_2014 = q13a,

    # Variáveis dependentes
    vt_governador_1t_2018 = q12g_1a,
    vt_governador_2t_2018 = q12g_2,
    vt_presidente_1t_2018 = q12p1_b,
    vt_presidente_2t_2018 = q12p2_b,

    # Avaliação retrospectiva
    aval_sit_econ_br = q11,
    recebeu_bf = p29,
    mudou_classe = p28,
    classe_antes = p28a,
    classe_depois = p28b,

    # Avaliação prospectiva
    # --- (não tem)

    # Identidade política
    vt_presidente_1t_2014 = q13b,
    opiniao_PT = q1501,
    # opiniao_PSDB = q1505,
    autoloc_esqdir = q18,
    partido_nao_gosta = p1a,

    # Contexto
    regiao = reg,
    estado = uf,
    cidade = cid,

    # Socioeconômicas e demográficas
    cor_raca = d12a,
    religiao = d10,
    evangelico = d10a,
    sexo = d2_sexo,
    escolaridade = d3_escola,
    vl_renda_fam = d9,
    fx_renda_fam = d9a,
    idade = d1a_id
  )

# 4. Tratamento -----------------------------------------------------------

## Variáveis Dependentes --------------------------------------------------

### Governador: Situação x Oposição ----

# Dataframe para fazer join
governadores <- clean_names(governadores)

# Limpa espaços em branco
governadores <- governadores |>
  mutate(
    across(.cols = everything(), .fns = str_squish),
    across(.cols = everything(), .fns = str_trim)
  )

# Ajeita o dataframe para join
governadores <- governadores |>
  group_by(estado) |>
  mutate(
    vt_ultimo_turno = case_when(
      all(vt_governador_2t_2018 == "Missing") ~ vt_governador_1t_2018,
      .default = vt_governador_2t_2018
    )
  ) |>
  select(estado, vt_ultimo_turno, classificacao, partido, resultado) |>
  filter(classificacao == "Situação") |>
  ungroup()

# Filtra somente quem votou tanto no 1T quanto no 2T de 2018
eseb_2018 <- eseb_2018 |>
  filter(
    as_factor(votou_1t_2018) == "Sim, votou",
    as_factor(votou_2t_2018) == "Sim, votou",
  )

# Faz o join com o dataframe de governadores
eseb_2018 <- eseb_2018 |>
  mutate(
    across(
      .cols = c("estado", "vt_governador_1t_2018"),
      .fns = as_factor
    )
  ) |>
  left_join(
    y = governadores,
    by = c("estado", "vt_governador_1t_2018" = "vt_ultimo_turno")
  ) |>
  rename(vt_governador_sit_opo = classificacao)

# Codifica nossa variável dependente como 0 e 1
eseb_2018 <- eseb_2018 |>
  mutate(
    vt_governador_sit_opo = case_when(
      vt_governador_sit_opo == "Situação" ~ 0,
      .default = 1
    )
  )
# Nota: veja que se for Situação, então é 0. Se for Oposição ou NA (que abarca
# casos como não lembra, voto nulo, em branco etc.), então é 1. Ou seja, vamos
# modelar a chance de votar na Oposição

# Analisa summary final da base
skim(eseb_2018)

### Presidente: Situação x Oposição ----
eseb_2018 <- eseb_2018 |>
  mutate(
    vt_presidente_2t_2018 = if_else(
      as_factor(vt_presidente_2t_2018) == "Fernando Haddad (PT)", 0, 1
    )
  )

## Variáveis Independentes ------------------------------------------------

glimpse(eseb_2018)

### Avaliação retrospectiva ----

# Avaliação da situação economica
eseb_2018 <- eseb_2018 |>
  mutate(
    aval_sit_econ_br = as_factor(aval_sit_econ_br),
    aval_sit_econ_br = case_when(
      str_detect(aval_sit_econ_br, "melhor") ~ "Melhor",
      str_detect(aval_sit_econ_br, "pior") ~ "Pior",
      aval_sit_econ_br == "Igual" ~ "Igual"
    ),
    aval_sit_econ_br = as_factor(aval_sit_econ_br),
    aval_sit_econ_br = relevel(aval_sit_econ_br, ref = "Melhor")
  )

# Remove valores não válidos
eseb_2018 <- eseb_2018 |>
  filter(
    !as_factor(recebeu_bf) %in% c("Não respondeu", "Não sabe"),
    !as_factor(mudou_classe) %in% c("Não respondeu", "Não sabe"),
    !as_factor(classe_antes) %in% c("Não respondeu", "Não sabe"),
    !as_factor(classe_depois) %in% c("Não respondeu", "Não sabe")
  )

# Define categoria de referência para variável recebeu_bf
eseb_2018 <- eseb_2018 |>
  mutate(
    recebeu_bf = as_factor(recebeu_bf),
    recebeu_bf = relevel(recebeu_bf, ref = "Sim")
  )

# Cria variável da direção da mudança de classe social e define categoria de
# referência
eseb_2018 <- eseb_2018 |>
  mutate(
    ascensao = case_when(
      as_factor(mudou_classe) == "Permaneceram na mesma classe" ~ "Igual",
      as_factor(mudou_classe) == "Mudaram de classe social" &
        (as.numeric(classe_antes) - as.numeric(classe_depois)) > 0 ~ "Subiu",
      as_factor(mudou_classe) == "Mudaram de classe social" &
        (as.numeric(classe_antes) - as.numeric(classe_depois)) < 0 ~ "Desceu"
    ),
    ascensao = as_factor(ascensao),
    ascensao = relevel(ascensao, ref = "Subiu")
  )

# Verifica se deu certo
eseb_2018 |>
  distinct(mudou_classe, classe_antes, classe_depois, ascensao) |>
  slice_sample(n = 5)

### Identidade política ----

# Cria dummy Dilma x Não Dilma nas eleições de 1T de 2014
eseb_2018 <- eseb_2018 |>
  filter(as_factor(votou_1t_2014) == "Sim, votou") |>
  mutate(
    vt_presidente_1t_2014 = as_factor(vt_presidente_1t_2014),
    vt_presidente_1t_2014 = case_when(
      vt_presidente_1t_2014 == "Dilma" ~ "Dilma",
      .default = "Não Dilma"
    ),
    vt_presidente_1t_2014 = as_factor(vt_presidente_1t_2014),
    vt_presidente_1t_2014 = relevel(vt_presidente_1t_2014, ref = "Dilma")
  )

# Retira respostas inválidas da v. de autolocalização ideológica
eseb_2018 <- eseb_2018 |>
  filter(
    !str_detect(as_factor(autoloc_esqdir), "Não")
  )

# Nao gosta do PT
eseb_2018 <- eseb_2018 |>
  mutate(
    partido_nao_gosta = if_else(
      as_factor(partido_nao_gosta) == "PT", "PT", "Outros"
    ),
    partido_nao_gosta = as_factor(partido_nao_gosta),
    partido_nao_gosta = relevel(partido_nao_gosta, ref = "Outros")
  )

### Contexto ----
eseb_2018 <- eseb_2018 |>
  mutate(regiao = as_factor(regiao), regiao = relevel(regiao, "Sudeste"))

eseb_2018 <- eseb_2018 |>
  mutate(
    estado = as_factor(estado),
    cidade = as_factor(cidade),
    cap_ncap = case_when(
      estado == "Rondônia" & cidade == "Porto Velho" ~ "Capital",
      estado == "Acre" & cidade == "Rio Branco" ~ "Capital",
      estado == "Amazonas" & cidade == "Manaus" ~ "Capital",
      estado == "Roraima" & cidade == "Boa Vista" ~ "Capital",
      estado == "Pará" & cidade == "Porto Velho" ~ "Capital",
      estado == "Amapá" & cidade == "Macapá" ~ "Capital",
      estado == "Tocantins" & cidade == "Palmas" ~ "Capital",
      estado == "Maranhão" & cidade == "São Luís" ~ "Capital",
      estado == "Piauí" & cidade == "Teresina" ~ "Capital",
      estado == "Rio Grande do Norte" & cidade == "Natal" ~ "Capital",
      estado == "Paraíba" & cidade == "João Pessoa" ~ "Capital",
      estado == "Pernambuco" & cidade == "Recife" ~ "Capital",
      estado == "Alagoas" & cidade == "Maceió" ~ "Capital",
      estado == "Sergipe" & cidade == "Aracaju" ~ "Capital",
      estado == "Bahia" & cidade == "Salvador" ~ "Capital",
      estado == "Minas Gerais" & cidade == "Belo Horizonte" ~ "Capital",
      estado == "Espírito Santo" & cidade == "Vitória" ~ "Capital",
      estado == "Rio de Janeiro" & cidade == "Rio de Janeiro" ~ "Capital",
      estado == "São Paulo" & cidade == "São Paulo" ~ "Capital",
      estado == "Paraná" & cidade == "Curitiba" ~ "Capital",
      estado == "Santa Catarina" & cidade == "Florianópolis" ~ "Capital",
      estado == "Rio Grande do Sul" & cidade == "Porto Alegre" ~ "Capital",
      estado == "Mato Grosso do Sul" & cidade == "Campo Grande" ~ "Capital",
      estado == "Mato Grosso" & cidade == "Cuiabá" ~ "Capital",
      estado == "Goiás" & cidade == "Goiânia" ~ "Capital",
      estado == "Distrito Federal" & cidade == "Brasília" ~ "Capital",
      .default = "Não capital"
    ),
    cap_ncap = as_factor(cap_ncap),
    cap_ncap = relevel(cap_ncap, "Não capital"),
  )

### Socioeconômicas e demográficas ----

# Cor ou raça
eseb_2018 <- eseb_2018 |>
  filter(!str_detect(as_factor(cor_raca), "Não")) |>
  mutate(
    cor_raca = case_when(
      as_factor(cor_raca) == "Branco" ~ "Branco",
      .default = "Não branco"
    ),
    cor_raca = as_factor(cor_raca),
    cor_raca = relevel(cor_raca, ref = "Não branco")
  )

# Religião
eseb_2018 <- eseb_2018 |>
  mutate(
    religiao = as_factor(religiao),
    religiao = case_when(
      religiao == "Católica" ~ "Católica",
      religiao == "Evangélica" ~ "Evangélica",
      .default = "Outras/não tem"
    ),
    religiao = as_factor(religiao),
    religiao = relevel(religiao, ref = "Católica"),
    evangelico = as_factor(evangelico),
    evangelico = case_when(
      religiao == "Não evangélico" ~ "Não evangélico",
      religiao == "Evangélica" & evangelico == "Assembléia de Deus" ~ "(Neo)pentecostal",
      religiao == "Evangélica" & evangelico == "Batista" ~ "Protestante",
      religiao == "Evangélica" & evangelico == "Universal do Reino de Deus" ~ "(Neo)pentecostal",
      religiao == "Evangélica" & evangelico == "Congregacional Cristã do Brasil" ~ "(Neo)pentecostal",
      religiao == "Evangélica" & evangelico == "Evangelho Quadrangular" ~ "(Neo)pentecostal",
      religiao == "Evangélica" & evangelico == "Pentecostal" ~ "(Neo)pentecostal",
      religiao == "Evangélica" & evangelico == "Deus é Amor" ~ "(Neo)pentecostal",
      religiao == "Evangélica" & evangelico == "Presbiteriana" ~ "Protestante",
      religiao == "Evangélica" & evangelico == "Adventista do Sétimo dia/adventista" ~ "Protestante",
      religiao == "Evangélica" & evangelico == "Igreja Mundial do Poder de Deus" ~ "(Neo)pentecostal",
      religiao == "Evangélica" & evangelico == "Igreja Internacional da Graça" ~ "(Neo)pentecostal",
      religiao == "Evangélica" & evangelico == "Maranata" ~ "(Neo)pentecostal",
      religiao == "Evangélica" & evangelico == "Comunidade Cristã" ~ "(Neo)pentecostal",
      religiao == "Evangélica" & evangelico == "Brasil para Cristo" ~ "(Neo)pentecostal",
      religiao == "Evangélica" & evangelico == "Luterana" ~ "Protestante",
      religiao == "Evangélica" & evangelico == "ICB - Casa da Benção" ~ "(Neo)pentecostal",
      religiao == "Evangélica" & evangelico == "Metodista" ~ "Protestante",
      religiao == "Evangélica" & evangelico == "Protestante" ~ "Protestante",
      .default = "(Neo)pentecostal"
    ),
    evangelico = as_factor(evangelico),
    evangelico = relevel(evangelico, "(Neo)pentecostal")
  )

# Sexo
eseb_2018 <- eseb_2018 |>
  mutate(sexo = as_factor(sexo), sexo = relevel(sexo, "Masculino"))

# Escolaridade
eseb_2018 |> count(escolaridade)

# Renda
eseb_2018 <- eseb_2018 |>
  mutate(
    vl_renda_fam = as.numeric(vl_renda_fam),
    fx_renda_fam = as_factor(fx_renda_fam),
    fx_renda_familiar = case_when(
      vl_renda_fam == 9999999 ~ fx_renda_fam,
      vl_renda_fam == 9999998 ~ fx_renda_fam,
      vl_renda_fam <= 954 ~ "Até 1 s.m.",
      vl_renda_fam > 954 & vl_renda_fam <= 1908 ~ "Mais de 1 até 2 s.m.",
      vl_renda_fam > 1908 & vl_renda_fam <= 4770 ~ "Mais de 2 até 5 s.m.",
      vl_renda_fam > 4770 & vl_renda_fam <= 9540 ~ "Mais de 5 até 10 s.m.",
      vl_renda_fam > 9540 ~ "Mais de 10 s.m."
    )
  )
eseb_2018 <- eseb_2018 |>
  mutate(
    fx_renda_familiar = case_when(
      fx_renda_familiar == "Até R$ 954,00 (até 1 salário mínimo)" ~ "Até 1 s.m.",
      fx_renda_familiar == "Mais de R$ 1.908,00 até R$ 4.770,00 (mais de 2 até 5 salários mínimos)" ~ "Mais de 2 até 5 s.m.",
      fx_renda_familiar == "Mais de R$ 14.310,00 até R$ 19.080,00 (mais de 15 até 20 salários mínimos)" ~ "Mais de 10 s.m.",
      fx_renda_familiar == "Mais de R$ 4.770,00 até R$ 9.540,00 (mais de 5 até 10 salários mínimos)" ~ "Mais de 5 até 10 s.m.",
      fx_renda_familiar == "Mais de R$ 9.540,00 até R$ 14.310,00 (mais de 10 até 15 salários mínimos)" ~ "Mais de 10 s.m.",
      fx_renda_familiar == "Mais de R$ 954,00 até R$ 1.908,00 (mais de 1 até 2 salários mínimos)" ~ "Mais de 1 até 2 s.m.",
      .default = fx_renda_familiar
    )
  )

eseb_2018 <- eseb_2018 |>
  filter(!str_detect(fx_renda_familiar, "Não"))

eseb_2018 <- eseb_2018 |>
  mutate(
    fx_renda_familiar = factor(
      fx_renda_familiar,
      levels = c(
        "Até 1 s.m.",
        "Mais de 1 até 2 s.m.",
        "Mais de 2 até 5 s.m.",
        "Mais de 5 até 10 s.m.",
        "Mais de 10 s.m."
      )
    )
  )


# 5. Exportação -----------------------------------------------------------

write_rds(
  x = eseb_2018,
  file = "02_relatorios/eseb2018.R"
)
