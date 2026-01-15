############################################################
# Projeto: Comércio Internacional e Crescimento Econômico
# Autor: Danilo Fernandes Ribeiro
# Descrição: Transformação das variáveis e análise gráfica
# Período: 2000–2022
############################################################

# ============================
# 1. Pacotes necessários
# ============================

library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)

# ============================
# 2. Transformação das variáveis
# ============================

# Logaritmos naturais
Dados <- Dados %>%
  mutate(
    ln_com   = log(`X+M`),
    ln_pibc  = log(`PIB per capita`),
    ln_cam   = log(`Taxa de Câmbio`),
    ln_Expor = log(Exportações),
    ln_Impor = log(Importações)
  )

# ============================
# 3. Gráfico 1: Tendência média do comércio (ln X + ln M)
# ============================

dados_media_comercio <- Dados %>%
  group_by(Ano, Dependentescomm) %>%
  summarise(
    Media_ln_com = mean(ln_Expor + ln_Impor, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(dados_media_comercio,
       aes(x = Ano,
           y = Media_ln_com,
           color = factor(Dependentescomm),
           group = Dependentescomm)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("0" = "royalblue", "1" = "orange"),
    labels = c("Grupo B – Não Dependentes", "Grupo A – Dependentes")
  ) +
  labs(
    title = "Tendência Média do Comércio Internacional",
    subtitle = "Média de ln(Exportações) + ln(Importações) por grupo",
    x = "Ano",
    y = "Média em logaritmo",
    color = "Grupo"
  ) +
  theme_minimal()

# ============================
# 4. Gráfico 2: Exportações vs Importações por grupo
# ============================

dados_fluxos_grupos <- Dados %>%
  group_by(Ano, Dependentescomm) %>%
  summarise(
    Média_ln_Expor = mean(ln_Expor, na.rm = TRUE),
    Média_ln_Impor = mean(ln_Impor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(Média_ln_Expor, Média_ln_Impor),
    names_to = "Fluxo",
    values_to = "Valor_Log"
  )

ggplot(dados_fluxos_grupos,
       aes(x = Ano,
           y = Valor_Log,
           color = Fluxo,
           group = Fluxo)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.5) +
  facet_wrap(
    ~ Dependentescomm,
    labeller = as_labeller(
      c("0" = "Grupo B – Não Dependentes",
        "1" = "Grupo A – Dependentes")
    )
  ) +
  scale_color_manual(
    values = c("Média_ln_Expor" = "tomato",
               "Média_ln_Impor" = "darkturquoise"),
    labels = c("Exportações (ln médio)",
               "Importações (ln médio)")
  ) +
  labs(
    title = "Evolução do Comércio Externo por Grupo",
    subtitle = "Comparação entre exportações e importações (2000–2022)",
    x = "Ano",
    y = "Valor médio em logaritmo",
    color = "Fluxo Comercial"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# ============================
# 5. Gráfico 3: Amostra total (64 países)
# ============================

dados_amostra_total <- Dados %>%
  group_by(Ano) %>%
  summarise(
    Média_ln_Expor = mean(ln_Expor, na.rm = TRUE),
    Média_ln_Impor = mean(ln_Impor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(Média_ln_Expor, Média_ln_Impor),
    names_to = "Fluxo",
    values_to = "Valor_Log"
  )

ggplot(dados_amostra_total,
       aes(x = Ano,
           y = Valor_Log,
           color = Fluxo,
           group = Fluxo)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("Média_ln_Expor" = "tomato",
               "Média_ln_Impor" = "darkturquoise"),
    labels = c("Exportações (ln médio)",
               "Importações (ln médio)")
  ) +
  labs(
    title = "Evolução do Comércio Externo – Amostra Global",
    subtitle = "Média dos logaritmos para 64 países (2000–2022)",
    x = "Ano",
    y = "Valor médio em logaritmo",
    color = "Fluxo Comercial"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
