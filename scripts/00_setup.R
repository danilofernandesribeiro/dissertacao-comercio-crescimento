# Limpa ambiente
rm(list = ls())

# Diretório raiz do projeto
setwd("dissertacao-comercio-crescimento")

# Pacotes necessários
packages <- c("tidyverse", "plm", "lmtest", "sandwich")

installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])

lapply(packages, library, character.only = TRUE)
