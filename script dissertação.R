#################################################################################
#################################################################################
# Mestrado em Economia - UFSCar
# Universidade Federal de São Carlos - UFSCar
# DISSERTAÇÃO DE DANILO FERNANDES RIBEIRO
# Orientadora Dra. Rosane
# Metodologia Dados em Painel Estático

###############################################################################
# Período de 2000-2022
# Países ==> 64 países
# DEFINIÇÃO DAS VARIÁVEIS
#################################################################################
# Pacotes necessário para a realização da estimação do modelo
# Propósito: Fornece funções para análise de regressão e diagnóstico de modelos

rm(list=ls());
library(plm)
library(lmtest)
library(MTest)
library(dplyr)     # Facilita operações comuns em data frames e tibbles como filtragem
library(psych)     # Uso Principal: Análise estatística e psicométrica
library(Hmisc)     # Uso Principal: Ferramentas para análise de dados e gráficos.
library(faraway)   # Uso Principal: Dados e funções para análise estatística
library(lmtest)    # Uso Principal: Testes para modelos de regressão
library(stargazer) # Uso Principal: Criação de tabelas de resultados para artigos.
library(broom)     # Uso Principal: Limpeza e organização de resultados de modelos
library(xtable)    # Uso Principal: Conversão de tabelas para LaTeX e HTML
library(knitr)     # Uso Principal: Integração de código e texto em documentos
library(psych)     # Uso Principal: Estimativas robustas de variância
library(stargazer) # Uso Principal: Criação de tabelas de resultados para artigos
library(xtable)    # Uso Principal: Conversão de tabelas para LaTeX e HTML
library(knitr)      # Uso Principal: Integração de código e texto em documentos
library(sandwich)   # Uso Principal: Estimativas robustas de variância.
library(margins)    # Uso Principal: Cálculo de efeitos marginais em modelos de regressão
library(glm.predict) #Uso Principal: Predições para modelos de regressão generalizados
library(systemfit) # Uso Principal: Ajuste de sistemas de equações simultâneas
library(texreg)    # Uso Principal: Criação de tabelas de resultados em LaTeX
library (miscTools)
library(pgmm)
library(tseries)   # Propósito: Análise e modelagem de séries temporais
library(tidyverse) # Propósito: Conjunto de pacotes para manipulação e visualização de dados
library(gplots)    # Propósito: Criação de gráficos e visualizações adicionais 
library(car)
library(corrplot)
library(ggcorrplot)
options(scipen=999)

################################################################################
#IMPORTANDO A BASE DE DADOS (File ==> Import dataset )

library(readxl)
Dados <- read_excel("diss/Script/Dados/Dados.xlsx", 
                    +     sheet = "Dados")
View(Dados)

###############################################################################
# Análise dos dados

head(Dados)
tail(Dados)
names(Dados)
str(Dados)
list(Dados)
any(is.na(Dados))

################################################################################

# Adicionar variáveis com o logaritmo natural
ln_com <- log(Dados$`X+M`)
ln_pibc <- log(Dados$`PIB per capita`)
ln_inv <-log(Dados$`Formação bruta do capital`)
ln_exp <-log(Dados$Exportações)
ln_imp <-log(Dados$Importações)

# Adicionar as novas variáveis ao data frame

Dados$ln_com <-ln_com
Dados$ln_pibc <-ln_pibc
Dados$ln_inv <-ln_inv
Dados$ln_exp <-ln_exp
Dados$ln_imp <-ln_imp


# Calcular desvio padrão
sd(Dados$ln_pibc, na.rm = TRUE)
sd(Dados$ln_pibc, na.rm = TRUE)
sd(Dados$ln_com, na.rm = TRUE)
sd(Dados$ln_inv, na.rm = TRUE)
sd(Dados$`Taxa de Câmbio`, na.rm = TRUE)
sd(Dados$Inflação, na.rm = TRUE)
sd(Dados$`Capital Humano`, na.rm = TRUE)


#########################################################################
# Exemplo: renomeando variáveis
names(Dados) <- make.names(names(Dados))

# Verificar os nomes das colunas
print(names(Dados))

#Calcular média, mínimo e máximo

describe(Dados)[, c("mean", "min", "max")]

################################################################################
#DECLARAR DADOS COMO DE PAINEL
pdata <- pdata.frame(Dados, index = c("Países", "Ano"))

# Contar o número de observações para cada combinação de índices
count_per_panel <- table(index(Dados))

# Verificar se todas as combinações têm o mesmo número de observações
is_balanced <- length(unique(count_per_panel)) == 1
is_balanced

################################################################################

#ESTIMATIVA DE MINIMOS QUADRADOS


mod_ols1 <- lm(ln_pibc ~ ln_com + ln_inv + 
                 Inflação + Taxa.de.Câmbio + Capital.Humano, 
               data = Dados)

summary(mod_ols1)

#Matriz de correlação
vars_continuas <- Dados[, c("ln_pibc", "ln_com", 
                            "ln_inv", "Inflação", "Taxa.de.Câmbio", "Capital.Humano")]
matriz_cor <- cor(vars_continuas, use = "complete.obs")
print("Matriz de correlação:")

corrplot(matriz_cor,
         method = "color",
         col = colorRampPalette(c("white", "skyblue", "steelblue"))(200),
         type = "upper",
         tl.col = "black",
         tl.cex = 0.9,
         addCoef.col = "black",
         number.cex = 0.7,
         diag = FALSE)

#Calcular o VIF
print("Fatores de Inflação da Variância (VIF):")
vif_resultado <- vif(mod_ols1)
print(round(vif_resultado, 2))

###############################################################################
#Divisão dos grupos
# 1. Transformar em fator (execute sozinho primeiro)
Dados$Dependentescomm <- as.factor(Dados$Dependentescomm)

# 2. Gerar as estatísticas (fechando o parêntese corretamente)
estatisticas_por_grupo <- describeBy(
  Dados[, c("ln_pibc", "ln_com", "ln_inv", "Inflação", "Taxa.de.Câmbio", "Capital.Humano")], 
  group = Dados$Dependentescomm
)

# Ver os resultados do Grupo 0 (Não Dependentes)
print(estatisticas_por_grupo$`0`)

# Ver os resultados do Grupo 1 (Dependentes)
print(estatisticas_por_grupo$`1`)

# Definir a ordem fixa das variáveis para ambos os gráficos
ordem_fixa <- c("ln_pibc", "ln_com", "Capital.Humano", "ln_inv", "Inflação", "Taxa.de.Câmbio")

# Recriar as matrizes garantindo que sigam essa ordem
cor_grupo0_fixa <- cor(Dados[Dados$Dependentescomm == 0, ordem_fixa], use = "complete.obs")
cor_grupo1_fixa <- cor(Dados[Dados$Dependentescomm == 1, ordem_fixa], use = "complete.obs")

# Gerar o Gráfico para o Grupo 0 (Sem reordenar)
p0_fixo <- ggcorrplot(cor_grupo0_fixa, 
                      hc.order = FALSE, # Mantém a ordem que definimos acima
                      type = "lower",
                      lab = TRUE, 
                      title = "Correlação: Grupo 0 (Não Dependentes)",
                      colors = c("#6D9EC1", "white", "#E46726"))

# Gerar o Gráfico para o Grupo 1 (Mesma ordem do Grupo 0)
p1_fixo <- ggcorrplot(cor_grupo1_fixa, 
                      hc.order = FALSE, # Mantém a mesma ordem do anterior
                      type = "lower",
                      lab = TRUE, 
                      title = "Correlação: Grupo 1 (Dependentes)",
                      colors = c("#6D9EC1", "white", "#E46726"))

# Exibir os gráficos

print(p0_fixo)
print(p1_fixo)

##############################################################################
# ESTIMATIVA DE EFEITO FIXO

modelo_fe <- plm( ln_pibc ~ ln_com + ln_inv + 
                    Inflação + Taxa.de.Câmbio + Capital.Humano,
                  data = Dados,
                  index = c("Países", "Ano"),
                  model = "within"  # efeito fixo
)

summary(modelo_fe)


################################################################################
#MODELO DE EFEITO ALEATÓRIO

modelore <- plm(ln_pibc ~ ln_com + ln_inv + 
                  Inflação + Taxa.de.Câmbio + Capital.Humano,
                data = Dados,
                index = c("Países", "Ano"),
                model = "random"
)

summary(modelore)


################################################################################
#EFEITO ALEATORIO X EFEITO FIXO --> null: random is better than fixed

#Teste F de efeito fixo

pFtest(modelo_fe, mod_ols1)

#Teste de Hausman

phtest(modelo_fe , modelore )


##############################################################################

#Teste de autocorrelação DW

pdw_result <- pdwtest(modelo_fe , alternative = "two.sided")
print(pdw_result)

#teste de Wooldridge
pbgtest(modelo_fe )


# TESTE PARA HETEROCEDASTICIA --> H0) The null hypothesis for the Breusch-Pagan test is homoskedasticity

bptest(modelo_fe , data = Dados,  studentize=F)

################################################################################

# Rodando o teste de Conditional LM para erros AR(1) ou MA(1)
conditional_lm_test <- pcdtest(modelo_fe, test = "lm")
print(conditional_lm_test)

################################################################################
#Driscoll-Kraay

summary(modelo_fe , vcov = function(x) vcovSCC(x, type = "HC1", maxlag = 2))


################################################################################
Dados1<-Dados
Dados1$id1<-2
Dados1$id1[Dados1$Países == "Arábia Saudita"| Dados1$Países == "Argélia"|
             Dados1$Países == "Armênia"| Dados1$Países == "Austrália"|
             Dados1$Países == "Bahrein"| Dados1$Países == "Brasil"|
             Dados1$Países == "Bolívia"| Dados1$Países == "Camerões"|
             Dados1$Países == "Chile"| Dados1$Países == "Colômbia"|
             Dados1$Países == "Costa do Marfim"| Dados1$Países == "Gabão"|
             Dados1$Países == "Gana"| Dados1$Países == "Grécia"|
             Dados1$Países == "Islândia"| Dados1$Países == "Noruega"|
             Dados1$Países == "Nova Zelândia"| Dados1$Países == "Rússia"|
             Dados1$Países == "África do sul"| Dados1$Países == "Ucrânia"|
             Dados1$Países == "Uganda"| Dados1$Países == "Uruguai"
]<-1# Dados1$id1[Dados1$id1 != 1]<-2
#DECLARAR DADOS COMO DE PAINEL
pdata.frame(Dados1, index=c("Países","Ano"))
modelo_fe_gruposD<- plm(ln_pibc ~ ln_com + ln_inv + Inflação + Taxa.de.Câmbio + Capital.Humano+
                          factor(id1)*ln_com + factor(id1)*ln_inv + 
                          factor(id1)*Inflação + factor(id1)*Taxa.de.Câmbio + 
                          factor(id1)*Capital.Humano,
                        data = Dados1,
                        model = "within",
)
summary(modelo_fe_gruposD)

#teste de Wooldridge
pbgtest(modelo_fe_gruposD)

#HETEROCEDASTICIA
bptest(modelo_fe_gruposD , data = Dados1,  studentize=F)

## Rodando o teste de Conditional LM para erros AR(1) ou MA(1)

conditional_lm_test <- pcdtest(modelo_fe_gruposD , test = "lm")
print(conditional_lm_test)

################################################################################
summary(modelo_fe_gruposD , vcov = function(x) vcovSCC(x, type = "HC1", maxlag = 2))

################################################################################
save.image("file = Resultados.Rdada")

