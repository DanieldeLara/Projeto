### Business Inteligence ###

# Pacotes
install.packages("plotly")
install.packages("Hmisc")
install.packages("esquisse")

library(readr)
library(ggplot2)
library(plotly)
library(e1071)
library(dplyr)
library(Hmisc)
library(esquisse)
library(devtools)

# 1. Faça a importação do arquivo do ENADE 2017

# Pasta de destino 

setwd("D:\\Testes")
getwd()

# Importando arquivo solicitado 
base_completa <- read.table("MICRODADOS_ENADE_2017.TXT", header = TRUE, sep = ";", dec = ",", colClasses = c(NT_OBJ_FG = "numeric"))
View(base_completa)

# 2. Faça o filtro escolhendo as seguintes variáveis e as classifique quanto ao tipo de variável: (0,5 pontos)
# NT_OBJ_FG, CO_GRUPO, CO_REGIAO_CURSO, QE_I02, CO_TURNO_GRADUACAO
base <- base_completa %>% dplyr::select(NT_OBJ_FG,
                                        CO_GRUPO,
                                        CO_REGIAO_CURSO,
                                        QE_I02,
                                        CO_TURNO_GRADUACAO)
View(base)

# Classifindo as variáveis
# NT_OBJ_FG: variável quantitativa contínua
# CO_GRUPO: variável qualitativa nominal
# CO_REGIAO_CURSO: variável qualitativa nominal
# QE_I02: variável qualitativa nominal
# CO_TURNO_GRADUACAO: variável qualitativa ordinal

# 3. Escolha um curso do ENADE
# Curso Engelharia Ambiental (6307)
ambiente <- base %>% filter(CO_GRUPO == 6307)
View(ambiente)

# 4. Tranforme todas as variáveis colocando em seus devidos rótulos para que facilite a análise descritiva
# Tranformando a variável CO_GRUPO
ambiente = ambiente %>% mutate(CURSO = case_when(CO_GRUPO == 6307 ~ "Engenharia Ambiental"))

# Transformando a variável CO_REGIAO_CURSO
ambiente = ambiente %>% mutate(REGIAO = case_when(CO_REGIAO_CURSO == 1 ~ "Norte",
                                                  CO_REGIAO_CURSO == 2 ~ "Nordeste",
                                                  CO_REGIAO_CURSO == 3 ~ "Sudeste",
                                                  CO_REGIAO_CURSO == 4 ~ "Sul",
                                                  CO_REGIAO_CURSO == 5 ~ "Centro-Oeste"))

# Transformando a variável QE_I02
ambiente = ambiente %>% mutate(RACA = case_when(QE_I02 == "A" ~ "Branca",
                                                QE_I02 == "B" ~ "Preta",
                                                QE_I02 == "C" ~ "Amarela",
                                                QE_I02 == "D" ~ "Parda",
                                                QE_I02 == "E" ~ "Indigena",
                                                QE_I02 == "F" ~ "Não quero declarar"))

# Transformando a variável CO_TURNO_GRADUACAO
ambiente = ambiente %>% mutate(TURNO = case_when(CO_TURNO_GRADUACAO == 1 ~ "Matutino",
                                                 CO_TURNO_GRADUACAO == 2 ~ "Vespertino",
                                                 CO_TURNO_GRADUACAO == 3 ~ "Integral",
                                                 CO_TURNO_GRADUACAO == 4 ~ "Noturno"))
View(ambiente)

# Removendo as variáveis antigas
ambiente = ambiente[-c(2,3,4,5)]

# Renomendo a variável nota
names(ambiente)[1] = "NOTAS"

# Checando a nova base
View(ambiente)

# Faça a estatística descritiva do seu banco de dados para ver se tem variáveis NA (missing)
ed = summary(ambiente)
ed
ambiente = ambiente %>% na.omit()
ed_sem_na = summary(ambiente)
ed_sem_na
View(ambiente)

# Estatística descritiva
media = mean(ambiente$NOTAS)
mediana = median(ambiente$NOTAS)
freq = table(ambiente$NOTAS)
max = max(freq)
min = min(freq)
nome = names(freq)
moda = nome[freq == max]
moda = as.numeric(moda)
amplitude = max(ambiente$NOTAS) - min(ambiente$NOTAS)
variancia = var(ambiente$NOTAS)
desvio = sd(ambiente$NOTAS)
cv = desvio / media
curtose = kurtosis(ambiente$NOTAS)
assimetria = skewness(ambiente$NOTAS)

resultados = c(media, mediana, moda, amplitude, variancia, desvio, cv, curtose,assimetria)
resultados

# Comentários
# Visto que a media é menor que a mediana(e a moda igual a mediana), parece que temos ama distribuição assimetrica
# A assimetria à esquerda, vista pelas medidas centrais, se confirmou pelo coeficiente de assimetria negativo (-0,167)
# A curtose menor que 0 (-0,33), indica que temos uma curva lentocurtica
# As medidas de dispersão, mostram que os dados estão mais proximos da hetereogeneidade
# O coeficiente de variacia mostra que em media os desvios padrão atingem em relação à media 36% do valor desta

# Grafico
hist_den = ggplot(ambiente, aes(x = NOTAS)) +
  geom_histogram(color = "black", fill = "purple", bins = 10) +
  geom_density(col = 2, size = 1, aes(y = 5 * ..count..)) +
  labs(title = "Histograma e curva de densidade das notas dos alunos de Engenharia Ambiental") +
  labs(x = "Notas", y = "frequencia")
hist_den
ggplotly(hist_den)

# O histograma mostra um comportamento de ilha, o que indica uma anomalidade nos dados, neste caso não houve nenhuma nota entre 50 e 60

# 6. Escolha uma das variáveis: Turnos (CO_TURNO_GRADUACAO) ou raça (QE_I02)e estude o comportamento das notas
# Primeira analise: Verificar a frequencia e proporção das variáveis
table(ambiente$TURNO)
prop.table(table(ambiente$TURNO))
prop.table(table(ambiente$TURNO)) * 100           

# Analisando apenas o turno das aulas, nota-se que a maior frequencia esta no periodo noturno
table(ambiente$TURNO, ambiente$REGIAO)
prop.table(table(ambiente$TURNO, ambiente$REGIAO)) * 100

# Ao agregar a região à analise nota-se que ainda no turno da noite a maior frequencia se dá na região sudeste
# Segunda analise: Notas por turno epor turno e região
nota_turno = ambiente %>% select(TURNO, NOTAS) %>%
  group_by(TURNO) %>%
  summarise(media = mean(NOTAS))
nota_turno

# Nota-se na tabela acima que os alunos do periodo integral possuem um melhoe desempenho em notas
nota_turno_reg = ambiente %>% select(TURNO, REGIAO, NOTAS) %>%
  group_by(TURNO, REGIAO) %>%
  summarise(media = mean(NOTAS))
nota_turno_reg

# Na analise feita nota-se que no periodo integral o pior desempenho região nordeste e melhor desempenho região sudeste
# em geral sudeste e sul é onde tem melhores desempenho das notas em todos os turnos
# Nordeste e norte é onde tem os piores desempenhos por notas.

nota_raca_reg = ambiente %>% select(RACA, REGIAO, NOTAS) %>%
  group_by(RACA, REGIAO) %>%
  summarise(media = mean(NOTAS))
nota_raca_reg

View(nota_raca_reg)




