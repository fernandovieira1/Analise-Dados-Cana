#### 0. CONFIGURAR O AMBIENTE ####
# 0.1 Carregar pacotes necessários ####
library(readxl)
library(tidyverse)
library(googledrive)
library(summarytools)
library(kableExtra)
library(psych)
library(officer)
library(ggcorrplot)
library(hrbrthemes)
library(viridis)

## 0.2 Importar dados e Criar df ####

# IMPORTANTE
# Definir o caminho local do arquivo
caminho_local <- 'C:/users/ferna/OneDrive/CAMINHO/arquivo_tal.xlsx'

# Verificar se está em nuvem ou local
if (dir.exists('/cloud')) {
  # Código para RStudio Cloud
  
  # ID do arquivo no Google Drive (PESQUISAR -- extraído do link)
  file_id <- '1M2ExnxtUy65xTsMENfAKAUCpQ2C93Dmx'
  
  # Baixar o arquivo e salvar localmente
  drive_download(as_id(file_id), path = 'arquivo.xlsx', overwrite = TRUE)
  # Ler o arquivo Excel do caminho atual (onde foi salvo no RStudio Cloud)
  df <- read_excel('arquivo.xlsx', 
                   sheet = 'Base de Dados')
  
} else {
  # Código para ambiente local (Windows ou Mac)
  # Ler o arquivo Excel
  df <- read_excel(caminho_local, 
                   sheet = 'Base de Dados')
}
1

## 0.3 Funções personalizadas ####
# Moda
# Função para calcular a moda
moda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


## 3. FORMATAR DF ####

# 3.1 Renomear colunas todas em minúscula
names(df)
df <- df %>% rename(safra=Safra,
                          mes=Mês,
                          unidade=Unidade,
                          tipo_propriedade=`Tipo Propriedade`,
                          percentual=`%`,
                          cargas_entregues = `Cargas Entregues`,
                          cargas_analisadas = `Cargas Analisadas`,
                          perc_analis_ton = `% Análise Ton`,
                          densidade_carga = `Densidade Carga`,
                          brix = `Brix`,
                          pol = Pol,
                          pureza = Pureza,
                          ar_caldo = `Ar Caldo`,
                          pc = PC,
                          fibra = `Fibra`,
                          agio = `Agio`,
                          atr = ATR,
                          tmp = TMP,
                          impureza = `Impureza`,
                          perc_broca = `% Broca`)


str(df) # tudo ok

## 4. DESCRITIVA ####

summary(df)
table(df$mes)
table(df$unidade)
table(df$tipo_propriedade)
