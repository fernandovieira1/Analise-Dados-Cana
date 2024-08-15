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
caminho_local <- 'C:\\Users\\ferna\\OneDrive\\5. Trabalho\\Expediente\\Ativos\\Consultoria\\Usina Pedra\\Docs Nicolella\\4-Dados e documentos\\Dados\\Usina da Pedra\\Historico de Moagem 2016 - 2023.xlsx'

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

# TC por mes e por tipo_propriedade
resumo_tc_mes_tipoProp <- df %>%
  group_by(TC, mes, tipo_propriedade)

resumo_tc_mes_tipoProp

summary(resumo_tc_mes_tipoProp)
table(resumo_tc_mes_tipoProp$mes)
table(resumo_tc_mes_tipoProp$unidade)
table(resumo_tc_mes_tipoProp$tipo_propriedade)

# Produção TC por unidade
ggplot(resumo_tc_mes_tipoProp, aes(x = unidade, y = TC, fill = unidade)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c('#4D4D4D', '#737373', '#A6A6A6', '#CCCCCC', '#E6E6E6')) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = 'Soma de TC por Unidade',
       x = 'Unidade',
       y = 'TC (Toneladas de Cana)') +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    axis.title.x = element_text(face = 'bold'),
    axis.title.y = element_text(face = 'bold'),
    legend.position = 'none'
  )

# Produção TC por mes e por tipo_propriedade
ggplot(resumo_tc_mes_tipoProp, aes(x = mes, y = TC, fill = tipo_propriedade)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values = c('#4D4D4D', '#737373', '#A6A6A6', '#CCCCCC', '#E6E6E6')) +
  facet_wrap(~ tipo_propriedade, ncol = 1) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = 'Distribuição de TC por Mês e Tipo de Propriedade',
       x = 'Mês',
       y = 'TC (Toneladas de Cana)') +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    axis.title.x = element_text(face = 'bold'),
    axis.title.y = element_text(face = 'bold')
  )

# Distribuição de TC por unidade 
ggplot(resumo_tc_mes_tipoProp, aes(x = unidade, y = TC, fill = unidade)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  scale_fill_manual(values = c("#4D4D4D", "#737373", "#A6A6A6", "#CCCCCC", "#E6E6E6")) +
  scale_y_continuous(labels = scales::comma) +  # Formata o eixo y com separadores de milhares
  labs(title = "Distribuição de TC por Unidade",
       x = "Unidade",
       y = "TC (Toneladas de Cana)") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "none"
  )
