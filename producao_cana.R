#### 0. CONFIGURAR O AMBIENTE ####
# 0.1 Carregar pacotes necessários ####
library(readxl)
library(tidyverse)
library(googledrive)

## 0.2 Importar dados e Criar df ####
# Definir o caminho local do arquivo
file_path <- "C:/caminho/para/seu/arquivo.xlsx"

# Verificar se está em nuvem ou local
if (dir.exists("/cloud")) {
  # Código para RStudio Cloud
  
  # ID do arquivo no Google Drive (extraído do link)
  file_id <- "13iRzte02t6sh89sCWj4N7D5Y4GBzbEZI"
  
  # Baixar o arquivo e salvar localmente
  drive_download(as_id(file_id), path = "arquivo.xlsx", overwrite = TRUE)
  # Ler o arquivo Excel do caminho atual (onde foi salvo no RStudio Cloud)
  df <- read_excel('arquivo.xlsx', 
                   skip = 1,
                   sheet = 'BASE_2016_2023')
  # Selecionar colunas específicas e remover outras
  df <- df %>% select(-c('LAYER MAPA', 'VATR SAFRA', 'NM', 'Layer safra seção'))
  
} else {
  # Código para ambiente local (Windows ou Mac)
  # Ler o arquivo Excel
  df <- read_excel(file_path, 
                   skip = 1,
                   sheet = 'BASE_2016_2023')
  
  # Selecionar colunas específicas e remover outras
  df <- df %>% select(-c('LAYER MAPA', 'VATR SAFRA', 'NM', 'Layer safra seção'))
}
2

## 0.3 Funções personalizadas ####
# Moda
# Função para calcular a moda
moda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#### 1. FORMATAR DF ####

# 1.1 Renomear colunas ####
names(df)
df <- df %>% rename(COD = CÓD,
                    SECAO = SEÇÃO,
                    TALHAO = TALHÃO,
                    AREA = ÁREA,
                    ESTAGIO = ESTÁGIO,
                    PRODUCAO_REAL = `PRODUÇÃO REAL`,
                    TCH_REAL = `TCH REAL`,
                    ATR_PRODUCAO = `ATR * PRODUÇÃO`,
                    DATA_COLHEITA = `DATA DE COLHEITA`,
                    MES_COLHEITA = `MÊS DE COLHEITA`,
                    TON_HA = `TON/HÁ`,
                    KG_ATR = `KG ATR`)

# 


# 1.2 Remover linhas com texto (chr) da coluna TON_HA ####
# - A coluna está toda em texto, mas alguns valores claramente são números.
# Precisamos decobrir quais são de fato character.
# df %>%
#   group_by(TON_HA) %>%
#   summarise(frequencia = n()) %>% View()
# Há 4 valores, todos começando com a letra 'p'.

nrow(df) # antes da exclusão dos iniciados em p em TON_HA

df <- df %>%
  filter(!grepl('^p', TON_HA, ignore.case = TRUE))

nrow(df) # após a exclusão dos iniciados em p em TON_HA

# 1.3 Formatar colunas ####
df$SAFRA <- as.Date(as.character(df$SAFRA), format = "%Y")
df$TIPO <- as.factor(df$TIPO)
df$COD <- as.factor(df$COD)
df$TALHAO <- as.factor(df$TALHAO)
df$ESTAGIO <- as.factor(df$ESTAGIO)
df$CORTE <- as.factor(df$CORTE)
df$VARIEDADE <- as.factor(df$VARIEDADE)
df$MES_COLHEITA <- as.factor(df$MES_COLHEITA)
df$TON_HA <- as.double(df$TON_HA)
                    
# 1.3 Checar se as colunas estão bem formatadas
str(df)

#### 2. VISÃO PRELIMINAR ####
head(df)

# 2. Visão preliminar ####
# (Q): quantitativo
# (C): categórico

# (Q) SAFRA: Ano da safra
# (C) UNIDADE: Nome do local da produção/colheita
# (C) TIPO: Tipo de contrato
# (C) COD: Código da unidade de beneficiamento
# (C) SECAO: Unidade de beneficiamento
# (C) TALHAO: Porção de terra onde a cana foi plantada
# (Q) AREA: Tamanho da área plantada (ha)
# (C) ESTAGIO: (???)
# (C) CORTE: (???)
# (C) VARIEDADE: variedade genética da cana
# (Q) PRODUCAO_REAL: total colheita (kg)
# (Q) TCH_REAL: Tonelada de cana por hectare (kg)
# (Q) ATR: Açúcar Total Recuperável (kg)
# (Q) ATR_PRODUCAO: (PRODUÇÃO REAL*ATR) (kg)
# (Q) DATA_COLHEITA: AE
# (C) MES_COLHEITA: AE
# (C) PARCEIRO: Nome do produtor parceiro
# (Q) TON_HA: (???) ==> muitos valores repetidos
# (Q) KG_ATR: (???) ==> muitos valores repetidos

tail(df)

#### 3. AED ####
# 3.1 Dados categóricos ####

head(df)
## 1. UNIDADE: Nome do local da produção/colheita
## 2. TIPO: Tipo de contrato (Parceria ou Arrendamento)
## 3. COD: Código da unidade de beneficiamento
## 4. SECAO: Unidade de beneficiamento
## 5. TALHAO: Porção de terra onde a cana foi plantada
## 6. ESTAGIO: (???)
## 7. CORTE: (???)
## 8. VARIEDADE: variedade genética da cana
## 9. MES_COLHEITA: AE
## 10. PARCEIRO: Nome do produtor parceiro

## 1. UNIDADE
# Nome do local da produção/colheita

# Qtde
df %>%
  count(UNIDADE)
# %
df %>%
  count(UNIDADE) %>%
  mutate(proporcao = n / sum(n))

## 2. TIPO
# Tipo de contrato (Parceria ou Arrendamento)

# Qtde
df %>%
  count(TIPO)
# %
df %>%
  count(TIPO) %>%
  mutate(proporcao = n / sum(n))

## 3. COD
# Código da unidade de beneficiamento

# Qtde
df %>%
  count(COD)
# %
df %>%
  count(COD) %>%
  mutate(proporcao = n / sum(n))

## 4. SECAO
# Unidade de beneficiamento

# Qtde
df %>%
  count(SECAO)
# %
df %>%
  count(SECAO) %>%
  mutate(proporcao = n / sum(n))

## 5. TALHAO
# Porção de terra onde a cana foi plantada

# Qtde
df %>%
  count(TALHAO)
# %
df %>%
  count(TALHAO) %>%
  mutate(proporcao = n / sum(n))

## 6. ESTAGIO
# (???)

# Qtde
df %>%
  count(ESTAGIO)
# %
df %>%
  count(ESTAGIO) %>%
  mutate(proporcao = n / sum(n))

## 7. CORTE
# (???)

# Qtde
df %>%
  count(CORTE)
# %
df %>%
  count(CORTE) %>%
  mutate(proporcao = n / sum(n))

## 8. VARIEDADE
# variedade genética da cana

# Qtde
df %>%
  count(VARIEDADE)
# %
df %>%
  count(VARIEDADE) %>%
  mutate(proporcao = n / sum(n))

## 9. MES_COLHEITA
# AE

# Qtde
df %>%
  count(MES_COLHEITA)
# %
df %>%
  count(MES_COLHEITA) %>%
  mutate(proporcao = n / sum(n))

## 10. PARCEIRO
# Nome do produtor parceiro

# Qtde
df %>%
  count(PARCEIRO) %>%
  arrange(desc(n))
# %
df %>%
  count(PARCEIRO) %>%
  mutate(proporcao = n / sum(n)) %>%
  arrange(desc(n))

# 3.2 Dados quantitativos ####

head(df)
## 1. SAFRA: Ano da safra
## 2. AREA: Tamanho da área plantada (ha)
## 3. PRODUCAO_REAL: total colheita (kg)
## 4. TCH_REAL: Tonelada de cana por hectare (kg)
## 5. ATR: Açúcar Total Recuperável (kg)
## 6. ATR_PRODUCAO: (PRODUÇÃO REAL*ATR) (kg)
## 7. DATA_COLHEITA: AE
## 8. TON_HA: Toneladas por ha
## 9. KG_ATR: Quilos por ha

#Removi histogramas e boxplots, mas podem ser úteis (a ver)

## 1. SAFRA
# Ano da safra
df %>%
  count(SAFRA)
# %
df %>%
  count(SAFRA) %>%
  mutate(proporcao = n / sum(n))

## 2. AREA
# Tamanho da área plantada (ha)
summary(df$AREA)
moda(df$AREA)

df %>%
  group_by(SAFRA) %>%
  summarise(media_area = mean(AREA, na.rm = TRUE),
            total_area = sum(AREA, na.rm = TRUE))

## 3. PRODUCAO_REAL
# total colheita (kg)
summary(df$PRODUCAO_REAL)
moda(df$PRODUCAO_REAL)

df %>%
  group_by(TIPO) %>%
  summarise(media_prodReal_tipo = mean(PRODUCAO_REAL, na.rm = TRUE),
            soma_prodReal_tipo = sum(PRODUCAO_REAL, na.rm = TRUE))

## 4. TCH_REAL
# Tonelada de cana por hectare (kg)
summary(df$TCH_REAL)
moda(df$TCH_REAL)

df %>%
  group_by(SAFRA) %>%
  summarise(media_area = mean(AREA, na.rm = TRUE),
            total_area = sum(AREA, na.rm = TRUE))

## 5. ATR
# Açúcar Total Recuperável (kg)
summary(df$ATR)
moda(df$ATR)

df %>%
  group_by(SAFRA) %>%
  summarise(media_ATR = mean(ATR, na.rm = TRUE),
            total_ATR = sum(ATR, na.rm = TRUE))

df %>%
  group_by(PARCEIRO) %>%
  summarise(media_ATR = mean(ATR, na.rm = TRUE),
            total_ATR = sum(ATR, na.rm = TRUE))

## 6. ATR_PRODUCAO
# (PRODUÇÃO REAL*ATR) (kg)
summary(df$ATR_PRODUCAO)
moda(df$ATR_PRODUCAO)


## 7. DATA_COLHEITA
# AE

# - Ignorarei esta informação por enquanto, 
# considerando que fizemos a análise do mês da colheita em coluna especifica.


## 8. TON_HA
# Toneladas por ha
summary(df$TON_HA)
moda(df$TON_HA)

## 9. KG_ATR
# Quilos por ha
summary(df$KG_ATR)
moda(df$KG_ATR)

