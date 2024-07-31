#### 0. CONFIGURAR O AMBIENTE ####
# 0.1 Carregar pacotes necessários ####
library(readxl)
library(tidyverse)
library(googledrive)
#drive_auth()

# ID do arquivo no Google Drive (extraído do link)
file_id <- "13iRzte02t6sh89sCWj4N7D5Y4GBzbEZI"

# Baixar o arquivo e salvar localmente
drive_download(as_id(file_id), path = "arquivo.xlsx", overwrite = TRUE)
2
# 0.3 Importar dados e Criar df ####
df <- read_excel('arquivo.xlsx', 
                 skip = 1,
                 sheet = 'BASE_2016_2023')
df <- df %>% select(-c('LAYER MAPA', 'VATR SAFRA', 'NM', 'Layer safra seção'))

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

# 1.2 Formatar colunas ####
df$SAFRA <- as.Date(as.character(df$SAFRA), format = "%Y")
df$TALHAO <- as.factor(df$TALHAO)
df$ESTAGIO <- as.factor(df$ESTAGIO)
df$CORTE <- as.factor(df$CORTE)
df$VARIEDADE <- as.factor(df$VARIEDADE)
df$MES_COLHEITA<- as.factor(df$MES_COLHEITA)
# df$TON_HA <- as.double(as.character(df$SAFRA))
## Consertar TON_HA
                    
# 1.3 Visão preliminar via do df
# Inicio e fim
head(df)
tail(df)

# Checar se as colunas estão bem formatadas
str(df) # PAREI AQUI

# Resumo das variáveis de interesse
summary(df)

# Análise da variável TCH REAL (ton/ha)
df %>% 
  summarise(
    mean_TCH_REAL = mean(TCH_REAL, na.rm = TRUE),
    sd_TCH_REAL = sd(TCH_REAL, na.rm = TRUE),
    min_TCH_REAL = min(TCH_REAL, na.rm = TRUE),
    max_TCH_REAL = max(TCH_REAL, na.rm = TRUE)
  )

# Histograma e boxplot de TCH REAL
ggplot(df, aes(x = TCH_REAL)) +
  geom_histogram(bins = 50, fill = 'blue', alpha = 0.7) +
  geom_density(color = 'red') +
  xlim(0, 250)

ggplot(df, aes(y = TCH_REAL)) +
  geom_boxplot(fill = 'blue', alpha = 0.7)

# TCH REAL por SAFRA
df %>%
  group_by(SAFRA) %>%
  summarise(mean_TCH_REAL = mean(TCH_REAL, na.rm = TRUE)) %>%
  ggplot(aes(x = SAFRA, y = mean_TCH_REAL)) +
  geom_line() +
  geom_point()

# Análise da variável ATR (kg)
df %>% 
  summarise(
    mean_ATR = mean(ATR, na.rm = TRUE),
    sd_ATR = sd(ATR, na.rm = TRUE),
    min_ATR = min(ATR, na.rm = TRUE),
    max_ATR = max(ATR, na.rm = TRUE)
  )

# Histograma e boxplot de ATR
ggplot(df, aes(x = ATR)) +
  geom_histogram(bins = 50, fill = 'green', alpha = 0.7) +
  geom_density(color = 'red')

ggplot(df, aes(y = ATR)) +
  geom_boxplot(fill = 'green', alpha = 0.7)

# ATR por SAFRA
df %>%
  group_by(SAFRA) %>%
  summarise(mean_ATR = mean(ATR, na.rm = TRUE)) %>%
  ggplot(aes(x = SAFRA, y = mean_ATR)) +
  geom_line() +
  geom_point()

# Análise da variável ATR * PRODUÇÃO (kg)
df %>% 
  summarise(
    mean_ATR_PRODUCAO = mean(ATR_PRODUCAO, na.rm = TRUE),
    sd_ATR_PRODUCAO = sd(ATR_PRODUCAO, na.rm = TRUE),
    min_ATR_PRODUCAO = min(ATR_PRODUCAO, na.rm = TRUE),
    max_ATR_PRODUCAO = max(ATR_PRODUCAO, na.rm = TRUE)
  )

# Histograma e boxplot de ATR * PRODUÇÃO
ggplot(df, aes(x = ATR_PRODUCAO)) +
  geom_histogram(bins = 50, fill = 'purple', alpha = 0.7) +
  geom_density(color = 'red')

ggplot(df, aes(y = ATR_PRODUCAO)) +
  geom_boxplot(fill = 'purple', alpha = 0.7)

# ATR * PRODUÇÃO por SAFRA
df %>%
  group_by(SAFRA) %>%
  summarise(mean_ATR_PRODUCAO = mean(ATR_PRODUCAO, na.rm = TRUE)) %>%
  ggplot(aes(x = SAFRA, y = mean_ATR_PRODUCAO)) +
  geom_line() +
  geom_point()

# Resumo final
# Mudar para dbl
df %>% 
  summarise(
    mean_TON_HA = mean(TON_HA, na.rm = TRUE),
    sd_TON_HA = sd(TON_HA, na.rm = TRUE),
    min_TON_HA = min(TON_HA, na.rm = TRUE),
    max_TON_HA = max(TON_HA, na.rm = TRUE),
    mean_KG_ATR = mean(TON_HA, na.rm = TRUE),
    sd_KG_ATR = sd(TON_HA, na.rm = TRUE),
    min_KG_ATR = min(TON_HA, na.rm = TRUE),
    max_KG_ATR = max(TON_HA, na.rm = TRUE)
  )

# Histograma e boxplot de TON/HÁ e KG ATR
ggplot(df, aes(x = TON_HA)) +
  geom_histogram(bins = 50, fill = 'orange', alpha = 0.7) +
  geom_density(color = 'red')

ggplot(df, aes(y = TON_HA)) +
  geom_boxplot(fill = 'orange', alpha = 0.7)

ggplot(df, aes(x = KG_ATR)) +
  geom_histogram(bins = 50, fill = 'pink', alpha = 0.7) +
  geom_density(color = 'red')

ggplot(df, aes(y = KG_ATR)) +
  geom_boxplot(fill = 'pink', alpha = 0.7)

# config cloud