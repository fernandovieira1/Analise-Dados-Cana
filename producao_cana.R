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

# 


# 1.2 Remover linhas com texto (chr) da coluna TON_HA ####
# - A coluna está toda em texto, mas alguns valores claramente são números.
# Precisamos decobrir quais são de fato character.
df %>%
  group_by(TON_HA) %>%
  summarise(frequencia = n()) %>% View()
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
# Qtde
df %>%
  count(UNIDADE)

# %
df %>%
  count(UNIDADE) %>%
  mutate(proporcao = n / sum(n))

## 2. TIPO
# Qtde
df %>%
  count(TIPO)

# %
df %>%
  count(TIPO) %>%
  mutate(proporcao = n / sum(n))

## 3. COD
# Qtde
df %>%
  count(COD)

# %
df %>%
  count(COD) %>%
  mutate(proporcao = n / sum(n))

## 4. SECAO
# Qtde
df %>%
  count(SECAO)

# %
df %>%
  count(SECAO) %>%
  mutate(proporcao = n / sum(n))

## 5. TALHAO
# Qtde
df %>%
  count(TALHAO)

# %
df %>%
  count(TALHAO) %>%
  mutate(proporcao = n / sum(n))

## 6. ESTAGIO
# Qtde
df %>%
  count(ESTAGIO)

# %
df %>%
  count(ESTAGIO) %>%
  mutate(proporcao = n / sum(n))

## 7. CORTE
# Qtde
df %>%
  count(CORTE)

# %
df %>%
  count(CORTE) %>%
  mutate(proporcao = n / sum(n))

## 8. VARIEDADE
# Qtde
df %>%
  count(VARIEDADE)

# %
df %>%
  count(VARIEDADE) %>%
  mutate(proporcao = n / sum(n))

## 9. MES_COLHEITA
# Qtde
df %>%
  count(MES_COLHEITA)

# %
df %>%
  count(MES_COLHEITA) %>%
  mutate(proporcao = n / sum(n))

## 10. PARCEIRO
# Qtde
df %>%
  count(PARCEIRO) %>%
  arrange(desc(n))

# %
df %>%
  count(PARCEIRO) %>%
  mutate(proporcao = n / sum(n)) %>%
  arrange(desc(n))



# Análise da variável TCH REAL (ton/ha)
df %>% 
  summarise(
    mean_TCH_REAL = mean(TCH_REAL, na.rm = TRUE),
    sd_TCH_REAL = sd(TCH_REAL, na.rm = TRUE),
    min_TCH_REAL = min(TCH_REAL, na.rm = TRUE),
    max_TCH_REAL = max(TCH_REAL, na.rm = TRUE)
  )

# Resumo das variáveis de interesse
summary(df)

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