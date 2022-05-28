library(lubridate)
library(magrittr)
library(ggplot2)
library(tidyverse)
library(tidytext)
####$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#### Read exported messages of Telegram  ##############
####$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
ler_html_telegram <- function(html_file) {
  # pega todas as mensagens
  divs <- xml2::read_html(html_file) %>% 
    xml2::xml_find_all("//div[@class='message default clearfix']")
  # nome da pessoa
  nomes <- divs %>% 
    xml2::xml_find_all("./div/div[@class='from_name']") %>% 
    xml2::xml_text() %>% 
    stringr::str_squish()
  # data e hora da mensagem
  data_horas <- divs %>% 
    xml2::xml_find_all("./div/div[@class='pull_right date details']") %>% 
    xml2::xml_attr("title") %>% 
    lubridate::dmy_hms()
  # texto da mensagem
  textos <- divs %>% 
    purrr::map(xml2::xml_find_first, "./div/div[@class='text']") %>% 
    purrr::map_chr(xml2::xml_text) %>% 
    stringr::str_squish()
  # retorna numa tabela
  tibble::tibble(
    data_hora = data_horas,
    nome = nomes,
    texto = textos
  )
}

## Escolhar o caminho onde estão as mensagens em HTML do Telegram
## exemplo
path0 <- "I:\\ChatExport_16_05_2020"
path1 <- "I:\\"
path2 <- "I:\\"
path3 <- "I:\\"
### Lendo as Mensagens
todos_arquivos0 <- fs::dir_ls(path0, regexp = "messages")
todos_arquivos1 <- fs::dir_ls(path1, regexp = "messages")
todos_arquivos2 <- fs::dir_ls(path2, regexp = "messages")
todos_arquivos3 <- fs::dir_ls(path3, regexp = "messages")
### df 16-05-2020 
d_msg16_05_2020 <- purrr::map_dfr(
  todos_arquivos0, 
  ler_html_telegram, 
  .id = "arquivo")
### df 26-05-2020 
d_msg26_05_2020 <- purrr::map_dfr(
  todos_arquivos1, 
  ler_html_telegram, 
  .id = "arquivo")
### df 05-07-2020
d_msg05_07_2020 <- purrr::map_dfr(
  todos_arquivos2, 
  ler_html_telegram, 
  .id = "arquivo")
### df 07-08-2020
d_msg07_08_2020 <- purrr::map_dfr(
  todos_arquivos3, 
  ler_html_telegram, 
  .id = "arquivo")
########## Salvando em.Rdata para não ter que ler os HTML's todo santo dia 
save("d_msg16_05_2020", file = "data\\ChatExport_16_05_2020\\d_msg16052020.RData")
save("d_msg26_05_2020", file = "data\\ChatExport_26_05_2020\\d_msg26052020.RData")
save("d_msg07_08_2020", file = "data\\ChatExport_07_08_2020\\d_msg07082020.RData")

#################################################################
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
#                                                               #    
#                          Comece daqui pra baixo!              #
#                                                               #  
#****************************************************************
load("data\\ChatExport_16_05_2020\\d_msg16052020.RData")
load("data\\ChatExport_26_05_2020\\d_msg26052020.RData")
load("data\\ChatExport_05_07_2020\\d_msg05072020.RData")
load("data\\ChatExport_07_08_2020\\d_msg07082020.RData")
load("data\\ChatExport_24_09_2020\\d_msg24092020.RData")
load("data\\ChatExport_05_01_2021\\d_msg05012021.RData")
################################################
### Base de Dados Total (20xx-20xx) ############
################################################

#### IMPORTANT: Merge all dfs and keep erased posts

df_final <- bind_rows(d_msg16_05_2020,
                      d_msg26_05_2020, 
                      d_msg05_07_2020, 
                      d_msg07_08_2020, 
                      d_msg24_09_2020,
                      d_msg05_01_2021, .id = NULL)

d_msg_distinct <- df_final %>% 
  distinct(data_hora, .keep_all = TRUE)

#### INSERINDO COLUNA COM Datas ###################

data <- as.Date(as.POSIXct(d_msg_distinct$data_hora))
d_msg_PUB_NA <- d_msg_distinct %>% 
  mutate(data = data)

#### Limpando a memória de DFs ###############

rm(data,
   d_msg16_05_2020, 
   d_msg26_05_2020, 
   d_msg05_07_2020, 
   d_msg07_08_2020, 
   d_msg_distinct,
   df_final)


$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  #########                                      ####
######### FINAL: ESCOLHA O PERÍODO DA ANÁLISE  ####
#########                                      ####
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
  d_msg_PUB_NA <- d_msg_PUB_NA %>%
  ## Abaixo, por exemplo, está o filtro para o ano de 2020
  filter(data <= as.Date("2020-06-30") & data > as.Date("2018-08-11"))
## Abaixo, TUDO ANTES DE 31/12/2020
filter(data < as.Date("2021-01-01"))


###########################################
### OPTIONAL: Invisibilizar os users  $$$$$
##########################################
library("digest")

## cria uma lista com os nomes dos users
lista <- d_msg_PUB_NA$nome
## Aplicando un hash para cada um (sapply) dos usuários únicos unique()
unicos <- sapply(unique(lista), digest)
## converto vetor em df
unicos <- data.frame(unicos)
## criando uma coluna chamada id com as hashs
colnames(unicos) <- "id"
## criando uma coluna chamada nomes com os nomes
unicos$nome <- row.names(unicos)
## data frame final com a coluna id
d_msg_PUB_NA_HASH <- left_join(d_msg_PUB_NA, unicos, by = "nome")

### remover tudo e deixar apenas d_msg_PUB_NA_HASH
rm(lista, unicos, d_msg_PUB_NA)

# Salvando
saveRDS(d_msg_PUB_NA_HASH, "d_msg_PUB_NA_HASH.rds")

# Restore it
d_msg_PUB_NA_HASH <- readRDS("d_msg_PUB_NA_HASH.rds")