# Importando base de dados
library("tidyverse")

enem <- read.csv2("MICRODADOS_ENEM_2023.csv", dec = ".")

glimpse(enem)

# Transformando em tibble
enem <- as_tibble(enem)
enem

# Limpando
micro_data_fltr <- enem %>%  filter(!is.na(CO_PROVA_LC)) %>%  
  select(TP_ESCOLA, NU_NOTA_CN, NU_NOTA_CH, NU_NOTA_LC, NU_NOTA_MT,
         TX_RESPOSTAS_CH, TX_RESPOSTAS_CN, TX_RESPOSTAS_LC, TX_RESPOSTAS_MT, TP_LINGUA,
         TX_GABARITO_CH, TX_GABARITO_CN, TX_GABARITO_LC, TX_GABARITO_MT)

# Funçãozinha pra ajudar a achar o total de acertos
contar_iguais <- function(string1, string2) {
  # Transformar as strings em vetores de caracteres
  vetor1 <- str_split(string1, "", simplify = TRUE)
  vetor2 <- str_split(string2, "", simplify = TRUE)
  
  # Encontrar o menor comprimento para evitar erros de comparação
  tamanho <- min(length(vetor1), length(vetor2))
  
  # Comparar as letras em cada posição
  igual_count <- sum(vetor1[1:tamanho] == vetor2[1:tamanho])
  
  return(igual_count)
}

############################ HUMANAS ###########################################

# Contando os acertos
micro_data_fltr <- micro_data_fltr %>% 
  rowwise() %>% 
  mutate(acertos_humanas = contar_iguais(TX_RESPOSTAS_CH, TX_GABARITO_CH)) %>% 
  ungroup()

# Tabela de resumo
humanas <- micro_data_fltr %>% 
  group_by(acertos_humanas) %>% 
  summarise(
    minino = min(NU_NOTA_CH, na.rm = TRUE),
    media = mean(NU_NOTA_CH, na.rm = TRUE),
    maximo = max(NU_NOTA_CH, na.rm = TRUE),
    quantidade_de_pessoas = n(),
  )


############################## Matemática ######################################

# Contando os acertos
micro_data_fltr <- micro_data_fltr %>% 
  rowwise() %>% 
  mutate(acertos_matematica = contar_iguais(TX_RESPOSTAS_MT, TX_GABARITO_MT)) %>% 
  ungroup()

# Tabela de resumo
matematica <- micro_data_fltr %>% 
  group_by(acertos_matematica) %>% 
  summarise(
    minimo = min(NU_NOTA_MT, na.rm = TRUE),
    media = mean(NU_NOTA_MT, na.rm = TRUE),
    maximo = max(NU_NOTA_MT, na.rm = TRUE),
    quantidade_de_pessoas = n()
  )


################################ Natureza ######################################

# Contando os acertos
micro_data_fltr <- micro_data_fltr %>% 
  rowwise() %>% 
  mutate(acertos_natureza = contar_iguais(TX_RESPOSTAS_CN, TX_GABARITO_CN)) %>% 
  ungroup()

# Tabela de resumo
natureza <- micro_data_fltr %>% 
  group_by(acertos_natureza) %>% 
  summarise(
    minimo = min(NU_NOTA_CN, na.rm = TRUE),
    media = mean(NU_NOTA_CN, na.rm = TRUE),
    maximo = max(NU_NOTA_CN, na.rm = TRUE),
    quantidade_de_pessoas = n()
  )

################################## Linguagem ###################################

# Removendo as questões estrangeiras extras
micro_data_fltr <- micro_data_fltr %>% 
  mutate(TX_GABARITO_LC = ifelse(TP_LINGUA == 0,
                                 str_remove(TX_GABARITO_LC, str_sub(TX_GABARITO_LC, 6, 10)),
                                 str_sub(TX_GABARITO_LC, 6, 50)))

# Filtrando os alunos que escolheram ingles
micro_data_ingles <- micro_data_fltr %>% 
  select(NU_NOTA_LC, TX_RESPOSTAS_LC, TP_LINGUA, TX_GABARITO_LC) %>% 
  filter(TP_LINGUA == 0)

# Filtrando os alunos que escolheram espanhol
micro_data_espanhol <- micro_data_fltr %>% 
  select(NU_NOTA_LC, TX_RESPOSTAS_LC, TP_LINGUA, TX_GABARITO_LC) %>% 
  filter(TP_LINGUA == 1)

# Contando os acertos
micro_data_ingles <- micro_data_ingles %>% 
  rowwise() %>% 
  mutate(acertos_linguagem = contar_iguais(TX_RESPOSTAS_LC, TX_GABARITO_LC)) %>% 
  ungroup()

# Tabela de resumo
linguagens_ingles <- micro_data_ingles %>% 
  group_by(acertos_linguagem) %>% 
  summarise(
    minimo = min(NU_NOTA_LC, na.rm = TRUE),
    media = mean(NU_NOTA_LC, na.rm = TRUE),
    maximo = max(NU_NOTA_LC, na.rm = TRUE),
    quantidade_de_pessoas = n()
  )

# Contando os acertos
micro_data_espanhol <- micro_data_espanhol %>% 
  rowwise() %>% 
  mutate(acertos_linguagem = contar_iguais(TX_RESPOSTAS_LC, TX_GABARITO_LC)) %>% 
  ungroup()

# Tabela de resumo
linguagens_espanhol <- micro_data_espanhol %>% 
  group_by(acertos_linguagem) %>% 
  summarise(
    minimo = min(NU_NOTA_LC, na.rm = TRUE),
    media = mean(NU_NOTA_LC, na.rm = TRUE),
    maximo = max(NU_NOTA_LC, na.rm = TRUE),
    quantidade_de_pessoas = n()
  )

#################################### EXPORTAÇÂO ################################

write_csv2(humanas, "humanas.csv")
write_csv2(linguagens_espanhol, "linguagens_espanhol.csv")
write_csv2(linguagens_ingles, "linguagens_ingles.csv")


########################## segundo desafio #####################################

# Criando as colunas de resposta e gabarito sem as linguas estrangeiras
micro_data_ingles <- micro_data_ingles %>% 
  mutate(gabarito_sem = str_sub(TX_GABARITO_LC, 6, 45))

micro_data_espanhol <- micro_data_espanhol %>% 
  mutate(gabarito_sem = str_sub(TX_GABARITO_LC, 6, 45))

micro_data_ingles <- micro_data_ingles %>% 
  mutate(resposta_sem = str_sub(TX_RESPOSTAS_LC, 6, 45))

micro_data_espanhol <- micro_data_espanhol %>% 
  mutate(resposta_sem = str_sub(TX_RESPOSTAS_LC, 6, 45))


# Verificando quais gabaritos são comuns para os que escolheram ingles e espanhol 
em_comum <- intersect(micro_data_ingles$resposta_sem, micro_data_espanhol$resposta_sem)
em_comum

# Testando alguns gabaritos pra comparar ingles com espanhol
teste_ingles <- micro_data_ingles %>% 
  filter(resposta_sem == "DCBABCDEDCBABCDEDCBABCDEDCBABCDEDCBABCDE") %>% 
  rowwise() %>% 
  mutate(acertos = contar_iguais(TX_RESPOSTAS_LC, TX_GABARITO_LC)) %>% 
  ungroup()

teste_espanhol <- micro_data_espanhol %>% 
  filter(resposta_sem == "DCBABCDEDCBABCDEDCBABCDEDCBABCDEDCBABCDE") %>% 
  rowwise() %>% 
  mutate(acertos = contar_iguais(TX_RESPOSTAS_LC, TX_GABARITO_LC)) %>% 
  ungroup()

teste_ingles2 <- micro_data_ingles %>% 
  filter(resposta_sem == "AABCCBEAACABABEACBBAACDEAEDADCDBEEDDECCE") %>% 
  rowwise() %>% 
  mutate(acertos = contar_iguais(TX_RESPOSTAS_LC, TX_GABARITO_LC)) %>% 
  ungroup()

teste_espanhol2 <- micro_data_espanhol %>% 
  filter(resposta_sem == "AABCCBEAACABABEACBBAACDEAEDADCDBEEDDECCE") %>% 
  rowwise() %>% 
  mutate(acertos = contar_iguais(TX_RESPOSTAS_LC, TX_GABARITO_LC)) %>% 
  ungroup()

teste_ingles3 <- micro_data_ingles %>% 
  filter(resposta_sem == "CAEAEDCACBBACABAAACAAAECBBEDBCCADEDEDDAC") %>% 
  rowwise() %>% 
  mutate(acertos = contar_iguais(TX_RESPOSTAS_LC, TX_GABARITO_LC)) %>% 
  ungroup()

teste_espanhol3 <- micro_data_espanhol %>% 
  filter(resposta_sem == "CAEAEDCACBBACABAAACAAAECBBEDBCCADEDEDDAC") %>% 
  rowwise() %>% 
  mutate(acertos = contar_iguais(TX_RESPOSTAS_LC, TX_GABARITO_LC)) %>% 
  ungroup()

teste_ingles4 <- micro_data_ingles %>% 
  filter(resposta_sem == "EDCBAEDCBAEDCBAEDCBAEDCBAEDCBAEDCBAEDCBA") %>% 
  rowwise() %>% 
  mutate(acertos = contar_iguais(TX_RESPOSTAS_LC, TX_GABARITO_LC)) %>% 
  ungroup()

teste_espanhol4 <- micro_data_espanhol %>% 
  filter(resposta_sem == "EDCBAEDCBAEDCBAEDCBAEDCBAEDCBAEDCBAEDCBA") %>% 
  rowwise() %>% 
  mutate(acertos = contar_iguais(TX_RESPOSTAS_LC, TX_GABARITO_LC)) %>% 
  ungroup()

teste_ingles5 <- micro_data_ingles %>% 
  filter(resposta_sem == "CCEEEDEACBBACAACAACAAAECBBEDBCCADBDEDDAC") %>% 
  rowwise() %>% 
  mutate(acertos = contar_iguais(TX_RESPOSTAS_LC, TX_GABARITO_LC)) %>% 
  ungroup()

teste_espanhol5 <- micro_data_espanhol %>% 
  filter(resposta_sem == "CCEEEDEACBBACAACAACAAAECBBEDBCCADBDEDDAC") %>% 
  rowwise() %>% 
  mutate(acertos = contar_iguais(TX_RESPOSTAS_LC, TX_GABARITO_LC)) %>% 
  ungroup()

# Juntando ingles e espanhol para cada gabarito e depois ordenando os acertos
junta <- rbind(teste_ingles, teste_espanhol) %>% 
  arrange(acertos)

junta2 <- rbind(teste_ingles2, teste_espanhol2) %>% 
  arrange(acertos)

junta3 <- rbind(teste_ingles3, teste_espanhol3) %>% 
  arrange(acertos)

junta4 <- rbind(teste_ingles4, teste_espanhol4) %>% 
  arrange(acertos)

junta5 <- rbind(teste_ingles5, teste_espanhol5) %>% 
  arrange(acertos)

############################## Exportando ###################################

write_csv2(junta, "gabarito1.csv")
write_csv2(junta2, "gabarito2.csv")
write_csv2(junta3, "gabarito3.csv")
write_csv2(junta4, "gabarito4.csv")
write_csv2(junta5, "gabarito5.csv")


########################### Gráficos ###########################################

myColors <- c("#fc5a03", "#010161", "#6de64c")


install.packages("ggthemes")
library(ggthemes)

matematica %>% ggplot(aes(x = acertos_matematica, y = media)) +
  geom_line(aes(col = "Média")) +
  geom_line(aes(y = minimo, col = "Minima")) +
  geom_line(aes(y = maximo, col = "Máxima")) +
  labs(title = "ENEM 2023",
       subtitle = "Notas dos alunos na área de matemática",
       x = "Quantidade de acertos",
       y = "Pontuação",
       color = "Notas") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  scale_color_manual(values = myColors)


natureza %>% ggplot(aes(x = acertos_natureza, y = media)) +
  geom_line(aes(col = "Média")) +
  geom_line(aes(y = minimo, col = "Minima")) +
  geom_line(aes(y = maximo, col = "Máxima")) +
  labs(title = "ENEM 2023",
       subtitle = "Notas dos alunos na área de natureza",
       x = "Quantidade de acertos",
       y = "Pontuação",
       color = "Notas") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  scale_color_manual(values = myColors)


humanas %>% ggplot(aes(x = acertos_humanas, y = media)) +
  geom_line(aes(col = "Média")) +
  geom_line(aes(y = humanas$minino, col = "Minima")) +
  geom_line(aes(y = maximo, col = "Máxima")) +
  labs(title = "ENEM 2023",
       subtitle = "Notas dos alunos na área de humanas",
       x = "Quantidade de acertos",
       y = "Pontuação",
       color = "Notas") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  scale_color_manual(values = myColors)


linguagens_espanhol %>% ggplot(aes(x = acertos_linguagem, y = media)) +
  geom_line(aes(col = "Média")) +
  geom_line(aes(y = minimo, col = "Minima")) +
  geom_line(aes(y = maximo, col = "Máxima")) +
  labs(title = "ENEM 2023",
       subtitle = "Notas dos alunos que escolheram espanhol na área de linguagem",
       x = "Quantidade de acertos",
       y = "Pontuação",
       color = "Notas") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  scale_color_manual(values = myColors)


linguagens_ingles %>% ggplot(aes(x = acertos_linguagem, y = media)) +
  geom_line(aes(col = "Média")) +
  geom_line(aes(y = minimo, col = "Minima")) +
  geom_line(aes(y = maximo, col = "Máxima")) +
  labs(title = "ENEM 2023",
       subtitle = "Notas dos alunos que escolheram ingles na área de linguagem",
       x = "Quantidade de acertos",
       y = "Pontuação",
       color = "Notas") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  scale_color_manual(values = myColors)
