# Social Network Analytics - Twitter
# Obtendo palavras relacioanadas ao tema "COVID"


# Carrega biblioteca com functions auxiliares e de autenticação no twitter - onde as credenciais ficam
source('utils.R')
source('autentication.R')

# Carregando os pacotes
library(devtools)
library(twitteR)
library(ROAuth)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(stringr)

# autenticando a seção do Twitter
create_oauth()

# Coletando os tweets por tema
tweets.covid19 = searchTwitter("covid", n = 2000, lang = "pt")

# Obtendo tweets por usuário
tweets.cnn = userTimeline(getUser('cnnbrasil'), n = 2000)

# Convertendo os tweets para texto
textos.covid19 = sapply(tweets.covid19, function(x) x$getText())
textos.cnn  = sapply(tweets.cnn, function(x) x$getText())

# tratamento para os tweets 
textos.covid19_limpo <- limpaTweets(textos.covid19_limpo)
textos.covid19_limpo = textos.covid19_limpo[textos.covid19_limpo != ""]
textos.cnn_limpo <- limpaTweets(textos.cnn)
textos.cnn_limpo = textos.cnn_limpo[textos.cnn_limpo != ""]


# Converte para Corpus
tweetcorpus_covid19 <- Corpus(VectorSource(textos.covid19_limpo))
tweetcorpus_covid19 <- limpaCorpus(tweetcorpus_covid19)

tweetcorpus_cnn <- Corpus(VectorSource(textos.cnn_limpo))
tweetcorpus_cnn <- limpaCorpus(tweetcorpus_cnn)


# Salvando o Corpus para continuar análise posteriormente
writeCorpus(x = tweetcorpus_covid19, path = "dados/CorpusCOVID")
writeCorpus(x = tweetcorpus_cnn, path = "dados/CorpusCNN")

# Lendo o Corpus ....
# 1. Pastas de origem dos arquivos gravados anteriormente:
current.folder <- paste0(getwd(), "/dados/")

# 2. Gerando o Corpus Volátil
tweetcorpus_covid19 <- VCorpus(DirSource(directory = paste0(current.folder, "CorpusCOVID"),
                                     pattern = "txt"), 
                          readerControl = list(reader = readPlain, 
                                               language = "UTF-8"))
tweetcorpus_cnn <- VCorpus(DirSource(paste0(current.folder, "CorpusCNN"),
                                     pattern = "txt"),
                           readerControl = list(reader = readPlain,
                                                language = "ASCII"))

# Converte o texto para a matriz de termos
require(vctrs)
vstopwords <- vec_c(stopwords(kind = "pt"), c("que", "por", "com", "para", "nao",
                               "esta", "como", "pra", "pela", "sem", "apos", "neste",
                               "esse", "mais", "des", "uma", "dizia", "vai", "desta",
                               "meu", "bem", "muito", "essa", "ter", "senhor", "sobre"))

termo_doc_covid19 = as.matrix(TermDocumentMatrix(tweetcorpus_covid19,
                                                 control = list(stopwords = vstopwords)))


termo_doc_cnn = as.matrix(TermDocumentMatrix(tweetcorpus_cnn,
                                             control = list(stopwords = vstopwords)))

# Verifica os primeiros 10 termos (linhas) com os primeiros 5 documentos (colunas)
termo_doc_covid19[1:10,1:5]
termo_doc_cnn[1:10, 1:5]

# Calcula a frequência de cada termo ao somar cada linha e coloca em ordem decrescente
frequencia_dos_termos_covid19 = sort(rowSums(termo_doc_covid19), decreasing = T)
frequencia_dos_termos_cnn = sort(rowSums(termo_doc_cnn), decreasing = TRUE)

# Cria um dataframe com o termo (palavra) e sua respectiva frequência 
df_covid19 = data.frame(termo = names(frequencia_dos_termos_covid19), 
                        frequencia = frequencia_dos_termos_covid19)

df_cnn = data.frame(termo = names(frequencia_dos_termos_cnn),
                    frequencia = frequencia_dos_termos_cnn)

# Remove so termos mais frequente (o usado para pesquisa no tweeter ou irrelevantes para o resultado final)
df_covid19 = df_covid19[-1,]
df_cnn = df_cnn[which(df_cnn$termo != "cnn" & df_cnn$termo != "feira"),]

# Salvando os datasets:
write.csv(x = df_covid19, file = "dados/df_tweets_covid19.csv")
write.csv2(x = df_cnn, file = "dados/df_tweets_cnn.csv")


# Merge dos dataframes df_cnn e df_covid
df_cnn_covid <- merge(x = df_cnn, y = df_covid19, 
                      by = "termo", all = TRUE)

# Removendo as colunas duplicadas
df_cnn_covid = subset(df_cnn_covid, select = -c(X.x, X.y))

# Verificando e tratando dados missing
summary(is.na(df_cnn_covid[c('frequencia.x', 'frequencia.y')]))
# frequencia.x    frequencia.y   
# Mode :logical   Mode :logical  
# FALSE:1848      FALSE:747      
# TRUE :490       TRUE :1591  

df_cnn_covid$frequencia.x[is.na(df_cnn_covid$frequencia.x)] <- 0
df_cnn_covid$frequencia.y[is.na(df_cnn_covid$frequencia.y)] <- 0

# Checando se ainda existem dados missings... (resultado esperado é ZERO)
sum(rowSums(is.na(df_cnn_covid)))
df_cnn_covid$frequencia <- df_cnn_covid$frequencia.x + df_cnn_covid$frequencia.y

# Removendo algumas outras palavras ...
`%notin%` <- Negate(`%in%`)
x <- df_cnn_covid[df_cnn_covid$termo %notin% c("estao", "quer", "ate", "pode", "ser", "ritm"),]

wordcloud(words = x$termo,
          freq = x$frequencia,
          max.words = 100,
          min.freq = 10,
          random.order = F,
          colors = brewer.pal(8, "PuOr"))
