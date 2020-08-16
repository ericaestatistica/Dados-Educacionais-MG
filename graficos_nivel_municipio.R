# Muda diretorio

setwd('C:\\Users\\EricaCastilho\\Dropbox\\Recrutamentos\\Trybe\\Desafio\\microdados_saeb_2017\\DADOS')

# Carrega os pacotes
require(data.table)
require(ggplot2)
require(tidyr)
library(rgdal) 
library(plyr)

# Limpa memnoria
gc()


# Faz leitura dos dados e filtra apenas os do MG
dados_alunos_quinto<-fread('TS_ALUNO_5EF.csv')
dados_professor<-fread('TS_PROFESSOR.csv')

dados_alunos_quinto<-dados_alunos_quinto[ID_UF==31]
dados_professor<-dados_professor[ID_UF==31]



# Nota media dos alunos por municipio

nota_media_municipio<-dados_alunos_quinto[ID_UF==31,.(media_matematica=mean(PROFICIENCIA_MT_SAEB,na.rm=T),
                                                      media_portugues=mean(PROFICIENCIA_LP_SAEB,na.rm=T),
                                                      nmatematica=length(na.omit(PROFICIENCIA_MT_SAEB)),
                                                      nportugues=length(na.omit(PROFICIENCIA_LP_SAEB))),
                                          by=list(ID_MUNICIPIO)]


## Leitura da base do Salario professores
setwd('C:\\Users\\EricaCastilho\\Dropbox\\Recrutamentos\\Trybe\\Desafio\\Indicadores INEP')

salario_professores<-fread('salario_medio_professores.csv',sep=';',dec=',')

# Muda nome das colunas
colnames(salario_professores)[5]<-'Nome_Municipio'

colnames(salario_professores)[4]<-'ID_MUNICIPIO'

colnames(salario_professores)[7]<-'Escolaridade'

# Faz o merge com a base de notas medias
salario_professores<-merge(salario_professores,nota_media_municipio,all.x = T)

# Salva base em formato Rdata

setwd('C:\\Users\\EricaCastilho\\Dropbox\\Recrutamentos\\Trybe\\Desafio\\App_MG')

save(salario_professores,file='Resultados_por_municipio.Rdata')


######################################
## Mapas
######################################

# Leitura do Arquivo .shapeFile
mapa <- readOGR("C:\\Users\\EricaCastilho\\Dropbox\\IDeA - compartilhado Erica", "MG_municipios_pol")


# Processa mapa e junta com os dados dos municipios
mapa@data$id <- rownames(mapa@data)

colnames(mapa@data)[1]<-'ID_MUNICIPIO'
mapa@data   <- join(mapa@data, salario_professores[Escolaridade=='Total'], by="ID_MUNICIPIO")

mapa.df     <- fortify(mapa)
mapa.df     <- join(mapa.df,mapa@data, by="id")

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  plot.title = element_text(hjust = 0.5))


# Mapa salario medio
mapa.gg = ggplot(mapa.df, aes(x=long, y=lat, group=group))+
  geom_polygon(aes(fill=SalarioMedio), color = "black")+
  theme_bw()+
  ditch_the_axes+
  coord_fixed()+
  scale_fill_gradient2(low = "red", mid = "yellow", high = "chartreuse4",
                       midpoint = 4000, limit = c(0,8000), space = "Lab", 
                       name="Salário médio") 
mapa.gg

# Mapa Matematica
mapa.gg = ggplot(mapa.df, aes(x=long, y=lat, group=group))+
  geom_polygon(aes(fill=media_matematica), color = "black")+
  theme_bw()+
  ditch_the_axes+
  coord_fixed()+
  scale_fill_gradient2(low = "red", mid = "yellow", high = "chartreuse4",
                       midpoint = mean(mapa.df$media_matematica,na.rm = T), limit =range(mapa.df$media_matematica,na.rm=T), space = "Lab", 
                       name="Proficiência em \n Matemática") 
mapa.gg

# Mapa Portugues

mapa.gg = ggplot(mapa.df, aes(x=long, y=lat, group=group))+
  geom_polygon(aes(fill=media_portugues), color = "black")+
  theme_bw()+
  ditch_the_axes+
  coord_fixed()+
  #scale_fill_gradientn(colours=c("red", "yellow", "chartreuse4"),
  #values=rescale(c(min(mapa.df$Efeito),0, max(mapa.df$Efeito))), guide="colorbar",
  #na.value = NA, name = "Efeito Município") 
  scale_fill_gradient2(low = "red", mid = "yellow", high = "chartreuse4",
                       midpoint = mean(mapa.df$media_portugues,na.rm = T), limit =range(mapa.df$media_portugues,na.rm=T), space = "Lab", 
                       name="Proficiência em \n Língua Portuguesa") 
mapa.gg


