# Muda diretorio
setwd('C:\\Users\\EricaCastilho\\Dropbox\\Recrutamentos\\Trybe\\Desafio\\microdados_saeb_2017\\DADOS')

# Carrega os pacotes
require(data.table)
require(ggplot2)
require(tidyr)

# Limpa memnoria
gc()

# Carrega banco de dados
dados_alunos_quinto<-fread('TS_ALUNO_5EF.csv')
dados_professor<-fread('TS_PROFESSOR.csv')
dados_escolas<-fread('TS_ESCOLA.csv')


#Filtra apenas o alunos de minas
dados_alunos_quinto<-dados_alunos_quinto[ID_UF==31]

###########################################
# Grafico de proficiencia por Gênero
##########################################

# Recodifica a variavavel 
dados_alunos_quinto$TX_RESP_Q001<-factor(dados_alunos_quinto$TX_RESP_Q001)
levels(dados_alunos_quinto$TX_RESP_Q001)<-c(NA,'Masculino','Feminino')

# Matematica
ggplot(dados_alunos_quinto[!is.na(TX_RESP_Q001),],aes(x=TX_RESP_Q001,y=PROFICIENCIA_MT_SAEB,
                                                                fill=TX_RESP_Q001))+
  geom_boxplot()+theme_minimal()+ylab('Proficiência em Matemática')+xlab('Gênero')+theme(legend.position = "none")

# Lingua Portuguesa
ggplot(dados_alunos_quinto[ID_UF==31 & !is.na(TX_RESP_Q001),],aes(x=TX_RESP_Q001,y=PROFICIENCIA_LP_SAEB,
                                                                  fill=TX_RESP_Q001))+
  geom_boxplot()+theme_minimal()+ylab('Proficiência em Língua Portuguesa')+xlab('Gênero')+theme(legend.position = "none")


###########################################
# Grafico de proficiencia por Raça
##########################################

# Recodifica a variavavel 
dados_alunos_quinto$TX_RESP_Q002<-factor(dados_alunos_quinto$TX_RESP_Q002)
levels(dados_alunos_quinto$TX_RESP_Q002)<-c(NA,'Branca','Preta','Parda',
                                            'Amarelo','Índigena','Não declarado')

# Matematica
ggplot(dados_alunos_quinto[TX_RESP_Q002=='Branca' | TX_RESP_Q002=='Preta'|
                             TX_RESP_Q002=='Parda',],aes(x=TX_RESP_Q002,y=PROFICIENCIA_MT_SAEB,
                                                      fill=TX_RESP_Q002))+
  geom_boxplot()+theme_minimal()+ylab('Proficiência em Matemática')+xlab('Raça')+theme(legend.position = "none")

# Lingua Portuguesa
ggplot(dados_alunos_quinto[TX_RESP_Q002=='Branca' | TX_RESP_Q002=='Preta'|
                             TX_RESP_Q002=='Parda',,],aes(x=TX_RESP_Q002,y=PROFICIENCIA_LP_SAEB,
                                                                  fill=TX_RESP_Q002))+
  geom_boxplot()+theme_minimal()+ylab('Proficiência em Língua Portuguesa')+xlab('Raça')+theme(legend.position = "none")

###########################################
# Grafico de proficiencia por Reprovação
##########################################

# Recodifica a variavavel 
dados_alunos_quinto$TX_RESP_Q045<-factor(dados_alunos_quinto$TX_RESP_Q045)
levels(dados_alunos_quinto$TX_RESP_Q045)<-c(NA,'Não','Sim, uma vez','Sim, duas vezes ou mais')

# Matematica
ggplot(dados_alunos_quinto[!is.na(TX_RESP_Q045),],aes(x=TX_RESP_Q045,y=PROFICIENCIA_LP_SAEB,
                                                          fill=TX_RESP_Q045))+
  geom_boxplot()+theme_minimal()+ylab('Proficiência em Língua Portuguesa')+xlab('Reprovação')+theme(legend.position = "none")


# Lingua Portuguesa
ggplot(dados_alunos_quinto[!is.na(TX_RESP_Q045),],aes(x=TX_RESP_Q045,y=PROFICIENCIA_MT_SAEB,
                                                      fill=TX_RESP_Q045))+
  geom_boxplot()+theme_minimal()+ylab('Proficiência em Matemática')+xlab('Reprovação')+theme(legend.position = "none")


#################################################
# Grafico de proficiencia por Escolaridade da mae
#################################################

# Recodifica a variavavel 
dados_alunos_quinto$TX_RESP_Q019<-factor(dados_alunos_quinto$TX_RESP_Q019)
levels(dados_alunos_quinto$TX_RESP_Q019)<-
  c(NA,'Nunca estudou.','Não completou a 4.ª série/5.º ano.',
    'Completou a 4.ª série/5.º ano, mas não completou a 8.ª série/9.º ano.',
    'Completou a 8.ª série/9.º ano, mas não completou o Ensino Médio.',	
    'Completou o Ensino Médio, mas não completou a Faculdade.',	
    'Completou a Faculdade.',NA)


# Matematica
ggplot(dados_alunos_quinto[!is.na(TX_RESP_Q019),],aes(x=TX_RESP_Q019,y=PROFICIENCIA_MT_SAEB,
                                                      fill=TX_RESP_Q019))+
  geom_boxplot()+theme_minimal()+ylab('Proficiência em Matemática')+
  xlab('')+theme(axis.title.x=element_blank(),
                 axis.text.x=element_blank(),
                 axis.ticks.x=element_blank())+labs(fill = "Escolaridade da mãe")


# Lingua Portuguesa
ggplot(dados_alunos_quinto[!is.na(TX_RESP_Q019),],aes(x=TX_RESP_Q019,y=PROFICIENCIA_LP_SAEB,
                                                      fill=TX_RESP_Q019))+
  geom_boxplot()+theme_minimal()+ylab('Proficiência em Língua Portuguesa')+
  xlab('')+theme(axis.title.x=element_blank(),
                 axis.text.x=element_blank(),
                 axis.ticks.x=element_blank())+labs(fill = "Escolaridade da mãe")

#################################################
# Grafico de proficiencia por Escolaridade do pai
#################################################

# Recodifica a variavavel 

dados_alunos_quinto$TX_RESP_Q023<-factor(dados_alunos_quinto$TX_RESP_Q023)
levels(dados_alunos_quinto$TX_RESP_Q023)<-
  c(NA,'Nunca estudou.','Não completou a 4.ª série/5.º ano.',
    'Completou a 4.ª série/5.º ano, mas não completou a 8.ª série/9.º ano.',
    'Completou a 8.ª série/9.º ano, mas não completou o Ensino Médio.',	
    'Completou o Ensino Médio, mas não completou a Faculdade.',	
    'Completou a Faculdade.',NA)




# Matematica
ggplot(dados_alunos_quinto[!is.na(TX_RESP_Q023),],aes(x=TX_RESP_Q023,y=PROFICIENCIA_MT_SAEB,
                                                      fill=TX_RESP_Q023))+
  geom_boxplot()+theme_minimal()+ylab('Proficiência em Matemática')+
  xlab('')+theme(axis.title.x=element_blank(),
                 axis.text.x=element_blank(),
                 axis.ticks.x=element_blank())+labs(fill = "Escolaridade do pai")



# Lingua Portuguesa

ggplot(dados_alunos_quinto[!is.na(TX_RESP_Q023),],aes(x=TX_RESP_Q023,y=PROFICIENCIA_LP_SAEB,
                                                      fill=TX_RESP_Q023))+
  geom_boxplot()+theme_minimal()+ylab('Proficiência em Língua Portuguesa')+
  xlab('')+theme(axis.title.x=element_blank(),
                 axis.text.x=element_blank(),
                 axis.ticks.x=element_blank())+labs(fill = "Escolaridade do pai")





#################################################
# Grafico de proficiencia por Incentivo dos pais
#################################################


# Recodifica a variavavel 

dados_alunos_quinto$TX_RESP_Q027<-factor(dados_alunos_quinto$TX_RESP_Q027)
levels(dados_alunos_quinto$TX_RESP_Q027)<-c(NA,'Sim','Não')


# Matematica
ggplot(dados_alunos_quinto[!is.na(TX_RESP_Q027),],aes(x=TX_RESP_Q027,y=PROFICIENCIA_MT_SAEB,
                                                      fill=TX_RESP_Q027))+
  geom_boxplot()+theme_minimal()+ylab('Proficiência em Matemática')+
  xlab('Seus pais ou responsáveis incentivam você a estudar?')+theme(legend.position = "none")

# Lingua Portuguesa
ggplot(dados_alunos_quinto[!is.na(TX_RESP_Q027),],aes(x=TX_RESP_Q027,y=PROFICIENCIA_LP_SAEB,
                                                      fill=TX_RESP_Q027))+
  geom_boxplot()+theme_minimal()+ylab('Proficiência em Língua Portuguesa')+
  xlab('Seus pais ou responsáveis incentivam você a estudar?')+theme(legend.position = "none")

