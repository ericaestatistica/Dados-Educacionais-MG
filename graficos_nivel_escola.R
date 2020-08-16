
# Muda diretorio
setwd('C:\\Users\\EricaCastilho\\Dropbox\\Recrutamentos\\Trybe\\Desafio\\microdados_saeb_2017\\DADOS')

# Carrega os pacotes
require(data.table)
require(ggplot2)
require(tidyr)

# Limpa memnoria
gc()

# Faz leitura dos dados e filtra apenas os do MG
dados_alunos_quinto<-fread('TS_ALUNO_5EF.csv')
dados_alunos_quinto<-dados_alunos_quinto[ID_UF==31,]

dados_professor<-fread('TS_PROFESSOR.csv')
dados_professor<-dados_professor[ID_UF==31,]

dados_escolas<-fread('TS_ESCOLA.csv')
dados_escolas<-dados_escolas[ID_UF==31,]

# Calcula nota media dos alunos por escola em cada municipio
nota_media_escola<-dados_alunos_quinto[,.(media_matematica=mean(PROFICIENCIA_MT_SAEB,na.rm=T),
                                           media_portugues=mean(PROFICIENCIA_LP_SAEB,na.rm=T),
                                           nmatematica=length(na.omit(PROFICIENCIA_MT_SAEB)),
                                           nportugues=length(na.omit(PROFICIENCIA_LP_SAEB))),
                                           by=list(ID_ESCOLA,ID_MUNICIPIO)]

# Estatisticas dos alunos por escola
estatisticas_aluno_escola<-dados_alunos_quinto[ID_UF==31,.(porcentagem_pretos=sum(TX_RESP_Q002=='B',na.rm=T)/length(na.omit(TX_RESP_Q002)),
                                                  porcentagem_reprovados=sum((TX_RESP_Q045=='B'|TX_RESP_Q045=='C'),na.rm=T)/length(na.omit(TX_RESP_Q045)),
                                                  porcentagem_mae_EM=sum((TX_RESP_Q019=='E'|TX_RESP_Q019=='F'),na.rm=T)/length(na.omit(TX_RESP_Q019)),
                                                  porcentagem_pai_EM=sum((TX_RESP_Q023=='E'|TX_RESP_Q023=='F'),na.rm=T)/length(na.omit(TX_RESP_Q023))),
                                               by=list(ID_ESCOLA,ID_MUNICIPIO)]

# Faz o merge entre as duas bases

estatisticas_aluno_escola<-merge(estatisticas_aluno_escola,nota_media_escola,all.x=T)


# Faz o merge com a base das escolas
estatisticas_aluno_escola<-merge(estatisticas_aluno_escola,
                                 dados_escolas[,c('ID_MUNICIPIO','ID_ESCOLA','PC_FORMACAO_DOCENTE_INICIAL',
                                                           'NIVEL_SOCIO_ECONOMICO')])

##############################
# Indicadores INEP
#############################


# Muda diretorio
setwd('C:\\Users\\EricaCastilho\\Dropbox\\Recrutamentos\\Trybe\\Desafio\\Indicadores INEP')


## Hora aula
horas_aula<-fread('horas_aula.csv',dec=',',sep=';')
horas_aula[,c('ID_ESCOLA','Horas_aula')]

estatisticas_aluno_escola<-merge(estatisticas_aluno_escola,
                                 horas_aula[,c('ID_ESCOLA','Horas_aula')],all.x = T,
                                 by='ID_ESCOLA')


## Alunos por turma
alunos_turma<-fread('alunos_turma.csv',dec=',',sep=';')
alunos_turma[,c('ID_ESCOLA','Alunos_turma')]

estatisticas_aluno_escola<-merge(estatisticas_aluno_escola,
                                 alunos_turma[,c('ID_ESCOLA','Alunos_turma')],all.x = T,
                                 by='ID_ESCOLA')


## Regularidade corpo docente
regularidade_corpo_docente<-fread('regularidade_corpo_docente.csv',dec=',',sep=';')
regularidade_corpo_docente[,c('ID_ESCOLA','IRD')]

estatisticas_aluno_escola<-merge(estatisticas_aluno_escola,
                                 regularidade_corpo_docente[,c('ID_ESCOLA','IRD')],all.x = T,
                                 by='ID_ESCOLA')



## Distorcao Idade Serie
distorcao_idade_serie<-fread('distorcao_idade_serie.csv',dec=',',sep=';')
distorcao_idade_serie[,c('ID_ESCOLA','Distorcao_idade_serie')]

estatisticas_aluno_escola<-merge(estatisticas_aluno_escola,
                                 distorcao_idade_serie[,c('ID_ESCOLA','Distorcao_idade_serie')],all.x = T,
                                 by='ID_ESCOLA')


# Salva base de dados como um objeto do tipo Rdata
setwd('C:\\Users\\EricaCastilho\\Dropbox\\Recrutamentos\\Trybe\\Desafio\\App_MG')

save(estatisticas_aluno_escola,file='Resultados_por_escola.Rdata')
