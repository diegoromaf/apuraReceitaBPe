#ESTACIONAMENTO
#* RESOLVER MERGE GIT
#resolver upload da base grande
#ha problema em algum sequenciamento?

#cleanEnv
cleanEnv <- function(){
  # Clean workspadyce
  rm(list=ls(.GlobalEnv), envir=.GlobalEnv)
  # Clear plots
  if(!is.null(dev.list()))
    dev.off();
  
  # Clear console
  cat("\014");
  # Set working directory
  setwd("~/coding/R/apuraReceitaBPe")
  return(getwd())
}

cleanEnv()

library(lubridate)

#preencheCampoDataEvento <- function(tipo_Doc){
#  select <- eventosReceitaBPe$TpDV == tipo_Doc
#  campo_Data = dataDocs[match(tipo_Doc,dataDocs[,"Documento"]),"Campo Data"]
#  eventosReceitaBPe[select,"dataEvento"] <- eventosReceitaBPe[select,campo_Data]
#  return(eventosReceitaBPe)
#}


## treats SAP S/4 ztsd033 table export
ztsd033 <- read.csv(
  "~/coding/R/BPe - RelReceita/ZTSD033.XLS",header = TRUE,
  sep="\t", fileEncoding='UTF-16LE',skip = 3,fill = TRUE )

#removing empty columns
ztsd033 <- Filter(function(x)!all(is.na(x)), ztsd033)

#sorting by IDServ & NroSequencia
ztsd033ordenado <- ztsd033[order(ztsd033$ID.referen,ztsd033$Nº.sequenc),]

#de-para de cod doc para campoData
docs <- c("ZYCT","ZYCV","ZYDS","ZYER","ZYES","ZYMO","ZYNO","ZYOS","ZYRE","ZYRO","ZYRS","ZYSP","ZYST","ZYV1","ZYV2","ZYVC")
campoData <- c("EVDAT","EVDAT","EVDAT","AUDAT","AUDAT","AUDAT","EVDAT","AUDAT","AUDAT","AUDAT","AUDAT","EVDAT","EVDAT","EVDAT","EVDAT","AUDAT")
tipoDocs <- c("Cancelamento","Cancelamento","Cancelamento","Embarque","Embarque","Venda Embarcada","N Embarque","Venda","Venda","Venda","Venda","Substituição","Substituição","Voucher","Voucher","Cancelamento")

# Take these vectors as input to the array.
dataDocs <- array(c(docs,campoData,tipoDocs),dim = c(16,3), 
                  dimnames = list(
                    c(1:16),
                    c("Documento","Campo Data","Tipo Doc"))
)


#seleciona Campo Data onde Documento é igual a ZYCT
#dataDocs[dataDocs[, "Documento"] == "ZYCT","Campo Data"]

#substitui EVDAT por Data.doc. e AUDAT por Data 
dataDocs[dataDocs[, "Campo Data"] == "EVDAT","Campo Data"] <- "Data"
dataDocs[dataDocs[, "Campo Data"] == "AUDAT","Campo Data"] <- "Data.doc."

receitaBPe <- array(dim = c(12,4,4), 
                    dimnames = list(
                      c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez"),
                      c("embarcarAnterior","emitidoAtual","embarcadoAtual","embarcarFuturo"),
                      c("2018","2019","2020","2021")
                    )
)




eventosReceitaBPe <- data.frame(
  as.integer(ztsd033ordenado$ID.Serviço),
  ztsd033ordenado$TpDV,
  ztsd033ordenado$Empr,   
  as.integer(ztsd033ordenado$Nº.sequenc),
  ztsd033ordenado$Cen.,
  dmy(ztsd033ordenado$Data.doc.),
  dmy(ztsd033ordenado$Data),
  as.integer(ztsd033ordenado$ID.referen),
  ztsd033ordenado$Chave.de.acesso.de.44.posições, 
  dmy(ztsd033ordenado$DataEmbarq),
  dmy(ztsd033ordenado$Validade),
  as.numeric(gsub (",", ".", ztsd033ordenado$Desconto)),
  as.numeric(gsub (",", ".", ztsd033ordenado$Valor.Tota)),
  dmy(ztsd033ordenado$Data)#,
) 


colnames(eventosReceitaBPe)<-(c(names(ztsd033ordenado)[1],names(ztsd033ordenado)[2],names(ztsd033ordenado)[3],names(ztsd033ordenado)[5],names(ztsd033ordenado)[6],names(ztsd033ordenado)[7],names(ztsd033ordenado)[8],names(ztsd033ordenado)[9],names(ztsd033ordenado)[10],names(ztsd033ordenado)[14],names(ztsd033ordenado)[15],names(ztsd033ordenado)[28],names(ztsd033ordenado)[30],"dataEvento"))


largest_less_than<-function(x,y){
  which(x == max(x[x < y]))
}


for (Documento in docs) {
  select <- eventosReceitaBPe$TpDV == Documento
  campo_Data = dataDocs[match(Documento,dataDocs[,"Documento"]),"Campo Data"]
  eventosReceitaBPe[select,"dataEvento"] <- eventosReceitaBPe[select,campo_Data]
}


#  select <- eventosReceitaBPe$TpDV == tipo_Doc
#  campo_Data = dataDocs[match(tipo_Doc,dataDocs[,"Documento"]),"Campo Data"]
#  eventosReceitaBPe[select,"dataEvento"] <- eventosReceitaBPe[select,campo_Data]
#  return(eventosReceitaBPe)


separaUltimoEventoBilhete<-function(IDRef, eventos, output){
  select <- eventos$ID.referen == IDRef
  
  output <- c(output, c(eventosReceitaBPe$ID.Serviço[select]))
}

#eventosReceitaBPe$dataEvento[1]>dmy("04.07.2019")
#c(EventosPeriodo,itemNovo)




#CRIAR SUBVETOR COM OS EVENTOS QUE INTERESSAM PARA AQUELE PERIODO
#MaxDataEvento para aquele ZZIDSERVOR



#forma 1:
#selecionar eventos que tenham dataEvento <= periodoAtual

#para cada ZZIDSERVOR
#pegar doc do máximo Nro.sequencia
#identificar tipo documento
#Cancelamento
#Substituição
#Venda
#Embarque
#No Show
#Voucher







#criar aqui vetor com o subcojunto dos eventos do zzidservor atual
#Campos necessários: ID Serviço	TpDV	Empr	Nº sequenc	Cen.	Data doc.	Data	ID referen	
#Chave de acesso de 44 posições	DataEmbarq	Validade	Desconto	Valor Tota	

#1 Integer as.integer(ztsd033ordenado$ID.Serviço)                               1
#2 Char ztsd033ordenado$TpDV                                                    2
#3 Char ztsd033ordenado$Empr                                                    3
#4 Integer as.integer(ztsd033ordenado$Nº.sequenc)                               5
#5 Char ztsd033ordenado$Cen.                                                    6
#6 Data ztsd033ordenado$Data.doc.                                               7
#7 Data ztsd033ordenado$Data                                                    8
#8 Integer as.integer(ztsd033ordenado$ID.referen)                               9
#9 Char ztsd033ordenado$Chave.de.acesso.de.44.posições                          10
#10 Data ztsd033ordenado$DataEmbarq                                             14
#11 Data ztsd033ordenado$Validade                                               15
#12 Real as.numeric (gsub (",", ".", ztsd033ordenado$Desconto))                 28
#13 Real as.numeric (gsub (",", ".", ztsd033ordenado$Valor.Tota))               30
#14 Data dataEvento ztsd033ordenado$(dataDocs[=ztsd033ordenado$TpDV,CampoData]  7&8


#para cada ZZIDSERVORAtual in ztsd033ordenado.ZZIDSERVOR
#eventosBilheteAtual = ztsd033ordenado$ZZIDSERVOR=ZZIDSERVORAtual)
#para cada eventosBilheteAtual
#identificaUltimoEventoPeriodoAtual = maxLinha onde mes(dataEvento)=mesAtual
#identificaUltimoEventoPeriodoAnterior = maxLinha onde mes(dataEvento)=mesAtual-1
#SE NAO ENCONTRA NO PERIODO ATUAL, JA CRAVA COMO ANTERIOR
#identifica tipo evento ultimoEventoPeriodoAtual
#    : Substituição -> Data Embarque TEM QUE SER > periodoAtual +EMBARCAR PERÍODO FUTURO; +EMITIDO?
#    : Venda -> Data Embarque TEM QUE SER > periodoAtual +EMBARCAR PERIODO FUTURO; + EMITIDO
#    : Embarque -> embarcadoPeriodoAtual
#    : No Show -> dataEmbarque(ou validade?) > `a embarcar periodos futuros





#DUVIDA SE PRECISA IDENTIFICAR ULTIMO EVENTO PERIODO ANTERIOR
#acredito que nao precisa
#sera tao simples quanto
#a embarcar periodos anteriores
#bilhetes como ultimo event atual Venda, No SHow, ou substituição com dataEvento<periodoAtual
#Emitido Periodo atual
#blhetes como ultimo evento atual uma venda ou substituição com dataEvento=periodoAtual
#Embarcado Periodo Atual
#bilhetes como ultimo evento atual um embarque
#A embarcar periodos futuros
#bilhetes como eventoatual venda e substituição com dataEvento <= periodoAtual 


#identifica tipo evento ultimoEventoPeriodoAnterior
#    : Substituição -> Data Embarque TEM QUE SER = periodoAtual `A EMBARCAR PERÍODOS ANTERIORES
#    : Venda -> Data Embarque TEM QUE SER > periodoAtual EMBARCAR PERIODO FUTURO
#    : No Show -> dataEmbarque(ou validade?) > periodoAtual 


#DUVIDA NA CONTA DA SUBSTITUIÇÃO

#como fazer a separacao dos eventos com relacao ao mes em questao
#como seperar os docs atuais e passados quando estiver olhhando  o zzidservor?

#criar subvetor com os eventos a serem tratados naquele mês
#e sempre tratar o vetor como um todo.
