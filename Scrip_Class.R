###############################################################################
##                                                                          ###
##  UNISINOS                                                                ###
##  Engenharia de Manutenção e Confiabilidade                               ###
##  Autor: Prof. Marcos Leandro Hoffmann Souza                              ###
##  Modelos de Confiabilidade                                               ###
##                                                                          ###
###############################################################################


## Setar o diretorio onde estão os arquivos


setwd(choose.dir(default = "", caption = "Selecione a pasta onde estão os arquivos que irá trabalhar"))


### Instalar os pacotes necessários

#install.packages("xlsx") # para este pacote funcionar, é necessário o java (jdk) instalado: https://www.oracle.com/technetwork/java/javase/downloads/index.html
#install.packages("survminer")
#install.packages("fitdistrplus")
#install.packages("SPREDA")
#install.packages("boot")
#install.packages("lattice")
#install.packages("survival")

### Carregar os pacotes

library(xlsx)
library(fitdistrplus)
library(SPREDA)
library(boot)
library(lattice)
library(survival)
library(survminer)

options(warn=-1) ## warnings (avisos sobre os pacotes, validade, modificações e etc..)

#### Coloque o nome do arquivo que deseja analisar ####
arquivo = "Dados_4.xlsx"


dados = read.xlsx(arquivo, sheetIndex = 1)


# Conhecendo o Shape dos dados 

plotdist(dados[,1], histo = TRUE, demp = TRUE)


### Encontrando o melhor ajustes das distribuições

df <- dados[,1]

###################################### Testar a distribuição Exponencial ##############################################
t1=fitdistr(df,"exponential")
t1.1=ks.test(df,"pexp",t1$estimate[1])
qqplot(qexp(ppoints(100),t1$estimate[1]), df, main="Exponencial", sub=bquote(lambda == .(signif(t1$estimate,2))), ylab="", xlab=paste("p-valor=", signif(t1.1$p.value,digits=5), ", MTBF=",round(mean(df),2)),type="l")
MTBF_Exp = round(mean(df),2)

## Tempo para verificar qual seria a confiabilidade #####                          
Tempo_Exp <- 12                                      

probfalha_Exp <- pexp(Tempo_Exp,t1$estimate[1])
conf_Exponencial <-paste(round((1-probfalha_Exp)*100,2), "% de confiabilidade para", sep= " ", Tempo_Exp, "horas")
conf_Exponencial


#################################### Testar a distribuição Weibull ###################################################
t2=fitdistr(df,"weibull")
t2.1=ks.test(df,"pweibull",t2$estimate[1],t2$estimate[2], alternative = "g")
qqplot(qweibull(ppoints(100),t2$estimate[1],t2$estimate[2]), df, main="Weibull", sub=substitute(paste(lambda,"=",t2.12,", ",theta,"=",t2.11),list(t2.11=signif(t2$estimate[2],2), t2.12=signif(t2$estimate[1],2))), ylab="", xlab=paste("p-valor=", signif(t2.1$p.value,digits=5),", MTBF=",round(t2$estimate[2] * gamma(1 + 1/t2$estimate[1]),2)),type="l")
MTBF_Wei= round(t2$estimate[2] * gamma(1 + 1/t2$estimate[1]),2)
## Tempo para verificar qual seria a confiabilidade #####                                

Tempo_Weibull <-1946

probfalha<- pweibull(Tempo_Weibull, t2$estimate[1], t2$estimate[2])
conf_Weibull <- paste(round((1- probfalha*1)*100,2),"% de confiabilidade para", sep= " ", Tempo_Weibull, "horas")
conf_Weibull


#################################### Testar a distribuição Lognormal #################################################
t4=fitdistr(df,"lognormal")
t4.1=ks.test(df,"plnorm",t4$estimate[1],t4$estimate[2], alternative = "g")
qqplot(qlnorm(ppoints(100),t4$estimate[1],t4$estimate[2]), df, main="Log-Normal", sub=substitute(paste(mu,"=",t4.11,", ",sigma,"=",t4.12),list(t4.11=signif(t4$estimate[1],2), t4.12=signif(t4$estimate[2],2))), ylab="", xlab=paste("p-valor=", signif(t4.1$p.value,digits=5),", MTBF =", round(exp((t4$estimate[1])+((t4$estimate[2])^2)/2),4)),type="l")
MTBF_LOG = round(exp((t4$estimate[1])+((t4$estimate[2])^2)/2),4)
## Tempo para verificar qual seria a confiabilidade #####     

Tempo_Lognormal <-60

probfalha<- plnorm(Tempo_Lognormal, t4$estimate[1], t4$estimate[2])
conf_LogNormal <- paste(round((1- probfalha*1)*100,2),"% de confiabilidade para", sep= " ", Tempo_Lognormal, "horas")
conf_LogNormal



################################### Plot da função de sobrevivência ###################################################

colnames(dados) <- c("Tempos")
dados$fail =c("F")
dados$fail <- ifelse(dados$fail=="S","T",as.character(dados$fail))
dados.dat <- data.frame(time=dados$Tempos,
                        event=1-as.numeric(as.logical(dados$fail)))

fit <- survfit(Surv(time)
               ~ event, data = dados.dat)



# Confiabilidade
ggsurvplot(fit, data = dados.dat)

# Taxa de Falha acumulada
ggsurvplot(fit, data = dados.dat, fun = "event")

# Taxa de risco acumulada.
ggsurvplot(fit, data = dados.dat, fun = "cumhaz")


######################### Simulação dos parâmetros da distribuição Weibull  #####################

fw <- fitdist(dados[,1], "weibull")

Boot <- bootdist(fw, niter = 5000) # O parâmetro 'niter' representa o número de rodadas a serem simuladas

summary(Boot)
plot(Boot)

quantile(Boot, probs = 0.05)

#################################################################################################################################################################################################
########################################################### Analisar o MTTR #############################################################################
########################################################################################################################################################


# Conhecendo o Shape dos dados de Reparo 

plotdist(dados[,2], histo = TRUE, demp = TRUE)


### Encontrando o melhor ajustes das distribuições

df <- dados[,2]

###################################### Testar a distribuição Exponencial ##############################################
t1=fitdistr(df,"exponential")
t1.1=ks.test(df,"pexp",t1$estimate[1])
qqplot(qexp(ppoints(100),t1$estimate[1]), df, main="Exponencial", sub=bquote(lambda == .(signif(t1$estimate,2))), ylab="", xlab=paste("p-valor=", signif(t1.1$p.value,digits=5), ", MTTR=",round(mean(df),2)),type="l")

#################################### Testar a distribuição Weibull ###################################################
t2=fitdistr(df,"weibull")
t2.1=ks.test(df,"pweibull",t2$estimate[1],t2$estimate[2], alternative = "g")
qqplot(qweibull(ppoints(100),t2$estimate[1],t2$estimate[2]), df, main="Weibull", sub=substitute(paste(lambda,"=",t2.12,", ",kappa,"=",t2.11),list(t2.11=signif(t2$estimate[2],2), t2.12=signif(t2$estimate[1],2))), ylab="", xlab=paste("p-valor=", signif(t2.1$p.value,digits=5),", MTTR=",round(t2$estimate[2] * gamma(1 + 1/t2$estimate[1]),2)),type="l")

#################################### Testar a distribuição Lognormal #################################################
t4=fitdistr(df,"lognormal")
t4.1=ks.test(df,"plnorm",t4$estimate[1],t4$estimate[2], alternative = "g")
qqplot(qlnorm(ppoints(100),t4$estimate[1],t4$estimate[2]), df, main="Log-Normal", sub=substitute(paste(mu,"=",t4.11,", ",sigma,"=",t4.12),list(t4.11=signif(t4$estimate[1],2), t4.12=signif(t4$estimate[2],2))), ylab="", xlab=paste("p-valor=", signif(t4.1$p.value,digits=5),", MTTR =", round(exp((t4$estimate[1])+((t4$estimate[2])^2)/2),4)),type="l")
MTTR = exp((t4$estimate[1])+((t4$estimate[2])^2)/2)


(Diponibilidade = paste("Disponibilidade de", round((MTBF_Wei /(MTBF_Wei + MTTR))*100,2),"%"))









