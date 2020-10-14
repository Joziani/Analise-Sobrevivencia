# UNIVERSIDADE FEDERAL DE OURO PRETO
# TRABALHO PRÁTICO DE ANÁLISE DE SOBREVIVÊNCIA
# BANCO DE DADOS TABAGISMO
# GRUPO: ANDRESSA FREITAS, IARA MENDES, JOZIANI MOTA, SAMUEL SABINO
# PROFESSOR: RIVERT PAULO BRAGA OLIVEIRA

# CARREGANDO OS PACOTES EXIGIDOS -----------------------------------------------

options(max.print=5.5E5) 

load <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 

packages <- c("survival",
              "ggfortify",
              "gridExtra",
              "survminer",
              "tidyverse",
              "ranger")
load(packages)


# IMPORTANDO A BASE DE DADOS "TABAGISMO" ---------------------------------------

tabagismo <- read.csv("tabagismo.txt", header = T,
                      sep = " ", 
                      encoding = "UTF-8")


# VISUALIZANDO OS DADOS E SUA ESTRUTURA ----------------------------------------

head(tabagismo)
#View(tabagismo)
str(tabagismo)


# TRANSFORMANDO E LIMAPANDO OS DADOS -------------------------------------------

# Convertendo as variaveis id e status para o tipo factor

tabagismo <- tabagismo %>% 
  mutate(
    grp = as.factor(grp),
    gender = as.factor(gender),
    race = as.factor(race),
    levelSmoking = as.factor(levelSmoking)
  )

str(tabagismo)

# Verificando valores missing

sapply(tabagismo, function(x) sum(is.na(x)))


# Verificando a relação entre as variáveis -------------------------------------

  ## Idade

tabagismo <- tabagismo %>% 
  mutate(idadec = cut(age, c(0,30,50,100),
                      labels = c("Jovem","Adulto","Idoso")))

summary(tabagismo$idadec)

ekm1 <- survfit(Surv(time,status) ~ idadec, 
                data = tabagismo, 
                conf.type=c("log-log"))

summary(ekm1)

## Gráfico de Probabilidade de sobrevivência

ggsurvplot(ekm1, data = tabagismo, 
           pval = T, 
           conf.int = T,
           legend.title = 'Faixa Et?ria: ',
           xlab = 'Tempo (dias)',
           ylab = expression(hat(S[km])(t)),
           surv.median.line = 'hv',
           legend.labs = c("Jovem", "Adulto", "Idoso"))


pairwise_survdiff(Surv(time,status) ~ idadec,
                  data = tabagismo)

# Pessoas idosas (com mais de 51 anos) parecem a ter menos propensão a 
# reincidência ao fumo com relação às pessoas jovens e adultas.


  ## Gênero 

summary(tabagismo$gender)

ekm2 <- survfit(Surv(time,status)~gender,
                data = tabagismo,
                conf.type=c("log-log"))

summary(ekm2)

## Gráfico de Probabilidade de sobrevivência

ggsurvplot(ekm2, data = tabagismo,
           pval = T, 
           conf.int = T,
           legend.title = 'G?nero: ',
           xlab = 'Tempo (dias)',
           ylab = expression(hat(S[km])(t)),
           surv.median.line = 'hv',
           legend.labs = c("Feminino", "Masculino"))

# Pessoas do sexo masculino parecem demorar um pouco mais para ter reincidência,
# após 50 dias, com relação às mulheres.


  ## Raça 

summary(tabagismo$race)

ekm3 <- survfit(Surv(time,status)~race,
                data = tabagismo,
                conf.type=c("log-log"))

summary(ekm3)

## Gráfico de Probabilidade de sobrevivência

ggsurvplot(ekm3, data = tabagismo,
           pval = T, 
           conf.int = T,
           legend.title = 'Ra?a: ',
           xlab = 'Tempo (dias)',
           ylab = expression(hat(S[km])(t)),
           surv.median.line = 'hv',
           legend.labs = c("Negra", "Hisp?nica", "Outra", "Branca"))

pairwise_survdiff(Surv(time,status) ~ race, 
                  data = tabagismo)

tabagismo$race

# Não existe diferença significativa entre as raças. 


## Período de tratamento 

summary(tabagismo$employment)

ekm4 <- survfit(Surv(time,status)~employment,
                data=tabagismo,
                conf.type=c("log-log"))

summary(ekm4)

## Gráfico de Probabilidade de sobrevivência

ggsurvplot(ekm4, data = tabagismo,
           pval = T,
           conf.int = T,
           legend.title = 'Per?odo de tempo: ',
           xlab = 'Tempo (dias)',
           ylab = expression(hat(S[km])(t)),
           surv.median.line = 'hv',
           legend.labs = c("Integral", "Outros", "Parcial"))

pairwise_survdiff(Surv(time,status) ~ employment, 
                  data = tabagismo)

# Não parece haver diferença significativa entre os períodos de tratamento.


# Grupos de tratamento

ekm <- survfit(Surv(time,status)~grp,
               data=tabagismo,
               conf.type=c("log-log"))
summary(ekm)

## Gráfico de Probabilidade de sobrevivência

ggsurvplot(ekm, data = tabagismo,
           pval = T,
           conf.int = T,
           legend.title = 'Tratamentos: ',
           xlab = 'Tempo (dias)',
           ylab = expression(hat(S[km])(t)),
           surv.median.line = 'hv',
           legend.labs = c("Combina??o", "Adesivo"))

# Utilizando a estimação de Kaplan-Meier e observando o gráfico, parece haver 
# diferença entre os tempos de vida dos dois grupos. O gráfico mostra que, 
# pessoas que fazem a combinação de 3 tratamentos tendem a ter uma 
# probabilidade menor de reincidência ao fumo se comparado ao grupo de pessoas
# que só utilizam o tratamento com adesivo.


# Aplicando Análise de Sobrevivência -------------------------------------------


# Modelos Paramétricos ---------------------------------------------------------

tabagismo$time[tabagismo$time==0]=0.00000001

# Os tempos at? a reincid?ncia foram

tempos=tabagismo$time
censura=tabagismo$status

# Ajuste exponencial

ajust1=survreg(Surv(tempos,censura)~1,dist="exponential")
ajust1
alpha=exp(ajust1$coefficients[1])# motivo no cap?tulo 4
alpha

# Ajuste weibull

ajust2=survreg(Surv(tempos,censura)~1,dist="weibull")
ajust2
alphaw=exp(ajust2$coefficients[1])# motivo no cap?tulo 4
alphaw
gama=1/ajust2$scale
gama

# Ajuste lognormal

ajust3=survreg(Surv(tempos,censura)~1,dist="lognorm")
ajust3
mu=ajust3$coefficients[1]
mu
sigma=ajust3$scale
sigma

# Ajuste via Kaplan-Meyer

ajustkm=survfit(Surv(tempos,censura)~1)
ajustkm

# organizando as estimativas de cada ajuste numa tabela

time=ajustkm$time
stkm<-ajustkm$surv
ste<-exp(-time/alpha)
stw<-exp(-(time/alphaw)^gama)
stln<-pnorm((-log(time)+mu)/sigma)
stall=as.data.frame(cbind(time,stkm,ste,stw,stln))
stall
riscacum=stall
riscacum[,2:5]=-log(riscacum[,2:5])
names(riscacum)=c("time","stkm","LambE","LambW","LambLn")
riscacum

# Método Gráfico 1 -------------------------------------------------------------

options(scipen=999)
theme_set(theme_bw())
dqqe<-ggplot(stall) + 
  geom_point(aes(x=stkm, y=ste),col="red",size=4) + 
  geom_abline(slope=1, intercept=0) +
  xlim(c(0, 1)) + 
  ylim(c(0, 1)) + 
  labs(y="S(t): Exponencial", 
       x="S(t): Kaplan-Meier")+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15))
dqqw<-ggplot(stall) + 
  geom_point(aes(x=stkm, y=stw),col="red",size=4) + 
  geom_abline(slope=1, intercept=0) +
  xlim(c(0, 1)) + 
  ylim(c(0, 1)) + 
  labs(y="S(t): Weibull", 
       x="S(t): Kaplan-Meier")+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15))
dqqln<-ggplot(stall) + 
  geom_point(aes(x=stkm, y=stln),col="red",size=4) + 
  geom_abline(slope=1, intercept=0) +
  xlim(c(0, 1)) + 
  ylim(c(0, 1)) + 
  labs(y="S(t): Lognormal", 
       x="S(t): Kaplan-Meier")+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15))
grid.arrange(dqqe, dqqw, dqqln, ncol=3)


# RISCO ACUMULADO --------------------------------------------------------------

options(scipen=999)
theme_set(theme_bw())
RACqqe<-ggplot(riscacum) + 
  geom_point(aes(x=stkm, y=LambE),col="red",size=4) + 
  geom_abline(slope=1, intercept=0) +
  xlim(c(0, 1)) + 
  ylim(c(0, 1)) + 
  labs(y=expression(paste(Lambda(t),": Exponencial")), 
       x=expression(paste(Lambda(t),": Kaplan-Meier")))+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15))
RACqqw<-ggplot(riscacum) + 
  geom_point(aes(x=stkm, y=LambW),col="red",size=4) + 
  geom_abline(slope=1, intercept=0) +
  xlim(c(0, 1)) + 
  ylim(c(0, 1)) + 
  labs(y=expression(paste(Lambda(t),": Weibull")), 
       x=expression(paste(Lambda(t),": Kaplan-Meier")))+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15))
RACqqln<-ggplot(riscacum) + 
  geom_point(aes(x=stkm, y=LambLn),col="red",size=4) + 
  geom_abline(slope=1, intercept=0) +
  xlim(c(0, 1)) + 
  ylim(c(0, 1)) + 
  labs(y=expression(paste(Lambda(t),": Lognormal")), 
       x=expression(paste(Lambda(t),": Kaplan-Meier")))+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15))
grid.arrange(RACqqe, RACqqw, RACqqln, ncol=3)


# Ajuste Método Gráfico 2 ------------------------------------------------------

options(scipen=999)
theme_set(theme_bw())
gle<-ggplot(stall) + 
  geom_point(aes(x=time, y=-log(stkm)),col="red",size=4) + 
  geom_smooth(aes(x=time, y=-log(stkm)),method=lm,col="red",se=FALSE)+
  geom_abline(slope=1/alpha, intercept=0,col="blue",show.legend = TRUE,lwd=2) +
  xlim(c(0, max(time))) + 
  ylim(c(0, max(-log(stkm)))) + 
  labs(y=expression(-log(S[km](t))), 
       x="t",title="Exponencial")+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15))
glw<-ggplot(stall) + 
  geom_point(aes(x=log(time), y=log(-log(stkm))),col="red",size=4) + 
  geom_smooth(aes(x=log(time), y=log(-log(stkm))),method=lm,col="red",se=FALSE)+
  geom_abline(slope=1, intercept=-log(alphaw),col="blue",lwd=2)+
  geom_abline(slope=gama, intercept=-gama*log(alphaw),col="green",lwd=2)+
  xlim(c(min(log(time)), max(log(time)))) + 
  ylim(c(min(log(-log(stkm))), max(log(-log(stkm))))) + 
  labs(y=expression(log(-log(S[km](t)))), 
       x="log(t)",title="Weibull")+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15))
glln<-ggplot(stall) + 
  geom_point(aes(x=log(time), y=qnorm(stkm)),col="red",size=4) + 
  geom_smooth(aes(x=log(time), y=qnorm(stkm)),method=lm,col="red",se=FALSE)+
  geom_abline(slope=-1/sigma, intercept=mu/sigma,col="grey",lwd=2) +
  xlim(c(min(log(time)), max(log(time)))) + 
  ylim(c(min(qnorm(stkm)), max(qnorm(stkm)))) + 
  labs(y=expression(paste(Phi^-1,S(t))), 
       x="log(t)",title="Lognormal")+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15))
grid.arrange(gle, glw, glln, ncol=3)


# TRV --------------------------------------------------------------------------

ajust1$loglik[2]
ajust2$loglik[2]
ajust3$loglik[2]

tabTRV=as.data.frame(cbind(c("Gama Generalizado",
                             "Exponencial",
                             "Weibull",
                             "Lognormal"),
                           round(cbind(c(-65.69,
                                         ajust1$loglik[2],
                                         ajust2$loglik[2],
                                         ajust3$loglik[2]), 
                                       c(2*(-65.69-65.69),
                                         2*(-65.69-ajust1$loglik[2]),
                                         2*(-65.69-ajust2$loglik[2]),
                                         2*(-65.69-ajust3$loglik[2])), 
                                       c(pchisq(2*(-65.69-(-65.69)),
                                                df=3-3,
                                                ncp = 0,
                                                lower.tail = FALSE,
                                                log.p = FALSE), 
                                         pchisq(2*(-65.69-ajust1$loglik[2]),
                                                df=3-1,
                                                ncp = 0,
                                                lower.tail = FALSE,
                                                log.p = FALSE), 
                                         pchisq(2*(-65.69-ajust2$loglik[2]),
                                                df=3-2,
                                                ncp = 0,
                                                lower.tail = FALSE,
                                                log.p = FALSE), 
                                         pchisq(2*(-65.69-ajust3$loglik[2]),
                                                df=3-2,
                                                ncp = 0,
                                                lower.tail = FALSE,
                                                log.p = FALSE))),
                                 digits=3)))

names(tabTRV)=c("Modelo",
                "logVer",
                "TRV",
                "valor p")

tabTRV[1,3:4]<-"-"
tabTRV

ggplot(data = stall, aes(x = time, y = stkm)) +
  geom_step(aes(color = "stkm")) + 
  geom_line(aes(y = ste, color = "ste")) +
  geom_line(aes(y = stw, color = "stw")) +
  geom_line(aes(y = stln, color = "stln"))+
  scale_color_manual(name = "Ajuste", 
                     values = c("stkm" = "black", "ste" = "blue", 
                                "stw" = "green", "stln" = "red"))+
  labs(y=expression(S(t)),x="Tempo")+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15))


# Quantidades de interesse

MTTFw=alphaw*(gamma(1+1/gama))
MTTFw
MTTFln=exp(mu+(sigma^2/2))
MTTFln


# Vari?ncia dos par?metros do modelo lognormal

ajust3$var #n?o condiz com o livro
vmu=ajust3$var[1,1]
vsigma=0.0176180 # obtido no Minitab pois no R a vari?ncia ? para log(sigma)
covmusigma=0.0020703 #obtido no Minitab pois no R a covari?ncia ? com log(sigma)

vMTTFln=vmu*(exp(mu+sigma^2/2))^2+vsigma*(sigma*exp(mu+sigma^2/2))^2+
  2*covmusigma*(exp(mu+sigma^2/2))*(sigma*exp(mu+sigma^2/2))
vMTTFln
c(MTTFln+qnorm(0.025)*sqrt(vMTTFln[1]),MTTFln+qnorm(0.975)*sqrt(vMTTFln[1]))

t05ln=exp(qnorm(0.5)*sigma+mu)
t05ln



# AJUSTE NÃO PARAMÉTRICO -------------------------------------------------------

# Agora, iremos verificar se as covariáveis possuem alguma influência 
# significativa neste estudo clínico.


# ESTIMAÇÃO DOS MODELOS  -------------------------------------------------------

# Fazer o modelo univariado de Cox para cada variável. 
  
  ## Modelo 1: O grupo com adesivo (patchOnly) foi significativo (0.00613)

fit1 <- coxph(Surv(time,status) ~ grp,
              data=tabagismo,x = T, method="breslow")
summary(fit1)


  ## Modelo 2: A idade foi significativa (0.0103)

fit2 <- coxph(Surv(time,status) ~ age,
              data=tabagismo,x = T, method="breslow")
summary(fit2)


  ## Modelo 3: A idade categorizada (idoso) foi significativa (0.0077)

fit3 <- coxph(Surv(time,status) ~ idadec,
              data=tabagismo,x = T, method="breslow")
summary(fit3)


  ## Modelo 4: O gênero não foi significativo (0.389)

fit4 <- coxph(Surv(time,status) ~ gender,
              data=tabagismo,x = T, method="breslow")
summary(fit4)


  ## Modelo 5: A raça não foi significativa (0.284)

fit5 <- coxph(Surv(time,status) ~ race,
              data=tabagismo,x = T, method="breslow")
summary(fit5)


  ## Modelo 6: O período de tratamento não foi significativo (0.168)

fit6 <- coxph(Surv(time,status) ~ employment,
              data=tabagismo,x = T, method="breslow")
summary(fit6)


  ## Modelo 7: O número de anos que o paciente fuma foi signif. a 10% (0.08)

fit7 <- coxph(Surv(time,status) ~ yearsSmoking,
              data=tabagismo,x = T, method="breslow")
summary(fit7)


## Modelo 8: O peso não foi significativo (0.866)

fit8 <- coxph(Surv(time,status) ~ levelSmoking,
              data=tabagismo,x = T, method="breslow")
summary(fit8)


  ## Modelo 9: O número de tentativas de parar de fumar não foi signif (0.965)

fit9 <- coxph(Surv(time,status) ~ priorAttempts,
              data=tabagismo,x = T, method="breslow")
summary(fit9)


  ## Modelo 10: O maior n° de dias que o paciente ficou sem fumar não foi 
                # significativo (0.116)

fit10 <- coxph(Surv(time,status) ~ longestNoSmoke,
              data=tabagismo,x = T, method="breslow")
summary(fit10)


# Testando o modelo multivariado


  ## Modelo 1

fitm1 <- coxph(Surv(time,status) ~ grp 
              + idadec 
              #+ gender 
              #+ race 
              + employment 
              #+ yearsSmoking 
              #+ levelSmoking 
             #+ priorAttempts 
              #+ longestNoSmoke
              ,data=tabagismo,x = T, 
              method="breslow")

summary(fitm1)

# Deixamos no modelo fitm1 apenas as variáveis grp, idade e período de tratamen.


  ## Modelo 2
fitm2 <- coxph(Surv(time,status) ~ grp 
               + age 
               #+ gender 
               + race 
               + employment 
               #+ yearsSmoking 
               #+ levelSmoking 
               #+ priorAttempts 
               #+ longestNoSmoke
               ,data=tabagismo,x = T, 
               method="breslow")

summary(fitm2)

# Deixamos as variáveis significativas e a raça, pois sem a raça o n° de 
# variáveis significativas dinimui.

  
  ## Modelo 3
fitm3 <- coxph(Surv(time,status) ~ grp 
               + age 
               #+ gender 
               #+ race 
               + employment 
               #+ yearsSmoking 
               #+ levelSmoking 
               #+ priorAttempts 
               #+ longestNoSmoke
               ,data=tabagismo,x = T, 
               method="breslow")

summary(fitm3)

# Deixamos somente as variáveis que foram significativas.


# verificando qual é o melhor modelo pelo TRV ---------------------------------

TRV1=2*(fitm2$loglik[2]-fitm1$loglik[1])
TRV2=2*(fitm3$loglik[2]-fitm2$loglik[2])

p1=round(pchisq(TRV1, df=3,
                 ncp = 0,
                 lower.tail = FALSE,
                 log.p = FALSE),
          digits=3)

p2=round(pchisq(TRV2,
                df=1,
                ncp = 0,
                lower.tail = FALSE,
                log.p = FALSE),
         digits=3)

TRVTab=as.data.frame(cbind(
  c("Modelo 2","Modelo 3"),
  c(round(TRV1,digits=3),round(TRV2,digits=3)),
  c(p1,p2)
))

colnames(TRVTab)=c("TRV-Modelo","Estatística TRV","valor-p")
TRVTab


# O modelo 3 (fitm3) parece ter tido o melhor ajuste.


# Teste global de Correlação de Pearson para os resíduos padronizados 
# de Schoenfeld

ftest<-cox.zph(fitm3,
               transform="identity",
               terms=TRUE) #g(t)=t
ftest
ggcoxzph(ftest)

# Pode observar que os valores dos coeficientes de correlação de Pearson são 
# todos próximos de zero. Observa-se também que os testes para cada covariável 
# apresentaram evidências que não permitem a rejeição da hipóteste nula, ou 
# seja, as taxas de falha são proporcionais (todos os valores-p são acima de
# 0,3).


# Verificando o ajuste do melhor modelo

resm<-resid(fitm3,type="martingale")
res<-tabagismo$status-resm
ajustkmres <- survfit(Surv(res,tabagismo$status)~1)
summary(ajustkmres)
timeres=ajustkmres$time
stkmres<-ajustkmres$surv
ste1<-exp(-timeres/1)

stallres=as.data.frame(cbind(timeres,stkmres,ste1))
stallres

kmXexp_res=ggplot(data = stallres, aes(x = timeres, y = stkmres)) +
  geom_step(aes(color = "stkmres")) + 
  geom_line(aes(y = ste1, color = "ste1")) +
  scale_colour_manual("Ajuste", labels = c("Exp(1)","K-M"),
                      values = c("blue","black")) +
  labs(y=expression(S(hat(e))),x=expression(paste(hat(e))))+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
        legend.title=element_text(size=15),legend.text=element_text(size=15))

options(scipen=999)
theme_set(theme_bw())
dqqres=ggplot(stallres) + 
  geom_point(aes(x=stkmres, y=ste1),col="red",size=4) + 
  geom_abline(slope=1, intercept=0) +
  xlim(c(0, 1)) + 
  ylim(c(0, 1)) + 
  labs(y="S(e): Exponencial(1)", 
       x="S(e): Kaplan-Meier")+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15))

grid.arrange(kmXexp_res,dqqres,ncol=2)


# Razão de riscos --------------------------------------------------------------

summary(fitm3)
fitm3$coefficients

exp(0.59225189-0.033848224*(60-30)+0.67592) 
#O risco de reincidência ao fumo de pacientes com 60 anos de idade, no tratamento 
# com adesivo e que se tratam em períodos não integral é 1,28 vezes maior do que
# em pacientes de 30 no mesmo tratamento.

exp(0.59225189-0.033848224*(60)+0.67592)/(exp(0.59225189-0.033848224*(60)+0.63345))
#O risco de reincidência ao fumo de pacientes com 60 anos de idade, no tratamento 
# com adesivo e que se tratam em outros períodos é 1,04 vezes maior do que
# em pacientes de 30 no mesmo tratamento que se tratam em período parcial.


# Sobrevivência e Risco basal--------------------------------------------------

Ht<-basehaz(fitm3,centered=F)
tempos<-Ht$time
H0<-Ht$hazard
S0<- exp(-H0)
SR0=round(cbind(tempos, S0,H0),digits=5)
SR0


idade=60
St_E1_ID=S0^exp(-0.03385*idade)
St_E2_ID=S0^exp(0.59225-0.03385*idade)
St_E3_ID=S0^exp(0.59225-0.03385*idade+0.67592)
St_E4_ID=S0^exp(0.59225-0.03385*idade+0.63345)
stall=as.data.frame(cbind(tempos,St_E1_ID,St_E2_ID,St_E3_ID,St_E4_ID))
stall

options(scipen=999)
theme_set(theme_bw())
st60<-ggplot(stall) + 
  geom_line(aes(x=tempos,y = St_E1_ID, color="St_E1_ID")) +
  geom_line(aes(x=tempos,y = St_E2_ID, color="St_E2_ID")) +
  geom_line(aes(x=tempos,y = St_E3_ID, color="St_E3_ID")) +
  geom_line(aes(x=tempos,y = St_E4_ID, color="St_E4_ID")) +
  scale_colour_manual("Tratamentos",
                      labels = c("Combinação em tempo integral",
                                 "Adesivo",
                                 "Adesivo e Outros períodos",
                                 " Adesivo e Período Parcial"),
                      values = c("black","red","blue","green")) +
  labs(x=expression(t),y=expression(S(t)),title="Idade=60 anos")+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
        legend.title=element_text(size=15),legend.text=element_text(size=15))
st60

idade=30
St_E1_ID=S0^exp(-0.03385*idade)
St_E2_ID=S0^exp(0.59225-0.03385*idade)
St_E3_ID=S0^exp(0.59225-0.03385*idade+0.67592)
St_E4_ID=S0^exp(0.59225-0.03385*idade+0.63345)
stall=as.data.frame(cbind(tempos,St_E1_ID,St_E2_ID,St_E3_ID,St_E4_ID))
stall

options(scipen=999)
theme_set(theme_bw())
st30<-ggplot(stall) + 
  geom_line(aes(x=tempos,y = St_E1_ID, color="St_E1_ID")) +
  geom_line(aes(x=tempos,y = St_E2_ID, color="St_E2_ID")) +
  geom_line(aes(x=tempos,y = St_E3_ID, color="St_E3_ID")) +
  geom_line(aes(x=tempos,y = St_E4_ID, color="St_E4_ID")) +
  scale_colour_manual("Situações",
                      labels = c("Combinação em tempo integral",
                                 "Adesivo",
                                 "Adesivo e Outros períodos",
                                 "Adesivo e Período Parcial"),
                      values = c("black","red","blue","green")) +
  labs(x=expression(t),y=expression(S(t)),title="Idade=30 anos")+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
        legend.title=element_text(size=15),legend.text=element_text(size=15))
st30

grid.arrange(st60, st30,ncol=2)

