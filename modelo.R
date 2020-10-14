####### Cliente: TC1388 - Flávio S1
####### Início: 11/09/2020
####### Autor: Joziani Mota


options(OutDec = ',')

##### Pacotes ------------------------------------------------------------------

if(!require(survival)){install.packages('survival'); require(survival)}
if(!require(ggfortify)){install.packages('ggfortify'); require(ggfortify)}
if(!require(tidyverse)){install.packages('tidyverse'); require(tidyverse)}
if(!require(survminer)){install.packages('survminer'); require(survminer)}
if(!require(ranger)){install.packages('ranger'); require(ranger)}
if(!require(gridExtra)){install.packages('gridExtra'); require(gridExtra)}


##### Leitura e manipulação dos Dados ------------------------------------------

Dados <- reactive({
  
  tabagismo <- read.csv("dados/tabagismo.txt", header = T,
                        sep = " ", 
                        encoding = "UTF-8")
  
  tabagismo <- tabagismo %>% 
    mutate(
      grp = as.factor(grp),
      gender = as.factor(gender),
      race = as.factor(race),
      levelSmoking = as.factor(levelSmoking),
      idadec = cut(age, c(0,30,50,100),
                   labels = c("Jovem","Adulto","Idoso"))
    )
  
  return(tabagismo)
  
})


##### shiny --------------------------------------------------------------------

modelo_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = 'modelo',
    h2(
      style = "color: #1E2733; font-size: 25px;",
      align = 'center',
      'MODELO FINAL'
    ),
    fluidRow(
      box(width = 9,
          title = 'coeficientes do modelo',
          DT::dataTableOutput(ns('coefmodel'))),
      box(width = 3,
          h3('Formula'),
          h5('Modelo = exp(0.59*grppatchOnly - 0.03*age + 0.68* employmentother + 0.63*employmentpt)'))
    ),
    fluidRow(
      box(width = 5,
          title = 'Teste de proporcionalidade',
          DT::dataTableOutput(ns('residuosmodel'))),
      box(width = 7,
          title = 'Gráfico de análise de residual',
          imageOutput(ns('residuosgrafico'))),
    ),
    fluidRow(
      box(width = 7,
          title = 'Ajuste do modelo',
          plotOutput(ns('ajuste')))
    ),
    fluidRow(
      box(width = 12,
          title = 'Sobrevivência e Risco Basal',
          plotOutput(ns('riscobasal')))
    )
  )
}


modelo_server <- function(input, output, session) {
  
  output$coefmodel <- DT::renderDataTable({
    
    tabagismo <<- Dados()
    
    fitm3 <<- coxph(Surv(time,status) ~ grp 
                   + age 
                   + employment 
                   ,data=tabagismo,x = T, 
                   method = "breslow")
    
    coefs <- summary(fitm3)
    
    coefs <- data.frame(coefs$coefficients, coefs$conf.int) %>% 
      mutate(Nomes = c('Grupo = Patchonly',
                       'Idade',
                       'Employment = Outro',
                       'Employment = Meio período'),
             IC = paste0('[', round(lower..95, 2), ';', round(upper..95, 2), ']'),
             coef = round(coef,2),
             exp.coef. = round(exp.coef.,2),
             se.coef. = round(se.coef.,2),
             Pr...z.. = round(Pr...z..,3)) %>% 
      select(Nomes, coef, exp.coef., se.coef., IC, Pr...z..)
    
    colnames(coefs) <- c('Nomes',
                         'Betas', 
                         'Risco Relativo', 
                         'E.P.(Betas)', 
                         'IC 95% (Razão de Risco)', 
                         'Valor-p')
    
    coefs %>% 
      DT::datatable(.,
                    rownames = FALSE,
                    escape = FALSE,
                    extensions = c('Scroller', 'Buttons'),
                    options = 
                      list(language = 
                             list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'),
                           pageLength = 20,
                           dom = 'Bdt',
                           buttons = c('excel', 'pdf'),
                           scrollX = TRUE,
                           scroller = TRUE,
                           autoWidth = TRUE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#1E2733', 'color': '#fff'});",
                             "}")
                      )
      )
    
  })
  
  
  output$residuosmodel <- DT::renderDataTable({
    
    ftest <<- cox.zph(fitm3,
                     transform="identity",
                     terms = TRUE)
    
    
    ftest <- tibble(Nomes = c('Grupo',
                              'Idade',
                              'Employment',
                              'Global'),
                    `Teste Qui-Quadrado` = c(0.00,1.08,0.91,1.49),
                    `Graus de liberdade` = c(1,1,2,4),
                    `Valor-p` = c(0.97,0.30,0.63,0.83))
    
    ftest %>% 
      DT::datatable(.,
                    rownames = FALSE,
                    escape = FALSE,
                    extensions = c('Scroller', 'Buttons'),
                    options = 
                      list(language = 
                             list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'),
                           pageLength = 20,
                           dom = 'Bdt',
                           buttons = c('excel', 'pdf'),
                           scrollX = TRUE,
                           scroller = TRUE,
                           autoWidth = TRUE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#1E2733', 'color': '#fff'});",
                             "}")
                      )
      )
    
  })
  
  
  output$residuosgrafico <-  renderImage({
    
    # svg('www/img/residuo.svg')
    #ggcoxzph(ftest)
    # dev.off()
    
    list(src = 'www/img/residuo.png',
         width = 500,
         height = 430,
         alt = ' Análise residual.')
    
  })
  
  
  output$ajuste <- renderPlot({
    
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
    
  })
  
  
  output$riscobasal <- renderPlot({
  
    #Sobrevivência e Risco basal--------------------------------------------------
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
      scale_colour_manual("Tratamentos",
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
    
    
  })
}
