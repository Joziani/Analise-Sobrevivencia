####### Cliente: TC1388 - Flávio S1
####### Início: 11/09/2020
####### Autor: Joziani Mota


options(OutDec = ',')

##### Pacotes ------------------------------------------------------------------

if(!require(survival)){install.packages('survival'); require(survival)}
if(!require(tidyverse)){install.packages('tidyverse'); require(tidyverse)}
if(!require(survminer)){install.packages('survminer'); require(survminer)}


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

descricao_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = 'descricao',
    h2(
      style = "color: #1E2733; font-size: 25px;",
      align = 'center',
      'Análise Descritiva'
    ),
    fluidRow(
      box(width = 12,
          title = 'Quantidade de dados faltantes',
          DT::dataTableOutput(ns('dadosfaltantes')))),
    fluidRow(
      column(
        width = 6,
        selectInput(
          inputId = ns("variavel"),
          label = "Variável",
          choices = c('Idade', 'Gênero', 'Raça', 'Período de tratamento', 'Grupo')
        )
      )
    ),
    fluidRow(
      box(title = 'Gráfico de Probabilidade de sobrevivência',
          plotOutput(ns('graficos')))
    )
  )
}


descricao_server <- function(input, output, session) {
  
  output$dadosfaltantes <- DT::renderDataTable({
    
    tabagismo <- Dados()
    
    Dfaltantes <- t(sapply(tabagismo, function(x) sum(is.na(x))) %>% 
                      data.frame()) 
    rownames(Dfaltantes) <- NULL
    
    Dfaltantes %>% 
      DT::datatable(.,
                    rownames = FALSE,
                    escape = FALSE,
                    extensions = c('Scroller'),
                    options = 
                      list(language = 
                             list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'),
                           pageLength = 20,
                           dom = 'Bdt',
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
  
  
  output$graficos <-  renderPlot({
    
    tabagismo <- Dados()
    
    if(input$variavel == 'Idade'){
      
      ekm1 <- survfit(Surv(time,status) ~ idadec, 
                      data = tabagismo, 
                      conf.type = c("log-log"))
      
      ## Gráfico de Probabilidade de sobrevivência
      
      ggsurvplot(ekm1, data = tabagismo, 
                 pval = T, 
                 conf.int = T,
                 legend.title = 'Faixa Etária: ',
                 xlab = 'Tempo (dias)',
                 ylab = expression(hat(S[km])(t)),
                 surv.median.line = 'hv',
                 legend.labs = c("Jovem", "Adulto", "Idoso"))
      
    }else if(input$variavel == 'Gênero'){
      
      ekm1 <- survfit(Surv(time,status) ~ gender, 
                      data = tabagismo, 
                      conf.type = c("log-log"))
      
      ## Gráfico de Probabilidade de sobrevivência
      
      ggsurvplot(ekm1, data = tabagismo, 
                 pval = T, 
                 conf.int = T,
                 legend.title = 'Gênero: ',
                 xlab = 'Tempo (dias)',
                 ylab = expression(hat(S[km])(t)),
                 surv.median.line = 'hv',
                 legend.labs = c("Feminino", "Masculino"))
      
    }else if(input$variavel == 'Raça'){
      
      ekm1 <- survfit(Surv(time,status) ~ race, 
                      data = tabagismo, 
                      conf.type = c("log-log"))
      
      ## Gráfico de Probabilidade de sobrevivência
      
      ggsurvplot(ekm1, data = tabagismo, 
                 pval = T, 
                 conf.int = T,
                 legend.title = 'Raça: ',
                 xlab = 'Tempo (dias)',
                 ylab = expression(hat(S[km])(t)),
                 surv.median.line = 'hv',
                 legend.labs = c("Negra", "Hispânica", "Outra", "Branca"))
      
    }else if(input$variavel == 'Período de tratamento'){
      
      ekm1 <- survfit(Surv(time,status) ~ employment, 
                      data = tabagismo, 
                      conf.type = c("log-log"))
      
      ## Gráfico de Probabilidade de sobrevivência
      
      ggsurvplot(ekm1, data = tabagismo, 
                 pval = T, 
                 conf.int = T,
                 legend.title = 'Período de tempo: ',
                 xlab = 'Tempo (dias)',
                 ylab = expression(hat(S[km])(t)),
                 surv.median.line = 'hv',
                 legend.labs = c("Integral", "Outros", "Parcial"))
      
    }else{
      
      ekm1 <- survfit(Surv(time,status) ~ grp, 
                      data = tabagismo, 
                      conf.type = c("log-log"))
      
      ## Gráfico de Probabilidade de sobrevivência
      
      ggsurvplot(ekm1, data = tabagismo, 
                 pval = T, 
                 conf.int = T,
                 legend.title = 'Tratamentos: ',
                 xlab = 'Tempo (dias)',
                 ylab = expression(hat(S[km])(t)),
                 surv.median.line = 'hv',
                 legend.labs = c("Combinação", "Adesivo"))
      
    }
    
    
  })
  
  
  
  
  
}
