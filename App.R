
# Sys.setlocale("LC_ALL", "English")

# Leitura dos Pacotes ####
rm(list=ls())
options(OutDec = ",")

library(tidyverse)
library(openxlsx)
library(readxl)
library(zoo)
library(shinydashboardPlus)
library(dashboardthemes)
#library(fireData)
library(highcharter)
library(stringr)
library(DT)
library(shinydashboard)
library(shiny)
library(shinyjs)
library(lubridate)
library(htmlwidgets)
library(shinycssloaders)
library(shinyWidgets)
library(shinyalert)
library(shinyBS)


#### Carregando Funções e Modulos ----------------------------------------------

source("funcoes/funcoes_dash.R", encoding = "UTF-8")
source("modulos/descricao.R", encoding = "UTF-8")
source("modulos/modelo.R", encoding = "UTF-8")


# Carregando os dados ---------------------------------------------------------



# Design ######################################################################

brbg <- hsv(0.5, .35, seq(.25, .95, length.out = 12))

logo_blue <- shinyDashboardLogoDIY(
  boldText = ""
  , mainText = ""
  , textSize = 16
  , badgeText = ""
  , badgeTextColor = "white"
  , badgeTextSize = 0
  , badgeBackColor = "#"
  , badgeBorderRadius = 3
)


header <- uiOutput("ui_menu_top")

sidebar <- uiOutput("mainsidebar")

body <- dashboardBody(
  tags$link(rel = "stylesheet", type = "text/css", href = "css/bootstrap.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "css/style_dash.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "css/style_button.css"),
  useShinyjs(),
  uiOutput("mainbody")
)


ui <- dashboardPagePlus(
  title = "Análise de Sobrevivência",
  header,
  sidebar,
  body
)


# Servidor ####################################################################

server <- function(input, output, session) {
  
  
  ### Interface ----------------------------------------------------------------
  
  output$mainsidebar <- renderUI({
    uiOutput("sidebarpanel")
  })
  
  # sidebar 
  output$sidebarpanel <- renderUI({
    
    removeClass(selector = ".sidebar-toggle", class = "sidebar-toggle-none")
    
    dashboardSidebar(
      
      sidebarMenu(
        id = "tabs",
        menuItem("Integrantes", tabName = "tab_inicio", icon = icon("user-friends")),
        menuItem("Sobre os Dados", tabName = "sobre_dados", icon = icon("align-justify")),
        menuItem("Descrição", tabName = "descricao", icon = icon("list-alt")),
        menuItem("Modelo", tabName = "modelo", icon = icon("laptop-medical"))
      )
    )
  })
  
  
  # Header UI
  output$ui_menu_top <- renderUI({
    
    dashboardHeader(
      title = tagList(
        h4("Análise de Sobrevivência")
      )
    )
    
  })
  
  
  
  
  ### Body ---------------------------------------------------------------------
  
  output$mainbody <- renderUI({
    uiOutput("body")
  })
  
  
  output$body <- renderUI({
    tabItems(
      tabItem(
        tabName = 'tab_inicio',   
        class = 'active',
        fluidRow(
          column(
            width = 12,
            h3('Grupo'),
            h5('Andressa de Souza Freitas'),
            h5('Iara Mendes Oliveira'),
            h5('Joziani Mota Vieira'),
            h5('Samuel Sabino Freitas'),
            h5('Yuri Marcos Tomáz de Sousa'),
          )
        )
      ),
      tabItem(
        tabName = 'sobre_dados',
        h1(class = 'title-header', 'Dados de cessação de tabagismo'),
        p('Estudo clínico aleatorizado de terapia tripla contra adesivo, para cessação do tabagismo. O conjunto de dados conta com 125 observações e as seguintes 12 variáveis.'),
        p('id: ID paciente'),
        p('time: Tempo em dias até reincidência do hábito de fumar;'),
        p('status: Indicador de reincidência (retorno ao fumo);'),
        p('grp: Grupo de tratamento (combination-combinação de 3 tratamentos; patchonly- tratamento somente com adesivo);'),
        p('age: Idade na época do início do estudo;'),
        p('gender: Feminino(Female) or Masculino(Male);'),
        p('race: Negro(a) (black), hispânico(a) (hispanic), branco(a) (white), outro (other);'),
        p('employment: ft (full-time- tempo integral), pt (part-time-meio período), ou other(outro);'),
        p('yearsSmoking: Número de anos que o paciente fuma;'),
        p('levelSmoking: Heavy (pesado) or light (leve);'),
        p('priorAttempts: Número de tentativas de parar de fumar;'),
        p('longestNoSmoke: O maior período de tempo, em dias, que previamente o paciente já conseguiu ficar sem fumar.')
      ),
      descricao_ui(id = "descricao"),
      modelo_ui(id = "modelo")
    )
  })
  
  
  
  ## Modulos -------------------------------------------------------------------
  
  callModule(
    module = descricao_server,
    id = "descricao"
  )
  
  callModule(
    module = modelo_server,
    id = "modelo"
  )
  
  
}


shinyApp(ui = ui, server = server)
