#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(class)
library(tidyverse)
library(psych)
load("Variables.Rdata")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Prediccion de hijos con base a las caracteristicas de un hogar"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
          selectInput("Educacion_jefe", 
                      label = "Selecciona el nivel de educacion del jefe(a) del hogar ",
                      choices = c("Basica Primaria (1 - 5)",
                                  "Basica secundaria (6 - 9)",
                                  "Media (10 - 13)",
                                  "Ninguno",
                                  "Postgrado con titulo",
                                  "Postgrado sin titulo",
                                  "Tecnico con titulo",
                                  "Tecnico sin titulo",
                                  "Tecnologico con titulo",
                                  "Tecnologico sin titulo",
                                  "Universitario con titulo",
                                  "Universitario sin titulo"
                                  ),
                      selected = "Basica Primaria (1 - 5)"),
          
          selectInput("Estado_civil_jefe", 
                      label = "Selecciona el estado civil del jefe(a) ",
                      choices = c("Esta casado(a)",
                                  "Esta separado(a) o divorciado(a)",
                                  "Esta soltero(a)",
                                  "Esta viudo(a)",
                                  "No esta casado(a) y vive en pareja hace dos anios o mas",
                                  "No esta casado(a) y vive en pareja hace menos de dos anios"
                      ),
                      selected = "Esta casado(a)"),
          
          sliderInput("Grado_satisfaccion_jefe",
                      "Grado de satisfaccion del jefe:",
                      min = 0,
                      max = 10,
                      value = 0),
        ),
        
      
        
 
        

        # Show a plot of the generated distribution
        mainPanel(
           h2("Predicciones del modelo"),
           p(textOutput("descripcionVariables")),
           strong(textOutput("textoResultado"))
        )
        
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$textoResultado <- renderText({
        mensajeError = "Lo sentimos para esos valores no tenemos una prediccion, sigue intentando con otros valores :)"
        EntradasParaHacerPrediccion <- datos  %>% 
          select(c('Nivel_educativo_jefe','Estado_civil_jefe','Grado_satisfaccion_de_vida_jefe')) %>% 
          add_row(Nivel_educativo_jefe = as.factor(input$Educacion_jefe),
                                     Estado_civil_jefe = as.factor(input$Estado_civil_jefe),
                                     Grado_satisfaccion_de_vida_jefe= input$Grado_satisfaccion_jefe ) %>%
          mutate(Nivel_educativo_jefe = as.data.frame(dummy.code(Nivel_educativo_jefe))) %>%
          mutate(Estado_civil_jefe = as.data.frame(dummy.code(Estado_civil_jefe)))  %>%
          tail(1)
        respuesta <- mensajeError
        try(
          {
            respuesta <- mod_knn_train<-knn(train=Train1,test=EntradasParaHacerPrediccion[-4],cl=y_Train,k=18,prob = TRUE, use.all = TRUE)    
          },
          silent=TRUE
        )
        
        if (respuesta == mensajeError)
        {
          return(respuesta)
        }
        respuesta <-paste("El numero de hijos sera: ", respuesta)
        return(as.character(as.vector(respuesta)))
    })
    output$descripcionVariables <- renderText({
      paste("Con el nivel educativo", input$Educacion_jefe,
                                         ". Con grado de satisfaccion", input$Grado_satisfaccion_jefe,
                                         ". Y estado civil ", input$Estado_civil_jefe, ".")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
