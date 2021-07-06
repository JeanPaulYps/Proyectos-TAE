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
library(caret)
load("Variables.Rdata")

# Define UI for application that draws a histogram
ui <- navbarPage("Prediccion hijos",
                    tabPanel("Bienvenida ",
                             h1("Bienvenido a la aplicacion"),
                             HTML(
                               '
<p>Con base a la información de la encuesta de Calidad de Vida publicada en el año 2019, el apartado de características
    y composición del hogar del Departamento Nacional de Estadísticas (DANE) entidad responsable de la planeación,
    levantamiento, procesamiento, análisis y difusión de las estadísticas oficiales en Colombia. Se desarrolló un modelo
    estadístico que prediga con base en unos datos específicos del hogar, la cantidad de hijos que ese hogar tiene.</p>
<h2 id="p-blico-objetivo">Público objetivo</h2>
<p>Esta aplicación esta dirigida a varios públicos objetivos: </p>
<ul>
    <li><strong>El estado colombiano:</strong> esta aplicación puede servir al estado colombiano a tomar decisiones con
        respecto a las características de ciertos hogares. También con la información aquí presentada puede ayudar a
        hacer nuevas preguntas, que puedan ayudar a tener un modelo mas preciso y que finalmente pueda ayudar a tomar
        decisiones con respecto a las ayudas sociales por poner un ejemplo.</li>
    <li><strong>Agencias de marketing:</strong> podría ser un apoyo para planear una campaña publicitaria. Esta
        aplicación daría algunas ideas de que características son las que influyen a que una persona tenga mas o menos
        hijos. Con esta información podrían indagar un poco mas en los motivos y presentar una campaña enfocada a
        padres, madres para que compren determinado producto.</li>
    <li><strong>Personas particulares que quieran tener una predicción de cuantos hijos tendrán:</strong> una persona
        con curiosidad de cuantos hijos va a tener en el futuro podría usarla para tener una idea de cuantos hijos
        tienen las personas con características similares a las de él. Ya que la mayoría de las variables que se usan ya
        están dadas desde el momento en el que una persona nace.</li>
</ul>
<h2 id="variables-que-se-pueden-modificar-del-hogar">Variables que se pueden modificar del hogar</h2>
<p>El modelo estadístico, da un pronostico de cuantos hijos tendrá un hogar con base en las siguientes características:
</p>
<ul>
    <li><strong>Nivel educativo alcanzado por familiares de la cabeza del hogar:</strong> en cada uno de estos podrás
        escoger desde no saber la información, pasando por los grados de colegio hasta un pregrado o postgrado de
        universidad.<ul>
            <li><strong>Escolaridad del padre de la cabeza del hogar:</strong> En este apartado debes seleccionar el
                nivel de escolaridad alcanzada por el padre de la persona que mantiene el hogar. Si tu fueras la cabeza
                de hogar sería tu padre.</li>
            <li><strong>Escolaridad del la madre de la cabeza del hogar:</strong> En este apartado debes seleccionar el
                nivel de escolaridad alcanzada por la madre de la persona que mantiene el hogar. Si tu fueras la cabeza
                de hogar sería tu madre.</li>
            <li><strong>Último nivel educativo alcanzado por la pareja de la cabeza del hogar:</strong> deberás
                seleccionar el nivel de escolaridad de la pareja que mantiene el hogar. Si tu fueras la cabeza de hogar
                sería tu pareja sentimental.</li>
        </ul>
    </li>
    <li><strong>Grado de satisfacción de vida de la pareja de la cabeza del hogar:</strong> de una escala de 0 al 10
        debes seleccionar que tan satisfecha se siente la pareja de la persona que mantiene el hogar. Donde 0 es muy
        insatisfecha y 10 es estar satisfecha.</li>
    <li><strong>Número de personas del hogar:</strong> selecciona el número de personas que hay en el hogar. Acá puedes
        seleccionar desde 3 a 19 personas que viven en un mismo hogar.</li>
</ul>
<h2 id="mas-informaci-n">Mas información</h2>
<p>Para leer el informe técnico <a href="https://rpubs.com/JeanPaulYps/776348">haz clic aquí</a></p>
<p>Video de la aplicación:</p>
<iframe width="560" height="315" src="https://www.youtube.com/embed/LB3P0-kc5eM" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen style="display:block; margin: 0 auto;"></iframe>'
                             )
                              
                             
                             
                             ),
          
                   tabPanel("Aplicacion", 
                            sidebarLayout(
                              sidebarPanel(
                                
                                selectInput("Escolaridad_padre_jefe", 
                                            label = "Selecciona el nivel de educacion del padre de la persona cabeza del hogar:",
                                            choices = c("No sabe" = "No sabe",
                                                        "Ninguno" = "Ninguno",
                                                        "Algunos años de primaria" = "Algunos anios de primaria",
                                                        "Toda la primaria" = "Toda la primaria" ,
                                                        "Algunos años de secundaria" = "Algunos anios de secundaria",
                                                        "Toda la secundaria" = "Toda la secundaria",
                                                        "Uno o mas años de tecnica o tecnologica" = "Uno o mas anios de tecnica o tecnologica",
                                                        "Tecnica o tecnologica completa" = "Tecnica o tecnologica completa",
                                                        "Uno o mas años de universidad" ="Uno o mas anios de universidad",
                                                        "Universitaria completa" = "Universitaria completa"
                                            ),
                                            selected = "No sabe´"),
                                
                                selectInput("Escolaridad_madre_pareja", 
                                            label = "Selecciona el nivel de educacion de la madre de la cabeza del hogar:",
                                            choices = c("No sabe" = "No sabe",
                                                        "Ninguno" = "Ninguno",
                                                        "Algunos años de primaria" = "Algunos anios de primaria",
                                                        "Toda la primaria" = "Toda la primaria" ,
                                                        "Algunos años de secundaria" = "Algunos anios de secundaria",
                                                        "Toda la secundaria" = "Toda la secundaria",
                                                        "Uno o mas años de tecnica o tecnologica" = "Uno o mas anios de tecnica o tecnologica",
                                                        "Tecnica o tecnologica completa" = "Tecnica o tecnologica completa",
                                                        "Uno o mas años de universidad" ="Uno o mas anios de universidad",
                                                        "Universitaria completa" = "Universitaria completa"
                                            ),
                                            selected = "No sabe´"),
                                
                                selectInput("Nivel_educativo_pareja", 
                                            label = "Selecciona el nivel de educacion de la pareja del jefe(a) del hogar ",
                                            choices = c("Ninguno" = "Ninguno",
                                                        "Basica Primaria (1 - 5)" = "Basica Primaria (1 - 5)" ,
                                                        "Basica secundaria (6 - 9)" = "Basica secundaria (6 - 9)",
                                                        "Media (10 - 13)" = "Media (10 - 13)",
                                                        "Tecnico sin titulo" = "Tecnico sin titulo",
                                                        "Tecnico con titulo" = "Tecnico con titulo",
                                                        "Tecnologico sin titulo" = "Tecnologico sin titulo",
                                                        "Tecnologico con titulo" = "Tecnologico con titulo",
                                                        "Universitario sin titulo" = "Universitario sin titulo",
                                                        "Universitario con titulo" = "Universitario con titulo",
                                                        "Postgrado sin titulo" = "Postgrado sin titulo",
                                                        "Postgrado con titulo" = "Postgrado con titulo"
                                            ),
                                            selected = "Basica Primaria (1 - 5)"),
                                
                                sliderInput("Grado_satisfaccion_de_vida_pareja",
                                            "Grado de satisfaccion de la pareja del jefe(a):",
                                            min = 0,
                                            max = 10,
                                            value = 0),
                                
                                sliderInput("Numero_personas_por_casa",
                                            "Numero de personas por casa:",
                                            min = 3,
                                            max = 19,
                                            value = 3),
                              ),
                              
                              
                              
                              
                              
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                h2("Predicciones del modelo"),
                                p(textOutput("descripcionVariables")),
                                strong(textOutput("textoResultado"))
                              )
                              
                            )
                        )
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
     output$textoResultado <- renderText({
         mensajeError = "Lo sentimos para esos valores no tenemos una prediccion, sigue intentando con otros valores :)"
         
         EntradasParaHacerPrediccion <- datos  %>% 
           select(c('Escolaridad_padre_jefe','Escolaridad_madre_pareja','Nivel_educativo_pareja', 
                    'Numero_personas_por_casa', 'Grado_satisfaccion_de_vida_pareja')) %>% 
           add_row(Escolaridad_padre_jefe = as.factor(input$Escolaridad_padre_jefe),
                   Escolaridad_madre_pareja = as.factor(input$Escolaridad_madre_pareja),
                   Nivel_educativo_pareja = as.factor(input$Nivel_educativo_pareja),
                   Numero_personas_por_casa = input$Numero_personas_por_casa, 
                   Grado_satisfaccion_de_vida_pareja = input$Grado_satisfaccion_de_vida_pareja,
                   ) %>%
           mutate(Escolaridad_padre_jefe = as.data.frame(dummy.code(Escolaridad_padre_jefe))) %>%
           mutate(Escolaridad_madre_pareja = as.data.frame(dummy.code(Escolaridad_madre_pareja)))  %>%
           mutate(Nivel_educativo_pareja = as.data.frame(dummy.code(Nivel_educativo_pareja)))  %>%
           tail(1)

        respuesta <- mensajeError
        try(
          {
            respuesta <- predict(mod_knn, EntradasParaHacerPrediccion)
            respuesta = round(respuesta)
          },
          silent=F
        )

        if (respuesta == mensajeError)
        {
          return(respuesta)
        }
        respuesta <-paste("El numero de hijos sera: ", respuesta)
        return(as.character(as.vector(respuesta)))
    })
    output$descripcionVariables <- renderText({
      paste("Con el nivel educativo del padre del jefe ", tolower(input$Escolaridad_padre_jefe),
            ". Con el grado de escolaridad de la madre de la pareja ", tolower(input$Escolaridad_madre_pareja), 
            ". Con el nivel educativo de pareja ", tolower(input$Nivel_educativo_pareja),
            ". Con el grado de satisfaccion de pareja con valor ", tolower(input$Grado_satisfaccion_de_vida_pareja),
            ". Con ", input$Numero_personas_por_casa, " personas en casa", sep="")
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
