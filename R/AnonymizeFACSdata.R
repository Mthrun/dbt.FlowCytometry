AnonymizeFACSdata=function(){


  #library(shiny)
  #library(shinyjs)
  #library(shinyFiles) #ToDo
  # in the running environment.
  globalVars <- reactiveValues(
    DataListe = NULL,
    currentpath=NULL #used later in shinyFiles
  )

  ## Shiny Oberflaeche ----

  # Define UI for application that draws a histogram
  uifacs <- fluidPage(

    # Application title
    titlePanel("Anonymize FACS Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        disabled(downloadButton(
          outputId = "saveFCS",
          label = "Speichere *.fcs",
          icon = icon("save")
        )),
        h1(),
        actionButton(inputId = "Exit",
                     label = "Exit",
                     icon = icon("window-close"))
      ),

      # Show a plot of the generated distribution
      mainPanel(
        fileInput(
          inputId = "dataInput",
          label = "Daten laden",
          buttonLabel = "Durchsuchen..",
          multiple = FALSE,
          accept = c('fcs','lmd')
        ),
        h4(span(textOutput("info"), style = "color:blue"))
      )
    )
  )

  # Define server logic required to draw a histogram
  serverfacs <- function(input, output, session) {
    #1GB limit
    options(shiny.maxRequestSize=1024*1024^2)

    inputDataReactive <- reactive(
      {
        if (is.null(input$dataInput)) {
          return(NULL)
        }
        tryCatch({
        DataListe  <- ReadFCS_FlowCompensated(input$dataInput$datapath,shiny = TRUE)
        },error=function(e){
          str = paste('ReadFCS_FlowCompensated: Data could not be loaded because ', e)
          warning(str)
          output$info <- renderText({
            str
          })
        })
        globalVars$DataListe=DataListe
        print(globalVars$DataListe$Extension)
        # try({
        # globalVars$currentpath=dirname(input$dataInput$datapath)
        # setwd(globalVars$currentpath)
        # })
        #
        output$info <- renderText({
          "Datei Geladen"
        })

      })


    observe({
      # What to observe
      #input$dataInput
      inputDataReactive()
      # What to do, when the data in the observed object changes.
      if (!is.null(input$dataInput))
      {
        if (!is.null(globalVars$DataListe))
          if (length(globalVars$DataListe) > 0)
            shinyjs::enable("saveFCS")
      }
    })

    # changes probably want to be made to exactly that function.)
    output$saveFCS <- downloadHandler(
      filename = function() {
        #F:\Subversion\PRO\Research\BlutVsKMA2020\02Marburg\99Anonymize
        paste("Bitte_ID_passendFuerLaborbuchEingeben", ".fcs", sep = "")
      },
      content = function(file) {
        # convert the Output to a proper .fcs format
        tryCatch({
          print(globalVars$DataListe$Extension)
          if(globalVars$DataListe$Extension=='fcs'){
            WriteFCS_Anonymized(file,RawFrame = globalVars$DataListe$RawData,AnnotatedDF = globalVars$DataListe$AnnotatedDataFrame,Header = globalVars$DataListe$Header,shiny = TRUE)
          }else{
            adf=globalVars$DataListe[[2]]$AnnotatedDataFrame
            adf@data$desc=globalVars$DataListe[[1]]$AnnotatedDataFrame@data$desc
            WriteFCS_Anonymized(file,RawFrame = globalVars$DataListe[[2]]$RawData,AnnotatedDF = adf,Header = globalVars$DataListe[[1]]$Header)
          }
          output$info <- renderText({
            "Datei Gespeichert"
          })

        }, error = function(e) {
          str = paste('WriteFCS_Anonymized: Data could not be saved because ', e)
          warning(str)
          output$info <- renderText({
            str
          })

        })
      }
    )

    observeEvent(input$Exit,
                 {
                   stopApp()
                 }
    )

  }#end server


  outputApp=runApp(list(ui = uifacs, server = serverfacs))
return(outputApp)
}
