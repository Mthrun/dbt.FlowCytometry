AnonymizeFACSdata=function(){
#AnonymizeFACSdata()
#Anonymize FACS data
#Interactive tool for Anonymization of FACS data (either *.fcs or *.lmd)

#INPUT
# none
#OUTPUT
# an *.fcs file is written out
#
#
#If *.lmd format is chosen, marburg style is assumed. This means the spillover matrix and the data is in the second dataset but the names of the parameters in the first dataset. Otherwise please use \code{\link{ReadFCS_FlowCompensated}} and \code{\link{WriteFCS_Anonymized}} manually.

#author: Michael Thrun



# technical note: Due to security reasons the output file path cannot be the file input path with usual shiny. However, in a future version shinyFiles could be used so that the choice for output file paht is automatically the same that input file path

  #library(shiny)
  #library(shinyjs)
  #library(shinyFiles) #ToDo
  # in the running environment global vars to be used
  globalVars <- reactiveValues(
    DataListe = NULL,
    currentpath=NULL #used later in shinyFiles
  )

  ## Shiny Oberflaeche ----

  # Define UI for application
  uifacs <- fluidPage(

    # Application title
    titlePanel("Anonymize FACS Data"),

    # Sidebar with a slider input for loading data and exit
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

      # Show text output to user what happens, or errors warnings
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

  # Define server logic 
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
        #print(globalVars$DataListe$Extension)
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
          #print(globalVars$DataListe$Extension)
          if(globalVars$DataListe$Extension=='fcs'){
            WriteFCS_Anonymized(file,RawFrame = globalVars$DataListe$RawData,AnnotatedDF = globalVars$DataListe$AnnotatedDataFrame,Header = globalVars$DataListe$Header,shiny = TRUE)
          }else{
		  #uebername der korrekten variablen bezeichnung aus erstem datensatz in 2ten datensatz
            adf=globalVars$DataListe[[2]]$AnnotatedDataFrame
            desc=globalVars$DataListe[[1]]$AnnotatedDataFrame@data$desc
			#wir wissen dann aber nichtmehr ob lin log usw, alsoe info loeschen
            desc=gsub(pattern = 'LIN','',desc)
            desc=gsub(pattern = 'LOG','',desc)
            desc=gsub(pattern = 'INT','',desc)
            desc=gsub(pattern = 'A','',desc)
            adf@data$desc=desc
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

#run the app as a function
  outputApp=runApp(list(ui = uifacs, server = serverfacs))
return(outputApp)
}
