#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rpivotTable)
library(webshot)
library(htmlwidgets)
webshot::install_phantomjs()
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Interact rpivotTable dynamically with Rmarkdown Reports"),
   mainPanel(
     downloadButton('describe_download',"Download Report",class="butt" ),br(),
     tags$head(tags$style(".butt{background-color:#230682;} .butt{color: #e6ebef;}")),
     radioButtons('format', 'Document format', c('PDF', 'Word'),
                  inline = TRUE),
   tags$head(tags$style(type = 'text/css',  '#Alert{ overflow-x: scroll; }')),
   fluidRow( rpivotTableOutput("PivotTable") )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  PivotData=reactive({
    rpivotTable(data = mtcars, rows = "vs", cols = "gear",aggregatorName="Count" ,  height = "780px",
                hiddenFromAggregators=c("vs","am" ),
                hiddenFromDragDrop=c("mpg", "cyl", "disp",  "hp", "drat",  "wt",  "qsec"),
                ### add onRefresh 
                onRefresh=htmlwidgets::JS("function(config) {  Shiny.onInputChange('myPivotData', config); }")
    )
  }) 
  
  output$PivotTable <- renderRpivotTable({
    PivotData()
  })
  
  pivotRefresh <- reactive({
    items <- input$myPivotData[c("cols","rows","vals", "exclusions","aggregatorName", "rendererName")]
    
    # need to remove the outside list container
    #  for rows and cols
    #  did not test thoroughly but these seemed to be
    #  the only two that require this
    items$cols <- unlist(items$cols,recursive=FALSE)
    items$rows <- unlist(items$rows,recursive=FALSE)
    
    items
    
  })
  
  observe({str(pivotRefresh())})
  
  PivotTable<-reactive({
    ### do ugly global assign ################
    ### after done with Shiny ################
    ### rp available to inspect ##############
    rp <<- do.call(rpivotTable,c(list(data=mtcars),pivotRefresh()))
  })
  
  output$describe_download = downloadHandler(
    filename<- function(){
      paste("Summary",Sys.Date(),switch(
        input$format, PDF = '.pdf', Word = '.docx'
      ),sep = "")
    },
    
    content = function(file) {
      if (input$format=="PDF"){
        #### Progressing indicator
        withProgress(message = 'Download in progress',
                     detail = 'This may take a while...', value = 0, {
                       for (i in 1:15) {
                         incProgress(1/15)
                         Sys.sleep(0.01)
                       }
                       
                       ## End of progression
                       src <- normalizePath('summary_report.Rmd')
                       
                       # temporarily switch to the temp dir, in case you do not have write
                       # permission to the current working directory
                       owd <- setwd(tempdir())
                       on.exit(setwd(owd))
                       file.copy(src, 'summary_report.Rmd', overwrite = TRUE)
                       
                       library(rmarkdown)
                       out <- render('summary_report.Rmd', pdf_document())
                       file.rename(out, file)
                       
                     })
        ### below is the end of pdf content
      }else{
        withProgress(message = 'Download in progress',
                     detail = 'This may take a while...', value = 0, {
                       for (i in 1:15) {
                         incProgress(1/15)
                         Sys.sleep(0.01)
                       }
                       
                       ## End of progression
                       src <- normalizePath('summary_report_word.Rmd')
                       
                       # temporarily switch to the temp dir, in case you do not have write
                       # permission to the current working directory
                       owd <- setwd(tempdir())
                       on.exit(setwd(owd))
                       file.copy(src, 'summary_report_word.Rmd', overwrite = TRUE)
                       
                       library(rmarkdown)
                       out <- render('summary_report_word.Rmd', word_document())
                       file.rename(out, file)
                     })
      }
      
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

