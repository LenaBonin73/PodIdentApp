files <- c("Boerries_transcriptome",
           "Rinschen_Goedel_transcriptome",
           "Kann_transcriptome",
           "Boerries_proteome",
           "CMS_proteome",
           "Rinschen_proteome",
           "Karaiskos_scell",
           "Chung_scell",
           "Park_scell")

fluidRow(
  column(4,
         style = "background-color: #E8E8E8",
         
         ##put input boxes here
         div(style="display: inline-block;vertical-align:top; width: 200px;",
             strong("Select dataset"), 
             selectizeInput(
               inputId = "datasetD",
               label = "",
               choices = files,
               selected = files[1],
               multiple = FALSE
             )),
         br(),
         downloadButton('goDownload', 'Download as CSV', class = "butt"),
         downloadButton("downloadALL", "Download all datasets as CSV", class = "butt"),
         tags$head(tags$style(".butt{background-color:#399DFC;} .butt{color: white;}"))
  ),
  
  # output
  
  column(8,
         dataTableOutput("firstrows")
  )
  
)