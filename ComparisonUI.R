source("global.R")

fluidRow(
  column(3,
         style = "background-color: #E8E8E8",
          ##put input boxes here
          div(style="display: inline-block;vertical-align:top; width: 200px;",
                        strong("Select genes"), 
                        selectizeInput(
                          inputId = "SelectizeGenes",
                          label = "",
                          choices = listOfGenes,
                          selected = NULL,
                          multiple = TRUE
                        )),
                    br(),
                    div(style="display: inline-block;vertical-align:top; width: 200px;",
                        textAreaInput(
                          inputId = "GeneList",
                          label = "Insert a list of gene names",
                          value = "",
                          width = '100%',
                          height = '1000%',
                          placeholder = "NPHS1 NPHS2 ACTN4 PECAM1"
                        )),
                    br(),
                    radioButtons("sep1", "Separator used in the list of genes",
                                 choices = c(Space = " ",
                                             Tab = "\n",
                                             Comma = ","),
                                 selected = " "),
                    br(),
                    actionBttn('goComp', 'GO', color = "primary", style = "fill", size = "sm")
         ),
  
  # output
  
  column(9,
         tabsetPanel(
           tabPanel("Percentiles", htmlOutput("barcodeText"),
                    tabPanel("barcodePlot", plotlyOutput("barcode")),
                    div(
                      style = "position: absolute; right: 0.5em; bottom: 0.5em;",
                      actionBttn(
                        inputId = "ab",
                        icon = icon("search-plus", class = "opt"),
                        style = "fill",
                        color = "danger",
                        size = "xs"
                      )
                    )),
           tabPanel("Normalized Fold-changes Lollipop", htmlOutput("lollipopText"),
                    tabPanel("lollipopPlot", plotlyOutput("lollipop")),
                    div(
                      style = "position: absolute; right: 0.5em; bottom: 0.5em;",
                      actionBttn(
                        inputId = "pl",
                        icon = icon("search-plus", class = "opt"),
                        style = "fill",
                        color = "danger",
                        size = "xs"
                      )
                    )),
           tabPanel("Normalized Fold-changes Heatmap", htmlOutput("heatmapText"),
                    tabPanel("heatmapPlot", plotlyOutput("heatmap")),
                    div(
                      style = "position: absolute; right: 0.5em; bottom: 0.5em;",
                      actionBttn(
                        inputId = "ph",
                        icon = icon("search-plus", class = "opt"),
                        style = "fill",
                        color = "danger",
                        size = "xs"
                      )
                    ))
         ))
)

#code couleur
#protéome : #FC3939 FC3992 F339FC
# transcriptome 5739FC 398CFC 39B5FC
# scell 027B4B 2DB84F 57CB1D

# Ex list genes
# NPHS1 NPHS2 ACTN1 ACTN2 ACTN3 ACTN4 PECAM1 Pecam PDGFRB PDGFRA PDGFA PDGFB PDGFC PDGFD