source("global.R")

##################################################################################
###################################### UI ########################################
##################################################################################

ui <- fluidPage(

   shinyUI(
     navbarPage( "PodIdent App",
     tags$style(HTML(" 
        .navbar-default .navbar-brand {color: white;}
        .navbar-default .navbar-brand:hover {color: white;}
        .navbar { background-color: #399DFC ;}
        .navbar-default .navbar-nav > li > a {color:black;}
        .navbar-default .navbar-nav > .active > a,
        .navbar-default .navbar-nav > .active > a:focus,
        .navbar-default .navbar-nav > .active > a:hover {color: white;background-color: #0184DE;}
        .navbar-default .navbar-nav > li > a:hover {color: black;background-color:#33AEFE; text-decoration:underline;}
                  ")),
         
                  tabPanel("Compare datasets",
                             source("ComparisonUI.R")$value),
                  tabPanel("Enrichment Plot",
                             source("EnrichmentUI.R")$value),
                  tabPanel("Download data", source('DownloadUI.R')$value),
                  tabPanel("Info", source('InfoUI.R')$value),
     selected = "Info"
                  )
          )
)

##################################################################################
################################## SERVER ########################################
##################################################################################

server <- function(input, output, session) {
  
  ########################### Enrichment ###########################
  source("EnrichmentOutput.R", local = T)
  
  ########################## Comparison ############################
  
  ### Filter selected genes ###
  
  # Update selected genes
  dataComp1 <- eventReactive(input$goComp, {
    # selected in the list
    if(length(input$SelectizeGenes)!=0){
      genes <- input$SelectizeGenes
    }
    # written manually
    if(input$GeneList != ""){
      genes <- strsplit(input$GeneList, input$sep1)
      genes <- as.vector(genes[[1]])
    }
    # no gene selected
    if(input$GeneList == "" & length(input$SelectizeGenes)==0){
      genes <- c()
    }
    genes
  })
  
  # Check selected genes exist 
  dataComp2 <- reactive({
    genes <- dataComp1()
    selected_genes <- mergeTab %>% 
      filter(Gene_name %in% genes)
  })
  
  # Select corresponding mouse gene names
  dataComp <- reactive({
    genes <- dataComp2()
    # gene names are Human gene names => we need the mouse name
    selected_genesMouse <- genes %>%
      select(Symbol_Mouse)
    selected_genes<- as.vector(selected_genesMouse$Symbol_Mouse)
  })
  
  
  ######## Percentile barcode plot ########
  
  source("BarcodeOutput.R", local = T)
  
  ############ Heatmap ##############
  
  source("HeatmapOutput.R", local = T)
  
  ########## Lollipop plot ##########
  
  source("LollipopOutput.R", local = T)
  
  ######################### Original datasets ########################
  
  source("DownloadOutput.R", local = T)
  
  ########################### Warning texts ##########################
  
  source("TextOutput.R", local = T)
  
} # close server function

# Run the application 
shinyApp(ui = ui, server = server)

################## Error when deploying the app : ################################### 

#Erreur : Unhandled Exception: Child Task 1018988132 failed: 
#Error parsing manifest: Unable to determine package source for Bioconductor 
#package BiocParallel: Repository must be specified
# if this error is yield when trying to deploy the app, then do the following : 
#> options(repos = BiocManager::repositories())
#> getOption("repos")
#####################################################################################
