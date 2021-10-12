library(shiny)
library(plotly)
library(tidyverse)
library(fgsea)
library(sqldf)
library(data.table)
library(heatmaply)
library(shinyWidgets)
library(DT)
library(zip)

################################# Packages version ###############################
# This shiny app has been developped using R 3.6.3 and the following packages : 

# shinyWidgets_0.6.0 heatmaply_1.1.1    viridis_0.5.1      viridisLite_0.3.0 
# data.table_1.13.2  sqldf_0.4-11       RSQLite_2.2.1      gsubfn_0.7        
# proto_1.0.0        fgsea_1.12.0       Rcpp_1.0.7         forcats_0.5.0     
# stringr_1.4.0      dplyr_1.0.6        purrr_0.3.4        readr_1.4.0       
# tidyr_1.1.2        tibble_3.0.4       tidyverse_1.3.0    plotly_4.9.2.1    
# ggplot2_3.3.5      shiny_1.5.0     
##################################################################################

load("RG_mouse_transcriptome.rdata")
load("Kann_mouse_podocyte_transcriptome.rdata")
load('Boerries_podocyte_transcriptome.rdata')
load('Boerries_podocyte_proteome.rdata')
load('CMS_proteome.rdata')
load('Karaiskos_single_cell.rdata')
load('Chung_single_cell.rdata')
load('Park_single_cell.rdata')
load('Rinschen_proteome.rdata')

files <- c("Boerries_transcriptome",
           "Rinschen_Goedel_transcriptome",
           "Kann_transcriptome",
           "Boerries_proteome",
           "CMS_proteome",
           "Rinschen_proteome",
           "Karaiskos_scell",
           "Chung_scell",
           "Park_scell")

load("keywords.rdata")
load("HOM_AllOrganism.rdata")
load("matchSymbolsHOM.rdata")
load("mergeTable.rdata")

icon_svg_path = "M15.608,6.262h-2.338v0.935h2.338c0.516,0,0.934,0.418,0.934,0.935v8.879c0,0.517-0.418,0.935-0.934,0.935H4.392c-0.516,0-0.935-0.418-0.935-0.935V8.131c0-0.516,0.419-0.935,0.935-0.935h2.336V6.262H4.392c-1.032,0-1.869,0.837-1.869,1.869v8.879c0,1.031,0.837,1.869,1.869,1.869h11.216c1.031,0,1.869-0.838,1.869-1.869V8.131C17.478,7.099,16.64,6.262,15.608,6.262z M9.513,11.973c0.017,0.082,0.047,0.162,0.109,0.226c0.104,0.106,0.243,0.143,0.378,0.126c0.135,0.017,0.274-0.02,0.377-0.126c0.064-0.065,0.097-0.147,0.115-0.231l1.708-1.751c0.178-0.183,0.178-0.479,0-0.662c-0.178-0.182-0.467-0.182-0.645,0l-1.101,1.129V1.588c0-0.258-0.204-0.467-0.456-0.467c-0.252,0-0.456,0.209-0.456,0.467v9.094L8.443,9.553c-0.178-0.182-0.467-0.182-0.645,0c-0.178,0.184-0.178,0.479,0,0.662L9.513,11.973z"

dl_button <- list(
  name = "Download as SVG",
  icon = list(
    path = icon_svg_path,
    transform = "scale(0.84) translate(-1, -1)"
  ),
  click = htmlwidgets::JS("
          function (gd) {
        Plotly.downloadImage(gd, {
        filename: 'svg_download',
        format: 'svg',
        width: gd._fullLayout.width,
        height: gd._fullLayout.height
    })
  }
   "))

###################################### UI ########################################
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
                             source("comparison.R")$value),
                  tabPanel("Enrichment Plot",
                             source("enrichment.R")$value),
                  tabPanel("Info", source('info.R')$value),
                  tabPanel("Download data", source('download.R')$value),
     selected = "Info"
                  )
          )
)

################################## SERVER ########################################
server <- function(input, output, session) {
  
  ######### Enrichment ###########
  
  # Function to display an enrichment plot given one file and one keyword
  EnrichmentPlot <- function(selectedFile, selectedKeyword){
    selected_by_key <- keywords %>% 
      filter(Keywords %like% selectedKeyword) %>% 
      select(Gene_name)
    
    selectedGenes <- mergeTab %>% 
      filter(Symbol_Human %in% selected_by_key$Gene_name) %>% 
      select(Symbol_Mouse)
    
    pathways <- list(keyword = selectedGenes$Symbol_Mouse)
    
    ranks<- selectedFile %>% 
      select(Gene_name, absolute_quanti) %>% 
      arrange(desc(absolute_quanti)) %>% 
      rename(ID = Gene_name, t = absolute_quanti)
    ranks <- setNames(ranks$t, ranks$ID)
    str(ranks)
    
    plotEnrichment(pathways[['keyword']], ranks)
  }
  
  # Update and select data according to the selected file
  dataEnrichment <- eventReactive(input$goEnrich,{
    input$selectedFile
  })
  
  
  # Update selected keyword
  selectedKeyword <- eventReactive(input$goEnrich,{
    input$keyword
  })
  
  # Display multiple gsea plots
  output$gseaMultiple <- renderUI({
    keywords <- selectedKeyword()
    datalist <- dataEnrichment()
    df <- data.frame(i = 1:length(datalist),
                     j = 1: length(keywords))
    
    mapply(function(i,j){
      id <- paste0("plot_", i,'_',j)
      plotlyOutput(outputId = id, height = 280, width = 250)
      
      # get the data and the enrichment plot
      dataI <- get(datalist[i])
      keywordj <- keywords[j]
      p <- EnrichmentPlot(dataI, keywordj)+
        labs(title = paste("Keyword: ",keywordj, "Dataset: ", datalist[i]))
      # render each plot
      output[[id]] <- renderPlotly({
        ggplotly(p)%>% 
          config(
            modeBarButtonsToRemove = list("hoverCompareCartesian", "hoverClosestCartesian", "lasso2d", "select2d"),
            modeBarButtonsToAdd = list(dl_button)
          )
      })
    },
    rep(1:length(datalist), times = length(keywords)),
    rep(1:length(keywords), each = length(datalist))
    )
  })

  
  ############# Comparison #############
  
  # Update selected genes
  dataComp1 <- eventReactive(input$goComp, {
    if(!is.null(input$file1)){
      req(input$file1)
      # when reading semicolon separated files,
      # having a comma separator causes `read.csv` to error
      tryCatch(
        {
          df <- read.csv(input$file1$datapath,
                         header = input$header,
                         sep = input$sep,
                         quote = input$quote)
          genes <- df[,1]
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
    }
    if(length(input$SelectizeGenes)!=0){
      genes <- input$SelectizeGenes
    }
    if(input$GeneList != ""){
      genes <- strsplit(input$GeneList, input$sep1)
      genes <- as.vector(genes[[1]])
    }
    if(input$GeneList == "" & length(input$SelectizeGenes)==0 & is.null(input$file1)){
      genes <- c()
    }
    genes
  })
  
  dataComp2 <- reactive({
    genes <- dataComp1()
    selected_genes <- mergeTab %>% 
      filter(Gene_name %in% genes)
  })
  
  dataComp <- reactive({
    genes <- dataComp2()
    # gene names are Human gene names => we need the mouse name
    selected_genesMouse <- genes %>%
      select(Symbol_Mouse)
    selected_genes<- as.vector(selected_genesMouse$Symbol_Mouse)
  })
  
  # Create dataset for barcode plot
  data_barcode <- reactive({
    # selected genes
    selected_genes <- dataComp()
    
    full_data <- data.frame(Gene_name = character(),
                            percentiles = numeric(),
                            dataset = character())
    for (f in 1:length(files)){
      df <- get(files[f])
      df <- df %>% 
        select(Gene_name, percentiles, dataset) %>%  
        filter(Gene_name %in% selected_genes)%>% 
        arrange(desc(percentiles)) %>% 
        distinct(Gene_name, .keep_all = T)
      full_data <- rbind(full_data, df)
    }
    # transform dataset as factor
    full_data$dataset <- as.factor(full_data$dataset)
    full_data$dataset<- ordered(full_data$dataset, levels = files)
    
    full_data
  })
  
  observeEvent((input$ab), {
    if(length(dataComp())!=0){
      if(nrow(data_barcode())!=0){
    showModal(modalDialog(
      renderPlot({
        plot_barcode() + theme(
          axis.title = element_text(size = 20),
          text = element_text(size = 20),
          plot.title = element_text(size = 26)
        )
      }, height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
      }
    }
  })
  
  plot_barcode <- reactive({
    full_data <- data_barcode()
    full_data$y=1 # because we need a y variable for geom_tile
    
    ggplot(full_data, aes(x=percentiles,y=y))+
      geom_tile(aes(fill=dataset), width = 0.4)+
      facet_wrap(~Gene_name, ncol = 2)+
      ylab("")+
      xlim(c(0,100))+
      theme_bw()+
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                            colour = "white"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "white"))+
      scale_fill_manual(values = c("Chung_scell"="#027B4B", 
                                    "Karaiskos_scell" = "#2DB84F", 
                                    "Park_scell" = "#57CB1D",
                                    "Boerries_transcriptome" = "#5739FC", 
                                    "Kann_transcriptome" = "#398CFC", 
                                    "Rinschen_Goedel_transcriptome" = "#39B5FC",
                                    "Boerries_proteome" = "#FC3939", 
                                    "Rinschen_proteome" = "#FC3992", 
                                    "CMS_proteome" = "#F339FC"))+
      NULL 
  })
  
  # Display barcode plot
  output$barcode <- renderPlotly({
    if (length(dataComp())!=0){
      if(nrow(data_barcode())!=0){
    p <- plot_barcode()
    ggplotly(p, height = 200+(length(dataComp())*20))%>% 
      config(
        modeBarButtonsToRemove = list("hoverCompareCartesian", "hoverClosestCartesian", "lasso2d", "select2d"),
        modeBarButtonsToAdd = list(dl_button)
      )
      }
    }
  })
  
  ####### Heatmap #######
  data_radar <- reactive({
    # selected genes
    selected_genes <- dataComp()
    
    Boerries_proteome_filtered <- Boerries_proteome %>% 
      select(Gene_name, normalized_fold) %>% 
      rename(Boerries_proteome = normalized_fold) %>% 
      filter(Gene_name %in% selected_genes)%>% 
      arrange(desc(abs(Boerries_proteome))) %>% 
      distinct(Gene_name, .keep_all = T)
    
    Boerries_transcriptome_filtered <- Boerries_transcriptome %>% 
      select(Gene_name, normalized_fold) %>% 
      rename(Boerries_transcriptome = normalized_fold) %>% 
      filter(Gene_name %in% selected_genes)%>% 
      arrange(desc(abs(Boerries_transcriptome))) %>% 
      distinct(Gene_name, .keep_all = T)
    
    Chung_filtered <- Chung_scell %>% 
      select(Gene_name, normalized_fold) %>% 
      rename(Chung_scell = normalized_fold) %>% 
      filter(Gene_name %in% selected_genes)%>% 
      arrange(desc(abs(Chung_scell))) %>% 
      distinct(Gene_name, .keep_all = T)
    
    CMS_filtered <- CMS_proteome %>% 
      select(Gene_name, normalized_fold) %>% 
      rename(CMS_proteome = normalized_fold) %>% 
      filter(Gene_name %in% selected_genes)%>% 
      arrange(desc(abs(CMS_proteome))) %>% 
      distinct(Gene_name, .keep_all = T)
    
    # No relative quantification for Kann transcriptome
    
    Karaiskos_filtered <- Karaiskos_scell %>% 
      select(Gene_name, normalized_fold) %>% 
      rename(Karaiskos_scell = normalized_fold) %>% 
      filter(Gene_name %in% selected_genes)%>% 
      arrange(desc(abs(Karaiskos_scell ))) %>% 
      distinct(Gene_name, .keep_all = T)
    
    Park_filtered <- Park_scell %>% 
      select(Gene_name, normalized_fold) %>% 
      rename(Park_scell = normalized_fold) %>% 
      filter(Gene_name %in% selected_genes)%>% 
      arrange(desc(abs(Park_scell))) %>% 
      distinct(Gene_name, .keep_all = T)
    
    Rinschen_filtered <- Rinschen_proteome%>% 
      select(Gene_name, normalized_fold) %>% 
      rename(Rinschen_proteome = normalized_fold) %>% 
      filter(Gene_name %in% selected_genes)%>% 
      arrange(desc(abs(Rinschen_proteome))) %>% 
      distinct(Gene_name, .keep_all = T)
    
    RG_filtered <- Rinschen_Goedel_transcriptome %>% 
      select(Gene_name, normalized_fold) %>% 
      rename(Rinschen_transcriptome = normalized_fold) %>% 
      filter(Gene_name %in% selected_genes)%>% 
      arrange(desc(Rinschen_transcriptome)) %>% 
      distinct(Gene_name, .keep_all = T)
    
    full_data <- merge(Boerries_proteome_filtered, CMS_filtered, "Gene_name", all = T)
    full_data <- merge(full_data, Rinschen_filtered, "Gene_name", all = T)
    full_data <- merge(full_data, Chung_filtered, "Gene_name", all = T)
    full_data <- merge(full_data, Karaiskos_filtered, "Gene_name", all = T)
    full_data <- merge(full_data, Park_filtered, "Gene_name", all = T)
    full_data <- merge(full_data, RG_filtered, "Gene_name", all = T)
    full_data <- merge(full_data, Boerries_transcriptome_filtered , "Gene_name", all = T)
    full_data <- arrange(full_data, Gene_name)
    full_data
  })
  
  observeEvent((input$ph), {
    if(length(dataComp())!=0){
      if(nrow(data_radar())>=2){
    showModal(modalDialog(
      renderPlotly({
        plot_heatmap()
      }),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
      }
    }
  })
  
  plot_heatmap <- reactive({
    if(length(dataComp())>=2){
    full_data <- data_radar()
    dist_no_na <- function(mat1) {
      mat <- mat1
      for (i in 1:nrow(mat)){
        for (j in 1:ncol(mat)){
          if (is.na(mat[i,j])){
            mat[i,j]<- 100 # we arbitraly decide to assign 100 because it is much higher than every existed number
          }
        }
      }
      edist <- dist(mat)
      return(edist)
    }
    
    heatmaply(full_data[,-1],
                  distfun = dist_no_na,
                  dendrogram = "row",
                  ylab = "",
                  xlab = "",
                  main = "",
                  #scale = "column",
                  margins = c(30,100,0,0),
                  #grid_color = "white",
                  grid_width = 0.00001,
                  #limits = c(-2,2),
                  scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
                    low = "blue", 
                    high = "red", 
                    mid = 'grey92',
                    midpoint = 0,
                    limits = c(min(full_data[,-1], na.rm = TRUE) , max(full_data[,-1], na.rm = TRUE) )
                  ),
                  titleX = FALSE,
                  hide_colorbar = FALSE,
                  branches_lwd = 0.1,
                  labRow = full_data[,1],
                  labCol = colnames(full_data[,-1]),
                  heatmap_layers = theme(axis.line=element_blank()))
    }
  })
  
  ####### Heatmap
  output$heatmap <- renderPlotly({
    if(length(dataComp())>=2){
      if(nrow(data_radar())>=2){
    p <- plot_heatmap()
    ggplotly(p) %>% 
      config(
        modeBarButtonsToRemove = list("hoverCompareCartesian", "hoverClosestCartesian", "lasso2d", "select2d"),
        modeBarButtonsToAdd = list(dl_button)
      )
      }
    }
  })
  
  ########## Lollipop plot ##########
  data_lollipop <- reactive({
    # selected genes
    selected_genes <- dataComp()
    
    Boerries_proteome_filtered <- Boerries_proteome %>% 
      select(Gene_name, normalized_fold) %>% 
      filter(Gene_name %in% selected_genes)%>% 
      arrange(desc(abs(normalized_fold))) %>% 
      distinct(Gene_name, .keep_all = T) %>% 
      mutate(dataset = "Boerries_p", method = "Proteome")
    
    Boerries_transcriptome_filtered <- Boerries_transcriptome %>% 
      select(Gene_name, normalized_fold) %>% 
      filter(Gene_name %in% selected_genes)%>% 
      arrange(desc(abs(normalized_fold))) %>% 
      distinct(Gene_name, .keep_all = T) %>% 
      mutate(dataset="Boerries_t", method = "Transcriptome")
    
    Chung_filtered <- Chung_scell %>% 
      select(Gene_name, normalized_fold) %>%
      filter(Gene_name %in% selected_genes)%>% 
      arrange(desc(abs(normalized_fold))) %>% 
      distinct(Gene_name, .keep_all = T) %>% 
      mutate(dataset = "Chung", method = "Single cell")
    
    CMS_filtered <- CMS_proteome %>% 
      select(Gene_name, normalized_fold) %>%
      filter(Gene_name %in% selected_genes)%>% 
      arrange(desc(abs(normalized_fold))) %>% 
      distinct(Gene_name, .keep_all = T) %>% 
      mutate(dataset="CMS", method = "Proteome")
    
    # No relative quantification for Kann transcriptome
    
    Karaiskos_filtered <- Karaiskos_scell %>% 
      select(Gene_name, normalized_fold) %>% 
      filter(Gene_name %in% selected_genes)%>% 
      arrange(desc(abs(normalized_fold))) %>% 
      distinct(Gene_name, .keep_all = T) %>% 
      mutate(dataset="Karaiskos", method = "Single cell")
    
    Park_filtered <- Park_scell %>% 
      select(Gene_name, normalized_fold) %>% 
      filter(Gene_name %in% selected_genes)%>% 
      arrange(desc(abs(normalized_fold))) %>% 
      distinct(Gene_name, .keep_all = T) %>% 
      mutate(dataset="Park", method = "Single cell")
    
    Rinschen_filtered <- Rinschen_proteome%>% 
      select(Gene_name, normalized_fold) %>% 
      filter(Gene_name %in% selected_genes)%>% 
      arrange(desc(abs(normalized_fold))) %>% 
      distinct(Gene_name, .keep_all = T) %>% 
      mutate(dataset="Rinschen", method = "Proteome")
    
    RG_filtered <- Rinschen_Goedel_transcriptome %>% 
      select(Gene_name, normalized_fold) %>% 
      filter(Gene_name %in% selected_genes)%>% 
      arrange(desc(normalized_fold)) %>% 
      distinct(Gene_name, .keep_all = T) %>% 
      mutate(dataset="Rinschen Goedel", method = "Transcriptome")
    
    full_data <- rbind(Boerries_proteome_filtered, Rinschen_filtered)
    full_data <- rbind(full_data, CMS_filtered)
    full_data <- rbind(full_data, Boerries_transcriptome_filtered)
    full_data <- rbind(full_data, RG_filtered)
    full_data <- rbind(full_data, Chung_filtered)
    full_data <- rbind(full_data, Karaiskos_filtered)
    full_data <- rbind(full_data, Park_filtered)
    
    full_data$dataset<- as.factor(full_data$dataset)
    full_data$dataset<- ordered(full_data$dataset, levels = c("CMS",
                                                              "Rinschen",
                                                              "Boerries_p",
                                                              "Rinschen Goedel",
                                                              "Boerries_t",
                                                              "Park",
                                                              "Karaiskos",
                                                              "Chung"
                                                              ) )
    full_data$method<- as.factor(full_data$method)
    
    full_data
  })
  
  observeEvent((input$pl), {
    if(length(dataComp())!=0){
      if(nrow(data_lollipop())!=0){
    showModal(modalDialog(
      renderPlot({
        plot_lollipop() + theme(
          axis.title = element_text(size = 20),
          text = element_text(size = 20),
          plot.title = element_text(size = 26)
        )
      }, height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
      }
    }
  })
  
  plot_lollipop <- reactive({
    full_data <- data_lollipop()
    ggplot(full_data, aes(x = dataset, y = normalized_fold, color = method)) +
      geom_segment(aes(x = dataset, xend = dataset, y = 0, yend = normalized_fold),
                   color = "gray", lwd = 1) +
      geom_hline(yintercept = 0)+
      facet_wrap(~Gene_name, ncol = 3)+
      geom_point(size = 3, pch = 19, bg = 2) + #18
      coord_flip() +
      theme_bw()+
      ylim(c(-1,1))+
      ylab("Normalized fold change")+
      xlab("")+
      labs(color = "")+
      NULL
  })
  
  output$lollipop <- renderPlotly({
    if(length(dataComp())!=0){
      if(nrow(data_lollipop())!=0){
    p <- plot_lollipop()
    ggplotly(p,
             height = 200+(length(dataComp())*40)) %>% 
      config(
        modeBarButtonsToRemove = list("hoverCompareCartesian", "hoverClosestCartesian", "lasso2d", "select2d"),
        modeBarButtonsToAdd = list(dl_button)
      )
      }
    }
  })
  
  ############## Warning text ##############
  
  # Common text for the three plots of Compare Datasets
  texta <- reactive({
    missing_genes <- setdiff(dataComp1(), dataComp2()$Gene_name)
    if(length(dataComp1())==0){
      texta <- "You must select at least one gene"
    }
    else if(length(missing_genes)>0){
      missing <- paste(missing_genes, collapse = ", ")
      texta <- paste("The following genes are not written correctly : ", missing,
                     "Please make sure you wrote them using the human gene symbol. 
      If you do not know it, try finding it in the gene list.")
    }
    else {texta <- ""}
    texta
  })
  
  ### Text for the heatmap ###
  output$heatmapText <- renderText({
    # Selected genes that exist but are not found in our datasets 
    not_found <- setdiff(dataComp(), unique(data_radar()$Gene_name) )
    # If less than 2 selected genes, heatmap not displayed
    if(length(dataComp1())<=1){
      texta <- "There are less than 2 selected genes. Select at least 2 genes to display the heatmap."
    }
    # We can display a heatmap and need texta()
    else {
      texta <- texta()
    }
    
    # text for the not_found genes : send a warning 
    if (length(not_found)>0){
      missing <- paste(not_found, collapse = ", ")
      textb <- paste("No data has been found in the dataset for the following genes even though they are
                     written correctly : ", missing)
    }
    else{ textb <- ""}
    
    #If after filtering less than 2 genes are found, heatmap cannot be displayed
    if(length(unique(data_radar()$Gene_name))<2 & length(dataComp1())>=2){
      textc <- "Less than 2 genes have been found so the heatmap cannot be displayed."
    }
    else{textc <- ""}
    
    #Text to be returned
    HTML(paste(texta, textb, textc, sep = '<br/><br/>'))  
  })
    
  ### Text barcode ###
  output$barcodeText <- renderText({
    texta <- texta()
    # Selected genes that exist but are not found in our datasets => send a warning
    not_found <- setdiff(dataComp(), unique(data_barcode()$Gene_name) )
    if (length(not_found)>0){
      missing <- paste(not_found, collapse = ", ")
      textb <- paste("No data has been found in the dataset for the following genes even though they are
                     written correctly : ", missing)
    }
    else{ textb <- ""}
    # Text to be returned
    HTML(paste(texta, textb, sep = '<br/><br/>'))
  })
  
  ### Text lollipop ###
  output$lollipopText <- renderUI({
    texta <- texta()
    # Selected genes that exist but are not found in our datasets => send a warning
    not_found <- setdiff(dataComp(), unique(data_lollipop()$Gene_name) )
    if (length(not_found)>0){
      missing <- paste(not_found, collapse = ", ")
      textb <- paste("No data has been found in the dataset for the following genes even though they are
                     written correctly : ", missing)
    }
    else{ textb <- ""}
    #Text to be returned
    HTML(paste(texta, textb, sep = '<br/><br/>'))
  })
  
  ##### Original datasets #####
  output$firstrows <- renderDataTable({
    data <- get(input$datasetD)
    datatable(
      data,
      caption = "First ten rows of the dataset",
      options = list(
        dom = '<"top">rt',
        scrollX=TRUE, 
        scrollCollapse=TRUE))
  })
  
  output$goDownload <- downloadHandler(
    filename = function() {
      paste(input$datasetD, ".csv", sep = "")
    },
    content = function(file) {
      data <- get(input$datasetD)
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$downloadALL <- downloadHandler(
    filename = function(){
      "origninal_datasets.zip"
    },
    content = function(file){
      #go to a temp dir to avoid permission issues
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      filesIn <- c()
      
      AllData <- files
      #loop through the sheets
      for (i in 1:length(AllData)){
        #write each sheet to a csv file, save the name
        fileName <- paste(AllData[i],".csv",sep = "")
        data<- get(AllData[i])
        print(head(data))
        write.csv(data, fileName, row.names = FALSE)
        filesIn <- c(filesIn, fileName)
      }
      #create the zip file
      zip(file,filesIn)
    },
    contentType = "application/zip"
  )
  
  
  
  
} # close server function

# Run the application 
shinyApp(ui = ui, server = server)

################## Error when deploying the app : ################################### 

#Erreur : Unhandled Exception: Child Task 1018988132 failed: 
#Error parsing manifest: Unable to determine package source for Bioconductor 
#package BiocParallel: Repository must be specified
# if this error is yield when trying to deploy the app, then do the following : 
#> options(repos = BiocInstaller::biocinstallRepos())
#> getOption("repos")
#####################################################################################
