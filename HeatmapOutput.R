############# Code for heatmap output ##################

# Select data
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

# Zoom button
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

# Make the heatmap
plot_heatmap <- reactive({
  if(length(dataComp())>=2){
    full_data <- data_radar()
    # Function to deal with missing values
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

#Display heatmap
output$heatmap <- renderPlotly({
  if(length(dataComp())>=2){ # At least 2 genes are needed to display the heatmap
    if(nrow(data_radar())>=2){ # At least 2 genes are needed to display the heatmap
      p <- plot_heatmap()
      ggplotly(p) %>% 
        layout(height = 300+(length(dataComp())*5)) %>% 
        config(
          modeBarButtonsToRemove = list("hoverCompareCartesian", "hoverClosestCartesian", "lasso2d", "select2d"),
          modeBarButtonsToAdd = list(dl_button)
        )
    }
  }
})