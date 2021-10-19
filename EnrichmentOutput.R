######################## Enrichment #####################

### Function to display an enrichment plot given one file and one keyword
EnrichmentPlot <- function(selectedFile, selectedKeyword){
  selected_by_key <- keywords %>% 
    filter(Keywords %like% selectedKeyword) %>% 
    select(Gene_name)
  
  # Select mouse gene names associated to the selected human genes
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
  rep(1:length(keywords), each = length(datalist))# These two lines are to work with the mapply function to get the right number of plots
  )
})
