######################## Enrichment #####################

## Create pathway list : list of gene names which are annotated by the keyword
Pathways <- function(selectedKeyword){
  selected_by_key <- keywords %>% 
    filter(Keywords %like% selectedKeyword) %>% 
    select(Gene_name)
  
  # Select mouse gene names associated to the selected human genes
  selectedGenes <- mergeTab %>% 
    filter(Symbol_Human %in% selected_by_key$Gene_name) %>% 
    select(Symbol_Mouse)
  
  pathways <- list(keyword = selectedGenes$Symbol_Mouse)
}

## Create list of gene names in the selected datatafile, ordered by the selected quantification
Ranks <- function(selectedFile, selectedQuanti){
  if(selectedQuanti == "Absolute"){
    ranks<- selectedFile %>% 
      select(Gene_name, absolute_quanti) %>% 
      arrange(desc(absolute_quanti)) %>% 
      rename(ID = Gene_name, t = absolute_quanti)
    ranks <- setNames(ranks$t, ranks$ID)
    str(ranks)
  }
  else{
    ranks<- selectedFile %>% 
      select(Gene_name, normalized_fold) %>% 
      arrange(desc(normalized_fold)) %>% 
      rename(ID = Gene_name, t = normalized_fold)
    ranks <- setNames(ranks$t, ranks$ID)
    str(ranks)
  }
  ranks
}

## Display multiple gsea plots
output$gseaMultiple <- renderUI({
  keywords <- selectedKeyword()
  datalist <- dataEnrichment()
  quanti <- selectedQuanti()
  
  if(quanti=="Normalized fold change" & "Kann_transcriptome"%in% datalist){}
  else{
    # Render 1 plot per keyword/dataset combination
    mapply(function(i,j){
      id <- paste0("plot_", i,'_',j)
      plotlyOutput(outputId = id, height = 280, width = 250)
      
      # get the data and the enrichment plot
      dataI <- get(datalist[i])
      keywordj <- keywords[j]
      pathways <- Pathways(keywordj)
      ranks <- Ranks(dataI, quanti)
      # get plot
      p <- plotEnrichment(pathways[['keyword']], ranks)
      # get ES
      ES <- fgsea(pathways = pathways, stats = ranks, nperm = 50)$ES
      # Add title to the plot
      p <- p + labs(title = paste("Keyword: ",keywordj, "Dataset: ", datalist[i], "ES: ", round(ES,3)))
      # render the plot
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
  }
})