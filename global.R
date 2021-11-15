################################# Packages version ###############################
# This shiny app has been developed using R 3.6.3 and the following packages : 

# zip_2.1.1          DT_0.18            shinyWidgets_0.6.0 heatmaply_1.1.1   
# viridis_0.5.1      viridisLite_0.3.0  data.table_1.13.2  sqldf_0.4-11      
# RSQLite_2.2.1      gsubfn_0.7         proto_1.0.0        fgsea_1.12.0      
# Rcpp_1.0.7         forcats_0.5.0      stringr_1.4.0      dplyr_1.0.6       
# purrr_0.3.4        readr_1.4.0        tidyr_1.1.2        tibble_3.0.4      
# tidyverse_1.3.0    plotly_4.9.2.1     ggplot2_3.3.5      shiny_1.5.0       
##################################################################################

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
library(data.table)

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

files_fold_change <- c("Boerries_transcriptome",
                       "Rinschen_Goedel_transcriptome",
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

listOfGenes <- as.data.table(mergeTab$Gene_name)
listOfGenes <- setNames(listOfGenes, "Gene name")

load("list_keywords.rdata")
list_keywords <- as.data.table(list_keywords)
list_keywords <- setNames(list_keywords, "Keyword")


# New button to download as SVG in plotly
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
