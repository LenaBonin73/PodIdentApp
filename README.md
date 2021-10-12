# PodIndentApp
Repository of the PodIndent app

PodIndent app is a R shiny application made to visualize, explore and compare data from different studies about podocytes. \\
The user can :
1) Compare data, in this case he has 3 possibilities : 
  - Compare them using percentiles : in this case a barcode plot is displayed for each gene under study. There should be one vertical line per dataset in which the gene is found. However, all lines might not be vizible without zooming in.
  - Compare them using normalized fold-change and vizualize it using lollipop plot : in this case there is one plot per gene under study. 
  - Compare them using normalized fold-change and vizualize it using a heatmap : the closer to 1 the more red, the closer to -1 the more blue. 0 is grey and missing value wight. In order for the heatmap to be displayed, at least two genes are necessary. 

2) Plot a gene set enrichment : 
The user can select one or more keyword(s) he wants to study and one or more dataset. Then an enrichment plot for each combination keyword/dataset is displayed. This plot is computed using the fgsea package (Korotkevich G, Sukhov V, Sergushichev A (2019). “Fast gene set enrichment analysis.” bioRxiv. doi: 10.1101/060012, http://biorxiv.org/content/early/2016/06/20/060012.)

3) Download the datasets : 
The user can select one dataset to download as CSV, in this case, in addition the first 10 rows of the dataset are displayed in the application. It is also possible to dowload directly all datasets as CSV, in that case they are downloaded in a zip file. 

