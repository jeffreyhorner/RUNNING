# RUNNING

This project is a repository of R code to create running/racing plots. Currently only finishing time and elevation data are used to create the plots.

# Getting Started

* Clone the RUNNING repo
* Download and install the R packages plyr, ggplot2, knitr, markdown, and XML 
* Then start R in the top level directory of the RUNNING directory, and execute the following commands:

    source('plots.R')
    createReport('reports/Report_template.Rmd')

# Write your own reports

Copy reports/Report_template.Rmd to reports/My_Report.Rmd and start creating your own reports.
