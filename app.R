# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
#rm(list=ls())
#cat("\014") 
library('shiny')
library('sf')
library('ggplot2')
library('rvest')

source('global.r')
source('ui.r', local = TRUE)
source('server.r')

# Run the application 
shinyApp(ui = ui, server = server)