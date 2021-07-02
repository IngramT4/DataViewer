library(shiny)
library(shinydashboard)
library(Hmisc)
library(DT)
library(readxl)
#library(xlsx)
library(shinyBS)
library(shinyjs)
library(stringi)
library(dplyr)

ui <- dashboardPage(
    dashboardHeader(title = "Data Cleaning"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "Home", icon = icon("dashboard")),
            menuItem("Merge Data", tabName = "Merge", icon = icon("ruler")),
            menuItem("View Data", tabName = "View", icon = icon("glasses")),
            menuItem("Deeper Summary",tabName="Summary",icon=icon("tree"))
        )
    ),
    dashboardBody(
        useShinyjs(),
        tags$head(
            tags$style(
                HTML(".shiny-notification {
           position:fixed;
           top: calc(90%);
           left: calc(40%);
           }
           "
                )
            )
        ),
        tabItems(
            tabItem("Home",
                    h1("Home Page"),
                    h3("Welcome to the data cleaning app"),
                    p("Get started by uploading your data. You can upload multiple files at a time by holding ctrl when selecting files. You can also upload multiple times wihtout overwriting previous uploads."),
                    box(
                        fileInput("fileinputcsv","File Path For csv",accept = c("text/csv",
                                                                                "text/comma-separated-values,text/plain",
                                                                                ".csv"),multiple = TRUE)
                    ),
                    box(
                        fileInput("fileinputxl","File Path For Excel", accept = ".xlsx")
                    ),
                    sidebarLayout(
                        sidebarPanel(
                            h3("Change class of selected dataset"),
                            uiOutput("choosedataset"),
                            uiOutput("chngclass"),
                            selectInput("classtype",
                                        "Select a class",
                                        c("Character","Numeric","Date","Factor")),
                            hidden(
                                selectInput("dateselect","Date formats",
                                            c("Y-m-d(1990-01-20)","y-m-d(90-01-20)","Y/m/d(1990/01/20)","y/m/d(90/01/20)",
                                              "d-m-Y(20-01-1990)","d-m-y(20-01-90)","d/m/Y(20/01/1990)","d/m/y(20/01/90)")),
                                textInput("dateinput","Manual date format")),
                            actionButton("butconvert", "Convert to class"),
                            bsPopover("butresetclass","This will reset the classes and any filters in the view tab.",placement="right",trigger="focus")
                        ),
                        
                        
                        mainPanel(
                            h3("Class of selected variables:"),
                            textOutput("checkclass"),
                            textOutput("cnvrtclass")
                        )
                    )
                    
            ),
            tabItem("Merge",
                    h4("Create merged data using uploaded files."),
                    h5("To begin, select two files you would like to merge togther. You must select one or more common columns to merge by. If you want to merge using multiple columns, make sure you have selected them in the order you wish to match them. The first columns selected of each dataset in the 'choose columns to merge by' section will be matched, the second column selected in that same section will be matched and so on. If you do not know the join patterns, here is a link to an explanation: https://statisticsglobe.com/r-dplyr-join-inner-left-right-full-semi-anti"),
                    fluidPage(
                        box(
                            h4("Merge files"),
                            uiOutput("choosefilemerge1"),
                            uiOutput("choosefilemerge2"),
                            radioButtons("join"," ",c("Inner join","Full join","Left join","Right join"))
                        ),
                        box(h4("Choose column(s) to merge by"),
                            uiOutput("choosemergevar1"),
                            uiOutput("choosemergevar2"),
                            textInput("namemerge","Name the merged dataset"),
                            actionButton("butmerge","Merge")
                        )
                    )),
            tabItem("View",
                    sidebarLayout(
                        sidebarPanel(
                            uiOutput("choosefileuse"),
                            actionButton("butchoose","Choose"),
                            radioButtons("chooseaction","Modify data",c("Filter data","Add/remove column")),
                            uiOutput("view"),
                            bsPopover("butview","Filter Table","Filters table based on the criteria given or leave the inputs empty to see current table. The table is saved after every filter, so you are able to filter it many times. To start over press reset table."),
                            uiOutput("filter"),
                            textInput("constraint","Criteria"),
                            radioButtons("choiceint","Choose a constraint",choices = c("Greater","Less","Equal"),selected = "Greater"),
                            actionButton("butview", "Filter"),
                            actionButton("butreset", "Reset Table"),
                            bsPopover("butreset","Reset","Your data will be reset to the original upload, but will maintain any changes to class from the class tab. Press view table to see the resetted table."),
                            h1(" "),
                            hidden(
                                uiOutput("createcol"),
                                textInput("namecolumn","Name column"),
                                actionButton("butcalculate", "Create"),
                                uiOutput("removecol"),
                                actionButton("butremovecol", "Remove column")),
                            actionButton("butcompreset", "Reset to original")
                        ),
                        mainPanel(
                            dataTableOutput("data"),
                            textOutput("class")
                        )
                    ),
                    h4("Compare filtered vs unfiltered data"),
                    sidebarLayout(
                        sidebarPanel(
                            uiOutput("sumryvarsori")),
                        mainPanel(
                            tabsetPanel( type = "tab",
                                         tabPanel("Summary",verbatimTextOutput("summary_original")),
                                         tabPanel("Description",verbatimTextOutput("describe_original")),
                                         tabPanel("Histogram",plotOutput("hist_original")),
                                         tabPanel("Table",verbatimTextOutput("table_original"))
                                         
                            )
                        )
                        
                    ),
                    sidebarLayout(
                        sidebarPanel(
                            uiOutput("sumryvarsfil")),
                        mainPanel(
                            tabsetPanel( type = "tab",
                                         tabPanel("Summary",verbatimTextOutput("summary_filter")),
                                         tabPanel("Description",verbatimTextOutput("describe_filter")),
                                         tabPanel("Histogram",plotOutput("hist_filter")),
                                         tabPanel("Table",verbatimTextOutput("table_filter"))
                                         
                            )
                        )
                        
                    )
            ),tabItem("Summary",
                      h1("Summary of variables by group"),
                      h3("Use this tool to group elements in a variable and see the summary stats for each group."),
                      sidebarLayout(
                          sidebarPanel(
                              uiOutput("sumdata"),
                              uiOutput("group_by"),
                              uiOutput("sumvars"),
                              actionButton("butgroup","Create table"),
                              bsPopover("group_by","Select one or many variables to group by. Any variable that has groups(levels) can be used.",placement="right",trigger="focus"),
                              bsPopover("sumvars", "Select variable to comapre between groups.",placement="right",trigger="focus")
                          ),
                          mainPanel(
                              dataTableOutput("group_by_table")
                          )
                      )
            )
        )
    )
)
