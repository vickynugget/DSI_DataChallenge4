#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(gtsummary)
library(shinyWidgets)
library(janitor)
library(gt)

blood_storage <- medicaldata::blood_storage
blood_storage_c <- blood_storage %>%
  clean_names()
# barplot input variables names
bar_var <- c('RBC Age Group' = 'rbc_age_group',
             'African American race' = 'aa', 
             'Tumor volume' = 't_vol',
             'Clinical T category' = 't_stage',
             'Biopsy Gleason score' = 'b_gs',
             'Surgical Gleason score' = 's_gs', 
             'Family history of disease' = 'fam_hx', 
             'Bladder neck positive' = 'bn', 
             'Organ confined' = 'organ_confined', 
             'Preoperative therapy' = 'preop_therapy', 
             'Any adjuvant therapy' = 'any_adj_therapy')
# scatter plot input variables names
scatter_var <- c('Preoperative prostate specific antigen' = 'preop_psa', 
                 'Prostate volume' = 'p_vol', 
                 'Age' = 'age')

table_var <- c('RBC Age Group' = 'rbc_age_group',
               'Age' = 'age', 
               'African American race' = 'aa', 
               'Family history of disease' = 'fam_hx', 
               'Prostate volume' = 'p_vol', 
               'Tumor volume' = 't_vol',
               'Clinical T category' = 't_stage',
               'Biopsy Gleason score' = 'b_gs',
               'Bladder neck positive' = 'bn',
               'Organ confined' = 'organ_confined', 
               'Preoperative prostate specific antigen' = 'preop_psa', 
               'Preoperative therapy' = 'preop_therapy', 
               'Number of allogeneic units' = 'units', 
               'Surgical Gleason score' = 's_gs', 
               'Preoperative therapy' = 'preop_therapy', 
               'Any adjuvant therapy' = 'any_adj_therapy', 
               'Time to biochemical recurrence of prostate cancer' = 'time_to_recurrence')

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # create differnt tabs
  tabsetPanel(
    # barplot tab
    tabPanel("bar", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 # select sidebar with to input the variable of the bar plot
                 selectInput(inputId = "variable",
                             label = "Variable:",
                             choices = bar_var) # choose from the values in bar_var vector
                 ),

               # Show a plot of the generated distribution
               mainPanel(
                 # output the description of dataset
                 h4('Description of dataset'),
                 p("This blood storage dataset contains 316 men who had undergone radical prostatectomy and received 
                 transfusion during or within 30 days of the surgical procedure and had available PSA follow-up data. 
                   There are 20 variables. 
                   The main exposure of interest was RBC storage duration group. 
                   A number of demographic, baseline and prognostic factors were also collected. 
                   The outcome was time to biochemical cancer recurrence. ", style = "font-family: 'times'; font-si16pt"),
                 # output the plot
                 plotOutput("barPlot"), 
                 # output the description of the plot
                 p("This is a bar plot that shows the recurrence rate under different factors. 
                   The x axis is different facts and y axis is the recurrence rate. 
                   Recurrence rate is calculated as the number of recured subjects divided by the total number of subjects in that group. 
                   Users can choose the y axis variable from the side bar. ", style = "font-family: 'times'; font-si16pt")
               )
             )
    ),
    
    #scatter plot tab
    tabPanel("scatter", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 
                 # select sidebar with to input the variable of the scatter plot
                 selectInput(inputId = 'y', 
                             label = 'Varibale', 
                             choices = scatter_var), # choose from the values in scatter_var vector
                 
                 # slider sidebar to input the range of time for the scatter plot
                 sliderInput("time", 
                             label = "Time to recur:",
                             min = 0, max = 103,
                             value = c(20, 70))
                 
                 ),

               # Show a plot of the generated distribution
               mainPanel(
                 # output the plot
                 plotlyOutput("scatter"), 
                 # output the description of the plot
                 p('This is a scatter plot of time of recurrence versus the three continious variables 
                 by the biochemical recurrence of prostate cancer. 
                   The x axis is time of recurrence and y axis is one of the continious variable, group by biochemical recurrence or not. 
                   Users can choose the y axis variable from the side bar and choose the range of time from the slider. '
                   , style = "font-family: 'times'; font-si16pt")
               )
             )
    ), 
    
    
    tabPanel("table", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 
                 # select sidebar with to input the variable of the scatter plot
                 selectizeInput(inputId = "table1", 
                             label = "table1", 
                             choices = table_var, 
                             multiple = TRUE, 
                             selected = 'rbc_age_group')
                 
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 # output the description of the table
                 p('This is a table that presents descriptive statistics of characteristics of the study population stratified by recurrence status. 
                   Mean, 25% and 75% quantiles are calculated for continious variables, 
                   Number of subjects and percentage of each group are calculated for catogical variables. 
                   P values is calculated by either Pearson\' Chi-squared test, Wilcoxon rank sum test and Fisher\' exact test. '
                   , style = "font-family: 'times'; font-si16pt"), 
                 # output the table
                 gt_output('table')
               )
             )
    )
  )
))
