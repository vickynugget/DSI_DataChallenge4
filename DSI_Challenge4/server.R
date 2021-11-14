library(shiny)
library(medicaldata)
library(snakecase)
library(plotly)
library(tidyverse)
library(RColorBrewer)
library(janitor)
library(gtsummary)
library(gt)

# read in the data
blood_storage <- medicaldata::blood_storage
# data wrangling
blood_storage_c <- blood_storage %>%
    clean_names() %>% # format column names
    # format values of the data from numeric into text
    mutate(recurrence = ifelse(recurrence == 1, 'Yes', 'No')) %>%
    mutate(fam_hx = ifelse(fam_hx == 1, 'Yes', 'No')) %>%
    mutate(rbc_age_group = case_when(
        rbc_age_group == 1 ~ '≤ 13 days (Younger)', 
        rbc_age_group == 2 ~ '13 - 18 days (Middle)', 
        rbc_age_group == 3 ~ '≥18 days (Older)')) %>%
    mutate(aa = ifelse(aa == 1, 'African American', 'non-African American')) %>%
    mutate(censor = ifelse(censor == 1, 'Yes', 'No')) %>%
    mutate(bn = ifelse(bn == 1, 'Yes', 'No')) %>%
    mutate(preop_therapy = ifelse(preop_therapy == 1, 'Yes', 'No')) %>%
    mutate(organ_confined = ifelse(organ_confined == 1, 'Yes', 'No')) %>%
    mutate(t_stage = ifelse(t_stage == 1, 'stage T1-T2a', 'stage T2b-T3')) %>%
    mutate(t_vol = case_when(
        t_vol == 1 ~ 'Low', 
        t_vol == 2 ~ 'Medium', 
        t_vol == 3 ~ 'Extensive')) %>%
    mutate(b_gs = case_when(
        b_gs == 1 ~ 'score 0 - 6', 
        b_gs == 2 ~ 'score 7', 
        b_gs == 3 ~ 'score 8 - 10')) %>%
    mutate(s_gs = case_when(
        s_gs == 1 ~ 'Not assigned', 
        s_gs == 2 ~ 'No residual disease or score 0 - 6', 
        s_gs == 3 ~ 'score 7', 
        s_gs == 4 ~ 'score 8 - 10')) %>%
    mutate(any_adj_therapy = ifelse(any_adj_therapy == 1, 'Yes', 'No')) %>%
    mutate(adj_rad_therapy = ifelse(adj_rad_therapy == 1, 'Yes', 'No')) 

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

# Define server logic
shinyServer(function(input, output) {
    
    # create a barplot
    output$barPlot <- renderPlot({
        
        blood_storage_c %>%
            # filter out NA factors
            filter(!is.na(!!sym(input$variable))) %>%
            # group by the input variable
            group_by(!!sym(input$variable)) %>%
            # calculate the recurrence rate based on the input variable
            summarize(recurrence_rate = sum(recurrence=='Yes')/length(recurrence)) %>%
            # initialize ggplot: x = input variable, y = recurrence rate
            ggplot(aes(x = as.factor(!!sym(input$variable)), 
                       y = recurrence_rate, 
                       # create in-graph label
                       label = paste(round(recurrence_rate*100, 2), '%'))) + 
            # create a bar plot
            geom_bar(stat = 'identity', width = 0.3, fill = 'coral') + 
            labs(x = names(bar_var[bar_var==input$variable]), # relabel x axis
                 y = 'Recurrence rate', # relabel y axis
                 # format the title
                 title = paste('Recurrence rate vs', 
                               to_any_case(names(bar_var[bar_var==input$variable]), 'title'))) + 
            # resize the title
            theme(plot.title = element_text(size=22), 
                  axis.text = element_text(size=13), 
                  axis.title = element_text(size=15)) + 
            # add in-graph label
            geom_text() + 
            # format recurrence rate into percentage
            scale_y_continuous(labels = scales::percent)
        


    })
    
    # create a plotly scatter plot
    output$scatter <- renderPlotly({
        
        # read the input time range
        df <- blood_storage_c
        df <- df[df$time_to_recurrence>input$time[1] & df$time_to_recurrence<input$time[2],]
        
        
        # create a scatter plot
        scatter.plot <- df %>%
          # filter out NA values
          filter(!is.na(!!sym(input$y))) %>%
          # initialize ggplot: x = time to recurrence, y = input variable
          ggplot(aes_string(x='time_to_recurrence', y = input$y)) +
              # create a scatter plot by recurrence
              geom_point(aes(color=as.factor(recurrence))) + 
              labs(x = 'Time to recurrence', # relabel x axis
                   y = names(scatter_var[scatter_var==input$y]), # relabel y axis
                   # format the title
                   title = paste(to_any_case(names(scatter_var[scatter_var==input$y]), 'title'), 
                                 'versus Time to recurrence')) + 
              # change color and legend title
              scale_color_brewer(name = "Recurrence", palette = "Reds") + 
              # resize the titles
              theme(plot.title = element_text(size=18), 
                    legend.text = element_text(size=10), 
                    legend.title = element_text(size=13))
        # make a plotly plot
        ggplotly(scatter.plot)
    })
    
    # table
    x <- reactive({input$table1})
    
    output$table <- render_gt({
      
      df <- blood_storage_c %>%
        select(x(), recurrence) 
      
      table1 <- df %>%
        tbl_summary(by = recurrence, 
                    statistic = list(all_continuous() ~ "{mean} ({p25}, {p75})", 
                                     all_categorical() ~ "{n} ({p}%)")) %>% 
        add_p() %>%
        as_gt() 
   
    })
    

})
