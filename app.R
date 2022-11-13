if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(shiny,
  tidyverse,  lubridate,  pdftools,  mltools,  openxlsx, 
  rstatix,  ggpubr, forcats,  DescTools, MetBrewer,
  cowplot, flextable, janitor, gtsummary, plotly,
  fuzzyjoin, magick, colorfindr, 
  pdfimager,OpenImageR, DT)

options(scipen = 999)

source("fun.R")
####Input####
ui <- fluidPage(
    titlePanel("BlotR"),

    fluidRow(
      column(width = 3,
        fileInput(
          'drop_in',
          "Input PDFs - single page Blot results", 
          multiple = T, 
          accept = "application/pdf"
        ),
        checkboxInput(
          inputId = "is_multiscanner",
          label = "Multiple scanners used - will check strip color\nNB may take a long time",
          value = FALSE
        ),
        actionButton(
          inputId = "run",
          label = "RUN"
        ),
      ),
      column(3,
        textInput(
          inputId = "filter_expressions",
          label = "Names for filtering",
          placeholder = "comma seperated names for excluding",
        ),
        textInput(
          inputId = "qc_expressions",
          label = "Quality Control values",
          placeholder = "comma seperated names for QCs"
        ),
        downloadButton(
          label = "Download all data",
          outputId = "download"
        ),
      ), 
      column(6,
        dataTableOutput("files",height = "250px")
      )),
    # Show tables and plots
    fluidRow(
      column(12,
             tabsetPanel(
               tabPanel("Summary of Tests", DT::dataTableOutput("all_data",height = "1200px")),
               tabPanel("Summary Graph", plotlyOutput("sum_graph",height = "1200px")),
               tabPanel("Repeated",plotlyOutput("repeated_graph",height = "1200px"))
             )))
    
    )
####server####
server <- function(input, output, session) {
  
  processed_data <- eventReactive(input$run,{
    do_run = TRUE
    
    #Variables are defined
    files_list <-input$drop_in$datapath
    file_name <-input$drop_in$name
    is_multiscanner <-input$is_multiscanner
    filter_expressions <-input$filter_expressions
    qc_expressions <-input$qc_expressions
    
    #Dont run unless there are files
    if(is.null(files_list)){do_run = FALSE}
    
    #Run the extraction script
    if(do_run){
      #remove pdfs that are multipage - not supported
      file_df <- tibble(
        file_location = files_list,
        name = file_name
        )%>% 
        rowwise()%>% 
        mutate(
          length = pdf_length(input = file_location)
        ) %>% 
        filter(length == 1) 
      
      #extract text from PDFs and find blot results
      blot_results <- tibble()
      withProgress(
        value = 0,
        message = "Reading Blot PDFs to table",
        {
          for (i in 1:nrow(file_df)) {
            blot_result<-extract_blot_data(
             name = file_df$name[i],
             file_location = file_df$file_location[i]
             )
          
            blot_results<-blot_results %>% bind_rows(blot_result)
          
            incProgress(
             1/nrow(file_df),
             detail = paste0("Reading Blot ",i, " of ", nrow(file_df)))
           }
        }
      )
      
      #Initial sorting/manipulation
      blot_results<-blot_results%>%
        mutate(
          Blot = str_to_lower(Blot),
          Blot = case_when(
            str_detect(Blot, "myositis") ~ "Myositis-Profile",
            str_detect(Blot, "ana profile3") ~ "ENA-Profile-3",
            str_detect(Blot, "ana profile5") ~ "ENA-Profile-5",
            str_detect(Blot, "liver") ~ "Liver-Profile",
            str_detect(Blot, "systemic") ~ "Nucleolar-Profile",
            str_detect(Blot, "neuronal") ~ "Neuronal-Profile",
            str_detect(Blot, "pns") ~ "Neuronal-Profile",
            TRUE ~ NA_character_
          ),
          Name = Name %>% str_to_lower() %>% str_replace_all("[:punct:]", "")
        ) %>% 
        type_convert()
      
      #Multiple scanners used - detect green strip color
      if (is_multiscanner) {
        scanner<-tibble()
        
        #Progress Bar
        withProgress(
          value = 0,
          message = "Detecting strip color",
          {for (i in 1:nrow(blot_results)) {
              
              location<-blot_results$location[i]
              
              scanner_result<-if_else(
                green_calculator(file_location = location)>1,
                true = "Flatbed",
                false = "BlotOne")
              
              scanner<-scanner %>% 
                bind_rows(tibble(scanner = scanner_result))
              
              incProgress(
                1/nrow(blot_results),
                detail = paste0("Blot ",i, " of ", nrow(blot_results)))
              }
            }
          )
        blot_results<-bind_cols(blot_results, scanner)
        
        return(blot_results)
      }
      else{
        blot_results<-blot_results %>% 
          mutate(scanner = "BlotOne")
        return(blot_results)
      }
      
      if(str_detect(filter_expressions,"[:graph:]")){
        filter_terms<-csv_to_filter(filter_expressions = filter_expressions)
        blot_results = exclude_filter(
          blot_results = blot_results,
          filter_terms = filter_terms
        )
        return(blot_results)  }
      
      if(str_detect(qc_expressions,"[:graph:]")){
        filter_terms<-csv_to_filter(filter_expressions = qc_expressions)
        blot_results = qc_filter(
          blot_results = blot_results,
          filter_terms = filter_terms
        )
        return(blot_results)  }else{
          blot_results<- blot_results %>% 
            mutate(qc_sample = NA_character_)
        }
      
      processed_data<-blot_results
      
      return(processed_data)
      
    }else{
      print("Not enough inputs")
      return()
    }
  })
  
  #Show the files uploaded
  output$files<- renderDataTable(input$drop_in)
  
  #Show the data
  output$all_data = renderDataTable({

    processed_data<-processed_data()
    
    data<- processed_data%>% 
      as_tibble() %>%
      pivot_longer(
        cols = Mi2a:Control,
        names_to = "antigen",
        values_to = "band_intensity",
        values_drop_na = T
      ) %>%
      mutate(
        result = case_when(
          band_intensity >50 & scanner == "FlatBed" ~ "3+",
          band_intensity >69 & scanner == "BlotOne" ~ "3+",
          band_intensity >25 & scanner == "FlatBed" ~ "2+",
          band_intensity >34 & scanner == "BlotOne" ~ "2+",
          band_intensity >10 & scanner == "FlatBed" ~ "1+",
          band_intensity >14 & scanner == "BlotOne" ~ "1+",
          TRUE ~ "Negative"
        )
      ) %>%
      group_by(antigen, result) %>% 
      summarise(Count = n()) %>% 
      pivot_wider(names_from = result, values_from = Count,values_fill = 0)
    
    return(data)
    })
  
  #Show a graph of all the results
  output$sum_graph = renderPlotly({
    processed_data<-processed_data() 
    
    data<-processed_data%>% 
      as_tibble()%>%
      pivot_longer(
        cols = Mi2a:Control,
        names_to = "antigen",
        values_to = "band_intensity",
        values_drop_na = T
      ) %>%
      mutate(
        result = case_when(
          band_intensity >9 & scanner == "FlatBed" ~ "Positive",
          band_intensity >14 & scanner == "BlotOne" ~ "Positive",
          TRUE ~ "Negative"
        )
      ) %>% 
      filter(result == "Positive") %>% 
      type_convert() %>% 
      # ggplot()+
      # aes(x = antigen, y = band_intensity)+
      # geom_violin()
    plot_ly(
      x = ~antigen,
      y = ~band_intensity,
      split = ~antigen,
      type = "violin",
      box = list(
        visible = T
      ),
      meanline = list(
        visible = T
      )) %>%
        layout(
          title = "All Blot Results",
          xaxis = list(
            title = "Antigen"
          ),
          yaxis = list(
            title = "Band Intensity",
            zeroline = F
          )
        )

  })
  
  #Show a graph of change over time/duplicated results
  output$repeated_graph = renderPlotly({
    gg_repeated<-processed_data() %>% repeated_graph() %>% ggplotly()
    
  })
  
  output$download = downloadHandler(
    filename = function(){
      paste0("BlotR Result ",now(), ".xlsx")
    }, content = function(file){
      write.xlsx(x = processed_data(),file = file)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server,)
