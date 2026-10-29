# Install packages 
# install.packages(c("tidyverse", "tidytext", "readxl", "stringr", "shiny", "spacyr", "DT", "shinyWidgets"))
# remove.packages(c("tidyverse", "tidytext", "readxl", "stringr", "shiny", "spacyr", "DT", "shinyWidgets"))

if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, tidytext, readxl, stringr, shiny, spacyr, DT, shinyWidgets)

library(tidyverse)
library(tidytext)
library(textclean)
library(readxl)
library(stringr)
library(shiny)
library(spacyr)
library(DT)
library(shinyWidgets)

# Install and initialize spacy model
spacy_install()

spacy_download_langmodel(lang_models = "en_core_web_lg")
spacy_initialize(model = "en_core_web_lg")







# Create GUI
ui <- fluidPage(
  # Title
  titlePanel("Comment Anonymizer Tool"),
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      # File input
      fileInput("file", "Upload Excel File", accept = ".xlsx"),
      # Check box to choose what to anonymize
      checkboxGroupInput(
        inputId = "anonymize_options",
        label = "Select what to anonymize:",
        choices = c("School Name" = "school", "Building Names" = "building", "Person Names (Entity Recognition)" = "person"),
        selected = c("school", "building", "person") # default
      ),
      # Anonymize button
      actionBttn("anonymize_btn", label = "Anonymize"),
      # Spaces
      br(),
      tags$hr(),
      # Number of replacements
      tags$strong(textOutput("replacement_summary")),
      # Spaces
      br(),
      # Download button
      downloadButton("download", "Download Anonymized File")
    ),
    # Table preview section
    mainPanel(
      # Header
      h2("Original Comments Preview"),
      # Original Data
      dataTableOutput("original_preview"),
      # Header
      h2("Anonymized Comments Preview"),
      # Anonymized data
      dataTableOutput("anonymized_preview")
    
    )
  )
)


# Create server
server <- function(input, output, session) {
  
  
  original_data <- reactive({
    req(input$file)
    read_excel(input$file$datapath)
  })
  
  output$original_preview <- renderDataTable({
    DT::datatable(original_data(), options = list(pageLength = 10))
  })
  
  
  
  anonymized_data <- eventReactive(input$anonymize_btn, {
    req(input$file)
    
  
    # STAGE 1: Read Excel File
    withProgress(message = "Anonymizing comments...", value = 0, {
      incProgress(0.1, detail = "Reading file...")
      Sys.sleep(0.5)
      comments <- read_excel(input$file$datapath)
    
      
      
      # Standardize column names
      names(comments) <- tolower(trimws(names(comments)))
      
      # Detect comment column with explicit comment like names
      comment_col <- names(comments)[str_detect(names(comments), "comment|response|text|data")][1]
      
      
      # ️Fallback: use first character column excluding ID like column names
      if (is.na(comment_col)) {
        char_cols <- names(comments)[sapply(comments, is.character)]
        
        # Remove columns that look like IDs
        char_cols <- char_cols[!str_detect(char_cols, "^id$|id_|_id$|identifier|key|unique identifier")]
        
        comment_col <- char_cols[1]
        
        
        if (length(char_cols) > 0) {
          # Pick column with longest text
          text_lengths <- sapply(comments[char_cols], function(col) {
            mean(nchar(col), na.rm = TRUE)
          })
          
          comment_col <- char_cols[which.max(text_lengths)]
        }
      }
      
      
      if (is.na(comment_col)) {
        validate(
          need(FALSE,
               "No usable text column found in the uploaded file. Please make sure the file contains open comment/text responses.")
        )
      }
      
      # Rename it so rest of code stays unchanged
      comments <- comments %>%
        rename(comment = all_of(comment_col))
      
      
      # Store original comments for side by side preview
      original_comments <- comments$comment
    
      # Ensure ID column
      if ("id" %in% names(comments)) {
      comments <- comments %>% 
        rename(doc_id = id) %>%
        mutate(doc_id = as.character(doc_id))
      # If there is no ID then one is created using rows    
      } else {
      comments$doc_id <- as.character(paste0("doc", seq_len(nrow(comments))))
        
      }
    
    
    
    
      # STAGE 2: Pre anonymize salve and buildings
      school_ident <- c("Salve Regina", "Salve", "SRU", "Salve Regina University", "Seahawks", "Seahawk")
      buildings <- c("Miley", "Walgreen", "McKillop", "Ochre Court", "Gerety", "O'Hare", "McAuley", "Rodgers")
    
    
      # Highlighted replacements HTML text for anonymized preview
      school_tag <- "<span style='background-color: yellow; font-weight: bold; color: red;'>[school]</span>"
      building_tag <- "<span style='background-color: lightblue; font-weight: bold; color: darkblue;'>[building]</span>"
      person_tag <- "<span style='background-color: lightgreen; font-weight: bold; color: darkgreen;'>[person]</span>"

      
      incProgress(0.3, detail = "Replacing...")
      Sys.sleep(0.5)
      
      
      
      
      # Replace by specified school identifiers first
      
      if ("school" %in% input$anonymize_options) {
        for (id in school_ident) {
      
        comments$comment <- str_replace_all(comments$comment, regex(paste0("\\b", id, "\\b"), ignore_case = T), school_tag)
        }
      }
      
      
      if ("building" %in% input$anonymize_options) {
        for (id in buildings) {
      
        comments$comment <- str_replace_all(comments$comment, regex(paste0("\\b", id, "\\b"), ignore_case = T), building_tag)
        }
      } 
    
    
      
      
      
      
      incProgress(0.6, detail = "Running NER...")
      Sys.sleep(0.5)
      # STAGE 3: Apply Named Entity Recognition for names
      
      
      if ("person" %in% input$anonymize_options) {
        parsed <- spacy_parse(comments$comment, entity = TRUE)
    
        # View entities
        person_entities <- entity_extract(parsed) %>%
          filter(entity_type == "PERSON") %>%
          distinct(.keep_all = TRUE) %>%
          select(doc_id, entity) 
    
    
        incProgress(0.8, detail = "Replacing names...")
        Sys.sleep(0.5)
        # Replace by named entities detected 
        comments$comment <- mapply(function(text, doc_id, entities_df) {
      
          relevant_entities <- entities_df %>% filter(doc_id == "" | doc_id == doc_id)
          for (name in relevant_entities$entity) {
            text <- str_replace_all(text, regex(paste0("\\b", name, "\\b"), ignore_case = TRUE), person_tag)
        
        
          }
        return(text)
        }, comments$comment, comments$doc_id, MoreArgs = list(entities_df = person_entities))
        comments
      }  
      
      
      incProgress(1, detail = "Done!")
      Sys.sleep(0.5)
      
      
      # Get replacement count
      comments$comment[is.na(comments$comment)] <- ""
      count <- sum(stringr::str_count(comments$comment, "\\[.*?\\]"))
      
      # Show alert with count
      sendSweetAlert(
        session,
        title = "Success!",
        text = paste("Total replacements made:", count),
        type = "success"
      )
      
    
      # Return comments
      return(comments)
    
  })
}) 

  
  #Anonymized Preview with highlighting
  output$anonymized_preview <- renderDataTable({
    req(input$anonymize_btn)
    DT::datatable(anonymized_data(), escape = FALSE, options = list(pageLength = 10))
    
    
  })
  
  # Clean download
  
  output$download <- downloadHandler(
    filename = function() {
      "anonymized OUTPUT.xlsx"
    },
    content = function(file) {
      
      clean_comments <- anonymized_data()
      clean_comments$comment <- stringr::str_remove_all(clean_comments$comment, "<[^>]+>")
      
      writexl::write_xlsx(clean_comments, path = file)
    }
  )
  # Count of words replaced 
  replacement_count <- reactive({
    comments <- anonymized_data()
    comments$comment[is.na(comments$comment)] = ""
    counts <- stringr::str_count(comments$comment, "\\[.*?\\]")
    sum(counts)
    

  
  
  })
  
  
  output$replacement_summary <- renderText({
    req(input$anonymize_btn)
    paste("Total replacements made:", replacement_count())
  })
  
}




# Initialize App
shinyApp(ui = ui, server = server)


 











#### Old code #### 



#data <- read_excel("//go.salve.edu/pirc$/NSSE/Open Comment Analysis/2025/Anonymize test 2.xlsx") 

# Define identifiers
#school_ident <- c("Salve Regina", "Salve", "SRU", "Salve Regina University", "Seahawks", "Seahawk")
#buildings <- c("Miley", "Walgreen", "McKillop", "Ochre Court", "Gerety", "O'Hare", "McAuley", "Rodgers")


# Replace
#for (id in school_ident) {
#  data$comment <- str_replace_all(data$comment, regex(id, ignore_case = T), "[school]")
#}

#for (id in buildings) {
#  data$comment <- str_replace_all(data$comment, regex(id, ignore_case = T), "[building]")
#}

# Export 
# write_excel_csv(data, "//go.salve.edu/pirc$/NSSE/Open Comment Analysis/2025/Anonymize test OUTPUT.csv", append = F)




# Apply NER
#parsed <- spacy_parse(data$comment, entity = TRUE)

# View entities
#person_entities <- entity_extract(parsed) %>%
# filter(entity_type == "PERSON") %>%
# distinct(.keep_all = TRUE) %>%
# select(doc_id, entity) 


# Names not picked up be NER
#missed_names <- parsed %>% 
#  filter(pos == "PROPN" & entity == "") %>%
#  select(doc_id, token) %>% 
#  distinct(token, .keep_all = TRUE) %>%
#  rename("entity" = "token")


# Combine all entities
#all_names <- rbind(person_entities, missed_names)


### replace
# Create function
#anonymize_all_names <- function(text, doc_id, entities_df) {

#  relevant_entities <- entities_df %>% filter(doc_id == "" | doc_id == doc_id)
#  for (name in relevant_entities$entity) {
#    text <- str_replace_all(text, regex(name, ignore_case = TRUE), "[person]")
# 
#    
#     }
#  return(text)
#}

#data$comment <- mapply(anonymize_all_names, 
#                           data$comment, 
#                           data$ID,
#                      MoreArgs = list(entities_df = all_names))
      
      
      


