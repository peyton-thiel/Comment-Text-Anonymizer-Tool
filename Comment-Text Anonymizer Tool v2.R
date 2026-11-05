# Install packages 
# install.packages(c("tidyverse", "tidytext", "readxl", "stringr", "shiny", "spacyr", "DT", "shinyWidgets"))
# remove.packages(c("tidyverse", "tidytext", "readxl", "stringr", "shiny", "spacyr", "DT", "shinyWidgets"))

# Potential improvements:
# Use name dictionary + udpipe POS tagging to remove common names that NER might miss
# Implement custom text box for users to tag names that might miss

if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, tidytext, readxl, stringr, shiny, spacyr, DT, shinyWidgets, udpipe, nametagger)

library(tidyverse)
library(tidytext)
library(textclean)
library(readxl)
library(stringr)
library(shiny)
library(DT)
library(shinyWidgets)
library(udpipe)
library(nametagger)



# Preparation #
# Install models and load source files
# nametagger NER model


ner_model <- nametagger_load_model("//go.salve.edu/pirc$/Comment-Text Anonymizer Tool/Models & Source Files/english-conll-140408.ner")




# udpipe model
model_dir = "//go.salve.edu/pirc$/Comment-Text Anonymizer Tool/Models & Source Files"

#look for existing udpipe model, download if none
existing_model <- any(file.exists(model_dir, pattern = "\\.udpipe$", full.names = TRUE))

if (existing_model == TRUE) {
  ud_model <- udpipe_load_model(list.files(model_dir, pattern = "\\.udpipe$", full.names = TRUE))
} else {
  model <- udpipe_download_model(language = "english", model_dir)
  ud_model <- udpipe_load_model(model$file_model)
}


# common name data set
common_names <- read.csv("//go.salve.edu/pirc$/Comment-Text Anonymizer Tool/Models & Source Files/US_Baby_Names_1880_2024 (1).csv")
common_names <- common_names %>%
  select(name) %>%
  unique()

# states data for deny list
states <- read_xlsx("//go.salve.edu/pirc$/Comment-Text Anonymizer Tool/Models & Source Files/US_States.xlsx")



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
        choices = c("School Name" = "school", "Building Names" = "building", "Person Names (Named Entity Recognition)" = "person"),
        selected = c("school", "building", "person") # default
      ),
      # Anonymize button
      actionBttn("anonymize_btn", label = "Anonymize"),
      # Spaces
      br(),
      tags$hr(),
      # Number of replacements
      uiOutput("replacement_summary"),
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
    
  
    ### STAGE 1: Read Excel File
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
    
    
    
    
      ### STAGE 2: Pre anonymize salve and buildings
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
      
      ### STAGE 3: Apply Named Entity Recognition using nametagger for names + fallbacks for better accuracy
      
      
      if ("person" %in% input$anonymize_options) {
      
        incProgress(0.7, detail = "Annotating text...")
        Sys.sleep(0.5)
        
       
        
        # View entities
        anno <- udpipe_annotate(ud_model, x = comments$comment, doc_id = comments$doc_id)
        anno <- as.data.frame(anno)
        
        # Predict names with nametagger
        ner_data <- anno %>%
          rename("text" = "token") %>%
          select(doc_id, sentence_id, text)
        
        preds <- predict(ner_model, ner_data)
        
        
      
        # Create entity list to remove
        person_entities <- preds %>%
          filter(entity == "B-PER") %>%
          distinct(doc_id, term) %>%
          rename(entity = term)
   
      
        
        
        ## Fallback 2: PROPN detection + name database
        propn_tokens <- anno %>%
          filter(upos == "PROPN") %>%
          select(doc_id, entity = token)
        
        
        # Take all detected proper nouns and only keep them if they appear in this name database
        propn_name_matches <- propn_tokens %>%
          filter(entity %in% common_names$name)
        
        
        # Combining with entities gathered from NER
        person_entities <- bind_rows(person_entities, propn_name_matches)
        
        
        
        # Deny list to improve model performance by filtering tagging for known mishandled names
        ### ADD NEW TERMS THAT GET OVER-ANONYMIZED HERE
        deny_list <- c(
          "Island")
        
        
        # adding states to deny list by extracting form excel as a vector then adding to deny list vector
        states_vector <- states$State
        
        # tokenizing state names to cover two worded state names
        states_tokens <- states_vector %>%
        str_split(" ") %>%
          unlist()
        
        
        
        # combining 
        deny_list <- c(deny_list, states_vector, states_tokens)
        
        
        person_entities <- person_entities %>%
          filter(!entity %in% deny_list)
        
        
    
        incProgress(0.8, detail = "Replacing names...")
        Sys.sleep(0.5)
        
        
        
        # Replace by named entities detected 
        comments$comment <- mapply(function(text, cur_id, entities_df) {
      
        # relevant_entities <- entities_df %>% filter(doc_id == "" | doc_id == doc_id)
          
          
          relevant_entities <- entities_df %>%
            filter(doc_id == cur_id)
          
          #ent_names <- unique(relevant_entities$entity) %>%
           # as.character(ent_names) %>%
           # ent_names[!is.na(ent_names)] %>%
           # ent_names[nchar(ent_names) > 0]
            
          
          #if (length(ent_names) == 0) return (text)
          
          
          #ent_names <- ent_names[order(nchar(ent_names), decreasing = TRUE)]
                        
          
          
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
    
    text <- comments$comment
    
    # Count of each category
    school_count   <- sum(str_count(text, "\\[school\\]"))
    building_count <- sum(str_count(text, "\\[building\\]"))
    person_count   <- sum(str_count(text, "\\[person\\]"))
    
    # Total
    total <- school_count + building_count + person_count
    
    
    list(
      total = total,
      school = school_count,
      building = building_count,
      person = person_count
    )
    
    #counts <- stringr::str_count(comments$comment, "\\[.*?\\]")
    #sum(counts)
    

  
  
  })
  
  
  output$replacement_summary <- renderUI({
    req(input$anonymize_btn)
    #paste("Total replacements made:", replacement_count())
    
    counts <- replacement_count()
    
    
    tagList(
      tags$strong(paste("Total replacements:", counts$total)),
      tags$br(),
      paste("School:", counts$school),
      tags$br(),
      paste("Building:", counts$building),
      tags$br(),
      paste("Person:", counts$person)
    )
    
    
  })
  
}




# Initialize App
shinyApp(ui = ui, server = server)


 











#### Old code (version 1 with spacyr) #### 



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
      
      
      


