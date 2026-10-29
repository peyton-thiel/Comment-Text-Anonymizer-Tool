
data <- read_xlsx("//go.salve.edu/pirc$/Comment-Text Anonymizer Tool/Test Files/test_survey_data_edge_cases.xlsx")

common_names <- read.csv("//go.salve.edu/pirc$/Comment-Text Anonymizer Tool/Models & Source Files/US_Baby_Names_1880_2024 (1).csv")
common_names <- common_names %>%
  select(name) %>%
  unique()


model_dir = "//go.salve.edu/pirc$/Comment-Text Anonymizer Tool/Models & Source Files"
ud_model <- udpipe_load_model(list.files(model_dir, pattern = "\\.udpipe$", full.names = TRUE))


anno <- udpipe_annotate(ud_model, data$Response)
anno <- as.data.frame(anno)


## Fallback 2: PROPN detection + name database
propn_tokens <- anno %>%
  filter(upos == "PROPN") %>%
  select(doc_id, entity = token)


# Take all detected proper nouns and only keep them if they appear in this name database
propn_name_matches <- propn_tokens %>%
  filter(entity %in% common_names$name)

states <- read_xlsx("//go.salve.edu/pirc$/Comment-Text Anonymizer Tool/Models & Source Files/US_States.xlsx")

deny_list <- c("Island")


states_vector <- states$State
deny_list <- c(deny_list, states_vector)
