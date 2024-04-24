#setpaths to the folders with each prompts outputs, change to your location
paths<-vector(length=5)
paths[1]<-("~/Dornburg_Davin2024/PromptOutputs/Prompt1")
paths[2]<-("~/Dornburg_Davin2024/PromptOutputs/Prompt2")
paths[3]<-("~/Dornburg_Davin2024/PromptOutputs/Prompt3")
paths[4]<-("~/Dornburg_Davin2024/PromptOutputs/Prompt4")
paths[5]<-("~/Dornburg_Davin2024/PromptOutputs/Prompt5")

#load libraries
library(tidyverse)
library(officer)

###########
#Functions
###########

#get word count function
wordCount<-function(docx){
summary_paragraphs <- docx_summary(docx)
# Filter to get paragraphs and their text content
text_content <- summary_paragraphs %>%
  filter(content_type == "paragraph") %>%
  pull(text) # Extracting text column
# Concatenate all paragraphs into one large text
all_text <- paste(text_content, collapse=" ")
# Split the concatenated text into words and count them
word_count <- strsplit(all_text, "\\s+") %>% # Split text into words based on whitespace
  unlist() %>% # Flatten the list to a vector
  length() # Count the number of words
  return(word_count)
}

#Function to word count all files in a directory
directoryWordCount<-function(directoryPath){
wordfiles<-list.files(directoryPath)
wordCounts<-vector(length=length(wordfiles))
setwd(directoryPath)
for (i in 1:length(wordfiles)){
sample_doc <- read_docx(wordfiles[i])
wordCounts[i]<-wordCount(sample_doc)
}
return(wordCounts)
}
###########

wordCountData<-matrix(nrow=5, ncol=10)
for (i in 1:5){
	wordCountData[i,]<-directoryWordCount(paths[i])
}

#reformat and plot word counts
wordCount_df <- as.data.frame(t(wordCountData))
names(wordCount_df) <- paste("Prompt", 1:5)
wordCount_long <- pivot_longer(wordCount_df, cols = everything(), names_to = "Prompt", values_to = "WordCount")

ggplot(wordCount_long, aes(x = Prompt, y = WordCount, fill = Prompt)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Box Plots of Word Counts by Prompt Type",
       x = "Prompt",
       y = "Word Count")
       
#ANOVA
anova_result <- aov(WordCount ~ Prompt, data = wordCount_long)
summary(anova_result)
#get the means/sd
group_means <- aggregate(WordCount ~ Prompt, data = wordCount_long, FUN = mean)
group_means

# Pairwise comparisons with Benjamini-Hochberg correction
pairwise_result <- pairwise.t.test(wordCount_long$WordCount, wordCount_long$Prompt, p.adjust.method = "BH")
pairwise_result
