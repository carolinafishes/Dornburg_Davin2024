## Scoring GPT output

#setpaths
inputPath<-"~/Dornburg_Davin2024/ScoredDataSheet1/Lesson Checklist - Sheet1.csv"

#load libraries
library(tidyverse)
library(vegan)
library(factoextra)
library(viridis)
library(usedist)

#load data (remember to cut the sums column)
data<-read.csv(inputPath)
readyData<-data[,c(3:52)]
readyData[is.na(readyData)]<-0
#create perfect data column 
Target<-rep(1,25)

###################
###Custom theme
###################
  ##Custom Theme
customTheme <- theme_classic() +
theme(axis.line.y = element_blank(),
axis.ticks.y = element_blank(),
axis.text = element_text(color="black"),
axis.title= element_blank(),
legend.position= "none")

####################
### Anova / pairwise comparisons
####################
#For ANOVA, note I'm using simDf as the dataframe, this needs to be adjusted once we have more scorings
#sum the columns of the matrix
column_sums <- colSums(readyData)
# Extract prefixes and suffixes
prefixes <- gsub("\\.(.*)", "", names(column_sums))
suffixes <- gsub(".*\\.", "", names(column_sums))
# Create the new dataframe
ANOVAdf <- data.frame(Prefix = prefixes, Suffix = suffixes, Sum = column_sums)
# Perform ANOVA using the Prefix as the grouping factor and Sum as the response variable
anova_result <- aov(Sum ~ Prefix, data = ANOVAdf)
# Output results
summary(anova_result)

# Pairwise comparisons with Benjamini-Hochberg correction
pairwise_result <- pairwise.t.test(ANOVAdf$Sum, ANOVAdf$Prefix, p.adjust.method = "BH")
# Print the results
print(pairwise_result)

####################
###Visual of group mean scores
####################

#lets start with a plot of the raw lines
  # Extracting numeric part of the Prefix
ANOVAdf$NumericPrefix <- as.numeric(gsub("X", "", ANOVAdf$Prefix))
# Plot
ANOVAdf%>%
ggplot(aes(x = NumericPrefix, y = Sum, group = Suffix)) + 
  geom_line(aes(linetype = as.factor(Suffix)), color = "lightgray") +  # Line for each suffix group
  geom_point(aes(shape = as.factor(NumericPrefix))) +  # Different shapes for each suffix
  scale_x_continuous(breaks = 1:5, labels = paste("X", 1:5, sep = "")) +  # x-axis labels
  geom_hline(yintercept = 25, color = "blue", linetype = "dashed") +  # Blue target line
  annotate("text", x = 0.5 + 0.5, y = 24.8, label = "Target Score", color = "blue", vjust = 1) +  # Target label
  labs(x = "Prefix", y = "Sum", title = "Overview of Prompt Scores") +
  scale_y_continuous(limits = c(15, 25), breaks=15:25)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  customTheme 

# Group by prefix and summarize
summary_df <- ANOVAdf %>%
  group_by(Prefix) %>%
  summarize(
    MeanSum = mean(Sum),
    LowerQuartile = quantile(Sum, 0.25),
    UpperQuartile = quantile(Sum, 0.75)
  )

#set a palette 
  palette<-c("#D7191C", "#2C7BB6")

#to plot, note that I am making floating error bars using this height variable to adjust the height of error bars above the segments with ticks
height_adjustment <- 0.2
tick_length <- 0.1
summary_df %>%
  mutate(PrefixAdjusted = as.numeric(as.factor(Prefix)) + height_adjustment) %>%
 ggplot(aes(x= MeanSum, y= Prefix, color= MeanSum)) +
  geom_point(size=4) + 
  geom_segment(aes(xend=1, yend= Prefix), size=2) + 
  geom_text(aes(label= MeanSum), color ="white", size=2) + 
  geom_segment(aes(x = LowerQuartile, xend = UpperQuartile, y = PrefixAdjusted, yend = PrefixAdjusted), size = .75) + # Manual error bars
  # Lower quartile ticks
  geom_segment(aes(x = LowerQuartile, xend = LowerQuartile, y = PrefixAdjusted - tick_length/2, yend = PrefixAdjusted + tick_length/2), size = .5) +
  # Upper quartile ticks
  geom_segment(aes(x = UpperQuartile, xend = UpperQuartile, y = PrefixAdjusted - tick_length/2, yend = PrefixAdjusted + tick_length/2), size = .5) +
  labs(title= "Prompt Scores",x="Average Score", y="Prompt Group") + scale_x_continuous("", expand = c(0,0), limits = c(1, 25.1), position = "top") + scale_color_gradientn(colors=palette) +scale_y_discrete(limits = rev(levels(summary_df $Prefix)))+ 
 customTheme 

####################
### Hierarchical Clustering
####################
##Visuals

#with jaccard distance
jac <- vegdist(t(readyData), method = "jaccard")
hc<-hclust(jac)
plot(hc, main = "Hierarchical Clustering with Jaccard Distance", xlab = "", sub = "")

#Visualize heatmap of the distance matrix
colors<-viridis(256, option="D")
labels <- gsub("X", "P", labels(jac))
jac<-dist_setNames(jac, labels)
fviz_dist(jac, order = FALSE, show_labels = TRUE,
  lab_size = NULL, gradient = list(low = colors[1], mid = colors[128], high =
 colors[256]))

####################
### NMDS plot
####################
#transpose data to put it into row form
nmdsData<-t(readyData)
groupings<-as.factor(ANOVAdf$Prefix)

nmds_result <- metaMDS(nmdsData, distance = "bray", k = 2)
plot(nmds_result$points[,1], nmds_result$points[,2], type = 'n', xlab = "NMDS1", ylab = "NMDS2")
points(nmds_result$points, col = as.factor(groupings))
ordiellipse(nmds_result, groups = groupings, kind = "sd", col = 1:length(levels(groupings)), label = TRUE)
#PERMANOVA
adonis_result <- adonis2(nmdsData ~ groupings, method = "bray")
print(adonis_result)
#ANOSIM
anosim_result <- anosim(nmdsData, distance = "bray", grouping=groupings)
print(anosim_result)
plot(anosim_result)

####################
### Trends in missingness
####################
df<-cbind(data[,1:2], readyData)

df_long <- df %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = c("Prefix", "Index"),
    names_sep = "\\.",
    values_to = "Value"
  )
  
  df_summary <- df_long %>%
  group_by(Component, Definition, Prefix) %>%
  summarize(Total = sum(Value, na.rm = TRUE))

  df_summary %>%
  ggplot(aes(x = Prefix, y = Total, fill = Prefix)) +
  geom_col() + 
  facet_wrap(~ Component + Definition, scales = "free_y", ncol = 5) +
  labs(x = "Prefix", y = "Total", title = "Total by Component and Definition") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 8)) +
        ylim(0,10)+
  scale_fill_brewer(palette = "Paired")




