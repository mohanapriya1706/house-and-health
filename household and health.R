library(lattice)
library(rmarkdown)
library(dplyr)
library(readxl)
df <-read_excel("e last nivi.xls")
head(df)
head(df)
names(df)
glimpse(df)
sapply(df, function(x) sum(is.na(x)))
class(df$Households_practicing_open_defecation)


df$Households_practicing_open_defecation <- as.numeric(df$Households_practicing_open_defecation)

df$Chronic_Obstructive_Pulmonary_Disease <- as.numeric(df$Chronic_Obstructive_Pulmonary_Disease)

df$Depression <- as.numeric(df$Depression)

df$Prevalence_of_diagnosed_Cancer_or_Malignant_Tumor <- as.numeric(df$Prevalence_of_diagnosed_Cancer_or_Malignant_Tumor)

df$Glaucoma <- as.numeric(df$Glaucoma)
summary(df$MeanHouseholdSize)
summary(df$Households_with_improved_sanitation)
summary(df$Households_practicing_open_defecation)
summary(df$Currently_smoking)
df_numeric <- select_if(df, is.numeric)
n <- ncol(df_numeric)

for (col in colnames(df_numeric)) {
  hist(df[[col]], main = paste("Histogram of", col), xlab = "Values", col = "pink")}

df %>% 
  select(Indicators,Households_exposed_to_indoor_pollution) %>%
  filter(Households_exposed_to_indoor_pollution > 15)

df %>% 
  select(Indicators,Currently_smoking) %>%
  filter(Currently_smoking > 30)

df %>% 
  select(Indicators,Currently_consuming_tobacco) %>%
  filter(Currently_consuming_tobacco > 40)

#hist(df$MeanHouseholdSize, col = "pink")

histogram(df$Households_with_improved_sanitation,col='darkgreen')

df %>% 
  select(Indicators,Households_with_improved_sanitation) %>%
  filter(Households_with_improved_sanitation < 62)

histogram(df$Households_practicing_open_defecation,col='red')
df %>% 
  select(Indicators,Households_practicing_open_defecation) %>%
  filter(Households_practicing_open_defecation > 40)

df_scatter <- df %>%
  select(Households_practicing_open_defecation,Households_exposed_to_indoor_pollution,Yoga_practice_meditation_asana_and_pranayama,Depression,Currently_smoking)
my_colors <- c("red", "blue")
for (col in colnames(df_scatter)) {
  boxplot(df[,col], main = paste("Boxplot of", col), xlab = col, ylab = "Value",col = my_colors)
}

typ_def <- df %>%
  select(Households_practicing_open_defecation,Typhoid,Indicators) 

xyplot(Typhoid~Households_practicing_open_defecation,typ_def)


xyplot(Households_exposed_to_indoor_pollution ~ Chronic_Obstructive_Pulmonary_Disease + Stroke,df)

xyplot(Yoga_practice_meditation_asana_and_pranayama ~  Depression + Sleep_problems  ,df)

xyplot(Chronic_Obstructive_Pulmonary_Disease ~ Currently_smoking + Currently_consuming_tobacco ,df)

xyplot(Chronic_heart_diseases ~ Currently_smoking + Currently_consuming_tobacco ,df)

xyplot(Cardiovascular_diseases~High_Cholesterol,df)






















































