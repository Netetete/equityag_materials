#title; "Lesson 3: Workflow and Statistical Tests in R"
#author: "Jeanette Huang"
#date: "2025-06-05"

#arrow reads parquets, tidyverse for data manipulation and visualization
library(arrow)
library(tidyverse)

#read in the data using the arrow package
#function readparquet has two arguments, the file path to the dataset and how to handle text strings (taxt as categorical data)
merged_data <- read_parquet(file = "merged_data.parquet", stringAsFactors = TRUE)

#exploring visualization
boxplot(formula = upward_mobility_rate_2010 ~ STATE_NAME_2010SVI, data = merged_data)

#identify and handle anomalies, like the state 06 shouldn't be there
#remove anomalies
merged_data <- merged_data[!(merged_data$STATE_NAME_2010SVI %in% "06"),]

#replot to see if the anomaly is gone
boxplot(formula = upward_mobility_rate_2010 ~ STATE_NAME_2010SVI, data = merged_data)

#now lets make a boxplot to explore total population by state
boxplot(formula = M_TOTPOP_2010SVI ~ STATE_NAME_2010SVI, data = merged_data)

#tips on boxplots
#formula syntax y ~ group
#the left of ~ is ploted on y axis
#right is grouping variable on the x axis

#3 intro to tidyverse and advanced data handling
#3.1 case sensitivity in R
boxplot(formula = M_TOTPOP_2010SVI ~ state_NAME_2010SVI, data = merged_data)
#keep your cases consistent

#3.2 skipped already have tidyverse

#3.3 tidyverse for summarizing
#traditional base R
#create two datasets filtering the rows on the state abbreviation
subset_data_ca <- subset(merged_data, STATE_ABBR_2010SVI == "CA")
subset_data_az <- subset(merged_data, STATE_ABBR_2010SVI == "AZ")

#calc the avg up mob rate in 2010 for each state using mean function
print(upward_mean_2010_ca <- mean(subset_data_ca$upward_mobility_rate_2010))

print(upward_mean_2010_az <- mean(subset_data_az$upward_mobility_rate_2010))

#3 both means attained, store them in a new data frame
upward_mobility_means_2010 <- data.frame(State = c("CA", "AZ"),
                                        up_2010_mean = c(upward_mean_2010_ca, upward_mean_2010_az))
#that was cool i guess but too long and repetitive, tidyverse to be more efficient
#subset, mean, data.frame used

#tidyverse, method
#use pipe operator %>% to chain mult commands
#state %>% group_by(STATE_ABBR_2010SVI) is the same as group_by(state, STATE_ABBR_2010SVI)
#ex starts with full dataset, groups by state, calcs the mean up mobil rate
upward <- merged_data %>%
  group_by(STATE_ABBR_2010SVI) %>%
  summarise(means_2010 = mean(upward_mobility_rate_2010, na.rm = TRUE))

#removes rows where state is NA
upward <- upward %>% filter(!is.na(STATE_ABBR_2010SVI))

#3.5 adding error bars with ggplot2
#visualize means and standard error
upward_stats <- merged_data %>%
  group_by(STATE_ABBR_2010SVI, COUNTY_2010SVI) %>%
  summarise(up_means = mean(upward_mobility_rate_2010, na.rm = TRUE),
            up_se = sd(upward_mobility_rate_2010, na.rm = TRUE)/sqrt(n()))

#grouping by both state and county
#calc mean up mobil, standard error (SE)(tells us the spread)(sd divided by square root of the sample size), more precise picture of variation across counties within each state

#remember to dro NA
upward_stats <- upward_stats %>% filter(!is.na(STATE_ABBR_2010SVI))

#basic ggplot with error bars
#data visualization tells a story
ggplot(data = upward_stats, mapping = aes(x = STATE_ABBR_2010SVI, y = up_means))

#blank because haven't told R what plot to make

#using means as point in this case, to geom_point. use a plus sign to add out component and out plot
ggplot(data = upward_stats, mapping = aes(x = STATE_ABBR_2010SVI, y = up_means)) + 
  geom_point()

#lets add error bars using geo_errorbar()
ggplot(data = upward_stats, mapping = aes(x = STATE_ABBR_2010SVI, y = up_means)) +
  geom_point() +
  geom_errorbar(mapping = aes(ymin = up_means - up_se,
                              ymax = up_means + up_se))

#tweak error bars for visibility (make them thinner)
ggplot(data = upward_stats, mapping = aes(x = STATE_ABBR_2010SVI, y = up_means)) + 
  geom_point() +
  geom_errorbar(mapping = aes(ymin = up_means - up_se, 
                              ymax = up_means + up_se),
                width = 0.30)
#so ggplot2 to create plots, geom _point adds dots for the mean up mobil rates, geom_errorbar shows variability around each mean using the SE, width 0.30 to make error bars look cleaner

#plotting many counties, grouped by state, it can get crowded
#simplify by summarizing at the state level and replotting
upward_stats_st <- merged_data %>%
  group_by(STATE_ABBR_2010SVI) %>%
  summarize(up_means = mean(upward_mobility_rate_2010),
            up_se = sd(upward_mobility_rate_2010)/sqrt(n()))

#drop the nas
upward_stats_st <- upward_stats_st %>% filter(!is.na(STATE_ABBR_2010SVI))

#redo the graph
ggplot(data = upward_stats_st, mapping = aes(x = STATE_ABBR_2010SVI, y = up_means)) +
  geom_point() + 
  geom_errorbar(mapping = aes(ymin = up_means - up_se,
                              ymax = up_means + up_se),
                width = 0.30)

#3.6 more tidyverse tricks
#create a working copy of data set
merged <- merged_data
#merged is your editable copy

#selecting columns, select remove columns with underscores and keeps those that start with upward
#had to take out some of the code in the original document
merged %>%
  select(!contains("_"), starts_with ("upward"))

#reordering columns
#relocate puts all columns containing STATE after the 2-2- upward mobil rate column
merged <- merged %>% 
  group_by(STATE_FIPS_2010SVI, COUNTY_2010SVI, CENSUSAREA_2010SVI) %>%
  mutate(uniqueid = row_number(), .before = contains("_"))

#summarize across mult columns
merged_stats_up <- merged %>% 
  dplyr::group_by(STATE_ABBR_2010SVI) %>% 
  dplyr::summarize(across(starts_with("upward"),
                          list(~mean(.x, na.rm = TRUE), 
                               ~sd(.x, na.rm = TRUE)))) 

#rename columns when summarizing, get clearer names for summary columns
merged_stats_up <- merged %>% 
  dplyr::group_by(STATE_ABBR_2010SVI) %>% 
  dplyr::summarize(across(starts_with("upward"), 
                          list(mean = ~mean(.x, na.rm = TRUE), 
                               sd = ~sd(.x, na.rm = TRUE)),
                          .names = "{gsub('_', '', col)}_{fn}")) 

#3.7 advanced modelling and nesting
#run models within groups
upward_models <- merged %>%
  filter(!is.na(STATE_ABBR_2010SVI),
         !is.na(upward_mobility_rate_2010),
         !is.na(POP2010)) %>%
  group_by(STATE_ABBR_2010SVI) %>% 
  summarise(model_list(lm(upward_mobility_rate_2010 ~ POP2010)))

#nest data now, turn grouped data into list columns
merged <- nest_by(state_group)

#4 running a basic statistical test
#made a bunch of changes to merged so reset
merged <- merged_data

#t-test
#are these two groups different enought that's unlikely this happened by chance
az <- merged_data[merged_data$STATE_NAME_2010SVI == "Arizona", ]
ca <- merged_data[merged_data$STATE_NAME_2010SVI == "California", ]

#data frame $ column name = single column epicness
#we can see how many rows and columns are in a data frame with the dim command
dim(merged_data)

#this returns all columns for the htird row in merged_data
merged_data[3,]

#or the third column
merged_data[,3]

print(nrow(az))
print(nrow(ca))

#why does az have fewer rows? ca has more counties
#t.test is upward mobil rate in two states significantly different

t.test(x = az$upward_mobility_rate_2010, y = ca$upward_mobility_rate_2010)
#since p = 0.1193 is greater than 0.001 and 95% confidence interval does not contain 0, we have evidence fo a statistically significant difference

#ANOVA multiple group comparisons
#comparing mboility amongst all counties in az
#anova analysis of variance
aov(formula = upward_mobility_rate_2010 ~ COUNTY_2010SVI, data = az)

mobility_rate_az_aov <- aov(formula = upward_mobility_rate_2010 ~ COUNTY_2010SVI, data = az)

#comparing mobility rate between counties, ~ mean explained by, mobility rate is explained by county

#now lets look at our model
summary(object = mobility_rate_az_aov)

#use sink to save this output to a file
sink(file = "output/az_mobility_anova.txt")
summary(object = mobility_rate_az_aov)
sink()

#linear regression
summary(merged_data[, 1:10], 6)
#cool but too much to look at

plot(x = merged_data$upward_mobility_rate_2010, y = merged_data$M_TOTPOP_2010SVI)

#now plot using log transformed data and see if relationship is more of a straight line
merged$logpop <- log10(merged$M_TOTPOP_2010SVI)

#plot again
plot(x = merged$upward_mobility_rate_2010, 
     y = merged$logpop, 
     xlab = "Upward Mobility", 
     ylab = "log10(Population)")

#run linear model
mobility_v_pop <- lm(upward_mobility_rate_2010 ~ logpop, data = merged)
summary(mobility_v_pop)

#save results tot a file instead of printing, use sink
sink(file = "output/mobility-pop-regression.txt")
summary(mobility_v_pop)
sink()

#multivarite regression
merged$az <- ifelse(merged$STATE_NAME_2010SVI == "Arizona", 1, 0)
merged$ca <- ifelse(merged$STATE_NAME_2010SVI == "California", 1, 0)

#add az as a predictor
mobility_v_pop_state <- lm(formula = upward_mobility_rate_2010 ~ logpop + az
                           , data = merged)
summary(mobility_v_pop_state)

sink(file = "output/mobility-pop-state-regression.txt")
summary(mobility_v_pop)
sink()

#student activities

boxplot(formula = upward_mobility_means_2010 ~ STATE_NAME_2010SVI, data = merged_data)

#anova
aov(formula = upward_mobility_rate_2010 ~ COUNTY_2010SVI, data = ca)
mobility_rate_ca_aov <- aov(formula = upward_mobility_rate_2010 ~ COUNTY_2010SVI, data = ca)
summary(object = mobility_rate_ca_aov)

#regression
plot(x = merged_data$upward_mobility_rate_2020, y = merged_data$M_TOTPOP_2010SVI)

#I liked this exercise a bit more than last time, now that i am more familiar with R. I still think we could use more
#context about functions, variables, data, etc. I still feel that it's hard to recognize what types of code works with
#what data, and I don't think I could string any code together on my own.