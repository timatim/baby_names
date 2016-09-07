# Chunk 1
#install.packages("dplyr", "knitr", "plotly")
library(dplyr)
library(knitr)
library(plotly)
# Chunk 3
load("babynames.Rda")
# Chunk 4
# most popular name
by_name = babynames %>% group_by(name)
top_names = arrange(summarise(by_name, count = sum(count)), desc(count))
top_names_10 = head(top_names, 10)
# Chunk 5
top_names_100 = head(top_names, 100)
plot_ly(x = top_names_100$name,
y = top_names_100$count,
orientation = "v",
type = "bar") %>%
layout(xaxis = list(title = "Name"), yaxis = list(title = "Occurences"), title = "Histogram of Top 100 Names", margin = list(b = 100))
# Chunk 6
# gender ambiguous name
ambig_name = function(qyear) {
by_sex_name = babynames %>%
filter(year == qyear) %>%
group_by(sex, name) %>%
summarise(count = sum(count))
ambiguous = merge(by_sex_name[by_sex_name$sex == 'F',], by_sex_name[by_sex_name$sex == 'M',], by = "name")
ambiguous$sex.x = NULL
ambiguous$sex.y = NULL
colnames(ambiguous) = c("name", "F_count", "M_count")
# calculate percentages
ambiguous$total_count = ambiguous$F_count + ambiguous$M_count
ambiguous$M_percent = ambiguous$M_count/ambiguous$total_count
ambiguous$F_percent = ambiguous$F_count/ambiguous$total_count
ambiguous$percent_diff = abs(ambiguous$M_percent - ambiguous$F_percent)
# sort on percentage difference, neglect names with too little total count
ambiguous %>%
arrange(percent_diff) %>%
filter(total_count >= 200)
}
ambig_name_2013 = head(ambig_name(2013), 10)
ambig_name_1945 = head(ambig_name(1945), 10)
# Chunk 7
ambig_names_2013_50 = head(ambig_name(2013), 50)
plot_ly(x = ambig_names_2013_50$name,
y = ambig_names_2013_50$F_count,
name = "Female",
orientation = "v",
type = "bar") %>%
add_trace(x = ambig_names_2013_50$name,
y = ambig_names_2013_50$F_count,
name = "Male",
orientation = "v",
type = "bar") %>%
layout(xaxis = list(title = "Name"), yaxis = list(title = "Occurences"), title = "Barplot of Top 50 Ambiguous Names in 2013", margin = list(b = 100))
# Chunk 8
ambig_names_1945_50 = head(ambig_name(1945), 50)
plot_ly(x = ambig_names_1945_50$name,
y = ambig_names_1945_50$F_count,
name = "Female",
orientation = "v",
type = "bar") %>%
add_trace(x = ambig_names_1945_50$name,
y = ambig_names_1945_50$M_count,
name = "Male",
orientation = "v",
type = "bar") %>%
layout(xaxis = list(title = "Name"), yaxis = list(title = "Occurences"), title = "Barplot of Top 50 Ambiguous Names in 1945", margin = list(b = 100))
# Chunk 9
# popularity 1980
babynames_1980 = by_name %>%
filter(year == 1980) %>%
summarise(count = sum(count)) %>%
arrange(desc(count))
total_1980 = sum(babynames_1980$count)
babynames_1980$percentage = babynames_1980$count / total_1980
# popularity 2015
babynames_2015 = by_name %>%
filter(year == 2015) %>%
summarise(count = sum(count)) %>%
arrange(desc(count))
total_2015 = sum(babynames_2015$count)
babynames_2015$percentage = babynames_2015$count / total_2015
# combine 1980 2015
babynames_1980_2015 = merge(babynames_1980, babynames_2015, by = "name")
colnames(babynames_1980_2015) = c("name", "count_1980", "percent_1980", "count_2015", "percent_2015")
# generate new statistics base on the two
babynames_1980_2015$count_diff = babynames_1980_2015$count_2015 - babynames_1980_2015$count_1980
babynames_1980_2015$percent_diff = babynames_1980_2015$percent_2015 - babynames_1980_2015$percent_1980
babynames_1980_2015$percent_growth = babynames_1980_2015$count_2015/babynames_1980_2015$count_1980
popularity_inc = head(arrange(babynames_1980_2015, desc(percent_diff)), 10)
# Chunk 10
plot_ly(x = popularity_inc$name,
y = popularity_inc$count_1980,
name = "1980",
orientation = "v",
type = "bar") %>%
add_trace(x = popularity_inc$name,
y = popularity_inc$count_2015,
name = "2015",
orientation = "v",
type = "bar") %>%
layout(xaxis = list(title = "Name"), yaxis = list(title = "Occurences"), title = "Comparison Barplot of Top 10 Popularity Gain Names Since 1980", margin = list(b = 100))
# Chunk 11
# group by sex
name_diversity = babynames %>%
group_by(sex, name) %>%
summarise(count = sum(count)) %>%
summarise(count = n())
# Chunk 12
# plot pie chart
plot_ly(labels = c("Female", "Male"),
values = name_diversity$count,
orientation = 'v',
type = 'pie',
title = "Pie Chart of Unique Male and Female Names")
# Chunk 13
# first sum then count to avoid state duplicates
name_diversity_time = babynames %>%
group_by(sex, year, name) %>%
summarise(count = sum(count)) %>%
summarise(count = n())
# Chunk 14
# plot scatter plot
plot_ly(x = name_diversity_time$year,
y = filter(name_diversity_time, sex == 'F')$count,
name = "Female",
type = 'scatter') %>%
add_trace(x = name_diversity_time$year,
y = filter(name_diversity_time, sex == 'M')$count,
name = "Male",
type = 'scatter') %>%
add_trace(x = name_diversity_time$year,
y = filter(name_diversity_time, sex == 'F')$count + filter(name_diversity_time, sex == 'M')$count,
name = "Total",
type = 'scatter') %>%
layout(xaxis = list(title = "Year"), yaxis = list(title = "Number of Names"), title = "Name Diversity Through the Years")
# Chunk 15
# group by state and name, neglect year
name_diversity_states = babynames%>%
group_by(state, name) %>%
summarise(count = sum(count)) %>%
summarise(name_count = n())
# Chunk 16
# zoom to usa
map_config = list(
scope = 'usa',
projection = list(type = 'albers usa')
)
# plot map
plot_ly(name_diversity_states, z = name_count, type = 'choropleth', locations = state,locationmode = 'USA-states', marker = list(line = list(color = toRGB("white"))), color = name_count, colors = 'Blues') %>%
layout(title = "Name Diversity by States", geo = map_config)
# Chunk 17
# state population
state_pop = babynames %>%
group_by(state) %>%
summarise(count = sum(count))
# calculate ratio
name_diversity_states$population = state_pop$count
name_diversity_states$ratio = name_diversity_states$population / name_diversity_states$name_count
kable(arrange(name_diversity_states, ratio), align = 'l')
# Chunk 18
# plot map
plot_ly(name_diversity_states, z = ratio, type = 'choropleth', locations = state,locationmode = 'USA-states', marker = list(line = list(color = toRGB("white"))), color = ratio, colors = 'Blues') %>%
layout(title = "Name Diversity and Population Ratio by States", geo = map_config)
