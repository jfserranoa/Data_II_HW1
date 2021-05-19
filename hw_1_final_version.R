#########################
####### Libraries #######
#########################

library(tidyverse)
library(forcats)
library(ggpubr)
library(data.table)

#########################
####### Path data #######
#########################

path <- "~/01_UCHICAGO/05_Winter_2021/01_Data_and_Programming_II_(PPHA30536)/01_Assignments/01_Assignment_1/homework-1-jfserranoa/"

############################
####### Loading data #######
############################

header_rows <- 4

# Data frame with total full-time and part-time unemployment by state
total_df <- read_csv(paste0(path,"SAEMP25N total.csv"),
                     skip = header_rows)

# Data frame with full-time and part-time unemployment by industry and state
industry_df <- read_csv(paste0(path,"SAEMP25N by industry.csv"),
                        skip = header_rows,
                        na = c("(D)", "(T)"))


#########################################
####### Question 2: Data Cleaning #######
#########################################

### Cleaning of data frames

# 1. Quick data exploration

# Number of rows per industry in industry_df
industry_df %>% 
  group_by(LineCode, Description) %>% 
  summarise(n = n())

# Number of rows per state in total_df
total_df %>% 
  group_by(GeoFips, GeoName) %>% 
  summarise(n = n())

# 2. Removing Footers and Unnecessary rows

# In industry_df, filter by LineCode all the rows different to NAs. 
# This will eliminate all the footer rows and the intermediate
# rows that do not have data.
industry_df <- industry_df %>% 
                filter(!is.na(LineCode))

# In total_df, keep only the rows that do not have NAs.
# ref: https://statisticsglobe.com/complete-cases-in-r-example/
total_df <- total_df[complete.cases(total_df), ]

# 3. Merge the data frames

merged_df <- left_join(industry_df, 
                       total_df[, c("GeoFips","2000","2017")], 
                       by = "GeoFips")

merged_df <- merged_df %>% 
              rename(industry_2000 = "2000.x",
                     industry_2017 = "2017.x",
                     state_2000 = "2000.y",
                     state_2017 = "2017.y")

#ref: https://hollyemblem.medium.com/renaming-columns-with-dplyr-in-r-55b42222cbdc

# 4. Compute the share of the total employment for industry 
# at a specific state and year.

merged_df <- merged_df %>% 
              mutate(`2000` = industry_2000/state_2000,
                     `2017` = industry_2017/state_2017) %>% 
              select(-c(industry_2000, 
                        industry_2017, 
                        state_2000, 
                        state_2017))

# 5. Reshape the merged data frame
# ref pivot_longer: https://tidyr.tidyverse.org/reference/pivot_longer.html
# ref pivot_wider: https://tidyr.tidyverse.org/reference/pivot_wider.html
# ref transform: https://stackoverflow.com/questions/2288485/how-to-convert-a-data-frame-column-to-numeric-type

remove(reshaped_df)
reshaped_df <- merged_df %>% 
                pivot_longer(c(`2000`,`2017`), 
                             names_to = "year") %>% 
                pivot_wider(id_cols = c("GeoName", "year"),
                            names_from = Description, 
                            values_from = value)

reshaped_df <- reshaped_df %>% 
                rename(state = GeoName) %>% 
                transform(year = as.numeric(year))


# 6. Output data frame to a .csv document
# ref write.csv: https://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/R/R-Manual/R-Manual5.html

write.csv(reshaped_df, "data.csv", row.names = FALSE)

##########################################
####### Question 3: Visualizations #######
##########################################

# ref arrange: https://www.rdocumentation.org/packages/dplyr/versions/0.7.8/topics/arrange

top_manufacturing_2000 <- 
  reshaped_df %>% 
    filter(year == 2000) %>% 
    arrange(desc(Manufacturing)) %>% 
    head(5) %>% 
    select(state, Manufacturing)

### 3.a Plot: Employment Rate per State for years 2000 and 2017
# ref reorder: https://www.rpubs.com/dvdunne/reorder_ggplot_barchart_axis
# scale_fill_brewer: http://rstudio-pubs-static.s3.amazonaws.com/5312_98fc1aba2d5740dd849a5ab797cc2c8d.html

p_employment <- 
  reshaped_df %>%
      select(state, year, Manufacturing) %>% 
      filter(state %in% top_manufacturing_2000$state,
             year %in% c(2000, 2017)) %>% 
      ggplot(aes(x = reorder(state, -Manufacturing), 
                 y = Manufacturing, 
                 fill = as_factor(year))) +
        geom_bar(position="dodge", stat = "identity") +
        scale_fill_brewer(palette = "Blues", direction = -1) +
        labs(title = "Employment Rate in Manufacturing",
             subtitle = "Top 5 States in Year 2000",
             x = NULL,
             y = "Employment_rate",
             fill = "Year") +
        theme_light() +
        theme(legend.position = "top",
              legend.text = element_text(size = 7),
              legend.title = element_text(size = 7),
              axis.title.y = element_text(size = 7))


change_2000_2017 <- 
      reshaped_df %>%
        select(state, year, Manufacturing) %>% 
        filter(state %in% top_manufacturing_2000$state,
               year %in% c(2000, 2017)) %>% 
        pivot_wider(names_from = year, values_from = Manufacturing) %>% 
        mutate(change = `2017`/`2000`-1)

# ref theme: https://stackoverflow.com/questions/35090883/remove-all-of-x-axis-labels-in-ggplot

p_changes <- 
    change_2000_2017 %>%   
        ggplot(aes(x = reorder(state, -change), 
               y = change, group = 1)) +  
            geom_point(size = 3, color = "red4") +
            geom_segment(aes(x = reorder(state, -change), 
                              xend = reorder(state, -change), 
                              y = 0, 
                              yend = change),
                              color = "grey") +
            geom_text(aes(label = round(change, digits = 2)),
                      hjust = 0.5,  vjust = -1.7, size = 3) +
            theme_minimal() +
            ylab("Change") +
            theme(axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.title.y = element_text(size = 7),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  panel.grid = element_blank())


# ref ggarrange: http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/#:~:text=The%20function%20ggarrange()%20%5Bin%20ggpubr%5D%20provides%20a%20convenient%20solution,a%20list%20of%20arranged%20ggplots.
ggarrange(p_employment, p_changes,
          ncol = 1, nrow = 2, 
          align = "v",
          heights = c(2.5, 1.0))


## 3.b Show which five states have the highest concentration of 
# employment in a single industry in each of 2000 and 2017, 
# and what those industries are.


# function max_concentration is used to find the max concentrated industry by state
# data -> is the clean data frame
# year -> is the year one want to review
# concentration -> is the value at which one define concentration (any number between 0 and 1)

# 1. The function finds the name and value of the industry with the maximum share 
# of employment for each of the states.
# 2. Then creates a data frame and evaluates if the states on it can be considered as having 
# high concentrations of share employment according to the parameter "concentration"
# 3. Arrange the selected states in descending order and select the "top" numeber of states
# defined as argument (5 for this assignment).

max_concentration <- 
  function(data, year, concentration, top){
        
        data_year = data[data$year == year,]
        data_industry = data_year %>% select(-c(year, state)) 
        max_names = colnames(data_industry)[apply(data_industry, 1, which.max)]
        max_values = apply(data_industry, 1, max, na.rm = TRUE)
        output = data.frame(data_year$state,
                            max_names,
                            max_values,
                            data_year$year)
        
        output = output[max_values >= concentration,]
        output = output %>% arrange(desc(max_values))%>% head(top)
        colnames(output) = c("state",
                             "industry",
                             "concentration",
                             "year")
        
        rownames(output) = c()
        
        return(output)
}



# vector with years to apply function max concentration
nyears <- c(2000, 2017)

# The user is the one who defines what number should be considered here
# as a high concentration. In this case I took it as 0.15. 
concentration <- 0.15

# number of states in the top of concentrations
top <- 5


# Top 5 five states with the highest concentration of 
# employment in a single industry in each of 2000 and 2017.

concentrated_states_00 <- max_concentration(reshaped_df,
                                            2000, concentration,
                                            top)

concentrated_states_17 <- max_concentration(reshaped_df,
                                            2017, concentration, 
                                            top)

concentrated_states_00
concentrated_states_17


# Initialization of a data frame to save the result of the function
consolidated <- data.frame(state = character(0),
                           industry = character(0),
                           concentration = numeric(0),
                           year = numeric(0))

# consolidated file with the top 5 states per each of the years defined
for(i in 1:length(nyears)){
      
      df_function = max_concentration(reshaped_df,
                                      nyears[i],
                                      concentration,
                                      top)
      consolidated = rbind(consolidated, df_function)
}

# Graph with the top 5 states with higher concentration of employment 
# in a single industry for the years 2000 and 2017. The code of the graph is not 
# generalized as the function. This only applies to this particular case.

consolidated %>%
  mutate(state = str_replace_all(consolidated$state,
                                 "District of Columbia",
                                 "DC")) %>% 
  ggplot(aes(x = reorder(state,-concentration), y = concentration)) + 
  geom_bar(position="dodge",
           stat = "identity",
           fill = "steelblue") +
  labs(title = "Top Concentrated States",
       subtitle = "Government and Gov Enterprises",
       x = "State",
       y = "") +
  theme_light() +
  theme(axis.text = element_text(angle = 90))+
  facet_wrap(~year)
  
