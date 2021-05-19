# Data and Programming II - Homework 1
## Data Cleaning Exercise: U.S. Bureau of Economic Analysis (BEA)
### Part I:
* File labeled **"Total"**: Has total annual employment per state for the years 2000 and 2017.
* File labeled **"by industry"**: Has employment per industry in each of 10 industries per state for the years 2000 and 2017.
* Load and merge the data into a panel dataframe, with the columns: **"state"**, **"year"**, and one for each of the 10 industries. Every state-year combination should uniquely identify a row. No more and no less than 12 columns should remain. Do any necessary cleaning for the data to be easily usable.
* The values should be given as the share of the total employment in that place and time, e.g. if total employment in a place and time was 100, and the employment in one industry was 10, then the value shown for that state-year industry should be 0.1.  The "total" values should not be a part of the final dataframe.

### Part II:
* Find the states with the top five share of manufacturing employment in the year 2000, then show how their share of employment in manufacturing changed between 2000 and 2017.  Use a basic plot to display the information.
* Show which five states have the highest concentration of employment in a single industry in each of 2000 and 2017, and what those industries are.
