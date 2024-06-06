# Income Inequality and Search Interest in Animal Welfare Products

This project is a part of an undergraduate summer research internship (USRI) program at Ivey Business School. The repository contains the data and analysis code for the research project "Income Inequality and Search Interest in Animal Welfare Products." The project investigates the relationship between income inequality and the search interest in animal welfare-related keywords across the United States.

## Data Collection

Data was collected spanning from 2006 to 2022 using the R programming language and various APIs. The data for the year 2020 were excluded due to the U.S. Census Bureau's temporary halt of the ACS that year. Our dataset includes 51 states, encompassing Washington D.C., and 16 years for a total of 816 observations.

### Data Sources

- **Google Trends**: Search interest data for animal welfare keywords. Retrieved from [Google Trends](https://trends.google.com/trends/)
- **Social Explorer**: Gini index data. Retrieved from [Social Explorer](https://www.socialexplorer.com/)
- **American Community Survey (ACS)**: Demographic and socioeconomic data for covariates. Retrieved from [U.S. Census Bureau](https://www.census.gov/programs-surveys/acs/)

## Project Structure

- `data/`: Contains the processed data files.
- `graph/`: Contains the output of the analysis, including tables and figures.
- `README.md`: This file.
