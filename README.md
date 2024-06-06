This project is a part of undergraduate summer research internship at Ivey Business School.

This repository contains the data and analysis code for the research project "Income Inequality and Search Interest in Animal Welfare Products." The project investigates the relationship between income inequality and the search interest in animal welfare-related keywords across the United States from 2006 to 2022.

Data was collected spanning from 2006 to 2022 using the R programming language and various APIs. The data for the year 2020 were excluded due to the U.S. Census Bureau's temporary halt of the ACS that year. The dataset includes 816 state-year observations (51 states, including D.C., and 16 years).

### Data Sources

- **Google Trends**: Search interest data for animal welfare keywords. Retrieved from [Google Trends](https://trends.google.com/trends/)
- **Social Explorer**: Gini index data. Retrieved from [Social Explorer](https://www.socialexplorer.com/)
- **American Community Survey (ACS)**: Demographic and socioeconomic data for covariates. Retrieved from [U.S. Census Bureau](https://www.census.gov/programs-surveys/acs/)

## Project Structure

- `data/`: Contains the processed data files.
- `scripts/`: Contains R scripts for data cleaning, analysis, and visualization.
- `graph/`: Contains the output of the analysis, including tables and figures.
- `README.md`: This file.
