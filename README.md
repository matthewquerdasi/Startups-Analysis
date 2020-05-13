# Startups-Analysis
Analysis of Crunchbase startups dataset using R and Shiny package. 

Analysis of a Crunchbase dataset containing data on over 50,000 startup companies from countries all over the world. Dataset includes startups from as early as 1902 up until 2014, and includes startups from over 750 unique market industries. 

Created a Shiny app that allows the user to (depending on the plot) select a market industry of interest, time frame, country, or funding variable to further explore. Analysis utilizes spatial data, time series, and pure numeric and categorical information to clearly display patterns and trends within each category and sub-category of interest.

Project was coded using R and the Shiny package.

Files included in repository: 
- Startups_project_analysis.R: file containing all data manipulation and Shiny code 
- investments_VC.csv: dataset file used for analysis containing all relevant data
- us_states.json: json file used in Leaflet chloropleth plot 

Note: Dataset originally imported from Kaggle 
Dataset link: https://www.kaggle.com/arindam235/startup-investments-crunchbase
