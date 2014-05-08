Let's make some semblance of order to this project:

# Steps

1. raw data tables -> useable data tables
    1. import SQL
        by hand
    2. export SQL to csv
        by hand
    3. csv tables -> R data.table, stored as Rds
        HOME/Dropbox/Currensee/Data Exploration/utilcode/csvs2rds.R
    4. reprocess base Rds tables to be used in later steps
        unclear
        I think I did this by hand, but if I didn't, then I have no idea where the R file responsible for this conversion is
        TODO: reconstruct this procedure

2. manually push useable Rds files to cluster
    list includes:
        - forexposition tracker
        - daily broker account performance
        - friend link data, symmetrized

3. produce user statistics
    
