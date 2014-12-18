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

        **TODO**: reconstruct this procedure

2. ~~manually push useable Rds files to cluster~~
    list includes:
    - forexposition tracker
    - daily broker account performance
    - friend link data, symmetrized

3. produce user statistics
    - prepare_ld_fpt.R
        - use raw tables to generate cleaned: forexposition tracker, daily
            broker account performance, symmetrized friend link data
        - replaces the above step
    - prep-active-user-stats.R
        - creates user history level statistics, e.g. # accts, friends, total amt made/lost
    - predict-trading.R
        - generate success-by-currency (i.e. how many of your friends did how well in what currency on what day)
        - generate user day stats (i.e. looks like various to-date statistics by user, such as how much they made, # wins/losses)
    - rds2disk.R
        - merges sbc files produced by predict-trading.R

4. data processing for analysis
    - generate-adoptions.R
        - generates daily adoption behavior tables
        - produces ffdf/all-adopts
    - add-user-cp-fes.R
        - adds a user fixed effect grouping to ffdf/all-adopts

5. analysis
    - adopt-analyis.R
        - main analysis file
        - kind of a mess, should be careful to run it more or less line by line
        - saves an Rdump of model files
    - adopt-analysis-ucfe.R
        - as above, but incorporates user-currency fixed effects for
            conditioning

6. output
    - maketable-7-29-2014.R
        - makes conditional logit tables
        - has to load a like 30gb R dump of model files


# TODO: 

1. clean this file up

