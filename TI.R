##################################################################################################################
# Program Name:        TI
# Current Version:     1.0
# Date:                2023-12-13
# Updated Date:
# Working Environment: R 4.3.0
# Author:              Sungjin Kim (sungjin.kim@cshs.org)
# Purpose:             To calculate toxicity index (TI) and export TI data.
# Reference:           Rogatko A, Babb JS, Wang H, Slifker MJ, Hudes GR. 
#                      Patient characteristics compete with dose as predictors of 
#                      acute treatment toxicity in early phase clinical trials.  
#                      Clin Cancer Res. 2004;10(14):4645-51. doi: 10.1158/1078-0432.CCR-03-0535. PMID: 15269136.
# Parameters: 
# data:                The name of the data set to be analyzed (long-format, individual toxicity grades in separate rows). 
# id:                  Subject unique id.
# grade:               Toxicity grade.
# fname:               File name for output data.
#
# Usage: 
# dt <- read.table(file = "sample data.csv", header=T, sep=",") 
# ti(data=dt, id=UniqueID, grade=AE_Grade, fname="Output_TI_r")
# where dataset dt has subject id of UniqueID and toxicity grade of AE_Grade as follows:
# UniqueID  AE_Grade   
# 1         	
# 2	        1
# 3	        1
# 3       	1
# 4         2
# 5         2
# 5	        1
# .......
##################################################################################################################

# Calculate TI and output to csv file
ti <- function(data, id, grade, fname) {
  
  library(dplyr)
  library(here)
  
  `%notin%` <- Negate(`%in%`)
  
  suppressWarnings({
    
    id <- enquo(id)            
    grade <- enquo(grade)  
  
    # Function to calculate toxicity index (TI)
    tic <- function(grade) {
    
      grade <- sort(grade, decreasing = TRUE)
      TI <- NULL
    
      for (i in 1:length(grade)) {
            TI[i] <- grade[i] * prod(( grade[1:i-1] + 1 )^-1)
      }
      return(sum(TI))
    }
  
    df <- data %>% 
           group_by(!!id) %>%
           mutate(TI = tic(!!grade)) %>%
           ungroup(!!id) %>% distinct(!!id, TI) 
  
    # Subject with missing grade 
    entiremissing_id <- dt %>% 
           group_by(!!id) %>%
           filter(all(is.na(!!grade))) %>% 
           distinct(!!id) %>% pull()
  
    # Subject with both missing and non-missing grades
    missing_id <- dt %>%
           group_by(!!id) %>%
           filter(is.na(!!grade)) %>% 
           filter(!!id %notin% entiremissing_id) %>% 
           distinct(!!id) %>% pull()

    file_name <- here::here(paste0(fname, ".csv"))
    
    write.table(df, file = file_name, append = TRUE, sep=',', row.names = FALSE)
    
    cat("", "\n", file = file_name, append = TRUE)
    cat("Note:", "\n", file = file_name, append = TRUE)
    cat("Subject with single or more than one record of grade which is/are all missing: ", entiremissing_id, "\n", file = file_name, append = TRUE)
    cat("Subject with both missing and non-missing records of grade (TI was calculated based on available non-missing grade values.): ", missing_id, "\n", file = file_name, append = TRUE)
    
  })  
}

