# Employment




```{.r .numberLines}
library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)
```

I load the employment section data for each year and combine it into a single tibble/dataframe for easy analysis. 

*There are parsing errors for some columns. I find it better to just choose the required columns for the analysis with `cols_only` and manually specify types for them then manually fix encoding issues.*


```{.r .numberLines}
root_dir <- "C:/Users/R/Desktop/pslm/data_clean"
year_dirs <- head(list.dirs(root_dir, recursive = F), -1)
all_files <- map(year_dirs, function(year_dir) paste(year_dir, "4 - Employment.csv", sep="/")) %>% 
  keep(file.exists)

# employment <- map_dfr(all_files, read_csv)
```

Each individual in the survey is uniquely identified by a triplet of (year, hhcode, idc) where `year` is the year of survey, `hhcode` is household code/ID and `idc` is person ID *within the household*. 


```{.r .numberLines}
# head(employment)
```

Since the survey questionnaire changes across years and some questions are not asked there are some columns with significant variations in the available data. This is before considering that even if a question is asked the respondent might not provide/know the answer. 


```{.r .numberLines}
# employment %>% 
#   group_by(year) %>% 
#   summarise_all(~ mean(!is.na(.)))
```


