--- 
title: "PSLM Stories"
author: "Rabeez Riaz"
site: bookdown::bookdown_site
output:
  bookdown::gitbook:
    df_print: paged
---



# The PSLM Survey {-}

The Pakistan Social And Living Standards Measurement Survey (PSLM) is the most frequent and up-to-date demographic survey conducted in Pakistan with sufficient detail to allow interesting explorations through the data. The survey responses (microdate) are shared by the Pakistan Bureau of Statistics (PBS) in form of Stata files dta along with pdf questionnaires used for each year. The questionnaire changes over the years and sometimes even entire sections are removed/added.

I have created scripts which clean and standardize the survey microdata. My goal is to provide an accessible dataset from where social scientists and analysts can start their actual work without having to worry about the very clunky structure of the raw data.

The scripts go through the following steps for each section available in each year:

- Load dta file
- Rename columns from numeric codes into human-readable phrases
I have chosen the phrases to ensure same questions (regardless of question number in survey) have same name across years adn to avoid and to avoid name conflicts for similar sounding questions. But they are still based on my opinions so I will include a mapping from my names to original codes - so anyone familiar with the PSLM questionnaire can use their own naming if they wish.
Fix encoding issues (especially across years). For example for a single section in some years province will be kept as a text value for other - years it will be a number (thankfully the mapping is consistent and available in PBS reports)
- Convert columns into appropriate data types
- Add a column denoting the year for survey
- Save each section for each year in a separate csv file

Things the scripts do not do:
1. Perform text cleaning (inconsistent cases, "KP"/"KPK" representing same province etc)
2. Imputing/removing missing values
3. Combining cleaned files into big files for a single year containing all sections (or similar thing for same sections across all years)

My reason for not doing any of these is to not get in the way of analysts who will have different objectives/goals with their analyses. I want to preserve as much of the original data semantics as possible while removing unnecessary cleaning steps which they will have to repeat themselves for all files.

## This repository

The goal here is to have illustrative examples of loading and performing simple data cleaning or exploration which any analyst can follow and replicate. 

Another goal is to provide a few examples of more detailed and targeted analyses which are supposed to show how much easier it is to answer rich questions if the major hurdle of parsing/cleaning the dta files is already done. 
