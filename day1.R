library(readr)
library(dplyr)
library(tidyr)


# data <- read_tsv("example_day1.txt", col_names = c("line"))
# data <- read_tsv("example2_day1.txt", col_names = c("line"))

data <- read_tsv("input_day1.txt", col_names = c("line"))

get_calibration_number <- function(string){

    tibble(character = strsplit(string, split = "")[[1]]) %>%
        filter(character %in% as.character(0:9)) %>%
        summarise(calibration = paste0(first(character), last(character))) %>%
        pull(calibration) %>%
        as.integer()

}

text_numbers_to_digits <- function(string){

    updated_string <- string
    updated_string <- gsub(updated_string, pattern = "zero", replacement = "zero0zero")
    updated_string <- gsub(updated_string, pattern = "one", replacement = "one1one")
    updated_string <- gsub(updated_string, pattern = "two", replacement = "two2two")
    updated_string <- gsub(updated_string, pattern = "three", replacement = "three3three")
    updated_string <- gsub(updated_string, pattern = "four", replacement = "four4four")
    updated_string <- gsub(updated_string, pattern = "five", replacement = "five5five")
    updated_string <- gsub(updated_string, pattern = "six", replacement = "six6six")
    updated_string <- gsub(updated_string, pattern = "seven", replacement = "seven7seven")
    updated_string <- gsub(updated_string, pattern = "eight", replacement = "eight8eight")
    updated_string <- gsub(updated_string, pattern = "nine", replacement = "nine9nine")

    return(updated_string)

}

get_calibration_total <- function(data, text_numbers = FALSE){

    extracted_data <- data %>%
        rowwise() %>%
        mutate(digitalised_line = if_else(text_numbers, text_numbers_to_digits(line), line),
               calibration_number = get_calibration_number(digitalised_line))

    return(sum(extracted_data$calibration_number))
}

# Part 1
get_calibration_total(data)

# Part 2
get_calibration_total(data, TRUE)
