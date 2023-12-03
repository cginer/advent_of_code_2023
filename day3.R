library(readr)
library(dplyr)
library(tidyr)


# data <- read_tsv("example_day3.txt", col_names = c("line"))

data <- read_tsv("input_day3.txt", col_names = c("line"))

get_symbol_locations <- function(data){
    len_lines <- nchar(data$line[1])

    symbol_data <- data %>%
        mutate(line_id = 1:n()) %>%
        separate(sep = "", col = line, into = as.character(0:len_lines), convert = TRUE) %>%
        select(-`0`) %>%
        pivot_longer(-line_id, names_to = "col_id", values_to = "value") %>%
        mutate(col_id = as.integer(col_id),
               symbol = if_else(value %in% c(0:9, "."), FALSE, TRUE)) %>%
        filter(symbol) %>%
        select(-symbol)

    return(symbol_data)
}

get_match_info <- function(string){
    match_info <- gregexpr(string, pattern = "[0-9]+")[[1]]

    tibble(start_col = match_info,
           end_col = as.integer(start_col + attributes(match_info)$match.length - 1)) %>%
        mutate(number = as.integer(substring(string, start_col, end_col)))

}

get_number_information <- function(data){
    len_lines <- nchar(data$line[1])

    data %>%
        mutate(line_number = 1:n()) %>%
        group_by(line_number) %>%
        summarise(get_match_info(line), .groups = "drop") %>%
        filter(!is.na(number))

}


# Part 1
find_symbol_adjacent_numbers <- function(data){
    symbol_data <- get_symbol_locations(data)
    number_data <- get_number_information(data)

    number_data %>%
        rowwise() %>%
        mutate(n_adjacent_symbols = symbol_data %>% filter(line_id >= line_number - 1, line_id <= line_number + 1,
                                                           col_id >= start_col - 1, col_id <= end_col + 1) %>% nrow()) %>%
        filter(n_adjacent_symbols > 0) %>%
        pull(number)
}


sum(find_symbol_adjacent_numbers(data))

# Part 2
get_gear_ratios <- function(data){
    symbol_data <- get_symbol_locations(data)
    number_data <- get_number_information(data)

    symbol_data %>%
        filter(value %in% "*") %>%
        rowwise() %>%
        mutate(adjacent_numbers = list(number_data %>%
                                filter(line_number >= line_id - 1, line_number <= line_id + 1,
                                       end_col >= col_id - 1, start_col <= col_id + 1)),
               n_adjacent_numbers = nrow(adjacent_numbers)) %>%
        filter(n_adjacent_numbers %in% 2) %>%
        mutate(gear_ratio = prod(adjacent_numbers$number)) %>%
        pull(gear_ratio)

}

sum(get_gear_ratios(data))
