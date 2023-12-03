library(readr)
library(dplyr)
library(tidyr)


# data <- read_tsv("example_day2.txt", col_names = c("line"))

data <- read_tsv("input_day2.txt", col_names = c("line"))


parse_game_data <- function(data){

    tidy_data <- data %>%
        extract(line, regex = "Game ([0-9]+): (.+)", into = c("ID", "set"), convert = TRUE) %>%
        separate_rows(set, sep = "; ") %>%
        group_by(ID) %>%
        mutate(set_number = 1:n()) %>%
        ungroup() %>%
        separate_rows(set, sep = ", ") %>%
        rename(content = set) %>%
        separate(content, into = c("number", "colour"), sep = " ", convert = TRUE)

    return(tidy_data)
}

# PART 1
bag_contents <- tibble(
    actual_number = c(12L, 13L, 14L),
    colour = c("red", "green", "blue")
)

possible_games <- function(tidy_data, bag_contents){

    games <- tidy_data %>%
        group_by(ID, colour) %>%
        summarise(max_number = max(number), .groups = "drop") %>%
        left_join(bag_contents, by = "colour") %>%
        filter(max_number <= actual_number) %>%
        group_by(ID) %>%
        filter(n() == 3L) %>%
        select(ID) %>%
        distinct() %>%
        pull(ID)

    return(games)
}

possible_games(parse_game_data(data),
               bag_contents) %>%
    sum()


# PART 2
min_powers <- function(tidy_data){
    tidy_data %>%
        group_by(ID, colour) %>%
        summarise(min_number = max(number), .groups = "drop") %>%
        group_by(ID) %>%
        summarise(power = prod(min_number)) %>%
        pull(power)
}

min_powers(parse_game_data(data)) %>%
    sum()
