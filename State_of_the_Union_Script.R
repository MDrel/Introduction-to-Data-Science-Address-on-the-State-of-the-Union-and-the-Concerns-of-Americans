library(tidyverse)
library(rvest)
library(tidytext)
library(dplyr)

get_the_address <- function(current_url){
    #read the page from the link given as an argument 
    va0 <- read_html(current_url)
    va1 <- va0 |> html_elements("p")
    
    #there is a specific type of element containing the name of the president
    president <- va0 |> html_element(".diet-title")|> 
        html_text2() 
    
    #there is a specific type of element that contains the date of the speech
    year <- va0 |> html_element(".date-display-single")|> 
        html_text2() |>
        #it is a string, so to extract the year, I can take the last four characters
        str_sub(start = -4, end = -1) |>
        #transform the year into a number for simplicity
        as.numeric()
    
    #find other people who spoke during the State of the Union Address
    to_be_removed <- va0 |> 
        html_elements("i") |> 
        html_text2() |> 
        as_tibble() |>
        distinct() |> 
        #the person speaking is indicated in italics, their name and surname followed by a period
        filter(str_detect(value, "\\.$")) |>
        #make sure not to remove the president's lines
        filter(value != "The President.") 
    
    #transform the data into a tibble containing lines of the speech
    va2 <- va1 |> html_text2()
    va3 <- va2 |> as_tibble() 
    
    #remove lines to be removed: 
    #names of the people speaking, their lines 
    for (each in to_be_removed |> pluck("value")){
        va3 <- va3 |> filter(!str_detect(value, each)) 
    }
    
    #remove indication that it is the president speaking,
    #remove descriptions of noise and disturbances
    va4 <- va3 |> 
        mutate(
            value = str_remove(value, "The President."),
            value = str_remove(value, "\\[[\\w\\s\\:\\.\\,\\;\\\"]{1,}\\]")
        )
    
    #removes website information
    va5 <- va4[-1, ]
    va6 <- va5 |>
        filter(row_number() <= n()-7)
    
    #give every word a separate row
    va7 <- va6 |> unnest_tokens(word, value)
    
    #remove stop words
    va8 <- va7 |> anti_join(stop_words)
    
    #remove numbers, calculate how many times each word appears
    va9 <- va8 |>
        group_by(word) |>
        filter(!str_detect(word, "\\d")) |>
        summarize(
            quantity = n()
        ) |> 
        arrange(desc(quantity)) 
    
    #add the name of the president and the year of the speech
    va10 <- va9 |> 
        mutate(
            President = president,
            year = year
        )
    
    return(va10)
}

#this page contains a table of links to all the State of the Union Addresses
menu_url <- "https://www.presidency.ucsb.edu/documents/presidential-documents-archive-guidebook/annual-messages-congress-the-state-the-union#Table%20of%20SOTU"

#read the page
first_page <- read_html(menu_url)

#extract links
all_links <- first_page |> 
    html_elements("a") |>
    html_attr("href") |>
    as_tibble()

#keep only the links to State of the Union Addresses
my_links <- all_links |>
    filter(str_detect(value, "/documents/address")) |>
    slice_head(n = 30)

#create an empty data frame
all_data <- data.frame()

#extract each speech one by one
for (each in my_links |> pluck("value")){
    current_url <- each
    #print(current_url)
    all_data <- bind_rows(all_data, get_the_address(current_url))
    #waits before reaching for another page
    Sys.sleep(2)
}

#only keep the most recent data
all_data <- all_data |> filter(year > 1995)   

#make names of presidents consistent
#only containing their first name, initial and last name
all_data <- all_data |>
    mutate(
        President = str_remove(President, ", Jr."), 
        President = str_remove(President, "\\(1st Term\\)")
    ) 

#find the most popular words 
popularity <- all_data |>
    group_by(word) |>
    summarize(
        n = n(),
        total = sum(quantity)
    ) |>
    arrange(n) |>
    #only leave the words that appear in every speech 
    filter(n == 28) |>
    print()

#remove words that appear in all of the speeches
#while some of them may be interesting, they mostly skew the results
#I am going look at some of those words later
smaller_data <- all_data |> anti_join(popularity)
smaller_data <- smaller_data |> filter(!str_detect(word, "america"))

#pick 5 most popular words for each year
smaller_data <- smaller_data |>
    group_by(year) |>
    slice_max(n = 5, quantity, with_ties = FALSE)

#plot most frequent words in the State of the Union Address each year
ggplot(data = smaller_data) +
    geom_col(
        mapping = aes(
            x = reorder_within(word, quantity, year),
            y = quantity,
            fill = President
        )
    ) +
    scale_x_reordered() +
    labs(
        title = "Most frequent words in the State of the Union Address",
        x = NULL, 
        y = NULL
    ) +
    facet_wrap(
        ~year,
        scales = "free_y", 
        ncol = 4
    ) +
    coord_flip() +
    theme_grey(base_size = 8) +
    theme(plot.title = element_text(size = 14))+ 
    scale_fill_manual(values=c("blue1", "chocolate3", "brown2", "cornflowerblue", "deepskyblue3"))


#I have picked the most interesting words
#including some interesting words that appear in every speech
#I am going to show how their frequency of usage has changes over the years

show_plot <- function(regex, title){
    new_data <- all_data |>
        mutate(
            #mark the derivatives of the word
            interesting_word = str_detect(word, regex)
        ) |>
        group_by(year, interesting_word, President) |>
        summarize(
            yearly = sum(quantity)
        ) |>
        filter(interesting_word == TRUE)
    
    ggplot(data = new_data) +
        geom_col(mapping = aes(x = year, y = yearly, fill = President)) +
        labs(
            title = str_c("Frequency of usage of the word ", title, " and its derivatives"),
            x = "Year", 
            y = "Quantity"
        ) +
        theme_grey(base_size = 9) +
        theme(plot.title = element_text(size = 14)) + 
        scale_fill_manual(values=c("blue1", "chocolate3", "brown2", "cornflowerblue", "deepskyblue3"))
}
    
show_plot("america[\\w]{0,}", "America")
show_plot("(child[\\w]{0,})|(famil(ies|y))", "child and its derivatives and family")
show_plot("health[\\w]{0,}", "health")
show_plot("centur(ies|y)", "century")
show_plot("terror[\\w]{0,}", "terror")
show_plot("immigra[\\w]{1,}", "immigration")

show_plot("econom[\\w]{1,}", "economy")
show_plot("business[\\w]{0,}", "business")

show_plot("future[\\w]{0,}", "future")
