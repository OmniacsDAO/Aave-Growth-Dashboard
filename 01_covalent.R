library(tidyverse)
library(httr)
library(lubridate)

# Aave lending pool address and my API key
aave_lending_pool <- "0x7d2768dE32b0b80b7a3454c06BdAc94A69DDc7A9"

## Make sure we don't have long keys set to scientific notation
options(scipen = 999)

fetch_topics <- function(lending_pool_address, max_topics = 13) {
    # Get the AAVE topics
    topic_fetch <- GET(paste0("https://api.covalenthq.com/v1/1/events/address/", lending_pool_address, "/abi/?&key=", api_key))
    topics <- content(topic_fetch, "parsed")
    
    # Get all the topics
    res <- lapply(topics$data$items[[1]]$abi_items, function(x) {
        return(list(name = x$abi$name, hash = x$topic_hash))
    })
    
    # Bind the topics to a dataframe
    topic_df <- res %>% bind_rows() %>% slice(1:max_topics)
    
    return(topic_df)
}

decode_topic <- function(topic_hash, topic_name, starting_block = 0, ending_block = 13000000, page_size = 1000000,
                         write_data = TRUE) {
    block_sequence <- seq(starting_block, ending_block, by = page_size)
    
    data_list = list()
    for (block in block_sequence) {
        print(block)
        end_block <- block + page_size - 1
        
        x <- GET(paste0("https://api.covalenthq.com/v1/1/events/topics/", topic_hash, "/?starting-block=", block, "&ending-block=", end_block, "&page-size=", page_size, "&key=", api_key))
        deposit <- content(x, "parsed")
        
        data_list <- c(data_list, deposit$data$items)
    }
    
    decoded <- list()
    blocks <- lapply(data_list, function(x) {
        
        decoded_params <- x$decoded$params
        decoded_params <- lapply(decoded_params, function(y) {
            y$tx_hash = x$tx_hash
            for (z in names(y)) {
                if (class(y[[z]][1]) == "logical") y[[z]] <- as.character(y[[z]])
            }

            return(y)
        })
        decoded <<- c(decoded, decoded_params)
        
        y <- x
        y$decoded <- NULL
        
        return(y)
    })
    
    decoded <- decoded %>% bind_rows()
    blocks <- blocks %>% bind_rows() %>% unnest(cols = c(raw_log_topics)) %>%
        group_by(tx_hash) %>%
        slice(1)
    
    final <- NULL
    if (nrow(decoded) > 0) {
        final <- decoded %>%
            select(name, value, tx_hash) %>%
            group_by(tx_hash, name) %>%
            slice(1) %>%  # TODO: Check
            spread(key = name, value = value) %>%
            left_join(blocks) %>%
            arrange(block_height)
        
        if (write_data) {
            write_csv(final, paste0("aave_", topic_name, ".csv"))
        }
    }
    
    return(final)
}

decode_topics <- function(topic_df) {
    # Process each topic one by one
    for (i in 1:nrow(topic_df)) {
        topic_hash <- topic_df$hash[i]
        topic_name <- topic_df$name[i]

        print(paste0("Processing ", topic_name, " (", i, "/", nrow(topic_df), ")"))
        
        decode_topic(topic_hash, topic_name)
    }
}

topic_df <- fetch_topics(aave_lending_pool)
decode_topics(tail(topic_df, n=10))
