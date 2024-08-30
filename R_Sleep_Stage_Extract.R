library(dplyr)
library(lubridate)
library(stringr)

parse_event_line <- function(line, base_date) {
  matches <- str_match(line, "Start: ([0-9:.]+); Duration \\[ms\\]: ([0-9]+); Event: ([^;]+);")
  
  if (!is.na(matches[1, 1])) {
    start_time_str <- matches[1, 2]
    duration_ms <- as.numeric(matches[1, 3])
    event <- matches[1, 4]
    
    # Create datetime object
    start_time <- ymd_hms(paste(base_date, start_time_str))
    
    # Check if the time is before the base date (indicating the following day)
    if (hour(start_time) < 23 || minute(start_time) < 5) {
      start_time <- start_time + days(1)
    }
    
    duration_s <- duration_ms / 1000
    
    return(data.frame(start_time = start_time, duration_s = duration_s, event = event, stringsAsFactors = FALSE))
  } else {
    return(NULL)
  }
}

file_path <- "C:/Users/sefarrell/OneDrive - Maynooth University/Documents/SF_Mon_Test.txt"
lines <- readLines(file_path)

#Base date, as days overlap from each other 
base_date <- "2024-08-19"  # From the [Rec. date:] field in your file

# Start of the event data
event_start_index <- grep("\\[Events Channel_08 Hypnogr.\\]", lines) + 1
event_lines <- lines[event_start_index:length(lines)]

# Parse the event lines
parsed_data <- do.call(rbind, lapply(event_lines, parse_event_line, base_date = base_date))
print("Parsed Data:")
print(parsed_data)

# Remove NULL rows produced
parsed_data <- parsed_data %>% filter(!is.na(start_time))

# Debug: Print parsed data after filtering
print("Parsed Data after Filtering:")
print(parsed_data)

# Function to expand events into 30-second epochs without overlap
expand_epochs <- function(data) {
  epochs <- data.frame()
  
  for (i in 1:nrow(data)) {
    start_time <- data$start_time[i]
    end_time <- start_time + seconds(data$duration_s[i])
    
    # Generate sequence of 30-second intervals
    epoch_times <- seq(from = start_time, to = end_time, by = "30 sec")
    
    # Ensure the end time is included if it doesn't fall exactly on a 30-second boundary
    if (end_time != tail(epoch_times, 1)) {
      epoch_times <- c(epoch_times, end_time)
    }
    
    # Create data frame for the epochs
    epoch_data <- data.frame(time = epoch_times, sleep = paste("Sleep stage", data$event[i]), stringsAsFactors = FALSE)
    
    # Append to the epochs data frame
    epochs <- rbind(epochs, epoch_data)
  }
  
  # Remove duplicates and ensure correct ordering
  epochs <- epochs %>%
    distinct() %>%
    arrange(time)
  
  # Handle overlapping events: keep the last event if times overlap
  epochs <- epochs %>%
    group_by(time) %>%
    filter(row_number() == n()) %>%
    ungroup()
  
  return(epochs)
}

# Expand the data into 30-second epochs
expanded_data <- expand_epochs(parsed_data)

# Debug: Print expanded data
print("Expanded Data:")
print(head(expanded_data, 20))

# Write expanded data to CSV
write.csv(expanded_data, "expanded_sleepdata.csv", row.names = FALSE)

# Test print
print("First 20 rows of the expanded data:")
print(head(expanded_data, 20))

