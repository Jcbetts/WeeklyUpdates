library(charlatan)
library(dplyr)
library(readr)
library(stringr)
library(tibble)

PrepData <- read.csv("Data/October/Week 2/10-13-25.csv")

# Change column names
PrepData <- rename(PrepData, ID = Household..,
               Name = Member,
               Email = X,
               Pass_Type = Pass.Code,
               Facility = Visit.Loc)

# change date format for POSIX
PrepData$Time <-format(strptime(PrepData$Time, "%I:%M %p"), format="%H:%M:%S")
PrepData$Date <- as.Date(PrepData$Date, format = "%m/%d/%Y")

# create date + time column to compare
PrepData <- PrepData |> 
  mutate(DateTime = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S"))

# for facility visits, two-minute cool-down not active... manually identifying and removing
PrepData <- PrepData |>
  arrange(ID, Name, DateTime) |> 
  group_by(ID, Name) |> 
  mutate(
    TimeSinceSwipe = as.numeric(difftime(DateTime, lag(DateTime), units = "mins"))) |>
  ungroup()

# filter for swipe minutes > 2 or NA (which is a member's first swipe)
PrepData <- PrepData |> 
  filter(TimeSinceSwipe > 2.00 | is.na(TimeSinceSwipe))

# facility assignment

PrepData$Facility <- str_replace(PrepData$Facility, "^(Turnstile|SM|SRC)$", "1")

PrepData$Facility <- str_replace(PrepData$Facility, "^(NREC-QCK|Turnstile-NREC|NREC)$", "2")

PrepData$Facility <- str_replace(PrepData$Facility, "^Turnstile-zBD$", "3")

PrepData$Facility <- str_replace(PrepData$Facility, "^Studio 91$", "4")

PrepData$Facility[PrepData$Facility == ""] <- "5"

# helpful to have a cleaned and unscrambled file to compare to cleaned and scrambled file to ensure nothing is lost...
write.csv(PrepData, "Data/CompareData.csv")

# Complete working function with detailed diagnostics
scramble_large_dataset_working <- function(data, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  cat("=== Starting Data Scrambling ===\n")
  cat("Original data:", nrow(data), "rows x", ncol(data), "columns\n")
  
  # Convert to character to avoid type issues
  PrepData$ID <- as.character(PrepData$ID)
  PrepData$Name <- as.character(PrepData$Name)
  PrepData$Email <- as.character(PrepData$Email)
  
  cat("Data types after conversion - ID:", class(PrepData$ID), "Name:", class(PrepData$Name), "Email:", class(PrepData$Email), "\n")
  
  # Remove NA values
  complete_cases <- complete.cases(PrepData[, c("ID", "Name", "Email")])
  df_clean <- PrepData[complete_cases, ]
  
  if (nrow(df_clean) != nrow(PrepData)) {
    cat("Removed", nrow(PrepData) - nrow(df_clean), "rows with missing values\n")
  }
  
  # Get unique combinations
  unique_people <- unique(df_clean[, c("ID", "Name", "Email")])
  n_unique <- nrow(unique_people)
  
  cat("Processing", n_unique, "unique individuals from", nrow(df_clean), "observations\n")

  # Add scrambled columns to unique_people dataframe
  unique_people$scrambled_id <- paste0("SUBJ_", sprintf("%06d", 1:n_unique))
  unique_people$scrambled_name <- paste("User", sample(1000:(1000+n_unique*2), n_unique))
  unique_people$scrambled_email <- paste0("user", sample(10000:99999, n_unique), "@example.com")
  
  # Use merge to join scrambled data back 
  cat("Merging scrambled data back to original dataset...\n")
  df_merged <- merge(df_clean, unique_people, by = c("ID", "Name", "Email"), all.x = TRUE)
  
  cat("After merge:", nrow(df_merged), "rows\n")
  
  # Check for any failed matches
  failed_matches <- sum(is.na(df_merged$scrambled_id))
  if (failed_matches > 0) {
    cat("WARNING:", failed_matches, "rows failed to match!\n")
  } else {
    cat("SUCCESS: All rows matched successfully!\n")
  }
  
  # Remove original sensitive columns
  df_final <- df_merged[, !names(df_merged) %in% c("ID", "Name", "Email")]
  
  # Create clean mapping table
  mapping_table <- unique_people[, c("ID", "Name", "Email", "scrambled_id", "scrambled_name", "scrambled_email")]
  names(mapping_table)[1:3] <- c("original_id", "original_name", "original_email")
  
  cat("=== Scrambling Complete ===\n")
  cat("Final dataset:", nrow(df_final), "rows\n")
  cat("Unique scrambled IDs:", length(unique(df_final$scrambled_id)), "\n")
  cat("Mapping table:", nrow(mapping_table), "mappings\n")
  
  return(list(
    data = df_final,
    mapping = mapping_table
  ))
}

# Run the working version
result <- scramble_large_dataset_working(data, seed = 42)
df_scrambled <- result$data
mapping_table <- result$mapping

write.csv(mapping_table, "Data/MappingTable.csv")

# Rename columns back to original values

df_scrambled <- rename(df_scrambled, ID = scrambled_id,
               Name = scrambled_name,
               Email = scrambled_email)

write_rds(df_scrambled, "Data/October/Week 2/New.rds")