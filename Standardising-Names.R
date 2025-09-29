# This is all functions that may or may not have been used to standardise trainer names
# Important: Trainer names should all be standardised including symbols "&/" and spaces so that it can be compared for trainer change
# This is because R will look up and down the rows and match text strings.
# !!!! Run this in small amount of names first to check if result is consistent
# I suggest you create a sub data set with just trainer names to test cleaning the names before you work on your main dataset
# Good luck!! 
standardize_first_name <- function(first) { # this may be the first one that did not work.
  # Define your mapping here. For example:
  lookup <- list(
    "Ken"   = "Kenny",
    "Kenny" = "Kenny",
    "Alf"   = "Alfred",
    "Alfred" = "Alfred"
    # Add additional mappings as needed.
  )
  # If the first name (exactly as provided) is in the lookup, return the canonical form.
  if(first %in% names(lookup)) {
    return(lookup[[first]])
  }
  return(first)
}

### Main Function to Standardize Names # This might be the function used
standardize_names <- function(names_column) {
  # Convert input to character.
  names_column <- as.character(names_column)
  
  # Step 1: Replace any "&" (with optional spaces) with " and "
  names_column <- str_replace_all(names_column, "\\s*&\\s*", " and ")
  
  # Step 2: Fix pattern "First and Second Last" → "First Last and Second Last"
  fix_two_first <- function(name) {
    if(!is.na(name) && str_detect(name, "^(\\w+)\\s+and\\s+(\\w+)\\s+(\\w+)$")) {
      m <- str_match(name, "^(\\w+)\\s+and\\s+(\\w+)\\s+(\\w+)$")
      return(paste0(m[2], " ", m[4], " and ", m[3], " ", m[4]))
    }
    return(name)
  }
  names_column <- sapply(names_column, fix_two_first, USE.NAMES = FALSE)
  
  # Step 3: Build candidate pool of full names.
  # We assume a candidate full name has a first name of at least 2 letters.
  candidates <- unique(names_column[str_detect(names_column, "^[A-Za-z]{2,}\\s+[A-Za-z]+$")])
  candidates <- candidates[!is.na(candidates)]
  
  # Step 4: Helper to combine separated initials.
  # E.g., "A J Brown" becomes "AJ Brown".
  combine_initials <- function(segment) {
    if(is.na(segment)) return(NA)
    tokens <- str_split(segment, "\\s+")[[1]]
    if(length(tokens) >= 2 && all(nchar(tokens[1:(length(tokens)-1)]) == 1)) {
      combined <- paste0(tokens[1:(length(tokens)-1)], collapse = "")
      tokens <- c(combined, tokens[length(tokens)])
    }
    paste(tokens, collapse = " ")
  }
  
  # Step 5: Process a single segment (representing one person's name).
  process_segment <- function(seg, candidates) {
    if(is.na(seg) || str_trim(seg) == "") return(NA)
    seg <- str_trim(seg)
    # Save the original tokens (before combining).
    orig_tokens <- str_split(seg, "\\s+")[[1]]
    # Combine separated initials.
    seg_combined <- combine_initials(seg)  # e.g., "J J Rayner" -> "JJ Rayner"
    tokens <- str_split(seg_combined, "\\s+")[[1]]
    # If the segment has exactly two tokens (first and last name):
    if(length(tokens) == 2) {
      # Determine if the original first-name tokens were all single letters.
      if(length(orig_tokens) >= 2 && all(nchar(orig_tokens[1:(length(orig_tokens)-1)]) == 1)) {
        # This is an abbreviation formed entirely by initials.
        # Use only the first letter of the first token for candidate lookup.
        lookup_initial <- orig_tokens[1]
        last_name <- orig_tokens[length(orig_tokens)]
        pattern <- paste0("^(?i)", lookup_initial, "[a-z]+\\s+", last_name, "$")
        cand <- candidates[str_detect(candidates, pattern)]
        if(length(cand) == 1) {
          return(cand)
        } else {
          return(seg_combined)
        }
      } else {
        # If the segment is not purely initials, return as is.
        return(seg_combined)
      }
    } else {
      return(seg_combined)
    }
  }
  
  # Step 6: Process each full name (which may contain multiple segments separated by " and ").
  process_name <- function(name) {
    if(is.na(name) || str_trim(name) == "") return(NA)
    segments <- str_split(name, "\\s+and\\s+")[[1]]
    new_segments <- sapply(segments, function(seg) {
      process_segment(seg, candidates)
    }, USE.NAMES = FALSE)
    new_name <- paste(new_segments, collapse = " and ")
    
    # Step 7: Now standardize the first name in each segment using our lookup.
    new_segments <- str_split(new_name, "\\s+and\\s+")[[1]]
    new_segments <- sapply(new_segments, function(seg) {
      parts <- str_split(seg, "\\s+")[[1]]
      if(length(parts) >= 2) {
        first <- parts[1]
        rest <- paste(parts[-1], collapse = " ")
        # Standardize the first name using the additional function.
        std_first <- standardize_first_name(first)
        return(paste(std_first, rest))
      } else {
        return(seg)
      }
    }, USE.NAMES = FALSE)
    final_name <- paste(new_segments, collapse = " and ")
    return(final_name)
  }
  
  # Apply processing to all names.
  result <- sapply(names_column, process_name, USE.NAMES = FALSE)
  return(result)
}



# Apply function to dynamically standardize all names 
df <- yourdata%>%
  mutate(Standardized_Name = standardize_names(trainernamecollumn))

#This is to see how many distinct trainers and to check if they are actually distinct 
distinct<-df%>% 
  group_by(Standardized_Name)%>%
  distinct(Trainer)


#trim partnerships into senior trainer name only
trim_before_and <- function(x) {
  sapply(x, function(str) {
    # Split the string at the word "and" (surrounded by spaces)
    parts <- str_split(str, "\\s+and\\s+", simplify = TRUE)
    # Return the first part trimmed of leading/trailing whitespace
    str_trim(parts[1])
  })
}

data<-dt%>%
  group_by(HorseID)%>%
  mutate(First_Row_Words = list(unique(unlist(str_split(first(Trainer), "\\s+")))),  # Extract first row's words per group
         Partial_Match = sapply(seq_along(Trainer), function(i) {
           current_words <- unique(unlist(str_split(Trainer[i], "\\s+")))  # Split current row into words
           sum(First_Row_Words[[1]] %in% current_words) >= 2  # Compare with first row of the group
         })) %>%
  ungroup() %>%
  select(-First_Row_Words)

# get the first trainer 
First_trainer<-dt %>%
  arrange(HorseID,RaceDate)%>%
  group_by(HorseID) %>%
  slice(1)%>%
  select(Horse,HorseID,TrainerName)

# Paste first trainer into the new collumn in the main dataset
setDT(dt)[setDT(First_trainer),First_trainer:=TrainerName,
                                 on=c("HorseID")]


# then just match if the two collumns in the race dataset
# You can then extract the ones that have at least one FALSE
groups_with_false <- data %>%
  group_by(HorseID) %>%
  filter(any(Partial_Match == FALSE)) %>%
  distinct(HorseID) %>%
  ungroup()

# append this back to your main dataset that you want to use
data<-data%>%
  mutate(Changed_trainers=ifelse(HorseID%in%groups_with_false$HorseID,TRUE,FALSE))
