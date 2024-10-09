# Regex to locate links in text
find_link <- regex("
  \\[   # Grab opening square bracket
  .+?   # Find smallest internal text as possible
  \\]   # Closing square bracket
  \\(   # Opening parenthesis
  .+?   # Link text, again as small as possible
  \\)   # Closing parenthesis
  ",
                   comments = TRUE)

# Function that removes links from text and replaces them with superscripts that are 
# referenced in an end-of-document list. 
sanitize_links <- function(text){
  if(PDF_EXPORT){
    str_extract_all(text, find_link) %>% 
      pluck(1) %>% 
      walk(function(link_from_text){
        title <- link_from_text %>% str_extract('\\[.+\\]') %>% str_remove_all('\\[|\\]') 
        link <- link_from_text %>% str_extract('\\(.+\\)') %>% str_remove_all('\\(|\\)')
        
        # add link to links array
        links <<- c(links, link)
        
        # Build replacement text
        new_text <- glue('{title}<sup>{length(links)}</sup>')
        
        # Replace text
        text <<- text %>% str_replace(fixed(link_from_text), new_text)
      })
  }
  text
}

# Take entire positions dataframe and removes the links 
# in descending order so links for the same position are
# right next to eachother in number. 
strip_links_from_cols <- function(data, cols_to_strip){
  for(i in 1:nrow(data)){
    for(col in cols_to_strip){
      data[i, col] <- sanitize_links(data[i, col])
    }
  }
  data
}
print_section <- function(position_data, section_id) {
  position_data %>%
    filter(in_resume) %>%  
    filter(section == section_id) %>% 
    arrange(desc(end)) %>% 
    mutate(id = 1:n()) %>%
    pivot_longer(
      starts_with('description'),
      names_to = 'description_num',
      values_to = 'description'
    ) %>% 
    filter(!is.na(description) | description_num == 'description_1') %>%
    group_by(id) %>% 
    mutate(
      descriptions = list(description),
      no_descriptions = is.na(description)
    ) %>% 
    ungroup() %>%
    filter(description_num == 'description_1') %>% 
    mutate(
      id = row_number(),
      start = as.character(start),  
      end = as.character(end)  
    ) %>%
    mutate(
      timeline = ifelse(is.na(start) | start == end, end, glue("{end} - {start}")),
      description_bullets = ifelse(
        no_descriptions,
        ' ',
        map_chr(descriptions, ~paste('-', ., collapse = '\n'))
      )
    ) %>% 
    strip_links_from_cols(c('title', 'description_bullets')) %>% 
    mutate_all(~ifelse(is.na(.), 'N/A', .)) %>% 
    # Handle the glue_data part with the necessary columns
    # Convert list column to a character string for glue_data
    mutate(descriptions = map_chr(descriptions, ~ paste(.x, collapse = "\n"))) %>%
    # Use glue_data for the final formatting
    glue_data(
      "### {title}",
      "\n\n",
      "{loc}",
      "\n\n",
      "{institution}",
      "\n\n",
      "{timeline}", 
      "\n\n",
      "{description_bullets}",
      "\n\n\n"
    )
}

print_section_oral <- function(position_data, section_id) {
  position_data %>%
    filter(in_resume, section == section_id) %>%
    arrange(desc(end)) %>%
    mutate(id = 1:n()) %>%
    pivot_longer(
      starts_with('description'),
      names_to = 'description_num',
      values_to = 'description'
    ) %>%
    filter(!is.na(description) | description_num == 'description_1') %>%
    group_by(id) %>%
    mutate(
      descriptions = list(description),
      no_descriptions = is.na(description)
    ) %>%
    ungroup() %>%
    filter(description_num == 'description_1') %>%
    arrange(desc(end), desc(start)) %>%
    mutate(
      start_display = as.character(start),
      end_display = as.character(end)
    ) %>%
    mutate(
      timeline = ifelse(is.na(start_display) | start_display == end_display, 
                        end_display, 
                        glue("{start_display} - {end_display}")),
      description_bullets = ifelse(
        no_descriptions,
        ' ',
        {
          unique_descriptions <- unique(unlist(descriptions))  # Ensure unique descriptions
          cat("Unique descriptions:", unique_descriptions, "\n")  # Debugging output
          paste('-', unique_descriptions, collapse = '\n')  # Combine unique descriptions into bullet points
        }
      )
    ) %>%
    strip_links_from_cols(c('title', 'description_bullets')) %>%
    mutate_all(~ifelse(is.na(.), 'N/A', .)) %>%
    # Ensure we only grab the unique title and other columns
    reframe(
      title = unique(title),
      loc = unique(loc),
      institution = unique(institution),
      timeline = unique(timeline),
      description_bullets = description_bullets  # Use the modified description_bullets
    ) %>%
    glue_data(
      "### {title}",
      "\n\n",
      "{loc}",
      "\n\n",
      "{institution}",
      "\n\n",
      "{timeline}", 
      "\n\n",
      "{description_bullets}",
      "\n\n\n"
    )
}

print_section_month <- function(position_data, section_id) {
  position_data %>%
    filter(in_resume) %>%  # Add this line to filter for in_resume == TRUE
    filter(section == section_id) %>% 
    arrange(desc(end)) %>% 
    mutate(id = 1:n()) %>% 
    pivot_longer(
      starts_with('description'),
      names_to = 'description_num',
      values_to = 'description'
    ) %>% 
    filter(!is.na(description) | description_num == 'description_1') %>%
    group_by(id) %>% 
    mutate(
      descriptions = list(description),
      no_descriptions = is.na(description)
    ) %>% 
    ungroup() %>% 
    filter(description_num == 'description_1') %>% 
    mutate(
      id = row_number(),
      start = as.character(start),  # Convert start column to character
      end = as.character(end)  # Convert end column to character
    ) %>%
    mutate(
      # Convert to dates for sorting
      start_date = dmy(paste("01", start)),
      end_date = dmy(paste("01", end)),
      # Retain original format for display
      start_display = start,
      end_display = end
    ) %>%
    arrange(desc(end_date), desc(start_date)) %>%
    mutate(
      timeline = ifelse(is.na(start_display) | start_display == end_display, 
                        end_display, 
                        glue("{start_display} - {end_display}")),
      description_bullets = ifelse(
        no_descriptions,
        ' ',
        map_chr(descriptions, ~ paste('-', ., collapse = '\n'))  # Ensure we handle lists properly
      )
    ) %>% 
    strip_links_from_cols(c('title', 'description_bullets')) %>% 
    mutate_all(~ifelse(is.na(.), 'N/A', .)) %>% 
    # Convert list column to a character string for glue_data
    mutate(description_bullets = paste(description_bullets, collapse = "\n")) %>%
    glue_data(
      "### {title}",
      "\n\n",
      "{loc}",
      "\n\n",
      "{institution}",
      "\n\n",
      "{timeline}", 
      "\n\n",
      "{description_bullets}",
      "\n\n\n"
    )
}

# Construct a bar chart of skills
build_skill_bars <- function(skills, out_of = 5){
  bar_color <- "#969696"
  bar_background <- "#d9d9d9"
  skills %>% 
    mutate(width_percent = round(100*level/out_of)) %>% 
    glue_data(
      "<div class = 'skill-bar'",
      "style = \"background:linear-gradient(to right,",
      "{bar_color} {width_percent}%,",
      "{bar_background} {width_percent}% 100%)\" >",
      "{skill}",
      "</div>"
    )
}


# Prints out from text_blocks spreadsheet blocks of text for the intro and asides. 
print_text_block <- function(text_blocks, label) {
  # Extract the text for the specified label
  text <- filter(text_blocks, loc == label)$text %>%
    # Sanitize links in the text (assuming sanitize_links() does this)
    sanitize_links() %>%
    # Wrap each text block in <p> tags
    sapply(function(block) paste0("<p>", block, "</p>")) %>%
    # Concatenate all text blocks into a single string
    paste(collapse = "")
  
  # Wrap the entire content in a <div> with class "subtitle" and print it
  cat(text)
}



print_reference <- function(reference) {
 reference %>%
    mutate(
      position_2 = ifelse(is.na(position_2), "", position_2), # Handle NA values in position_2
      positions = ifelse(position_2 == "", position_1, paste(position_1, position_2, sep = "; ")), # Concatenate positions
      email = ifelse(is.na(email), "N/A", email) # Handle NA values in email
    ) %>%
    rowwise() %>%
    glue_data(
      "{name}",
      "\n{positions}",
      "\n{email}",
      "\n\n" # Add space between entries
    ) 
}



print_skills <- function(skills) {
  skills %>%
    mutate(
      skill_set = set_1,
      # Ensure 'name' is character, not factor, for `if_else` compatibility
      name = as.character(name),
      # Generate display text for each row
      name_display = if_else(is.na(name), "", glue("{name}: \n"))
    ) %>%
    glue_data(
      "{name_display}{skill_set}\n\n" # Add space between entries, only display name if not "NA", and add colon and space after the name
    ) 
}
