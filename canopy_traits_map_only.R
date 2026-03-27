# install.packages(c("readr", "dplyr", "tidyr", "leaflet", "htmltools", "purrr", "htmlwidgets"))

library(readr)
library(dplyr)
library(tidyr)
library(leaflet)
library(htmltools)
library(purrr)
library(htmlwidgets)
library(stringr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(ggtext)

# READ UPDATED FILE
dat <- read_csv("Canopy_traits_summary.csv", show_col_types = FALSE)

# Function to make all text safe UTF-8
clean_utf8 <- function(x) {
  x <- as.character(x)
  x <- enc2utf8(x)
  x <- iconv(x, from = "", to = "UTF-8", sub = "")
  x
}

# Clean all character columns
dat <- dat %>%
  mutate(across(where(is.character), clean_utf8))

# Helper to italicise species
italicise_species <- function(x) {
  x <- clean_utf8(x)
  x <- x[!is.na(x) & trimws(x) != ""]
  if (length(x) == 0) return("None listed")
  paste0("• <i>", x, "</i>", collapse = "<br/>")
}

# Combine species columns
dat2 <- dat %>%
  mutate(
    species_list = pmap(
      list(
        `Please now enter up to 5 species you plan to sample. Enter the scientific name of the species in each case. Please use the 'accepted name' on WoRMS.\n\nSpecies 1`,
        `Species 2`,
        `Species 3`,
        `Species 4`,
        `Species 5`,
        `List any additional species you plan to sample, if applicable`
      ),
      \(s1, s2, s3, s4, s5, s6) c(s1, s2, s3, s4, s5, s6)
    ),
    species_html = map_chr(species_list, italicise_species)
  )

# Build long table
sites_long <- bind_rows(
  dat2 %>%
    transmute(
      site = "site1",
      person = `What is your name?`,
      research_group = `What is the name of the Research Group you lead or work in?`,
      university = `What is the name of the institution where you are based?`,
      sampling = `When are you going to conduct your sampling and trait screening? Enter the expected completion date.`,
      submission = `When are you planning to submit your data to the Canopy Traits coordinators?`,
      species_html = species_html,
      lat = site1_lat2,
      lon = site1_lon3
    ),
  dat2 %>%
    transmute(
      site = "site2",
      person = `What is your name?`,
      research_group = `What is the name of the Research Group you lead or work in?`,
      university = `What is the name of the institution where you are based?`,
      sampling = `When are you going to conduct your sampling and trait screening? Enter the expected completion date.`,
      submission = `When are you planning to submit your data to the Canopy Traits coordinators?`,
      species_html = species_html,
      lat = site2_lat4,
      lon = site2_lon5
    ),
  dat2 %>%
    transmute(
      site = "site3",
      person = `What is your name?`,
      research_group = `What is the name of the Research Group you lead or work in?`,
      university = `What is the name of the institution where you are based?`,
      sampling = `When are you going to conduct your sampling and trait screening? Enter the expected completion date.`,
      submission = `When are you planning to submit your data to the Canopy Traits coordinators?`,
      species_html = species_html,
      lat = site3_lat6,
      lon = site3_lon7
    ),
  dat2 %>%
    transmute(
      site = "site4",
      person = `What is your name?`,
      research_group = `What is the name of the Research Group you lead or work in?`,
      university = `What is the name of the institution where you are based?`,
      sampling = `When are you going to conduct your sampling and trait screening? Enter the expected completion date.`,
      submission = `When are you planning to submit your data to the Canopy Traits coordinators?`,
      species_html = species_html,
      lat = site4_lat,
      lon = site4_lon
    )
) %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  mutate(
    lat = as.numeric(lat),
    lon = as.numeric(lon),
    person = trimws(clean_utf8(person)),
    research_group = clean_utf8(research_group),
    university = clean_utf8(university),
    sampling = clean_utf8(sampling),
    submission = clean_utf8(submission),
    
    # ✅ Maggie Reddy site-specific species fix
    species_html = case_when(
      person == "Maggie Reddy" & lon < 0 ~
        "• <i>Laminaria digitata</i><br/>• <i>Laminaria hyperborea</i>",
      person == "Maggie Reddy" & lon > 0 ~
        "• <i>Ecklonia maxima</i><br/>• <i>Ecklonia radiata</i>",
      TRUE ~ species_html
    ),
    
    popup_text = paste0(
      "<div style='font-family: Arial, sans-serif; font-size: 13px; line-height: 1.35;'>",
      
      "<div style='font-size: 15px; margin-bottom: 4px;'><b>Lead:</b> ",
      ifelse(is.na(person) | trimws(person) == "", "NA", person),
      "</div>",
      
      "<div>", ifelse(is.na(research_group) | trimws(research_group) == "", "NA", research_group), "</div>",
      "<div>", ifelse(is.na(university) | trimws(university) == "", "NA", university), "</div>",
      
      "<div style='margin-top: 8px;'><b>Species:</b><br/>", species_html, "</div>",
      
      "<div style='margin-top: 8px;'>",
      "<b>Sampling:</b> ", ifelse(is.na(sampling) | trimws(sampling) == "", "NA", sampling), "<br/>",
      "<b>Data submission:</b> ", ifelse(is.na(submission) | trimws(submission) == "", "NA", submission),
      "</div>",
      
      "</div>"
    ),
    
    hover_label = ifelse(
      is.na(person) | trimws(person) == "",
      "NA",
      person
    )
  ) %>%
  filter(lat >= -90, lat <= 90, lon >= -180, lon <= 180)

# Create map
m <- leaflet(sites_long) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    lng = ~lon,
    lat = ~lat,
    radius = 5,
    stroke = TRUE,
    weight = 1,
    fillOpacity = 0.85,
    popup = ~lapply(popup_text, HTML),
    label = ~hover_label
  )

# View map
m

# Save map
saveWidget(m, "canopy_traits_world_map.html", selfcontained = TRUE)