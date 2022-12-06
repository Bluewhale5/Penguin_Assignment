
#Function to clean data

cleaning <- function(data_raw){
  data_raw %>%
    clean_names() %>%
    remove_empty(c("rows", "cols")) %>%
    select(-starts_with("delta")) %>%
    select(-comments)
}

#Function to subset the body mass data and remove missing data for both body mass and sex.

remove_empty_body_mass <- function(data_clean){
  data_clean %>%
    filter(!is.na(body_mass_g)) %>%
    filter(!is.na(sex)) %>%
    select(species, sex, body_mass_g)
}
