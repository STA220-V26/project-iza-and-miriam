# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tibble", "dplyr", "ggplot2","tarchetypes") # Packages that your targets need for their tasks.
  # format = "qs", # Optionally set the default storage format. qs is fast. try to use qs format
  #
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
  #
  #   controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}.
  # The following example is a controller for Sun Grid Engine (SGE).
  #
  #   controller = crew.cluster::crew_controller_sge(
  #     # Number of workers that the pipeline can scale up to:
  #     workers = 10,
  #     # It is recommended to set an idle time so workers can shut themselves
  #     # down if they are not running tasks.
  #     seconds_idle = 120,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.2".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# Run the R scripts in the R/ folder with your custom functions: every script file under R folder will run 
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:

#downloads the dataset if it dosen't exists yet
if (!fs::file_exists("data.zip")) {
curl::curl_download(
"https://github.com/eribul/cs/raw/refs/heads/main/data.zip",
"data.zip",
quiet = FALSE
)
}

list(
  # make the zipdata object refer to the data.zip file path
  tar_target(zipdata, "data.zip", format = "file"),

  # TODO: Something related to zip should be added here:
  # Unzip the downloaded dataset and return the extracted file names
  tar_target(csv_files, zip::unzip(zipdata)),

  tar_target(
    name = allergies, #name to reference what you create 
    command = read.csv("data-fixed/allergies.csv") #can call a function 
    # format = "qs" # Efficient storage for general data objects.
  ),
  tar_target(
    name = patients, #name to reference what you create 
    command = read.csv("data-fixed/patients.csv") #can call a function 
    # format = "qs" # Efficient storage for general data objects.
  ),
  tar_target(
    name = combined_data, #name to reference what you create 
    dplyr::left_join(patients, allergies, by = c("id" = "patient")) #can call a function 
    # format = "qs" # Efficient storage for general data objects.
  ),
  #pick out just the food allergies
  tar_target(
    food_rows,
    combined_data %>%
      dplyr::filter(category == "food")
  ),
#pick out those that have a food allergy, which the 1 indicates
  tar_target(
    food_patients,
    food_rows %>%
      dplyr::distinct(id) %>%
      dplyr::mutate(food_allergy = 1)
  ),

  tar_target(
    analysis_data,
    combined_data %>%
      dplyr::distinct(id, gender, race) %>%
      dplyr::left_join(food_patients, by = "id") %>%
      dplyr::mutate(
        food_allergy = ifelse(is.na(food_allergy), 0, food_allergy),
        gender = as.factor(gender),
        race = as.factor(race)
      )
  ),

  tar_target(
    allergy_model_gender,
    glm(
      food_allergy ~ gender,
      data = analysis_data,
      family = binomial()
    )
  ),

  tar_target(
    allergy_model_gender_race,
    glm(
      food_allergy ~ gender + race,
      data = analysis_data,
      family = binomial()
    )
  ),
  tar_target(
  allergy_prop_plot,
  analysis_data %>%
    dplyr::group_by(gender) %>%
    dplyr::summarise(
      prop_allergy = mean(food_allergy),
      .groups = "drop"
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = gender, y = prop_allergy, fill = gender)) +
    ggplot2::geom_col() +
    ggplot2::labs(
      title = "Proportion of food allergy by gender",
      x = "Gender",
      y = "Proportion with food allergy"
    ) +
    ggplot2::theme_minimal()
),
 tar_target(
  allergy_prop_plot_race,
  analysis_data %>%
    dplyr::group_by(race) %>%
    dplyr::summarise(
      prop_allergy = mean(food_allergy),
      .groups = "drop"
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = race, y = prop_allergy, fill = race)) +
    ggplot2::geom_col() +
    ggplot2::labs(
      title = "Proportion of food allergy by race",
      x = "Race",
      y = "Proportion with food allergy"
    ) +
    ggplot2::theme_minimal()
),
  tar_quarto(report, "report.qmd", quiet = TRUE)

)




#> summary(tar_read(allergy_model_gender))
# summary(tar_read(allergy_model_gender_race))


#tar_make()
#Tar_visnetwork

