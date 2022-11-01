```{r extract-sections, echo = FALSE, eval = FALSE}
#This chunk is to extract the r code contained in each child. I use it so that I can run sections more easily from the console. It's not necessary for the analysis. This is why I keep it eval = FALSE.

library(here)
library(knitr)
library(purrr)

list_of_main_sections = c(
  "abundance",
  "biomass_local",
  "biomass_regional",
  "body_size_distribution",
  "body_size_median",
  "data",
  "evaporation",
  "tests"
)

for (main_section in list_of_main_sections) {
  sections.path = here("r_files", main_section, "sections")
  extracted.path = here("r_files", main_section, "extracted_sections")
  
  r.files = list.files(sections.path)
  rmd.files = r.files[grepl(".Rmd", r.files)]
  
  map(rmd.files, function(file.i) {
    file.name = gsub(".Rmd", "", file.i)
    extracted.file = paste0(file.name, ".R")
    purl(file.path(sections.path, file.i),
         file.path(extracted.path, extracted.file))
  })
}

rm(extracted.path, list_of_main_sections, main_section, r.files, rmd.files, sections.path)
```