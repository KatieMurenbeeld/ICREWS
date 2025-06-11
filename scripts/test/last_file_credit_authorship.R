library(CRediTas)
library(tidyverse)

# Create a credi table to export
cras_table <- template_create(authors = c("Kathryn Murenbeeld", 
                                          "Co Author1", 
                                          "Co Author 2"))
knitr::kable(cras_table)


# Save the table as a csv
#write.csv(cras_table, here::here("outputs/tables/icrews_archetypes_credit_table.csv"), row.names=FALSE)

fix(cras_table)

# Create the CRediT Statement
# save this file somewhere else not a temp file
textfile <- tempfile()

cras_write(cras_table, textfile, markdown = TRUE, quiet = TRUE)

read_file(textfile)
