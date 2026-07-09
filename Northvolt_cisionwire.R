library(stringr)
library(dplyr)
library(tidyr)
library(readr)

# Läs in hela filen
text <- read_file("cision.txt")

# Dela upp artiklar
artiklar <- str_split(text, "={5,}")[[1]]

# Funktion för att extrahera data
parse_artikel <- function(a) {
  rader <- str_split(a, "\n")[[1]] %>% str_trim()
  rader <- rader[rader != ""]
  
  # Kolla om raden finns
  publicerat_print <- any(str_detect(rader, "Publicerat i print\\."))
  
  # ta bort raden från texten
  rader <- rader[!str_detect(rader, "Publicerat i print\\.")]
  
  # plocka ut info
  rubrik <- rader[1]
  företag <- rader[2]
  källa_rad <- rader[3]
  
  datum <- str_extract(källa_rad, "\\d{4}-\\d{2}-\\d{2}")
  text <- paste(rader[-(1:3)], collapse = " ")
  
  tibble(
    datum = datum,
    källa = "Cisionwire",
    företag = företag,
    rubrik = rubrik,
    publicerat_print = publicerat_print,
    text = text
  )
}

# Applicera på alla artiklar
df <- lapply(artiklar, parse_artikel) %>%
  bind_rows()

# Spara som CSV
#write_csv(df, "cisionwire.csv")
