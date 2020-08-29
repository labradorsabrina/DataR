friends <- c("Megan", "Janet", "Tina")

info_list <- list(
  Megan = list(
    jersey = 1363,
    color = "green"
  ),
  Janet = list(
    jersey = 6729,
    color = "green"
  ),
  Tina = list(
    jersey = 7501,
    color = "orange"
  ),
  Esther = list(
    jersey = 3432,
    color = "purple"
  ),
  Feng = list(
    jersey = 4221,
    color = "blue"
  )
)

print_information <- function(name) {
  print(paste(name, "is #", info_list[[name]]$jersey, "wearing the color", info_list[[name]]$color))
}

for (name in friends) {
  print_information(name)
}

race_results <- c("Gi", "Francesca", "Lea", "Vivian", "Jessica", "Esther", "Mary", "Yasmina", "Megan", "Janet", "Tiffany", "Kishan", "Feng", "Z", "Tina")

find_place <- function(runner) {
  for(place in 1:length(race_results)){
      if (race_results[place] == runner){
        return(place)
      }
  }
  return(length(race_results)+1)
}

find_place("Francesca")
find_place("Sabrina")

lapply("Francesca", find_place)
sapply("Francesca", find_place)