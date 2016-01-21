library(recommenderlab)

makeData <- function() {
  # zwraca macierz[użytkownik, film]
  rawData = read.table(file.path("ml", "u.data"))
  
  # tworzenie pustej macierzy: każdy wiersz reprezentuje oceny od użytkownika,
  # każda kolumna to film
  moviesCount = 1682
  usersCount = 943
  usersRatings = matrix(rep(0, moviesCount*usersCount), ncol=moviesCount)
  
  # wypełnienie macierzy
  for(ratingNr in 1:nrow(rawData)) {
    usersRatings[rawData[ratingNr, 1], rawData[ratingNr, 2]] = rawData[ratingNr, 3]
  }
  
  return(usersRatings)
}

printDataForUser <- function(data, userId) {
  print(data[userId, ])
}

printDataForMovie <- function(data, movieId) {
  print(data[, movieId])
}

getRatedMovies <- function(data, userId) {
  moviesList = vector()
  moviesCount = 1682
  for(i in 1:moviesCount) {
    if(data[userId, i] != 0) {
      moviesList = append(moviesList, i)
    }
  }
  return (moviesList)
}
