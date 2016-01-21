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

# zwraca liczbę wspólnie ocenionych filmów
getCommonCount <- function(data, user1, user2) {
  return(length(intersect(getRatedMovies(data, user1), getRatedMovies(data, user2))))
}

# zwraca wspólnie ocenione filmy
getCommonMovies <- function(data, user1, user2) {
  return(intersect(getRatedMovies(data, user1), getRatedMovies(data, user2)))
}

# znajduje użytkowników którzy ocenili te same filmy co user, sortuje i zwraca count wyników
findSameMoviesWatchers <- function(data, user, count) {
  usersCount = 943
  users = vector()
  for(user2 in 1:usersCount) {
    if(user2 != user) {
      users = append(users, c(user2, getCommonCount(data, user, user2)))
    }
  }
  retval = matrix(users, ncol=2, byrow=TRUE)
  colnames(retval) = c("userid", "sameMovies")
  return(head(retval[order(retval[,2], decreasing = TRUE),], n=count))
}
