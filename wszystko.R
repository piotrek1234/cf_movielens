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

# modyfikuje dane zastępująć oceny liczbą 1 lub 0 (w zależności czy ocena różni się
# nie więcej niż diff od oceny użytkownika)
modifyDataSameRating <- function(data, users, user, diff) {
  # users można zastąpić przez findSameMoviesWatchers(data, user, ileś)[,1]
  moviesCount = 1682
  usersCount = 943
  ratings = matrix(rep(NA, (moviesCount+1)*length(users)), byrow=TRUE, ncols=moviesCount)
  # w ratings będą siedzieć informacje czy ocena jest bliska czy nie
  i=1
  for(u in users) {
    for(m in 1:moviesCount) {
      # ostatnia kolumna - id użytkownika
      ratings[i, moviesCount+1] = u
      # sprawdzić czy jeden ma wpisane 0 (nie oglądał) i coś z tym zrobić
      # jeśli różnica jest mniejsza niż diff to gdzieś wstawić
      if(abs(data[u, m] - data[user, m]) < diff) {
        # różnica ocen mniejsza niż diff
        # rozkminić indeksy czy są na pewno spoko
        ratings[i, m] = 1
      }
      else {
        ratings[i, m] = 0
      }
      i=i+1
    }
    # powycinać kolumny ze zbędnymi filmami
    # wynikiem ma być macierz: wiersz to użytkownik, w kolumnach są filmy, a wartość 1 lub 0
    # to info czy ocenił film tak jak my. W pierwszej kolumnie można ewentualnie powpisywać
    # id użytkownika
  }
}
