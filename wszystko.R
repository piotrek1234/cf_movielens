library(recommenderlab)
library(party)

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
findSameMoviesWatchers <- function(data, user, movie, count=1000) {
  usersCount = 943
  users = vector()
  for(user2 in 1:usersCount) {
    if(user2 != user) {
      if(data[user2, movie] != 0) {
        commonMovies = getCommonCount(data, user, user2)
        if(commonMovies > 0) {
          difference = averageDifference(data, user, user2)
          users = append(users, c(user2, commonMovies, difference, 100*(difference^2+1)/(commonMovies+1)))
        }
      }
    }
  }
  retval = matrix(users, ncol=4, byrow=TRUE)
  retval[,4]=max(retval[,4])-retval[,4]
  colnames(retval) = c("userid", "sameMovies", "avgDifference", "importance")
  return(head(retval[order(retval[,4], decreasing = TRUE),], n=count))
}

averageDifference <- function(data, user1, user2) {
  sumDiffs = 0
  commonMovies = getCommonMovies(data, user1, user2)
  moviesCount = length(commonMovies)
  for(m in commonMovies) {
    sumDiffs = sumDiffs + abs(data[user1, m] - data[user2, m])
  }
  return(sumDiffs/moviesCount)
}

getTreeAttributes <- function(data, watchers, user, movie) {
  moviesCount = 1682
  movies = vector()
  for(m in 1:moviesCount) {
    movies = append(movies, c(m, sum(data[watchers[,1],m]>0)))
  }
  retval = matrix(movies, ncol=2, byrow=TRUE)
  colnames(retval) = c("movieId", "ratingsCount")
  return(retval[order(retval[,2], decreasing = TRUE),])
}

plantTree <- function(data, weights, user, movie) {
  
}

# modyfikuje dane zastępująć oceny liczbą 1 lub 0 (w zależności czy ocena różni się
# nie więcej niż diff od oceny użytkownika)
dataSameRating <- function(data, users, user, diff) {
  # users można zastąpić przez findSameMoviesWatchers(data, user, ileś)[,1]
  moviesCount = 1682
  usersCount = length(users)
  ratings = matrix(rep(NA, (moviesCount+1)*length(users)), byrow=TRUE, ncol=(moviesCount+1))
  # w ratings będą siedzieć informacje czy ocena jest bliska czy nie
  i=1
  for(u in 1:usersCount) {
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
