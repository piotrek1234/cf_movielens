# Próba zastąpienia algorytmów IBCF, UBCF z recommenderlab własnym algorytmem
# Piotr Kuciński, Tomasz Rydzewski
#-------------------------------------
# Może się zdarzyć że przed uruchomieniem skryptu trzeba zmienić bieżący katalog
# na ten, który zawiera folder ml (setwd())
# ------------------------------------
# analiza danych wejściowych: funkcja datainfo()
# generowanie krzywych ROC i drzew: funkcja testy1()
#
#

library(recommenderlab)
library(party)
library(rpart)
library(ROCR)

wd <- function() {
  setwd('c:/users/piotrek/desktop/cf_movielens')
}

# zwraca macierz[użytkownik, film] - na przecięciu jest ocena lub 0
makeData <- function() {
  rawData = read.table(file.path("ml", "u.data"))
  
  # tworzenie pustej macierzy: każdy wiersz reprezentuje oceny od użytkownika,
  # każda kolumna to film
  moviesCount = 1682
  usersCount = 943
  usersRatings = matrix(rep(0, moviesCount*usersCount), ncol=moviesCount)
  
  for(ratingNr in 1:nrow(rawData)) {
    usersRatings[rawData[ratingNr, 1], rawData[ratingNr, 2]] = rawData[ratingNr, 3]
  }
  colnames(usersRatings) = c(as.character(1:moviesCount))
  return(usersRatings)
}

# zwraca filmy ocenione przez konkretnego użytkownika
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

# średnia różnica ocen między user1 a user2 na wspólnych filmach
averageDifference <- function(data, user1, user2) {
  sumDiffs = 0
  commonMovies = getCommonMovies(data, user1, user2)
  moviesCount = length(commonMovies)
  for(m in commonMovies) {
    sumDiffs = sumDiffs + abs(data[user1, m] - data[user2, m])
  }
  return(sumDiffs/moviesCount)
}

# zwraca filmy, które były najwięcej razy ocenione przez branych pod
# uwagę użytkowników (tych kórzy oglądali oceniany teraz film). Będą
# to atrybuty do budowy drzewa
getTreeAttributes <- function(data, watchers, user, movie, count=7) {
  moviesCount = 1682
  movies = vector()
  for(m in 1:moviesCount) {
    if(m != movie) {
      movies = append(movies, c(m, sum(data[watchers[,1],m]>0)))
    }
  }
  retval = matrix(movies, ncol=2, byrow=TRUE)
  colnames(retval) = c("movieId", "ratingsCount")
  return(head(retval[order(retval[,2], decreasing = TRUE),], count))
}

# zwraca dataframe zawierający oceny od branych pod uwagę użytkowników
# dla podanych atrybutów (filmów) i binaryzuje klasę przykładów (oceniany film)
prepareDataFrame <- function(data, movie, users, movies, threshold) {
  retval = as.data.frame(data[users[,1],append(movie, movies[,1])], row.names = as.character(users[,1]))
  #retval = as.data.frame(data[users,movies[,1]])
  retval[,1] = ifelse(retval[,1]>=threshold, 1, 0)
  return(retval)
}

# zwraca oceny od konkretnego użytkownika dla podanych atrybutów (filmów)
testDataFrame <- function(data, movies, user) {
  retval = as.data.frame(data[c(user, user), movies[,1]])
  return(retval)
}

# tworzy drzewo
plantTree <- function(user, movie, threshold, attrCount=7) {
  ratings=makeData()
  watchers = findSameMoviesWatchers(ratings, user, movie)
  att = getTreeAttributes(ratings, watchers, user, movie, attrCount)
  dtfr = prepareDataFrame(ratings, movie, watchers, att, threshold)
  frm = paste('`', movie, '` ~ ', paste(paste('`', att[,1], '`', sep=''), collapse=" + "), sep='')
  #tree = ctree(as.formula(frm), dtfr, weights = as.integer(watchers[,4]))
  tree = rpart(as.formula(frm), dtfr, method = "class", control = rpart.control(minsplit=5))
  
  print('P(1|x) = ')
  print(predict(tree, testDataFrame(ratings, att, user))[1,2])
  
  return(tree)
}

testDataFrame2 <- function(data, movies, user) {
  retval = as.data.frame(data[c(user, user), movies[,1]])
  return(retval)
}

# drzewa dla użytkownika i jego filmów. Tworzy krzywą ROC
plantTree1uNm <- function(ratings, user, movieList, threshold, attrCount=7) {
  #ratings=makeData()
  predictions = vector()
  labels = vector()
  
  pdfPath = "1uNm.pdf"
  #pdf(file=pdfPath)
  error = 0
  
  for(movie in movieList) {
    watchers = findSameMoviesWatchers(ratings, user, movie) # użytkownicy którzy oglądali te filmy co user
    att = getTreeAttributes(ratings, watchers, user, movie, attrCount) # atrybuty do budowy drzewa
    dtfr = prepareDataFrame(ratings, movie, watchers, att, threshold) # dataframe do budowy drzewa
    frm = paste('`', movie, '` ~ ', paste(paste('`', att[,1], '`', sep=''), collapse=" + "), sep='') # formuła: od czego zależy klasa (ocena)
    tree = rpart(as.formula(frm), dtfr, method = "class", control = rpart.control(minsplit=5)) # budowa drzewa

    propab = predict(tree, testDataFrame(ratings, att, user))[1,2] # ocena wyznaczona przez drzewo
    realrating = ratings[user, movie] # prawdziwa ocena użytkownika dla filmu
    
    print(paste('P(1|movie=', movie, ') = ', propab, ' (real rating: ', realrating, ')', sep=''))
    png(paste('trees/', user, '_', movie, '_th', threshold, '_attr', attrCount, '.png', sep=''))
    plot(tree, main=paste('Decision tree for user ', user, ' movie ', movie, 
                          '\n', 'P(1|movie)=', propab, ', real rating = ', realrating, sep=''))
    text(tree)
    dev.off()
    #roc:
    predictions = append(predictions, propab) # P(1|movie)
    labels = append(labels, ifelse(realrating>=threshold, 1, 0)) # prawdziwa klasa
    
    # porównanie rzeczywistej i "policzonej" oceny:
    realr = ifelse(realrating >= threshold, 1, 0)
    compr = ifelse(propab >= 0.5, 1, 0)
    error = error + abs(realr - compr)
    
  }
  avgError = error/length(movieList) # średni błąd oceny dla użytkownika
  #retval = as.matrix(trs, ncol=1, byrow=TRUE)
  #colnames(retval) = c("P(1|movie)")
  
  #roc:
  pred = prediction(predictions, labels)
  roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
  png(paste('roc/', user, 'th_', threshold, '.png', sep=''))
  plot(roc.perf, main=paste("ROC for user ", user, '\n(threshold: ', threshold, ')', sep=''))
  abline(a=0, b=1)
  dev.off()
  
  return(avgError)
}

# tworzy drzewa dla użytkownika i jego filmów
applyTree1uNm <- function(user, threshold, attrCount, moviesCount) {
  ratings = makeData()
  plantTree1uNm(ratings, user, head(getRatedMovies(makeData(), user), moviesCount), threshold, attrCount)
}
# tworzy drzewa dla pdanych użytkowników i ich filmów
applyTreeNuNm <- function(users, threshold, attrCount, moviesCount) {
  ratings = makeData()
  errors = vector()
  for(u in users) {
    error = plantTree1uNm(ratings, u, head(getRatedMovies(ratings, u), moviesCount), threshold, attrCount)
    errors = append(errors, c(u, error))
  }
  errMat = matrix(errors, ncol=2, byrow=TRUE)
  colnames(errMat) = c('user', 'avgError')
  
  return(errMat)
}

# tworzy drzewo dla użytkownika i filmu
applyTree1u1m <- function(user, movie, threshold, attrCount) {
  return(plantTree(user, movie, threshold, attrCount))
}

# informacje o danych wejściowych
datainfo <- function() {
  ratings = makeData()
  moviesCount = 1682
  usersCount = 943
  
  # histogram ocen dla filmów
  png('plots/hist(ratings).png')
  hist(ratings)
  dev.off()
  
  # histogram ocen dla filmów (oprócz braków)
  png('plots/hist(ratings_without_0).png')
  hist(ratings, xlim=range(c(1,5)), ylim=range(c(0,40000)))
  dev.off()
  
  # histogram ocen filmów na użytkownika
  moviesPerUser = vector()
  for(u in 1:usersCount) {
    moviesPerUser = append(moviesPerUser, sum(ratings[u,] > 0))
  }
  png('plots/hist(movies_per_user).png')
  hist(moviesPerUser, ylim=range(c(0, 450)), xlim=range(c(0, 800)))
  dev.off()
  print(paste('Średnio ocen na użytkownika: ', mean(moviesPerUser), sep=''))
  
  #histogram ocen na film
  ratingsPerMovie = vector()
  for(m in 1:moviesCount) {
    ratingsPerMovie = append(ratingsPerMovie, sum(ratings[,m] > 0))
  }
  png('plots/hist(ratings_per_movie.png')
  hist(ratingsPerMovie, ylim=range(c(0, 1200)), xlim=range(c(0, 600)))
  dev.off()
  print(paste('Średnio ocen na film: ', mean(ratingsPerMovie), sep=''))
}

testy1 <- function() {
  # w roc/ tworzą się wykresy ROC dla użytkowników
  # w trees/ tworzą się drzewa dla filmów i użytkowników
  # funkcja applyTree1uNm zwróci średni błąd dla wyznaczenia ocen użytkownikowi
  
  # threshold=3, attributes=5
  applyTree1uNm(587,3,5,10)
  applyTree1uNm(654,3,5,10)
  applyTree1uNm(120,3,5,10)
  applyTree1uNm(58,3,5,10)
  
  # threshold=3, attributes=10
  applyTree1uNm(587,3,10,10)
  applyTree1uNm(654,3,10,10)
  applyTree1uNm(120,3,10,10)
  applyTree1uNm(58,3,10,10)
  
  # threshold=4, attributes=5
  applyTree1uNm(587,4,5,10)
  applyTree1uNm(654,4,5,10)
  applyTree1uNm(120,4,5,10)
  applyTree1uNm(58,4,5,10)
}
