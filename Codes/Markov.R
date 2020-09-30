########## Markov chains 

# https://cran.r-project.org/web/packages/markovchain/vignettes/an_introduction_to_markovchain_package.pdf



library(markovchain)
library(dplyr)
library(igraph)




source("C:/Users/Sébastien CARARO/Desktop/ATP& &Others/R codes persos/PBP/functions.R")





ppoint_srv1 <- 0.7
ppoint_srv2 <- 0.6

score0 <- matrix(0, nrow = 1, ncol = 9)
colnames(score0) <- c("V1","V2","Sets1","Sets2","Game1","Game2","Point1","Point2","Server")
score0[1, "Server"] <- 1


########### I. Let's modelize a game  #################

s0game <- t(matrix(0, nrow = 17, ncol = 1))
colnames(s0game) <- c("0-0","0-15","15-0","15-15",
                      "30-0","0-30","40-0","30-15",
                      "15-30","0-40","40-15","15-40",
                      "30-30(DEUCE)","40-30(A-40)","30-40(40-A)",
                      "HOLD", "BREAK")
s0game[1,"0-0"] <- 1



resGAME <- function(ppoint_server, s_game){
  MC_game1 <- MCgame2(ppoint_server)  #cause we want only phold 
  #plot(MC_game1, edge.arrow.size=0.1, vertex.size = 15, main = "Game model")
  resGAME <- s_game*(MC_game1 ^ 10)
  #print(resGAME)
  return(resGAME)
}











########### II. Let's modelize a tiebreak  #################



s0tb <- t(matrix(0, nrow = 54, ncol = 1))
colnames(s0tb) <- c("0-0","0-1","1-0","1-1",
                    "2-0","0-2","3-0","2-1",
                    "1-2","0-3","4-0","3-1",
                    "2-2","1-3","0-4","5-0",
                    "4-1", "3-2","2-3","1-4",
                    "0-5","5-1","4-2","3-3",
                    "2-4","1-5","5-2","4-3","3-4",
                    "2-5","5-3","4-4","3-5","5-4",
                    "4-5", "5-5","6-5","5-6",
                    "6-6","SETv1","SETv2","6-0",
                    "6-1","6-2","6-3","6-4","4-6",
                    "3-6","2-6","1-6","0-6","7-7","7-6","6-7")
s0tb[1,"0-0"] <- 1


resTIE2 <- function (ppoint_srv1, ppoint_srv2, s_tb){
  MC_tb <- MCtb2(ppoint_srv1, ppoint_srv2)
  #plot(MC_tb, edge.arrow.size=0.5, vertex.size = 15, main = "Tie-break model")
  resTIE <- s_tb*(MC_tb ^ 1000)
  #print(resTIE)
  return(resTIE)
}

########### III. Let's modelize a set  #################
phold1 <- resGAME(ppoint_srv1, s0game)[1,"HOLD"]
phold2 <- resGAME(ppoint_srv2, s0game)[1, "HOLD"]
ptie1 <- resTIE2(ppoint_srv1, ppoint_srv2, s0tb)[1, "SETv1"]


s0set <- t(matrix(0, nrow = 41, ncol = 1))
colnames(s0set) <- c("0-0","0-1","1-0","1-1",
                     "2-0","0-2","3-0","2-1",
                     "1-2","0-3","4-0","3-1",
                     "2-2","1-3","0-4","5-0",
                     "4-1", "3-2","2-3","1-4",
                     "0-5","5-1","4-2","3-3",
                     "2-4","1-5","5-2","4-3","3-4",
                     "2-5","5-3","4-4","3-5","5-4",
                     "4-5", "5-5","6-5","5-6",
                     "6-6","SETv1","SETv2")
s0set[1,"0-0"] <- 1




resSET <- function(phold1, phold2, ptie1, s_set){
  MC_set <- MCset(phold1, phold2, ptie1)
  #plot(MC_set, edge.arrow.size=0.5, vertex.size = 15, main = "Set model")
  resSET <- s_set*(MC_set ^ 14)    # max 13 games
  #print(resSET)
  return(resSET)
}

############# IV. Let's modelize a match ##############

# probabilities of winning a set : input parameters 
pset_v1 <- resSET( phold1, phold2, ptie1, s0set)[1,"SETv1"]
                   


s0match <- t(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0))
colnames(s0match) <- c("0-0","0-1","1-0","1-1","2-0","0-2","2-1","1-2","V1","V2")




resMATCH <- function(pset_v1, s_match){
  MC_match <- MCmatch(pset_v1)
  #plot(MC_match, edge.arrow.size=0.5, vertex.size = 15, main = "Match model") 
  resMATCH <- s_match*(MC_match ^ 5)   # 2 sets 
  #print(resMATCH)
  return(resMATCH)
}


resMATCH2 <- function(pset_v1, s_match){
  MC_match <- MCmatch2(pset_v1)
  #plot(MC_match, edge.arrow.size=0.5, vertex.size = 15, main = "Match model") 
  resMATCH <- s_match*(MC_match ^ 5)   # 2 sets 
  #print(resMATCH)
  return(resMATCH)
}


############# V. Let's concatenate all of these blocks ##############





# A. Let's modelize from scratch 

s1match <- s0match
s1set <- s0set
s1game <- s0game
s1tb <- s0tb


ppoint_srv1 <- 0.8
ppoint_srv2 <- 0.75

phold1 <- resGAME(ppoint_srv1, s0game)[1,"HOLD"]
phold2 <- resGAME(ppoint_srv2, s0game)[1, "HOLD"]
ptie1 <- resTIE2(ppoint_srv1, ppoint_srv2, s0tb)[1, "SETv1"]


pset_v1 <- resSET( phold1, phold2, ptie1, s0set)[1,"SETv1"]


resTEST <- resMATCH2(pset_v1, s0match)
resTEST

# B. Then we can modelize during the first set taking only the gamescore (no sub game predictions)
# First we input the begining score 


predict1 <- function(gamescore, phold1, phold2, ptie1, pset_v1, s0match, s0set, s0game, s0tb){


s1match <- s0match
s1set <- s0set
s1game <- s0game
s1tb <- s0tb
  


s1set[1, "0-0"] <- 0
s1set[1, gamescore] <- 1
s1set


s1match[1,"1-0"] <- resSET( phold1, phold2, ptie1, s1set)[1,"SETv1"]
s1match[1,"0-1"] <- resSET( phold1, phold2, ptie1, s1set)[1,"SETv2"]
s1match[1, "0-0"] <- 0

resTEST <- resMATCH2(pset_v1, s1match)
  

return(resTEST)
}

# C. We can modelize during the second set 



predict2 <- function(setscore, gamescore, phold1, phold2, ptie1, pset_v1, s0match, s0set, s0game, s0tb){

s1match <- s0match
s1set <- s0set
s1game <- s0game
s1tb <- s0tb



s1match[1, "0-0"] <- 0
s1match[1, setscore] <- 1
s1match
s1set[1, "0-0"] <- 0
s1set[1, gamescore] <- 1
s1set




if (setscore == "1-0"){
  s1match[1,"2-0"] <- resSET( phold1, phold2, ptie1, s1set)[1,"SETv1"]
  s1match[1,"1-1"] <- resSET( phold1, phold2, ptie1, s1set)[1,"SETv2"]
  s1match[1, "1-0"] <- 0
  
  resTEST <- resMATCH2(pset_v1, s1match)
  resMATCH(pset_v1, s1match)

}

if (setscore == "0-1"){
  s1match[1,"0-2"] <- resSET( phold1, phold2, ptie1, s1set)[1,"SETv2"]
  s1match[1,"1-1"] <- resSET( phold1, phold2, ptie1, s1set)[1,"SETv1"]
  s1match[1, "1-0"] <- 0
  
  resTEST <- resMATCH2(pset_v1, s1match)
  resMATCH(pset_v1, s1match)
  
}

return(resTEST)

}



# D. We can modelize during the third set : this is just one set simulation 




predict3 <- function(gamescore, phold1, phold2, ptie1, pset_v1, s0match, s0set, s0game, s0tb){
s1match <- s0match
s1set <- s0set
s1game <- s0game
s1tb <- s0tb


setscore <- "1-1"


s1match[1, "0-0"] <- 0
s1match[1, setscore] <- 1
s1match
s1set[1, "0-0"] <- 0
s1set[1, gamescore] <- 1
s1set


s1match[1,"2-1"] <- resSET( phold1, phold2, ptie1, s1set)[1,"SETv1"]
s1match[1,"1-2"] <- resSET( phold1, phold2, ptie1, s1set)[1,"SETv2"]
s1match[1, "1-1"] <- 0

resTEST <- resMATCH2(pset_v1, s1match)
resMATCH(pset_v1, s1match)

return(resTEST)
}


# E. Then we combine all these subfunctions into a simpler one 


determiMM <- function(ppoint_srv1, ppoint_srv2, setscore, gamescore, 
                  ingamesc, server = 1, s0match, s0set, s0game, s0tb){
  # Computing constants
  phold1 <- resGAME(ppoint_srv1, s0game)[1,"HOLD"]
  phold2 <- resGAME(ppoint_srv2, s0game)[1, "HOLD"]
  ptie1 <- resTIE2(ppoint_srv1, ppoint_srv2, s0tb)[1, "SETv1"]
  pset_v1 <- resSET( phold1, phold2, ptie1, s0set)[1,"SETv1"]
  
  
  # Modelization
  cat("Let's modelize this match from the score...",setscore, gamescore, ingamesc, "\n")
  if (setscore == "0-0"){resTEST <- predict1(gamescore, phold1, phold2, ptie1, pset_v1, s0match, s0set, s0game, s0tb)}
  if (setscore == "1-1"){resTEST <- predict3(gamescore, phold1, phold2, ptie1, pset_v1, s0match, s0set, s0game, s0tb)}
  else{resTEST <- predict2(setscore, gamescore, phold1, phold2, ptie1, pset_v1, s0match, s0set, s0game, s0tb)}
  return(resTEST)
}








