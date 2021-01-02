##### Clean functions 
## This file is structure to build Markov chains models, in this order :
# I - Game model 
# II - Tie-break model
# III - Set model
# IV - Match model

library(markovchain)

################### 0 - Define constants #######################################
# (a) Standard state for a game
s0game <- t(matrix(0, nrow = 17, ncol = 1))
colnames(s0game) <- c("0-0","0-15","15-0","15-15",
                      "30-0","0-30","40-0","30-15",
                      "15-30","0-40","40-15","15-40",
                      "30-30(DEUCE)","40-30(A-40)","30-40(40-A)",
                      "HOLD", "BREAK")
s0game[1,"0-0"] <- 1

# (b) Standard state for a tie-break
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

# (c) Standard state for a set
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

# (d) Standard state for a match
s0match <- t(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0))
colnames(s0match) <- c("0-0","0-1","1-0","1-1","2-0","0-2","2-1","1-2","V1","V2")

################# I - Game model  ##############################################
MCgame2 <- function(ppoint_server) {  
  # Build the transition matrix for a game
  # Input : probability that the server wins a point
  # Output : Markov transition matrix
  
  # Define the states
  ppoint_ret = 1 - ppoint_server
  STATES = c("0-0","0-15","15-0","15-15",
             "30-0","0-30","40-0","30-15",
             "15-30","0-40","40-15","15-40",
             "30-30(DEUCE)","40-30(A-40)","30-40(40-A)",
             "HOLD", "BREAK")
  tMat = matrix(0, nrow = 17, ncol = 17, byrow = TRUE)
  rownames(tMat) = STATES
  colnames(tMat) = STATES
  
  # Set the correct probabilities
  tMat["0-0","15-0"] <- ppoint_server
  tMat["15-0","30-0"] <- ppoint_server
  tMat["0-15","15-15"] <- ppoint_server
  tMat["30-0","40-0"] <- ppoint_server
  tMat["15-15","30-15"] <- ppoint_server
  tMat["0-30","15-30"] <- ppoint_server
  tMat["40-0","HOLD"] <- ppoint_server
  tMat["30-15","40-15"] <- ppoint_server
  tMat["40-15","HOLD"] <- ppoint_server
  tMat["40-30(A-40)","HOLD"] <- ppoint_server
  tMat["0-40","15-40"] <- ppoint_server
  tMat["15-40","30-40(40-A)"] <- ppoint_server
  tMat["30-40(40-A)","30-30(DEUCE)"] <- ppoint_server
  tMat["15-30","30-30(DEUCE)"] <- ppoint_server
  tMat["30-30(DEUCE)","40-30(A-40)"] <- ppoint_server
  
  tMat["0-0","0-15"] <- ppoint_ret
  tMat["15-0","15-15"] <- ppoint_ret
  tMat["0-15","0-30"] <- ppoint_ret
  tMat["30-0","30-15"] <- ppoint_ret
  tMat["15-15","15-30"] <- ppoint_ret
  tMat["0-30","0-40"] <- ppoint_ret
  tMat["40-0","40-15"] <- ppoint_ret
  tMat["30-15","30-30(DEUCE)"] <- ppoint_ret
  tMat["40-15","40-30(A-40)"] <- ppoint_ret
  tMat["40-30(A-40)","30-30(DEUCE)"] <- ppoint_ret
  tMat["0-40","BREAK"] <- ppoint_ret
  tMat["15-40","BREAK"] <- ppoint_ret
  tMat["30-40(40-A)","BREAK"] <- ppoint_ret
  tMat["15-30","15-40"] <- ppoint_ret
  tMat["30-30(DEUCE)","30-40(40-A)"] <- ppoint_ret
  
  # Stationary states
  tMat["BREAK","BREAK"] <- 1
  tMat["HOLD","HOLD"] <- 1
  
  # Create the Markov Chain Object
  MC_game2 <- new("markovchain", states = STATES,
                  transitionMatrix = tMat, name ="MCgame2")
  return(MC_game2)
  
}

resGAME <- function(ppoint_server, s_game, graph = FALSE){
  # This function computes outcomes probabilities for a service game
  # ppoint_server : probability that the server wins a point. Between 0 and 1
  # s_game : state of the game in a Markov sense. If you want to start from 0-0 just take s0game
  # Otherwise just put 1 in the score you want to start from, e.g. if I want to compute probabilities from 0-15
  # I just need to set this : s_game <- s0game;s_game[1,'0-0'] <- 0; s_game[1, '0-15'] <- 1
  # graph : should we display the Markov Chain representation ? boolean
  #
  # Output : Markov matrix containing stable states (HOLD or BREAK) probabilities
  MC_game1 <- MCgame2(ppoint_server)  
  if(graph){plot(MC_game1, edge.arrow.size=0.1, vertex.size = 15, main = "Game model")}
  resGAME <- s_game*(MC_game1 ^ 10000)
  return(resGAME)
}

################## II - Tie-break model ########################################
MCtb2 <- function(ppoint_srv1, ppoint_srv2) {
  # Build the transition matrix for a tie-break
  # Input : probability that the servers win a point when serving (2 probas)
  # Output : Markov transition matrix  
  
  # Define the matrix
  STATES = c("0-0","0-1","1-0","1-1",
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
  
  tMat = matrix(0, nrow = 54, ncol = 54, byrow = TRUE)
  rownames(tMat) = STATES
  colnames(tMat) = STATES
  
  # Set the probabilities
  tMat["0-0","1-0"] <- ppoint_srv1
  tMat["3-0","4-0"] <- ppoint_srv1
  tMat["2-1","3-1"] <- ppoint_srv1
  tMat["1-2","2-2"] <- ppoint_srv1
  tMat["0-3","1-3"]  <- ppoint_srv1
  tMat["4-0","5-0"] <- ppoint_srv1
  tMat["3-1","4-1"] <- ppoint_srv1
  tMat["2-2","3-2"] <- ppoint_srv1
  tMat["1-3","2-3"] <- ppoint_srv1
  tMat["0-4","1-4"] <- ppoint_srv1
  tMat["6-1","SETv1"] <- ppoint_srv1
  tMat["5-2","6-2"] <- ppoint_srv1
  tMat["4-3","5-3"] <- ppoint_srv1
  tMat["3-4","4-4"]  <- ppoint_srv1
  tMat["2-5","3-5"] <- ppoint_srv1
  tMat["1-6","2-6"] <- ppoint_srv1
  tMat["6-2","SETv1"] <- ppoint_srv1
  tMat["5-3","6-3"] <- ppoint_srv1
  tMat["4-4","5-4"] <- ppoint_srv1
  tMat["3-5","4-5"]  <- ppoint_srv1
  tMat["2-6","3-6"] <- ppoint_srv1
  tMat["6-5","SETv1"] <- ppoint_srv1
  tMat["5-6","6-6"] <- ppoint_srv1
  tMat["6-6","7-6"] <- ppoint_srv1

  tMat["0-0","0-1"] <- 1 - ppoint_srv1
  tMat["3-0","3-1"] <- 1 - ppoint_srv1
  tMat["2-1","2-2"] <- 1 - ppoint_srv1
  tMat["1-2","1-3"] <- 1 - ppoint_srv1
  tMat["0-3","0-4"]  <- 1 - ppoint_srv1
  tMat["4-0","4-1"] <- 1 - ppoint_srv1
  tMat["3-1","3-2"] <- 1 - ppoint_srv1
  tMat["2-2","2-3"] <- 1 - ppoint_srv1
  tMat["1-3","1-4"] <- 1 - ppoint_srv1
  tMat["0-4","0-5"] <- 1 - ppoint_srv1
  tMat["6-1","6-2"] <- 1 - ppoint_srv1
  tMat["5-2","5-3"] <- 1 - ppoint_srv1
  tMat["4-3","4-4"] <- 1 - ppoint_srv1
  tMat["3-4","3-5"]  <- 1 - ppoint_srv1
  tMat["2-5","2-6"] <- 1 - ppoint_srv1
  tMat["1-6","SETv2"] <- 1 - ppoint_srv1
  tMat["6-2","6-3"] <- 1 - ppoint_srv1
  tMat["5-3","5-4"] <- 1 - ppoint_srv1
  tMat["4-4","4-5"] <- 1 - ppoint_srv1
  tMat["3-5","3-6"]  <- 1 - ppoint_srv1
  tMat["2-6","SETv2"] <- 1 - ppoint_srv1
  tMat["6-5","6-6"] <- 1 - ppoint_srv1
  tMat["5-6","SETv2"] <- 1 - ppoint_srv1
  tMat["3-4","3-5"] <- 1 - ppoint_srv1
  tMat["6-6","6-7"] <- 1 - ppoint_srv1
  
  tMat["1-0","1-1"] <- ppoint_srv2
  tMat["0-1","0-2"] <- ppoint_srv2
  tMat["2-0","2-1"] <- ppoint_srv2
  tMat["1-1","1-2"] <- ppoint_srv2
  tMat["0-2","0-3"]  <- ppoint_srv2
  tMat["0-3","0-4"] <- ppoint_srv2
  tMat["5-0","5-1"] <- ppoint_srv2
  tMat["4-1","4-2"] <- ppoint_srv2
  tMat["3-2","3-3"] <- ppoint_srv2
  tMat["2-3","2-4"] <- ppoint_srv2
  tMat["1-4","1-5"] <- ppoint_srv2
  tMat["0-5","0-6"] <- ppoint_srv2
  tMat["6-0","6-1"] <- ppoint_srv2
  tMat["5-1","5-2"]  <- ppoint_srv2
  tMat["4-2","5-2"] <- ppoint_srv2
  tMat["3-3","3-4"] <- ppoint_srv2
  tMat["2-4","2-5"] <- ppoint_srv2
  tMat["1-5","1-6"] <- ppoint_srv2
  tMat["0-6","SETv2"] <- ppoint_srv2
  tMat["6-3","6-4"] <- ppoint_srv2
  tMat["5-4","5-5"] <- ppoint_srv2
  tMat["4-5","4-6"] <- ppoint_srv2
  tMat["3-6","SETv2"] <- ppoint_srv2
  tMat["6-4","6-5"] <- ppoint_srv2
  tMat["5-5","5-6"] <- ppoint_srv2
  tMat["4-6","SETv2"] <- ppoint_srv2
  tMat["6-7","SETv2"] <- ppoint_srv2
  tMat["7-6","7-7"] <- ppoint_srv2
  tMat["4-2","4-3"] <- ppoint_srv2
  tMat["7-7","5-6"] <- ppoint_srv2

  tMat["1-0","2-0"] <- 1 - ppoint_srv2
  tMat["0-1","1-1"] <- 1 - ppoint_srv2
  tMat["2-0","3-0"] <- 1 - ppoint_srv2
  tMat["1-1","2-1"] <- 1 - ppoint_srv2
  tMat["0-2","1-2"]  <- 1 - ppoint_srv2
  tMat["0-3","1-3"] <- 1 - ppoint_srv2
  tMat["5-0","6-0"] <- 1 - ppoint_srv2
  tMat["4-1","5-1"] <- 1 - ppoint_srv2
  tMat["3-2","4-2"] <- 1 - ppoint_srv2
  tMat["2-3","3-3"] <- 1 - ppoint_srv2
  tMat["1-4","2-4"] <- 1 - ppoint_srv2
  tMat["0-5","1-5"] <- 1 - ppoint_srv2
  tMat["6-0","SETv1"] <- 1 - ppoint_srv2
  tMat["5-1","6-1"]  <- 1 - ppoint_srv2
  tMat["4-2","5-2"] <- 1 - ppoint_srv2
  tMat["3-3","4-3"] <- 1 - ppoint_srv2
  tMat["2-4","3-4"] <- 1 - ppoint_srv2
  tMat["1-5","2-5"] <- 1 - ppoint_srv2
  tMat["0-6","1-6"] <- 1 - ppoint_srv2
  tMat["6-3","SETv1"] <- 1 - ppoint_srv2
  tMat["5-4","6-4"] <- 1 - ppoint_srv2
  tMat["4-5","5-5"] <- 1 - ppoint_srv2
  tMat["3-6","4-6"] <- 1 - ppoint_srv2
  tMat["6-4","SETv1"] <- 1 - ppoint_srv2
  tMat["5-5","6-5"] <- 1 - ppoint_srv2
  tMat["4-6","5-6"] <- 1 - ppoint_srv2
  tMat["3-6","4-6"] <- 1 - ppoint_srv2
  tMat["6-7","7-7"] <- 1 - ppoint_srv2
  tMat["7-6","SETv1"] <- 1 - ppoint_srv2
  tMat["7-7","6-5"] <- 1 - ppoint_srv2
  
  # Set stationary states
  tMat["SETv1","SETv1"] <- 1
  tMat["SETv2","SETv2"] <- 1
  
  # Create the Markov Chain object
  MC_set <- new("markovchain", states = STATES,
                transitionMatrix = tMat, name ="MCset" )
  return(MC_set)
  
}

resTIE <- function (ppoint_srv1, ppoint_srv2, s_tb, graph = FALSE){
  # This function computes outcomes probabilities for a tie-break
  # ppoint_srv1 : probability that the first player wins a point on his serve. Between 0 and 1
  # ppoint_srv2 : probability that the second player wins a point on his serve. Between 0 and 1
  # s_tb : state of the tie-break in a Markov sense. If you want to start from 0-0 just take s0tb
  # Otherwise just put 1 in the score you want to start from, e.g. if I want to compute probabilities from 2-3
  # I just need to set this : s_tb <- s0tb;s_tb[1,'0-0'] <- 0; s_tb[1, '2-3'] <- 1
  # graph : should we display the Markov Chain representation ? boolean
  #
  # Output : Markov matrix containing stable states (SETv1 or SETv2) probabilities
  MC_tb <- MCtb2(ppoint_srv1, ppoint_srv2)
  if(graph){plot(MC_tb, edge.arrow.size=0.5, vertex.size = 15, main = "Tie-break model")}
  resTIE <- s_tb*(MC_tb ^ 1000)
  return(resTIE)
}

################## III - Set model #############################################
MCset <- function(phold1, phold2, ptie1) {
  # Build the transition matrix for a tie-break
  # Input : probability that the servers hold a game when serving (2 probas), probability that player 1 wins a tie-break
  # Output : Markov transition matrix  
  
  # Define the matrix
  STATES = c("0-0","0-1","1-0","1-1",
             "2-0","0-2","3-0","2-1",
             "1-2","0-3","4-0","3-1",
             "2-2","1-3","0-4","5-0",
             "4-1", "3-2","2-3","1-4",
             "0-5","5-1","4-2","3-3",
             "2-4","1-5","5-2","4-3","3-4",
             "2-5","5-3","4-4","3-5","5-4",
             "4-5", "5-5","6-5","5-6",
             "6-6","SETv1","SETv2")
  
  tMat = matrix(0, nrow = 41, ncol = 41, byrow = TRUE)
  rownames(tMat) = STATES
  colnames(tMat) = STATES
  
  # Set probabilities
  tMat["0-0","1-0"] <- phold1
  tMat["2-0","3-0"] <- phold1
  tMat["1-1","2-1"] <- phold1
  tMat["0-2","1-2"] <- phold1
  tMat["4-0","5-0"]  <- phold1
  tMat["3-1","4-1"] <- phold1
  tMat["2-2","3-2"] <- phold1
  tMat["1-3","2-3"] <- phold1
  tMat["0-4","1-4"] <- phold1
  tMat["5-1","SETv1"] <- phold1
  tMat["4-2","5-2"] <- phold1
  tMat["3-3","4-3"] <- phold1
  tMat["2-4","3-4"] <- phold1
  tMat["1-5","2-5"]  <- phold1
  tMat["5-3","SETv1"] <- phold1
  tMat["4-4","5-4"] <- phold1
  tMat["3-5","4-5"] <- phold1
  tMat["5-5","6-5"] <- phold1
  
  tMat["0-0","0-1"] <- 1 - phold1
  tMat["2-0","2-1"] <- 1 - phold1
  tMat["1-1","1-2"] <- 1 - phold1
  tMat["0-2","0-3"] <- 1 - phold1
  tMat["4-0","4-1"]  <- 1 - phold1
  tMat["3-1","3-2"] <- 1 - phold1
  tMat["2-2","2-3"] <- 1 - phold1
  tMat["1-3","1-4"] <- 1 - phold1
  tMat["0-4","0-5"] <- 1 - phold1
  tMat["5-1","5-2"] <- 1 - phold1
  tMat["4-2","4-3"] <- 1 - phold1
  tMat["3-3","3-4"] <- 1 - phold1
  tMat["2-4","2-5"] <- 1 - phold1
  tMat["1-5","SETv2"]  <- 1 - phold1
  tMat["5-3","5-4"] <- 1 - phold1
  tMat["4-4","4-5"] <- 1 - phold1
  tMat["3-5","SETv2"] <- 1 - phold1
  tMat["5-5","5-6"] <- 1 - phold1
  
  tMat["1-0","1-1"] <- phold2
  tMat["0-1","0-2"] <- phold2
  tMat["3-0","3-1"] <- phold2
  tMat["2-1","2-2"] <- phold2
  tMat["1-2","1-3"]  <- phold2
  tMat["0-3","0-4"] <- phold2
  tMat["5-0","5-1"] <- phold2
  tMat["4-1","4-2"] <- phold2
  tMat["3-2","3-3"] <- phold2
  tMat["2-3","2-4"] <- phold2
  tMat["1-4","1-5"] <- phold2
  tMat["0-5","SETv2"] <- phold2
  tMat["5-2","5-3"] <- phold2
  tMat["4-3","4-4"]  <- phold2
  tMat["3-4","3-5"] <- phold2
  tMat["2-5","SETv2"] <- phold2
  tMat["5-4","5-5"] <- phold2
  tMat["4-5","SETv2"] <- phold2
  tMat["5-6","SETv2"] <- phold2
  tMat["6-5","6-6"] <- phold2

  tMat["1-0","2-0"] <- 1 - phold2
  tMat["0-1","1-1"] <- 1 - phold2
  tMat["3-0","4-0"] <- 1 - phold2
  tMat["2-1","3-1"] <- 1 - phold2
  tMat["1-2","2-2"]  <- 1 - phold2
  tMat["0-3","1-3"] <- 1 - phold2
  tMat["5-0","SETv1"] <- 1 - phold2
  tMat["4-1","5-1"] <- 1 - phold2
  tMat["3-2","4-2"] <- 1 - phold2
  tMat["2-3","3-3"] <- 1 - phold2
  tMat["1-4","2-4"] <- 1 - phold2
  tMat["0-5","1-5"] <- 1 - phold2
  tMat["5-2","SETv1"] <- 1 - phold2
  tMat["4-3","5-3"]  <- 1 - phold2
  tMat["3-4","4-4"] <- 1 - phold2
  tMat["2-5","3-5"] <- 1 - phold2
  tMat["5-4","SETv1"] <- 1 - phold2
  tMat["4-5","5-5"] <- 1 - phold2
  tMat["5-6","6-6"] <- 1 - phold2
  tMat["6-5","SETv1"] <- 1 - phold2
  
  # Set stationary states
  tMat["SETv1","SETv1"] <- 1
  tMat["SETv2","SETv2"] <- 1
  
  # Set tie-break cases
  tMat["6-6","SETv1"] <- ptie1
  tMat["6-6","SETv2"] <- 1 - ptie1
  
  # Build the Markov chain object
  MC_set <- new("markovchain", states = STATES,
                transitionMatrix = tMat, name ="MCset" )
  return(MC_set)
  
}

resSET <- function(phold1, phold2, ptie1, s_set, graph = FALSE){
  # This function computes outcomes probabilities for a set
  # phold1 : probability that the first player wins a game on his serve. Between 0 and 1
  # phold2 : probability that the second player wins a game on his serve. Between 0 and 1
  # s_set : state of the set in a Markov sense. If you want to start from 0-0 just take s0set
  # Otherwise just put 1 in the score you want to start from, e.g. if I want to compute probabilities from 2-3
  # I just need to set this : s_set <- s0set;s_set[1,'0-0'] <- 0; s_set[1, '2-3'] <- 1
  # NB : player 1 serves first at the beginning of the match
  # graph : should we display the Markov Chain representation ? boolean
  #
  # Output : Markov matrix containing stable states (SETv1 or SETv2) probabilities
  MC_set <- MCset(phold1, phold2, ptie1)
  if(graph){plot(MC_set, edge.arrow.size=0.5, vertex.size = 15, main = "Set model")}
  resSET <- s_set*(MC_set ^ 100)    
  return(resSET)
}


################## IV - Match model ############################################

MCmatch <- function(pset_v1) {
  # Build the transition matrix for a tie-break
  # Input : probability that the player 1 wins a given set
  # Output : Markov transition matrix  
  
  # Define the matrix
  pset_v2 = 1 - pset_v1
  STATES = c("0-0","0-1","1-0","1-1","2-0","0-2","2-1","1-2","V1","V2")
  tMat = matrix(0, nrow = 10, ncol = 10, byrow = TRUE)
  rownames(tMat) = STATES
  colnames(tMat) = STATES
  
  # Set probabilities
  tMat["0-0","1-0"] <- pset_v1
  tMat["1-0","2-0"] <- pset_v1
  tMat["0-1","1-1"] <- pset_v1
  tMat["1-1","2-1"] <- pset_v1
  
  tMat["0-0","0-1"] <- pset_v2
  tMat["1-0","1-1"] <- pset_v2
  tMat["0-1","0-2"] <- pset_v2
  tMat["1-1","1-2"] <- pset_v2
  
  # Set stationary states
  tMat["2-0","2-0"] <- 1
  tMat["2-1","2-1"] <- 1
  tMat["0-2","0-2"] <- 1
  tMat["1-2","1-2"] <- 1
  
  tMat["V1","V1"] <- 1
  tMat["V2","V2"] <- 1
  
  # Build the Markov chain object
  MC_match <- new("markovchain", states = STATES,
                  transitionMatrix = tMat, name ="MCmatch" )
  return(MC_match)
  
}


resMATCH <- function(pset_v1, s_match, graph = FALSE){
  # This function computes outcomes probabilities for a match
  # pset_v1 : probability that the first player wins a set when the initial score is 0-0. Between 0 and 1
  # s_match : state of the set in a Markov sense. If you want to start from 0-0 just take s0match
  # Otherwise just put 1 in the score you want to start from, e.g. if I want to compute probabilities from 1-0
  # I just need to set this : s_match <- s0match;s_match[1,'0-0'] <- 0; s_match[1, '2-3'] <- 1
  # NB : player 1 serves first at the beginning of the match
  # graph : should we display the Markov Chain representation ? boolean
  #
  # Output : Markov matrix containing final score (2-0, 2-1, 1-2, 0-2) probabilities
  MC_match <- MCmatch(pset_v1)
  if(graph){plot(MC_match, edge.arrow.size=0.5, vertex.size = 15, main = "Match model")}
  resMATCH <- s_match*(MC_match ^ 5)   # 2 sets so 5 iterations is sufficient to obtain the stationary states
  return(resMATCH)
}


############# V. Let's concatenate all of these blocks ##############
# Then we can modelize during the first set taking only the gamescore (no sub game predictions)
# First we input the begining score 

predict1 <- function(gamescore, phold1, phold2, ptie1, pset_v1, s0match, s0set, s0game, s0tb){
  # This function computes outcome probabilities for a match when we are still in the first set
  # gamescore : state expressed as a chr : e.g. '3-4'. Must belong to s0set states 
  # phold1 : probability that the player 1 wins a game when serving
  # phold2 : probability that the player 2 wins a game when serving
  # ptie1 : probability that the player 1 wins a tie-break
  # pset_v1 : probability that the player 1 wins a set
  # s0match, s0set, s0game, s0tb : constants. Do not modify. 
  # 
  # Output : Markov matrix containing stable states (V1 or V2) probabilities
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
  
  resTEST <- resMATCH(pset_v1, s1match)
  
  return(resTEST)
}

# C. We can modelize during the second set 
predict2 <- function(setscore, gamescore, phold1, phold2, ptie1, pset_v1, s0match, s0set, s0game, s0tb){
  # This function computes outcome probabilities for a match when we are in the second set
  # setscore : state expressed as a chr : e.g. '1-0'. Either '0-1' or '1-0'
  # gamescore : state expressed as a chr : e.g. '3-4'. Must belong to s0set states 
  # phold1 : probability that the player 1 wins a game when serving
  # phold2 : probability that the player 2 wins a game when serving
  # ptie1 : probability that the player 1 wins a tie-break
  # pset_v1 : probability that the player 1 wins a set
  # s0match, s0set, s0game, s0tb : constants. Do not modify. 
  # 
  # Output : Markov matrix containing stable states (V1 or V2) probabilities
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
    
    resTEST <- resMATCH(pset_v1, s1match)
    resMATCH(pset_v1, s1match)
  }
  
  if (setscore == "0-1"){
    s1match[1,"0-2"] <- resSET( phold1, phold2, ptie1, s1set)[1,"SETv2"]
    s1match[1,"1-1"] <- resSET( phold1, phold2, ptie1, s1set)[1,"SETv1"]
    s1match[1, "0-1"] <- 0
    
    
    resTEST <- resMATCH(pset_v1, s1match)
    resMATCH(pset_v1, s1match)
    
  }
  return(resTEST)
}

# D. We can modelize during the third set : this is just one set simulation 
predict3 <- function(gamescore, phold1, phold2, ptie1, pset_v1, s0match, s0set, s0game, s0tb){
  # This function computes outcome probabilities for a match when we are in the third set
  # gamescore : state expressed as a chr : e.g. '3-4'. Must belong to s0set states 
  # phold1 : probability that the player 1 wins a game when serving
  # phold2 : probability that the player 2 wins a game when serving
  # ptie1 : probability that the player 1 wins a tie-break
  # pset_v1 : probability that the player 1 wins a set
  # s0match, s0set, s0game, s0tb : constants. Do not modify. 
  # 
  # Output : Markov matrix containing stable states (V1 or V2) probabilities
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
  
  resTEST <- resMATCH(pset_v1, s1match)
  resMATCH(pset_v1, s1match)
  
  return(resTEST)
}

#E.  -  Then we combine all these subfunctions into a simpler one 
determiMM <- function(ppoint_srv1, ppoint_srv2, setscore, gamescore, 
                      s0match, s0set, s0game, s0tb){
  # This function computes outcome probabilities for a match, given the score 
  # ppoint_srv1 : probability that the player 1 wins a point on his serve
  # ppoint_srv2 : probability that the player 1 wins a point on his serve
  # setscore : state expressed as a chr : e.g. '1-0'. Must belong to s0match states
  # gamescore : state expressed as a chr : e.g. '3-4'. Must belong to s0set states 
  # s0match, s0set, s0game, s0tb : constants. Do not modify. 
  # 
  # Output : Markov matrix containing stable states (V1 or V2) probabilities
  # Computing constants
  phold1 <- resGAME(ppoint_srv1, s0game)[1,"HOLD"]
  phold2 <- resGAME(ppoint_srv2, s0game)[1, "HOLD"]
  ptie1 <- resTIE(ppoint_srv1, ppoint_srv2, s0tb)[1, "SETv1"]
  pset_v1 <- resSET( phold1, phold2, ptie1, s0set)[1,"SETv1"]
  
  
  # Modelization
  cat("Let's modelize this match from the score...",setscore, gamescore, "\n")
  if (setscore == "0-0"){resTEST <- predict1(gamescore, phold1, phold2, ptie1, pset_v1, s0match, s0set, s0game, s0tb)}
  if (setscore == "1-1"){resTEST <- predict3(gamescore, phold1, phold2, ptie1, pset_v1, s0match, s0set, s0game, s0tb)}
  if (setscore == "1-0" | setscore == "0-1" ){resTEST <- predict2(setscore, gamescore, phold1, phold2, ptie1, pset_v1, s0match, s0set, s0game, s0tb)}
  #print(resTEST)
  return(resTEST)
}



