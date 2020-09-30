############## FUNCTIONS ###################

library(markovchain)


MCmatch <- function(pset_v1) {

pset_v2 = 1 - pset_v1
STATES = c("0-0","0-1","1-0","1-1","2-0","0-2","2-1","1-2","V1","V2")
tMat = matrix(0, nrow = 10, ncol = 10, byrow = TRUE)
rownames(tMat) = STATES
colnames(tMat) = STATES
tMat[1,3] <- pset_v1
tMat[3,5] <- pset_v1
tMat[4,7] <- pset_v1
tMat[2,4] <- pset_v1

tMat[1,2] <- pset_v2
tMat[2,6] <- pset_v2
tMat[3,4] <- pset_v2
tMat[4,8] <- pset_v2

tMat[5,5] <- 1
tMat[6,6] <- 1
tMat[7,7] <- 1
tMat[8,8] <- 1


tMat["V1","V1"] <- 1
tMat["V2","V2"] <- 1

tMat


MC_match <- new("markovchain", states = STATES,
                transitionMatrix = tMat, name ="MCmatch" )
return(MC_match)

}

MCmatch2 <- function(pset_v1) {
  
  pset_v2 = 1 - pset_v1
  STATES = c("0-0","0-1","1-0","1-1","2-0","0-2","2-1","1-2","V1","V2")
  tMat = matrix(0, nrow = 10, ncol = 10, byrow = TRUE)
  rownames(tMat) = STATES
  colnames(tMat) = STATES
  tMat[1,3] <- pset_v1
  tMat[3,5] <- pset_v1
  tMat[4,7] <- pset_v1
  tMat[2,4] <- pset_v1
  
  tMat[1,2] <- pset_v2
  tMat[2,6] <- pset_v2
  tMat[3,4] <- pset_v2
  tMat[4,8] <- pset_v2
  
  tMat[5,"V1"] <- 1
  tMat[6,"V2"] <- 1
  tMat[7,"V1"] <- 1
  tMat[8,"V2"] <- 1
  
  
  tMat["V1","V1"] <- 1
  tMat["V2","V2"] <- 1
  
  tMat
  
  
  MC_match <- new("markovchain", states = STATES,
                  transitionMatrix = tMat, name ="MCmatch" )
  return(MC_match)
  
}


MCgame <- function(ppoint_server) {
  
  ppoint_ret = 1 - ppoint_server
  STATES = c("0-0","0-15","15-0","15-15",
             "30-0","0-30","40-0","30-15",
             "15-30","0-40","40-15","15-40",
             "30-30(DEUCE)","40-30(A-40)","30-40(40-A)",
             "HOLD", "BREAK")
  tMat = matrix(0, nrow = 17, ncol = 17, byrow = TRUE)
  rownames(tMat) = STATES
  colnames(tMat) = STATES
  
  tMat["0-0","15-0"] <- ppoint_server
  tMat["15-0","30-0"] <- ppoint_server
  tMat["0-15","15-15"] <- ppoint_server
  tMat["30-0","40-0"] <- ppoint_server
  tMat["15-15","30-15"] <- ppoint_server
  tMat["0-30","15-30"] <- ppoint_server
  tMat["40-0","HOLD"] <- ppoint_server
  tMat["30-15","40-15"] <- ppoint_server
  tMat["40-15","HOLD"] <- ppoint_server
  tMat["30-30(DEUCE)","40-30(A-40)"] <- ppoint_server
  tMat["40-30(A-40)","HOLD"] <- ppoint_server
  tMat["0-40","15-40"] <- ppoint_server
  tMat["15-40","30-40(40-A)"] <- ppoint_server
  tMat["30-40(40-A)","30-30(DEUCE)"] <- ppoint_server
  tMat["15-30","30-30(DEUCE)"] <- ppoint_server
  
  tMat["0-0","0-15"] <- 1 - ppoint_server
  tMat["15-0","15-15"] <- 1 - ppoint_server
  tMat["0-15","0-30"] <- 1 - ppoint_server
  tMat["30-0","30-15"] <- 1 - ppoint_server
  tMat["15-15","15-30"] <- 1 - ppoint_server
  tMat["0-30","0-40"] <- 1 - ppoint_server
  tMat["40-0","40-15"] <- 1 - ppoint_server
  tMat["30-15","30-30(DEUCE)"] <- 1 - ppoint_server
  tMat["40-15","40-30(A-40)"] <- 1 - ppoint_server
  tMat["30-30(DEUCE)","30-40(40-A)"] <- 1 - ppoint_server
  tMat["40-30(A-40)","30-30(DEUCE)"] <- 1 - ppoint_server
  tMat["0-40","BREAK"] <- 1 - ppoint_server
  tMat["15-40","BREAK"] <- 1 - ppoint_server
  tMat["30-40(40-A)","BREAK"] <- 1 - ppoint_server
  tMat["15-30","15-40"] <- 1 - ppoint_server
  
  tMat["BREAK","BREAK"] <- 1
  tMat["HOLD","HOLD"] <- 1
  
  tMat
  
  
  MC_game <- new("markovchain", states = STATES,
                  transitionMatrix = tMat, name ="MCgame" )
  return(MC_game)
  
}

MCgame2 <- function(ppoint_server) {  
  #this fuction omits the possibility of infinit deuces : useful to compute only win probs
  
  ppoint_ret = 1 - ppoint_server
  STATES = c("0-0","0-15","15-0","15-15",
             "30-0","0-30","40-0","30-15",
             "15-30","0-40","40-15","15-40",
             "30-30(DEUCE)","40-30(A-40)","30-40(40-A)",
             "HOLD", "BREAK")
  tMat = matrix(0, nrow = 17, ncol = 17, byrow = TRUE)
  rownames(tMat) = STATES
  colnames(tMat) = STATES
  
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
  
  
  phold_deuce = (ppoint_server^2)/(ppoint_server^2 + ppoint_ret^2)
  tMat["30-30(DEUCE)","HOLD"] <- phold_deuce
  tMat["30-30(DEUCE)","BREAK"] <- 1 - phold_deuce
  
  tMat["BREAK","BREAK"] <- 1
  tMat["HOLD","HOLD"] <- 1
  
  tMat
  
  
  MC_game2 <- new("markovchain", states = STATES,
                 transitionMatrix = tMat, name ="MCgame2" )
  return(MC_game2)
  
}


MCset <- function(phold1, phold2, ptie1) {
  
  
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
  
  
  
  tMat["SETv1","SETv1"] <- 1
  tMat["SETv2","SETv2"] <- 1
  
  tMat["6-6","SETv1"] <- ptie1
  tMat["6-6","SETv2"] <- 1 - ptie1
  
  tMat
  
  
  MC_set <- new("markovchain", states = STATES,
                 transitionMatrix = tMat, name ="MCset" )
  return(MC_set)
  
}

MCtb <- function(ppoint_srv1, ppoint_srv2) {
  
  
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
  tMat["6-6","6-7"] <- 1 - ppoint_srv1
  tMat["3-4","3-5"] <- 1 - ppoint_srv1
  
  
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
  
  
  
  
  tMat["SETv1","SETv1"] <- 1
  tMat["SETv2","SETv2"] <- 1
  
  tMat
  
  
  MC_set <- new("markovchain", states = STATES,
                transitionMatrix = tMat, name ="MCset" )
  return(MC_set)
  
}

MCtb2 <- function(ppoint_srv1, ppoint_srv2) {
  #this fuction omits the possibility of infinit deuces : useful to compute only win probs
  
  
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
  
  
  
  
  
  p1_66 <- (((ppoint_srv1)*(1-ppoint_srv2))/((ppoint_srv1)*(1-ppoint_srv2) + (ppoint_srv2)*(1-ppoint_srv1)))
  tMat["6-6","SETv1"] <- p1_66
  tMat["6-6","SETv2"] <- 1 - p1_66
  
  
  
  
  tMat["SETv1","SETv1"] <- 1
  tMat["SETv2","SETv2"] <- 1
  
  tMat
  
  
  MC_set <- new("markovchain", states = STATES,
                transitionMatrix = tMat, name ="MCset" )
  return(MC_set)
  
}



