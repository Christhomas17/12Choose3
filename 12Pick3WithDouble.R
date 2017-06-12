
TotalPicks <- 15

Mega <- c(1,2,3)
Maxi <- c(4,5,6)
Major <- c(7,8,9)
Mini <- c(10,11,12)

MegaProb = 0
DoubleMegaProb = 0
MaxiProb = 0
DoubleMaxiProb = 0
MajorProb = 0
DoubleMajorProb = 0
MiniProb = 0
DoubleMiniProb = 0




allowed <- function(PickNumber){
  if(x[PickNumber]==0){
    
    x[PickNumber] <<- 1
    #assign(x[PickNumber],1,envir = .GlobalEnv)
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

reset <- function(PickNumber){
  x[PickNumber]<<- 0
}



pick_probs <- function(){
  
  for(a in 1:TotalPicks){
    allowed(a)
      
      for(b in 1:TotalPicks){
        if(allowed(b)){
          
          for(c in 1:TotalPicks){
            if(allowed(c)){
              if(is.win(c(a,b,c)) != "Win"){
                
                for(d in 1:TotalPicks){
                  if(allowed(d)){
                    if(is.win(c(a,b,c,d)) != "Win"){
                      
                      for(e in 1:TotalPicks){
                        if(allowed(e)){
                          if(is.win(c(a,b,c,d,e)) != "Win"){
                            
                            for(f in 1:TotalPicks){
                              if(allowed(f)){
                                if(is.win(c(a,b,c,d,e,f)) != "Win"){
                                  
                                  for(g in 1:TotalPicks){
                                    if(allowed(g)){
                                      if(is.win(c(a,b,c,d,e,f,g)) != "Win"){
                                        
                                        for(h in 1:TotalPicks){
                                          if(allowed(h)){
                                            if(is.win(c(a,b,c,d,e,f,g,h)) != "Win"){
                                              
                                              for(i in 1:TotalPicks){
                                                if(allowed(i)){
                                                  is.win(c(a,b,c,d,e,f,g,h,i)) 
                                                    
                                                  reset(i)
                                                }
                                              }
                                            }
                                          reset(h)
                                          }
                                        }
                                      }
                                    reset(g)
                                    }
                                  }
                                }
                              reset(f)
                              }
                            }
                          }
                          reset(e)
                        }
                      }
                    }
                  reset(d)
                  }
                }
              }
            reset(c)
            }
          }
        reset(b)  
        }
      }
    
    reset(a)
  }
}
                                                
                                                  
                                             
              

get.prob <- function(Picks){
  
  prob = 1
  denom = sum(PickWeights)
  num = 0
  
  for(i in 1:length((Picks))){
    denom = denom - num
    num = PickWeights[Picks[i]]
    prob = prob*num/denom
  }
  #print(prob)
  return(prob)
}

#Picks <- c(1,2,4,3)
#print(get.prob(Picks))
#c <- get.prob(Picks)

is.win <- function(Picks){
  
  #write.csv(data.frame(Picks), "results.csv", append = TRUE)
  #print(Picks)
  print(sum(MegaProb,DoubleMegaProb,MajorProb,DoubleMajorProb,MiniProb,
            DoubleMiniProb,MaxiProb,DoubleMaxiProb))
  if(sum(x[1:3]) == 3){
    if(sum(x[13:15])==3){
      DoubleMegaProb <<- DoubleMegaProb + get.prob(Picks)
    }
    else{
      MegaProb <<- MegaProb + get.prob(Picks)
    }
    return("Win")
  }
  
  if(sum(x[4:6]) == 3){
    if(sum(x[13:15])==3){
      DoubleMaxiProb <<- DoubleMaxiProb + get.prob(Picks)
    }
    else{
      MaxiProb <<- MaxiProb + get.prob(Picks)
    }
    return("Win")
  }
  
  if(sum(x[7:9]) == 3){
    if(sum(x[13:15])==3){
      DoubleMajorProb <<- DoubleMajorProb + get.prob(Picks)
    }
    else{
      MajorProb <<- MajorProb + get.prob(Picks)
    }
    return("Win")
  }
  
  if(sum(x[10:12]) == 3){
    if(sum(x[13:15])==3){
      DoubleMiniProb <<- DoubleMiniProb + get.prob(Picks)
    }
    else{
      MiniProb <<- MiniProb + get.prob(Picks)
    }
    return("Win")
  }
  
  else{
    return("Not a win")
  }
  
}
#if(is.win(Picks) != "Win")

#is.win(Picks)

#for testing purposes
#x = c(1,1,1,0,0,0,0,0,0,0,0,0,1,1,1)
#Picks <- c(1,2,4,7)





PickWeights<-read.csv("Weights88.csv")
PickWeights <- PickWeights[,-1]

#creates an array of 0. 1 will represent picks used and 0 will represent numbers 
#we can still choose
x <- numeric(15)




pick_probs()


PickWeights<-read.csv("88 Weights.csv")
PickWeights <- PickWeights[,-1]
names <- c("Mega Prob", "Mega Double Prob", "Maxi Prob", "Maxi Double Prob",
           "Major Prob", "Major Double Prob", "Mini Prob", "Mini Double Prob") 
results <- c(MegaProb, DoubleMegaProb, MaxiProb, DoubleMaxiProb, MajorProb, DoubleMajorProb,
             MiniProb, DoubleMiniProb)
FinalResults <- data.frame(names,results)
write.csv(FinalResults,"88probs.csv")

PickWeights<-read.csv("68 Weights.csv")
PickWeights <- PickWeights[,-1]
names <- c("Mega Prob", "Mega Double Prob", "Maxi Prob", "Maxi Double Prob",
           "Major Prob", "Major Double Prob", "Mini Prob", "Mini Double Prob") 
results <- c(MegaProb, DoubleMegaProb, MaxiProb, DoubleMaxiProb, MajorProb, DoubleMajorProb,
             MiniProb, DoubleMiniProb)
FinalResults <- data.frame(names,results)
write.csv(FinalResults,"68probs.csv")

PickWeights<-read.csv("38 Weights.csv")
PickWeights <- PickWeights[,-1]
names <- c("Mega Prob", "Mega Double Prob", "Maxi Prob", "Maxi Double Prob",
           "Major Prob", "Major Double Prob", "Mini Prob", "Mini Double Prob") 
results <- c(MegaProb, DoubleMegaProb, MaxiProb, DoubleMaxiProb, MajorProb, DoubleMajorProb,
             MiniProb, DoubleMiniProb)
FinalResults <- data.frame(names,results)
write.csv(FinalResults,"38probs.csv")

PickWeights<-read.csv("18 Weights.csv")
PickWeights <- PickWeights[,-1]
names <- c("Mega Prob", "Mega Double Prob", "Maxi Prob", "Maxi Double Prob",
           "Major Prob", "Major Double Prob", "Mini Prob", "Mini Double Prob") 
results <- c(MegaProb, DoubleMegaProb, MaxiProb, DoubleMaxiProb, MajorProb, DoubleMajorProb,
             MiniProb, DoubleMiniProb)
FinalResults <- data.frame(names,results)
write.csv(FinalResults,"18probs.csv")


