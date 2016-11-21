
makedeck<-function(n=6){
  card<-rep(c('ace','two','three','four','five','six',
          'seven','eight','nine','ten',
          'jack','queen','king'),4)
  
  deck<-rep(c(11,2:10,rep(10,3)),4)
  
  names(deck)<-card
  deck<-rep(deck,n)
  return(deck)
}


shuffledeck<-function(deckin){
  shuflen<-length(deckin)
  return(deckin[sample(1:shuflen,shuflen)])
}

gamelist<-function(deckcount=6){
  gamedeckout<-makedeck(deckcount)
  deckshufflesout<-0
  deckcountout<-deckcount
  gamedeckout<-shuffledeck(gamedeckout)
  cardsdealtout<-0
  endcardout<-sample(218:250,1)
  dealerhandout<-vector()
  playerhandout<-vector()
  #T= hand is over
  dealerdoneout<-F
  playerdoneout<-F
  blackjackout<-0
  winout<-0
  tieout<-0
  lossout<-0
  bustout<-0
  amountleftout<-0
  totalbetamountout<-0
  singlebetout<-1
  curbetout<-singlebetout
  outlist<-list(gamedeck=gamedeckout,decks=deckcountout,deckshuffles=deckshufflesout,cardsdealt=cardsdealtout,endcard=endcardout,
                dealerhand=dealerhandout,playerhand=playerhandout,dealerdone=dealerdoneout,playerdone=playerdoneout,
                blackjack=blackjackout,win=winout,tie=tieout,loss=lossout,bust=bustout,
                amountleft=amountleftout,totalbetamount=totalbetamountout,singlebet=singlebetout,curbet=curbetout)
  return(outlist)
}

firstdeal<-function(gamelistin){
  #dealers second card is always up
  gamelistin$curbet<-gamelistin$singlebet
  gamelistin$totalbetamount<-gamelistin$totalbetamount+gamelistin$curbet
  gamelistin$playerhand<-gamelistin$gamedeck[c(1,3)]
  gamelistin$dealerhand<-gamelistin$gamedeck[c(2,4)]
  gamelistin$gamedeck<-gamelistin$gamedeck[5:length(gamelistin$gamedeck)]
  if(sum(gamelistin$dealerhand)==21){
    gamelistin$playerdone<-T
    gamelistin$dealerdone<-T
    if(sum(gamelistin$playerhand)==21){
      gamelistin$tie<-gamelistin$tie+1
      gamelistin$amountleft<-gamelistin$amountleft+gamelistin$curbet
    }
    else{
      gamelistin$loss<-gamelistin$loss+1
    }
  }
  else if(sum(gamelistin$playerhand)==21){
    gamelistin$blackjack<-gamelistin$blackjack+1
    gamelistin$amountleft<-gamelistin$amountleft+gamelistin$curbet*2.5
    gamelistin$playerdone<-T
    gamelistin$dealerdone<-T
  }
  else if((sum(gamelistin$playerhand)==10) || (sum(gamelistin$playerhand)==11)){
    gamelistin$totalbetamount<-gamelistin$totalbetamount+gamelistin$curbet  
    gamelistin$curbet<-gamelistin$curbet*2
    gamelistin$playerhand<-c(gamelistin$playerhand,gamelistin$gamedeck[1])
    gamelistin$gamedeck<-gamelistin$gamedeck[2:length(gamelistin$gamedeck)]
    gamelistin$playerdone<-T
  }
  
  return(gamelistin)
}

handover<-function(gamelistin){
  gamelistin$cardsdealt<-gamelistin$cardsdealt+length(gamelistin$dealerhand)+length(gamelistin$playerhand)
  gamelistin$dealerhand<-vector()
  gamelistin$playerhand<-vector()
  if(gamelistin$cardsdealt>=gamelistin$endcard){
    gamelistin$gamedeck<-shuffledeck(makedeck(gamelistin$decks))
    gamelistin$cardsdealt<-0
    gamelistin$endcard<-sample(218:250,1)
    gamelistin$deckshuffles<-gamelistin$deckshuffles+1
  }
  gamelistin$playerdone<-F
  gamelistin$dealerdone<-F
  return(gamelistin)
}

evaldealer<-function(gamelistin){
  if(sum(gamelistin$dealerhand)==21){
    gamelistin$loss<-gamelistin$loss+1
    gamelistin$dealerdone<-T
  }
  else if(sum(gamelistin$dealerhand)<17){
    gamelistin$dealerhand<-c(gamelistin$dealerhand,gamelistin$gamedeck[1])
    gamelistin$gamedeck<-gamelistin$gamedeck[2:length(gamelistin$gamedeck)]
  }
  else if(sum(gamelistin$dealerhand)>21){
    if(11%in%gamelistin$dealerhand){
      gamelistin$dealerhand[min(which(gamelistin$dealerhand==11))]<-1
    }
    else{
      gamelistin$win<-gamelistin$win+1
      gamelistin$amountleft<-gamelistin$amountleft+gamelistin$curbet
      gamelistin$dealerdone<-T
    }
  }
  else{
    if(sum(gamelistin$dealerhand)>sum(gamelistin$playerhand)){
      gamelistin$loss<-gamelistin$loss+1
    }
    else if(sum(gamelistin$dealerhand)==sum(gamelistin$playerhand)){
      gamelistin$tie<-gamelistin$tie+1
      gamelistin$amountleft<-gamelistin$amountleft+gamelistin$curbet
    }
    else{
      gamelistin$win<-gamelistin$win+1
      gamelistin$amountleft<-gamelistin$amountleft+2*gamelistin$curbet
    }
    gamelistin$dealerdone<-T
  }
  return(gamelistin)
  
}

s1<-function(gamelistin){
  if(sum(gamelistin$playerhand)<=11){
    gamelistin$playerhand<-c(gamelistin$playerhand,gamelistin$gamedeck[1])
    gamelistin$gamedeck<-gamelistin$gamedeck[2:length(gamelistin$gamedeck)]
  }
  else{
    gamelistin$playerdone<-T
  }
  return(gamelistin)
}

s2<-function(gamelistin){
  if(sum(gamelistin$playerhand)==21){
    gamelistin$playerdone<-T
    gamelistin$dealerdone<-T
    gamelistin$win<-gamelistin$win+1
    gamelistin$amountleft<-gamelistin$amountleft+2*gamelistin$curbet
  }
  
  else if(sum(gamelistin$playerhand)<17){
    gamelistin$playerhand<-c(gamelistin$playerhand,gamelistin$gamedeck[1])
    gamelistin$gamedeck<-gamelistin$gamedeck[2:length(gamelistin$gamedeck)]
  }
  else if(sum(gamelistin$playerhand)>21){
    if(11%in%gamelistin$playerhand){
      gamelistin$playerhand[min(which(gamelistin$playerhand==11))]<-1
    }
    else{
      gamelistin$bust<-gamelistin$bust+1
      gamelistin$playerdone<-T
      gamelistin$dealerdone<-T
    }
  }
  else{
    gamelistin$playerdone<-T
  }
  return(gamelistin)
}


s3<-function(gamelistin){
  if((gamelistin$dealerhand[2]+10)>sum(gamelistin$playerhand)){
    gamelistin$playerhand<-c(gamelistin$playerhand,gamelistin$gamedeck[1])
    gamelistin$gamedeck<-gamelistin$gamedeck[2:length(gamelistin$gamedeck)]
  }
  
  else if(sum(gamelistin$playerhand)>21){
    if(11%in%gamelistin$playerhand){
      gamelistin$playerhand[min(which(gamelistin$playerhand==11))]<-1
    }
    else{
      gamelistin$bust<-gamelistin$bust+1
      gamelistin$playerdone<-T
      gamelistin$dealerdone<-T
    }
  }
    
  else{
    gamelistin$playerdone<-T
  }
    
  return(gamelistin)
}

s4<-function(gamelistin){

  if(sum(gamelistin$playerhand)<17){
    gamelistin$playerhand<-c(gamelistin$playerhand,gamelistin$gamedeck[1])
    gamelistin$gamedeck<-gamelistin$gamedeck[2:length(gamelistin$gamedeck)]
  }
  
  else if(sum(gamelistin$playerhand)<18 & 11%in%gamelistin$playerhand){
    gamelistin$playerhand<-c(gamelistin$playerhand,gamelistin$gamedeck[1])
    gamelistin$gamedeck<-gamelistin$gamedeck[2:length(gamelistin$gamedeck)]
  }
  
  else if(sum(gamelistin$playerhand)==21){
    gamelistin$playerdone<-T
    gamelistin$dealerdone<-T
    gamelistin$win<-gamelistin$win+1
    gamelistin$amountleft<-gamelistin$amountleft+2*gamelistin$curbet
  }
  
  else if(sum(gamelistin$playerhand)>21){
    if(11%in%gamelistin$playerhand){
      gamelistin$playerhand[min(which(gamelistin$playerhand==11))]<-1
    }
    else{
      gamelistin$bust<-gamelistin$bust+1
      gamelistin$playerdone<-T
      gamelistin$dealerdone<-T
    }
  }
  
  else{
    gamelistin$playerdone<-T
  }
  
  return(gamelistin)
}

s5<-function(gamelistin){
  
  if(sum(gamelistin$playerhand)<17){
    gamelistin$playerhand<-c(gamelistin$playerhand,gamelistin$gamedeck[1])
    gamelistin$gamedeck<-gamelistin$gamedeck[2:length(gamelistin$gamedeck)]
  }
  
  else if(sum(gamelistin$playerhand)<19 & 11%in%gamelistin$playerhand){
    gamelistin$playerhand<-c(gamelistin$playerhand,gamelistin$gamedeck[1])
    gamelistin$gamedeck<-gamelistin$gamedeck[2:length(gamelistin$gamedeck)]
  }
  
  else if(sum(gamelistin$playerhand)==21){
    gamelistin$playerdone<-T
    gamelistin$dealerdone<-T
    gamelistin$win<-gamelistin$win+1
    gamelistin$amountleft<-gamelistin$amountleft+2*gamelistin$curbet
  }
  
  else if(sum(gamelistin$playerhand)>21){
    if(11%in%gamelistin$playerhand){
      gamelistin$playerhand[min(which(gamelistin$playerhand==11))]<-1
    }
    else{
      gamelistin$bust<-gamelistin$bust+1
      gamelistin$playerdone<-T
      gamelistin$dealerdone<-T
    }
  }
  
  else{
    gamelistin$playerdone<-T
  }
  
  return(gamelistin)
}


s6<-function(gamelistin){
  if(gamelistin$dealerhand[2]<=6 & gamelistin$dealerhand[2]>=3){
    return(s1(gamelistin))
  }
  else{
    return(s2(gamelistin))
  }
}




s1ddtest<-gamelist()


ptm <- proc.time()
i<-1


while(s1ddtest$totalbetamount<=20){
  # print(i)
  # i<-i+1
  
  s1ddtest<-firstdeal(s1ddtest)
  
  while(s1ddtest$playerdone==F){
    s1ddtest<-s1(s1ddtest)
  }

  print('player hand')
  print(s1ddtest$playerhand)
  print('current bet')
  print(s1ddtest$curbet)

  while(s1ddtest$dealerdone==F){
    s1ddtest<-evaldealer(s1ddtest)
  }
  
  print('dealer hand')
  print(s1ddtest$dealerhand)
  
  s1ddtest<-handover(s1ddtest)  
}




all<-list(s1test,s2test,s3test,s4test,s5test,s6test)

for(i in 1:6){
  print(all[[i]]$win)
}


ptm<-proc.time()-ptm