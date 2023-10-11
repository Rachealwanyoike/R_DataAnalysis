 install.packages("dplyr")
install.packages("purrr")
l-=[[[ibrary(dplyr)
library(purrr)

#Make the card faces
suits <- c('spades', 'clubs', 'diamonds', 'hearts')

suit <- unlist(map(suits, rep, 13))

head(suit)

#Make the card faces
faces <- c('king', 'queen', 'jack', 'ten', 'nine', 'eight', 'seven', 'six', 'five', 'four', 'three', 'two', 'ace')
face <- rep(faces, 4)

head(face)


#Make the deck
deck <- data.frame(face, suit)

head(deck)

#Check the deck
deck %>%
  group_by(suit) %>%
  summarise(cards = n())

#Deal the cards
deal <- function(x) {
  sample_n(deck, x, replace = F)
}

deal(5)

#Blackjack
deal <- function(x, set = NULL){
  hand <- set$myhand
  dealerhand <- set$dealerhand
  myhand = list()
  #remove the existing hand from the deck if it is included
  if(!is.null(hand)) {
    deck <- deck %>% anti_join(hand)
  }
  #remove the existing dealer's hand from the deck if it is included
  if(!is.null(dealerhand)) {
    deck <- deck %>% anti_join(dealerhand)
  }
  #if there is not dealer hand included then we need to get him 2 cards
  if(is.null(dealerhand)) {
    dealerhand <- sample_n(deck, 2, replace = F)
    #once this is done we need to remove those 2 cards from the deck
    deck <- deck %>% anti_join(dealerhand)
  }
  #the new cards need to be drawn from the deck
  newhand <- sample_n(deck, x, replace = F)
  #if the hand is not null then 
  if(!is.null(hand)) {
    myhand <- rbind(hand, newhand) 
  } else {
    myhand <- newhand
  }
  hand <- list(myhand = myhand, dealerhand = dealerhand)
  return(hand)
  
}

#Deal the cards and get your set
set <- deal(2)

#Deal to yourself
set$myhand
#Deal to the dealer
set$dealerhand
#Play
set <- deal(1, set)
set


