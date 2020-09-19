#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
} 



#' @title Select Door
#' @description Select a Door Function
#' @details select a door of 1 
#' @param No parameters
#' @return return the first door selection  # number between 1 and 3
#' @examples object <- function
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick ) 
}



#' @title Open Goat Door
#' @description the administrator opens up the door that is a goat door. 
#' @details After the select door function, the administrator will need to open the next door that is not (!==) a car. 
#' @param include the game and pick from prior function
#' @return The function will return the number of the first opened door.# if contestant selected car,# number between 1 and 3
# randomly select one of two goats
#' @examples object <- function (parameters)
#' @export
open_goat_door <- function( game, a.pick )
{
  doors <- c(1,2,3)
  
  if( game[ a.pick ] == "car" )
  { 
    goat.doors <- doors[ game != "car" ] 
    opened.door <- sample( goat.doors, size=1 )
  }
  if( game[ a.pick ] == "goat" )
  { 
    opened.door <- doors[ game != "car" & doors != a.pick ] 
  }
  return( opened.door ) 
}



#' @title Change Door
#' @description Does the contestant want to change their pick
#' @details If the user wants to stick with their pick, we will call it the final pick. 
#' @param User opened.door function, and a.pick which was the first door picked
#' @return  Return the door number
#' @examples object <- function     return (number)
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
  doors <- c(1,2,3) 
  
  if( stay )
  {
    final.pick <- a.pick
  }
  if( ! stay )
  {
    final.pick <- doors[ doors != opened.door & doors != a.pick ] 
  }
  
  return( final.pick )  # number between 1 and 3
}



#' @title Determine winner
#' @description Determine if the end user won the game or not
#' @details In the final pick function, we figure out if the door picked was a winner by selecting the door with the car or if the door was a goat door, losing the game.
#' @param keep final pick from previous funtion
#' @return return win or lose
#' @examples object <- function return (WIN)
#' @export
determine_winner <- function( final.pick, game )
{
  if( game[ final.pick ] == "car" )
  {
    return( "WIN" )
  }
  if( game[ final.pick ] == "goat" )
  {
    return( "LOSE" )
  }
}





#' @title Play the Game
#' @description Play the game using create game() function, select foor, open door functions.
#' @details Play the game, the first door opened will be the goat door, the next is the pick from the user, does the user want to switch or stay, and did the user win or lose. 
#' @param there are no parameters
#' @return return game results
#' @examples return data frame of switch or sstay and what the outcome was in each case.
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )
  
  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )
  
  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title Play Games
#' @description Play the game on a loop
#' @details Walk through the game multiple times return the dataframe of the game outcome
#' @param run the loop 100 times
#' @return return results in a table.
#' @examples for (i in 1:n) loop
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1
  
  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )
  
  table( results.df ) %>% 
    prop.table( margin=1 ) %>%  # row proportions
    round( 2 ) %>% 
    print()
  
  return( results.df )
  
}
