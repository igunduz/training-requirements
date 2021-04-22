if (interactive()) {
  con <- stdin()
} else {
  con <- "stdin"
}

tictoctoe <- function(){
  board <- as.character(1:9)
  winner <- FALSE
  
  tr <- list(
    c(1,2,3),
    c(4,5,6),
    c(7,8,9),
    c(1,4,7),
    c(2,5,8),
    c(3,6,9),
    c(1,5,9),
    c(3,5,7)
  )
  
  display <- function(board){
    cat(" ",board[1],"|",board[2],"|",board[3],"\n")
    cat(" ---+---+---","\n")
    cat(" ",board[4],"|",board[5],"|",board[6],"\n")
    cat(" ---+---+---","\n")
    cat(" ",board[7],"|",board[8],"|",board[9],"\n")  } 
  
  update <- function(board, player, position){
    if(player == 1){
      board[position] <- "X"
    }
    else if(player == 2){
      board[position] <- "O"
    }
    return(board)
  }
  
  check_winner <- function(board){
    for(i in 1:length(tr)){
      if(sum(tr[[i]] %in% which(board == "X")) == 3){
        winner <- TRUE
        cat("X wins! Do you want to play again? Type yes (Y/y) or no (N/n)." , "\n")
        ans <- readLines(con = con, n = 1)
        if(ans != "Y" &&
           ans != "N" &&
           ans != "n" &&
           ans != "y"){
          cat("Invalid selection. You can restart the game if you want to play.")
          q()}
        else if (ans == "Y" ||
                 ans == "y"){
          tictoctoe()}
        else {
          q() }
      }
    }
    for(i in 1:length(tr)){
      if(sum(tr[[i]] %in% which(board == "O")) == 3){
        winner <- TRUE
        cat("O wins! Do you want to play again? Type yes (Y/y) or no (N/n)." , "\n")
        ans <- readLines(con = con, n = 1)
        if(ans != "Y" &&
           ans != "N" &&
           ans != "n" &&
           ans != "y"){
          cat("Invalid selection. You can restart the game if you want to play.")
          q()}
        else if (ans == "Y" ||
                 ans == "y"){
          tictoctoe()}
        else {
          q() }
      }
    }
    if(sum(board == as.character(1:9)) == 0 && winner == FALSE){
      cat("Game is over and nobody win. Do you want to play again? Type yes (Y) or no (N)." , "\n")
      ans <- readLines(con = con, n = 1)
      if(ans != "Y" &&
         ans != "N" &&
         ans != "n" &&
         ans != "y"){
        cat("Invalid selection. You can restart the game if you want to play.")
        q()}
      else if (ans == "Y" ||
               ans == "y"){
        tictoctoe()}
      else {
        q() }
    }
    return(winner)
  }
  
  check_moves <- function(board){ 
    notallowed <- TRUE
    while(notallowed){ 
      cat("Please type the position you want to play (1 to 9) :")
      position <- readLines(con = con, n = 1)
      if (position != "1" &&
          position != "2" &&
          position != "3" &&
          position != "4" &&
          position != "5" &&
          position != "6" &&
          position != "7" &&
          position != "8" &&
          position != "9") { 
        cat("Please enter a valid number!", "\n") }
      else { position <- as.double(position)
      if(board[position] != "X" && board[position] != "O"){
        board <- update(board,position)
        notallowed = FALSE }
      else if (board[position] == "X" || board[position] == "O"){
        cat("Already played there, try again. ", "\n")  }
      }
    }
    return(position)
  }
  
  boot_vs_p1 <- function(board,player){
    while(winner == FALSE){
      if(player == 1){
        position <- check_moves(board)
        board <- update(board, player, position)
        winner <- check_winner(board)
        player <- 2 
      }
      else if(player == 2){
        position <- as.double(sample(1:9,1))
        if(board[position] != "X" && board[position] != "O"){
          board <- update(board, player, position)
          cat("\n")
          cat("Computer made a move.", "\n")
          display(board)
          winner <- check_winner(board)
          player <- 1 }
        else{
          position <- as.double(sample(1:9,1)) }
        
      }
    }
  }
  
  boot_vs_p2 <- function(board,player){
    while(winner == FALSE){
      if(player == 1){
        position <- as.double(sample(1:9,1))
        if(board[position] != "X" && board[position] != "O"){
          board <- update(board, player, position)
          cat("\n")
          cat("Computer made a move.", "\n")
          display(board)
          winner <- check_winner(board)
          player <- 2
        }
        else{
          position <- as.double(sample(1:9,1))
        }
        
      }
      else if(player == 2){
        position <- check_moves(board)
        board <- update(board, player, position)
        winner <- check_winner(board)
        player <- 1 
      }
    }
  }
  
  
  p1_vs_p2 <- function(board,player){
    while(winner == FALSE){
      position <- check_moves(board)
      if(player == 1){
        board <- update(board, player, position)
        display(board)
        winner <- check_winner(board)
        player <- 2 
      }
      else if(player == 2){
        board <- update(board, player, position)
        display(board)
        winner <- check_winner(board)
        player <- 1 
      }
    }
  }
  
  cat("Welcome to tic toc toe game!","\n") 
  cat("How many players? :")
  select_mode <- readLines(con = con, n = 1)
  if(select_mode == "1"){
    check <- TRUE
    while(check){ 
      cat("Type 1 for selecting 'x', 2 for 'O' :")
      player <- readLines(con = con, n = 1)
      if (player != "1" &&
          player != "2"){
        cat("Please enter a valid selection! (1 or 2) ", "\n")}
      else{
        player <- as.double(player)
        if(player == 1){
          cat("X plays first. ", "\n")
          display(board)
          boot_vs_p1(board,player)
          check <- FALSE}
        else if(player == 2){
          cat("X plays first. ", "\n")
          player <- 1;
          boot_vs_p2(board,player)
          check <- FALSE}
      }
    }
  }
  
  else if (select_mode == "2"){
    player <- 1; cat("X plays first. ", "\n")
    display(board)
    p1_vs_p2(board,player) }
  
  else if (select_mode != "1" &&
           select_mode != "2"){
    cat("We support maximum 2 players.")}
}

tictoctoe()