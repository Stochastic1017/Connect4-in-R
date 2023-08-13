
## Remove objects from specified environment
rm(list = ls())


#####################################################
############## Begin create a 6x7 Grid ##############

create_grid = function() # creates Connect 4 grid with 6 rows and 7 columns
{
  x = rep(1:7, each = 6) # 7 - columns
  y = rep(1:6, times = 7) # 6 - rows
  z_x = seq(1,7,1) # initial row to choose which column to drop 'O' or 'X' in
  z_y = rep(0, each = 7) # points to click in order to drop 'O' or 'X'
  
  plot(x, y, 
       type = 'n', 
       xlab = '',
       xaxt = 'n',
       xlim = c(0.5, 7.5), 
       ylim = c(7.5, -1.5)) # plots points
  
  points(z_x, z_y, pch = 25, bg = 'green') # columns to choose where to drop 'O' or 'X'
  
  axis(3)                                           
  mtext(text = "x", side = 3, line = 2.5) # changes x-axis location to top
  
  segments(x0 = rep(0.5, each = 6),
           y0 = seq(0.5, 6.5, 1),
           x1 = rep(7.5, each = 6),
           y1 = seq(0.5, 6.5, 1)) # plots horizontal lines
  
  segments(x0 = seq(0.5, 7.5, 1),
           y0 = rep(0.5, each = 7),
           x1 = seq(0.5, 7.5, 1),
           y1 = rep(6.5, each = 7)) # plots vertical lines
  
  board = matrix(data = rep(x = 'E', time = (6*7)), nrow = 6, ncol = 7) # initializing an empty board
  
  return(board)
}

############## End create a 6x7 Grid ##############
###################################################


########################################################
############## Begin check for win status ##############

won = function(player, board) # checks for win status at each step
{
  # Horizontal Check
  for (i in 1:6) # loops through all rows
    {
    for (j in 1:4) # loops through all 4-columns of each row
      {
      if (board[i,j] == player && 
          board[i,j+1] == player && 
          board[i,j+2] == player && 
          board[i,j+3] == player)
        {
        return(c(TRUE, i, j, i, j+1, i, j+2, i, j+3))
        }
       }
     }
  
  # Vertical Check
  for (j in 1:7) # loops through all columns
  {
    for (i in 1:3) # loops through all 4-rows of each column
    {
      if (board[i,j] == player && 
          board[i+1,j] == player && 
          board[i+2,j] == player && 
          board[i+3,j] == player)
      {
        return(c(TRUE, i, j, i+1, j, i+2, j, i+3, j)) # if true, return coordinates
      }
    }
  }
  
  # Right diagonal Check (left - to - right)
 for (i in 1:3) # loops through first three rows (as after fourth row number of right diagonals < 4)
 {
   for (j in 1:4) # loops through all top-right element of each row
   {
     if (board[i,j] == player && 
         board[i+1,j+1] == player && 
         board[i+2,j+2] == player && 
         board[i+3,j+3] == player)
     {
       return(c(TRUE, i, j, i+1, j+1, i+2, j+2, i+3, j+3)) # if true, return coordinates
     }
   }
 }
  
  # Left Diagonal Check (right - to - left)
  for (i in 1:3) # loops through first three rows (as after fourth row number of right diagonals < 4)
  {
    for (j in 4:7) # loops through all bottom-right element of each row
    {
      if (board[i,j] == player && 
          board[i+1,j-1] == player && 
          board[i+2,j-2] == player && 
          board[i+3,j-3] == player)
      {
        return(c(TRUE, i, j, i+1, j-1, i+2, j-2, i+3, j-3)) # if true, return coordinates
      }
    }
  }
  
  return(c(FALSE, 0, 0, 0, 0, 0, 0, 0, 0)) # if no win in any direction, return false
}

############## End check for win status ##############
######################################################

##################################################
############## Begin Playing Loop ################

playing_loop = function(board) # Playing loop using clickable user inputs
{
  z_x = seq(1,7,1) # initial row to choose which column to drop 'O' or 'X' in
  z_y = rep(0, each = 7) # points to click in order to drop 'O' or 'X'
  
  chosen = c() # initializing vector of user inputs and indexes
  player = 'X' # player starts first, computer goes next
  count = 0 # count of game steps
  
  if (count == 0)
  {
    text(x = 4, y = -1, labels = "Click green triangles to choose column", col = "green", cex = 1)
  }
  
  while (count > -1)
  {
    if (player == 'X') # human playing
    {
      index = identify(x = z_x, y = z_y, n = 1, plot = F) # allows user to click column choice
      chosen = append(chosen, index) # appending index to vector of chosen inputs
      col_c = z_x[index] # column to drop 'O' or 'X' in
      
      if (index %in% chosen) # check if column already has an 'O' or 'X' in it
      {
        count = count + 1
        tot = 0
        for (ele in chosen)
        {
          if (ele == index)
          {
            tot = tot + 1
          }
        }
        if (tot > 6) # if entire column is filled to the top
        {
          count = count - 1 # reduce count by 1
          print('Invalid Move, Choose a non-empty column!')
          next # continue loop
        }
        row_c = 7 -  tot
      }
      
      board[row_c, col_c] = player
      cat(sep = "", "count = ", count, ", player = ", player, "\n") #print the current player for the debugging
      print(board)
      text(x = col_c, y = row_c, col = 'red', labels = player) 
      
      if (won(player, board)[1] == 1)
      {
        segments(y0 = c(won(player, board)[2], won(player, board)[4], won(player, board)[6]), 
                 x0 = c(won(player, board)[3], won(player, board)[5], won(player, board)[7]), 
                 y1 = c(won(player, board)[4], won(player, board)[6], won(player, board)[8]), 
                 x1 = c(won(player, board)[5], won(player, board)[7],won(player, board)[9]) , col = 'red')
        
        text(x = 4, y = 7.2, labels = paste(player, " won!"), col = "red", cex = 1.5) #announce the winner on the top
        break
      } else 
      {
        player = ifelse(test = (player == "X"), yes = "O", no = "X") #switch the players: "X" <==> "O"
      }
    } else # computer playing
    {
      index = sample(z_x, replace = TRUE, size = 1) # randomly choose a column from 1:7
      chosen = append(chosen, index) # appending index to vector of chosen inputs
      col_c = z_x[index] # column to drop 'O' or 'X' in
      
      if (index %in% chosen) # check if column already has an 'O' or 'X' in it
      {
        count = count + 1
        tot = 0
        for (ele in chosen)
        {
          if (ele == index)
          {
            tot = tot + 1
          }
        }
        if (tot > 6) # if entire column is filled to the top
        {
          count = count - 1 # reduce count by 1
          next # continue loop
        }
        row_c = 7 -  tot
      }
      
      board[row_c, col_c] = player
      cat(sep = "", "count = ", count, ", player = ", player, "\n") #print the current player for the debugging
      print(board)
      text(x = col_c, y = row_c, col = 'blue', labels = player) 
      
      if (won(player, board)[1] == 1)
      {
        segments(y0 = c(won(player, board)[2], won(player, board)[4], won(player, board)[6]), 
                 x0 = c(won(player, board)[3], won(player, board)[5], won(player, board)[7]), 
                 y1 = c(won(player, board)[4], won(player, board)[6], won(player, board)[8]), 
                 x1 = c(won(player, board)[5], won(player, board)[7],won(player, board)[9]) , col = 'blue')
        
        text(x = 4, y = 7.2, labels = paste(player, " won!"), col = "blue", cex = 1.5) #announce the winner on the top
        break
      } else 
      {
        player = ifelse(test = (player == "X"), yes = "O", no = "X") #switch the players: "X" <==> "O"
      }
    }
    
    if (count > 1) # at least both players have played once
    {
      if (length(unique(c(board))) == 2) # checks if every element in matrix is completely covered by 'O' and 'X' without win
      {
        text(x = 4, y = 7.2, labels = 'Draw!', col = "green", cex = 1.5) #announce draw
        break
      } 
    }
  }
}

############## End Playing Loop ################
################################################

play_connect_4 = function()
{
  board = create_grid()
  playing_loop(board)
}

play_connect_4()

