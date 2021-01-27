! Student Name: Miriam Snow
! Student Number: 0954174
! Student Email: msnow01@uoguelph.ca

! Program Description: 
! This program allows a user to play a game of tic tac toe against the computer
! X value is represented by a 1 in the tictac 1D array and is the Human's move
! O value is represented by a 4 in the tictac 1D array and in the Computer's move

program tictactoe
   integer, dimension(9) :: tictac  
   integer :: userMove
   logical :: occupied
   integer :: num
   logical :: over
   integer :: winner
   over = .FALSE.
   winner = 0

!initialize tictac array with all zeros
    do i = 1,9
        tictac(i) = 0
    end do
    
    write (*,"(A)") "Play Tic-Tac-Toe. Enter 1-9 to play. You are X."
    write (*,*)
    call showBoard(tictac)
    
! keep getting input from the player until the game is over
    do m = 1,9

        do 
            userMove = getMove()
            occupied = chkplay(tictac,userMove)
            if (occupied .eqv. .TRUE.) then
                exit
            else 
                write(*,"(A)") "Sorry, that space is occupied"
            end if
        end do
        
        call playTicTacToe(tictac, userMove)
        over = .FALSE.
        winner = 0
        num = chkover(tictac, over, winner)
        if (over .eqv. .TRUE.) then
            call showBoard(tictac)
            write(*,"(A)", advance="no") "Game Over! The winner is "
            if (winner == 1) then
                write(*,"(A)") "X (you) "
                exit
            else if (winner == 4) then
                write(*,"(A)") "O (computer)"
                exit
            else if (winner == 0) then
                write(*,"(A)") " nobody, it's a draw"
                exit
            end if
        end if
        
        call computerMove(tictac)
        over = .FALSE.
        winner = 0
        num = chkover(tictac, over, winner)
        if (over .eqv. .TRUE.) then
            call showBoard(tictac)
            write(*,"(A)", advance="no") "Game Over! The winner is "
            if (winner == 1) then
                write(*,"(A)") "X (you)"
                exit
            else if (winner == 4) then
                write(*,"(A)") "O (computer)"
                exit
            else if (winner == 0) then
                write(*,"(A)") " nobody, it's a draw"
                exit
            end if
        end if
        
        call showBoard(tictac)
    
    end do
       
    
    
contains

! Function: getMove() - get a human's next move. It will check to make sure it is a valid integer between 1 and 9
! Parameters: no parameters
! Return: valid integer between 1 and 9
integer function getMove()
    integer :: move, error
    
    do 
        write (*,"(A)") "Your move?"
        read (*,"(i10)",iostat=error) move
        
        if (move > 9) then 
            write (*,"(A)") "Please input an integer from 1 to 9"
        else if (move < 1) then
            write (*,"(A)") "Please input an integer from 1 to 9"
        else if (error /= 0) then
            write (*,"(A)") "Please input an integer from 1 to 9"
        else 
            exit
        end if
        
    end do
         
    getMove = move
    
end function getMove

! Function: chkplay() - check to see if the space of the next human's move is empty or not
! Parameters: array to check and index of array value to check
! Return: True if space is okay to occupy, false if space is already occupied
logical function chkplay(array, num)
    integer, dimension(9), intent(in out) :: array
    integer, intent (in out) :: num
    if (array(num) == 0) then
        chkplay = .TRUE.
    else 
        chkplay = .FALSE.
    end if
end function chkplay

! Function: same() - checks to see if numbers are the same. 
! Parameters: 3 numbers to check equality of
! Return: True if they are the same, false otherwise
logical function same(num1,num2,num3)
    integer, intent (in) :: num1
    integer, intent (in) :: num2
    integer, intent (in) :: num3
    integer :: oneAndTwo
    integer :: twoAndThree
    integer :: oneAndThree
    oneAndTwo = 0
    twoAndThree = 0
    oneAndThree = 0
    
    if (num1 == num2) then
        oneAndTwo = 1
    end if
    if (num1 == num3) then
        oneAndThree = 1
    end if
    if (num2 == num3) then
        twoAndThree = 1
    end if
    
    if ((twoAndThree == 1) .AND. (oneAndTwo == 1) .AND. (oneAndThree == 1) .AND. (num1 /= 0)) then
        same = .TRUE.
    else 
        same = .FALSE.
    end if
end function same

! Function: chkover - checks to see if there is a winner and/or if the game is over
! Parameter: array of tic tac toe board game, over (logical variable to tell if the game is over or not), winner (integer 0, 1 or 2 representing draw, X or 0)
! Return: 0 to end the function
integer function chkover(tictac, over, winner)
    integer, dimension(9), intent(in out) :: tictac
    logical, intent (in out) :: over
    integer, intent (in out) :: winner
    logical :: sameVal
    integer :: flag
    sameVal = .FALSE.
    over = .FALSE.
    winner = 0
    
! check rows  
    sameVal = same(tictac(1), tictac(2), tictac(3))
    if (sameVal .eqv. .TRUE.) then
        winner = tictac(1)
        over = .TRUE.
        chkover = 0
        return
    end if
    
    sameVal = same(tictac(4), tictac(5), tictac(6))
    if (sameVal .eqv. .TRUE.) then
        winner = tictac(4)
        over = .TRUE.
        chkover = 0
        return
    end if
    
    sameVal = same(tictac(7), tictac(8), tictac(9))
    if (sameVal .eqv. .TRUE.) then
        winner = tictac(7)
        over = .TRUE.
        chkover = 0
        return
    end if
    

! check columns    
    sameVal = same(tictac(1), tictac(4), tictac(7))
    if (sameVal .eqv. .TRUE.) then
        winner = tictac(1)
        over = .TRUE.
        chkover = 0
        return
    end if
    
    sameVal = same(tictac(2), tictac(5), tictac(8))
    if (sameVal .eqv. .TRUE.) then
        winner = tictac(2)
        over = .TRUE.
        chkover = 0
        return
    end if
    
    sameVal = same(tictac(3), tictac(6), tictac(9))
    if (sameVal .eqv. .TRUE.) then
        winner = tictac(3)
        over = .TRUE.
        chkover = 0
        return
    end if
    
!check diagonal
    sameVal = same(tictac(3),tictac(5),tictac(7))
    if (sameVal .eqv. .TRUE.) then 
        winner = tictac(3)
        over = .TRUE.
        chkover = 0
        return
    end if
    
    
    sameVal = same(tictac(1),tictac(5),tictac(9))
    if (sameVal .eqv. .TRUE.) then 
        winner = tictac(1)
        over = .TRUE.
        chkover = 0
        return
    end if
    
!check if they are all full, meaning the game is over but not winner
    flag = 0
    do i = 1, 9
        if (tictac(i) == 0) then
            flag = 1
        end if
    end do
    
    if (flag == 0) then
        over = .TRUE.
        winner = 0
        chkover = 0
        return
    end if
    
chkover = 0
   
end function chkover

  
   
end program tictactoe

! Subroutine: playTicTacToe() - plays the human's move 
! Parameter: array of tictactoe board game, human's move (integer value)
subroutine playTicTacToe(array, move)
   
    integer, dimension(9), intent(in out) :: array
    integer, intent (in out) :: move
    array(move) = 1

end subroutine playTicTacToe

! Subroutine: showBoard() - displays the tic tac toe board with Xs and Os in corresponding spots
! Parameter: array of tictactoe board game
subroutine showBoard(n)
   integer :: flag, i
   integer, dimension(9), intent(in out) :: n
   
   flag = 0;
   do i = 1,9
      if (n(i) /= 0) then
         flag = 1
      end if
   end do
   
   if (flag == 0) then
      write(*, "(A)") " 1 | 2 | 3 "
      write(*, "(A)") "---+---+---"
      write(*, "(A)") " 4 | 5 | 6 "
      write(*, "(A)") "---+---+---"
      write(*, "(A)") " 7 | 8 | 9 "
   else 
       if (n(1) == 1) then
           write(*,"(A)", advance="no") " X |"
       else if (n(1) == 4) then
          write(*,"(A)", advance="no") " O |"
       else 
          write(*,"(A)", advance="no") "   |"
       end if
       
       if (n(2) == 1) then
           write(*,"(A)", advance="no") " X |"
       else if (n(2) == 4) then
          write(*,"(A)", advance="no") " O |"
       else 
          write(*,"(A)", advance="no") "   |"
       end if
       
       if (n(3) == 1) then
           write(*,"(A)") " X"
       else if (n(3) == 4) then
          write(*,"(A)") " O"
       else 
          write(*,*) " "
       end if
       
       write(*,"(A)") "---+---+---"
       
          if (n(4) == 1) then
           write(*,"(A)", advance="no") " X |"
       else if (n(4) == 4) then
          write(*,"(A)", advance="no") " O |"
       else 
          write(*,"(A)", advance="no") "   |"
       end if
       
       if (n(5) == 1) then
           write(*,"(A)", advance="no") " X |"
       else if (n(5) == 4) then
          write(*,"(A)", advance="no") " O |"
       else 
          write(*,"(A)", advance="no") "   |"
       end if
       
       if (n(6) == 1) then
           write(*,"(A)") " X"
       else if (n(6) == 4) then
          write(*,"(A)") " O"
       else 
          write(*,*) " "
       end if
       
       write(*,"(A)") "---+---+---"
       
       if (n(7) == 1) then
           write(*,"(A)", advance="no") " X |"
       else if (n(7) == 4) then
          write(*,"(A)", advance="no") " O |"
       else 
          write(*,"(A)", advance="no") "   |"
       end if
       
       if (n(8) == 1) then
           write(*,"(A)", advance="no") " X |"
       else if (n(8) == 4) then
          write(*,"(A)", advance="no") " O |"
       else 
          write(*,"(A)", advance="no") "   |"
       end if
       
       if (n(9) == 1) then
           write(*,"(A)") " X"
       else if (n(9) == 4) then
          write(*,"(A)") " O"
       else 
          write(*,*) " "
       end if
   end if
   write (*,*)
end subroutine showBoard

! Subroutine: computerMove() - plays the computers move based on calculated algorithm
! Parameter: array of tictactoe board
subroutine computerMove(n)
    integer, dimension(9), intent(in out) :: n

! check for two Os in a row, column or diagonal
!case 1
    if ((n(1) + n(2) + n(3)) == 8) then
        if (n(1) == 0) then
            n(1) = 4
        else if (n(2) == 0) then
            n(2) = 4
        else if (n(3) == 0) then 
            n(3) = 4
        end if
! case 2
    else if ((n(4) + n(5) + n(6)) == 8) then
        if (n(4) == 0) then
            n(4) = 4
        else if (n(5) == 0) then
            n(5) = 4
        else if (n(6) == 0) then 
            n(6) = 4
        end if
! case 3
    else if ((n(7) + n(8) + n(9)) == 8) then
        if (n(7) == 0) then
            n(7) = 4
        else if (n(8) == 0) then
            n(8) = 4
        else if (n(9) == 0) then 
            n(9) = 4
        end if
! case 4
    else if ((n(1) + n(4) + n(7)) == 8) then
        if (n(1) == 0) then
            n(1) = 4
        else if (n(4) == 0) then
            n(4) = 4
        else if (n(7) == 0) then 
            n(7) = 4
        end if
! case 5
    else if ((n(5) + n(2) + n(8)) == 8) then
        if (n(5) == 0) then
            n(5) = 4
        else if (n(2) == 0) then
            n(2) = 4
        else if (n(8) == 0) then 
            n(8) = 4
        end if
! case 6
    else if ((n(6) + n(9) + n(3)) == 8) then
        if (n(6) == 0) then
            n(6) = 4
        else if (n(9) == 0) then
            n(9) = 4
        else if (n(3) == 0) then 
            n(3) = 4
        end if
! case 7
    else if ((n(5) + n(7) + n(3)) == 8) then
        if (n(5) == 0) then
            n(5) = 4
        else if (n(7) == 0) then
            n(7) = 4
        else if (n(3) == 0) then 
            n(3) = 4
        end if
! case 8
    else if ((n(1) + n(5) + n(9)) == 8) then    
        if (n(1) == 0) then
            n(1) = 4
        else if (n(5) == 0) then
            n(5) = 4
        else if (n(9) == 0) then 
            n(9) = 4
        end if
!check for two Xs to block
! case 1
    else if ((n(1) + n(2) + n(3)) == 2) then
        if (n(1) == 0) then
            n(1) = 4
        else if (n(2) == 0) then
            n(2) = 4
        else if (n(3) == 0) then 
            n(3) = 4
        end if
! case 2
    else if ((n(4) + n(5) + n(6)) == 2) then
        if (n(4) == 0) then
            n(4) = 4
        else if (n(5) == 0) then
            n(5) = 4
        else if (n(6) == 0) then 
            n(6) = 4
        end if
! case 3
    else if ((n(7) + n(8) + n(9)) == 2) then
        if (n(7) == 0) then
            n(7) = 4
        else if (n(8) == 0) then
            n(8) = 4
        else if (n(9) == 0) then 
            n(9) = 4
        end if
! case 4
    else if ((n(1) + n(4) + n(7)) == 2) then
        if (n(1) == 0) then
            n(1) = 4
        else if (n(4) == 0) then
            n(4) = 4
        else if (n(7) == 0) then 
            n(7) = 4
        end if
! case 5    
    else if ((n(5) + n(2) + n(8)) == 2) then
        if (n(5) == 0) then
            n(5) = 4
        else if (n(2) == 0) then
            n(2) = 4
        else if (n(8) == 0) then 
            n(8) = 4
        end if
! case 6
    else if ((n(6) + n(9) + n(3)) == 2) then
        if (n(6) == 0) then
            n(6) = 4
        else if (n(9) == 0) then
            n(9) = 4
        else if (n(3) == 0) then 
            n(3) = 4
        end if
! case 7
    else if ((n(5) + n(7) + n(3)) == 2) then
        if (n(5) == 0) then
            n(5) = 4
        else if (n(7) == 0) then
            n(7) = 4
        else if (n(3) == 0) then 
            n(3) = 4
        end if
! case 8
    else if ((n(1) + n(5) + n(9)) == 2) then
        if (n(1) == 0) then
            n(1) = 4
        else if (n(5) == 0) then
            n(5) = 4
        else if (n(9) == 0) then 
            n(9) = 4
        end if    
! if there is nowhere to win or nowhere to block, place the move in a random, open spot
    else 
        do i=1,9
            if (n(i) == 0) then
                n(i) = 4
                exit
            end if
        end do
    end if
    
    
end subroutine computerMove
