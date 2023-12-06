NB. Generic draughts engine
NB. Spaces are either 0 (empty), _1 (white piece), 1 (black piece), _2 (white crowned piece), 2 (black crowned piece)

NB. coclass 'draughts'

newBoard =: 3 : 0 NB. Creates/clears the board.
   white_spaces =. ((4 10 $ ((10 $ (<'  '), <_1), 10 $ (10 $ (<_1), <'  ')))
   middle =. 2 10 $ (10 $ (<'  '), <0), 10 $ (<0), <'  ')
   black_spaces =. 4 10 $ ((10 $ (<'  '), <1), 10 $ (10 $ (<1), <'  '))
   
   board =: white_spaces, middle, black_spaces
)

NB. Utilities
NB. =====================================================

round =: +/<.(0.5&+)

getCoords =: 3 : 0 NB. Gets grid coordinates of draughts position notation

   if. 0 = 2 | (<. (y - 1) % 5) do. NB. Case even row (Origin of board is top-left)
      (<. (y - 1) % 5), (round 10 | 1 + 2*(y - 1))  

   else. NB. Case odd row.
      (<. (y - 1) % 5), (round 10 | 2*(y - 1))

   end.
)

getNotation =: 3 : 0 NB. Gets draughts position notation from coordinates
   +/@((5 0.5)&*) (((0 2)&+)`((0 1)&+)@.((0&=)@:(2&|)@:(0&{)@])) y
)

NB. Checking and Interpreting the Board
NB. =====================================================

checkCondition =: {{> (< getCoords y) { board}} NB. Gets value at notation position

getDiagonals =: 3 : 0 NB. Gets diagonals of a notation position
   
   northeast =. i. 0 0
   northwest =. i. 0 0
   southeast =. i. 0 0
   southwest =. i. 0 0

   if. 0 < 0 { getCoords y do. NB. Not in top row
      
      position =. (getCoords y) + _1 _1 NB. Northeast diagonal
      while. (10 > >./ position) *. _1 < <./ position  do.
         northeast =. northeast, 1 2 $ ((getNotation position), checkCondition getNotation position)
         position =. position - 1 1
      end.

      position =. (getCoords y) + _1 1 NB. Northwest diagonal
      while. (10 > >./ position) *. _1 < <./ position  do.
         northwest =. northwest, 1 2 $ ((getNotation position), checkCondition getNotation position)
         position =. position + _1 1
      end.
   end.

   if. 9 > 0 { getCoords y do. NB. Not in bottom row
      
      position =. (getCoords y) + 1 _1 NB. Southeast diagonal
      while. (10 > >./ position) *. _1 < <./ position  do.
         southeast =. southeast, 1 2 $ ((getNotation position), checkCondition getNotation position)
         position =. position + 1 _1
      end.

      position =. (getCoords y) + 1 1 NB. Southwest diagonal
      while. (10 > >./ position) *. _1 < <./ position  do.
         southwest =. southwest, 1 2 $ ((getNotation position), checkCondition getNotation position)
         position =. position + 1 1
      end.
   end.

   northeast; northwest; southeast; southwest
)

checkCaptures =: 4 : 0 NB. Checking a piece's captures. Optionally bans a direction using x
   directions =. (I. -. (0 { x) = i. 4 ) { i. 4 NB. (_1 bans none, 0 bans ne, 1 bans nw, 2 bans se, 3 bans sw)
   diagonals =. > directions { getDiagonals 0 { y
   targetSign =. -*1 { y

   validLoc =. -. 0 = ((0 ({"_1) 1 }. ]) >)"_1 diagonals
   if. 1 = |_1 { y do.
      validLoc =. validLoc *."_1 ((# validLoc), # 0 { validLoc) $ (1, (_1 + # 0 { validLoc) $ 0)
   end.

   blankSpace =. 0 = ((1 ({"_1) 1 }. ]) >)"_1 diagonals
   enemyPresent =. targetSign = *((1 ({"_1) _1 }. ]) >)"_1 diagonals
   allyPresent =. (-targetSign) = *((1 ({"_1) _1 }. ]) >)"_1 diagonals

   absPartialSums  =. (+/@:|)\

   validMove =. *./>(1 = absPartialSums"_1 enemyPresent); (0 = absPartialSums"_1 allyPresent); blankSpace; validLoc
   
   coords =. <"_1;(i. # diagonals) (<@(,"_1 (1 + I.)))"_1 validMove
   
   captures =. (0 {"_1 coords { diagonals)(,"_1) (3 - (0 {"_1 > coords) { directions)
   
   if. 0 { $ captures do.
      (0 { y) ,"_1 captures
   else. 
      0 $ 0
   end.
)

possibleCaptures =: 4 : 0 NB. Checks a player's possible captures, and recursively finds the longest.
   
   if. 0 = # x do.
      x =. _1 ,"_1 (1 + I. y = (*@:checkCondition)"_1 (1 + i.50))  NB. All spaces currently occupied by a player's pieces.
      x =. x ,"1 ,. checkCondition"0 (1 {"1 x)
   else. 
      x =. x ,"1 y  
   end.

   captures =. ,/"1 ((0 {"1 x) ((1 { ]),"1 checkCaptures)"0 1 (_2 {."1 x))

   if. 3 = # $ captures do.
      captures =. ,/ captures
   end.
   
   if. 1 < # $ captures do.
      
      captures =. (I. 0 ([ < (1 { ]))"_1 captures) { captures
      x =. (I. 0 ([ < (1 { ]))"_1 captures) { x

      nextLevel =. captures (((1 2 { [) ,"1 (]) (] possibleCaptures [) (|."1@:(_2 {."1 [)))"1) ,. 0 {"1 captures

      if. nextLevel do.
         nextLevel
      else.
         1 2 {"1 captures
      end.
   else. NB. Returns an empty array if no captures are found.
      0 $ 0
    end.
)

captureList =: 3 : 0 NB. Finds a player's possible captures and returns them in draughts notation.
   rawCaptures =. '' possibleCaptures y

   if. $ rawCaptures do.
      rawCaptures =. (((*/ $ rawCaptures)%(_1 { $ rawCaptures)), _1 { $ rawCaptures) ($ ,) rawCaptures

      NB. Collapses the array to 2 dimensions.
      validCaptures =. (I. ((0&<)@:(_1&{)@])"1 rawCaptures) { rawCaptures

      NB. Generates the string expression representing the capture.
      (('x' (I.@:(32&=)@:(a. i. ]))} ])@:":@:(] (]{[) I.@:((1 = +/@:((_1&{ ]) = ]))\)))"1 validCaptures return.
   end.

   0 $ 0
)

checkMoves =: 3 : 0
   diagonals =. > getDiagonals y
   piece =. checkCondition y
   if. 1 = |piece do. 
      if. piece > 0 do.
        diagonals =. 0 1 { diagonals
      else.
         diagonals =. 2 3 { diagonals
      end.

      moveSpaces =. 0 {"1 (0 {"2 (I. ((0 < 0 {"1 (0 {"2 diagonals)) *. (0 = 1 {"1 (0 {"2 diagonals)))) { diagonals)
      (((":y)&,)@:('-'&,)@:":)"0 moveSpaces return.
   end.

   if. 2 = |piece do.
      absPartialSums =. (+/@:|)\

      moveSpaces =. ((I.@:(0&<)@]) { ]) ,/ (0 {"1 ((I.@:(0&=)@:absPartialSums@:(1 {"1  ])) { (])))"2 diagonals
      (((":y)&,)@:('-'&,)@:":)"0 moveSpaces return.
   end.
   0 $ 0
)

moveList =: 3 : 0
   alliedSpaces =. 1 + I. y = *"0 ((<@:getCoords)"0 (1 + i.50)) (>"0@:{) board
   ,:"1 ((I.@:-.@:(' '&=)@:(1&{"1)@:]) { ])  ,/ checkMoves"0 alliedSpaces
)

getValidMoves =: 3 : 0
   captures =. captureList y
   if. $ captures do.
      captures
   else. 
      moveList y
   end.
)

gameState =: 3 : 0 NB. Takes the board as an argument and checks if a player has won the game.
   if. -. * +/ 0 < ((<@:getCoords)"0 (1 + i.50)) (>"0@:{) y do. _1 return. end.

   if. -. * +/ 0 > ((<@:getCoords)"0 (1 + i.50)) (>"0@:{) y do. 1 return. end.

   0
)

NB. Functions that change the board
NB. =====================================================

newBoard =: 3 : 0 NB. Creates/clears the board.
   board =: ((4 10 $ ((10 $ (<'  '), <_1), 10 $ (10 $ (<_1), <'  '))), 2 10 $ (10 $ (<'  '), <0), 10 $ (<0), <'  '), 4 10 $ ((10 $ (<'  '), <1), 10 $ (10 $ (<1), <'  '))
)

setSpace =: 4 : 0 
   board =: (<x) (<"1 y)} board
)

executeMove =: 3 : 0
   if. +/ 'x' = y do. NB. Commanded move is a capture
      points =. ". (' ' (I.@:(120&=)@:(a. i. ]))} ]) y
      smoutput points
      for_i. i. _1 + # points do.
         first =. i { points
         second =. (i + 1) { points
         difference =. (getCoords second) - getCoords first
         diagonal =. 1 {"1 (| 0 { difference) {. > (+/ 2 1 * 0 < *difference) { getDiagonals first
         jumped =. getNotation ((getCoords first) + (0 { 1 + I. 0 < | diagonal) * *difference)

         board =: (checkCondition first) setSpace (getCoords second) 
         board =: 0 setSpace (getCoords first)
         board =: 0 setSpace (getCoords jumped)
      end.
      return.
   end.

   if. +/ '-' = y do.
      divider =. 0 { I. '-' = y
      first =. ". (I. divider > i. # y) { y
      second =. ". (I. divider < i. # y) { y
   
      board =: (checkCondition first) setSpace (getCoords second)
      board =: 0 setSpace (getCoords first)
   end.

)

executePromotions =: 3 : 0 
   white_promos =. (I. 1 = checkCondition"0 (1 2 3 4 5)) { 1 2 3 4 5
   black_promos =. (I. _1 = checkCondition"0 (46 47 48 49 50)) { 46 47 48 49 50

   if. # white_promos do.
      board =: 2 setSpace (getCoords"0 white_promos)
   end.
   if. # black_promos do.
      board =: _2 setSpace (getCoords"0 black_promos)
   end.
)