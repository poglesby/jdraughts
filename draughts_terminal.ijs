NB. Console interface for Draughts Game
NB. Should work in all J frontends

DraughtsCon_z_ =: conew&'draughtsui'

require 'games/draughts/draughts_rules'
coclass 'draughtsui'
coinsert 'draughts'

NB. Variables
NB. =====================================================

player =: 1

NB. Methods
NB. =====================================================

create =: 3 : 0
    smoutput Instructions
    newBoard ''
    history =: 0 $ 0
    update_turn player
    game_log =: 0 5 $ 0
    smoutput board
)

destroy =: codestroy
quit =: destroy

move =: 3 : 0
    current_move =. y
    if. (# y) < # 0 { 0 { valid_moves do.
        current_move =. current_move, (((# 0 { 0 { valid_moves) - # y) $ ' ')
    end.

    if. +/ */"1 current_move ="1 valid_moves do.
        executeMove current_move
        executePromotions ''
        player =: -player
        update_turn player
        game_log =: game_log, ,: current_move
        smoutput board
    else.
        smoutput Illegal_Move
    end.
)

update_turn =: 3 : 0
    end =. gameState board
    if. end do.
        smoutput (> (end = _1) { 'Black';'White'), ' won the game! Congratulations!'
        smoutput Victory
        codestroy ''
    else.
        history =: history, < board
        valid_moves =: getValidMoves y
        smoutput 'It''s ', (> (player = _1) { 'Black';'White'), '''s turn.'
    end.
)

undo =: 3 : 0 
    board =: > (- y + 1) { history
    history =: (- y + 2) }. history
    game_log =: (-y) }. game_log
    player =: player * _1 + 2 * 2 = y +. 2
    update_turn player
    smoutput board
)

log =: 3 : 0
    full_log =. game_log(],"1[) '.  '(([)(],"1[)(":"0@:(1&+)@i.@#@])) game_log
    if. # y do.
        smoutput (-y) {. full_log
    else.
        smoutput full_log
    end.
)


NB. Text Nouns
NB. =====================================================

Instructions =: 0 : 0 
    === International Draughts ===
    
    Goal: 
    Remove all of the opponent's pieces from the board.

    Rules:
    Pieces move and capture diagonally.
    Black pieces move first and are represented by positive numbers.
    White pieces are represented by negative numbers.
    Blank spaces are represented as 0.
    Pieces are represented as magnitude 1 until crowned.
    Crowned pieces are magnitude 2.
    Captures are forced, so even if a move is possible it may not be valid in certain positions.
    For a more complete explanation, view
    https://en.wikipedia.org/wiki/International_draughts#Rules

    How to play:
     - The left top space is 1, and the bottom right space is 50
     - Only the spaces occupied by numbers are counted
     - Moves are formatted as 
        '<current space>-<end of move>'
        ex. '32-28'
     - Captures are formatted as
        '<current space>x<end of capture>'
        ex. '28x17'
     - When chained captures are possible, the additional steps are added in order
        ex. '28x17x26'
     - To move, use the command
        move__brd <move in string format> 
     - To undo a move, use the command
        undo__brd <number of moves to undo>
     - To see a log of the moves made throughout the game, use the command
        log__brd ''
       or, to see only a certain number of recent moves, use
        log__brd <number>
     - To quit the game, use the command 
        quit__brd ''
     - Start a new game using the command
        brd =: DraughtsCon ''
    
)

Illegal_Move =: 0 : 0
    That move is invalid in this position.
    If you would like a list of valid moves, please check the list 
        valid_moves 
)

Victory =: 0 : 0
    To play another game, use 
        DraughtsCon '' 
)

NB. Auto-run UI
NB. =====================================================

cocurrent 'base'
brd =: DraughtsCon ''