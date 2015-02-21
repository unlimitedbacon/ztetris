; If you make any changes to the source, please tell me what and why.
; And you are NOT allowed to distribute a modified source, nor the
; compiled version of it. Any changes should be made for personal use only.
; original by: Jimmy Mardell <mja@algonet.se>

#include "kernel.inc"
    .db "KEXC"
    .db KEXC_ENTRY_POINT
    .dw start
    .db KEXC_STACK_SIZE
    .dw 20
    .db KEXC_NAME
    .dw name
    .db KEXC_HEADER_END
name:
    .db "ZTetris 1.1",0

APD_BUF   = $86EC

cBitOfs   .equ 0       ; WORD
cBit      .equ 2       ; 4 BYTE
cXY       .equ 6       ; WORD
cB        .equ 8       ; 24 BYTE
cRot      .equ 32      ; BYTE
flags     .equ 33      ; BYTE  (0 - Quit, 1 - Update)
newXY     .equ 34      ; WORD
newRot    .equ 36      ; BYTE
counter   .equ 37      ; BYTE
next      .equ 38      ; BYTE
linesflag .equ 39      ; BYTE
score     .equ 40      ; WORD
scoreU    .equ 42      ; BYTE
lines     .equ 43      ; BYTE
level     .equ 44      ; BYTE
string    .equ 45      ; 6 BYTE
place     .equ 51      ; BYTE
cNBitOfs  .equ 52      ; WORD
players   .equ 54      ; BYTE
lcounter  .equ 55      ; WORD
declines  .equ 57      ; BYTE
stlevel   .equ 58      ; BYTE
lastbar   .equ 59      ; BYTE
hsflag    .equ 60      ; BYTE
scrflag   .equ 61      ; BYTE
gap       .equ 62      ; BYTE
sbyte     .equ 63      ; BYTE
sthigh    .equ 64      ; BYTE
high      .equ 65      ; BYTE
linkcnt   .equ 66      ; BYTE
board     .equ 67      ; 40 BYTE
;107 Bytes total

start:
    ld hl,Resume          
    ld a,(hl)
; Knightos TODO:
; Disable the TIOS style resume code (storing persistent variables in the program itself)
; and make it instead simply pause and switch back to castle.
; Perhaps add option to store savegame to flash.
    or a                ; Check if the game should resume
    jr z,ReProgStart
    ld (hl),0           ; Clear that flag so it doesn't resume next time
    inc hl
    ld de,cBitOfs
    ld bc,67
    ldir                ; Copy variables
    ld de,board
    ld bc,40
    ldir
    call ShowLayout
    call ShowInfo
    call ShowWell
    call ShowCurB
    ld de,$1403
    ld hl,cB+16
    call ShowB          ; This will show the next bit
    jp MainLoop

PutDigit:               ; Puts digit A on the correct place
    push af             ; Used when choosing ProgStart level
    ld l,2
    cp 5
    jr c,FirstRow
    inc l
    sub 5
FirstRow:
    add a,a
    inc a
    ld h,a
    ld (currow),hl
    pop af
    push af
    add a,48
    bcall(_putc)        ; display # in a
    pop af
    ret

ReProgStart:
    call ShowFrame      ; Show title
    ld de,$1010
    call FastVputs      ; "Choose player mode"
    ld de,$0404
    call FastPuts       ; "1 player"
    ld de,$0405
    call FastPuts       ; "2 players"
    xor a
    ld (stlevel),a      ; When ProgStarting a new game, stLevel and stHigh
    ld (sthigh),a       ; will be reset
    inc a
    ld (players),a      ; Default option, 1 player
ChoosePlayers:
    ld a,(players)
    ld h,$03
    push af
    add a,3
    ld l,a
    ld a,5
    ld (currow),hl
    bcall(_putc)        ; Put the small arrow
    pop af
    ld h,$03
    sub 6
    neg
    ld l,a
    ld a,32
    ld (currow),hl
    bcall(_putc)        ; And remove it from the other position
WKCP:
    bcall(_getcsc)
    ld hl,players
    cp $0f
    jp z,Quit
    cp $09
    jr z,LevelChoose
    cp $04
    jr z,ChangePlayers
    cp $01
    jr z,ChangePlayers
    jr WKCP
ChangePlayers:
    ld a,(hl)
    xor 3               ; This will turn 1 -> 2 and 2 -> 1
    ld (hl),a
    jr ChoosePlayers
OnePlayer:
    ld (hl),1
    jr LevelChoose
TwoPlayers:
    ld (hl),2
LevelChoose:
    call ShowFrame      ; I know I used these 2x, but that's to stop the on screen trash.
    ld a,(stlevel)
    ld (level),a        ; The cursor will ProgStart at the last played level
    ld a,(sthigh)
    ld (high),a         ; And with the high
NewDigit:
    call FixIt
    ld de,$0C02
    ld hl,HighTxt
    call FastPuts       ; "High"
    ld a,(players)
    dec a               ; Check if the hiscore should be shown
    jr nz,Show2PlayOpt  ; or two player options
    ld hl,Hiscore
    ld a,$23
    ld b,3
NewPos:
    ld e,$12
    ld d,a
    add a,6
    push af
    push bc
    call FastVputs      ; Show name
    ld a,$46
    ld (pencol),a
    ld b,5
    push hl
    call LD_HL_MHL         ; Get that persons score

    call DM_HL_DECI3      ; And show it
    pop hl
    inc hl
    inc hl            ; HL -> next hiscore table entry
    pop bc
    pop af
    djnz NewPos
    jr WaitKey
Show2PlayOpt:
    ld de,$0005
    ld hl,SLTxt
    call FastPuts           ; "Send 2-4 lines"
    push de
    ld de,$0006
    ld hl,InfoText+12
    call FastPuts       ; "Lines "
    set 3,(iy+5)
    ld a,76
    call FastPutc       ; Invert the 'S'
    pop de
    ld a,83
    call FastPutc       ; And the 'L'
    xor a
    ld (declines),a       ; Clear the flags to the two option above
    ld (scrflag),a
WaitKey:
    res 3,(iy+5)
    call FixIt
    ld a,(level)
    set 3,(iy+5)
    call PutDigit     ; Invert the ProgStarting level digit
    res 3,(iy+5)
    ld a,(high)
    ld de,$0E03
    add a,48
    call FastPutc       ; And show the High
    ld a,(players)
    dec a               ; If two players, the two players options
    jr z,GetKey           ; should be shown as well
    ld de,$0606
    ld hl,ScrambleTxt
    ld a,(scrflag)
    or a
    jr z,ShowScrFlag
    push de
    ld de,12
    add hl,de
    pop de
ShowScrFlag:
    call FastPuts           ; Show "scrambled" or "unscrambled"
GetKey:
    bcall(_getcsc)
    or a
    jr z,GetKey
    cp $0f
    jp z,Quit
    cp $09
    jp z,ProgStartGame
    cp $32
    jr z,DecHigh
    cp $31
    jr z,IncHigh
    cp $15               ;Right Parentheses
    jr z,ChangeScrFlag
    cp $2b
    jr nz,CheckLevChg
    ld a,(players)
    dec a
    jr z,GetKey
    ld a,(declines)
    xor 1               ; Change the declines flag (1-3 or 2-4)
    ld (declines),a
    add a,a
    add a,a
    ld de,$0505
    ld hl,NLTxt
    push de
    ld d,0
    ld e,a
    add hl,de
    pop de
    call FastPuts           ; Update it on the screen
ToWaitKey:           ; This label is to avoid JPs below (saves a few bytes)
    jr WaitKey
CheckLevChg:
    dec a
    jr z,ChangeRow
    dec a
    jr z,LevLeft
    dec a
    jr z,LevRight
    dec a
    jr nz,ToWaitKey
    jr ChangeRow

FixIt:
    ld de,$0002
    ld hl,PixelFixer
    call FastPuts
    inc de
FastPuts:
    ld (currow),de
    bcall(_puts)
    ret

DecHigh:
    ld a,(high)
    or a
    jr z,GetKey           ; Don't decrease if high is 0
    dec a
    ld (high),a
    jr ToWaitKey

IncHigh:
    ld a,(high)
    cp 5
    jr z,GetKey           ; Don't increase if high is 5
    inc a
    ld (high),a
    jr ToWaitKey

ChangeRow:
    ld a,(level)
    add a,5           ; Changing row is like adding with 5, mod 10
ChkLevEdges:
    daa               ; Modulo 10 (sort of)
    and $0F
SetLevel:
    ld b,a
    ld a,(level)
    call PutDigit        ; Remove the inverted digit
    ld a,b
    ld (level),a           ; And set the new level
    jr ToWaitKey
LevLeft:
    ld a,(level)
    dec a
    jr ChkLevEdges
LevRight:
    ld a,(level)
    inc a
    jr ChkLevEdges

ChangeScrFlag:
    ld hl,scrflag
    ld a,(hl)
    xor 1
    ld (hl),a
    jr ToWaitKey

ProgStartGame:
    ld a,(level)
    ld (stlevel),a        ; Copy the selected level and high so they will
    ld a,(high)           ; be default next time
    ld (sthigh),a
    ld a,(players)
    dec a
    jr z,NoWait
    xor a
    ld (lastbar),a

    call ShowFrame
    ld de,$0304
    ld hl,WaitTxt
    call FastPuts           ; "* WAITING *"

    call ReceiveByte
    or a
    jr nz,NoWait           ; If byte gotten, the other calc was waiting
    ld a,1
    ld (hsflag),a           ; This will allow the user to cancel with EXIT
    ld a,$AA
    call SendByte        ; Else wait until the other calc responds

NoWait:
    xor a
    ld (hsflag),a
    ld (sbyte),a           ; This is a linkbuffer byte
    bcall(_clrlcdf)
    call RandP           ; Randomize the first piece

    ld hl,$FFFF
    ld (board+2),hl
    ld hl,board+4
    ld b,18
InitRow:           ; Setting up the border aroudn the well
    ld (hl),%00000111
    inc hl
    ld (hl),%11100000
    inc hl
    djnz InitRow

    call ShowLayout
    ld hl,linkcnt           ; This counter decrease every frame. When 0,
    ld (hl),10           ; check link port. If too often check, it slows down
    ld a,(players)
    dec a
    ld a,0            ; Can't use 'xor a' here! It would affect the Z flag
    call nz,ShowBar      ; Show the bar at height 0
ResetVars:
    xor a
    ld (flags),a           ; Clear a lot of vars
    ld (cBit),a
    ld hl,0
    ld (scoreU),hl
    ld (score),hl         ; Clears lines as well
    call NewB           ; Prepares a new piece
    ld a,(high)
    or a
    jr z,MainLoop
    ld hl,scrflag           ; If starting with trash lines, they should
    ld b,(hl)           ; always be scrambled even though the option
    push bc           ; unscrambled is set
    push hl
    ld (hl),1           ; So temporary set it to scrambled lines
    add a,a           ; No trash lines = high*2
    call AddLines        ; Create trash lines
    pop hl
    pop bc
    ld (hl),b           ; And reset the scrflag

MainLoop:           ; The main loop
    ld hl,LevelCnts
    ld a,(level)
    ld d,0
    ld e,a
    add hl,de
    ld a,(hl)           ; A = the delay time of the current level
    ld (counter),a
DelayLoop:
    ld hl,flags
    res 1,(hl)           ; Clear the update flag
    
    bcall(_getcsc)
    cp $0f
    jp z,AbortGame
    cp $37
    jp z,Pause
    cp $36
    jp z,Rotate
    cp $30
    jp z,RotateBack
    cp $38
    jp z,TeacherKey
    cp $28
    jp z,Drop
    dec a
    jr z,MoveDown
    dec a
    jr z,MoveLeft
    dec a
    jr z,MoveRight
    dec a
    jr z,Rotate

Wait:
    ld hl,flags
    bit 0,(hl)           ; Check if the player became gameover this frame
    jp nz,GameOver
    bit 1,(hl)           ; Check if anything happened (movements)
    call nz,Update        ; If so, update that

    ld bc,3200
dwait:
    dec bc
    ld a,b
    or c
    jr nz,dwait
    call ionFastCopy
    
    ld hl,linkcnt
    dec (hl)
    call z,GetLinkInfo    ; If the link counter reaches zero, check link port
    ld hl,counter
    dec (hl)           ; Decrease the counter
    jr nz,DelayLoop       ; If not zero, check for keys again
    jr FallDown
MoveDown:
    ld hl,scoreU
    inc (hl)           ; When DOWN is pressed, increase the score
FallDown:
    call GetLinkInfo     ; Before moving down, always check linkport
    ld hl,newXY
    dec (hl)           ; Decrease the y coordinate
    call Update           ; Check if possible
    jp z,MainLoop        ; If so, repeat mainloop
BotReached:
    ld hl,cB           ; Else the bottom is reached
    ld de,(cXY)
    ld b,4
StoreB:            ; Store the piece in the well
    push hl
    call LD_HL_MHL
    add hl,de
    call PutCoord
    pop hl
    inc hl
    inc hl
    djnz StoreB
    ld a,(players)
    dec a
    call nz,CheckBar     ; If two player, check your highest line
    call NewB           ; Last, randomize a new piece
    jp MainLoop

MoveRight:
    ld hl,newXY+1
    inc (hl)           ; Increase X coordinate
    jr SetUpdateFlag      ; Set update flag

MoveLeft:
    ld hl,newXY+1
    dec (hl)           ; Decrease left coordinate
SetUpdateFlag:
    ld hl,flags
    set 1,(hl)           ; Set update flag
    jr Wait

Rotate:
    ld hl,newRot
    inc (hl)
    jr SetUpdateFlag

RotateBack:
    ld hl,newRot
    dec (hl)
    jr SetUpdateFlag

Drop:
    ld hl,scoreU           ; When dropping, increase score with the
    inc (hl)           ; number of fallen steps
    ld hl,newXY
    dec (hl)           ; Decrease Y coordinate
    call Update           ; Update it on screen
    jr z,Drop           ; If OK, move down again
    call GetLinkInfo     ; Get link info
    jr BotReached           ; Bottom reached, store piece.

TeacherKey:
    ld a,(players)
    dec a
    jp nz,Wait           ; If two players, teacher key not allowed
    inc a
    ld de,Resume
    ld (de),a           ; Set the resume flag
    inc de
    ld hl,cBitOfs
    ld bc,67           ; Copy all variables
    ldir
    ld hl,board
    ld bc,40
    ldir               ; And the well
    jp Quit

Pause:
    ld a,(players)
    dec a
    jp nz,Wait           ; Pause not allowed in two player game
    call ShowFrame
    ld de,$0404
    ld hl,PauseTxt
    call FastPuts           ; "* PAUSE *"
    ld b,24
PLoop2:
    ld (hl),$FF
PLoop:
    ei
    halt
    bcall(_getcsc)
    cp 9
    jp z,Wait
    dec (hl)
    xor a
    or (hl)     ;Make  HL activate Z-Flag
    jr nz,PLoop
    ld a,b
    dec b
    or a
    jr nz,PLoop2
PsuedoAPD:
    DI            ; disable interrupts
    LD A,$01
    OUT ($03),A    ;turn off screen
    EX AF,AF'
    EXX
    EI            ; enable interrupts
    jr Pause 

ShowBar:           ; Show the bar
    ld (lastbar),a
    ld hl,GRAPH_MEM + (63 * 12) + 11
    ld de,-12
    ld b,64
    add a,a
    add a,a
SB_Rep:
    ld c,a
    ld a,$0F
    and (hl)
    ld (hl),a
    ld a,c
    or a
    jr z,ClearBar
    ld a,$60
    or (hl)
    ld (hl),a
    dec c
ClearBar:
    ld a,c
    add hl,de
    djnz SB_Rep
    ret

AddLines:           ; Add A lines, scrambled or unscrambled
    ld b,a
    push bc
    ld c,b
    ld b,0
    sla c
    ld de,board+34
    ld h,d
    ld l,e
    sbc hl,bc
    ld a,32
    sub c
    ld b,0
    ld c,a
    lddr
    pop bc
    ld a,b
    push af
    ld a,10
    call PRandom
    ld (gap),a
    ld hl,board+4
AddTrashRow:
    push bc
    ld de,$FFFF
    push hl
    ld b,5
Holes:
    push bc
    ld hl,$FFFB
    ld a,(scrflag)
    or a
    jr nz,RandGap
    ld a,(gap)
    jr PutGap
RandGap:
    ld a,10
    call PRandom
PutGap:
    ld b,a
    inc b
RotWord:
    .db $CB,$35           ; SLL L - an undocumented Z80 instruction
    rl h
    djnz RotWord
    pop bc
    ld a,d
    and h
    ld d,a
    ld a,e
    and l
    ld e,a
    djnz Holes
    pop hl
    ld (hl),e
    inc hl
    ld (hl),d
    inc hl
    pop bc
    djnz AddTrashRow
    ld hl,newXY
    pop af
    add a,(hl)
    cp $10
    jr c,TopNotReached
    ld a,$10
TopNotReached:
    ld (hl),a
    call Update
    push af
    call ShowWell
    call ShowCurB
    pop af
    jp nz,GameOver
    ret

Update:            ; Update the piece on the screen.
    call TestNewB        ; Return NZ if not possible move
    jr nz,Sync
    call EraseCurB
    ld hl,(newXY)
    ld (cXY),hl
    ld a,(newRot)
    ld (cRot),a
    ld b,0
    call Uncrunch
    call ShowCurB
    xor a
Sync:
    push af
    ld hl,(cXY)
    ld (newXY),hl
    ld a,(cRot)
    ld (newRot),a
    pop af
    ret

GetLinkInfo:           ; Fins out what happens to the opponent
    ld hl,linkcnt
    ld (hl),10           ; Reset the link counter
    ld a,(players)
    dec a
    ret z               ; If one player, leave this routine
    call ReceiveByte     ; Get a byte
    or a
    jr z,CheckSByte       ; If no byte received, check if a byte should be sent
    ld b,a
    and $0F
    ld c,a
    ld a,b
    srl a
    srl a
    srl a
    srl a
    cp $0F
    jr z,PenaltyRows
    cp $0C
    jp z,YouWinP
    cp $0D
    jr z,UpdateBar
    cp $0E
    ret nz
    ld c,16
UpdateBar:
    ld a,c
    jp ShowBar
PenaltyRows:
    ld a,c
    inc a
    jp AddLines

CheckSByte:
    ld a,(sbyte)           ; Check if byte in send buffer
    or a
    call nz,SendByte     ; If so, send it
    ret

AbortGame:
    ld a,(players)
    dec a
    jp z,CheckHiscore    ; If one player abort, check hiscore table

GameOver:
    ld a,(players)
    dec a
    jr z,FlashGameOver    ; If a two player game, send a byte telling
    ld a,$C0           ; that you lost
    ld b,3
SendWinByte:
    push bc
    call SendByte
    call ReceiveByte     ; This is for clearing up stuff (if both sent
    ld a,(sbyte)           ; at the same time)
    or a
    pop bc
    jr z,FlashGameOver
    djnz SendWinByte
FlashGameOver:
    bcall(_getcsc)
    cp $09
    jr z,CheckHiscore
    cp $0f
    jr z,CheckHiscore
    ld a,(iy+5)
    xor 8
    ld (iy+5),a
    call ionFastCopy
    ld de,$0303
    ld hl,GameOverText
    call FastPuts
    ei
    ld b,20
FlashWait:
    halt
    djnz FlashWait
    jr FlashGameOver

YouWinP:
    pop hl
YouWin:
    bcall(_getcsc)
    cp $09
    jr z,CheckHiscore
    cp $0f
    jr z,CheckHiscore
    ld a,(iy+5)
    xor 8
    ld (iy+5),a
    call ionFastCopy
    ld de,$0403
    ld hl,WinTxt
    call FastPuts
    ei
    ld b,20
WFlashWait:
    halt
    djnz WFlashWait
    jr YouWin

CheckHiscore:
    call Quitter
    ld a,(players)
    dec a
    jp nz,LevelChoose    ; No hiscore when two players
    ld hl,Hiscore+14     ; HL -> hiscore
    ld de,(score)          ; DE = your score
    ld b,3
CheckP:
    push hl
    call LD_HL_MHL
    bcall(_cphlde)
    pop hl
    jr c,ScoreGreater
    push de
    ld de,16
    add hl,de           ; HL -> next score in hiscore table
    pop de
    djnz CheckP
    jp LevelChoose       ; Not in hiscore table
ScoreGreater:
    ld a,4
    sub b
    ld (place),a
    cp 3               ; If last place, no moves in hiscore table
    jr z,MoveDone
    ld hl,Hiscore+19
MoveAgain:           ; Else move the other people down
    ld d,h
    ld e,l
    ld bc,16
    add hl,bc
    ex de,hl
    ld bc,13
    ldir
    ld hl,Hiscore+3
    dec a
    jr z,MoveAgain
MoveDone:
    ld a,(place)           ; Find out where to enter your name
    ld hl,Hiscore+3
    ld de,16
    dec a
    jr z,EnterName
    add hl,de
    dec a
    jr z,EnterName
    add hl,de
EnterName:
    push hl
    ld b,10
RepClear:
    ld (hl),32           ; Clear the previous name
    inc hl
    djnz RepClear
    inc hl
    ld de,(score)
    ld (hl),e           ; Put your score in the table
    inc hl
    ld (hl),d
    call ShowFrame
    ld de,$1211
    ld hl,EnterTxt
    call FastVputs           ; "You entered ..."
    ld de,$1915
    ld (pencol),de
    call FastVputs           ; "Enter your name"
    ld hl,$0305
    ld (currow),hl
    pop hl
    ld b,0            ; B = number of letters entered so far

WaK:               ; A simple string input routine follows
    push hl
    bcall(_getcsc)
    cp $38
    jr z,BackSpace
    cp $09
    jr z,NameDone
    cp $11
    jr nz,CheckLetter
    ld a,32
    pop hl
    jr PutLetter
CheckLetter:
    ld hl,Letters
    push bc
    ld bc,26
    cpir
    ld d,c
    pop bc
    pop hl
    jr nz,WaK
    ld a,65
    add a,d
PutLetter:
    ld c,a
    ld a,b
    cp 10
    jr z,WaK
    ld (hl),c
    inc hl
    inc b
    ld a,c
    bcall(_putc)
    jr WaK
BackSpace:
    pop hl
    ld a,b
    or a
    jr z,WaK
    dec b
    dec hl
    push hl
    ld (hl),32
    ld hl,curcol
    dec (hl)
    ld a,32
    bcall(_putc)
    dec (hl)
    pop hl
    jr WaK
NameDone:
    pop hl
    jp LevelChoose

CheckBar:           ; Find out how it goes for ya
    xor a
    ld hl,board+4
    ld b,16
RepCheckBar:
    push hl
    push af
    call LD_HL_MHL
    pop af
    ld de,%1110000000000111   ; This would indicate an empty row
    bcall(_cphlde)
    pop hl
    jr z,EmptyRow
    inc a
    inc hl
    inc hl
    djnz RepCheckBar
EmptyRow:
    add a,$D0
    jp SendByte           ; Send high information to opponent

ShowCurB:           ; This shows the current piece
    ld de,(cXY)
    ld hl,cB
ShowB:
    ld b,4
SNext:
    push hl
    call LD_HL_MHL
    add hl,de
    call PutBlock        ; Put one block of the piece
    pop hl
    inc hl
    inc hl
    djnz SNext
    ret

EraseCurB:           ; And this erase the current piece
    ld de,(cXY)
    ld hl,cB
EraseB:
    ld b,4
ENext:
    push hl
    call LD_HL_MHL
    add hl,de
    call EraseBlock
    pop hl
    inc hl
    inc hl
    djnz ENext
    ret

TestNewB:           ; Check if the piece is at an allowed position
    ld b,8
    ld a,(newRot)
    call Uncrunch
    ld de,(newXY)
    ld hl,cB+8
    ld b,4
TNext:
    push hl
    call LD_HL_MHL
    add hl,de
    call GetCoord
    jr nz,NotPoss
    pop hl
    inc hl
    inc hl
    djnz TNext
    xor a
    ret
NotPoss:
    pop hl
    ret

NewB:               ; Creates a new piece and updates information
    ld hl,(scoreU)
    ld h,0
    bcall(_divhlby10)        ; L = (scoreU) DIV 10
    ld (scoreU),a           ; (scoreU) = (scoreU) MOD 10
    ld a,l
    or a
    jr z,CheckLines       ; If scoreU was <10, no 'real' score update
    ex de,hl
    ld hl,(score)
    add hl,de
    ld (score),hl           ; Else update the score
    call ShowInfo
CheckLines:
    xor a
    ld (linesflag),a      ; This holds how many lines you eliminated
    ld hl,board+4
    ld b,17
RepScan:
    push hl
    call LD_HL_MHL
    ld de,$FFFF           ; This would indicate a full row
    bcall(_cphlde)
    jr nz,NextRow           ; If it wasn't check next row
    pop hl
    push bc
    push de
    push hl
    ld d,h
    ld e,l
    inc hl
    inc hl
    ld c,b
    ld b,0
    sla c
    ldir               ; Move everything down
    ld hl,linesflag
    inc (hl)           ; Increase lines eliminated
    ld hl,lines
    inc (hl)           ; And also the total line counter
    pop hl
    pop de
    pop bc
    jr RepScan
NextRow:
    pop hl
    inc hl
    inc hl
    djnz RepScan
    ld a,(linesflag)
    or a
    jr z,NoScoring        ; If no lines gotten, no score increase
    push af
    ld b,a
    ld a,(players)
    dec a
    jr z,NoPenLines       ; If one player, no penalty lines sent
    push bc
    call CheckBar
    pop bc
    dec b
    jr z,NoPenLines       ; If only one line eliminated, no penatly lines
    ld a,(declines)       ; The flag, 1-3 or 2-4 lines to send
    sub b
    neg               ; Now A = no of penalty lines
    push af
    or $F0
    call SendByte        ; Send it over to the opponent
    pop af
    ld hl,lastbar
    add a,(hl)           ; Increase the opponents bar
    inc a
    call ShowBar           ; And show it
NoPenLines:
    pop af
    ld hl,Scoring
    ld d,0
    ld e,a
    dec e
    add hl,de
    ld h,(hl)           ; H = score for level 0
    ld a,(level)
    inc a
    ld l,a
    bcall(_htimesl)       ; Multiply with (level+1)
    ex de,hl
    ld hl,score
    push hl
    call LD_HL_MHL
    add hl,de           ; Add with total score so far
    ex de,hl
    pop hl
    ld (hl),e           ; Store the new score
    inc hl
    ld (hl),d
    call ShowWell        ; Update the well
NoScoring:
    ld h,0
    ld a,(lines)
    ld l,a
    bcall(_divhlby10)        ; HL = lines DIV 10
    ld a,(level)
    cp l               ; Check if the level should increase
    jr nc,NoNewLevel
    ld a,l
    ld (level),a           ; Update the levle
    call PastePattern
    call ShowPattern
    ld a,(players)
    dec a
    jr z,NoNewLevel
    ld a,(lastbar)
    call ShowBar
NoNewLevel:

    call ShowInfo
CreateNew:
    call CreateNewPiece  ; Randomize new piece
    xor a
    ld (cRot),a
    ld (newRot),a
    ld b,0
    call Uncrunch        ; Uncrunch the piece
    ld hl,$0610
    ld (cXY),hl
    ld (newXY),hl
    call TestNewB        ; Check if it's possible to put out the piece
    jr z,NotDead
    ld hl,flags
    set 0,(hl)           ; If not, set dead flag
NotDead:
    jp ShowCurB           ; Show the current piece
ShowInfo:           ; Updates score, level and lives
    set 7, (iy+$14)
    ld de,$070C
    ld hl,score
    call LD_HL_MHL
    ld b,5
    call F_DM_HL_DECI3
    ld de,$1918
    ld hl,level
    ld l,(hl)
    ld h,0
    ld b,2
    call F_DM_HL_DECI3
    ld de,$2B13
    ld hl,lines
    ld l,(hl)
    ld h,0
    ld b,3
    call F_DM_HL_DECI3
    res 7, (iy+$14)
    ret      
CreateNewPiece:
    ld de,$1403
    ld hl,cB+16           ; Remove the next piece
    call EraseB
RandP:
    ld a,(next)
    ld (cBit),a
    ld hl,(cNBitOfs)      ; Make it the current piece instead
    ld (cBitOfs),hl
    ld a,7
    call PRandom        ; Get a random number between 0-6
    inc a               ; Increase with 1 to get between 1-7
    ld (next),a
    add a,a
    add a,a
    add a,a
    ld hl,BitData-8
    ld d,0
    ld e,a
    add hl,de           ; Find out where the bit structure is
    ld (cNBitOfs),hl
    ld b,16
    xor a
    call Uncrunch2
    ld de,$1403
    ld hl,cB+16           ; Remove the next piece
    jp ShowB
Uncrunch:           ; Extracts the piece from compressed data
    ld hl,(cBitOfs)
Uncrunch2:
    and %00000011
    ld d,0
    ld e,a
    sla e
    add hl,de
    push hl
    ld hl,cB
    ld d,0
    ld e,b
    add hl,de
    ex de,hl
    pop hl
    call LD_HL_MHL
    ld b,8
URep:
    ld a,l
    and %00000011
    ld (de),a
    inc de
    rr h
    rr l
    rr h
    rr l
    djnz URep
    ret

PutCoord:           ; Stores a block in the wello
    push hl
    call GetBoardOfs
    or (hl)
    ld (hl),a
    pop hl
    ret

GetCoord:           ; Finds out if there is a block in the well at H,L
    push hl
    call GetBoardOfs
    and (hl)
    pop hl
    ret

GetBoardOfs:           ; Convert location H,L to an address, HL
    push bc
    ld c,l
    sla c
    bit 3,h
    jr z,LeftPart
    inc c
LeftPart:
    ld b,h
    ld a,1
RotateAgain:
    rlca
    djnz RotateAgain
    ld hl,board
    ld b,0
    add hl,bc
    pop bc
    ret

PutBlock:           ; Put block at H,L
    push bc
    push de
    push hl
    call GetBlockOfs
PutRow:
    push af
    or (hl)
    ld (hl),a
    pop af
    add hl,de
    djnz PutRow
    pop hl
    pop de
    pop bc
    ret

EraseBlock:           ; Erase a block at H,L
    push bc
    push de
    push hl
    call GetBlockOfs
    xor $FF
EraseRow:
    push af
    and (hl)
    ld (hl),a
    pop af
    add hl,de
    djnz EraseRow
    pop hl
    pop de
    pop bc
    ret

GetBlockOfs:           ; Finds out where on the screen H,L is
    ld c,h
    ld h,0
    ld a,17
    sub l
    jp m, OffScreen
    ld l,a
    sla l
    add a,l \ ld l,a
    sla l
    add hl,hl
    add hl,hl
    add hl,hl
    ld a,c
    srl a
    add a,4
    ld d,0
    ld e,a
    add hl,de
    ld de,GRAPH_MEM
    add hl,de
    ld b,4
    ld de,12
    ld a,$F0 
    bit 0,c
    ret z
    ld a,$0F
    ret

OffScreen:
     xor a
     ret

PRandom:           ; Creates a pseudorandom number 0 <= x < A
    push bc
    push de
    push hl
    ld b,a
    ld a,r
    add a,a
    ld hl,0
    ld d,0
    ld e,a
RMul:
    add hl,de
    djnz RMul
    ld a,h
    pop hl
    pop de
    pop bc
    ret

ShowLayout:           ; Shows the game layout
    set 7, (iy+$14)     
    push de
    ld de,$000C
    ld hl,InfoText
    call FastVputs
    ld de,$120C
    call FastVputs
    ld de,$240C
    call FastVputs
    pop de
    res 7, (iy+$14)
GFXNewRow:
    ld de,12
    ld b,64
    ld ix,GRAPH_MEM
DWNextRow:
    ld (ix+5),$10
    ld (ix+10),$08
    add ix,de
    djnz DWNextRow

ShowPattern:           ; Show pattern for the current level
    ld hl,Pattern
    ld a,(level)
    cp 16
    jr c,Below16
    ld a,15               ; If level>15, show pattern for level 15
Below16:
    add a,a
    add a,a
    add a,a
    ld d,0
    ld e,a
    add hl,de
    push hl
    pop ix
    ld de,APD_BUF
    ld b,8
SP_Row:
    push bc
    push ix
    pop hl
    ld b,8
SP_Line:
    ld a,(hl)
    inc hl
    push bc
    ld b,12            ; was 16
SP_Byte:
    ld (de),a
    inc de
    djnz SP_Byte
    pop bc
    djnz SP_Line
    pop bc
    djnz SP_Row
    ld ix,Gaps
    ld b,(ix-1)
MakeGap:
    push bc
    ld h,(ix+1)
    ld l,(ix)
    ld de,APD_BUF
    add hl,de
    ld a,(ix+2)
    ld b,(ix+3)
MK_Row:
    push bc
    push hl
    ld d,a
    ld b,(ix+4)
MK_Col:
    ld c,a
    and (hl)
    ld (hl),a
    ld a,c
    rrca
    jr c,MK_SameByte
    inc hl
MK_SameByte:
    djnz MK_Col
    ld a,d
    pop hl
    pop bc
    ld de,12            ;was 16
    add hl,de
    djnz MK_Row
    ld de,5
    add ix,de
    pop bc
    djnz MakeGap

PastePattern:          ; XOR the pattern on the screen
    ld hl,768
    ld ix,GRAPH_MEM
    ld de,APD_BUF
PasteIt:
    ld a,(de)
    xor (ix)
    ld (ix),a
    inc de
    dec hl
    inc ix
    ld a,h
    or l
    jr nz,PasteIt
    ret

ShowWell:           ; Show the whole well
    ld hl,$0302
SWNewRow:
    ld b,10
RepPut:
    call GetCoord
    push af
    call z,EraseBlock
    pop af
    call nz,PutBlock
    inc h
    djnz RepPut
    ld h,$03
    inc l
    ld a,l
    cp $12
    jr nz,SWNewRow
    ret

FastVputs:
    ld (pencol),de
    bcall(_vputs)
    ret
FastPutc:
    ld (currow),de
    bcall(_putc)
    ret

ShowFrame:           ; Clears the screen and shows some info
    bcall(_clrlcdf)
    ld de,$0000
    set 3,(iy+5)
    ld hl,Title
    call FastPuts
    res 3,(iy+5)
    ld de,57*256+0
    ld hl,Coder
    jr FastVputs

Quit:
    set 6,(iy+9)        ;Restore the StatVars, to avoid screen garbage
    set 1,(iy+13)        ;Same with TextShadow
    set 2,(iy+8)
Quitter:
    bcall(_cleargbuf)
    ret

F_DM_HL_DECI3:
    ld (pencol),de
DM_HL_DECI3:           ; Display HL in menu style with leading zeros
    ld de,string+5
    xor a
    ld (de),a
RepUnp:
    dec de
    bcall(_divhlby10)
    add a,48
    ld (de),a
    djnz RepUnp
    ex de,hl
    bcall(_vputs)
    ret

LD_HL_MHL:
    push de
    ld e,(hl)
    inc hl
    ld d,(hl)
    pop hl
    ex de,hl
    ret


;         ██     ██   ██    ███   ███   ███
;  ████   ██    ██     ██    █    █       █
;

BitData:           ; Compressed data of the pieces (28 pieces)
    .dw %0001010110011000,%0110010100000100
    .dw %0010000101011001,%0110101001010100
    .dw %0001010110010000,%0010011001010100
    .dw %0001010110011010,%0110010101001000
    .dw %0101100101001000,%0101100101001000
    .dw %0101100101001000,%0101100101001000
    .dw %0001010110010100,%0001011001010100
    .dw %0001011001011001,%0110010101001001
    .dw %0000010101001001,%0010000101010100
    .dw %0000010101001001,%0010000101010100
    .dw %0001010101001000,%0001000001100101
    .dw %0001010101001000,%0001000001100101
    .dw %0001010110011101,%0111011001010100
    .dw %0001010110011101,%0111011001010100

Scoring:           ; Score for each level
    .db 4,10,30,120

LevelCnts:
     .db 40,36,32,28,25,22,19,17,15,13,11,10,9,8,7,6,5,4,3,2,1

Resume:
    .db 0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

Hiscore:
    .db "1. ----------",0,0,0
    .db "2. ----------",0,0,0
    .db "3. ----------",0,0,0

Title:
    .db "  ZTetris v1.1  ",0

Coder:
    .db "by: Sam H/Jimmy M/Pat D/AE",0

PlChoose:
    .db "Choose player mode",0
    .db "1 player",0
    .db "2 players",0

GameOverText:
    .db " Game Over ",0

WinTxt:
    .db " You Win ",0

PauseTxt:
    .db "* PAUSE *",0

WaitTxt:
    .db "* WAITING *",0

EnterTxt:
    .db "You have a hiscore!",0
    .db "Enter your name:",0

HighTxt:
    .db "High",0

SLTxt:
    .db "Send 2-4 lines",0
NLTxt:
    .db "2-4",0
    .db "1-3",0

ScrambleTxt:
    .db "unscramble",0
    .db " scrambled ",0

Letters:
    .db $1A,$22,$2A,$0B,$13,$1B,$23,$2B,$0C,$14,$1C
    .db $24,$2C,$0D,$15,$1D,$25,$2D,$0E,$16,$1E,$26
    .db $2E,$1F,$27,$2F

PixelFixer:        ;Pixel Line Bug Fix
    .db " 0 1 2 3 4",0
    .db " 5 6 7 8 9",0

InfoText:
    .db "Score",0
    .db "Level",0
    .db "Lines",0
Pattern:           ; Pattern for each level
    .db $AA,$55,$AA,$55,$AA,$55,$AA,$55
    .db $88,$FF,$22,$FF,$88,$FF,$22,$FF
    .db $FF,$99,$99,$FF,$FF,$99,$99,$FF
    .db $50,$D7,$14,$F7,$00,$F7,$14,$D7
    .db $EA,$AA,$AE,$00,$57,$55,$75,$00
    .db $FE,$AA,$AA,$AA,$28,$AA,$FE,$00
    .db $55,$55,$AA,$AA,$55,$55,$AA,$AA
    .db $80,$FE,$02,$FB,$08,$EF,$20,$BF
    .db $EE,$EE,$EE,$00,$77,$77,$77,$00
    .db $66,$CC,$99,$33,$66,$CC,$99,$33
    .db $CC,$33,$CC,$33,$CC,$33,$CC,$33
    .db $FE,$82,$BA,$AA,$BA,$82,$FE,$00
    .db $CC,$CC,$33,$33,$CC,$CC,$33,$33
    .db $FF,$AA,$FF,$AA,$FF,$AA,$FF,$AA
    .db $7C,$FE,$7C,$00,$7C,$FE,$7C,$00
    .db $FF,$EF,$47,$12,$B8,$FD,$FF,$FF

    .db 5
Gaps:               ; Gaps where the pattern shouldn't be shown
    .dw $005 \ .db $EF,64,42 ;well
    .dw $001 \ .db $EF,14,23 ;score
    .dw $0D9 \ .db $EF,14,23 ;level
    .dw $1B1 \ .db $EF,14,23 ;Lines
    .dw $271 \ .db $EF,10,23 ;Next piece

#ifdef TI83P
.include "linkrout.h"

#else
.include "link83.h"
#endif

.end
