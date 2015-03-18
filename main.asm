#include "kernel.inc"
#include "corelib.inc"
    .db "KEXC"
    .db KEXC_ENTRY_POINT
    .dw start
    .db KEXC_STACK_SIZE
    .dw 20
    .db KEXC_KERNEL_VER
    .db 0, 6
    .db KEXC_NAME
    .dw name
    .db KEXC_DESCRIPTION
    .dw description
    .db KEXC_HEADER_END
name:
    .db "ZTetris",0
description:
    .db "The Best TI-83+ Tetris",0

corelibPath:
    .db "/lib/core", 0

; 2 byte variables are stored little-endian (i.e. reverse order of registers)
cBitOfs   .equ 0       ;* WORD
cBit      .equ 2       ;* 4 BYTE
cXY       .equ 6       ;* WORD
cB        .equ 8       ;* 24 BYTE
cRot      .equ 32      ;* BYTE
flags     .equ 33      ;* BYTE  (0 - Quit, 1 - Update)
newXY     .equ 34      ;* WORD
newRot    .equ 36      ;* BYTE
counter   .equ 37      ;* BYTE
next      .equ 38      ;* BYTE
linesflag .equ 39      ;* BYTE
score     .equ 40      ;* WORD
scoreU    .equ 42      ;* BYTE
lines     .equ 43      ;* BYTE
level     .equ 44      ;* BYTE
string    .equ 45      ;* 6 BYTE
place     .equ 51      ;* BYTE
cNBitOfs  .equ 52      ;* WORD
players   .equ 54      ;* BYTE
lcounter  .equ 55      ;* WORD
declines  .equ 57      ;* BYTE
stlevel   .equ 58      ;* BYTE
lastbar   .equ 59      ;* BYTE
hsflag    .equ 60      ;* BYTE
scrflag   .equ 61      ;* BYTE
gap       .equ 62      ;* BYTE
sbyte     .equ 63      ;* BYTE
sthigh    .equ 64      ;* BYTE
high      .equ 65      ;* BYTE
linkcnt   .equ 66      ;* BYTE
lastKey   .equ 67      ;* BYTE
lastKCnt  .equ 68      ;* BYTE
board     .equ 69      ;* 40 BYTE
;108 Bytes total
APD_BUF   .equ 109     ;* 768 BYTE
;876 Bytes total
memSize   .equ 877

start:
    ; KnightOS:
    ; TIOS stores system flags in IY+offset
    ; we don't need this so we can reclaim IY for the screen buffer
    pcall(getLcdLock)
    pcall(getKeypadLock)
    kld(de,corelibPath)
    pcall(loadLibrary)
    pcall(allocScreenBuffer)
    ret nz                             ; Exit if insufficient memory
    ld bc, memSize
    pcall(malloc)
    ret nz                             ; Exit if insufficient memory
    ; Read high scores from file
    kld(de, hiscorePath)
    pcall(fileExists)
    kjp(nz, ReProgStart)
    pcall(openFileRead)
    push ix
        kld(ix, Hiscore)
        ld bc, 48
        pcall(streamReadBuffer)
    pop ix
    pcall(closeStream)
    ; KnightOS TODO:
    ; Add way to save/load game from file
    jr ReProgStart

; KnightOS TODO:
; Put Menu and Game code in seperate files

; Invert selected level
drawLevelCursor:                       ; Puts digit A on the correct place
    push af                            ; Used when choosing ProgStart level
    push bc
        ld l, 15
        ld b, a
        inc b
        cp 5
        jr c, FirstRow
        ld l, 23
        sub 5
        ld b, a
        inc b
FirstRow:
        add a, 5
        djnz FirstRow
        add a, 10
        ld e, a
        ld bc, 7 << 8 | 5
        pcall(rectXOR)
    pop bc
    pop af
    ret

; KnightOS TODO:
; This function is only used once and should probably be spliced straight into the code
drawLevelGrid:
    ld de, 16 << 8 | 16
    xor a
    ld b, 5
levelNumsLoop1:                        ; Print first row of numbers
    pcall(drawDecA)
    inc a
    inc d
    inc d
    djnz levelNumsLoop1
    ld de, 16 << 8 | 24
    ld b, 5
levelNumsLoop2:                        ; Print second row of numbers
    pcall(drawDecA)
    inc a
    inc d
    inc d
    djnz levelNumsLoop2
    ld c, 15
    ld l, 15
    ld a, 14
    ld b, 5
levelNumsLoop3:                        ; Draw vertical lines
    pcall(drawVLine)
    add a, 6
    djnz levelNumsLoop3
    pcall(drawVLine)
    ld de, 14 << 8 | 14
    ld hl, 44 << 8 | 14
    ld a, e
    ld b, 3
levelNumsLoop4:                        ; Draw horizontal lines
    pcall(drawLine)
    add a, 8
    ld e, a
    ld l, a
    djnz levelNumsLoop4
    ret

drawHeightNum:
    ld bc, 7 << 8 | 5
    ld e, 64
    ld l, 21
    ; KnightOS TODO:
    ; These pushes and pops are a workaround for KnightOS bug #277
    ; I could fix it but I don't feel like it right now
    push af
        pcall(rectAND)                 ; Clear area
    pop af
    ld de, 65 << 8 | 22
    pcall(drawDecA)                    ; And show the High
    ret

ReProgStart:
    kcall(ShowFrame)                   ; Show title
    ld de, 2 << 8 | 50                 ; X = 2, Y = 50
    kld(hl,Coder)
    pcall(drawStr)
    kld(hl,PlChoose)                   ; "Choose player mode"
    ld de, 15 << 8 | 18                   ; X = 15 Y = 18
    pcall(drawStr)
    kld(hl,PlChoose1)                  ; "1 player"
    ld de, 32 << 8 | 24
    pcall(drawStr)
    kld(hl,PlChoose2)                  ; "2 player"
    ld de, 32 << 8 | 30
    pcall(drawStr)
    xor a
    ld (ix+stlevel),a                  ; When ProgStarting a new game, stLevel and stHigh
    ld (ix+sthigh),a                   ; will be reset
    inc a
    ld (ix+players),a                  ; Default option, 1 player
ChoosePlayers:
    ld a,(ix+players)
    ld d, 28                           ; X = 29
    push af
        add a,3
        push af                        ; Multiply by 6 (1 row of text)
            sla a
            ld e, a
        pop af
        sla a
        sla a
        add a, e
        ld e,a
        ld a, 62                       ; >
        pcall(drawChar)                ; Put the small arrow
    pop af
    ld d, 28
    sub 6
    neg
    push af
        sla a
        ld e, a
    pop af
    sla a
    sla a
    add a, e
    ld e, a
    ld a, 62
    pcall(drawCharAND)                 ; And remove it from the other position
WKCP:
    pcall(fastCopy)
    pcall(flushKeys)
    corelib(appWaitKey)
    push ix \ pop hl
    push de
        ld de, players
        add hl, de
    pop de
    cp kMode
    kjp(z,Quit)
    cp kEnter
    jr z,LevelChoose
    cp k2nd
    jr z,LevelChoose
    cp kUp
    jr z,ChangePlayers
    cp kDown
    jr z,ChangePlayers
    jr WKCP
ChangePlayers:
    ld a,(hl)
    xor 3                              ; This will turn 1 -> 2 and 2 -> 1
    ld (hl),a
    jr ChoosePlayers
LevelChoose:
    kcall(ShowFrame)                   ; I know I used these 2x, but that's to stop the on screen trash.
    ld a,(ix+stlevel)
    ld (ix+level),a                    ; The cursor will ProgStart at the last played level
    ld a,(ix+sthigh)
    ld (ix+high),a                     ; And with the high
NewDigit:
    ld de, 14 << 8 | 8                 ; "Level:"
    kld(hl,levelTxt)
    pcall(drawStr)
    kcall(drawLevelGrid)
    ld de, 63 << 8 | 14
    kld(hl,HighTxt)                    ; "Height"
    pcall(drawStr)
    ld c, 7                            ; Draw box for height number
    ld a, 63
    ld l, 21
    pcall(drawVLine)
    add a, 6
    pcall(drawVLine)
    ld de, 63 << 8 | 20
    ld hl, 69 << 8 | 20
    pcall(drawLine)
    ld e, 28
    ld l, 28
    pcall(drawLine)
    ld a, (ix+level)
    kcall(drawLevelCursor)             ; Invert the ProgStarting level digit
    ld a,(ix+high)
    kcall(drawHeightNum)
    ld a,(ix+players)
    dec a                              ; Check if the hiscore should be shown
    jr nz,Show2PlayOpt                 ; or two player options
    ld de, 14 << 8 | 32
    kld(hl,hiScoresTxt)
    pcall(drawStr)
    kld(hl,Hiscore)
    ld a, 38
    ld b, 3
NewPos:
    ld d, 14
    ld e, a
    add a,6
    push af
    push bc
        pcall(drawStr)                 ; Show name
        ld a,0x46
        ld bc, 14
        add hl, bc
        ld b,5                         ; Show 5 digits for high scores
        push hl
            kcall(LD_HL_MHL)           ; Get that persons score
            ld d, 63
            kcall(DM_HL_DECI3)         ; And show it
        pop hl
        inc hl
        inc hl                         ; HL -> next hiscore table entry
    pop bc
    pop af
    djnz NewPos
    jr ZWaitKey
Show2PlayOpt:
    ld de, 14 << 8 | 38
    kld(hl,SLTxt)
    pcall(drawStr)                     ; "Send 2-4 lines"
    ld de, 14 << 8 | 44
    kld(hl,InfoText3)
    pcall(drawStr)                     ; "Lines "
    ld bc, 13 << 8 | 5
    ld e, 13
    ld l, 37
    pcall(rectXOR)                     ; Invert S and L
    xor a
    ld (ix+declines),a                 ; Clear the flags to the two option above
    ld (ix+scrflag),a
ZWaitKey:
    ld a,(ix+players)
    dec a                              ; If two players, the two players options
    jr z,ZGetKey                       ; should be shown as well
    ld bc, 7 << 8 | 43
    ld e, 32
    ld l, 44
    pcall(rectAND)
    kld(hl,ScrambleTxt)
    ld a,(ix+scrflag)
    or a
    jr z,ShowScrFlag
    ld de,12
    add hl,de
ShowScrFlag:
    ld de, 32 << 8 | 44
    pcall(drawStr)                     ; Show "scrambled" or "unscrambled"
ZGetKey:
    pcall(fastCopy)
    pcall(flushKeys)
    corelib(appWaitKey)
    or a
    jr z,ZGetKey
    cp kMode
    kjp(z,Quit)
    cp kEnter
    kjp(z,ProgStartGame)
    cp k2nd
    kjp(z,ProgStartGame)
    cp kMinus
    jr z,DecHigh
    cp kPlus
    jr z,IncHigh
    cp kRParen                         ; Right Parentheses
    kjp(z,ChangeScrFlag)
    cp kLn
    jr nz,CheckLevChg
    ld a,(ix+players)
    dec a
    jr z,ZGetKey
    ld a,(ix+declines)
    xor 1                              ; Change the declines flag (1-3 or 2-4)
    ld (ix+declines),a
    add a,a
    add a,a
    ld bc, 6 << 8 | 12
    ld e, 31
    ld l, 38
    push af                            ; KnightOS TODO: Bug #277
        pcall(rectAND)
    pop af
    kld(hl,NLTxt)
    ld d,0
    ld e,a
    add hl,de
    ld de, 31 << 8 | 38
    pcall(drawStr)                     ; Update it on the screen
    jr ZGetKey
CheckLevChg:
    dec a
    jr z,ChangeRow
    dec a
    jr z,LevLeft
    dec a
    jr z,LevRight
    dec a
    kjp(nz,ZGetKey)
    jr ChangeRow

DecHigh:
    ld a,(ix+high)
    or a
    jr z,ZGetKey                       ; Don't decrease if high is 0
    dec a
    ld (ix+high),a
    kcall(drawHeightNum)
    kjp(ZWaitKey)

IncHigh:
    ld a,(ix+high)
    cp 5
    jr z,ZGetKey                       ; Don't increase if high is 5
    inc a
    ld (ix+high),a
    kcall(drawHeightNum)
    kjp(ZWaitKey)

ChangeRow:
    ld a,(ix+level)
    add a,5                            ; Changing row is like adding with 5, mod 10
ChkLevEdges:
    daa                                ; Modulo 10 (sort of)
    and 0x0F
SetLevel:
    ld b,a
    ld a,(ix+level)
    kcall(drawLevelCursor)             ; Remove the inverted digit
    ld a,b
    ld (ix+level),a                    ; And set the new level
    kcall(drawLevelCursor)
    kjp(ZWaitKey)
LevLeft:
    ld a,(ix+level)
    dec a
    jr ChkLevEdges
LevRight:
    ld a,(ix+level)
    inc a
    jr ChkLevEdges

ChangeScrFlag:
    ld a, 1
    xor (ix+scrflag)
    ld (ix+scrflag), a
    kjp(ZWaitKey)

ProgStartGame:
    ld a,(ix+level)
    ld (ix+stlevel),a                  ; Copy the selected level and high so they will
    ld a,(ix+high)                     ; be default next time
    ld (ix+sthigh),a
    ld a,(ix+players)
    dec a
    jr z,NoWait
    xor a
    ld (ix+lastbar),a

    kcall(ShowFrame)
    ld de, 29 << 8 | 26
    kld(hl,WaitTxt)
    pcall(drawStr)                     ; "* WAITING *"
    pcall(fastCopy)

    kcall(ReceiveByte)
    or a
    jr nz,NoWait                       ; If byte gotten, the other calc was waiting
    ld a,1
    ld (ix+hsflag),a                   ; This will allow the user to cancel with EXIT
    ld a,0xAA
    kcall(SendByte)                    ; Else wait until the other calc responds

NoWait:
    xor a
    ld (ix+hsflag),a
    ld (ix+sbyte),a                    ; This is a linkbuffer byte
    pcall(clearBuffer)
    kcall(RandP)                       ; Randomize the first piece

    ld hl,0xFFFF
    ld (ix+board+3),h
    ld (ix+board+2),l
    push ix \ pop hl
    push de
        ld de, board+4
        add hl, de
    pop de
    ld b,18
InitRow:                               ; Setting up the border aroudn the well
    ld (hl),0b00000111
    inc hl
    ld (hl),0b11100000
    inc hl
    djnz InitRow

    kcall(ShowLayout)
    ld (ix+linkcnt),10                 ; This counter decrease every frame. When 0,
    ld a,(ix+players)                  ; check link port. If too often check, it slows down
    dec a
    ld a,0                             ; Can't use 'xor a' here! It would affect the Z flag
    kcall(nz,ShowBar)                  ; Show the bar at height 0
ResetVars:
    xor a
    ld (ix+flags),a                    ; Clear a lot of vars
    ld (ix+cBit),a
    ld (ix+scoreU), a                  ; Clears lines as well
    ld (ix+lines), a
    ld (ix+score+1), a
    ld (ix+score), a
    ld (ix + lastKey), a
    ld (ix + lastKCnt), 5
    kcall(NewB)                        ; Prepares a new piece
    pcall(fastCopy)
    ld a,(ix+high)
    or a
    jr z,MainLoop
    ld b,(ix+scrflag)                  ; If starting with trash lines, they should
    push bc                            ; always be scrambled even though the option         
    push hl                            ; unscrambled is set
    ld (ix+scrflag),1                  ; So temporary set it to scrambled lines
    add a,a                            ; No trash lines = high*2
    kcall(AddLines)                    ; Create trash lines
    pop hl
    pop bc
    ld (ix+scrflag),b                  ; And reset the scrflag
    pcall(fastCopy)

MainLoop:                              ; The main loop
    kld(hl,LevelCnts)
    ld a,(ix+level)
    ld d,0
    ld e,a
    add hl,de
    ld a,(hl)                          ; A = the delay time of the current level
    ld (ix+counter),a
DelayLoop:
    ; Timing measurements for this loop (T-States):
    ; 6 Mhz:                    15 Mhz:
    ; MirageOS:     Ion:        KnightOS (ZTv1.1.1):    KnightOS (ZTv1.1.2):
    ; 166655        144840      86509                   412569
    ; 166737        144777      86509                   412562
    ; 166602        143577      85704                   412562
    ; 166667        144782      86509                   413367
    ; 166667        143577      86509                   412569
    ; 166667        144777      86509                   413367
    res 1,(ix+flags)                   ; Clear the update flag
    ; KnightOS TODO:
    ; Switch to interrupt based timing once its implemented in KnightOS
    ; This is what the MirageOS version uses
    corelib(appGetKey)                 ; TIOS:     2596 T-States
                                       ; KnightOS: 2199 T-States
    ; KnightOS TODO:
    ; corelib suspends the thread when switching to the castle or threadlist
    ; but it might be fun to show off KnightOS's multitasking abilities by not doing that
    ; KnightOS TODO:
    ; Redraw immediately after a context switch

    ; TIOS's getCSC filters repeats, but KnightOS's getKey does not
    ; The following remembers the last key pressed
    ; and only repeats it after 5 loops.
    ; lastKey is reset if the key is released
    ; so that you can move faster than the natural repeat
    cp (ix + lastKey)
    jr nz, notSameKey
    dec (ix + lastKCnt)
    jr z, repeatKey
    xor a
    jr doKey
notSameKey:
    ld (ix + lastKey), a               ; Save last key
repeatKey:
    ld b, 5                            ; Reset key repeat counter
    ld (ix + lastKCnt), b
doKey:
    cp kClear
    kjp(z,AbortGame)
    cp kMode
    kjp(z,Pause)
    cp k2nd
    kjp(z,Rotate)
    cp kAlpha
    kjp(z,RotateBack)
    cp kDel
    kjp(z,TeacherKey)
    cp kXTThetaN
    kjp(z,Drop)
    dec a
    jr z,MoveDown
    dec a
    jr z,MoveLeft
    dec a
    jr z,MoveRight
    dec a
    jr z,Rotate

Wait:
    bit 0,(ix+flags)                   ; Check if the player became gameover this frame
    kjp(nz,GameOver)
    bit 1,(ix+flags)                   ; Check if anything happened (movements)
    kcall(nz,Update)                   ; If so, update that

    ;ld bc,3200                        ; TIOS:     84910, 86115 T-States
                                       ; KnightOS: 84010
    ; KnightOS TODO:
    ; Change delay depending on clock speed of CPU
    ;ld bc, 6258                       ; 6 Mhz Calcs
    ld bc, 15645                       ; 15 Mhz Calcs
                                       ; This gives correct fall rate on the TI-84+SE
dwait:
    dec bc
    ld a,b
    or c
    jr nz,dwait
    
    dec (ix+linkcnt)
    kcall(z,GetLinkInfo)              ; If the link counter reaches zero, check link port
    dec (ix+counter)                   ; Decrease the counter
    jr nz,DelayLoop                    ; If not zero, check for keys again
    jr FallDown
MoveDown:
    inc (ix+scoreU)                    ; When DOWN is pressed, increase the score
FallDown:
    kcall(GetLinkInfo)                 ; Before moving down, always check linkport
    dec (ix+newXY)                     ; Decrease the y coordinate
    kcall(Update)                      ; Check if possible
    kjp(z,MainLoop)                    ; If so, repeat mainloop
BotReached:
    push ix \ pop hl                   ; Else the bottom is reached
    ld de, cB
    add hl, de
    ld d, (ix+cXY+1)
    ld e, (ix+cXY)
    ld b,4
StoreB:                                ; Store the piece in the well
    push hl
    kcall(LD_HL_MHL)
    add hl,de
    kcall(PutCoord)
    pop hl
    inc hl
    inc hl
    djnz StoreB
    ld a,(ix+players)
    dec a
    kcall(nz,CheckBar)                 ; If two player, check your highest line
    kcall(NewB)                        ; Last, randomize a new piece
    pcall(fastCopy)
    kjp(MainLoop)

MoveRight:
    inc (ix+newXY+1)                   ; Increase X coordinate
    jr SetUpdateFlag                   ; Set update flag

MoveLeft:
    dec (ix+newXY+1)                   ; Decrease left coordinate
SetUpdateFlag:
    set 1,(ix+flags)                   ; Set update flag
    jr Wait

Rotate:
    inc (ix+newRot)
    jr SetUpdateFlag

RotateBack:
    dec (ix+newRot)
    jr SetUpdateFlag

Drop:                                  ; When dropping, increase score with the
    inc (ix+scoreU)                    ; number of fallen steps
    dec (ix+newXY)                     ; Decrease Y coordinate
    kcall(Update)                      ; Update it on screen
    jr z,Drop                          ; If OK, move down again
    kcall(GetLinkInfo)                 ; Get link info
    jr BotReached                      ; Bottom reached, store piece.

TeacherKey:
    ld a,(ix+players)
    dec a
    kjp(nz,Wait)                       ; If two players, teacher key not allowed
    corelib(launchCastle)

Pause:
    ld a,(ix+players)
    dec a
    kjp(nz,Wait)                       ; Pause not allowed in two player game
    ; KnightOS TODO:
    ; This and the similar code in GameOver should be turned into a function
    ; Draw box
    ;ld de, 32 << 8 | 27
    ;ld hl, 64 << 8 | 27
    ;pcall(drawLine)
    ;ld e, 35
    ;ld l, 35
    ;pcall(drawLine)
    ;ld a, 32
    ;ld l, 28
    ;ld c, 7
    ;pcall(drawVLine)
    ;add a, 32
    ;pcall(drawVLine)
    ;; Clear area
    ;ld bc, 7 << 8 | 31
    ;ld e, 33
    ;ld l, 28
    ;pcall(rectAND)
    ;; Draw text
    ;ld de, 34 << 8 | 29
    kld(hl,PauseTxt)
    ;pcall(drawStr)                     ; "* PAUSE *"
    ;pcall(fastCopy)
    kld(de, pauseOptions)
    xor a
    ld b, a
    corelib(showMessage)
    ; KnightOS TODO:
    ; Put calculator to sleep after a while
    cp 1
    kjp(z, Quit)
Unpause::
    ; Redraw game grid
    pcall(clearBuffer)
    kcall(ShowLayout)
    kcall(ShowInfo)
    kcall(ShowWell)
    kcall(ShowCurB)
    push ix \ pop hl                   ; Show next piece
    ld de, cB+16
    add hl, de
    ld de, 0x1403
    kcall(ShowB)
    ;ld a, (ix+players)                ; Show bar in multiplayer mode
    ;dec a
    ;jr z, _
    ;ld a, (ix+lastbar)
    ;kcall(ShowBar)
    pcall(fastCopy)
    pcall(flushKeys)
    kjp(Wait)

ShowBar:                               ; Show the bar
    ld (ix+lastbar),a
    push iy \ pop hl
    ld de, (63 * 12) + 11
    add hl, de
    ld de,-12
    ld b,64
    add a,a
    add a,a
SB_Rep:
    ld c,a
    ld a,0x0F
    and (hl)
    ld (hl),a
    ld a,c
    or a
    jr z,ClearBar
    ld a,0x60
    or (hl)
    ld (hl),a
    dec c
ClearBar:
    ld a,c
    add hl,de
    djnz SB_Rep
    ret

AddLines:                              ; Add A lines, scrambled or unscrambled
    ld b,a
    push bc
    ld c,b
    ld b,0
    sla c
    push ix \ pop hl
    ld de, board+34
    add hl, de
    ld d, h
    ld e, l
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
    kcall(PRandom)
    ld (ix+gap),a
    push ix \ pop hl
    ld de, board+4
    add hl, de
AddTrashRow:
    push bc
    ld de,0xFFFF
    push hl
    ld b,5
Holes:
    push bc
    ld hl,0xFFFB
    ld a,(ix+scrflag)
    or a
    jr nz,RandGap
    ld a,(ix+gap)
    jr PutGap
RandGap:
    ld a,10
    kcall(PRandom)
PutGap:
    ld b,a
    inc b
RotWord:
    .db 0xCB,0x35                        ; SLL L - an undocumented Z80 instruction
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
    pop af
    add a, (ix+newXY)
    cp 0x10
    jr c,TopNotReached
    ld a,0x10
TopNotReached:
    ld (ix+newXY), a
    kcall(Update)
    push af
    kcall(ShowWell)
    kcall(ShowCurB)
    pop af
    kjp(nz,GameOver)
    ret

Update:                                ; Update the piece on the screen.
    kcall(TestNewB)                    ; Return NZ if not possible move
    jr nz,Sync
    kcall(EraseCurB)
    ld h, (ix+newXY+1)
    ld l, (ix+newXY)
    ld (ix+cXY+1), h
    ld (ix+cXY), l
    ld a,(ix+newRot)
    ld (ix+cRot),a
    ld b,0
    kcall(Uncrunch)
    kcall(ShowCurB)
    xor a
Sync:
    push af
        ld h, (ix+cXY+1)
        ld l, (ix+cXY)
        ld (ix+newXY+1), h
        ld (ix+newXY), l
        ld a,(ix+cRot)
        ld (ix+newRot),a
        pcall(fastCopy)
    pop af
    ret

GetLinkInfo:                           ; Fins out what happens to the opponent
    ld (ix+linkcnt),10                 ; Reset the link counter
    ld a,(ix+players)
    dec a
    ret z                              ; If one player, leave this routine
    kcall(ReceiveByte)                 ; Get a byte
    or a
    jr z,CheckSByte                    ; If no byte received, check if a byte should be sent
    ld b,a
    and 0x0F
    ld c,a
    ld a,b
    srl a
    srl a
    srl a
    srl a
    cp 0x0F
    jr z,PenaltyRows
    cp 0x0C
    kjp(z,YouWinP)
    cp 0x0D
    jr z,UpdateBar
    cp 0x0E
    ret nz
    ld c,16
UpdateBar:
    ld a,c
    kjp(ShowBar)
PenaltyRows:
    ld a,c
    inc a
    kjp(AddLines)

CheckSByte:
    ld a,(ix+sbyte)                    ; Check if byte in send buffer
    or a
    kcall(nz,SendByte)                 ; If so, send it
    ret

AbortGame:
    ld a,(ix+players)
    dec a
    kjp(z,CheckHiscore)                ; If one player abort, check hiscore table

GameOver:
    pcall(flushKeys)
    ; Draw box
    ld de, 29 << 8 | 27
    ld hl, 66 << 8 | 27
    pcall(drawLine)
    ld e, 35
    ld l, 35
    pcall(drawLine)
    ld a, 29
    ld l, 28
    ld c, 7
    pcall(drawVLine)
    add a, 37
    pcall(drawVLine)
    ; Clear area
    ld bc, 7 << 8 | 36
    ld e, 30
    ld l, 28
    pcall(rectAND)
    ; Draw text
    ld de, 31 << 8 | 29
    kld(hl,GameOverText)
    pcall(drawStr)
    ld a,(ix+players)
    dec a
    jr z,FlashGameOver                 ; If a two player game, send a byte telling
    ld a,0xC0                          ; that you lost
    ld b,3
SendWinByte:
    push bc
        kcall(SendByte)
        kcall(ReceiveByte)             ; This is for clearing up stuff (if both sent
        ld a,(ix+sbyte)                ; at the same time)
        or a
    pop bc
    jr z,FlashGameOver
    djnz SendWinByte
FlashGameOver:
    corelib(appGetKey)
    cp kEnter
    jr z,CheckHiscore
    cp k2nd
    jr z,CheckHiscore
    pcall(fastCopy)
    ld bc, 7 << 8 | 36
    ld e, 30
    ld l, 28
    pcall(rectXOR)
    ei
    ld b,20
FlashWait:
    halt
    djnz FlashWait
    jr FlashGameOver

YouWinP:
    pop hl
YouWin:
    ; KnightOS TODO:
    ; Fix Winning screen
    ; I think this is just for multiplayer
    pcall(flushKeys)
    corelib(appWaitKey)
    cp 0x09
    jr z,CheckHiscore
    cp 0x0f
    jr z,CheckHiscore
    ;ld a,(iy+5)                       ; Invert textFlags
    ;xor 8
    ;ld (iy+5),a
    pcall(fastCopy)
    ld de,0x0403
    kld(hl,WinTxt)
    pcall(drawStr)
    ei
    ld b,20
WFlashWait:
    halt
    djnz WFlashWait
    jr YouWin

CheckHiscore:
    kcall(Quit)
    ld a,(ix+players)
    dec a
    kjp(nz,LevelChoose)                ; No hiscore when two players
    kld(hl,Hiscore+14)                 ; HL -> hiscore
    ld d, (ix+score+1)                 ; DE = your score
    ld e, (ix+score)
    ld b,3
CheckP:
    push hl
    kcall(LD_HL_MHL)
    pcall(cpHLDE)
    pop hl
    jr c,ScoreGreater
    push de
    ld de,16
    add hl,de                          ; HL -> next score in hiscore table
    pop de
    djnz CheckP
    kjp(LevelChoose)                   ; Not in hiscore table
ScoreGreater:
    ld a,4
    sub b
    ld (ix+place),a
    cp 3                               ; If last place, no moves in hiscore table
    jr z,MoveDone
    kld(hl,Hiscore+19)
MoveAgain:                             ; Else move the other people down
    ld d,h
    ld e,l
    ld bc,16
    add hl,bc
    ex de,hl
    ld bc,13
    ldir
    kld(hl,Hiscore+3)
    dec a
    jr z,MoveAgain
MoveDone:
    ld a,(ix+place)                    ; Find out where to enter your name
    kld(hl,Hiscore+3)
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
        ld (hl),32                         ; Clear the previous name
        inc hl
        djnz RepClear
        inc hl
        ld d, (ix+score+1)
        ld e, (ix+score)
        ld (hl),e                          ; Put your score in the table
        inc hl
        ld (hl),d
    pop hl
    ; Ask for name
    push ix
        push hl \ pop ix
        kld(hl,EnterTxt)
        ld bc, 10
        corelib(promptString)
    pop ix
    ; Store high scores to file
    kld(de, hiscoreDir)
    pcall(directoryExists)
    pcall(nz, createDirectory)
    kld(de, hiscorePath)
    pcall(fileExists)
    pcall(z, deleteFile)
    pcall(openFileWrite)
    push ix
        kld(ix, Hiscore)
        ld bc, 48
        pcall(streamWriteBuffer)
    pop ix
    pcall(closeStream)
    kjp(LevelChoose)

CheckBar:                              ; Find out how it goes for ya
    xor a
    push ix \ pop hl
    push de
        ld de, board+4
        add hl, de
    pop de
    ld b,16
RepCheckBar:
    push hl
    push af
    kcall(LD_HL_MHL)
    pop af
    ld de,0b1110000000000111            ; This would indicate an empty row
    pcall(cpHLDE)
    pop hl
    jr z,EmptyRow
    inc a
    inc hl
    inc hl
    djnz RepCheckBar
EmptyRow:
    add a,0xD0
    kjp(SendByte)                      ; Send high information to opponent

ShowCurB:                              ; This shows the current piece
    push ix \ pop hl
    ld de, cB
    add hl, de
    ld d, (ix+cXY+1)
    ld e, (ix+cXY)
ShowB:
    ld b,4
SNext:
    push hl
    kcall(LD_HL_MHL)
    add hl,de
    kcall(PutBlock)                    ; Put one block of the piece
    pop hl
    inc hl
    inc hl
    djnz SNext
    ret

EraseCurB:                             ; And this erase the current piece
    push ix \ pop hl
    ld de, cB
    add hl, de
    ld d, (ix+cXY+1)
    ld e, (ix+cXY)
EraseB:
    ld b,4
ENext:
    push hl
    kcall(LD_HL_MHL)
    add hl,de
    kcall(EraseBlock)
    pop hl
    inc hl
    inc hl
    djnz ENext
    ret

TestNewB:                              ; Check if the piece is at an allowed position
    ld b,8
    ld a,(ix+newRot)
    kcall(Uncrunch)
    push ix \ pop hl
    ld de, cB+8
    add hl, de
    ld d, (ix+newXY+1)
    ld e, (ix+newXY)
    ld b,4
TNext:
    push hl
    kcall(LD_HL_MHL)
    add hl,de
    kcall(GetCoord)
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

NewB:                                  ; Creates a new piece and updates information
    ld l,(ix+scoreU)
    ld h,0
    push bc
        ld c, 10
        pcall(divHLByC)                ; L = (scoreU) DIV 10
    pop bc
    ld (ix+scoreU),a                   ; (scoreU) = (scoreU) MOD 10
    ld a,l
    or a
    jr z,CheckLines                    ; If scoreU was <10, no 'real' score update
    ex de,hl
    ld h, (ix+score+1)
    ld l, (ix+score)
    add hl,de
    ld (ix+score+1), h                 ; Else update the score
    ld (ix+score), l
    kcall(ShowInfo)
CheckLines:
    xor a
    ld (ix+linesflag),a                ; This holds how many lines you eliminated
    push ix \ pop hl
    push de
        ld de, board+4
        add hl, de
    pop de
    ld b,17
RepScan:
    push hl
    kcall(LD_HL_MHL)
    ld de,0xFFFF                       ; This would indicate a full row
    pcall(cpHLDE)
    jr nz,NextRow                      ; If it wasn't check next row
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
    ldir                               ; Move everything down
    inc (ix+linesflag)                 ; Increase lines eliminated
    inc (ix+lines)                     ; And also the total line counter
    pop hl
    pop de
    pop bc
    jr RepScan
NextRow:
    pop hl
    inc hl
    inc hl
    djnz RepScan
    ld a,(ix+linesflag)
    or a
    jr z,NoScoring                     ; If no lines gotten, no score increase
    push af
    ld b,a
    ld a,(ix+players)
    dec a
    jr z,NoPenLines                    ; If one player, no penalty lines sent
    push bc
    kcall(CheckBar)
    pop bc
    dec b
    jr z,NoPenLines                    ; If only one line eliminated, no penatly lines
    ld a,(ix+declines)                 ; The flag, 1-3 or 2-4 lines to send
    sub b
    neg                                ; Now A = no of penalty lines
    push af
    or 0xF0
    kcall(SendByte)                   ; Send it over to the opponent
    pop af
    add a,(ix+lastbar)                 ; Increase the opponents bar
    inc a
    kcall(ShowBar)                     ; And show it
NoPenLines:
    pop af
    kld(hl,Scoring)
    ld d,0
    ld e,a
    dec e
    add hl,de
    ld h,(hl)                          ; H = score for level 0
    ld a,(ix+level)
    inc a
    ld l,a
    push de
        ld e, l
        pcall(mul8By8)                 ; Multiply with (level+1)
    pop de
    ex de,hl
    push ix \ pop hl
    push de
        ld de, score
        add hl, de
    pop de
    push hl
    kcall(LD_HL_MHL)
    add hl,de                          ; Add with total score so far
    ex de,hl
    pop hl
    ld (hl),e                          ; Store the new score
    inc hl
    ld (hl),d
    kcall(ShowWell)                    ; Update the well
NoScoring:
    ld h,0
    ld a,(ix+lines)
    ld l,a
    push bc
        ld c, 10
        pcall(divHLByC)                ; HL = lines DIV 10
    pop bc
    ld a,(ix+level)
    cp l                               ; Check if the level should increase
    jr nc,NoNewLevel
    ld a,l
    ld (ix+level),a                    ; Update the levle
    kcall(PastePattern)
    kcall(ShowPattern)
    ld a,(ix+players)
    dec a
    jr z,NoNewLevel
    ld a,(ix+lastbar)
    kcall(ShowBar)
NoNewLevel:

    kcall(ShowInfo)
CreateNew:
    kcall(CreateNewPiece)              ; Randomize new piece
    xor a
    ld (ix+cRot),a
    ld (ix+newRot),a
    ld b,0
    kcall(Uncrunch)                    ; Uncrunch the piece
    ld hl,0x0610
    ld (ix+cXY+1), h
    ld (ix+cXY), l
    ld (ix+newXY+1), h
    ld (ix+newXY), l
    kcall(TestNewB)                    ; Check if it's possible to put out the piece
    jr z,NotDead
    set 0,(ix+flags)                   ; If not, set dead flag
NotDead:
    kjp(ShowCurB)                      ; Show the current piece
ShowInfo:                              ; Updates score, level and lives
    ; Blank areas before drawing text
    ld bc, 5 << 8 | 20
    ld e, 12
    ld l, 8
    pcall(rectAND)
    ld bc, 5 << 8 | 8
    ld e, 24
    ld l, 26
    pcall(rectAND)
    ld bc, 5 << 8 | 12
    ld e, 20
    ld l, 44
    pcall(rectAND)
    ; Print score
    push ix \ pop hl
    ld de, score
    add hl, de
    ld de, 12 << 8 | 8
    kcall(LD_HL_MHL)
    ld b,5
    kcall(DM_HL_DECI3)
    ; Print level
    ld de, 24 << 8 | 26
    ld l,(ix+level)
    ld h,0
    ld b,2
    kcall(DM_HL_DECI3)
    ; Print lines
    ld de, 20 << 8 | 44
    ld l,(ix+lines)
    ld h,0
    ld b,3
    kcall(DM_HL_DECI3)
    ret
CreateNewPiece:
    push ix \ pop hl                   ; Remove the next piece
    ld de, cB+16
    add hl, de
    ld de,0x1403
    kcall(EraseB)
RandP:
    ld a,(ix+next)
    ld (ix+cBit),a
    ld h, (ix+cNBitOfs+1)              ; Make it the current piece instead
    ld l, (ix+cNBitOfs)
    ld (ix+cBitOfs),l
    ld (ix+cBitOfs+1),h
    ld a,7
    kcall(PRandom)                     ; Get a random number between 0-6
    inc a                              ; Increase with 1 to get between 1-7
    ld (ix+next),a
    add a,a
    add a,a
    add a,a
    kld(hl,BitData-8)
    ld d,0
    ld e,a
    add hl,de                          ; Find out where the bit structure is
    ld (ix+cNBitOfs+1), h
    ld (ix+cNBitOfs), l
    ld b,16
    xor a
    kcall(Uncrunch2)
    push ix \ pop hl                   ; Remove the next piece
    ld de, cB+16
    add hl, de
    ld de,0x1403
    kjp(ShowB)
Uncrunch:                              ; Extracts the piece from compressed data
    ld l,(ix+cBitOfs)
    ld h,(ix+cBitOfs+1)
Uncrunch2:
    and 0b00000011
    ld d,0
    ld e,a
    sla e
    add hl,de
    push hl
    push ix \ pop hl
    ld de, cB
    add hl, de
    ld d,0
    ld e,b
    add hl,de
    ex de,hl
    pop hl
    kcall(LD_HL_MHL)
    ld b,8
URep:
    ld a,l
    and 0b00000011
    ld (de),a
    inc de
    rr h
    rr l
    rr h
    rr l
    djnz URep
    ret

PutCoord:                              ; Stores a block in the wello
    push hl
    kcall(GetBoardOfs)
    or (hl)
    ld (hl),a
    pop hl
    ret

GetCoord:                              ; Finds out if there is a block in the well at H,L
    push hl
    kcall(GetBoardOfs)
    and (hl)
    pop hl
    ret

GetBoardOfs:                           ; Convert location H,L to an address, HL
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
    push ix \ pop hl
    push de
        ld de, board
        add hl, de
    pop de
    ld b,0
    add hl,bc
    pop bc
    ret

PutBlock:                              ; Put block at H,L
    push bc
    push de
    push hl
    kcall(GetBlockOfs)
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

EraseBlock:                            ; Erase a block at H,L
    push bc
    push de
    push hl
    kcall(GetBlockOfs)
    xor 0xFF
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

GetBlockOfs:                           ; Finds out where on the screen H,L is
    ld c,h
    ld h,0
    ld a,17
    sub l
    kjp(m, OffScreen)
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
    push iy \ pop de
    add hl,de
    ld b,4
    ld de,12
    ld a,0xF0
    bit 0,c
    ret z
    ld a,0x0F
    ret

OffScreen:
     xor a
     ret

; KnightOS TODO:
; Replace with better RNG
PRandom:                               ; Creates a pseudorandom number 0 <= x < A
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

ShowLayout:                            ; Shows the game layout
    push de
        ld de, 12 << 8 | 1
        kld(hl,InfoText1)
        pcall(drawStr)
        ld de, 12 << 8 | 19
        kld(hl,InfoText2)
        pcall(drawStr)
        ld de, 12 << 8 | 37
        kld(hl,InfoText3)
        pcall(drawStr)
    pop de
GFXNewRow:
    ld de,12
    ld b,64
    push iy
DWNextRow:
        ld (iy+5),0x10
        ld (iy+10),0x08
        add iy,de
        djnz DWNextRow
    pop iy

ShowPattern:                           ; Show pattern for the current level
    kld(hl,Pattern)
    ld a,(ix+level)
    cp 16
    jr c,Below16
    ld a,15                            ; If level>15, show pattern for level 15
Below16:
    add a,a
    add a,a
    add a,a
    ld d,0
    ld e,a
    add hl,de
    push iy
        push hl \ pop iy
        push hl
            push ix \ pop hl
            ld de, APD_BUF
            add hl, de
            ex de, hl
        pop hl
        ld b,8
SP_Row:
        push bc
            push iy \ pop hl
            ld b,8
SP_Line:
            ld a,(hl)
            inc hl
            push bc
                ld b,12                    ; was 16
SP_Byte:
                ld (de),a
                inc de
                djnz SP_Byte
            pop bc
            djnz SP_Line
        pop bc
        djnz SP_Row
        kld(iy,Gaps)
        ld b,(iy-1)
MakeGap:
        push bc
            ld h,(iy+1)
            ld l,(iy)
            push hl
                push ix \ pop hl
                ld de, APD_BUF
                add hl, de
                ex de, hl
            pop hl
            add hl,de
            ld a,(iy+2)
            ld b,(iy+3)
MK_Row:
            push bc
            push hl
                ld d,a
                ld b,(iy+4)
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
            ld de,12                   ; was 16
            add hl,de
            djnz MK_Row
            ld de,5
            add iy,de
        pop bc
        djnz MakeGap
    pop iy

PastePattern:                          ; XOR the pattern on the screen
    push iy
        ld hl,768
        push hl
            push ix \ pop hl
            ld de, APD_BUF
            add hl, de
            ex de, hl
        pop hl
PasteIt:
        ld a,(de)
        xor (iy)
        ld (iy),a
        inc de
        dec hl
        inc iy
        ld a,h
        or l
        jr nz,PasteIt
    pop iy
    ret

ShowWell:                              ; Show the whole well
    ld hl,0x0302
SWNewRow:
    ld b,10
RepPut:
    kcall(GetCoord)
    push af
    kcall(z,EraseBlock)
    pop af
    kcall(nz,PutBlock)
    inc h
    djnz RepPut
    ld h,0x03
    inc l
    ld a,l
    cp 0x12
    jr nz,SWNewRow
    ret

ShowFrame:                             ; Clears the screen and shows some info
    kld(hl,Title)
    xor a                              ; Draw castle and threadlist icons
    corelib(drawWindow)
    ret
; KnightOS TODO:
; This function can probably be removed
Quit:
    ;set 6,(iy+9)                      ; Restore the StatVars, to avoid screen garbage
    ;set 1,(iy+13)                     ; Same with TextShadow
    ;set 2,(iy+8)                      ; Enable automatic powerdown
;Quitter:
    ;pcall(_cleargbuf)
    ret

; Draws a box with some text in it
textBox:

DM_HL_DECI3:                           ; Display HL in menu style with leading zeros
    push de
        push hl
            push ix \ pop hl
            push de
                ld de, string+5
                add hl, de
            pop de
            ex de, hl
        pop hl
        xor a
        ld (de),a
RepUnp:
        dec de
        push bc
            ld c, 10
            pcall(divHLByC)
        pop bc
        add a,48
        ld (de),a
        djnz RepUnp
        ex de,hl
    pop de
    pcall(drawStr)
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

BitData:                             ; Compressed data of the pieces (28 pieces)
    .dw 0b0001010110011000,0b0110010100000100
    .dw 0b0010000101011001,0b0110101001010100
    .dw 0b0001010110010000,0b0010011001010100
    .dw 0b0001010110011010,0b0110010101001000
    .dw 0b0101100101001000,0b0101100101001000
    .dw 0b0101100101001000,0b0101100101001000
    .dw 0b0001010110010100,0b0001011001010100
    .dw 0b0001011001011001,0b0110010101001001
    .dw 0b0000010101001001,0b0010000101010100
    .dw 0b0000010101001001,0b0010000101010100
    .dw 0b0001010101001000,0b0001000001100101
    .dw 0b0001010101001000,0b0001000001100101
    .dw 0b0001010110011101,0b0111011001010100
    .dw 0b0001010110011101,0b0111011001010100

Scoring:                             ; Score for each level
    .db 4,10,30,120

LevelCnts:
    .db 40,36,32,28,25,22,19,17,15,13,11,10, 9, 8, 7, 6, 5, 4, 3, 2, 1

hiscoreDir:
    .db "/var/ztetris",0
hiscorePath:
    .db "/var/ztetris/hiscore",0

Hiscore:
    .db "1. ----------",0,0,0
    .db "2. ----------",0,0,0
    .db "3. ----------",0,0,0

Title:
    .db "ZTetris v1.2",0

Coder:
    .db "by: Sam H/Jimmy M/Pat D/AE",0

PlChoose:
    .db "Choose player mode",0
PlChoose1:
    .db "1 player",0
PlChoose2:
    .db "2 players",0

GameOverText:
    .db "Game Over",0

WinTxt:
    .db "You Win",0

PauseTxt:
    .db "\nPaused",0

WaitTxt:
    .db "Waiting...",0

pauseOptions:
    .db 2, "Continue", 0, "Quit", 0

EnterTxt:
    .db "You have a hiscore!",0
    ;.db "Enter your name:",0

levelTxt:
    .db "Level",0

HighTxt:
    .db "Height",0

hiScoresTxt:
    .db "High Scores",0

SLTxt:
    .db "Send 2-4 lines",0
NLTxt:
    .db "2-4",0
    .db "1-3",0

ScrambleTxt:
    .db "unscrambled",0
    .db "scrambled",0

InfoText1:
    .db "Score",0
InfoText2:
    .db "Level",0
InfoText3:
    .db "Lines",0
Pattern:                             ; Pattern for each level
    .db 0xAA,0x55,0xAA,0x55,0xAA,0x55,0xAA,0x55
    .db 0x88,0xFF,0x22,0xFF,0x88,0xFF,0x22,0xFF
    .db 0xFF,0x99,0x99,0xFF,0xFF,0x99,0x99,0xFF
    .db 0x50,0xD7,0x14,0xF7,0x00,0xF7,0x14,0xD7
    .db 0xEA,0xAA,0xAE,0x00,0x57,0x55,0x75,0x00
    .db 0xFE,0xAA,0xAA,0xAA,0x28,0xAA,0xFE,0x00
    .db 0x55,0x55,0xAA,0xAA,0x55,0x55,0xAA,0xAA
    .db 0x80,0xFE,0x02,0xFB,0x08,0xEF,0x20,0xBF
    .db 0xEE,0xEE,0xEE,0x00,0x77,0x77,0x77,0x00
    .db 0x66,0xCC,0x99,0x33,0x66,0xCC,0x99,0x33
    .db 0xCC,0x33,0xCC,0x33,0xCC,0x33,0xCC,0x33
    .db 0xFE,0x82,0xBA,0xAA,0xBA,0x82,0xFE,0x00
    .db 0xCC,0xCC,0x33,0x33,0xCC,0xCC,0x33,0x33
    .db 0xFF,0xAA,0xFF,0xAA,0xFF,0xAA,0xFF,0xAA
    .db 0x7C,0xFE,0x7C,0x00,0x7C,0xFE,0x7C,0x00
    .db 0xFF,0xEF,0x47,0x12,0xB8,0xFD,0xFF,0xFF

    .db 5
Gaps:                                ; Gaps where the pattern shouldn't be shown
    .dw 0x005 \ .db 0xEF,64,42         ;well
    .dw 0x001 \ .db 0xEF,14,23         ;score
    .dw 0x0D9 \ .db 0xEF,14,23         ;level
    .dw 0x1B1 \ .db 0xEF,14,23         ;Lines
    .dw 0x271 \ .db 0xEF,10,23         ;Next piece

#include "link.asm"
