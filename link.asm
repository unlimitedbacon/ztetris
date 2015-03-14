;(Just a note)
;(The 83+ link port works very simply (the rest should be like this))
;(Line 0 is bit 0)
;(Line 1 is bit 1)
;(It works the same for input and output)
;
; SOME VERY NICE LINK ROUTINES MADE BY PASCAL BOURON
;
; I've made some modifications so it fits ZTetris
;
; Ported to TI83 and highly optimized by Florent Dhordain 06/04/98 MMDDYY
;


ReceiveByte:
 ld l,0	; l used as lcounter
 ld b,8			 ; 8 bits to receive
; ld a,$D0		; (reset the port to high both lines)
 ld a,0
 out (0),a
rb_w_Start:
 in a,(0)
; and $C
; cp $C				; wait one line becomes low
 and	3			; (check if either line is low)
 cp	3
 jr nz,rb_get_bit
 kcall(Test_ON)
 jr rb_w_Start
rb_get_bit:
 ld l,0   		; reset lcounter
; cp 8			; is it a 0 ? (is line1 low?)
 cp 2
 jr z,rb_receive_zero	; Yeah !
; ld a,$D1
 ld	a,1
 out (0),a		; Ack for a '1'
 rr c			; put the 1 (cf has been set by cp)
rb_waitstop1:
 kcall(Test_On)
 in a, (0)
; and $8			; wait 'til Ack is recognized
 and 2				; (line1 pulled low?)
 jr z, rb_waitstop1
 jr rb_stopok

rb_receive_zero:
; ld a,$d2		; Ack for a '0'
 ld a,2
 out (0),a		; (pull line1 low)
 rr c			; put the 0 (cf has been cleared by cp)
rb_waitStop0:
 kcall(Test_On)
 in a,(0)
; and $4			; wait 'til Ack is recognized
 and 1				; (is line0 low (the ack))
 jr z,rb_waitStop0
rb_stopok:              ; here, one line is low cause of us(recv)
; ld a,$d0               ; the sender calc has already set lines to hi
 ld a,0			; (reset port to lines high)
 out (0),a
 djnz rb_w_Start
 ld a,c
 ret

Test_ON:
 dec l
 ret nz  ; exit only if cnt = 0
 pop hl  ;Back to the place you were before.	Gotta love it!
; ld a, $d0
 ld a,0
 out (0), a
 xor a
 ret

SendByte:
 ld hl,0
 ld (ix + lcounter), l
 ld (ix + lcounter +1), h
 ld b,8
 ld c,a			; byte to send (using bit rotates into carry to find bit to send)
 ld d, a			; Save it
; ld a,$D0
 ld a,0			; (reset port to all high)
 out (0),a
w_setport3:
 in a,(0)
; and $C
; cp $C				; wait both lines are low
 and 3				; (are either line low?)
 cp 3
 jr z, calc_bit
 kcall(SendTest_ON)
 jr w_setport3
calc_bit:
 rr c				; (is the next bit to send a 1 or 0?)
; ld a, $d2
 ld a,2
 jr c,send_one			; (if its a 1, goto send_one)
send_zero:
 dec a
send_one:
 out (0),A
wait_setport:
 kcall(SendTest_ON)
 in a,(0)
; and $C			; wait both become low
 and 3				; (are both lines low?)
 jr nz,wait_setport
; ld a,$D0			; put them back to hi
 ld a,0				; (reset both lines to high)
 out (0),A
 djnz w_setport3
 ld a,d			; Restore byte
 ret

SendTest_ON:
 ld a,(hsflag)
 or a
 jr z,LongWait
; bcall(_getk)	; dunno, I don't have a getk equate
; cp $09
 pcall(getKey)
 cp 0x0f			; If this is clear, exit !
 ret nz
 pop hl
 pop hl
 kjp(Quit)
LongWait:
 ld l, (ix + lcounter)
 ld h, (ix + lcounter + 1)
 inc hl
 ld (ix + lcounter), l
 ld (ix + lcounter + 1), h
 bit 6, h		; Has the counter reached $4000
 ret z			; no, continue
 pop hl
 ret


