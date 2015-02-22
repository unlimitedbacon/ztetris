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
 ld a,$D0
 out (0),a
rb_w_Start:
 in a,(0)
 and $C
 cp $C				; wait one line becomes low
 jr nz,rb_get_bit
 call Test_ON
 jr rb_w_Start
rb_get_bit:
 ld l,0   		; reset lcounter
 cp 8			; is it a 0 ?
 jr z,rb_receive_zero	; Yeah !
 ld a,$D1
 out (0),a		; Ack for a '1'
 rr c			; put the 1 (cf has been set by cp)
rb_waitstop1:
 call Test_On
 in a, (0)
 and $8			; wait 'til Ack is recognized
 jr z, rb_waitstop1
 jr rb_stopok

rb_receive_zero:
 ld a,$d2		; Ack for a '0'
 out (0),a
 rr c			; put the 0 (cf has been cleared by cp)
rb_waitStop0:
 call Test_On
 in a,(0)
 and $4			; wait 'til Ack is recognized
 jr z,rb_waitStop0
rb_stopok:              ; here, one line is low cause of us(recv)
 ld a,$d0               ; the sender calc has already set lines to hi
 out (0),a
 djnz rb_w_Start
 ld a,c
 ret

Test_ON:
 dec l
 ret nz  ; exit only if cnt = 0
 pop hl  ;Back to the place you were before.	Gotta love it!
 ld a, $d0
 out (0), a
 xor a
 ret

SendByte:
 ld hl,0
 ld (lcounter),hl
 ld b,8
 ld c,a			; byte to send
 ld d, a			; Save it
 ld a,$D0
 out (0),a
w_setport3:
 in a,(0)
 and $C
 cp $C				; wait both lines are low
 jr z, calc_bit
 call SendTest_ON
 jr w_setport3
calc_bit:
 rr c
 ld a, $d2
 jr c,send_one
send_zero:
 dec a
send_one:
 out (0),A
wait_setport:
 call SendTest_ON
 in a,(0)
 and $C			; wait both become low
 jr nz,wait_setport
 ld a,$D0			; put them back to hi
 out (0),A
 djnz w_setport3
 ld a,d			; Restore byte
 ret

SendTest_ON:
 ld a,(hsflag)
 or a
 jr z,LongWait
 bcall(_getcsc)
 cp $0f			; If this is clear, exit !
 ret nz
 pop hl
 pop hl
 jp Quit
LongWait:
 ld hl,(lcounter)
 inc hl
 ld (lcounter),hl
 bit 6, h		; Has the counter reached $4000
 ret z			; no, continue
 pop hl
 ret


