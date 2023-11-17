; CBMII Test TOD-Error with CMOS CPU
;	
!cpu 6502
!ct scr		; standard text/char conversion table -> Screencode (pet = PETSCII, raw)
!to "t0400.prg", cbm
; ***************************************** CONSTANTS *********************************************
CODESTART		= $0400
SYSTEMBANK		= $f		; #SYSTEMBANK
; CIA register
TOD10			= $8		; tod 10th of seconds
TODSEC			= $9		; tod seconds
TODMIN			= $a		; tod monutes
TODHR			= $b		; tod hours
; ***************************************** ADDRESSES *********************************************
!addr CodeBank		= $00		; code bank register
!addr IndirectBank	= $01		; indirect bank register
!addr ScreenRAM		= $d000		; Screen RAM
!addr CIA		= $dc00		; CIA address	
; ***************************************** ZERO PAGE *********************************************
; io-pointer
!addr screen		= $f0		; 16 pointer screen
!addr cia_10th		= $f2		; 16 pointer CIA TOD 10th
!addr cia_sec		= $f4		; 16 pointer CIA TOD sec
!addr cia_min		= $f6		; 16 pointer CIA TOD min
!addr cia_hr		= $f8		; 16 pointer CIA TOD hr
; varibales
!addr pointer1		= $fa		; 16bit pointer
!addr pointer2		= $fc		; 16bit pointer
!addr temp1		= $fe		; temp
; ******************************************* CODE ************************************************
!zone code
*= CODESTART
	sei				; disable interrupts
	cld
	ldx #$ff
	txs				; init stack
	jsr InitIOPointer		; init io pointer
; TOD tests
TODTest:
	lda #SYSTEMBANK
	sta IndirectBank		; systembank for cia
; set TOD
	sed				; decimal mode
	ldy #$00
	lda #$01
	sta (cia_hr),y		; set TOD starting with hours (halts TOD) to time1
	lda #$00
	sta (cia_min),y
	lda #$01
	sta (cia_sec),y
	lda #$09
	sta (cia_10th),y		; set TOD 10th (starts TOD)
; print start seconds
	lda (cia_sec),y		; load start seconds
	jsr Hex2Screencode		; calc screen code for a byte
	sty temp1
	ldy #0				; column
	sta (screen),y			; print to screen
	iny
	lda temp1
	sta (screen),y
	ldy #$00
; wait for change of 10th
chglp:	lda (cia_10th),y
	cmp #$00
	bne chglp
; print new seconds
	lda (cia_sec),y		; load new seconds
	jsr Hex2Screencode		; calc screen code for a byte
	sty temp1
	ldy #3				; column
	sta (screen),y			; print to screen
	iny
	lda temp1
	sta (screen),y
	cld				; reset decimal flag
	brk
; ----------------------------------------------------------------------------
; calc screen codes for a byte and return in a and y
Hex2Screencode:
	pha				; remember value on stack
	jsr Nibble2Screencode		; calc low nibble
	tay				; remember lower digit in Y
	pla
	lsr				; upper nibble -> lower nibble
	lsr
	lsr
	lsr
; calc screen code dec / hex
Nibble2Screencode:
	and #$0f			; isolate low nibble
	cmp #$0a
	bmi scdec			; skip if 0-9
	sec
	sbc #$09			; calc screencode A-F -> 01-06
	bne scend			; jump always
scdec:	ora #$30			; calc screencode 0-9 -> 30-39
scend:	rts
; ----------------------------------------------------------------------------
; init io pointer
InitIOPointer:
	lda #<screen			; zp io pointer address
	sta pointer1
	lda #$00
	sta pointer1+1
	lda #<ioregs
	sta pointer2
	lda #>ioregs
	sta pointer2+1
	ldy #$09
	lda CodeBank
	sta IndirectBank
cpptlp:	lda (pointer2),y
	sta (pointer1),y
	dey
	bpl cpptlp
	rts
; ----------------------------------------------------------------------------
ioregs:	!byte <(ScreenRAM+80*10), >(ScreenRAM+80*10)			; screen
	!byte <(CIA+TOD10), >(CIA+TOD10), <(CIA+TODSEC), >(CIA+TODSEC)	; cia register
	!byte <(CIA+TODMIN), >(CIA+TODMIN), <(CIA+TODHR), >(CIA+TODHR)
