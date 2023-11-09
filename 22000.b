; CBMII b128-256 burnin 
; disassembled by vossi 10/2023
;
; NOTE: ROM Checksum routine is faulty:
;	with not 256kB it loads wrong ROM start addresses from ZP instead immediate E0, A0, 80 
;
!cpu 6502
!ct scr		; standard text/char conversion table -> Screencode (pet = PETSCII, raw)
!to "load 1.prg", cbm
; FIX_ROMCHECKSUMS = 1		; fixes ROM Checksums for non 256kB machines
; FIX_CIATNT = 1		; fixes TNT text in CIA if tod and timer test failed
; ***************************************** CONSTANTS *********************************************
CODESTART		= $2000		; code start
CODEEND			= $3347		; code end for program checksum
SYSTEMBANK		= $f		; #SYSTEMBANK
FILL			= $aa
; TPI register
PC			= $2 *2		; port c
MIR			= $5 *2		; interrupt mask register
CR			= $6 *2		; control register
; CIA register
TALO			= $4 *2		; timer a lo
TAHI			= $5 *2		; timer a hi
TBLO			= $6 *2		; timer b lo
TBHI			= $7 *2		; timer b hi
TOD10			= $8 *2		; tod 10th of seconds
TODSEC			= $9 *2		; tod seconds
TODMIN			= $a *2		; tod monutes
TODHR			= $b *2		; tod hours
ICR			= $d *2		; interrupt control register
CRA			= $e *2		; control register b
CRB			= $f *2		; control register b
; SID register
OSC1			= $00 *2	; oscillator 1
OSC3			= $0e *2	; oscillator 2
FREQHI			= $01 *2	; frequency hi
OSCCTL			= $04 *2	; oscillator control
ATKDCY			= $05 *2	; attack/decay
SUSREL			= $06 *2	; sustain/release
VOLUME			= $18 *2	; volume
; ***************************************** ADDRESSES *********************************************
!addr CodeBank		= $00		; code bank register
!addr IndirectBank	= $01		; indirect bank register
!addr ScreenRAM		= $d000		; Screen RAM
!addr CRT		= $d800		; CRT
!addr HW_NMI		= $fffa		; system NMI vector
!addr HW_RESET		= $fffc		; system RESET vector
!addr HW_IRQ		= $fffe		; system IRQ vector
; ***************************************** ZERO PAGE *********************************************
!addr pointer1		= $12		; 16bit pointer
!addr tod_count1	= $12		; tod test counter
!addr tod_count2	= $13		; tod test counter
!addr screen_pointer	= $14		; 16bit pointer screen
!addr tod_state		= $16		; TOD state - $00=ok, $ff = bad
!addr timer_state	= $16		; timer state - $00 = ok
!addr cia_tod_fail	= $1b		; 0 = TOD ok, $ff = tod failed
!addr cia_tmr_fail	= $1c		; 0 = timer ok, $ff = timer failed
!addr temp_checksum1	= $1d		; temp test program checksum
!addr temp_checksum2	= $1e		; temp test program checksum
!addr actual_codebank	= $20		; actual code bank
!addr cycles		= $21		; 4 bytes cycle counter
!addr temp_and_value	= $2a		; temp screen data and value
!addr banks		= $2f		; RAM banks
!addr pointer2		= $35		; 16bit pointer
!addr screen_pos	= $39		; add value screen position rom checksum
!addr temp_irq		= $39		; temp irq timer tests
!addr temp3		= $3b		; temp timer tests
!addr time2_hours	= $42		; time 1 hours
!addr time2_minutes	= $43		; time 1 minutes
!addr time2_seconds	= $44		; time 1 seconds
!addr time2_10th	= $45		; time 1 10th seconds
!addr time1_hours	= $46		; time 1 hours
!addr time1_minutes	= $47		; time 1 minutes
!addr time1_seconds	= $48		; time 1 seconds
!addr time1_10th	= $49		; time 1 10th seconds
!addr temp2		= $4a		; temp
!addr tod_count3	= $4b		; tod test counter
!addr temp_count_sum	= $4c		; temp/counter
!addr temp1		= $4d		; temp
!addr screendata_pointer= $37		; 16bit pointer screen data
!addr temp_dec_value	= $55		; temp dec value
!addr temp_or_value	= $55		; temp screen data or value
!addr actual_indirbank	= $58		; actual indirect bank
!addr pointer3		= $5b		; 16bit pointer
!addr pointer4		= $5d		; 16bit pointer
!addr crtadr		= $9f		; pointer crt address register 
!addr crtdata		= $a1		; pointer crt data register 
; io-pointer multi-usage!
!addr tpi1		= $5f		; 8 pointer to TPI1 regs
!addr sid		= $5f		; 29 pointer to SID regs
!addr tpi2		= $6f		; 8 pointer to TPI2 regs
!addr cia		= $7f		; 16 pointer to CIA regs
!addr acia		= $a3		; 4 pointer to ACIA regs
!addr nmi_pointer	= $f0		; nmi pointer
!addr reset_pointer	= $f2		; reset pointer
!addr irq_pointer	= $f4		; irq pointer
; ******************************************* CODE ************************************************
*= CODESTART
	sei
	cld
	ldx #$ff
	txs
; clear zero page
	ldy #$02
	lda #$00
clrzplp:sta $0000,y
	iny
	bne clrzplp
; check if 128k or 256k machine
	ldx #$02
	stx banks			; default 2 banks
	inx
	stx IndirectBank		; set bank 3 as indirect bank
	lda #$60
	sta pointer1+1			; check from $6000
	lda #$a5
	sta temp2			; remember test byte $a5
chkbklp:sta (pointer1),y
	lda (pointer1),y
	cmp temp2			; check if RAM here
	beq cbm256			; RAM in bank 3
	iny
	bne chkbklp			; check next byte
	beq drawscr			; no RAM from $6000 to $60ff
cbm256:	lda #$04
	sta banks			; store 4 banks
; draw chips
drawscr:jsr ClearScreen			; sub: clear screen
	ldx #0
	jsr SetChipAddress		; sub: set pointer2 to chip graphics address
	lda #$f0
	ldy #$d0
	jsr DrawChips			; sub: draw chip graphics to screen
	lda #$20
	ldy #$d3
	jsr DrawChips			; sub: draw chip graphics to screen
	lda #$50
	ldy #$d5
	jsr DrawChips			; sub: draw chip graphics to screen
	ldx #3
	jsr SetChipAddress		; sub: set pointer2 to chip graphics address
	lda #$30
	ldy #$d2
	jsr DrawChips			; sub: draw chip graphics to screen
	lda #$60
	ldy #$d4
	jsr DrawChips			; sub: draw chip graphics to screen
	lda #$90
	ldy #$d6
	jsr DrawChips			; sub: draw chip graphics to screen
	ldx #2
	jsr SetChipAddress		; sub: set pointer2 to chip graphics address
	lda #$e0
	ldy #$d1
	jsr DrawChips			; sub: draw chip graphics to screen
	lda #$10
	ldy #$d4
	jsr DrawChips			; sub: draw chip graphics to screen
	lda #$40
	ldy #$d6
	jsr DrawChips			; sub: draw chip graphics to screen
	ldx #1
	jsr SetChipAddress		; sub: set pointer2 to chip graphics address
	lda #$40
	ldy #$d1
	jsr DrawChips			; sub: draw chip graphics to screen
	lda #$70
	ldy #$d3
	jsr DrawChips			; sub: draw chip graphics to screen
	lda #$a0
	ldy #$d5
	jsr DrawChips			; sub: draw chip graphics to screen
; draw screen text
	ldx #$00
	jsr DrawText			; sub: draw screen text
	ldx #$01
	jsr DrawText			; sub: draw screen text
	ldx #$02
	jsr DrawText			; sub: draw screen text
	ldx #$2b
	jsr DrawText			; sub: draw screen text
	jsr InitTPI2Pointer		; sub: init tpi2 pointer
	lda #SYSTEMBANK
	sta IndirectBank		; systembank for IO
	ldy #$00
	lda (tpi2+CR),y
	and #$fe			; tpi2 mc=0 disable interrupt controller
	sta (tpi2+CR),y
	lda #$00
	sta (tpi2+MIR),y		; tpi2 mir=0 mask all interrrupts
	lda (tpi2+PC),y			; read tpi2 port c
	and #$80			; isolate bit#7 low/high profile
	bne hiprof			; branch if high profile
; low profile
	ldx #$0b
	stx temp_count_sum
	ldx #$03			; lines *---- ----* and low-ics
	stx temp1
	bne drwiclp			; jump always
; high profile
hiprof:	ldx #$2f
	jsr DrawText			; sub: draw screen text "HIGH"
	ldx #$08
	stx temp_count_sum
	ldx #$06			; lines *---- ----* 
	stx temp1
-	ldx temp1
	jsr DrawTextPure		; sub: draw without and
	inc temp1
	dec temp_count_sum
	bne -
	ldx #$03
	stx temp_count_sum
	ldx #$30			; high-ics
	stx temp1
; draw lines+ics low / ics only high
drwiclp:ldx temp1
	jsr DrawTextPure
	inc temp1
	dec temp_count_sum
	bne drwiclp
; draw segment, execute, test
	ldx #$14
	stx temp_count_sum
	ldx #$17			; segment, execute, test
	stx temp1
-	ldx temp1
	jsr DrawText			; sub: draw screen text
	inc temp1
	dec temp_count_sum
	bne -
; draw multiple chars to screen positions from table
	ldx #$0c			; char to draw
	stx temp2
--	lda pos_count,x			; positions to draw
	sta temp_count_sum
	lda char,x
	and #$3f
	sta temp1
-	ldx temp2
	lda screenposlo_lo,x		; load position table lo pointer
	sta pointer3
	lda screenposlo_hi,x
	sta pointer3+1
	lda screenposhi_lo,x		; load position table hi pointer
	sta pointer4
	lda screenposhi_hi,x
	sta pointer4+1
	lda CodeBank
	sta IndirectBank
	ldy temp_count_sum
	lda (pointer3),y		; load screen postion
	sta pointer1
	lda (pointer4),y
	sta pointer1+1
	lda #SYSTEMBANK
	sta IndirectBank		; systembank for Screen
	lda temp1
	ldy #$00
	sta (pointer1),y		; draw char to screen
	ldx temp_count_sum
	dex
	stx temp_count_sum
	bpl -				; next postion
	ldx temp2
	dex
	stx temp2
	bpl --				; next char
;
	lda banks
	cmp #$04
	beq +				; skip for 256kB
	ldx #$38			; NO RAM
	jsr DrawText			; sub: draw screen text
	ldx #$37			; NO RAM
	jsr DrawText			; sub: draw screen text
+	jmp Main
; ----------------------------------------------------------------------------
; draw chip graphics to screen
DrawChips:
	sta pointer1
	sty pointer1+1
	ldx CodeBank
	stx actual_codebank		; remember code bank
	ldx #$10
	stx $1f
	txa
	jsr mul5			; sub a x5
	sec
	sbc #$01
	sta $3e
drawlp2:ldy #$04
drawlp1:lda actual_codebank
	sta IndirectBank		; set indirect bank to actual codebank
	lda (pointer2),y
	dey
	sty $3d
	ldy $3e
	ldx #SYSTEMBANK
	stx IndirectBank		; systembank for screen
	sta (pointer1),y
	dey
	sty $3e
	ldy $3d
	bpl drawlp1
	ldx $1f
	dex
	stx $1f
	bne drawlp2
	rts
; ----------------------------------------------------------------------------
; set pointer2 to chip graphics address
SetChipAddress:
	lda chip_data_lo,x
	sta pointer2
	lda chip_data_hi,x
	sta pointer2+1
	rts
; ----------------------------------------------------------------------------
; Clear screen
ClearScreen:
	lda #>ScreenRAM
	sta pointer1+1
	ldy #<ScreenRAM
	sty pointer1
	lda #SYSTEMBANK
	sta IndirectBank		; systembank for screen
	lda #' '			; space
	ldx #$08			; 8 pages
clrsclp:sta (pointer1),y
	iny
	bne clrsclp
	inc pointer1+1
	dex
	bne clrsclp
	rts
; ----------------------------------------------------------------------------
; main test program loop
Main:	
	jsr InitSIDPointer		; sub: init sid pointer
	jsr InitCRTPointer		; sub: init crt pointer
	lda #SYSTEMBANK
	sta IndirectBank		; systembank
	jsr PlaySound			; sub: play ping sound
	jsr ProgramChecksum		; calc program checksum (F5)
	jsr ROMChecksums		; calc and print rom checksums
	jsr DummySub
	jsr DummySub
	jsr DummySub
	jsr DummySub
	jsr DummySub
	jsr DummySub
	jsr DummySub
	jsr DummySub
	jsr DummySub
	jsr DummySub
	jsr DummySub
	jsr DummySub
	jsr DummySub
	jsr DummySub
	jsr TimerTest			; timer tests
	jsr TODTest			; TOD test
	jsr RAMTest			; RAM tests
; increase cycles
	ldx #$03			; four bytes (00000000-99999999)
	sed				; decimal mode
	sec				; increase viy carry flag
inclp:	lda cycles,x
	adc #$00			; add carry
	sta cycles,x
	bcc prtcycl			; all bytes done
	dex
	bpl inclp			; next byte (00-99)
; prepare print cycles
prtcycl:lda #$2d			; screen position for cycles
	sta pointer1
	lda #$d0
	sta pointer1+1
	lda #SYSTEMBANK
	sta IndirectBank		; systembank for screen
; skip leading zeros
	ldx #$ff
	ldy #$00
bytzero:inx
	cpx #$04			; last byte
	bne loadbyt			; branch if not last byte
	dex
	bne prdiglo			
loadbyt:lda cycles,x			; load byte
	beq bytzero			; skip zero bytes
	and #$f0			; isloate high nible (decimal!)
	beq prdiglo			; skip high nible printing if zero
; print digits
prdighi:jsr UpperNibble2Screencode	; calc screen code for high nibble
	sta (pointer1),y		; print low nible digit to screen position
	iny
prdiglo:lda cycles,x			; load digit
	jsr Nibble2Screencode		; calc screen code for low nibble
	sta (pointer1),y		; print digit to screen position
	iny
	inx
	lda cycles,x			; load next digit
	cpx #$04			; check for last digit
	bne prdighi			; next digit
	cld				; disable decimal mode
	jmp Main			; start over
; ----------------------------------------------------------------------------
; delay subroutine
DummySub:
	rts				; delay only
; ----------------------------------------------------------------------------
; 
l225a:	lda IndirectBank
	sta actual_indirbank
	lda #SYSTEMBANK
	sta IndirectBank
	ldy #$00
	lda #$0c
	sta ($9f),y
	lda $3f
	eor #$20
	sta ($a1),y
	sta $3f
	lda actual_indirbank
	sta IndirectBank
	rts
; ----------------------------------------------------------------------------
; play ping sound
PlaySound:
	ldy #$00
	lda IndirectBank
	sta actual_indirbank		; remember bank
	lda #SYSTEMBANK			; 15 also for volume	
	sta IndirectBank		; systembank for SID
	sta (sid+VOLUME),y		; volume 15
	lda #$00
	sta (sid+OSC1+ATKDCY),y
	lda #$fa
	sta (sid+OSC1+SUSREL),y
	lda #$40
	sta (sid+OSC1+FREQHI),y
	lda #$80
	sta (sid+OSC3+FREQHI),y
	lda #$15			; OSC1 on
	sta (sid+OSC1+OSCCTL),y
	lda #$14			; OSC1 off
	sta (sid+OSC1+OSCCTL),y
	lda actual_indirbank
	sta IndirectBank		; restore bank
	rts
; ----------------------------------------------------------------------------
; calc program checksum (F5)
ProgramChecksum:
	ldy #<CODEEND			; programend low
	lda #$00
	sta pointer1
	sta temp2
	lda #>CODEEND			; programend hi
	sta pointer1+1
	sec
	sbc #>CODESTART			; program start
	tax				; x= programsize pages (+y bytes)
	lda CodeBank
	sta IndirectBank		; codebank
cksumlp:lda (pointer1),y		; load code byte
	eor temp2
	sta temp2			; exor byte and save for next byte
	dey
	bne cksumlp			; next byte
	dec pointer1+1
	dex
	bpl cksumlp			; next page
; check if zeropage with checksum is ok
	lda temp_checksum1
	bne +				; not zero from clear zp -> faulty
	dec temp_checksum1
	lda temp2			; checksum
	sta temp_checksum2
+	lda temp2
	cmp temp_checksum2
	beq prntsum			; zeropage ok -> print checksum
	ldx #$2e		
	jsr DrawTextReverse		; draw bad program checksum reverse
; print checksum
prntsum:lda temp2			; checksum
	jsr Hex2Screencode		; calc screen code for a byte
	sty temp2
	ldy #$00
	ldx #$2e
	stx pointer1
	ldx #$d7
	stx pointer1+1			; set screen position
	ldx #SYSTEMBANK
	stx IndirectBank		; systembank for screen
	sta (pointer1),y		; print checksum
	iny
	lda temp2
	sta (pointer1),y
	rts
; ----------------------------------------------------------------------------
; calc and print rom checksums ***** FAULTY *****
; with not 256kB it loads wrong ROM start addresses from ZP instead immediate E0, A0, 80 
ROMChecksums:
	clv				; NO SENSE - first adc changes overflow flag
	lda #SYSTEMBANK
	sta IndirectBank		; systembank
	ldy #$00			; first screen position
	ldx #$20			; rom size 2000
	lda #$e0			; kernal address e000
	jsr prntrom			; calculate and print checksum of one rom
	ldy #$05			; screen pos basic hi
	ldx #$20
	lda banks
	cmp #$04
	beq banks4			; branch 4 ram banks
; ******************** FIX Romchecksum for non-256kB machines ********************
!ifdef FIX_ROMCHECKSUMS{
	lda #$a0			; basic hi address a000
} else{
	lda $a0				; ***** NO SENSE - area of IO pointer !!!
}
	bvc bashi			; ***** NO SENSE - branch dependent of last adc/sbc ? 
banks4:	lda #$a0			; basic hi address a000
bashi:	jsr prntrom			; calculate and print checksum of one rom
	ldy #$0a
	ldx #$20
	lda banks
	cmp #$04
	beq baslo			; branch 4 ram banks
; ******************** FIX Romchecksum for non-256kB machines ********************
!ifdef FIX_ROMCHECKSUMS{
	lda #$80			; basic hi address 8000
} else{
	lda $80				; ***** NO SENSE - area of IO pointer !!!
}
	bvc prntrom			; ***** NO SENSE - branch dependent of last adc/sbc ? 
baslo:	lda #$80			; basic low address 8000
; calculate and print checksum of rom
prntrom:sty screen_pos
	sta pointer1+1			; set pointer hi to rom start
	dex
	txa
	clc
	adc pointer1+1			; calc rom end 
	sta pointer1+1
	lda #$00
	sta temp_count_sum		; init sum
	sta pointer1
	tay
rsumlp:	clc
	lda (pointer1),y		; load byte
	adc temp_count_sum		; add previous value
	adc #$00			; add carry
	adc #$00			; ***** why ?
	sta temp_count_sum		; store new sum
	dey
	bne rsumlp			; next byte
	dec pointer1+1
	dex
	bpl rsumlp			; next page
; print sum
	lda screen_pos			; screen pos add value
	clc
	adc #$a1			; calc screen position lo
	sta pointer3
	lda #$d6
	adc #$00			; add carry of screen pos lo
	sta pointer3+1
	lda temp_count_sum
	jsr Hex2Screencode		; calc screen code for a byte
	sty temp2
	ldy #$00
	sta (pointer3),y		; print to screen
	iny
	lda temp2
	sta (pointer3),y
	rts
; ----------------------------------------------------------------------------
; unused
	lda temp_count_sum
	inc pointer1+1
	cmp pointer1+1
	beq +
	jsr DrawBad			; draw chip "BAD"
+	rts
; ----------------------------------------------------------------------------
; timer tests
TimerTest:
	sei
	ldx #$36			; "6526 TIMER TESTS"
	jsr DrawText			; sub: draw screen text
	jsr InitSystemVectors		; sub: init system hardware vectors
	jsr InitCIAPointer		; sub: init cia pointer
	lda #SYSTEMBANK
	sta IndirectBank
	jsr eciairq
; test timers with IRQ
	ldy #$00
	lda #$00
	sta (cia+CRB),y			; disable timer B
	lda #$08
	sta (cia+CRA),y			; timer A one-shot
	sty timer_state			; clear fault counter
	ldx #$01			; irq-bit timer A
	jsr CheckIRQTimerA		; check timer A IRQ
	beq tok1
	dec timer_state			; dec fault counter
tok1:	jsr CheckIRQ
	bne tok2
	dec timer_state			; dec fault counter
tok2:	ldx #$01
	lda #$00
	sta (cia+CRA),y			; disable timer A
	jsr CheckIRQTimerA		; check timer a no IRQ
	beq tok3
	dec timer_state			; dec fault counter
tok3:	ldx #$01
	jsr CheckIRQ
	beq tok4
	dec timer_state			; dec fault counter
tok4:	lda (cia+CRA),y
	and #$fe
	sta (cia+CRA),y			; disable mtimer A
	lda #$08
	sta (cia+CRB),y			; timer B one shot
	ldx #$02			; irq-bit timer B
	jsr CheckIRQTimerB		; check timer B IRQ
	beq tok5
	dec timer_state			; dec fault counter
; test timer regs + load + load force
tok5:	lda (cia+CRB),y
	and #$fe
	sta (cia+CRB),y			; disable timer B	
	lda #$40			; test timer regs $40 times
	sta temp_dec_value
tireglp:lda #$00
	sta (cia+CRA),y
	sta (cia+CRB),y			; clear and stop all timers
	lda #$55
	sta (cia+TALO),y
	sta (cia+TAHI),y		; load with hi
	lda (cia+TALO),y
	cmp #$55			; check timer A lo reg with $55
	beq tok6
	dec timer_state			; dec fault counter
tok6:	lda (cia+TAHI),y
	cmp #$55			; check timer A hi reg with $55
	beq tok7
	dec timer_state			; dec fault counter
tok7:	lda #$aa
	sta (cia+TAHI),y
	sta (cia+TALO),y		; load with hi only!		
	lda (cia+TALO),y
	cmp #$55			; check timer A still $55 because load only with hi
	beq tok8
	dec timer_state			; dec fault counter
tok8:	lda (cia+TAHI),y
	cmp #$aa			; check timer A hi reg with $aa
	beq tok9
	lda #$ff
	sta timer_state			; set fault counter = $ff
tok9:	lda #$10
	sta (cia+CRA),y			; timer A force load to load lo
	lda (cia+TALO),y
	cmp #$aa			; check timer A lo reg with $aa
	beq tok10
	dec timer_state			; dec fault counter
tok10:	lda (cia+TAHI),y
	cmp #$aa			; check timer A hi reg is still $aa
	beq tok11
	dec timer_state			; dec fault counter
tok11:	lda #$55
	sta (cia+TBLO),y
	sta (cia+TBHI),y		; load with hi
	lda (cia+TBLO),y
	cmp #$55			; check timer B lo reg with $55
	beq tok12
	dec timer_state			; dec fault counter
tok12:	lda (cia+TBHI),y
	cmp #$55			; check timer B hi reg with $55
	beq tok13
	dec timer_state			; dec fault counter
tok13:	lda #$aa
	sta (cia+TBHI),y
	sta (cia+TBLO),y		; load with hi only!
	lda (cia+TBLO),y
	cmp #$55			; check timer B still $55 because load only with hi
	beq tok14
	dec timer_state			; dec fault counter
tok14:	lda (cia+TBHI),y
	cmp #$aa			; check timer B hi reg with $aa
	beq tok15
	dec timer_state			; dec fault counter
tok15:	lda #$10
	sta (cia+CRB),y			; timer B force load to load lo
	lda (cia+TBLO),y
	cmp #$aa			; check timer B hi reg is still $aa
	beq tok16
	dec timer_state			; dec fault counter
tok16:	lda (cia+TBHI),y
	cmp #$aa			; check timer B lo reg with $aa
	beq tok17
	dec timer_state			; dec fault counter
tok17:	lda #$09
	sta (cia+CRA),y			; timer A start with one shot
	lda #$cc
	sta (cia+TALO),y
	sta (cia+TAHI),y		; load $cccc in latch only because timer runs
	lda (cia+TALO),y
	cmp #$aa			; check value not load while timer A runs
	bmi tok18
	dec timer_state			; dec fault counter
tok18:	lda (cia+TAHI),y
	cmp #$aa			; check value not load while timer A runs
	beq tok19
	dec timer_state			; dec fault counter
tok19:	lda #$19
	ldx #$00
	sta (cia+CRA),y			; timer A start with one shot, force load
	txa
	sta (cia+CRA),y			; stop timer A after only a few cycles
	lda (cia+TALO),y
	and #$fe			; eleminate bit 0
	cmp #$c4			; check im timer A lo is now $c4 +/- 1 bit
	beq tok20
	dec timer_state			; dec fault counter
tok20:	lda (cia+TAHI),y
	cmp #$cc			; check timer hi has not changed
	beq tok21
	dec timer_state			; dec fault counter
tok21:	lda #$09
	sta (cia+CRB),y			; timer B start with one shot
	lda #$cc
	sta (cia+TBLO),y
	sta (cia+TBHI),y		; load $cccc in latch only because timer runs
	lda (cia+TBLO),y
	cmp #$aa			; check value not load while timer A runs
	bmi tok22
	dec timer_state			; dec fault counter
tok22:	lda (cia+TBHI),y
	cmp #$aa			; check value not load while timer A runs
	beq tok23
	dec timer_state			; dec fault counter
tok23:	lda #$19
	ldx #$00
	sta (cia+CRB),y			; timer B start with one shot, force load
	txa
	sta (cia+CRB),y			; stop timer B after only a few cycles
	lda (cia+TBLO),y
	and #$fe			; eleminate bit 0
	cmp #$c4			; check im timer A lo is now $c4 +/- 1 bit
	beq tok24
	dec timer_state			; dec fault counter
tok24:	lda (cia+TBHI),y
	cmp #$cc			; check timer hi has not changed
	beq tok25
	dec timer_state			; dec fault counter
tok25:	dec temp_dec_value
	bmi tok26
	jmp tireglp			; repeat $40 times
;
tok26:	lda #$00
	sta (cia+TAHI),y
	sta (cia+TBHI),y		; clear timers hi
	lda #$01
	sta (cia+TBLO),y		; load timer B lo = $01
	lda #$08
	sta (cia+TALO),y		; load timer A lo = $08
	lda #$51
	sta (cia+CRB),y			; start timer B, force load, counts underflow of timer A
	lda #$19
	sta (cia+CRA),y			; starts timer A, one shot, force load
	tax				; use $19 as counter
tdelay	dex
	bne tdelay			; delay
	txa				; $00 to a
	sta (cia+CRA),y			; stop + clear both timers
	sta (cia+CRB),y
	lda (cia+TBHI),y
	beq tok27			; timer B hi should be 0
	dec timer_state			; dec fault counter
tok27:	lda (cia+TBLO),y
	beq tok28			; timer B lo should be 0 because it counted underflow of timer A
	dec timer_state			; dec fault counter
tok28:	lda (cia+TAHI),y
	cmp #$00			; timer A hi should be 0
	beq tok29
	dec timer_state			; dec fault counter
tok29:	lda (cia+TALO),y
	cmp #$08			; timer A lo $08 because force load
	beq tok30
	dec timer_state			; dec fault counter
tok30:	lda timer_state			; dec fault counter
	beq tmrend			; skip if test ok
; timer fails
	lda #$ff
	sta cia_tmr_fail		; remember timer failed
	lda #$bf
	sta pointer3
	lda #$d5
	sta pointer3+1
	jsr DrawBad			; draw chip "BAD"
	lda cia_tod_fail
	ldx #$2d			; "TMR"
	bne drawtmr			; jump always -> draw text
	; unused - never reachable
	ldx #$34			; "TNT"
drawtmr:jsr DrawText			; sub: draw screen text
tmrend:	rts
; ----------------------------------------------------------------------------
; check time A IRQ
CheckIRQTimerA:
	lda #$88			; set timer A to $8888
	sta (cia+TALO),y
	sta (cia+TAHI),y
	lda (cia+CRA),y
	ora #$01
	sta (cia+CRA),y			; start timer A
	jsr Delay
	bne CheckIRQ			; jump always
; check timer B IRQ
CheckIRQTimerB:
	lda #$88			; set timer B to $8888
	sta (cia+TBLO),y
	sta (cia+TBHI),y
	lda (cia+CRB),y
	ora #$01
	sta (cia+CRB),y
	jsr Delay
	bne CheckIRQ			; jump always
CheckIRQ:
	jsr cciairq			; load, clear IRQ reg
	txa
	sta temp_irq			; remember irq bit from x for timer
	sta (cia+ICR),y			; clear mask bit for timer IRQ
	ldx #$00			; reset counter for waiting for IRQ
	stx temp3			; reset hi counter for waiting
irqlp:	lda (cia+ICR),y			; load IRQ reg
	bne CheckTimerIRQok
	inx
	bne irqlp			; wait for IRQ
	inc temp3
	lda #$0f
	cmp temp3
	bpl irqlp			; wait for IRQ
	ldx temp3
	rts				; returns X=$0a if no IRQ occured
; ----------------------------------------------------------------------------
; check if correct IRQ + time
CheckTimerIRQok:
	and temp_irq			; isolate timer IRQ bit
	cmp temp_irq			; check if timer IRQ bit set?
	beq irqok			; skip if IRQ ok
	dec timer_state			; dec fault counter
irqok:	cpx #$db			; compare time
	beq timelok			; skip if IRQ ok
	dec timer_state			; dec fault counter
timelok:ldx temp3
	cpx #$0a
	beq timehok
	dec timer_state			; dec fault counter
timehok:rts
; ----------------------------------------------------------------------------
; Delay
Delay:
	lda #$05
	clc
-	sbc #$01
	bpl -
	rts
; ----------------------------------------------------------------------------
; enable all CIA interrupts
eciairq:ldy #$00
	lda #$7f			; clear all irq mask bits
	sta (cia+ICR),y
; clear CIA interrupt reg
cciairq:ldy #$00
	lda (cia+ICR),y			; clear irq reg
	rts
; ----------------------------------------------------------------------------
; interrupt handler
InterruptHandler:
	rti				; return from interrupt
; TOD tests
TODTest:
	sei				; disable interrupts (ALARM test checks reg)
	ldx #$35			; "6526 TOD TESTS"
	jsr DrawText			; sub: draw screen text
	jsr InitCIAPointer		; sub: init cia pointer
	jsr eciairq			; enable cia irq's
	ldy #$00
	sty tod_state			; init TOD state to 0 = ok
	lda #SYSTEMBANK
	sta IndirectBank		; systembank for cia
	sty time2_minutes		; init h,m,s vars to 0
	sty time2_seconds
	sty time2_10th
	sty time1_minutes
	sty time1_seconds
	sty time1_10th
	lda #$01
	sta time2_hours			; init hours vars to 1
	sta time1_hours
; test 10x 10th change in first second
chk10lp:lda time2_10th
	sta time1_10th			; copy 10th to time2
	jsr TODCheck			; check if TOD increases one 10th
	lda tod_state
	bne todfai1			; branch -> TOD failure
	lda time2_seconds
	beq chk10lp			; check all 10th
	jsr PlaySound			; play sound after 1 second
; test last 10th increases a second
	lda #$09
	sta time1_10th
chkslp: lda time2_seconds
	sta time1_seconds
	jsr TODCheck
	lda tod_state
	bne todfai1
	lda time2_minutes
	beq chkslp			; check all seconds
	jsr PlaySound
; test minutes change
	lda #$59
	sta time1_seconds
chkmlp:	lda time2_minutes
	sta time1_minutes
	jsr TODCheck
	lda tod_state
	bne todfai1			; branch -> TOD failure
	lda time2_hours
	cmp #$01
	beq chkmlp			; check all minutes
	jsr PlaySound
; test hours change
	lda #$59
	sta time1_minutes
chkhlp:	lda time2_hours
	sta time1_hours
	jsr TODCheck
	lda tod_state
	bne todfai1			; branch -> TOD failure
	lda time2_hours
	cmp #$01
	bne chkh12			; skip if not 1
	lda #$81
	sta time2_hours			; set pm
	bne chkhlp			; check all hours
chkh12:	cmp #$12
	bne chkhlp			; check all hours
	sta time1_hours
	jsr TODCheck
	lda tod_state			; load state
todfai1:bne todfail			; branch -> TOD failure
; TOD alarm test
	lda (cia+ICR),y			; clear cia irq reg
	lda #$7f
	sta (cia+ICR),y			; clear all irq mask bits
	lda #$80
	sta (cia+CRB),y			; set bit #7 - TOD ALARM
	lda time2_hours
	sta (cia+TODHR),y		; set ALARM
	lda time2_minutes
	sta (cia+TODMIN),y
	lda time2_seconds
	sta (cia+TODSEC),y
	lda time2_10th
	clc
	adc #$01
	sta (cia+TOD10),y		; set ALARM to time2 + one 10th
	sty tod_count1			; clear counter
	sty tod_count2
alarmlp:lda (cia+ICR),y
	bne chkalar			; irq -> test for ALARM irq bit #2
	dec tod_count1
	bne alarmlp			; wait for ALARM
	dec tod_count2
	bne alarmlp			; wait for ALARM
	beq todfail			; branch -> TOD failure
chkalar:cmp #$04			; test ALARM irq bit
	beq todend			; skip if tod ALARM OK	
; tod fails
todfail:lda #$ff
	sta cia_tod_fail		; remember tod failed
	lda #$bf
	sta pointer3
	lda #$d5
	sta pointer3+1
	jsr DrawBad			; draw chip BAD
	lda cia_tmr_fail
!ifdef FIX_CIATNT{
	bne ciatnt				; branch if timer also failed 
	ldx #$33			; "TOD"
	bne drawtod			; jump always -> draw text
ciatnt:	ldx #$34			; "TNT"
} else{
	bne todend			; skip if timer already failed 
	ldx #$33			; "TOD"
	bne drawtod			; jump always -> draw text
; unused - never reachable
	ldx #$34			; "TNT"
}
drawtod:jsr DrawText			; sub: draw screen text
todend:	rts
; ----------------------------------------------------------------------------
; set TOD to time1 and set time2 = time1 + one 10th
; count for TOD change and compares to time2
TODCheck:
	sed				; decimal mode
	sty tod_count1			; clear counter
	sty tod_count2
	sty tod_count3
	lda time1_hours
	sta time2_hours
	sta (cia+TODHR),y		; set TOD starting with hours (halts TOD) to time1
	lda time1_minutes
	sta time2_minutes
	sta (cia+TODMIN),y
	lda time1_seconds
	sta time2_seconds
	sta (cia+TODSEC),y
	lda time1_10th
	sta (cia+TOD10),y		; set TOD 10th (starts TOD)
	clc
	adc #$01
	sta time2_10th			; set time2 = time1 + one 10th
	cmp #$10
	bne chktod			; skip if < 10
	sty time2_10th			; reset time2 10th
	clc
	lda time2_seconds
	adc #$01
	sta time2_seconds		; inc time2 seconds
	cmp #$60
	bne chktod			; skip if < 60
	sty time2_seconds		; reset time2 seconds
	clc
	lda time2_minutes
	adc #$01
	sta time2_minutes		; inc time2 minutes
	cmp #$60
	bne chktod			; skip if < 60
	sty time2_minutes		; reset time minutes
	clc
	lda time2_hours
	adc #$01
	sta time2_hours
	and #$1f			; isolate hours (without pm flag)
	cmp #$13
	bne chk12			; branch if time2 hours <>13
; set hours at 13 to 1 and toggles AM/PM
	lda time2_hours			; load time2 hours with pm flag
	and #$81			; at 13 reset hours to 1, preserve pm flag
	sta time2_hours
	bne togglpm			; jump always -> toggle am/pm
chk12:	cmp #$12
	beq togglpm			; if hours = 12 -> toggle pm flag
; toggles AM/PM back at 1
	cmp #$01
	bne chktod			; branch if hours > 1 and < 12
togglpm:lda #$80
	eor time2_hours			; toogle pm flag
	sta time2_hours
; count time for change of TOD to time1 init value
chktod:	lda (cia+TOD10),y
	cmp time1_10th
	bne todchg
	dec tod_count1			; dec counter if no change
	bne chktod
	dec tod_count2
	bne chktod
	dec tod_count3
	bne chktod
	beq todbad			; if TOD doesn't change in 999999 cycles -> bad
; compare new time to time2
todchg:	cmp time2_10th			; compare if TOD is now = time2
	bne todbad			; if not -> failure
	lda (cia+TODSEC),y
	cmp time2_seconds
	bne todbad			; ********** CMOS ERROR: TOD seconds still 1, but time2_seconds=2 **********
	lda (cia+TODMIN),y
	cmp time2_minutes
	bne todbad
	lda (cia+TODHR),y
	cmp time2_hours
	beq todok
todbad:	lda #$ff			; state = TOD bad
	sta tod_state
todok:	cld				; reset decimal flag
	rts
; ----------------------------------------------------------------------------
; RAM Tests
RAMTest:
	ldy banks
	dey
	sty $57
	ldx CodeBank
	stx $31
	dex
	bne l2713
	ldx banks
l2713:	stx $2b
	stx $30
	jsr l2a34
	jsr l2a0f
	ldx $2b
	stx IndirectBank
	jsr l27e5
	ldx $2b
	stx $31
	dex
	bne l272d
	ldx banks
l272d:	stx $2b
	dec $57
	bne l2713
	ldx $31
	stx $2b
	jsr l2a0f
	ldy banks
	dey
	sty $57
	ldx CodeBank
	stx $30
	stx $2b
	dex
	bne l274a
	ldx banks
l274a:	lda $16,x
	beq l2760
l274e:	dex
	bne l2753
	ldx banks
l2753:	dec $57
	bne l274a
	ldx $2b
	stx $31
	jsr l2a0f
	bne l279f
l2760:	stx $56
	txa
	ldx #$00
	ldy #$00
	jsr l2c1f
	lda CodeBank
	jsr l2c16
	ldx #$33
	inx
	jsr l2bad
	beq l277b
	ldx $56
	bpl l274e
l277b:	ldy CodeBank
	ldx $56
	stx CodeBank
	nop
	nop
	nop
	nop
	sty $2b
	ldx CodeBank
	stx $31
	jsr l2a0f
	jsr l2a34
	ldx $2b
	stx IndirectBank
	jsr l27e5
	ldx $2b
	stx $31
	jsr l2a0f
l279f:	jsr l27a3
	rts
; ----------------------------------------------------------------------------
; 
l27a3:	ldx #$2c
	jsr DrawText			; sub: draw screen text
	lda #SYSTEMBANK
	sta IndirectBank
	ldy #$02
	ldx #$00
	lda #$08
	jsr l27ea
	lda CodeBank
	ldx #$d0
	ldy #$00
	jsr l2c1f
	lda #$0f
	jsr l2c16
	ldx #$08
	jsr l2bad
	lda #$d8
	ldy #$00
	ldx #$d0
	jsr l27ea
	lda #$0f
	ldx #$d0
	ldy #$00
	jsr l2c1f
	lda CodeBank
	jsr l2c16
	ldx #$08
	jsr l2bad
	rts
; ----------------------------------------------------------------------------
; 
l27e5:	lda #$00
	tax
	ldy #$02
l27ea:	sty $41
	stx $40
	sta $25
	dey
	sty $26
	lda #$00
	sta pointer1
	lda IndirectBank
	cmp #SYSTEMBANK
	beq l2805
	jsr PlaySound
	ldx #$0e
	jsr DrawText			; sub: draw screen text
l2805:	ldy $41
	lda $40
	sta pointer1+1
l280b:	tya
	sta temp_count_sum
	sta (pointer1),y
	lda (pointer1),y
	eor temp_count_sum
	beq l2819
	jsr l2a59
l2819:	iny
	bne l280b
	inc pointer1+1
	lda pointer1+1
	cmp $25
	bne l280b
	ldx #$0f
	jsr l2b7e
l2829:	tya
	sta temp_count_sum
	lda (pointer1),y
	eor temp_count_sum
	beq l2835
	jsr l2a59
l2835:	lda pointer1+1
	sta (pointer1),y
	lda (pointer1),y
	eor pointer1+1
	beq l2842
	jsr l2a59
l2842:	iny
	bne l2829
	inc pointer1+1
	lda pointer1+1
	cmp $25
	bne l2829
	ldx #$10
	jsr l2b7e
	lda #$55
	sta temp_count_sum
	lda #$aa
	sta temp1
l285a:	lda (pointer1),y
	eor pointer1+1
	beq l2863
	jsr l2a59
l2863:	lda #$55
	sta (pointer1),y
	lda (pointer1),y
	eor temp_count_sum
	beq l2870
	jsr l2a59
l2870:	iny
	lda (pointer1),y
	eor pointer1+1
	beq l287a
	jsr l2a59
l287a:	lda #$aa
	sta (pointer1),y
	lda (pointer1),y
	eor temp1
	beq l2887
	jsr l2a59
l2887:	iny
	bne l285a
	inc pointer1+1
	lda pointer1+1
	cmp $25
	bne l285a
	ldx #$11
	jsr l2b7e
l2897:	lda (pointer1),y
	eor temp_count_sum
	beq l28a0
	jsr l2a59
l28a0:	lda #$aa
	sta (pointer1),y
	lda (pointer1),y
	eor temp1
	beq l28ad
	jsr l2a59
l28ad:	iny
	lda (pointer1),y
	eor temp1
	beq l28b7
	jsr l2a59
l28b7:	lda #$55
	sta (pointer1),y
	lda (pointer1),y
	eor temp_count_sum
	beq l28c4
	jsr l2a59
l28c4:	iny
	bne l2897
	inc pointer1+1
	lda pointer1+1
	cmp $25
	bne l2897
	ldx #$12
	jsr l2b7e
	ldx #$5a
	stx $4e
l28d8:	lda (pointer1),y
	eor temp1
	beq l28e1
	jsr l2a59
l28e1:	txa
	sta (pointer1),y
	lda (pointer1),y
	eor $4e
	beq l28ed
	jsr l2a59
l28ed:	iny
	lda (pointer1),y
	eor temp_count_sum
	beq l28f7
	jsr l2a59
l28f7:	txa
	sta (pointer1),y
	lda (pointer1),y
	eor $4e
	beq l2903
	jsr l2a59
l2903:	iny
	bne l28d8
	inc pointer1+1
	lda pointer1+1
	cmp $25
	bne l28d8
	jsr PlaySound
	ldx #$13
	jsr l2b95
	ldx #$5a
	stx temp_count_sum
	ldx #$a5
	stx temp1
l291e:	lda (pointer1),y
	eor temp_count_sum
	beq l2927
	jsr l2a59
l2927:	txa
	sta (pointer1),y
	lda (pointer1),y
	eor temp1
	beq l2933
	jsr l2a59
l2933:	dey
	cpy #$ff
	bne l291e
	dec pointer1+1
	lda pointer1+1
	cmp $40
	bne l291e
l2940:	lda (pointer1),y
	eor temp_count_sum
	beq l2949
	jsr l2a59
l2949:	txa
	sta (pointer1),y
	lda (pointer1),y
	eor temp1
	beq l2955
	jsr l2a59
l2955:	dey
	cpy $26
	bne l2940
	ldx #$14
	jsr l2b95
	ldx #$5a
l2961:	lda (pointer1),y
	eor temp1
	beq l296a
	jsr l2a59
l296a:	txa
	sta (pointer1),y
	lda (pointer1),y
	eor temp_count_sum
	beq l2976
	jsr l2a59
l2976:	dey
	cpy #$ff
	bne l2961
	dec pointer1+1
	lda pointer1+1
	cmp $40
	bne l2961
l2983:	lda (pointer1),y
	eor temp1
	beq l298c
	jsr l2a59
l298c:	txa
	sta (pointer1),y
	lda (pointer1),y
	eor temp_count_sum
	beq l2998
	jsr l2a59
l2998:	dey
	cpy $26
	bne l2983
	ldx #$15
	jsr l2b7e
	ldx #$ff
	stx $4e
l29a6:	lda (pointer1),y
	eor temp_count_sum
	beq l29af
	jsr l2a59
l29af:	txa
	sta (pointer1),y
	lda (pointer1),y
	eor $4e
	beq l29bb
	jsr l2a59
l29bb:	iny
	bne l29a6
	inc pointer1+1
	lda pointer1+1
	cmp $25
	bne l29a6
	ldx #$16
	jsr l2b95
	ldx #$00
	stx temp_count_sum
	ldx #$ff
	stx temp1
l29d3:	txa
	lda (pointer1),y
	eor temp1
	beq l29dd
	jsr l2a59
l29dd:	sta (pointer1),y
	lda (pointer1),y
	eor temp_count_sum
	beq l29e8
	jsr l2a59
l29e8:	dey
	cpy #$ff
	bne l29d3
	dec pointer1+1
	lda pointer1+1
	cmp $40
	bne l29d3
l29f5:	txa
	lda (pointer1),y
	eor temp1
	beq l29ff
	jsr l2a59
l29ff:	sta (pointer1),y
	lda (pointer1),y
	eor temp_count_sum
	beq l2a0a
	jsr l2a59
l2a0a:	dey
	cpy $26
	bne l29f5
l2a0f:	lda #$30
	sta pointer1
	lda #$d7
	sta pointer1+1
	ldx #SYSTEMBANK
	stx IndirectBank
	ldx $2b
	dex
	txa
	jsr calc2
	tay
	lda #$2a
	sta (pointer1),y
	ldx $31
	dex
l2a2a:	txa
	jsr calc2
	tay
	lda #$20
	sta (pointer1),y
	rts
; ----------------------------------------------------------------------------
; 
l2a34:	lda #$80
	sta pointer1
	lda #$d7
	sta pointer1+1
	ldx #SYSTEMBANK
	stx IndirectBank
	ldx CodeBank
	dex
	txa
	jsr calc2
	tay
	lda #$2a
	sta (pointer1),y
	ldx $30
	dex
	txa
	jsr calc2
	tay
	lda #$20
	sta (pointer1),y
	rts
; ----------------------------------------------------------------------------
; 
l2a59:	clv
	sta $27
	stx $28
	sty $29
	jsr Hex2Screencode			; calc screen code for a byte
	ldx IndirectBank
	stx actual_indirbank
	cpx #SYSTEMBANK
	bne l2a6f
	ldx CodeBank
	bne l2a71
l2a6f:	ldx #SYSTEMBANK
l2a71:	stx IndirectBank
	sty temp2
	ldy #$00
	ldx #$e0
	stx pointer3
	ldx #$d6
	stx pointer3+1
	sta (pointer3),y
	iny
	lda temp2
	sta (pointer3),y
	lda actual_indirbank
	jsr Hex2Screencode			; calc screen code for a byte
	tya
	ldy #$03
	sta (pointer3),y
	lda pointer1+1
	jsr Hex2Screencode			; calc screen code for a byte
	sty temp2
	ldy #$05
	sta (pointer3),y
	iny
	lda temp2
	sta (pointer3),y
	lda $29
	jsr Hex2Screencode			; calc screen code for a byte
	sty temp2
	ldy #$07
	sta (pointer3),y
	iny
	lda temp2
	sta (pointer3),y
	lda CodeBank
	jsr Hex2Screencode			; calc screen code for a byte
	tya
	ldy #$0b
	sta (pointer3),y
	ldy actual_indirbank
	cpy #$0f
	beq l2b1c
	dey
	lda #$ff
	sta $0017,y
	tya
	jsr calc2
	tay
	lda #$31
	sta pointer3
	lda #$d7
	sta pointer3+1
	lda (pointer3),y
	bmi l2ae3
	ldx #$13
l2ad9:	lda (pointer3),y
	ora #$80
	sta (pointer3),y
	iny
	dex
	bne l2ad9
l2ae3:	ldy actual_indirbank
	dey
	lda screen_pos_lo,y
	sta pointer3
	lda screen_pos_hi,y
	sta pointer3+1
	ldx #$08
	stx $56
l2af4:	lda $27
	clc
	rol
	sta $27
	bcc l2aff
	jsr DrawBad			; draw chip "BAD"
l2aff:	lda #$05
	clc
	adc pointer3
	sta pointer3
	bcc l2b0a
	inc pointer3+1
l2b0a:	ldx $56
	dex
	stx $56
	bne l2af4
l2b11:	ldx actual_indirbank
	stx IndirectBank
	ldx $28
	ldy $29
	lda #$00
	rts
; ----------------------------------------------------------------------------
; 
l2b1c:	lda pointer1+1
	and #$f0
	bne l2b2f
	lda #$a1
	sta pointer3
	lda #$d5
	sta pointer3+1
	jsr DrawBad			; draw chip "BAD"
	bmi l2b11
l2b2f:	lda #$a6
	sta pointer3
	lda #$d5
	sta pointer3+1
	lda CodeBank
	jsr l2b40
	bmi l2b11
; draw bad chip reverse
DrawBad:
	lda #SYSTEMBANK
l2b40:	sta IndirectBank		; systembank for screen
	tya
	pha				; preserve y reg
	ldy #$00
	lda (pointer3),y		; load screen char at pointer3
	bmi alrebad			; skip if already reverse
	lda pointer3
	sta pointer4			; copy pointer3 to 4
	lda pointer3+1
	sta pointer4+1	
; draw 2 lines chip number reverse
	ldx #$02			; 2 lines
revline:ldy #$02			; 3 chars
revlp:	lda (pointer4),y
	ora #$80
	sta (pointer4),y		; reverse char
	dey
	bpl revlp			; next char
	lda #80				; add 80 chars = one line
	clc
	adc pointer4
	sta pointer4
	bcc +				; skip if no carry
	inc pointer4+1			; inc pointer hi
+	dex
	bne revline			; draw next line
; draw BAD in third line
	ldy #$02
badlp:	lda chip_bad,y			; draw BAD in chip
	and #$3f
	ora #$80			; reverse
	sta (pointer4),y
	dey
	bpl badlp
alrebad:pla
	tay
	rts
; ----------------------------------------------------------------------------
; 
l2b7e:	stx $50
	lda IndirectBank
	cmp #SYSTEMBANK
	beq l2b8e
	jsr l225a
	ldx $50
	jsr DrawText			; sub: draw screen text
l2b8e:	ldy $41
	lda $40
	sta pointer1+1
	rts
; ----------------------------------------------------------------------------
; 
l2b95:	stx $50
	lda IndirectBank
	cmp #SYSTEMBANK
	beq l2ba5
	jsr l225a
	ldx $50
	jsr DrawText			; sub: draw screen text
l2ba5:	ldy $25
	dey
	sty pointer1+1
	ldy #$ff
	rts
; ----------------------------------------------------------------------------
; 
l2bad:	stx $34
	stx temp2
	ldx IndirectBank
	stx actual_indirbank
	ldy #$00
	cpy $2d
	bne l2bbd
	ldy #$02
l2bbd:	sty $3d
l2bbf:	ldx $31
	stx IndirectBank
	lda ($32),y
	ldx $2b
	stx IndirectBank
	sta ($2c),y
	iny
	bne l2bbf
	inc $33
	inc $2d
	dec $34
	bne l2bbf
	lda temp2
	sta $34
	lda #$00
	sta $11
	ldy $3d
	lda temp1
	ora $2c
	bne l2be8
	ldy #$12
l2be8:	lda temp_count_sum
	sta $33
	lda temp1
	sta $2d
l2bf0:	ldx $31
	stx IndirectBank
	lda ($32),y
	sta $10
	ldx $2b
	stx IndirectBank
	lda ($2c),y
	eor $10
	ora $11
	sta $11
	iny
	bne l2bf0
	inc $33
	inc $2d
	dec $34
	bne l2bf0
	ldx actual_indirbank
	stx IndirectBank
	lda $11
	rts
; ----------------------------------------------------------------------------
; 
l2c16:	sta $31
	stx $33
	stx temp_count_sum
	sty $32
	rts
; ----------------------------------------------------------------------------
; 
l2c1f:	sta $2b
	stx $2d
	stx temp1
	sty $2c
	rts
; ----------------------------------------------------------------------------
; calc screen codes for a byte and return in a and y
Hex2Screencode:
	pha				; remember value on stack
	jsr Nibble2Screencode		; calc low nibble
	tay				; remember lower digit in Y
	pla
; calc screen code for high nibble
UpperNibble2Screencode:
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
; 
calc2:	jsr mul5
	asl
	asl
	rts
; ----------------------------------------------------------------------------
; 
mul5:	clc
	sta temp2
	asl
	asl
	adc temp2
	rts
; ----------------------------------------------------------------------------
; init cia pointer
InitCIAPointer:
	lda #cia
	sta pointer1
	lda #$00
	sta pointer1+1
	lda #<ciaregs
	ldx #>ciaregs
	ldy #$1f
	jsr CopyPointer			; sub: copy pointer addresses
	rts
; ----------------------------------------------------------------------------
; init tpi2 pointer
InitTPI2Pointer:
	lda #tpi2
	sta pointer1
	lda #$00
	sta pointer1+1
	lda #<tpi2regs
	ldx #>tpi2regs
	ldy #$0f
	jsr CopyPointer			; sub: copy pointer addresses
	rts
; ----------------------------------------------------------------------------
; init tpi1 pointer - UNUSED
	lda #tpi1
	sta pointer1
	lda #$00
	sta pointer1+1
	lda #<tpi1regs
	ldx #>tpi1regs
	ldy #$0f
	jsr CopyPointer			; sub: copy pointer addresses
	rts
; ----------------------------------------------------------------------------
; init crt pointer
InitCRTPointer:
	lda #<CRT
	sta crtadr
	lda #>CRT
	sta crtadr+1
	lda #<(CRT+1)
	sta crtdata
	lda #>(CRT+1)
	sta crtdata+1
	rts
; ----------------------------------------------------------------------------
; init acia pointer - UNUSED
	lda #acia
	sta pointer1
	lda #$00
	sta pointer1+1
	lda #<aciaregs
	ldx #>aciaregs
	ldy #$07
	jsr CopyPointer			; sub: copy pointer addresses
	rts
; ----------------------------------------------------------------------------
; init sid pointer
InitSIDPointer:
	lda #sid
	sta pointer1
	lda #$00
	sta pointer1+1
	lda #<sidregs
	ldx #>sidregs
	ldy #$39
	jsr CopyPointer			; sub: copy pointer addresses
	rts
; ----------------------------------------------------------------------------
; init system vectors for timer test
InitSystemVectors:
	lda #nmi_pointer
	sta HW_NMI
	lda #$00
	sta HW_NMI+1
	lda #reset_pointer
	sta HW_RESET
	lda #$00
	sta HW_RESET+1
	lda #irq_pointer
	sta HW_IRQ
	lda #$00
	sta HW_IRQ+1
	lda #<InterruptHandler
	sta nmi_pointer
	sta reset_pointer
	sta irq_pointer
	lda #>InterruptHandler
	sta nmi_pointer+1
	sta reset_pointer+1
	sta irq_pointer+1
	rts
; ----------------------------------------------------------------------------
; copy data y+1 bytes from ax to pointer1
CopyPointer:
	sta pointer3
	stx pointer3+1
	lda CodeBank
	sta IndirectBank
cpptlp:	lda (pointer3),y
	sta (pointer1),y
	dey
	bpl cpptlp
	rts
; ----------------------------------------------------------------------------
; draw reverse
DrawTextReverse:
	pha
	tya
	pha
	txa
	pha
	lda #$80			; reverse
	sta temp_or_value
	bne drawtx2			; jump always
; draw pure withour AND
DrawTextPure:
	pha
	tya
	pha
	txa
	pha
	lda #$00
	sta temp_or_value
	lda #$ff			; data and value
	bne drawtx1			; jump always
; draw screen text
DrawText:
	pha				; save regs
	tya
	pha
	txa
	pha
	lda #$00			; data or value
	sta temp_or_value
drawtx2:lda #$3f			; data and value
drawtx1	sta temp_and_value
	lda IndirectBank		; remember indirect bank
	sta actual_indirbank
	lda scrdata_count,x
	tay
	lda scrdata_lo,x
	sta screendata_pointer
	lda scrdata_hi,x
l2d2d:	sta screendata_pointer+1
	lda screen_lo,x
	sta screen_pointer
	lda screen_hi,x
	sta screen_pointer+1
	ldx #SYSTEMBANK
-	lda CodeBank
	sta IndirectBank
	lda (screendata_pointer),y
	and temp_and_value
	ora temp_or_value
	stx IndirectBank
	sta (screen_pointer),y
	dey
	bpl -
	lda actual_indirbank
	sta IndirectBank		; restore indirect bank
	pla				; restore regs
	tax
	pla
	tay
	pla
	rts
; ----------------------------------------------------------------------------
; screen ic-numbers/text data
*= $2d56
high:		!scr "HIGH"

ichigh1:	!scr $5d, " 37", $5d, $5d, " 45", $5d, $5d, " 50", $5d, $5d, " 59", $5d
		!scr $5d, " 67", $5d, $5d, " 76", $5d, $5d, " 85", $5d, $5d, " 89", $5d
		!scr $5d, " 38", $5d, $5d, " 46", $5d, $5d, " 51", $5d, $5d, " 60", $5d
		!scr $5d, " 68", $5d, $5d, " 77", $5d, $5d, " 86", $5d, $5d, " 90", $5d
		
ichigh2:	!scr $5d, " 39", $5d, $5d, " 47", $5d, $5d, " 52", $5d, $5d, " 61", $5d
		!scr $5d, " 69", $5d, $5d, " 78", $5d, $5d, " 87", $5d, $5d, " 91", $5d
		!scr $5d, " 40", $5d, $5d, " 48", $5d, $5d, " 53", $5d, $5d, " 93", $5d
		!scr $5d, " 70", $5d, $5d, " 79", $5d, $5d, " 88", $5d, $5d, " 92", $5d

ichigh3:	!scr $5d, "  9", $5d, $5d, " 29", $5d, $5d, "   ", $5d, $5d, " 17", $5d
		!scr $5d, " 13", $5d, $5d, " 12", $5d, $5d, " 11", $5d, $5d, "  8", $5d
		!scr $5d, "  5", $5d, $5d, "  4", $5d, $5d, " 10", $5d, $5d, "  6", $5d
		!scr $5d, "  7", $5d, $5d, "  2", $5d, $5d, "  1", $5d, $5d, " 84", $5d

title:		!scr "CBM-II    LOW PROFILE   970148.A.6   CYCLES: 0"

iclow1:		!scr $5d, " 79", $5d, $5d, " 78", $5d, $5d, " 77", $5d, $5d, " 76", $5d
		!scr $5d, " 75", $5d, $5d, " 74", $5d, $5d, " 73", $5d, $5d, " 72", $5d
		!scr $5d, " 71", $5d, $5d, " 70", $5d, $5d, " 69", $5d, $5d, " 68", $5d
		!scr $5d, " 67", $5d, $5d, " 66", $5d, $5d, " 65", $5d, $5d, " 64", $5d

iclow2:		!scr $5d, " 52", $5d, $5d, " 51", $5d, $5d, " 50", $5d, $5d, " 49", $5d
		!scr $5d, " 48", $5d, $5d, " 47", $5d, $5d, " 46", $5d, $5d, " 45", $5d
		!scr $5d, " 36", $5d, $5d, " 35", $5d, $5d, " 34", $5d, $5d, " 33", $5d
		!scr $5d, " 32", $5d, $5d, " 31", $5d, $5d, " 30", $5d, $5d, " 29", $5d

iclow3:		!scr $5d, " 58", $5d, $5d, " 26", $5d, $5d, "   ", $5d, $5d, " 61", $5d
		!scr $5d, " 60", $5d, $5d, " 59", $5d, $5d, "  1", $5d, $5d, "  2", $5d
		!scr $5d, "  3", $5d, $5d, "  4", $5d, $5d, "  7", $5d, $5d, " 10", $5d
		!scr $5d, " 15", $5d, $5d, " 16", $5d, $5d, " 17", $5d, $5d, " 28", $5d
; io register addresses
sidregs:	!byte $00, $da, $01, $da, $02, $da, $03, $da	; sid register
		!byte $04, $da, $05, $da, $06, $da, $07, $da
		!byte $08, $da, $09, $da, $0a, $da, $0b, $da
		!byte $0c, $da, $0d, $da, $0e, $da, $0f, $da
		!byte $10, $da, $11, $da, $12, $da, $13, $da
		!byte $14, $da, $15, $da, $16, $da, $17, $da
		!byte $18, $da, $19, $da, $1a, $da, $1b, $da
		!byte $1c, $da

ciaregs:	!byte $00, $dc, $01, $dc, $02, $dc, $03, $dc	; cia register
		!byte $04, $dc, $05, $dc, $06, $dc, $07, $dc
		!byte $08, $dc, $09, $dc, $0a, $dc, $0b, $dc
		!byte $0c, $dc, $0d, $dc, $0e, $dc, $0f, $dc

aciaregs:	!byte $00, $dd, $01, $dd, $02, $dd, $03, $dd	; acia register

tpi1regs:	!byte $00, $de, $01, $de, $02, $de, $03, $de	; tpi1 register
		!byte $04, $de, $05, $de, $06, $de, $07, $de

tpi2regs:	!byte $00, $df, $01, $df, $02, $df, $03, $df	; tpi2 register
		!byte $04, $df, $05, $df, $06, $df, $07, $df
; screen text data
ramsegf:	!scr "RAM SEG $F"
romssegf:	!scr "ROMS SEG $F"
loadrtest:	!scr "LO ADR BYTE TEST "
hiadrtest:	!scr "HI ADR BYTE TEST "
chkrbrd:	!scr "CHKRBRD $55, $AA "
aa55:		!scr "AA, $55 "
marchinc:	!scr "MARCH INC ADR $5A "
decadra5:	!scr "DEC ADR $A5 "
dec5a:		!scr "5A "
incadr:		!scr "INC ADR $FF "
decadr00:	!scr "DEC ADR $00 "
staticram:	!scr "STATIC RAM TESTS  "
todtests:	!scr "6526 TOD TESTS    "
timertests:	!scr "6526 TIMERS TESTS "
tmr:		!scr "TMR"
tod:		!scr "TOD"
tnt:		!scr "TNT"
ics:		!scr "6526 6525 ", $67, "160 ", $67, "161 6845 6581 6551 1489 1488 6525"
line:		!scr "*-----------*"
segment:	!scr "SEGMENT:"
test:		!scr "TEST"
execute:	!scr "EXECUTE"
; screen positions
screen_pos_lo:	!byte $41, $69, $71, $99
screen_pos_hi:	!byte $d1, $d1, $d3, $d3
; screen text data
bProgramChecksum:	!scr " * *  BAD PROGRAM CHECKSUM  * * "
noram:		!scr " *************  NO RAM  *************** "
; screendata count -1
scrdata_count:	!byte 45,  9, 10, 79, 79, 79, 11, 11	; data $00-$07
		!byte 11, 11, 11, 11, 11, 11, 16, 16	; data $08-$0f
		!byte 16,  7, 17, 11,  2, 11, 11,  7	; data $10-$17
		!byte  7,  7,  7,  7,  7,  7,  7,  7	; data $18-$1f
		!byte  7,  7,  7,  6,  6,  6,  6,  3	; data $20-$27
		!byte  3,  3,  3, 48, 17,  2, 31,  3	; data $28-$2f
		!byte 79, 79, 79,  2,  2, 17, 17, 39	; data $30-$37
		!byte 39				; data $38
; screendata addresses lo
scrdata_lo:	!byte <title, <ramsegf, <romssegf, <iclow1, <iclow2, <iclow3, <line, <line
		!byte <line, <line, <(line+1), <(line+1), <(line+1), <(line+1), <loadrtest, <hiadrtest
		!byte <chkrbrd, <aa55, <marchinc, <decadra5, <dec5a, <incadr, <decadr00, <segment
		!byte <segment, <segment, <segment, <segment, <segment, <segment, <segment, <segment
		!byte <segment, <segment, <segment, <execute, <execute, <execute, <execute, <test
		!byte <test, <test, <test, <ics, <staticram, <tmr, <bProgramChecksum, <high
		!byte <ichigh1, <ichigh2, <ichigh3, <tod, <tnt, <todtests, <timertests, <noram
		!byte <noram
; screendata addresses hi
scrdata_hi:	!byte >title, >ramsegf, >romssegf, >iclow1, >iclow2, >iclow3, >line, >line
		!byte >line, >line, >(line+1), >(line+1), >(line+1), >(line+1), >loadrtest, >hiadrtest
		!byte >chkrbrd, >aa55, >marchinc, >decadra5, >dec5a, >incadr, >decadr00, >segment
		!byte >segment, >segment, >segment, >segment, >segment, >segment, >segment, >segment
		!byte >segment, >segment, >segment, >execute, >execute, >execute, >execute, >test
		!byte >test, >test, >test, >ics, >staticram, >tmr, >bProgramChecksum, >high
		!byte >ichigh1, >ichigh2, >ichigh3, >tod, >tnt, >todtests, >timertests, >noram
		!byte >noram
; screen RAM addresses lo
screen_lo:	!byte $00, $00, $11, $90, $c0, $f0, $a2, $ca
		!byte $d2, $fa, $ba, $e2, $ea, $12, $37, $37
		!byte $37, $40, $37, $3d, $46, $3d, $3d, $af
		!byte $d7, $df, $07, $3a, $4e, $62, $76, $8a
		!byte $9e, $b2, $c6, $82, $96, $aa, $be, $32
		!byte $46, $5a, $6e, $1f, $37, $af, $08, $09
		!byte $90, $c0, $f0, $af, $af, $37, $37, $20
		!byte $48 
; screen RAM addresses hi
screen_hi:	!byte $d0, $d5, $d5, $d1, $d3, $d5, $d0, $d0
		!byte $d2, $d2, $d0, $d0, $d2, $d3, $d0, $d0
		!byte $d0, $d0, $d0, $d0, $d0, $d0, $d0, $d0
		!byte $d0, $d2, $d3, $d7, $d7, $d7, $d7, $d7
		!byte $d7, $d7, $d7, $d7, $d7, $d7, $d7, $d7
		!byte $d7, $d7, $d7, $d5, $d0, $d6, $d7, $d0
		!byte $d1, $d3, $d5, $d6, $d6, $d0, $d0, $d3
		!byte $d3
; ----------------------------------------------------------------------------
; chip graphics data
chip_top:	!byte $70, $40, $60, $40, $6e		; ic upper line
chip_u:		!byte $5d, ' ', ' ', 'u', $5d		; ic line with U
chip_ok:	!byte $5d, ' ', 'o', 'k', $5d		; ic line with OK
chip_bottom:	!byte $6d, $40, $40, $40, $7d		; ic lower line
chip_bad:	!byte 'B', 'A', 'D'			; BAD
; chip graphics addresses
chip_data_lo:	!byte <chip_top, <chip_u, <chip_ok ,<chip_bottom ,<chip_bad
chip_data_hi:	!byte >chip_top, >chip_u, >chip_ok ,>chip_bottom ,>chip_bad
; ----------------------------------------------------------------------------
; char screen positions
pos0lo:		!byte $15, $3d, $45, $6d, $52
pos0hi:		!byte $d1, $d1, $d3, $d3, $d5
pos1lo:		!byte $b8, $10, $38, $40, $68, $42, $92
pos1hi:		!byte $d0, $d1, $d1, $d3, $d3, $d7, $d7
pos2lo:		!byte $e0, $0b, $33, temp3, $63, $56, $a6
pos2hi:		!byte $d0, $d1, $d1, $d3, $d3, $d7, $d7
pos3lo:		!byte $06, $2e, $e8, $36, $5e, $6a, $ba
pos3hi:		!byte $d1, $d1, $d2, $d3, $d3, $d7, $d7
pos4lo:		!byte $01, $29, $10, $31, $59, $7e, $ce
pos4hi:		!byte $d1, $d1, $d3, $d3, $d3, $d7, $d7
pos5lo:		!byte $fc, $24, $2c, $54
pos5hi:		!byte $d0, $d1, $d3, $d3
pos6lo:		!byte $f7, $1f, $27, $4f
pos6hi:		!byte $d0, $d1, $d3, $d3
pos7lo:		!byte $f2, $1a, $22, $4a
pos7hi:		!byte $d0, $d1, $d3, $d3
pos8lo:		!byte $6b
pos8hi:		!byte $d5
posAlo:		!byte $66
posAhi:		!byte $d5
posDlo:		!byte $57
posDhi:		!byte $d5
posElo:		!byte $61
posEhi:		!byte $d5
pos_lo:		!byte $ad, $4c, $4d
pos_hi:		!byte $d5, $d6, $d6
; char count -1
pos_count:	!byte $04, $06, $06, $06, $06, $03, $03, $03, $00, $00, $00, $00, $02
; pointer to screenposition lo - lo bytes
screenposlo_lo:	!byte <pos0lo, <pos1lo, <pos2lo, <pos3lo, <pos4lo, <pos5lo, <pos6lo, <pos7lo, <pos8lo, <posAlo, <posDlo, <posElo, <pos_lo
; pointer to screenposition lo - hi bytes
screenposlo_hi:	!byte >pos0lo, >pos1lo, >pos2lo, >pos3lo, >pos4lo, >pos5lo, >pos6lo, >pos7lo, >pos8lo, >posAlo, >posDlo, >posElo, >pos_lo
; pointer to screenposition hi - lo bytes
screenposhi_lo:	!byte <pos0hi, <pos1hi, <pos2hi, <pos3hi, <pos4hi, <pos5hi, <pos6hi, <pos7hi, <pos8hi, <posAhi, <posDhi, <posEhi, <pos_hi
; pointer to screenposition hi - hi bytes
screenposhi_hi:	!byte >pos0hi, >pos1hi, >pos2hi, >pos3hi, >pos4hi, >pos5hi, >pos6hi, >pos7hi, >pos8hi, >posAhi, >posDhi, >posEhi, >pos_hi
; screen char
char:		!byte '0', '1', '2', '3', '4', '5', '6', '7', '8', 'A', 'D', 'E', ' '
; UNUSED
		!byte FILL, FILL, FILL, FILL, FILL, FILL, FILL, FILL, FILL
