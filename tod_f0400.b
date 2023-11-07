; CBMII b128-256 burnin 
; disassembled by vossi 10/2023
;
; NOTE: ROM Checksum routine is faulty:
;	with not 256kB it loads wrong ROM start addresses from ZP instead immediate E0, A0, 80 
;
!cpu 6502
!ct scr		; standard text/char conversion table -> Screencode (pet = PETSCII, raw)
!to "tod_f0400.prg", cbm
; ***************************************** CONSTANTS *********************************************
CODESTART		= $0400		; code start
; TPI register
PC			= $2		; port c
MIR			= $5		; interrupt mask register
CR			= $6		; control register
; CIA register
talo			= $4		; timer a lo
tahi			= $5		; timer a hi
tblo			= $6		; timer b lo
tbhi			= $7		; timer b hi
tod10			= $8		; tod 10th of seconds
todsec			= $9		; tod seconds
todmin			= $a		; tod monutes
todhr			= $b		; tod hours
icr			= $d		; interrupt control register
cra			= $e		; control register b
crb			= $f		; control register b
; SID register
OSC1			= $00		; oscillator 1
OSC3			= $0e		; oscillator 2
FREQHI			= $01		; frequency hi
OSCCTL			= $04		; oscillator control
ATKDCY			= $05		; attack/decay
SUSREL			= $06		; sustain/release
VOLUME			= $18		; volume
; ***************************************** ADDRESSES *********************************************
!addr CodeBank		= $00		; code bank register
!addr IndirectBank	= $01		; indirect bank register
!addr ScreenRAM		= $d000		; Screen RAM
!addr CRT		= $d800		; CRT
!addr CIA		= $dc00
!addr TPI2		= $df00
!addr SID		= $da00
; ***************************************** ZERO PAGE *********************************************
!addr time1_hours	= $f0		; time 1 hours
!addr time1_minutes	= $f1		; time 1 minutes
!addr time1_seconds	= $f2		; time 1 seconds
!addr time1_10th	= $f3		; time 1 10th seconds
!addr time2_hours	= $f4		; time 1 hours
!addr time2_minutes	= $f5		; time 1 minutes
!addr time2_seconds	= $f6		; time 1 seconds
!addr time2_10th	= $f7		; time 1 10th seconds
!addr tod_count1	= $f8		; tod test counter
!addr tod_count2	= $f9		; tod test counter
!addr tod_count3	= $fa		; tod test counter
!addr tod_state		= $fb		; TOD state - $ff = bad
; ******************************************* CODE ************************************************
*= CODESTART
	sei
	cld
	ldx #$ff
	txs
; clear zero page f0-ff
	ldy #$f0
	lda #$00
clrzplp:sta $0000,y
	iny
	bne clrzplp

	lda TPI2+CR
	and #$fe			; TPI2 mc=0 disable interrupt controller
	sta TPI2+CR
	lda #$00
	sta TPI2+MIR		; TPI2 mir=0 mask all interrrupts
; TOD tests
; enable all CIA interrupts
	lda #$7f			; clear all irq mask bits
	sta CIA+icr
	lda CIA+icr			; clear irq reg

	ldy #$00
	sty tod_state			; init TOD state to 0 = ok
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
	jsr todchk1			; check if TOD increases one 10th
	lda tod_state
	bne todfai1			; branch -> TOD failure
	lda time2_seconds
	beq chk10lp			; check all 10th
	jsr playsnd			; play sound after 1 second
; test last 10th increases a second
	lda #$09
	sta time1_10th
chkslp: lda time2_seconds
	sta time1_seconds
	jsr todchk1
	lda tod_state
	bne todfai1
	lda time2_minutes
	beq chkslp			; check all seconds
	jsr playsnd
; test minutes change
	lda #$59
	sta time1_seconds
chkmlp:	lda time2_minutes
	sta time1_minutes
	jsr todchk1
	lda tod_state
	bne todfai1			; branch -> TOD failure
	lda time2_hours
	cmp #$01
	beq chkmlp			; check all minutes
	jsr playsnd
; test hours change
	lda #$59
	sta time1_minutes
chkhlp:	lda time2_hours
	sta time1_hours
	jsr todchk1
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
	jsr todchk1
	lda tod_state			; load state
todfai1:bne todfail			; branch -> TOD failure
; TOD alarm test
	lda CIA+icr			; clear cia irq reg
	lda #$7f
	sta CIA+icr			; clear all irq mask bits
	lda #$80
	sta CIA+crb			; set bit #7 - TOD ALARM
	lda time2_hours
	sta CIA+todhr		; set ALARM
	lda time2_minutes
	sta CIA+todmin
	lda time2_seconds
	sta CIA+todsec
	lda time2_10th
	clc
	adc #$01
	sta CIA+tod10		; set ALARM to time2 + one 10th
	sty tod_count1			; clear counter
	sty tod_count2
alarmlp:lda CIA+icr
	bne chkalar			; irq -> test for ALARM irq bit #2
	dec tod_count1
	bne alarmlp			; wait for ALARM
	dec tod_count2
	bne alarmlp			; wait for ALARM
	beq todfail			; branch -> TOD failure
chkalar:cmp #$04			; test ALARM irq bit
	beq todend			; skip if tod ALARM OK	
; tod fails
todfail:lda #'B'
	sta ScreenRAM
	lda #'A'
	sta ScreenRAM+1
	lda #'D'
	sta ScreenRAM+2
	brk
todend:	lda #'O'
	sta ScreenRAM
	lda #'K'
	sta ScreenRAM+1
	brk
; ----------------------------------------------------------------------------
; set TOD to time1 and set time2 = time1 + one 10th
; count for TOD change and compares to time2
todchk1:sed				; decimal mode
	sty tod_count1			; clear counter
	sty tod_count2
	sty tod_count3
	lda time1_hours
	sta time2_hours
	sta CIA+todhr		; set TOD starting with hours (halts TOD) to time1
	lda time1_minutes
	sta time2_minutes
	sta CIA+todmin
	lda time1_seconds
	sta time2_seconds
	sta CIA+todsec
	lda time1_10th
	sta CIA+tod10		; set TOD 10th (starts TOD)
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
chktod:	lda CIA+tod10
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
todchg:	lda CIA+todhr		; ************** STOPS TOD
	lda CIA+todsec
	cmp time2_seconds
	bne todbad			; ********** CMOS ERROR: TOD seconds still 1, but time2_seconds=2 **********
	lda CIA+todmin
	cmp time2_minutes
	bne todbad
	lda CIA+todhr
	cmp time2_hours
	beq todok
todbad:	lda #$ff			; state = TOD bad
	sta tod_state
todok:	cld				; reset decimal flag
	rts
; ----------------------------------------------------------------------------
; delay
delay:	ldy #$40
delaylp:dey
	bne delaylp
	rts
; ----------------------------------------------------------------------------
; play ping sound
playsnd:lda #$0f
	sta SID+VOLUME			; volume 15
	lda #$00
	sta SID+OSC1+ATKDCY
	lda #$fa
	sta SID+OSC1+SUSREL
	lda #$40
	sta SID+OSC1+FREQHI
	lda #$80
	sta SID+OSC3+FREQHI
	lda #$15			; OSC1 on
	sta SID+OSC1+OSCCTL
	lda #$14			; OSC1 off
	sta SID+OSC1+OSCCTL
	rts
