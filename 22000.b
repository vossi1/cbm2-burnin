; CBMII b128-256 burnin 
; disassembled by vossi 10/2023
!cpu 6502
!ct scr		; standard text/char conversion table -> Screencode (pet = PETSCII, raw)
!to "load 1.prg", cbm
; ***************************************** CONSTANTS *********************************************
SYSTEMBANK		= $0f		; #SYSTEMBANK
FILL			= $aa
; TPI register
pc			= $02 *2	; TPI port c
mir			= $05 *2	; TPI interrupt mask register
cr			= $06 *2	; TPI control register
; ***************************************** ADDRESSES *********************************************
!addr CodeBank		= $00		; code bank register
!addr IndirectBank	= $01		; indirect bank register
!addr ScreenRAM		= $d000		; Screen RAM
!addr CRT		= $d800		; CRT
; ***************************************** ZERO PAGE *********************************************
!addr pointer1		= $12		; 16bit pointer
!addr screen_pointer	= $14		; 16bit pointer screen
!addr actual_codebank	= $20		; actual code bank
!addr temp_and_value	= $2a		; temp screen data and value
!addr banks		= $2f		; RAM banks
!addr pointer2		= $35		; 16bit pointer
!addr temp2		= $4a		; temp
!addr temp_count	= $4c		; temp/counter
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
!addr tpi1		= $5f		; 16 pointer to TPI1 regs
!addr sid		= $5f		; 29 pointer to SID regs
!addr tpi2		= $6f		; 8 pointer to TPI2 regs
!addr cia		= $7f		; 16 pointer to CIA regs
!addr acia		= $a3		; 4 pointer to ACIA regs
; ******************************************* CODE ************************************************
*= $2000
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
drawscr:jsr clrscrn			; sub: clear screen
	ldx #0
	jsr chipadr			; sub: set pointer2 to chip graphics address
	lda #$f0
	ldy #$d0
	jsr drawics			; sub: draw chip graphics to screen
	lda #$20
	ldy #$d3
	jsr drawics			; sub: draw chip graphics to screen
l2045:	lda #$50
	ldy #$d5
	jsr drawics			; sub: draw chip graphics to screen
	ldx #3
	jsr chipadr			; sub: set pointer2 to chip graphics address
	lda #$30
l2053:	ldy #$d2
	jsr drawics			; sub: draw chip graphics to screen
	lda #$60
	ldy #$d4
	jsr drawics			; sub: draw chip graphics to screen
	lda #$90
	ldy #$d6
	jsr drawics			; sub: draw chip graphics to screen
	ldx #2
	jsr chipadr			; sub: set pointer2 to chip graphics address
	lda #$e0
	ldy #$d1
	jsr drawics			; sub: draw chip graphics to screen
	lda #$10
	ldy #$d4
	jsr drawics			; sub: draw chip graphics to screen
	lda #$40
	ldy #$d6
	jsr drawics			; sub: draw chip graphics to screen
	ldx #1
	jsr chipadr			; sub: set pointer2 to chip graphics address
	lda #$40
	ldy #$d1
	jsr drawics			; sub: draw chip graphics to screen
	lda #$70
	ldy #$d3
	jsr drawics			; sub: draw chip graphics to screen
	lda #$a0
	ldy #$d5
	jsr drawics			; sub: draw chip graphics to screen
; draw screen text
	ldx #$00
	jsr drawtxt			; sub: draw screen text
	ldx #$01
	jsr drawtxt			; sub: draw screen text
	ldx #$02
	jsr drawtxt			; sub: draw screen text
	ldx #$2b
	jsr drawtxt			; sub: draw screen text
	jsr itpi2pt			; sub: init tpi2 pointer
	lda #SYSTEMBANK
	sta IndirectBank
	ldy #$00
	lda (tpi2+cr),y
	and #$fe			; tpi2 mc=0 disable interrupt controller
	sta (tpi2+cr),y
	lda #$00
	sta (tpi2+mir),y		; tpi2 mir=0 mask all interrrupts
	lda (tpi2+pc),y			; read tpi2 port c
	and #$80			; isolate bit#7 low/high profile
	bne hiprof			; branch if high profile
; low profile
	ldx #$0b
	stx temp_count
	ldx #$03			; lines *---- ----* and low-ics
	stx temp1
	bne drwiclp			; jump always
; high profile
hiprof:	ldx #$2f
	jsr drawtxt			; sub: draw screen text "HIGH"
	ldx #$08
	stx temp_count
	ldx #$06			; lines *---- ----* 
	stx temp1
-	ldx temp1
	jsr drawpur			; sub: draw without and
	inc temp1
	dec temp_count
	bne -
	ldx #$03
	stx temp_count
	ldx #$30			; high-ics
	stx temp1
; draw lines+ics low / ics only high
drwiclp:ldx temp1
	jsr drawpur
	inc temp1
	dec temp_count
	bne drwiclp
; draw segment, execute, test
	ldx #$14
	stx temp_count
	ldx #$17			; segment, execute, test
	stx temp1
-	ldx temp1
	jsr drawtxt			; sub: draw screen text
	inc temp1
	dec temp_count
	bne -
; draw multiple chars to screen positions from table
	ldx #$0c			; char to draw
	stx temp2
--	lda pos_count,x			; positions to draw
	sta temp_count
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
	ldy temp_count
	lda (pointer3),y		; load screen postion
	sta pointer1
	lda (pointer4),y
	sta pointer1+1
	lda #SYSTEMBANK
	sta IndirectBank
	lda temp1
	ldy #$00
	sta (pointer1),y		; draw char to screen
	ldx temp_count
	dex
	stx temp_count
	bpl -				; next postion
	ldx temp2
	dex
	stx temp2
	bpl --				; next char
;
	lda banks
	cmp #$04
	beq l216b
	ldx #$38
	jsr drawtxt			; sub: draw screen text
	ldx #$37
	jsr drawtxt			; sub: draw screen text
l216b:	jmp l21cb
; ----------------------------------------------------------------------------
; draw chip graphics to screen
drawics:sta pointer1
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
	stx IndirectBank		; set indirect bank to systembank
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
chipadr:lda chip_data_lo,x
	sta pointer2
	lda chip_data_hi,x
	sta pointer2+1
	rts
; ----------------------------------------------------------------------------
; Clear screen
clrscrn:lda #>ScreenRAM
	sta pointer1+1
	ldy #<ScreenRAM
	sty pointer1
	lda #SYSTEMBANK
	sta IndirectBank
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
; 
l21cb:	jsr isidpt			; sub: init sid pointer
	jsr icrtpt			; sub: init crt pointer
	lda #SYSTEMBANK
	sta IndirectBank
	jsr l2275
	jsr l229e
	jsr l22f2
	jsr l2259
	jsr l2259
	jsr l2259
	jsr l2259
	jsr l2259
	jsr l2259
	jsr l2259
	jsr l2259
	jsr l2259
	jsr l2259
	jsr l2259
	jsr l2259
	jsr l2259
	jsr l2259
	jsr l2372
	jsr l2592
	jsr l2705
	ldx #$03
	sed
	sec
l2215:	lda $21,x
	adc #$00
	sta $21,x
	bcc l2220
	dex
	bpl l2215
l2220:	lda #$2d
	sta pointer1
	lda #$d0
	sta pointer1+1
	lda #SYSTEMBANK
	sta IndirectBank
	ldx #$ff
	ldy #$00
l2230:	inx
	cpx #$04
	bne l2238
	dex
	bne l2246
l2238:	lda $21,x
	beq l2230
	and #$f0
	beq l2246
l2240:	jsr l2c2e
	sta (pointer1),y
	iny
l2246:	lda $21,x
	jsr l2c32
	sta (pointer1),y
	iny
	inx
	lda $21,x
	cpx #$04
	bne l2240
	cld
	jmp l21cb
l2259:	rts
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
; 
l2275:	ldy #$00
	lda IndirectBank
	sta actual_indirbank
	lda #SYSTEMBANK
	sta IndirectBank
	sta ($8f),y
	lda #$00
	sta ($69),y
	lda #$fa
	sta ($6b),y
	lda #$40
	sta ($61),y
	lda #$80
	sta ($7d),y
	lda #$15
	sta ($67),y
	lda #$14
	sta ($67),y
	lda actual_indirbank
	sta IndirectBank
	rts
; ----------------------------------------------------------------------------
; 
l229e:	ldy #$47
	lda #$00
	sta pointer1
	sta temp2
	lda #$33
	sta pointer1+1
	sec
	sbc #$20
	tax
	lda CodeBank
	sta IndirectBank
l22b2:	lda (pointer1),y
	eor temp2
	sta temp2
	dey
	bne l22b2
	dec pointer1+1
	dex
	bpl l22b2
	lda $1d
	bne l22ca
	dec $1d
	lda temp2
	sta $1e
l22ca:	lda temp2
	cmp $1e
	beq l22d5
	ldx #$2e
	jsr drwor80
l22d5:	lda temp2
	jsr l2c28
	sty temp2
	ldy #$00
	ldx #$2e
	stx pointer1
	ldx #$d7
	stx pointer1+1
	ldx #SYSTEMBANK
	stx IndirectBank
	sta (pointer1),y
	iny
	lda temp2
	sta (pointer1),y
	rts
; ----------------------------------------------------------------------------
; 
l22f2:	clv
	lda #SYSTEMBANK
	sta IndirectBank
	ldy #$00
	ldx #$20
	lda #$e0
	jsr l2323
	ldy #$05
	ldx #$20
	lda banks
	cmp #$04
	beq l230e
	lda $a0
	bvc l2310
l230e:	lda #$a0
l2310:	jsr l2323
	ldy #$0a
	ldx #$20
	lda banks
	cmp #$04
	beq l2321
	lda $80
	bvc l2323
l2321:	lda #$80
l2323:	sty $39
	sta pointer1+1
	dex
	txa
	clc
	adc pointer1+1
	sta pointer1+1
	lda #$00
	sta temp_count
	sta pointer1
	tay
l2335:	clc
	lda (pointer1),y
	adc temp_count
	adc #$00
	adc #$00
	sta temp_count
	dey
	bne l2335
	dec pointer1+1
	dex
	bpl l2335
	lda $39
	clc
	adc #$a1
	sta pointer3
	lda #$d6
	adc #$00
	sta pointer3+1
	lda temp_count
	jsr l2c28
	sty temp2
	ldy #$00
	sta (pointer3),y
	iny
	lda temp2
	sta (pointer3),y
	rts
; ----------------------------------------------------------------------------
; 
	lda temp_count
	inc pointer1+1
	cmp pointer1+1
	beq l2371
	jsr l2b3e
l2371:	rts
; ----------------------------------------------------------------------------
; 
l2372:	sei
	ldx #$36
	jsr drawtxt			; sub: draw screen text
	jsr l2cb9
	jsr iciapt			; sub: init cia pointer
	lda #SYSTEMBANK
	sta IndirectBank
	jsr l2586
	ldy #$00
	lda #$00
	sta ($9d),y
	lda #$08
	sta ($9b),y
	sty $16
	ldx #$01
	jsr l2527
	beq l239a
	dec $16
l239a:	jsr l2549
	bne l23a1
	dec $16
l23a1:	ldx #$01
	lda #$00
	sta ($9b),y
	jsr l2527
	beq l23ae
	dec $16
l23ae:	ldx #$01
	jsr l2549
	beq l23b7
	dec $16
l23b7:	lda ($9b),y
	and #$fe
	sta ($9b),y
	lda #$08
	sta ($9d),y
	ldx #$02
	jsr l2538
	beq l23ca
	dec $16
l23ca:	lda ($9d),y
	and #$fe
	sta ($9d),y
	lda #$40
	sta temp_dec_value
l23d4:	lda #$00
	sta ($9b),y
	sta ($9d),y
	lda #$55
	sta ($87),y
	sta ($89),y
	lda ($87),y
	cmp #$55
	beq l23e8
	dec $16
l23e8:	lda ($89),y
	cmp #$55
	beq l23f0
	dec $16
l23f0:	lda #$aa
	sta ($89),y
	sta ($87),y
	lda ($87),y
	cmp #$55
	beq l23fe
	dec $16
l23fe:	lda ($89),y
	cmp #$aa
	beq l2408
	lda #$ff
	sta $16
l2408:	lda #$10
	sta ($9b),y
	lda ($87),y
	cmp #$aa
	beq l2414
	dec $16
l2414:	lda ($89),y
	cmp #$aa
	beq l241c
	dec $16
l241c:	lda #$55
	sta ($8b),y
l2420:	sta ($8d),y
	lda ($8b),y
	cmp #$55
	beq l242a
	dec $16
l242a:	lda ($8d),y
	cmp #$55
	beq l2432
	dec $16
l2432:	lda #$aa
	sta ($8d),y
	sta ($8b),y
	lda ($8b),y
	cmp #$55
	beq l2440
	dec $16
l2440:	lda ($8d),y
	cmp #$aa
	beq l2448
	dec $16
l2448:	lda #$10
	sta ($9d),y
	lda ($8b),y
	cmp #$aa
	beq l2454
	dec $16
l2454:	lda ($8d),y
	cmp #$aa
	beq l245c
	dec $16
l245c:	lda #$09
	sta ($9b),y
	lda #$cc
	sta ($87),y
	sta ($89),y
	lda ($87),y
	cmp #$aa
	bmi l246e
	dec $16
l246e:	lda ($89),y
	cmp #$aa
	beq l2476
	dec $16
l2476:	lda #$19
	ldx #$00
	sta ($9b),y
	txa
	sta ($9b),y
	lda ($87),y
	and #$fe
	cmp #$c4
	beq l2489
	dec $16
l2489:	lda ($89),y
	cmp #$cc
	beq l2491
	dec $16
l2491:	lda #$09
	sta ($9d),y
	lda #$cc
	sta ($8b),y
	sta ($8d),y
	lda ($8b),y
	cmp #$aa
	bmi l24a3
	dec $16
l24a3:	lda ($8d),y
	cmp #$aa
	beq l24ab
	dec $16
l24ab:	lda #$19
	ldx #$00
	sta ($9d),y
	txa
	sta ($9d),y
	lda ($8b),y
	and #$fe
	cmp #$c4
	beq l24be
	dec $16
l24be:	lda ($8d),y
	cmp #$cc
	beq l24c6
	dec $16
l24c6:	dec temp_dec_value
	bmi l24cd
	jmp l23d4
l24cd:	lda #$00
	sta ($89),y
	sta ($8d),y
	lda #$01
	sta ($8b),y
	lda #$08
	sta ($87),y
	lda #$51
	sta ($9d),y
	lda #$19
	sta ($9b),y
	tax
l24e4:	dex
	bne l24e4
	txa
	sta ($9b),y
	sta ($9d),y
	lda ($8d),y
	beq l24f2
	dec $16
l24f2:	lda ($8b),y
	beq l24f8
	dec $16
l24f8:	lda ($89),y
	cmp #$00
	beq l2500
	dec $16
l2500:	lda ($87),y
	cmp #$08
	beq l2508
	dec $16
l2508:	lda $16
	beq l2526
	lda #$ff
	sta $1c
	lda #$bf
	sta pointer3
	lda #$d5
	sta pointer3+1
	jsr l2b3e
	lda $1b
	ldx #$2d
	bne l2523
	ldx #$34
l2523:	jsr drawtxt			; sub: draw screen text
l2526:	rts
; ----------------------------------------------------------------------------
; 
l2527:	lda #$88
	sta ($87),y
	sta ($89),y
	lda ($9b),y
	ora #$01
	sta ($9b),y
	jsr l257e
	bne l2549
l2538:	lda #$88
	sta ($8b),y
	sta ($8d),y
	lda ($9d),y
	ora #$01
	sta ($9d),y
	jsr l257e
	bne l2549
l2549:	jsr l258c
	txa
	sta $39
	sta ($99),y
	ldx #$00
	stx $3b
l2555:	lda ($99),y
	bne l2567
	inx
	bne l2555
	inc $3b
	lda #$0f
	cmp $3b
	bpl l2555
	ldx $3b
	rts
; ----------------------------------------------------------------------------
; 
l2567:	and $39
	cmp $39
	beq l256f
	dec $16
l256f:	cpx #$db
	beq l2575
	dec $16
l2575:	ldx $3b
	cpx #$0a
	beq l257d
	dec $16
l257d:	rts
; ----------------------------------------------------------------------------
; 
l257e:	lda #$05
	clc
l2581:	sbc #$01
	bpl l2581
	rts
; ----------------------------------------------------------------------------
; 
l2586:	ldy #$00
	lda #$7f
	sta ($99),y
l258c:	ldy #$00
	lda ($99),y
	rts
; ----------------------------------------------------------------------------
; 
	rti
l2592:	sei
	ldx #$35
	jsr drawtxt			; sub: draw screen text
	jsr iciapt			; sub: init cia pointer
	jsr l2586
	ldy #$00
	sty $16
	lda #SYSTEMBANK
	sta IndirectBank
	sty $43
	sty $44
	sty $45
	sty $47
	sty $48
	sty $49
	lda #$01
	sta $42
	sta $46
l25b8:	lda $45
	sta $49
	jsr l2670
	lda $16
	bne l261e
	lda $44
	beq l25b8
	jsr l2275
	lda #$09
	sta $49
l25ce:	lda $44
	sta $48
	jsr l2670
	lda $16
	bne l261e
	lda $43
	beq l25ce
	jsr l2275
	lda #$59
	sta $48
l25e4:	lda $43
	sta $47
	jsr l2670
	lda $16
	bne l261e
	lda $42
	cmp #$01
	beq l25e4
	jsr l2275
	lda #$59
	sta $47
l25fc:	lda $42
	sta $46
	jsr l2670
	lda $16
	bne l261e
	lda $42
	cmp #$01
	bne l2613
	lda #$81
	sta $42
	bne l25fc
l2613:	cmp #$12
	bne l25fc
	sta $46
	jsr l2670
	lda $16
l261e:	bne l2653
	lda ($99),y
	lda #$7f
	sta ($99),y
	lda #$80
	sta ($9d),y
	lda $42
	sta ($95),y
	lda $43
	sta ($93),y
	lda $44
	sta ($91),y
	lda $45
	clc
	adc #$01
	sta ($8f),y
	sty pointer1
	sty pointer1+1
l2641:	lda ($99),y
	bne l264f
	dec pointer1
	bne l2641
	dec pointer1+1
	bne l2641
	beq l2653
l264f:	cmp #$04
	beq l266f
l2653:	lda #$ff
	sta $1b
	lda #$bf
	sta pointer3
	lda #$d5
	sta pointer3+1
	jsr l2b3e
	lda $1c
	bne l266f
	ldx #$33
	bne l266c
	ldx #$34
l266c:	jsr drawtxt			; sub: draw screen text
l266f:	rts
; ----------------------------------------------------------------------------
; 
l2670:	sed
	sty pointer1
	sty pointer1+1
	sty $4b
	lda $46
	sta $42
	sta ($95),y
	lda $47
	sta $43
	sta ($93),y
	lda $48
	sta $44
	sta ($91),y
	lda $49
	sta ($8f),y
	clc
	adc #$01
	sta $45
	cmp #$10
	bne l26d5
	sty $45
	clc
	lda $44
	adc #$01
	sta $44
	cmp #$60
	bne l26d5
	sty $44
	clc
	lda $43
	adc #$01
	sta $43
	cmp #$60
	bne l26d5
	sty $43
	clc
	lda $42
	adc #$01
	sta $42
	and #$1f
	cmp #$13
	bne l26c7
	lda $42
	and #$81
	sta $42
	bne l26cf
l26c7:	cmp #$12
	beq l26cf
	cmp #$01
	bne l26d5
l26cf:	lda #$80
	eor $42
	sta $42
l26d5:	lda ($8f),y
	cmp $49
	bne l26e9
	dec pointer1
	bne l26d5
	dec pointer1+1
	bne l26d5
	dec $4b
	bne l26d5
	beq l26ff
l26e9:	cmp $45
	bne l26ff
	lda ($91),y
	cmp $44
	bne l26ff
	lda ($93),y
	cmp $43
	bne l26ff
	lda ($95),y
	cmp $42
	beq l2703
l26ff:	lda #$ff
	sta $16
l2703:	cld
	rts
; ----------------------------------------------------------------------------
; 
l2705:	ldy banks
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
	jsr drawtxt			; sub: draw screen text
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
	jsr l2275
	ldx #$0e
	jsr drawtxt			; sub: draw screen text
l2805:	ldy $41
	lda $40
	sta pointer1+1
l280b:	tya
	sta temp_count
	sta (pointer1),y
	lda (pointer1),y
	eor temp_count
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
	sta temp_count
	lda (pointer1),y
	eor temp_count
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
	sta temp_count
	lda #$aa
	sta temp1
l285a:	lda (pointer1),y
	eor pointer1+1
	beq l2863
	jsr l2a59
l2863:	lda #$55
	sta (pointer1),y
	lda (pointer1),y
	eor temp_count
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
	eor temp_count
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
	eor temp_count
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
	eor temp_count
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
	jsr l2275
	ldx #$13
	jsr l2b95
	ldx #$5a
	stx temp_count
	ldx #$a5
	stx temp1
l291e:	lda (pointer1),y
	eor temp_count
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
	eor temp_count
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
	eor temp_count
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
	eor temp_count
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
	eor temp_count
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
	stx temp_count
	ldx #$ff
	stx temp1
l29d3:	txa
	lda (pointer1),y
	eor temp1
	beq l29dd
	jsr l2a59
l29dd:	sta (pointer1),y
	lda (pointer1),y
	eor temp_count
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
	eor temp_count
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
	jsr l2c28
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
	jsr l2c28
	tya
	ldy #$03
	sta (pointer3),y
	lda pointer1+1
	jsr l2c28
	sty temp2
	ldy #$05
	sta (pointer3),y
	iny
	lda temp2
	sta (pointer3),y
	lda $29
	jsr l2c28
	sty temp2
	ldy #$07
	sta (pointer3),y
	iny
	lda temp2
	sta (pointer3),y
	lda CodeBank
	jsr l2c28
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
	jsr l2b3e
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
	jsr l2b3e
	bmi l2b11
l2b2f:	lda #$a6
	sta pointer3
	lda #$d5
	sta pointer3+1
	lda CodeBank
	jsr l2b40
	bmi l2b11
l2b3e:	lda #SYSTEMBANK
l2b40:	sta IndirectBank
	tya
	pha
	ldy #$00
	lda (pointer3),y
	bmi l2b7b
	lda pointer3
	sta pointer4
	lda pointer3+1
	sta pointer4+1
	ldx #$02
l2b54:	ldy #$02
l2b56:	lda (pointer4),y
	ora #$80
	sta (pointer4),y
	dey
	bpl l2b56
	lda #$50
	clc
	adc pointer4
	sta pointer4
	bcc l2b6a
	inc pointer4+1
l2b6a:	dex
	bne l2b54
	ldy #$02
l2b6f:	lda chip_bad,y
	and #$3f
	ora #$80
	sta (pointer4),y
	dey
	bpl l2b6f
l2b7b:	pla
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
	jsr drawtxt			; sub: draw screen text
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
	jsr drawtxt			; sub: draw screen text
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
l2be8:	lda temp_count
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
	stx temp_count
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
; 
l2c28:	pha
	jsr l2c32
	tay
	pla
l2c2e:	lsr
	lsr
	lsr
	lsr
l2c32:	and #$0f
	cmp #$0a
	bmi l2c3d
	sec
	sbc #$09
	bne l2c3f
l2c3d:	ora #$30
l2c3f:	rts
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
iciapt:	lda #cia
	sta pointer1
	lda #$00
	sta pointer1+1
	lda #<ciaregs
	ldx #>ciaregs
	ldy #$1f
	jsr cpydata			; sub: copy data
	rts
; ----------------------------------------------------------------------------
; init tpi2 pointer
itpi2pt:lda #tpi2
	sta pointer1
	lda #$00
	sta pointer1+1
	lda #<tpi2regs
	ldx #>tpi2regs
	ldy #$0f
	jsr cpydata			; sub: copy data
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
	jsr cpydata			; sub: copy data
	rts
; ----------------------------------------------------------------------------
; init crt pointer
icrtpt:	lda #<CRT
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
	jsr cpydata			; sub: copy data
	rts
; ----------------------------------------------------------------------------
; init sid pointer
isidpt:	lda #sid
	sta pointer1
	lda #$00
	sta pointer1+1
	lda #<sidregs
	ldx #>sidregs
	ldy #$39
	jsr cpydata			; sub: copy data
	rts
; ----------------------------------------------------------------------------
; 
l2cb9:	lda #$f0
	sta $fffa
	lda #$00
	sta $fffb
	lda #$f2
	sta $fffc
	lda #$00
	sta $fffd
	lda #$f4
	sta $fffe
	lda #$00
	sta $ffff
	lda #$91
	sta $f0
	sta $f2
	sta $f4
	lda #$25
	sta $f1
	sta $f3
	sta $f5
	rts
; ----------------------------------------------------------------------------
; copy data y+1 bytes from ax to pointer1
cpydata:sta pointer3
	stx pointer3+1
	lda CodeBank
	sta IndirectBank
-	lda (pointer3),y
	sta (pointer1),y
	dey
	bpl -
	rts
; ----------------------------------------------------------------------------
; draw with OR $80
drwor80:pha
	tya
	pha
	txa
	pha
	lda #$80
	sta temp_or_value
	bne drawtx2			; jump always
; draw pure withour AND
drawpur:pha
	tya
	pha
	txa
	pha
	lda #$00
	sta temp_or_value
	lda #$ff			; data and value
	bne drawtx1			; jump always
; draw screen text
drawtxt:pha				; save regs
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
timertsts:	!scr "6526 TIMERS TESTS "
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
bchksum:	!scr " * *  BAD PROGRAM CHECKSUM  * * "
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
		!byte <test, <test, <test, <ics, <staticram, <tmr, <bchksum, <high
		!byte <ichigh1, <ichigh2, <ichigh3, <tod, <tnt, <todtests, <timertsts, <noram
		!byte <noram
; screendata addresses hi
scrdata_hi:	!byte >title, >ramsegf, >romssegf, >iclow1, >iclow2, >iclow3, >line, >line
		!byte >line, >line, >(line+1), >(line+1), >(line+1), >(line+1), >loadrtest, >hiadrtest
		!byte >chkrbrd, >aa55, >marchinc, >decadra5, >dec5a, >incadr, >decadr00, >segment
		!byte >segment, >segment, >segment, >segment, >segment, >segment, >segment, >segment
		!byte >segment, >segment, >segment, >execute, >execute, >execute, >execute, >test
		!byte >test, >test, >test, >ics, >staticram, >tmr, >bchksum, >high
		!byte >ichigh1, >ichigh2, >ichigh3, >tod, >tnt, >todtests, >timertsts, >noram
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
pos2lo:		!byte $e0, $0b, $33, $3b, $63, $56, $a6
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
