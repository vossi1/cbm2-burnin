; CBMII b128-256 burnin 
; disassembled by vossi 10/2023
!cpu 6502
!to "load 1.prg", cbm
; ***************************************** CONSTANTS *********************************************
SYSTEMBANK		= $0f		; #SYSTEMBANK
; ***************************************** ADDRESSES *********************************************
!addr CodeBank		= $00		; code bank register
!addr IndirectBank	= $01		; indirect bank register
;!addr ScreenRAM		= $d000		; Screen RAM
; ***************************************** ZERO PAGE *********************************************
!addr pointer1		= $12		; 16bit pointer
!addr banks		= $2f		; RAM banks
; ******************************************* CODE ************************************************
*= $2000
	sei
	cld
	ldx #$ff
	txs
	ldy #$02			; clear zero page
	lda #$00
clrzplp:sta $0000,y
	iny
	bne clrzplp
	ldx #$02		; ***** check if 128k or 256k RAM
	stx banks			; default 2 banks
	inx
	stx IndirectBank		; set bank 3 as indirect bank
	lda #$60
	sta pointer1+1			; check from $6000
	lda #$a5
	sta $4a				; remember test byte $a5
chkbklp:sta (pointer1),y
	lda (pointer1),y
	cmp $4a				; check if RAM here
	beq cbm256			; RAM in bank 3
	iny
	bne chkbklp			; check next byte
	beq cbm128			; no RAM from $6000 to $60ff
cbm256:	lda #$04
	sta banks			; store 4 banks
cbm128:	jsr l21b0
	ldx #$00
	jsr l21a5
	lda #$f0
	ldy #$d0
	jsr l216e
	lda #$20
	ldy #$d3
	jsr l216e
l2045:	lda #$50
	ldy #$d5
	jsr l216e
	ldx #$03
	jsr l21a5
	lda #$30
l2053:	ldy #$d2
	jsr l216e
	lda #$60
	ldy #$d4
	jsr l216e
	lda #$90
	ldy #$d6
	jsr l216e
	ldx #$02
	jsr l21a5
	lda #$e0
	ldy #$d1
	jsr l216e
	lda #$10
	ldy #$d4
	jsr l216e
	lda #$40
	ldy #$d6
	jsr l216e
	ldx #$01
	jsr l21a5
	lda #$40
	ldy #$d1
	jsr l216e
	lda #$70
	ldy #$d3
	jsr l216e
	lda #$a0
	ldy #$d5
	jsr l216e
	ldx #$00
	jsr l2d10
	ldx #$01
	jsr l2d10
	ldx #$02
	jsr l2d10
	ldx #$2b
	jsr l2d10
	jsr l2c60
	lda #SYSTEMBANK
	sta IndirectBank
	ldy #$00
	lda ($7b),y
	and #$fe
	sta ($7b),y
	lda #$00
	sta ($79),y
	lda ($73),y
	and #$80
	bne l20d1
	ldx #$0b
	stx $4c
	ldx #$03
	stx $4d
	bne l20f1
l20d1:	ldx #$2f
	jsr l2d10
	ldx #$08
	stx $4c
	ldx #$06
	stx $4d
l20de:	ldx $4d
	jsr l2d03
	inc $4d
	dec $4c
	bne l20de
	ldx #$03
	stx $4c
	ldx #$30
	stx $4d
l20f1:	ldx $4d
	jsr l2d03
	inc $4d
	dec $4c
	bne l20f1
	ldx #$14
	stx $4c
	ldx #$17
	stx $4d
l2104:	ldx $4d
	jsr l2d10
	inc $4d
	dec $4c
	bne l2104
	ldx #$0c
	stx $4a
l2113:	lda l32f9,x
	sta $4c
	lda l333a,x
	and #$3f
	sta $4d
l211f:	ldx $4a
	lda l3306,x
	sta $5b
	lda l3313,x
	sta $5c
	lda l3320,x
	sta $5d
	lda l332d,x
	sta $5e
	lda CodeBank
	sta IndirectBank
	ldy $4c
	lda ($5b),y
	sta pointer1
	lda ($5d),y
	sta pointer1+1
	lda #SYSTEMBANK
	sta IndirectBank
	lda $4d
	ldy #$00
	sta (pointer1),y
	ldx $4c
	dex
	stx $4c
	bpl l211f
	ldx $4a
	dex
	stx $4a
	bpl l2113
	lda banks
	cmp #$04
	beq l216b
	ldx #$38
	jsr l2d10
	ldx #$37
	jsr l2d10
l216b:	jmp l21cb
l216e:	sta pointer1
	sty pointer1+1
	ldx CodeBank
	stx $20
	ldx #$10
	stx $1f
	txa
	jsr l2c46
	sec
	sbc #$01
	sta $3e
l2183:	ldy #$04
l2185:	lda $20
	sta IndirectBank
	lda ($35),y
	dey
	sty $3d
	ldy $3e
	ldx #SYSTEMBANK
	stx IndirectBank
	sta (pointer1),y
	dey
	sty $3e
	ldy $3d
	bpl l2185
	ldx $1f
	dex
	stx $1f
	bne l2183
	rts
l21a5:	lda l3287,x
	sta $35
	lda l328c,x
	sta $36
	rts
l21b0:	lda #$d0
	sta pointer1+1
	ldy #$00
	sty pointer1
	lda #SYSTEMBANK
	sta IndirectBank
	lda #$20
	ldx #$08
l21c0:	sta (pointer1),y
	iny
	bne l21c0
	inc pointer1+1
	dex
	bne l21c0
	rts
l21cb:	jsr l2ca7
	jsr l2c84
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
l225a:	lda IndirectBank
	sta $58
	lda #SYSTEMBANK
	sta IndirectBank
	ldy #$00
	lda #$0c
	sta ($9f),y
	lda $3f
	eor #$20
	sta ($a1),y
	sta $3f
	lda $58
	sta IndirectBank
	rts
l2275:	ldy #$00
	lda IndirectBank
	sta $58
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
	lda $58
	sta IndirectBank
	rts
l229e:	ldy #$47
	lda #$00
	sta pointer1
	sta $4a
	lda #$33
	sta pointer1+1
	sec
	sbc #$20
	tax
	lda CodeBank
	sta IndirectBank
l22b2:	lda (pointer1),y
	eor $4a
	sta $4a
	dey
	bne l22b2
	dec pointer1+1
	dex
	bpl l22b2
	lda $1d
	bne l22ca
	dec $1d
	lda $4a
	sta $1e
l22ca:	lda $4a
	cmp $1e
	beq l22d5
	ldx #$2e
	jsr l2cf8
l22d5:	lda $4a
	jsr l2c28
	sty $4a
	ldy #$00
	ldx #$2e
	stx pointer1
	ldx #$d7
	stx pointer1+1
	ldx #SYSTEMBANK
	stx IndirectBank
	sta (pointer1),y
	iny
	lda $4a
	sta (pointer1),y
	rts
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
	sta $4c
	sta pointer1
	tay
l2335:	clc
	lda (pointer1),y
	adc $4c
	adc #$00
	adc #$00
	sta $4c
	dey
	bne l2335
	dec pointer1+1
	dex
	bpl l2335
	lda $39
	clc
	adc #$a1
	sta $5b
	lda #$d6
	adc #$00
	sta $5c
	lda $4c
	jsr l2c28
	sty $4a
	ldy #$00
	sta ($5b),y
	iny
	lda $4a
	sta ($5b),y
	rts
	lda $4c
	inc pointer1+1
	cmp pointer1+1
	beq l2371
	jsr l2b3e
l2371:	rts
l2372:	sei
	ldx #$36
	jsr l2d10
	jsr l2cb9
	jsr l2c4e
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
	sta $55
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
l24c6:	dec $55
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
	sta $5b
	lda #$d5
	sta $5c
	jsr l2b3e
	lda $1b
	ldx #$2d
	bne l2523
	ldx #$34
l2523:	jsr l2d10
l2526:	rts
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
l257e:	lda #$05
	clc
l2581:	sbc #$01
	bpl l2581
	rts
l2586:	ldy #$00
	lda #$7f
	sta ($99),y
l258c:	ldy #$00
	lda ($99),y
	rts
	rti
l2592:	sei
	ldx #$35
	jsr l2d10
	jsr l2c4e
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
	sta $5b
	lda #$d5
	sta $5c
	jsr l2b3e
	lda $1c
	bne l266f
	ldx #$33
	bne l266c
	ldx #$34
l266c:	jsr l2d10
l266f:	rts
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
l27a3:	ldx #$2c
	jsr l2d10
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
	jsr l2d10
l2805:	ldy $41
	lda $40
	sta pointer1+1
l280b:	tya
	sta $4c
	sta (pointer1),y
	lda (pointer1),y
	eor $4c
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
	sta $4c
	lda (pointer1),y
	eor $4c
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
	sta $4c
	lda #$aa
	sta $4d
l285a:	lda (pointer1),y
	eor pointer1+1
	beq l2863
	jsr l2a59
l2863:	lda #$55
	sta (pointer1),y
	lda (pointer1),y
	eor $4c
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
	eor $4d
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
	eor $4c
	beq l28a0
	jsr l2a59
l28a0:	lda #$aa
	sta (pointer1),y
	lda (pointer1),y
	eor $4d
	beq l28ad
	jsr l2a59
l28ad:	iny
	lda (pointer1),y
	eor $4d
	beq l28b7
	jsr l2a59
l28b7:	lda #$55
	sta (pointer1),y
	lda (pointer1),y
	eor $4c
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
	eor $4d
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
	eor $4c
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
	stx $4c
	ldx #$a5
	stx $4d
l291e:	lda (pointer1),y
	eor $4c
	beq l2927
	jsr l2a59
l2927:	txa
	sta (pointer1),y
	lda (pointer1),y
	eor $4d
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
	eor $4c
	beq l2949
	jsr l2a59
l2949:	txa
	sta (pointer1),y
	lda (pointer1),y
	eor $4d
	beq l2955
	jsr l2a59
l2955:	dey
	cpy $26
	bne l2940
	ldx #$14
	jsr l2b95
	ldx #$5a
l2961:	lda (pointer1),y
	eor $4d
	beq l296a
	jsr l2a59
l296a:	txa
	sta (pointer1),y
	lda (pointer1),y
	eor $4c
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
	eor $4d
	beq l298c
	jsr l2a59
l298c:	txa
	sta (pointer1),y
	lda (pointer1),y
	eor $4c
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
	eor $4c
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
	stx $4c
	ldx #$ff
	stx $4d
l29d3:	txa
	lda (pointer1),y
	eor $4d
	beq l29dd
	jsr l2a59
l29dd:	sta (pointer1),y
	lda (pointer1),y
	eor $4c
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
	eor $4d
	beq l29ff
	jsr l2a59
l29ff:	sta (pointer1),y
	lda (pointer1),y
	eor $4c
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
	jsr l2c40
	tay
	lda #$2a
	sta (pointer1),y
	ldx $31
	dex
l2a2a:	txa
	jsr l2c40
	tay
	lda #$20
	sta (pointer1),y
	rts
l2a34:	lda #$80
	sta pointer1
	lda #$d7
	sta pointer1+1
	ldx #SYSTEMBANK
	stx IndirectBank
	ldx CodeBank
	dex
	txa
	jsr l2c40
	tay
	lda #$2a
	sta (pointer1),y
	ldx $30
	dex
	txa
	jsr l2c40
	tay
	lda #$20
	sta (pointer1),y
	rts
l2a59:	clv
	sta $27
	stx $28
	sty $29
	jsr l2c28
	ldx IndirectBank
	stx $58
	cpx #SYSTEMBANK
	bne l2a6f
	ldx CodeBank
	bne l2a71
l2a6f:	ldx #SYSTEMBANK
l2a71:	stx IndirectBank
	sty $4a
	ldy #$00
	ldx #$e0
	stx $5b
	ldx #$d6
	stx $5c
	sta ($5b),y
	iny
	lda $4a
	sta ($5b),y
	lda $58
	jsr l2c28
	tya
	ldy #$03
	sta ($5b),y
	lda pointer1+1
	jsr l2c28
	sty $4a
	ldy #$05
	sta ($5b),y
	iny
	lda $4a
	sta ($5b),y
	lda $29
	jsr l2c28
	sty $4a
	ldy #$07
	sta ($5b),y
	iny
	lda $4a
	sta ($5b),y
	lda CodeBank
	jsr l2c28
	tya
	ldy #$0b
	sta ($5b),y
	ldy $58
	cpy #$0f
	beq l2b1c
	dey
	lda #$ff
	sta $0017,y
	tya
	jsr l2c40
	tay
	lda #$31
	sta $5b
	lda #$d7
	sta $5c
	lda ($5b),y
	bmi l2ae3
	ldx #$13
l2ad9:	lda ($5b),y
	ora #$80
	sta ($5b),y
	iny
	dex
	bne l2ad9
l2ae3:	ldy $58
	dey
	lda l3103,y
	sta $5b
	lda l3107,y
	sta $5c
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
	adc $5b
	sta $5b
	bcc l2b0a
	inc $5c
l2b0a:	ldx $56
	dex
	stx $56
	bne l2af4
l2b11:	ldx $58
	stx IndirectBank
	ldx $28
	ldy $29
	lda #$00
	rts
l2b1c:	lda pointer1+1
	and #$f0
	bne l2b2f
	lda #$a1
	sta $5b
	lda #$d5
	sta $5c
	jsr l2b3e
	bmi l2b11
l2b2f:	lda #$a6
	sta $5b
	lda #$d5
	sta $5c
	lda CodeBank
	jsr l2b40
	bmi l2b11
l2b3e:	lda #SYSTEMBANK
l2b40:	sta IndirectBank
	tya
	pha
	ldy #$00
	lda ($5b),y
	bmi l2b7b
	lda $5b
	sta $5d
	lda $5c
	sta $5e
	ldx #$02
l2b54:	ldy #$02
l2b56:	lda ($5d),y
	ora #$80
	sta ($5d),y
	dey
	bpl l2b56
	lda #$50
	clc
	adc $5d
	sta $5d
	bcc l2b6a
	inc $5e
l2b6a:	dex
	bne l2b54
	ldy #$02
l2b6f:	lda l3284,y
	and #$3f
	ora #$80
	sta ($5d),y
	dey
	bpl l2b6f
l2b7b:	pla
	tay
	rts
l2b7e:	stx $50
	lda IndirectBank
	cmp #SYSTEMBANK
	beq l2b8e
	jsr l225a
	ldx $50
	jsr l2d10
l2b8e:	ldy $41
	lda $40
	sta pointer1+1
	rts
l2b95:	stx $50
	lda IndirectBank
	cmp #SYSTEMBANK
	beq l2ba5
	jsr l225a
	ldx $50
	jsr l2d10
l2ba5:	ldy $25
	dey
	sty pointer1+1
	ldy #$ff
	rts
l2bad:	stx $34
	stx $4a
	ldx IndirectBank
	stx $58
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
	lda $4a
	sta $34
	lda #$00
	sta $11
	ldy $3d
	lda $4d
	ora $2c
	bne l2be8
	ldy #$12
l2be8:	lda $4c
	sta $33
	lda $4d
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
	ldx $58
	stx IndirectBank
	lda $11
	rts
l2c16:	sta $31
	stx $33
	stx $4c
	sty $32
	rts
l2c1f:	sta $2b
	stx $2d
	stx $4d
	sty $2c
	rts
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
l2c40:	jsr l2c46
	asl
	asl
	rts
l2c46:	clc
	sta $4a
	asl
	asl
	adc $4a
	rts
l2c4e:	lda #$7f
	sta pointer1
	lda #$00
	sta pointer1+1
	lda #$a2
	ldx #$2f
	ldy #$1f
	jsr l2ce8
	rts
l2c60:	lda #$6f
	sta pointer1
	lda #$00
	sta pointer1+1
	lda #$da
	ldx #$2f
	ldy #$0f
	jsr l2ce8
	rts
	lda #$5f
	sta pointer1
	lda #$00
	sta pointer1+1
	lda #$ca
	ldx #$2f
	ldy #$0f
	jsr l2ce8
	rts
l2c84:	lda #$00
	sta $9f
	lda #$d8
	sta $a0
	lda #$01
	sta $a1
	lda #$d8
	sta $a2
	rts
	lda #$a3
	sta pointer1
	lda #$00
	sta pointer1+1
	lda #$c2
	ldx #$2f
	ldy #$07
	jsr l2ce8
	rts
l2ca7:	lda #$5f
	sta pointer1
	lda #$00
	sta pointer1+1
	lda #$68
	ldx #$2f
	ldy #$39
	jsr l2ce8
	rts
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
l2ce8:	sta $5b
	stx $5c
	lda CodeBank
	sta IndirectBank
l2cf0:	lda ($5b),y
	sta (pointer1),y
	dey
	bpl l2cf0
	rts
l2cf8:	pha
	tya
	pha
	txa
	pha
	lda #$80
	sta $55
	bne l2d19
l2d03:	pha
	tya
	pha
	txa
	pha
	lda #$00
	sta $55
	lda #$ff
	bne l2d1b
l2d10:	pha
	tya
	pha
	txa
	pha
	lda #$00
	sta $55
l2d19:	lda #$3f
l2d1b:	sta $2a
	lda IndirectBank
	sta $58
	lda l3153,x
	tay
	lda l318c,x
	sta $37
	lda l31c5,x
l2d2d:	sta $38
	lda l31fe,x
	sta $14
	lda l3237,x
	sta $15
	ldx #SYSTEMBANK
l2d3b:	lda CodeBank
	sta IndirectBank
	lda ($37),y
	and $2a
	ora $55
	stx IndirectBank
	sta ($14),y
	dey
	bpl l2d3b
	lda $58
	sta IndirectBank
	pla
	tax
	pla
	tay
	pla
	rts

!byte $48, $49, $47, $48, $5d, $20, $33, $37
!byte $5d, $5d, $20, $34, $35, $5d, $5d, $20
!byte $35, $30, $5d, $5d, $20, $35, $39, $5d
!byte $5d, $20, $36, $37, $5d, $5d, $20, $37
!byte $36, $5d, $5d, $20, $38, $35, $5d, $5d
!byte $20, $38, $39, $5d, $5d, $20, $33, $38
!byte $5d, $5d, $20, $34, $36, $5d, $5d, $20
!byte $35, $31, $5d, $5d, $20, $36, $30, $5d
!byte $5d, $20, $36, $38, $5d, $5d, $20, $37
!byte $37, $5d, $5d, $20, $38, $36, $5d, $5d
!byte $20, $39, $30, $5d, $5d, $20, $33, $39
!byte $5d, $5d, $20, $34, $37, $5d, $5d, $20
!byte $35, $32, $5d, $5d, $20, $36, $31, $5d
!byte $5d, $20, $36, $39, $5d, $5d, $20, $37
!byte $38, $5d, $5d, $20, $38, $37, $5d, $5d
!byte $20, $39, $31, $5d, $5d, $20, $34, $30
!byte $5d, $5d, $20, $34, $38, $5d, $5d, $20
!byte $35, $33, $5d, $5d, $20, $39, $33, $5d
!byte $5d, $20, $37, $30, $5d, $5d, $20, $37
!byte $39, $5d, $5d, $20, $38, $38, $5d, $5d
!byte $20, $39, $32, $5d, $5d, $20, $20, $39
!byte $5d, $5d, $20, $32, $39, $5d, $5d, $20
!byte $20, $20, $5d, $5d, $20, $31, $37, $5d
!byte $5d, $20, $31, $33, $5d, $5d, $20, $31
!byte $32, $5d, $5d, $20, $31, $31, $5d, $5d
!byte $20, $20, $38, $5d, $5d, $20, $20, $35
!byte $5d, $5d, $20, $20, $34, $5d, $5d, $20
!byte $31, $30, $5d, $5d, $20, $20, $36, $5d
!byte $5d, $20, $20, $37, $5d, $5d, $20, $20
!byte $32, $5d, $5d, $20, $20, $31, $5d, $5d
!byte $20, $38, $34, $5d, $43, $42, $4d, $2d
!byte $49, $49, $20, $20, $20, $20, $4c, $4f
!byte $57, $20, $50, $52, $4f, $46, $49, $4c
!byte $45, $20, $20, $20, $39, $37, $30, $31
!byte $34, $38, $2e, $41, $2e, $36, $20, $20
!byte $20, $43, $59, $43, $4c, $45, $53, $3a
!byte $20, $30, $5d, $20, $37, $39, $5d, $5d
!byte $20, $37, $38, $5d, $5d, $20, $37, $37
!byte $5d, $5d, $20, $37, $36, $5d, $5d, $20
!byte $37, $35, $5d, $5d, $20, $37, $34, $5d
!byte $5d, $20, $37, $33, $5d, $5d, $20, $37
!byte $32, $5d, $5d, $20, $37, $31, $5d, $5d
!byte $20, $37, $30, $5d, $5d, $20, $36, $39
!byte $5d, $5d, $20, $36, $38, $5d, $5d, $20
!byte $36, $37, $5d, $5d, $20, $36, $36, $5d
!byte $5d, $20, $36, $35, $5d, $5d, $20, $36
!byte $34, $5d, $5d, $20, $35, $32, $5d, $5d
!byte $20, $35, $31, $5d, $5d, $20, $35, $30
!byte $5d, $5d, $20, $34, $39, $5d, $5d, $20
!byte $34, $38, $5d, $5d, $20, $34, $37, $5d
!byte $5d, $20, $34, $36, $5d, $5d, $20, $34
!byte $35, $5d, $5d, $20, $33, $36, $5d, $5d
!byte $20, $33, $35, $5d, $5d, $20, $33, $34
!byte $5d, $5d, $20, $33, $33, $5d, $5d, $20
!byte $33, $32, $5d, $5d, $20, $33, $31, $5d
!byte $5d, $20, $33, $30, $5d, $5d, $20, $32
!byte $39, $5d, $5d, $20, $35, $38, $5d, $5d
!byte $20, $32, $36, $5d, $5d, $20, $20, $20
!byte $5d, $5d, $20, $36, $31, $5d, $5d, $20
!byte $36, $30, $5d, $5d, $20, $35, $39, $5d
!byte $5d, $20, $20, $31, $5d, $5d, $20, $20
!byte $32, $5d, $5d, $20, $20, $33, $5d, $5d
!byte $20, $20, $34, $5d, $5d, $20, $20, $37
!byte $5d, $5d, $20, $31, $30, $5d, $5d, $20
!byte $31, $35, $5d, $5d, $20, $31, $36, $5d
!byte $5d, $20, $31, $37, $5d, $5d, $20, $32
!byte $38, $5d, $00, $da, $01, $da, $02, $da
!byte $03, $da, $04, $da, $05, $da, $06, $da
!byte $07, $da, $08, $da, $09, $da, $0a, $da
!byte $0b, $da, $0c, $da, $0d, $da, $0e, $da
!byte $0f, $da, $10, $da, $11, $da, $12, $da
!byte $13, $da, $14, $da, $15, $da, $16, $da
!byte $17, $da, $18, $da, $19, $da, $1a, $da
!byte $1b, $da, $1c, $da, $00, $dc, $01, $dc
!byte $02, $dc, $03, $dc, $04, $dc, $05, $dc
!byte $06, $dc, $07, $dc, $08, $dc, $09, $dc
!byte $0a, $dc, $0b, $dc, $0c, $dc, $0d, $dc
!byte $0e, $dc, $0f, $dc, $00, $dd, $01, $dd
!byte $02, $dd, $03, $dd, $00, $de, $01, $de
!byte $02, $de, $03, $de, $04, $de, $05, $de
!byte $06, $de, $07, $de, $00, $df, $01, $df
!byte $02, $df, $03, $df, $04, $df, $05, $df
!byte $06, $df, $07, $df, $52, $41, $4d, $20
!byte $53, $45, $47, $20, $24, $46, $52, $4f
!byte $4d, $53, $20, $53, $45, $47, $20, $24
!byte $46, $4c, $4f, $20, $41, $44, $52, $20
!byte $42, $59, $54, $45, $20, $54, $45, $53
!byte $54, $20, $48, $49, $20, $41, $44, $52
!byte $20, $42, $59, $54, $45, $20, $54, $45
!byte $53, $54, $20, $43, $48, $4b, $52, $42
!byte $52, $44, $20, $24, $35, $35, $2c, $20
!byte $24, $41, $41, $20, $41, $41, $2c, $20
!byte $24, $35, $35, $20, $4d, $41, $52, $43
!byte $48, $20, $49, $4e, $43, $20, $41, $44
!byte $52, $20, $24, $35, $41, $20, $44, $45
!byte $43, $20, $41, $44, $52, $20, $24, $41
!byte $35, $20, $35, $41, $20, $49, $4e, $43
!byte $20, $41, $44, $52, $20, $24, $46, $46
!byte $20, $44, $45, $43, $20, $41, $44, $52
!byte $20, $24, $30, $30, $20, $53, $54, $41
!byte $54, $49, $43, $20, $52, $41, $4d, $20
!byte $54, $45, $53, $54, $53, $20, $20, $36
!byte $35, $32, $36, $20, $54, $4f, $44, $20
!byte $54, $45, $53, $54, $53, $20, $20, $20
!byte $20, $36, $35, $32, $36, $20, $54, $49
!byte $4d, $45, $52, $53, $20, $54, $45, $53
!byte $54, $53, $20, $54, $4d, $52, $54, $4f
!byte $44, $54, $4e, $54, $36, $35, $32, $36
!byte $20, $36, $35, $32, $35, $20, $67, $31
!byte $36, $30, $20, $67, $31, $36, $31, $20
!byte $36, $38, $34, $35, $20, $36, $35, $38
!byte $31, $20, $36, $35, $35, $31, $20, $31
!byte $34, $38, $39, $20, $31, $34, $38, $38
!byte $20, $36, $35, $32, $35, $2a, $2d, $2d
!byte $2d, $2d, $2d, $2d, $2d, $2d, $2d, $2d
!byte $2d, $2a, $53, $45, $47, $4d, $45, $4e
!byte $54, $3a, $54, $45, $53, $54, $45, $58
!byte $45, $43, $55, $54, $45
l3103: 
!byte $41, $69, $71, $99
l3107:
!byte $d1, $d1, $d3, $d3, $20, $2a, $20
!byte $2a, $20, $20, $42, $41, $44, $20, $50
!byte $52, $4f, $47, $52, $41, $4d, $20, $43
!byte $48, $45, $43, $4b, $53, $55, $4d, $20
!byte $20, $2a, $20, $2a, $20, $20, $2a, $2a
!byte $2a, $2a, $2a, $2a, $2a, $2a, $2a, $2a
!byte $2a, $2a, $2a, $20, $20, $4e, $4f, $20
!byte $52, $41, $4d, $20, $20, $2a, $2a, $2a
!byte $2a, $2a, $2a, $2a, $2a, $2a, $2a, $2a
!byte $2a, $2a, $2a, $2a, $20
l3153:
!byte $2d, $09, $0a
!byte $4f, $4f, $4f, $0b, $0b, $0b, $0b, $0b
!byte $0b, $0b, $0b, $10, $10, $10, $07, $11
!byte $0b, $02, $0b, $0b, $07, $07, $07, $07
!byte $07, $07, $07, $07, $07, $07, $07, $07
!byte $06, $06, $06, $06, $03, $03, $03, $03
!byte $30, $11, $02, $1f, $03, $4f, $4f, $4f
!byte $02, $02, $11, $11, $27, $27
l318c:
!byte $4a, $ea
!byte $f4, $78, $c8, $18, $e3, $e3, $e3, $e3
!byte $e4, $e4, $e4, $e4, $ff, $10, $21, $32
!byte $3a, $4c, $58, $5b, $67, $f0, $f0, $f0
!byte $f0, $f0, $f0, $f0, $f0, $f0, $f0, $f0
!byte $f0, $fc, $fc, $fc, $fc, $f8, $f8, $f8
!byte $f8, $b2, $73, $a9, $0b, $56, $5a, $aa
!byte $fa, $ac, $af, $85, $97, $2b, $2b 
l31c5:
!byte $2e
!byte $2f, $2f, $2e, $2e, $2f, $30, $30, $30
!byte $30, $30, $30, $30, $30, $2f, $30, $30
!byte $30, $30, $30, $30, $30, $30, $30, $30
!byte $30, $30, $30, $30, $30, $30, $30, $30
!byte $30, $30, $30, $30, $30, $30, $30, $30
!byte $30, $30, $30, $30, $30, $31, $2d, $2d
!byte $2d, $2d, $30, $30, $30, $30, $31, $31
l31fe:
!byte $00, $00, $11, $90, $c0, $f0, $a2, $ca
!byte $d2, $fa, $ba, $e2, $ea, $12, $37, $37
!byte $37, $40, $37, $3d, $46, $3d, $3d, $af
!byte $d7, $df, $07, $3a, $4e, $62, $76, $8a
!byte $9e, $b2, $c6, $82, $96, $aa, $be, $32
!byte $46, $5a, $6e, $1f, $37, $af, $08, $09
!byte $90, $c0, $f0, $af, $af, $37, $37, $20
!byte $48 
l3237:
!byte $d0, $d5, $d5, $d1, $d3, $d5, $d0
!byte $d0, $d2, $d2, $d0, $d0, $d2, $d3, $d0
!byte $d0, $d0, $d0, $d0, $d0, $d0, $d0, $d0
!byte $d0, $d0, $d2, $d3, $d7, $d7, $d7, $d7
!byte $d7, $d7, $d7, $d7, $d7, $d7, $d7, $d7
!byte $d7, $d7, $d7, $d7, $d5, $d0, $d6, $d7
!byte $d0, $d1, $d3, $d5, $d6, $d6, $d0, $d0
!byte $d3, $d3, $70, $40, $60, $40, $6e, $5d
!byte $20, $20, $15, $5d, $5d, $20, $0f, $0b
!byte $5d, $6d, $40, $40, $40, $7d
l3284:
!byte $42, $41, $44
l3287:
!byte $70, $75, $7a, $7f, $84
l328c:
!byte $32, $32, $32, $32, $32, $15, $3d, $45, $6d, $52
!byte $d1, $d1, $d3, $d3, $d5, $b8, $10, $38
!byte $40, $68, $42, $92, $d0, $d1, $d1, $d3
!byte $d3, $d7, $d7, $e0, $0b, $33, $3b, $63
!byte $56, $a6, $d0, $d1, $d1, $d3, $d3, $d7
!byte $d7, $06, $2e, $e8, $36, $5e, $6a, $ba
!byte $d1, $d1, $d2, $d3, $d3, $d7, $d7, $01
!byte $29, $10, $31, $59, $7e, $ce, $d1, $d1
!byte $d3, $d3, $d3, $d7, $d7, $fc, $24, $2c
!byte $54, $d0, $d1, $d3, $d3, $f7, $1f, $27
!byte $4f, $d0, $d1, $d3, $d3, $f2, $1a, $22
!byte $4a, $d0, $d1, $d3, $d3, $6b, $d5, $66
!byte $d5, $57, $d5, $61, $d5, $ad, $4c, $4d
!byte $d5, $d6, $d6

l32f9:	
!byte $04, $06, $06, $06, $06, $03, $03, $03, $00, $00, $00, $00, $02
l3306:
!byte $91, $9b, $a9, $b7, $c5, $d3, $db, $e3, $eb, $ed, $ef, $f1, $f3
l3313:
!byte $32, $32, $32, $32, $32, $32, $32, $32, $32, $32, $32, $32, $32
l3320:
!byte $96, $a2, $b0, $be, $cc, $d7, $df, $e7, $ec, $ee, $f0, $f2, $f6
l332d:
!byte $32, $32, $32, $32, $32, $32, $32, $32, $32, $32, $32, $32, $32
l333a:
!byte $30, $31, $32, $33, $34, $35, $36, $37, $38, $41, $44, $45, $20
!byte $aa, $aa, $aa, $aa, $aa, $aa, $aa, $aa, $aa
