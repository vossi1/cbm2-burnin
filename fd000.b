; disassembled by DASM6502b v.3.1 by Marat Fayzullin
; modified by Vossi 02/2019
!cpu 6502
*= $d000
	lda #$02
	sta $00
	nop
	nop
	nop
	nop
	jmp $2000
	tax
	tax
	tax
	tax
