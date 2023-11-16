; CBMII b128-256 burnin - 
; disassembled by vossi 11/2023
;
; machine code starter loaded in program bank and systembank
;
!cpu 6502
!to "load 2.prg", cbm
; ***************************************** CONSTANTS *********************************************
!addr PROGRAMSTART	= $2000		; code start of main program
; ***************************************** ADDRESSES *********************************************
!addr CodeBank		= $00		; code bank register
; ******************************************* CODE ************************************************
!zone code
*= $d000
	lda #$04		; default bank 4 -doesn't matter because modified from basic loader
	sta CodeBank		; switch to program-bank
	nop
	nop
	nop
	nop
	jmp PROGRAMSTART	; start main program
	tax
	tax
	tax
	tax
