; Test-Loader by Vossi 11/2023
; Loader
!cpu 6502
!to "t.prg", cbm
FILL		= $00		; fills free memory areas with $aa
; ***************************************** ADDRESSES *********************************************
!addr Test	= $0400
; *************************************** BASIC LOADER ********************************************
!initmem FILL
!zone basic
*= $0003
	!byte $1b, $00, $0a, $00, $dd, $22, $54, $30, $34, $30, $30, $22, $2c, $20, $42, $31
	!byte $2c, $50, $30, $31, $30, $32, $34, $00, $4f, $00, $14, $00, $97, $38, $33, $2c
	!byte $31, $32, $30, $3a, $97, $38, $34, $2c, $31, $36, $39, $3a, $97, $38, $35, $2c
	!byte $31, $3a, $97, $38, $36, $2c, $31, $33, $33, $3a, $97, $38, $37, $2c, $30, $3a
	!byte $97, $38, $38, $2c, $32, $33, $34, $3a, $9e, $38, $33, $00, $00, $00

	; 10 bload"pm500", b0
	; 20 poke83,120:poke84,169:poke85,0:poke86,133:poke87,0:poke88,234:sys83
; ***************************************** ZERO PAGE *********************************************
!zone zeropage
; I/O pointer table
*= $0051
; ***************************************** ZONE MAIN *********************************************
!zone main
; bank 15 - 6 bytes poked from BASIC
; $0053					; sys83 goes here in bank 15
;	sei				; disable interrrupts
;	lda #$01
;	sta $00				; switch to code bank 0
; $0058	nop				; from here the CPU continues in bank 0
*= $0058
	sei				; disable interrrupts
	jmp Test
