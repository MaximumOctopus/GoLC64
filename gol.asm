; ==============================================================
; =                                                            =
; = Conway's Game of Life, Commodore C64 v0.2                  =
; =                                                            =
; = (c) Paul Alan Freshney 2023 / paul@freshney.org			   =
; =                                                            =
; = https://github.com/MaximumOctopus/GoLC64                   =
; =                                                            =
; = February 6th 2023                                          =
; =                                                            =
; ==============================================================

; Processes cells $000-$0FF in frame 1
;           cells $100-$1FF in frame 2
;           cells $200-$2FF in frame 3
;           cells $300-$3FF in frame 4
; Copies cell data from cell RAM to screen in frame 5

; Joystick 1:
;   fire, set a cell "live" (when draw mode active)
;   up, clears the screen
;   down, clears the screen with random data

; Joystick 2:
;   fire, toggles mode
;     draw mode: pauses game of life animation.
;     gol mode : starts game of life animation.

				; == debug ==================================================================================================
				
				FRAMETIME    = 1			; set it to 1 to show frame execution time

				; == constants ==============================================================================================

				VICBASE      = $8000
				SCREENRAM    = VICBASE   + $2000
				CHARRAM      = VICBASE   + $3000
				SPRITEPTR    = SCREENRAM + $03f8
	
				COLOURRAM    = $d800
	
				CELLON       = $01
				CELLOFF      = $00
				
				XMIN         = $18
				YMIN         = $32
				XMAX         = $50		; when sprite one, bit #8 ($d010:0) is 1
				YMAX         = $f2
				
				PIPELINE     = $16		; $00-$03 for cell processing
				COPYBUFFER   = $04		; $04 for copy from cell data to screen (gives cell update rate of 10Hz)
				BLANKSCREEN  = $05		; $05 to clear the screen the next frame
				RANDOMSCREEN = $06 		; $06 to clear the screen with random data next frame
										
				TOGGLE       = $17
				JOYCOPY      = $18		; copy of joystick 2 used to debounce mode change
				
				; ===========================================================================================================

				*= $0801
				.word (+), 2005  		;pointer, line number
				.null $9e, format("%d", start)
+				.word 0          		;basic line end

				*=$4000

start			sei           			;disable interrupts
		 
				lda #$00
				tax
				tay
		
				lda #$00       			; black
				; sta $d011      		; turn off screen. (now you have only borders!)
				sta $d020      			; make border black.
				sta $d021      			; make background colour 0 black.
		
				lda #$8c				; character data at $3000, screen at $2000 (+ base VIC address)
				sta $d018				; 
				
				lda #$36
				sta $01
			
				lda $dd00
				and #%11111100
				ora #%00000001 			; using bank2
				sta $dd00			
		
				ldx #$00
cls				lda CELLRAM,x
				sta SCREENRAM,x			; clear the screen		
				lda CELLRAM+$100,x
				sta SCREENRAM+$100,x
				lda CELLRAM+$200,x
				sta SCREENRAM+$200,x
				lda CELLRAM+$300,x
				sta SCREENRAM+$300,x
				
				lda #$01				; white organisms
				sta COLOURRAM,x
				sta COLOURRAM+$100,x
				sta COLOURRAM+$200,x
				sta COLOURRAM+$300,x
				
				inx
				bne cls
				
				lda #$00
				sta PIPELINE
				sta TOGGLE
				
				lda #$ff
				sta JOYCOPY
				
				lda #$00
				sta $60					; 0 neighbours
				sta $61					; 1 neighbours
				sta $64					; 4 neighbours
				sta $65					; 5 neighbours
				sta $66					; 6 neighbours					
				sta $67					; 7 neighbours
				sta $68					; 8 neighbours
				lda #$01
				sta $62					; 2 neighbours
				sta $63					; 3 neighbours
								
				; ===========================================================================================================
				
sprite_init		lda	#$01				; 
				sta	$d015				; turn on sprite #1
			
				lda #$00
				ldy #$02				; shared sprite colour (red)
				sta	SPRITEPTR			; sprite shape (base VIC bank address + $03F8 + sprite # (0-7))
				sty $d027				; sprite colour
				
				lda #XMIN
				sta $d000
				lda #YMIN
				sta $d001
				
				; ===========================================================================================================

				ldx #$00
				lda #$00
main	    	bit $d011 				; Wait for new frame
				bpl *-3
				bit $d011
				bmi *-3	

				.if FRAMETIME
				lda #$00
				sta $d020
				.endif
				
				lda TOGGLE
				beq jumpsetup
				
				jmp post
		
jumpsetup		lda #$41
				
				clc
				adc PIPELINE
				sta jumpselect+2
				
jumpselect		jmp $4100

; ======================================================================================================

				.align $100

chunk1
				
				ldx #$00				; offset
cgol1			lda #$00				; number of live neighbours
				
cgolq			clc
				adc SCREENRAM-41,x
				adc SCREENRAM-40,x	
				adc SCREENRAM-39,x
				adc SCREENRAM-1,x
				adc SCREENRAM+1,x
				adc SCREENRAM+39,x	
				adc SCREENRAM+40,x
				adc SCREENRAM+41,x	
						
applyrules		beq store
				
				cmp #2
				beq nextloop
				
				tay
				lda $60,y
				sta CELLRAM,x
				inx
				bne cgol1
				
				jmp chunk1finish
				
store			sta CELLRAM,x
				inx
				bne cgolq
				
				jmp chunk1finish
				
nextloop		inx
				bne cgol1
				
chunk1finish	inc PIPELINE

				jmp post

; ======================================================================================================

				.align $100

chunk2
				
				ldx #$00				; offset
cgol2			lda #$00				; number of live neighbours
				
cgol2q			clc
				adc SCREENRAM+$100-41,x
				adc SCREENRAM+$100-40,x	
				adc SCREENRAM+$100-39,x
				adc SCREENRAM+$100-1,x
				adc SCREENRAM+$100+1,x
				adc SCREENRAM+$100+39,x	
				adc SCREENRAM+$100+40,x
				adc SCREENRAM+$100+41,x	
						
applyrules2		beq store2
				
				cmp #2
				beq nextloop2
				
				tay
				lda $60,y
				sta CELLRAM+$100,x
				
				inx
				bne cgol2
				
				jmp chunkfinish2
				
store2			sta CELLRAM+$100,x
				inx
				bne cgol2q
				
				jmp chunkfinish2
				
nextloop2		inx
				bne cgol2
				
chunkfinish2	inc PIPELINE

				jmp post

; ======================================================================================================

				.align $100

chunk3
				
				ldx #$00				; offset
cgol3			lda #$00				; number of live neighbours
				
cgol3q			clc
				adc SCREENRAM+$200-41,x
				adc SCREENRAM+$200-40,x	
				adc SCREENRAM+$200-39,x
				adc SCREENRAM+$200-1,x
				adc SCREENRAM+$200+1,x
				adc SCREENRAM+$200+39,x	
				adc SCREENRAM+$200+40,x
				adc SCREENRAM+$200+41,x	
						
applyrules3		beq store3
						
				cmp #2
				beq nextloop3
				
				tay
				lda $60,y
				sta CELLRAM+$200,x
				
				inx
				bne cgol3
				
				jmp chunkfinish3

store3			sta CELLRAM+$200,x
				inx
				bne cgol3q
				
				jmp chunkfinish3
				
nextloop3		inx
				bne cgol3
				
chunkfinish3	inc PIPELINE

				jmp post

; ======================================================================================================

				.align $100

chunk4
				
				ldx #$00				; offset
cgol4			lda #$00				; number of live neighbours
				
cgol4q			clc						
				adc SCREENRAM+$300-41,x
				adc SCREENRAM+$300-40,x	
				adc SCREENRAM+$300-39,x
				adc SCREENRAM+$300-1,x
				adc SCREENRAM+$300+1,x
				adc SCREENRAM+$300+39,x	
				adc SCREENRAM+$300+40,x
				adc SCREENRAM+$300+41,x	
						
applyrules4		beq store4
				
				cmp #2
				beq nextloop4
				
				tay
				lda $60,y
				sta CELLRAM+$300,x
				
				inx
				bne cgol4
				
				jmp chunkfinish4

store4			sta CELLRAM+$300,x
				inx
				bne cgol4q
				
				jmp chunkfinish4
				
nextloop4		inx
				bne cgol4
				
chunkfinish4	inc PIPELINE

				jmp post

; ======================================================================================================

				.align $100

cellcopyinit	ldx #$00
cellcopy		lda CELLRAM,x 
				sta SCREENRAM,x				
				lda CELLRAM+$100,x
				sta SCREENRAM+$100,x
				lda CELLRAM+$200,x
				sta SCREENRAM+$200,x
				lda CELLRAM+$300,x
				sta SCREENRAM+$300,x
				inx
				bne cellcopy
				
				sta PIPELINE
				
				jmp post

; ======================================================================================================

				.align $100

clearscreeninit lda #$00
				ldx #$00
clearcopy		sta SCREENRAM,x				
				sta SCREENRAM+$100,x
				sta SCREENRAM+$200,x
				sta SCREENRAM+$300,x
				inx
				bne clearcopy
				
				sta PIPELINE
				
				jmp post
				
; ======================================================================================================

				.align $100
				
clearrandominit lda #$ff  				; maximum frequency value
				sta $d40e 				; voice 3 frequency low byte
				sta $d40f 				; voice 3 frequency high byte
				lda #$80  				; noise waveform, gate bit off
				sta $d412				; voice 3 control register
				
				lda #$00
				ldx #$00
clearrandom		lda $d41b
				and #$01				; gives random 0 (cell dead), 1 (cell alive)
				sta SCREENRAM,x
				lda $d41b
				and #$01				; gives random 0 (cell dead), 1 (cell alive)
				sta SCREENRAM+$100,x
				lda $d41b
				and #$01				; gives random 0 (cell dead), 1 (cell alive)
				sta SCREENRAM+$200,x
				lda $d41b		
				and #$01				; gives random 0 (cell dead), 1 (cell alive)
				sta SCREENRAM+$300,x
				inx
				bne clearrandom
				
				stx PIPELINE			; ensures cell processing starts from the beginning

				jmp post

; ======================================================================================================

				.align $100

post			

; ======================================================================================================

joyupclear		lda #$01                
				bit $dc01
				bne joydownrnd
				
				lda #BLANKSCREEN		; screen clear will occur next frame
				sta PIPELINE
				
				jmp joytogglemode
				
joydownrnd		lda #$02
				bit $dc01
				bne joytogglemode
				
				lda #RANDOMSCREEN		; random screen fill will occur next frame
				sta PIPELINE				

joytogglemode	lda #$10
				bit $dc01     
				bne joyclear

				cmp JOYCOPY
				beq joy2check
				
				sta JOYCOPY
				
				lda TOGGLE
				beq setdrawmode
		
cleardrawmode   lda #$00
				sta TOGGLE
				sta PIPELINE
				
				ldy #$02				; alter sprite colour to red
				sty $d027				; 
				
				sta $d020				; change border to black

				jmp joy2check
		
setdrawmode		lda #$01
				sta TOGGLE
				
				lda #$0e				; alter sprite colour to yellow
				sta $d027				; 
				
				lda #$0c				; change border to grey
				sta $d020				;

				jmp joy2check

joyclear		lda #$ff
				sta JOYCOPY

joy2check		lda $dc00
				cmp #$7f
				bne joyleft
				
				jmp joyend

joyleft			lda #$04              
				bit $dc00             
				bne joyright  			

				lda #$01				; only check for going out of screen bounds if bit #8 of x-coordinate is not set
				bit $d010				;
				beq moveleft			;
				
				lda $d000
				sec
				sbc #$08
				sta $d000
				bcs lsram
				
				lda #$00				; clear bit #8 if moved left of x-coord 256
				sta $d010
				
				jmp lsram
				
moveleft		lda $d000				; 
				cmp #XMIN				;
				beq joyright			;

				sec
				sbc #$08
				sta $d000
				
lsram			sec						; update the location of the screen ram address under the sprite cursor
				lda writeto+1			;
				sbc #$01				; 1 byte, to the left
				sta writeto+1			;
				lda writeto+2			;
				sbc #$00				;
				sta writeto+2			;
				
				jmp joyfire

joyright		lda #$08                
				bit $dc00               
				bne joyup

				lda #$01				; only check for going out of screen bounds if bit #8 of x-coordinate is set
				bit $d010				;
				beq moveright			;
  
				lda $d000
				cmp #XMAX
				beq joyup
  
moveright		lda $d000
				clc
				adc #$08
				sta $d000
				bcc rsram
	
				lda #$01				; passed x-coord 255, let's set bit #8 of x-coordinate
				sta $d010
	
rsram			clc						; update the location of the screen ram address under the sprite cursor
				lda writeto+1			;
				adc #$01				; 1 byte, to the right
				sta writeto+1			;
				lda writeto+2			;
				adc #$00				;
				sta writeto+2			;
				
				jmp joyfire
  
joyup			lda #$01                
				bit $dc00               
				bne joydown
				
				lda $d001
				cmp #YMIN
				beq joyend

				sec
				sbc #$08
				sta $d001
				
				sec						; update the location of the screen ram address under the sprite cursor
				lda writeto+1			;
				sbc #$28				; 40 bytes, one row above
				sta writeto+1			;
				lda writeto+2			;
				sbc #$00				;
				sta writeto+2			;
				
				jmp joyfire
 
joydown			lda #$02      
				bit $dc00     
				bne joyfire   
  
				lda $d001
				cmp #YMAX
				beq joyend  
  
				clc
				adc #$08
				sta $d001

				clc						; update the location of the screen ram address under the sprite cursor
				lda writeto+1			;
				adc #$28				; 40 bytes, one row below
				sta writeto+1			;
				lda writeto+2			;
				adc #$00				;
				sta writeto+2			;	
 
joyfire			lda #$10      
				bit $dc00     
				bne joyend

				lda TOGGLE				; only allow drawing in drawmode
				beq joyend

				lda #CELLON
writeto			sta SCREENRAM			; modified by code, represents the position in screen RAM below the sprite cursor

joyend

; ======================================================================================================
				.if FRAMETIME
				lda #$01
				sta $d020
				.endif

				jmp main

; ======================================================================================================
; ======================================================================================================
; ======================================================================================================

				.align $100

cellram			.fill 1024, [CELLON, CELLOFF, CELLOFF, CELLON, CELLOFF, CELLON, CELLOFF, CELLON, CELLON]

; ======================================================================================================

				*=$8000
				
				; sprite cursor
				.byte $ff,$00,$00,$ff,$00,$00,$c3,$00,$00,$c3,$00,$00,$c3,$00,$00,$c3
				.byte $00,$00,$ff,$00,$00,$ff,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
				.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
				.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		
; ======================================================================================================
		
				*=CHARRAM
		
				.byte $00, $00, $00, $00, $00, $00, $00, $00
				.byte $fe, $fe, $fe, $fe, $fe, $fe, $fe, $00