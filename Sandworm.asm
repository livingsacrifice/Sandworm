; [Credit]
; forked codebase from Robin Sergeant's Snake (https://github.com/RobinSergeant/2600-Snakes)
;
; [SANDWORM]
; Sandworm is an asymmetrical two-player game. Player 1 controls the sandworm and attempts to consume as many sandpeople as he can (similar to classic game, Snake). Player 2 controls the sandpeople, mastering the sandwalk while escaping to the corners of the desert. CPU will control movements for one of the players in 1P mode.
; 
; [PLAY]
; Play using Stella emulator/core or via https://javatari.org
;
; [Objective]
; Player 1 wins when sandworm reaches 99 segments in length ('99' will display in top-right)
; Player 2 wins when 10 sandpeople successfully escape the sandworm ('A' will display in bottom-right)
; Player 1 loses when sandworm collides with boundary or itself

; [Controls]
; Select: Toggles Player 1 controlling sandworm (black background) or the sandpeople (dark-gray background)
; Reset: Resets game, including high score
; Player 1:
; - Joystick: Changes direction for the head of sandworm
; - Fire: Enters tunneling mode (also resets board after game over)
; Player 2:
; - Joystick: Moves sandperson
; - Fire: Aborts any queued movements commands, to stand still

; [Game tracking]
; Top left: High score (max length of sandworm)
; Top right: Current score (length of sandworm)
; Bottom left: Tunneling charges (# of times P1 can enter tunneling mode)
; Bottom right: Escapes (# of times sandpeople have escaped the sandworm)

; [Game mechanics]
; Eating: Sandworm eats a person when the head occupies same location as a sandperson. Sandworm will then grow in length and a new sandperson will spawn in a random location (Spawn will avoid current body of snake or a position within 3 movements of the corner).
; Escape: Sandpeople escape the sandworm by exiting at one of the four corners of the desert. Sandpeople initially move 4x slower than the sandworm but will gradually speed up as the number of escapes increase.
; Tunneling: Sandworm will occasionally enter tunneling mode (either by consuming a charge with FIRE button or by sandperson failing to sandwalk properly). While tunneling is active, sandpeople will move 15x slower than the sandworm and the screen will shake.
; Sandwalking: Sandpeople will trigger a free tunnel by the sandworm if they engage in frequent or repetitious movements.
; Awareness: In reverse 1P mode, the sandworm will gradually become more aware of the location of a sandperson, based on frequency of moves.

            processor 6502

            include "vcs.h"

            include "macro.h"


      SEG.U vars


	  
;===============================================================================
; C O N S T A N T S
;===============================================================================	  

MUNCH_DELAY = 20
ESCAPE_FAIL = 10			; game ends when escape count = 10
DISPLAY_POS = [160 - 43] / 2

DIR_LEFT    = %10000000
DIR_RIGHT   = %11000000
DIR_UP      = %00100000
DIR_DOWN    = %00110000

MOV_LEFT    = %00000000
MOV_RIGHT   = %01000000
MOV_UP      = %10000000
MOV_DOWN    = %11000000

DEFAULT_COLOR = #$D8 ;green
TUNNEL_COLOR = #$08	;gray
BLACK = #$00
DARK_GRAY = #$02
RED = #$36
YELLOW = #$1E
WHITE = #$0E
HEADCOLOR = #$BE
MOVELIMIT_PERSON = #$07

; flags for GameState:
; FRUIT_LOADED => bit 0; ORA #$01 to set; AND #$FE to clear; fruit is loaded
; TUNNELING   => bit 1; ORA #$02 to set; AND #$FD to clear; sandworm is tunneling
; BLOCK_INPUT => bit 2; ORA #$04 to set; AND #$FB to clear; block during restart wait
; REV_P12 => bit 3; ORA #$08 to set; AND #$F7 to clear; reverse worm/player control
; P2_ACTIVE => bit 4; ORA #$10 to set; AND #$EF to clear; uses player 2 input
; RES_PRESS => bit 5; ORA #$20 to set; AND #$DF to clear; one-shots reset button
; SEL_PRESS => bit 6; ORA #$40 to set; AND #$BF to clear; one-shots select button
; GAME_OVER => bit 7; ORA #$80 to set; AND #$7F to clear; game is over
FRUIT_LOADED    = %00000001
TUNNELING       = %00000010
BLOCK_DIR       = %00000100
REV_P12         = %00001000
P2_ACTIVE       = %00010000
RES_PRESS		= %00100000
SEL_PRESS		= %01000000
GAME_OVER    	= %10000000
; flags for GameState 2: lower nibble tracks move count of person
; FIRE_PRESS => bit 4; ORA #$10 to set; AND #$EF to clear; one-shots fire button
FIRE_PRESS		= %00010000
MAX_SCORE 		= 99 ; can increase to ~200 if not using BCD

;===============================================================================
; Z P - V A R I A B L E S
;===============================================================================
      ORG $80

Score       		ds 1	; $80
HiScore     		ds 1	; $81
GameState   		ds 1	; $82
GameState2   		ds 1	; $83
FrameToggle 		ds 1	; $84

Sound       		ds 1	; $85
RandomNum   		ds 1	; $86

DisplayLeft  		ds 17	; $87
DisplayRight 		ds 17	; $98

Direction     		ds 1	; $A9
NewDirection  		ds 1	; $AA

MotionCount 		ds 1	; $AB
MotionDelay 		ds 1	; $AC
MoveCount 			ds 1    ; $AD ; AND #$F0 = current moves; AND #$0F = move limit
MoveCountTemp 		ds 1 	; $AE     ; hold MoveCount during tunneling
TunnelCount    		ds 1	; $AF    ; AND #$F0 = avail tunnels; AND #$0F = used tunnels
EscapeCount     	ds 1	; $B0
NewDirectionPerson 	ds 1	; $B1
StayMoveHist 		ds 1	; $B2	; stores last 8 move/stay flags of person
PersMoveHist 		ds 1	; $B3	; stores last 4 moves of person
StickFigureColor 	ds 1	; $B4

HeadPosition    	ds 1	; $B5
TailPosition    	ds 1	; $B6
FruitPosition   	ds 1	; $B7
TempFruitPosition 	ds 1	; $B8
TempMoveHist		ds 1	; $C9 (fix index later)

FreeIndex       	ds 1	; $B9
FreeOffset      	ds 1	; $BA
TailOffset      	ds 1	; $BB
TailLoc = Body

;StickFigurePtr 		ds 2	; $BB
FaceSpritePtr   	ds 2	; $BC

PF2a_Current  		ds 1	; $BE
PF0b_Current  		ds 1	; $BF
PF1b_Current  		ds 1	; $C0
Mask_Current  		ds 1	; $C1
Fruit_Current 		ds 1	; $C2

TempPF2      		ds 1	; $C3
PF2a_Temp    		ds 1	; $C4
PF0b_Temp    		ds 1	; $C5
PF1b_Temp    		ds 1	; $C6
Fruit_Temp	 		ds 1	; $C7
Mask_Temp    		ds 1	; $C8

SpritePtr0 = PF2a_Current	; $BE
SpritePtr1 = PF1b_Current	; $C0
SpritePtr2 = PF2a_Temp		; $C4
SpritePtr3 = PF1b_Temp		; $C6

Temp = PF2a_Current
STACK_POS = Mask_Temp

Body        		ds #$FF - STACK_POS		; $CB+
; 10 bytes required for stack
; 6 for $C_, 16 for $D_, 16 for $E_, 6 for $F_
; max body length handled = 44*4 => 176


;===============================================================================
; M A C R O S
;===============================================================================

;-------------------------------------------------------
; Usage: TIMER_SETUP lines
; where lines is the number of scanlines to skip (> 2).
; The timer will be set so that it expires before this number
; of scanlines. A WSYNC will be done first.

    MAC TIMER_SETUP
.lines  SET {1}
.cycles SET ((.lines * 76) - 13)
; special case for when we have two timer events in a line
; and our 2nd event straddles the WSYNC boundary
	if (.cycles % 64) < 12
		lda #(.cycles / 64) - 1
		sta WSYNC
        else
		lda #(.cycles / 64)
		sta WSYNC
        endif
        sta TIM64T
    ENDM

;-------------------------------------------------------
; Use with TIMER_SETUP to wait for timer to complete.
; Performs a WSYNC afterwards.

    MAC TIMER_WAIT
.waittimer
        lda INTIM
        bne .waittimer
        sta WSYNC
    ENDM

;===============================================================================
; R O M - C O D E (Reset)
;===============================================================================

            SEG

            ORG $F000

Reset
		CLEAN_START
		
		lda #DEFAULT_COLOR
        sta RandomNum ; use this color as random number seed!
		
Restart		;moved Restart back a lot to allow game over screen
        ldx #STACK_POS
        txs

		lda #BLACK
		sta COLUBK
		
		lda #DEFAULT_COLOR
        sta COLUPF
		
; set background color to dark gray instead of black if reversed positions		
		lda GameState
        and #$08
        bne .flipColor     ; reversed controls
		lda #BLACK
		sta COLUBK
		jmp .resume
.flipColor		
		lda #DARK_GRAY
		sta COLUBK
.resume		

        lda #1
        sta VDELP1

        lda #5
        sta AUDV0
        
        lda #10
        sta MotionDelay

        lda #$FF
        sta DisplayLeft+16
        sta DisplayRight+16

        lda #>Char0
        sta FaceSpritePtr+1
;        sta StickFigurePtr+1
        lda #<Face
        sta FaceSpritePtr
;        lda #<StickFigure
;        sta StickFigurePtr

;Restart     ; old Restart position; moved this backward to have game over screen persist without breaking everything
		lda #0
        sta TailLoc
        sta FreeIndex
        sta TailOffset
        sta FreeOffset
        sta EscapeCount		
		sta NewDirectionPerson
		sta PersMoveHist
		sta StayMoveHist

		ldx #16
.ResetPF    		
		dex
        sta DisplayLeft,x
        sta DisplayRight,x
        bne .ResetPF				

        lda #$12
        sta TailPosition
        lda #$32
        sta HeadPosition

        ldx #1
        ldy #2
        jsr UpdatePlayField
        inx
        jsr UpdatePlayField

        lda MotionDelay
        sta MotionCount

        lda #DIR_RIGHT
        sta Direction		

        jsr GrowBody
        jsr GrowBody
        lda #3
        sta Score
        lda #$40
        sta MoveCount
        lda #10
        sta MotionDelay
		lda #1
        sta TunnelCount
			
		lda GameState
        ora #$05		; set fruit loaded and enable input
        and #$FD		; clear tunneling bit
		and #$7F		; clear game over bit
		and #$EF		; clear P2 active bit
        sta GameState
		lda GameState2
		and #$F0		; reset lower nibble for person move count
		sta GameState2
        lda #DEFAULT_COLOR
        sta COLUPF
		lda #WHITE
		sta StickFigureColor
			
		lda RandomNum     ; check RandomNum corresponds to a free square
		cmp #$12             ; Compare with $12
		beq .add_one         ; If equal, branch to add $01
		cmp #$22             ; Compare with $22
		beq .add_one         ; If equal, branch to add $01
		cmp #$32             ; Compare with $32
		beq .add_one         ; If equal, branch to add $01
		jmp .no_change       ; Otherwise, jump to no_change
.add_one
		adc #$01         ; Add $01 to A
		sta FruitPosition ; Store result in FruitPosition
		jmp .done         ; Jump to done
.no_change
		sta FruitPosition ; If no change, store original RandomNum
.done

;===============================================================================
; R O M - C O D E (FrameCode)
;===============================================================================

StartOfFrame

   ; Start of vertical blank processing

        VERTICAL_SYNC

        ; 36 scanlines of vertical blank...

        ldx #16
        jsr WaitForLines						; 32 scanlines
		;splitting the WaitForLines in 2 to avoid HMOVE blank issue
		

        ; 2 scan lines to position sprite
        lda #DISPLAY_POS        ; 2
        ldx #1                  ; 2
        jsr SetHorizPos         ; 6 + 6 + 31    ; #33
;		SLEEP 10
        sta WSYNC               ; 3             ; #34
        lda #DISPLAY_POS        ; 2
        clc                     ; 2
        adc #6                  ; 2
        ldx #0                  ; 2
        jsr SetHorizPos         ; 6 + 6 + 31    ; #35
;	    SLEEP 10
        ; 1 scanline to move sprite
        sta WSYNC								; #36
        sta HMOVE
		
        ldx #16
        jsr WaitForLines						; 32 scanlines
		
        lda #0
        sta VBLANK

        ; 192 scanlines of picture...

         ; 14 scan lines of nothing
;			  sta WSYNC
              ldx #14				; 2
              lda GameState			; 3	5
              and #$02				; 2	7
              beq ContinueWait      ; 2 9 sandowrm already tunneling
              lda FrameToggle		; 3 12
              bne Toggle			; 2	14
              ldx #15				; 2	16
Toggle
              eor #$01				; 2 18
              sta FrameToggle		; 3 21
ContinueWait
              jsr WaitForLines
			  ; 3 for sta wsync
			  ; 2 for bne
			  ; 3 to rts
        ; 1 scan line to choose sprites			#15 total
              lda #>Char0           ; 2
              sta SpritePtr0+1      ; 3
              sta SpritePtr1+1      ; 3
              sta SpritePtr2+1      ; 3
              sta SpritePtr3+1      ; 3
              lda Score             ; 3
              and #$0F              ; 2
              asl                   ; 2
              asl                   ; 2
              asl                   ; 2 => 25
              sta SpritePtr0        ; 3
              lda Score             ; 3
              and #$F0              ; 2
              lsr                   ; 2 => 35
              sta SpritePtr1        ; 3
              lda HiScore           ; 3
              and #$0F              ; 2
              asl                   ; 3
              asl                   ; 2
              asl                   ; 2 => 50
              sta SpritePtr2        ; 3
              lda HiScore           ; 3
              and #$F0              ; 2
              lsr                   ; 2 => 60
              sta SpritePtr3        ; 3 => 63 cycles (+8?)
              sta WSYNC				; 3

        ; 8 scan lines to draw sprite			#23 total
              ldy #7				; 2		5
              ldx #$C2				; 2		7
              lda #2				; 2		9
              sta NUSIZ0			; 3		12
              sta NUSIZ1			; 3		15
DrawSprite			

              sta WSYNC				; 3		3
              lda (SpritePtr3),y	; 5		8
              sta GRP1				; 3		11
              lda (SpritePtr2),y	; 5		16
              sta GRP0				; 3		19
			  lda #TUNNEL_COLOR		; 2		21
			  sta COLUP1			; 3		24
			  sta COLUP0			; 3		27 
              inx					; 2		29
              inx					; 2		31   
			  SLEEP 3				; 3		34
			  lda (SpritePtr1),y	; 5		39
              sta GRP1				; 3		42
              lda (SpritePtr0),y	; 5		47
              sta GRP0				; 3		50
			  lda #DEFAULT_COLOR	; 2		52
			  sta COLUP1			; 3		55
			  sta COLUP0			; 3		58
              dey					; 2		60 
              bpl DrawSprite		; 3		63

RunCode
              ; 5 scan lines to position game sprites	#28 total
              sta WSYNC				; 3     ; 1st
              lda FruitPosition		; 3
              and #$F0				; 2
              lsr					; 2
              lsr					; 2
              clc					; 2
              adc #48				; 2
              ldx #1				; 2
              jsr SetHorizPos		; 6     ; 2nd
              sta WSYNC				; 3     ; 3rd
              lda HeadPosition		; 3
              and #$F0				; 2
              lsr					; 2
              lsr					; 2
              clc					; 2
              adc #48				; 2
              ldx #0				; 2
              jsr SetHorizPos		; 6     ; 4th
              sta WSYNC				; 3     ; 5th
              sta HMOVE				; 3		6					
              ldx #0				; 2		8
              ldy #0				; 2		10
              stx Mask_Current		; 3		13
              stx Fruit_Current		; 3		16
              stx NUSIZ0			; 3		19
              stx NUSIZ1			; 3		22
              lda #$FF				; 2		24
              sta PF2a_Current		; 3		27
              sta PF0b_Current		; 3		30
              lda #$F0				; 2		32 		removing topright corner
              sta PF1b_Current		; 3		35
              lda StickFigureColor				; 3		38
              sta COLUP1			; 3		41
			  lda #HEADCOLOR			; 2		43
			  sta COLUP0			; 3		46
			  lda #0				; 2  	48
			  sta TempPF2			; 3		51 		removing topleft corner

			; 8*17 = 136	#164 total
Line1         sta WSYNC				; 3
              lda #0				; 2
              sta PF0				; 3	=> 8
              lda TempPF2				; 3
              sta PF1				; 3	=> 13
              lda PF2a_Current		; 3
              sta PF2				; 3	=> 19
              lda (FaceSpritePtr),y	; 5
              and Mask_Current    	; 3
              sta GRP0            	; 3 => 30
              lda StickFigure,y	; 4
              and Fruit_Current    	; 3
              sta GRP1            	; 3 => 41
              lda PF0b_Current		; 3
              sta PF0				; 3	=> 47
              lda PF1b_Current		; 3
              sta PF1				; 3	=> 53
              lda #0				; 2
              sta PF2				; 3	=> 58
              lda DisplayRight,x  	; 4
              asl                 	; 2
              asl                 	; 2
              asl                 	; 2
              asl                 	; 2
              sta PF0b_Temp       	; 3 => 73
              ldy #1				; 2 => 75 cycles
Line2         sta WSYNC				; 3 
              lda #0				; 2
              sta PF0				; 3	=> 8
              lda TempPF2				; 2
              sta PF1				; 3 => 13
              lda PF2a_Current		; 3
              sta PF2				; 3 => 19
              lda (FaceSpritePtr),y	; 5
              and Mask_Current    	; 3
              sta GRP0            	; 3 => 30
              lda StickFigure,y	; 4
              and Fruit_Current    	; 3
              sta GRP1            	; 3 => 41
              lda PF0b_Current		; 3
              sta PF0				; 3 => 47
              lda PF1b_Current		; 3
              sta PF1				; 3 => 53
              lda #0				; 2
              sta PF2				; 3 => 58
              lda DisplayRight,x    ; 4
              lsr                   ; 2
              lsr                   ; 2
              lsr                   ; 2
              lsr                   ; 2
              sta PF1b_Temp			; 3 => 73
			  ldy #2				; 2 => 75 cycles

Line3         sta WSYNC				; 3
              lda #0				; 2
              sta PF0				; 3	=> 8
              lda TempPF2				; 2
              sta PF1				; 3	=> 13
              lda PF2a_Current		; 3
              sta PF2				; 3	=> 19
              lda (FaceSpritePtr),y	; 5
              and Mask_Current    	; 3
              sta GRP0            	; 3 => 30
              lda StickFigure,y	; 4
              and Fruit_Current    	; 3
              sta GRP1            	; 3 => 41
              lda PF0b_Current		; 3
              sta PF0				; 3	=> 47
              lda PF1b_Current		; 3
              sta PF1				; 3	=> 53
              lda #0				; 2
              sta PF2				; 3	=> 58
              ldy PF1b_Temp       	; 3
              lda PFData,y        	; 4
              sta PF1b_Temp       	; 3 => 68
              ldy #3				; 2	=> 70 cycles

Line4         sta WSYNC				; 3	
              lda #0				; 2
              sta PF0				; 3	=> 8
              lda TempPF2				; 2
              sta PF1				; 3	=> 13
              lda PF2a_Current		; 3
              sta PF2				; 3	=> 19
              lda (FaceSpritePtr),y	; 5
              and Mask_Current    	; 3
              sta GRP0            	; 3 => 30
              lda StickFigure,y	; 4
              and Fruit_Current    	; 3
              sta GRP1            	; 3 => 41
              lda PF0b_Current		; 3
              sta PF0				; 3	=> 47
              lda PF1b_Current		; 3
              sta PF1				; 3	=> 53
              lda #0				; 2
              sta PF2				; 3	=> 58
              lda DisplayLeft,x   	; 4
              sta PF2a_Temp       	; 3	=> 65
              lda HeadPosition		; 3
              and #$0F            	; 2
              sta Mask_Temp       	; 3 => 73
              ldy #4				; 2	=> 75 cycles

Line5         sta WSYNC				; 3 
              lda #0				; 2
              sta PF0				; 3 => 8
              lda TempPF2				; 2
              sta PF1				; 3 => 13
              lda PF2a_Current		; 3
              sta PF2				; 3	=> 19
              lda (FaceSpritePtr),y	; 5
              and Mask_Current    	; 3
              sta GRP0            	; 3 => 30
              lda StickFigure,y	; 4
              and Fruit_Current    	; 3
              sta GRP1            	; 3 => 41
              lda PF0b_Current		; 3
              sta PF0				; 3	=> 47
              lda PF1b_Current		; 3
              sta PF1				; 3 => 53
              lda #0				; 2
              sta PF2				; 3 => 58
              cpx Mask_Temp       	; 3
              bne SetMask			; 3
              lda #$FF				; 2
SetMask       sta Mask_Temp			; 3	=> 69
              ldy #5				; 2 => 71 cycles

Line6         sta WSYNC				; 3
              lda #0				; 2
              sta PF0				; 3	=> 8
              lda TempPF2				; 2
              sta PF1				; 3	=> 13
              lda PF2a_Current		; 3
              sta PF2				; 3	=> 19
              lda (FaceSpritePtr),y	; 5
              and Mask_Current    	; 3
              sta GRP0            	; 3 => 30
              lda StickFigure,y	; 4
              and Fruit_Current    	; 3
              sta GRP1            	; 3 => 41
              lda PF0b_Current		; 3
              sta PF0				; 3	=> 47
              lda PF1b_Current		; 3	
              sta PF1				; 3 => 53
              lda #0				; 2
              sta PF2				; 3	=> 58
              sta PF0				; 3	=> 61
              lda FruitPosition		; 3
              and #$0F            	; 2
              sta Fruit_Temp       	; 3 => 69
              ldy #6				; 2 => 71 cycles
;			  lda #0
;			  sta FrameToggle
			  
Line7         sta WSYNC				; 3
              lda TempPF2			; 2
              sta PF1				; 3	=> 8
              lda PF2a_Current		; 3
              sta PF2				; 3	=> 14
              lda (FaceSpritePtr),y	; 5
              and Mask_Current    	; 3
              sta GRP0            	; 3 => 25
              lda StickFigure,y	; 4
              and Fruit_Current    	; 3
              sta GRP1            	; 3 => 36
              lda PF0b_Current		; 3
              sta PF0				; 3	=> 42
              lda PF1b_Current		; 3
              sta PF1				; 3	=> 48
              lda #0				; 2
              sta PF2				; 3	=> 53
			  sta PF0				; 3 => 56
              cpx Fruit_Temp       	; 3
              bne SetFruitMask		; 3
              lda #$FF				; 2
SetFruitMask  sta Fruit_Temp		; 3	=> 67
              inx					; 2 => 69
              ldy #0				; 2	=> 71 cycles

Line8         sta WSYNC				; 3
              lda TempPF2			; 2
              sta PF1				; 3	=> 8
			  lda #1				; 3
			  sta TempPF2			; 3 => 14
              lda PF2a_Current		; 3
              sta PF2				; 3	=> 20
              lda PF2a_Temp       	; 3
              sta PF2a_Current    	; 3 => 26
              lda Mask_Temp       	; 3
              sta Mask_Current    	; 3 => 32
              lda Fruit_Temp       	; 3
              sta Fruit_Current    	; 3 => 38
              lda PF0b_Current		; 3
              sta PF0				; 3	=> 44
              lda PF1b_Current		; 3
              sta PF1				; 3	=> 50
              lda #0				; 2
              sta PF2				; 3	=> 55
              lda PF0b_Temp       	; 3
              sta PF0b_Current    	; 3 => 61
              lda PF1b_Temp       	; 3
              sta PF1b_Current    	; 3	=> 67

              cpx #17             	; 2
              beq LinesPost            	; 2
              jmp Line1           	; 3 => 74 cycles

			; 8 lines => #172 total  (209 after this)
LinesPost    
			  ldy #0
RepPost			  
			  sta WSYNC				; 3
              lda #0				; 2
              sta PF0				; 3	=> 8
              lda #0				; 2
              sta PF1				; 3	=> 13
              lda #$FF				; 2
              sta PF2				; 3	=> 19
              lda (FaceSpritePtr),y	; 5
              and Mask_Current    	; 3
              sta GRP0            	; 3 => 30
              lda #$FF				; 2
              sta PF0				; 3	=> 35
              lda #$F0				; 2
              sta PF1				; 3	=> 40
              lda StickFigure,y	; 4
              and Fruit_Current    	; 3
              sta GRP1            	; 3 => 51
              lda #0				; 2
              sta PF2				; 3	=> 56
              iny
			  cpy #8
			  beq PostDone			  
			  jmp RepPost
			; 1 line	#173 total
PostDone          sta WSYNC             ; 3
              ldx #0                ; 2
              stx PF0               ; 3
              stx PF1               ; 3
              stx PF2               ; 3
              ; 20 scan lines of nothing
              ldx #2			    ; 2	  			  	
              jsr WaitForLines      ; 6??
		; 2 scan lines to position sprite	;3 => #176 total
			lda #DISPLAY_POS        ; 2
			ldx #1                  ; 2
			jsr SetHorizPos         ; 6+6+27    ; 1st
			sta WSYNC               ; 3         ; 2nd
			lda #DISPLAY_POS        ; 2
			clc                     ; 2
			adc #6                  ; 2
			ldx #0                  ; 2
			jsr SetHorizPos	        ; 6+6+27    ; 3rd
		; 1 scanline to move sprite		#177 total
			sta WSYNC               ; 3
			sta HMOVE               ; 3

			lda #0                  ; 2
			sta VBLANK              ; 3     11
		; 1 scan line to choose sprites		
              lda #>Char0           ; 2
              sta SpritePtr0+1      ; 3
;              sta SpritePtr1+1      ; 3
              sta SpritePtr2+1      ; 3
;              sta SpritePtr3+1      ; 3
             lda EscapeCount       ; 3     28
              and #$0F              ; 2
              asl                   ; 2
              asl                   ; 2
              asl                   ; 2     36
              sta SpritePtr0        ; 3
;              lda EscapeCount       ; 3     42
;              and #$F0              ; 2
;              lsr                   ; 2     46
;              sta SpritePtr1        ; 3
              lda TunnelCount         ; 3     52
              and #$0F              ; 2
              asl                   ; 2
              asl                   ; 2
              asl                   ; 2     60
              sta SpritePtr2        ; 3
;              lda TunnelCount         ; 3     66
;              and #$F0              ; 2
;              lsr                   ; 2     70
;              sta SpritePtr3        ; 3     73 cycles
              sta WSYNC				; 3    
		; 8 => #185 total
        ; 8 scan lines to draw sprite
              ldy #7				; 2		5
;              ldx #$C2				; 2		
			  ldx #TUNNEL_COLOR		; 2		7 new version
              lda #2				; 2		9
              sta NUSIZ0			; 3		12
              sta NUSIZ1			; 3		15
			  stx COLUP1			; 3		18 new version 
			  ldx #DEFAULT_COLOR	; 2		20
			  stx COLUP0			; 3		23 new version
			  ldx #0				; 2		25
		  
DrawSprite2    
;			  SLEEP 8
              sta WSYNC				; 3		3
			  lda FaceOpen,x		; 4		7
              sta GRP1				; 3		10 cycles x 8
              lda (SpritePtr2),y	; 5		15
              sta GRP0				; 3		18
;              stx COLUP1			; 3		
;              stx COLUP0			; 3		
			  SLEEP 8				; 8 	26 need sleep for timing
              lda StickFigure,x		; 4		30
              sta GRP1				; 3		33
              lda (SpritePtr0),y	; 5		38
              inx					; 2		40
;              inx					; 2		
              dey					; 2		42
              SLEEP 10				; 10	52 need sleep for timing
              sta GRP0				; 3		55
              bpl DrawSprite2		; 3		58
			  ; (225 after this)
			 ; 3 lines	#188 total
; this is used to 'shake' the screen during 'tunneling'
			  sta WSYNC
              ldx #3				; 2		60 (no branch)
              lda FrameToggle		; 3		63
              beq Toggle2			; 2		65
              ldx #2				; 2		67
Toggle2	  			  			  	
              jsr WaitForLines

			  
              lda #%01000010

              sta VBLANK            ; end of screen - enter blanking
			  
			  ; 30 scanlines of overscan...
              TIMER_SETUP 30

;===============================================================================
; R O M - C O D E (Game Logic)
;===============================================================================
			  lda #0
			  sta CTRLPF
			  STA    VDELP0         
              ldx Sound             
              lda Effects,x         
              beq ClearSound        
              sta AUDC0             
              inx                   
              lda Effects,x         
              sta AUDF0             
              inc Sound             
              inc Sound             	
			  jmp CheckStick
ClearSound			  
		      lda #0
			  sta AUDC0
			  sta AUDF0
CheckStick    
			  jsr CheckFireButton 
			  jsr CheckFireButtonPerson
			  lda GameState
			  and #$80
			  bne .checkSwitch
.continue			 
              jsr CheckJoystick     
              stx NewDirection      
			  jsr CheckJoystickPerson
			  stx NewDirectionPerson
			  

.checkSwitch			  
; *** check switches: ***
; checking RESET
			LDA    SWCHB             
			LSR                      
			BCC    .checkToggleRES   
			LDA    GameState			; RESET not pressed, clear RES_PRESS
			AND    #$DF
			STA    GameState
			JMP    .noReset
; RES_PRESS => bit 5; ORA #$20 to set; AND #$DF to clear; one-shots reset button
.checkToggleRES					; RESET pressed, check for RES_PRESS bit
			LDA	  GameState
			AND    #$20
			BNE    .noReset			; branch if RES_PRESS high
			LDA    GameState			
			ORA    #$20				; set RES_PRESS
			STA    GameState
			JMP    Reset             ; 	 process RESET	
.noReset
; checking SELECT       
			LDA    SWCHB
			AND    #$02              ; SELECT was pressed        
			BEQ    .checkToggleSEL   ; If SELECT is pressed (bit 1 is 0), check toggle

			; If SELECT is not pressed, clear SEL_PRESS
			LDA    GameState          
			AND    #$BF               ; Clear bit 6 (SEL_PRESS)
			STA    GameState          
			JMP    .finishInput       ; Continue to Motion

.checkToggleSEL
			LDA    GameState          
			AND    #$40               ; Check SEL_PRESS (bit 6)
			BNE    .finishInput       ; If bit 6 is set, skip toggling
			
			LDA 	GameState
			AND		#$80
			BNE	   .continueCheck			

			LDA 	NewDirection
			BNE		.finishInput
			
.continueCheck
			; only proceed if gameover (bit7 high of gamestate) or NewDirection = #0
			; Toggle bit 4 and set SEL_PRESS
			LDA    GameState          
			EOR    #$08               ; Toggle bit 4 (worm/person control)
			ORA    #$40               ; Set SEL_PRESS (bit 6)
			STA    GameState           
              and #$08
              bne .revColor     ; reversed controls
			  lda #BLACK
			  sta COLUBK
			  jmp .finishInput
.revColor		
			  lda #DARK_GRAY
			  sta COLUBK
.finishInput
			  lda GameState
			  and #$80
			  bne WaitOver	 
	   
Motion        dec MotionCount       
              bne CheckFruit        
Timeout       lda MotionDelay      
              sta MotionCount       
			  
              lda NewDirection      
              beq WaitOver          

              jsr UpdateDirection  
			  
			  lda GameState
              and #$02
              bne .TunnelFace     ; sandowrm tunneling
              lda #<Face            
              sta FaceSpritePtr     
			  jmp .EndFace
.TunnelFace			
              lda #<FaceOpen           
              sta FaceSpritePtr     
.EndFace			  
              jsr Move              
              bvc CheckLen          
              jmp GameOver        ; Lose!  

CheckLen      lda Score             
              cmp #MAX_SCORE       
              bne CheckFruit        
              lda #0                
              sta NewDirection    ; Win!

;    lda FruitPosition ; if GS bit 0 = zero then we needto place the next fruit
CheckFruit    lda GameState         
              and #$01              
              bne WaitOver         
              lda RandomNum         ; check RandomNum corresponds to a free square
              jsr UnpackA           
              jsr CheckPlayField    
              bvs WaitOver         
			  jsr CheckCorner
			  bvs WaitOver			; check if fruit is in a corner			  
              sta FruitPosition     ; free square, not in corner
			  lda GameState2
			  and #$F0				; reset person moves count
			  sta GameState2
              lda GameState         
              ORA #$01              ; set fruit loaded flag
              sta GameState         ; 
			  lda #WHITE
			  sta StickFigureColor
			  lda #0
			  sta NewDirectionPerson 	; reset direction of person after each move
			  sta PersMoveHist
			  sta StayMoveHist

WaitOver      jsr GetRandom         ; cycle through random number every frame
              sta WSYNC             
              TIMER_WAIT            
              jmp StartOfFrame     

;===============================================================================
; R O M - C O D E (Subroutines)
;===============================================================================

; wait for X scanlines
WaitForLines SUBROUTINE
              dex
              sta WSYNC
              bne WaitForLines
              rts
			  
CheckFireButtonPerson SUBROUTINE
			lda GameState
			and #$08
			bne .CheckPlayer2Input
			lda INPT5
			jmp .Continue
.CheckPlayer2Input
			lda INPT4
.Continue
			and #$80
			bne .Return
			lda #0
			sta NewDirectionPerson
.Return
			rts

CheckFireButton SUBROUTINE
; TUNNELING   => bit 1; ORA #$02 to set; AND #$FD to clear; sandworm is tunneling
			lda GameState
			and #$80
			bne .CheckPlayer1Input
			lda GameState
            and #$08               ; Check bit 3 of GameState
            bne .CheckPlayer2Input       ; If bit 3 is set, use Player 2 	
.CheckPlayer1Input			
            lda INPT4       ; Load the input status for Player 1's fire button
			jmp .Continue
.CheckPlayer2Input
			lda INPT5
.Continue			
            and #$80        ; Mask the fire button bit (bit 7)
            beq .FirePressed ; If the button is pressed, bit 7 will be 0
			
			; If FIRE is not pressed, clear FIRE_PRESS
			LDA    GameState2          ; 3
			AND    #$EF               ; 2      Clear bit 0 (FIRE_PRESS)
			STA    GameState2          ; 3			
            rts             ; Return if not pressed

.FirePressed:
			; check if fire button already pressed
			LDA    GameState2          ; 3
			AND    #$10               ; 2      Check FIRE_PRESS (bit 0)
			BNE    .Return             ; 2      If bit 6 is set, skip toggling
            ; Do something when fire button is pressed
			lda GameState
			and #$80
			bne .Restart		; restart if game over
			; Don't do anything if NewDirection = 0
			lda NewDirection
			beq .Return
			; check if currently tunneling
			lda GameState
            and #$02
            bne .Return     ; sandowrm already tunneling
			; Process Fire
            lda TunnelCount
            beq .Return
            dec TunnelCount
            lda MoveCount
            sta MoveCountTemp
            lda #$F0
            sta MoveCount
            lda GameState
            ora #$02
            sta GameState
            lda #TUNNEL_COLOR
            sta COLUPF
			lda #RED
			sta StickFigureColor
			lda RoarIndex         ; trigger sound effect
            sta Sound
			rts
.Restart
			lda #0
			sta NewDirection
			jmp GameOver2
.Return
            rts

CheckJoystick SUBROUTINE
			lda GameState
			and #$04			; check if input allowed, bit 2
			beq .return
			
			lda GameState
			and #$80			; check if game over, bit 7
			bne .return
            
			ldx NewDirection
			
            lda GameState
            and #$08               ; Check bit 3 of GameState
            bne .CheckPlayer2       ; If bit 3 is set, use Player 2 

;CheckLeft
			lda #$40
			bit SWCHA
			bne .CheckRight			
            ldx #DIR_LEFT
            rts
.CheckRight 
			lda #$80
			bit SWCHA
			bne .CheckUP
            ldx #DIR_RIGHT
            rts
.CheckUP    
			lda #$10
            bit SWCHA
            bne .CheckDOWN
            ldx #DIR_UP
            rts
.CheckDOWN  
			lda #$20
            bit SWCHA
            bne .return
            ldx #DIR_DOWN
			rts
			
.CheckPlayer2 
;CheckLeftPlayer2 
            lda #$04
			bit SWCHA
            bne .CheckRightPlayer2
            ldx #DIR_LEFT
			lda GameState
			ora #$10
			sta GameState
            rts
.CheckRightPlayer2  
			lda #$08
			bit SWCHA
            bne .CheckUPPlayer2
            ldx #DIR_RIGHT
			lda GameState
			ora #$10
			sta GameState
            rts
.CheckUPPlayer2  
			lda #$01
            bit SWCHA
            bne .CheckDOWNPlayer2
            ldx #DIR_UP
			lda GameState
			ora #$10
			sta GameState
            rts
.CheckDOWNPlayer2  
			lda #$02
            bit SWCHA
            bne .return
            ldx #DIR_DOWN
			lda GameState
			ora #$10
			sta GameState
;            rts			
.return     rts

CheckJoystickPerson SUBROUTINE
			lda GameState
			and #$04			; check if input allowed, bit 2
			beq .return
			
			lda GameState
			and #$80			; check if game over, bit 7
			bne .return
            
			ldx NewDirectionPerson
			
            lda GameState
            and #$08               ; Check bit 3 of GameState
            beq .CheckPlayer2       ; If bit 3 is not set, use Player 2 

;CheckLeft
			lda #$40
			bit SWCHA
			bne .CheckRight	
			lda NewDirection
			bne .contL
			lda #DIR_RIGHT
			sta NewDirection
.contL			
            ldx #DIR_LEFT
            rts
.CheckRight 
			lda #$80
			bit SWCHA
			bne .CheckUP
			lda NewDirection
			bne .contR
			lda #DIR_RIGHT
			sta NewDirection
.contR			
            ldx #DIR_RIGHT
            rts
.CheckUP    
			lda #$10
            bit SWCHA
            bne .CheckDOWN
			lda NewDirection
			bne .contU
			lda #DIR_RIGHT
			sta NewDirection
.contU			
            ldx #DIR_UP
            rts
.CheckDOWN  
			lda #$20
            bit SWCHA
            bne .return
			lda NewDirection
			bne .contD
			lda #DIR_RIGHT
			sta NewDirection
.contD			
            ldx #DIR_DOWN
			rts		
.return     rts			
.CheckPlayer2 
; P2_ACTIVE => bit 4; ORA #$10 to set; AND #$EF to clear; uses player 2 input
;CheckLeftPlayer2 
            lda #$04
			bit SWCHA
            bne .CheckRightPlayer2
            ldx #DIR_LEFT
			lda GameState
			ora #$10
			sta GameState
            rts
.CheckRightPlayer2  
			lda #$08
			bit SWCHA
            bne .CheckUPPlayer2
            ldx #DIR_RIGHT
			lda GameState
			ora #$10
			sta GameState
            rts
.CheckUPPlayer2  
			lda #$01
            bit SWCHA
            bne .CheckDOWNPlayer2
            ldx #DIR_UP
			lda GameState
			ora #$10
			sta GameState
            rts
.CheckDOWNPlayer2  
			lda #$02
            bit SWCHA
            bne .return
            ldx #DIR_DOWN
			lda GameState
			ora #$10
			sta GameState
            rts	

UpdateDirection SUBROUTINE
            lda NewDirection
            bit Direction
            bne .return           ; overlap indicates invalid direction change
            sta Direction
.return     rts

; Move sandworm
Move SUBROUTINE move
; want to add logic here to automatically make moves in reverse 1P mode
; P2_ACTIVE => bit 4; ORA #$10 to set; AND #$EF to clear; uses player 2 input
; REV_P12 => bit 3; ORA #$08 to set; AND #$F7 to clear; reverse worm/player control
			lda GameState
			and #$10	; check p2_active
			bne .continueManualRep
			lda GameState
			and #$08	; check reverse mode
			beq .continueManualRep
; code here to determine Direction based on logic
			lda GameState
            and #$02
            beq .rollToChase     ; sandowrm not tunneling
			jmp .checkX
.rollToChase			
			jsr TrackStayMoveBits
			txa
			cmp #4
			bcs .checkX
			asl
			asl
			asl
			asl
			asl
			asl
;			tay
			cmp RandomNum
			bcs .checkX
			jmp .neither
.checkX		
			lda FruitPosition
			jsr UnpackA
			stx TempFruitPosition
			lda HeadPosition
			jsr UnpackA
			txa
			cmp TempFruitPosition
			bcc .x_is_less
			beq .checkY
; x is more
			lda Direction 
;			cmp #DIR_RIGHT
;			beq .checkY
			dex
			jsr CheckPlayField
			bvs .checkY
			lda #DIR_LEFT
			sta Direction
			jmp .continueManual
.x_is_less
			lda Direction
;			cmp #DIR_LEFT
;			beq .checkY
			inx
			jsr CheckPlayField
			bvs .checkY
			lda #DIR_RIGHT
			sta Direction
			jmp .continueManual		
.continueManualRep
			jmp .continueManual		
.checkY	
			lda FruitPosition
			jsr UnpackA
			sty TempFruitPosition
			lda HeadPosition
			jsr UnpackA
			tya
			cmp TempFruitPosition
			bcc .y_is_less
			beq .neither
; y is more	
			lda Direction
;			cmp #DIR_DOWN
;			beq .neither
			dey
			jsr CheckPlayField
			bvs .neither
			lda #DIR_UP
			sta Direction
			jmp .continueManual
.y_is_less
			lda Direction
;			cmp #DIR_UP
;			beq .neither
			iny
			jsr CheckPlayField
			bvs .neither
			lda #DIR_DOWN
			sta Direction
			jmp .continueManual		
.neither	
; check current direction
			lda Direction
			cmp #DIR_RIGHT
			bne .currentNotRight
			lda HeadPosition
			jsr UnpackA
			inx
			cpx #16
			beq .checkRight
			jsr CheckPlayField
			bvs .checkRight
			jmp .continueManual
.currentNotRight
			lda Direction
			cmp #DIR_LEFT
			bne .currentNotLeft
			lda HeadPosition
			jsr UnpackA
			dex
			bmi .checkRight
			jsr CheckPlayField
			bvs .checkRight
			jmp .continueManual
.currentNotLeft
			lda Direction
			cmp #DIR_UP
			bne .currentNotUp
			lda HeadPosition
			jsr UnpackA
			dey
			bmi .checkRight
			jsr CheckPlayField
			bvs .checkRight
			jmp .continueManual
.currentNotUp
			lda Direction
			cmp #DIR_DOWN
			bne .checkRight
			lda HeadPosition
			jsr UnpackA
			iny
			cpx #16
			beq .checkRight
			jsr CheckPlayField
			bvs .checkRight
			jmp .continueManual
;go through each direction, taking first without a collision
.checkRight	
			lda HeadPosition
			jsr UnpackA
			inx
			cpx #16
			beq .checkUp
			jsr CheckPlayField
			bvs .checkUp
			lda #DIR_RIGHT
			sta Direction
			sta NewDirection
			jmp .continueManual
.checkLeft			
			lda HeadPosition
			jsr UnpackA
			dex
			bmi .checkDown
			jsr CheckPlayField
			bvs .checkDown
			lda #DIR_LEFT
			sta Direction
			sta NewDirection
			jmp .continueManual
.checkUp			
			lda HeadPosition
			jsr UnpackA
			dey
			bmi .checkLeft
			jsr CheckPlayField
			bvs .checkLeft
			lda #DIR_UP
			sta Direction
			sta NewDirection
			jmp .continueManual
.checkDown		
			lda #DIR_DOWN
			sta Direction
			sta NewDirection
.continueManual		
            lda HeadPosition
			; 2nd jsr's here
            jsr UnpackA			; 2nd level
            jsr UpdatePlayField ;2nd level + 0.5; Put body segment at old head location
            jsr UnpackA			; 2nd level
            lda Direction           ; Use direction to calculate new head loc
.left       cmp #DIR_LEFT
            bne .right
            dex
            bpl .continue
            bmi .collision_rep
.right      cmp #DIR_RIGHT
            bne .up
            inx
            cpx #16
            bne .continue
            beq .collision_rep
.up         cmp #DIR_UP
            bne .down
            dey
            bpl .continue
            bmi .collision_rep
.down       iny
            cpy #16
            beq .collision
.continue   jsr PackXY				; 2nd level
			cmp TailPosition
			bne .check_collision
			jmp .continue_move
.check_collision
            jsr CheckPlayField      ;2nd level +0.5 check that new location is empty
            bvs .collision
.continue_move
            sta HeadPosition
            cmp FruitPosition       ; check if new location contains fruit
            beq .grow               ; if so grow rather than move
			jsr CheckMoveCount		; 2nd level			  
			lda EscapeCount
			cmp #ESCAPE_FAIL
			bcs .collision
			lda HeadPosition
            cmp FruitPosition       ; check if new location contains fruit
            beq .grow               ; if so grow rather than move
.move       jsr TrimTail			; goes up to 3.5 levels
            jsr GrowBody			; 2nd level

            rts
.collision_rep
            jmp .collision
            rts
.grow       jsr GrowBody			; 2nd level
            lda #0
            sta FruitPosition
			lda #BLACK
			sta StickFigureColor
            lda #DEFAULT_COLOR
            sta COLUPF

            lda GameState
            and #$02
            beq .skipTunnel     ; sandowrm already tunneling
            lda GameState
            AND #$FD 
            sta GameState           ; clear tunneling, restore movecount
            lda MoveCountTemp
            and #$F0
            sta MoveCount
.skipTunnel
			lda MoveCount
			and #$F0
			sta MoveCount
            lda GameState
            and #$FE                ; clear fruit loaded
            sta GameState
            sed                     ; increase score using BCD addition
            clc
            lda Score
            adc #1
            cld
            sta Score
            and #$0F
            cmp #$05
            bne .noMod5
            inc TunnelCount
.noMod5
            lda #<FaceClose         ; set face sprite to a closed mouth
            sta FaceSpritePtr
            lda #MUNCH_DELAY        ; increase motion delay to make visible
            sta MotionCount
            lda #ChimeIndex         ; trigger sound effect
            sta Sound
            clv
            rts
.collision  bit .return             ; set overflow flag
.return     rts

CheckMoveCount SUBROUTINE
			; 2nd level called from Move subroutine
			lda MoveCount
            clc
            adc #1
            sta MoveCount
            lda MoveCount
            and #$0F
            sta Temp
            lda MoveCount
            lsr
            lsr
            lsr
            lsr
            and #$0F
            cmp Temp
            beq .equal
            jmp .notequal
.equal      
			;jsr TrackFruitMoves
			lda GameState2
			and #$0F
			cmp #$0F
			beq .noIncrement
			inc GameState2
.noIncrement
			jsr MoveFruitToCorner   ; 3rd level, move the foodposition   
			lda #0
			sta NewDirectionPerson	; reset direction of person after each move
            lda MoveCount
            and #$F0
            sta MoveCount    			
.notequal   clv
			rts
			
TrackStayMoveBits SUBROUTINE
			lda StayMoveHist
			ldx #$00
			ldy #$08
.countBits
			asl
			bcc .noIncrement
			inx
.noIncrement
			dey
			bne .countBits
			rts
			
TestFruitMoves SUBROUTINE
			lda PersMoveHist
			sta TempMoveHist
			asl
			asl
			sta TempMoveHist
			txa
			ora TempMoveHist
			sta TempMoveHist
			jsr TrackStayMoveBits
			cpx #$02
			bcs .greater_4
			rts
.greater_4
			cpx #$03
			bcs .continue4
			jmp .greater_3
.continue4	
			lda TempMoveHist
			lsr
			lsr
			lsr
			lsr
			sta Temp
			lda TempMoveHist
			and #$0F
			cmp Temp
			beq .pattern					
.greater_3
			lda TempMoveHist
			and #%00110000
			lsr
			lsr
			lsr
			lsr
			sta Temp
			lda TempMoveHist
			and #%00001100
			lsr
			lsr
			cmp Temp
			bne .return
			lda TempMoveHist
			and #%00000011
			cmp Temp
			bne .return
.pattern
			bit .return         ; set overflow flag
.return	
			rts

TrackFruitMoves SUBROUTINE
; reference GameState2; increment lower nibble if less than 1111
; StayMoveHist ; stores last 8 move/stay flags of person
			
; PersMoveHist ; stores last 4 moves of person
			lda PersMoveHist
			asl
			asl
			sta PersMoveHist
			txa
			ora PersMoveHist
			sta PersMoveHist
			lda StayMoveHist
			asl
			ora #$01
			sta StayMoveHist
; add code here to check for movement patterns
;, but only if reverse mode or 2P
			lda GameState
			and #$10	; check p2_active
			bne .continue
			lda GameState
			and #$08	; check reverse mode
			bne .continue
			jmp .return
.continue		
; check if currently tunneling
			lda GameState
            and #$02
            beq .continue2     ; sandowrm already tunneling
			jmp .return
.continue2			
; check GameState2 and #$0F to see 
;			lda GameState2
;			and #$0F
;			cmp #$03	;if there's been at least 3 move scans, continue
;			bcs .greater_6
;			jmp .return
;.greater_6		
	; check total high bits of StayMoveHist >= 6
	; this is now working. other is not
			jsr TrackStayMoveBits
			cpx #MOVELIMIT_PERSON
			bcs .tunnel		; trigger on >= X moves out of 8 scans
			cpx #$03
			bcs .greater_4
			lda #WHITE
			sta StickFigureColor
			jmp .return
			
.greater_4			
			cpx #$04
			bcs .continue4
			jmp .greater_3
.continue4			
;			lda #YELLOW
;			sta StickFigureColor
	;if >= 4, then check lower nibble equal upper nibble of PersMoveHist
			lda PersMoveHist
			lsr
			lsr
			lsr
			lsr
			sta Temp
			lda PersMoveHist
			and #$0F
			cmp Temp
			beq .tunnel				

.greater_3		
			cpx #$05
			bcs .colorYELLOW
			jmp .continue3
.colorYELLOW	
			lda #YELLOW
			sta StickFigureColor
.continue3
	;if >= 3, then check 2 bits repeat 3 times
			lda PersMoveHist
			and #%00110000
			lsr
			lsr
			lsr
			lsr
			sta Temp
			lda PersMoveHist
			and #%00001100
			lsr
			lsr
			cmp Temp
			bne .return
			lda PersMoveHist
			and #%00000011
			cmp Temp
			bne .return
			; continue to tunnel
			

; trigger free tunnel on any of these
.tunnel
			; Process Fire
            lda MoveCount
            sta MoveCountTemp
            lda #$F0
            sta MoveCount
            lda GameState
            ora #$02
            sta GameState
            lda #TUNNEL_COLOR
            sta COLUPF
			lda #RED
			sta StickFigureColor
			lda RoarIndex         ; trigger sound effect
            sta Sound
			rts
			
.return			
			rts

MoveFruitToCorner SUBROUTINE
;MOV_LEFT    = %00000000
;MOV_RIGHT   = %01000000
;MOV_UP      = %10000000
;MOV_DOWN    = %11000000
			; 3rd level, move->checkmovecount
			lda GameState
            and #$02
            bne .SkipChange     ; sandowrm tunneling
			lda #DEFAULT_COLOR
			sta COLUPF
			;check if p2active
			
; P2_ACTIVE => bit 4; ORA #$10 to set; AND #$EF to clear; uses player 2 input
; REV_P12 => bit 3; ORA #$08 to set; AND #$F7 to clear; reverse worm/player control
			lda GameState
			and #$18	;check if either bit 4 or 3 is high. manual control if so
			beq .SkipChange
			jmp .HandleManualMove
.SkipChange		
			jsr TrackStayMoveBits
			cpx #6				; AI avoids moving 7th time out of 8 scans
			bcs .NoMoveRep2	
            lda RandomNum         ; Load a random number
            and #$01              ; Use the least significant bit
            beq .MoveX            ; If 0, move in X direction
            jmp .MoveY            ; If 1, move in Y direction
.NoMoveRep2	; not moving at all
			jmp .NoMove
			rts  ; added extra, branch-out-of-range	
.MoveX
            jsr GetRandom		; 4th level
.CheckX			
            lda FruitPosition
            jsr UnpackA			; 4th level
            cpx #8
            bcc .CheckXDown
            cpx #15
            bne .cont_CheckX
			jmp .CheckY
.cont_CheckX			
            inx
            jsr PackXY			; 4th level
			sta TempFruitPosition			
            jsr CheckPlayField	; 4th level, 0.5 for pha/pla
            bvs .NoMoveRep
			;check for pattern now
			ldx #$01
			jsr TestFruitMoves
			bvs .CheckXRev		
			lda TempFruitPosition
            sta FruitPosition
			; moving right
			ldx #$01
			jsr TrackFruitMoves
            rts 	
.CheckXRev
			lda FruitPosition
			jsr UnpackA
			cpx #0
			beq .NoMoveRep
			dex
			jsr PackXY
			sta TempFruitPosition
			jsr CheckPlayField
			bvs .NoMoveRep
			lda TempFruitPosition
			sta FruitPosition
			ldx #$00
			jsr TrackFruitMoves
			rts
.CheckXDown
			lda FruitPosition
            jsr UnpackA			; 4th level
            cpx #0
            beq .CheckY
            dex
            jsr PackXY			; 4th level
			sta TempFruitPosition			
            jsr CheckPlayField	; 4th level + 0.5
            bvs .NoMoveRep
			;check for pattern now
			ldx #$00
			jsr TestFruitMoves
			bvs .CheckXDownRev	
			lda TempFruitPosition
            sta FruitPosition
			; moving left
			ldx #$00
			jsr TrackFruitMoves
            rts
.CheckXDownRev	
			lda FruitPosition
			jsr UnpackA
			cpx #15
			beq .NoMoveRep
			inx
			jsr PackXY
			sta TempFruitPosition
			jsr CheckPlayField
			bvs .NoMoveRep
			lda TempFruitPosition
			sta FruitPosition
			ldx #$01
			jsr TrackFruitMoves
			rts		
.NoMoveRep	; not moving at all
			jmp .NoMove
			rts  ; added extra, branch-out-of-range		
.CheckY     
			lda FruitPosition
            jsr UnpackA			; 4th level
			cpy #8
            bcc .CheckYDown
            cpy #15
			bne .cont_CheckY
            jmp .EscapePositionRep
.cont_CheckY			
            iny
            jsr PackXY			; 4th level
			sta TempFruitPosition			
            jsr CheckPlayField	; 4th level + 0.5
            bvs .NoMoveRep
			;check for pattern now
			ldx #$02
			jsr TestFruitMoves
			bvs .CheckYRev
			lda TempFruitPosition
            sta FruitPosition
			; moving up
			ldx #$02
			jsr TrackFruitMoves
            rts
.CheckYRev
			lda FruitPosition
			jsr UnpackA
			cpy #0
			beq .NoMoveRep
			dey
			jsr PackXY
			sta TempFruitPosition
			jsr CheckPlayField
			bvs .NoMoveRep
			lda TempFruitPosition
			sta FruitPosition
			ldx #$03
			jsr TrackFruitMoves
			rts
.CheckYDown 
			lda FruitPosition
            jsr UnpackA			; 4th level
			cpy #0
			bne .cont_CheckYDown
            jmp .EscapePositionRep
.cont_CheckYDown			
            dey
            jsr PackXY			; 4th level
			sta TempFruitPosition			
            jsr CheckPlayField	; 4th level
            bvs .NoMoveRep
			;check for pattern now
			ldx #$03
			jsr TestFruitMoves
			bvs .CheckYDownRev		
			lda TempFruitPosition
            sta FruitPosition
			; moving down
			ldx #$03
			jsr TrackFruitMoves
            rts	
.CheckYDownRev
			lda FruitPosition
			jsr UnpackA
			cpy #15
			beq .NoMoveRep
			iny
			jsr PackXY
			sta TempFruitPosition
			jsr CheckPlayField
			bvs .NoMoveRep3
			lda TempFruitPosition
			sta FruitPosition
			ldx #$02
			jsr TrackFruitMoves
			rts	
.NoMoveRep3	; not moving at all
			jmp .NoMove
			rts  ; added extra, branch-out-of-range		
.MoveY
            jsr GetRandom		; 4th level
.CheckY2			
            lda FruitPosition
            jsr UnpackA			; 4th level
            cpy #8
            bcc .CheckYDown2
            cpy #15
			bne .cont_CheckY2
            jmp .CheckX2
.cont_CheckY2			
            iny
            jsr PackXY			; 4th level
			sta TempFruitPosition			
            jsr CheckPlayField	; 4th level + 0.5
            bvs .NoMoveRep3
			;check for pattern now
			ldx #$02
			jsr TestFruitMoves
			bvs .CheckY2Rev
			lda TempFruitPosition
            sta FruitPosition
			; moving up
			ldx #$02
			jsr TrackFruitMoves
            rts	
.CheckY2Rev
			lda FruitPosition
			jsr UnpackA
			cpy #0
			beq .NoMoveRep3
			dey
			jsr PackXY
			sta TempFruitPosition
			jsr CheckPlayField
			bvs .NoMoveRep3
			lda TempFruitPosition
			sta FruitPosition
			ldx #$03
			jsr TrackFruitMoves
			rts	

.EscapePositionRep
			jmp .EscapePosition
.CheckYDown2
			lda FruitPosition
            jsr UnpackA			; 4th level
            cpy #0
            beq .CheckX2
            dey
            jsr PackXY			; 4th level
			sta TempFruitPosition			
            jsr CheckPlayField	; 4th level + 0.5
            bvs .NoMoveRep3
			;check for pattern now
			ldx #$03
			jsr TestFruitMoves
			bvs .CheckYDown2Rev
			lda TempFruitPosition
            sta FruitPosition
			; moving down
			ldx #$03
			jsr TrackFruitMoves
            rts
.CheckYDown2Rev
			lda FruitPosition
			jsr UnpackA
			cpy #15
			beq .NoMoveRep4
			iny
			jsr PackXY
			sta TempFruitPosition
			jsr CheckPlayField
			bvs .NoMoveRep4
			lda TempFruitPosition
			sta FruitPosition
			ldx #$02
			jsr TrackFruitMoves
			rts	
.NoMoveRep4	; not moving at all
			jmp .NoMove
			rts  ; added extra, branch-out-of-range		
.CheckX2    
			lda FruitPosition
            jsr UnpackA			; 4th level
			cpx #8
            bcc .CheckXDown2
            cpx #15
			bne .cont_CheckX2
            jmp .EscapePosition
.cont_CheckX2			
            inx
            jsr PackXY			; 4th level
			sta TempFruitPosition			
            jsr CheckPlayField	; 4th level + 0.5
            bvs .NoMove
			;check for pattern now
			ldx #$01
			jsr TestFruitMoves
			bvs .CheckX2Rev
			lda TempFruitPosition
            sta FruitPosition
			; moving right
			ldx #$01
			jsr TrackFruitMoves
            rts
.CheckX2Rev
			lda FruitPosition
			jsr UnpackA
			cpx #0
			beq .NoMoveRep4
			dex
			jsr PackXY
			sta TempFruitPosition
			jsr CheckPlayField
			bvs .NoMoveRep4
			lda TempFruitPosition
			sta FruitPosition
			ldx #$00
			jsr TrackFruitMoves
			rts	
.CheckXDown2 
			lda FruitPosition
            jsr UnpackA			; 4th level
			cpx #0
            beq .EscapePosition
            dex
            jsr PackXY			; 4th level
			sta TempFruitPosition			
            jsr CheckPlayField	; 4th level + 0.5
            bvs .NoMove
			;check for pattern now
			ldx #$00
			jsr TestFruitMoves
			bvs .CheckXDown2Rev
			lda TempFruitPosition
            sta FruitPosition
			; moving left
			ldx #$00
			jsr TrackFruitMoves
            rts
.CheckXDown2Rev
			lda FruitPosition
			jsr UnpackA
			cpx #15
			beq .NoMove
			inx
			jsr PackXY
			sta TempFruitPosition
			jsr CheckPlayField
			bvs .NoMove
			lda TempFruitPosition
			sta FruitPosition
			ldx #$01
			jsr TrackFruitMoves
			rts
.NoMove		; not moving at all
			lda StayMoveHist
			asl
			sta StayMoveHist
			lda #WHITE
			sta StickFigureColor
			rts
.EscapePosition                       ; person escaped
            lda EscapeCount
			;sed
            clc
            adc #1
			;cld
            sta EscapeCount
            lda #YELLOW
            sta COLUPF
            lda #EscapeIndex         ; trigger sound effect
            sta Sound
            lda GameState
            and #$02
            beq .SkipUnTunnel     ; sandowrm not tunneling
            lda GameState
            AND #$FD 
            sta GameState           ; reset tunneling to clear
            lda MoveCountTemp
            and #$F0
            sta MoveCount
.SkipUnTunnel
            jsr UpdateMotionDelay	; 4th level
			lda EscapeCount
			and #$01
			bne .SkipCountLimit
            lda MoveCount
            and #$F0
            lsr
            lsr
            lsr
            lsr
            sta Temp
            dec Temp
            cmp #1
            bne .SkipClamp
            lda #1
            sta Temp
.SkipClamp  lda Temp
            asl
            asl
            asl
            asl
;            and #$F0
            sta MoveCount
.SkipCountLimit			
			lda MoveCount
			and #$F0
			sta MoveCount
            lda GameState
            and #$FE
            sta GameState
            lda GameState
            and #$01
            lda RandomNum     ; check RandomNum corresponds to a free square
            jsr UnpackA				; 4th level
            jsr CheckPlayField		; 4th level + 0.5
			bvs .NotLoaded
			jsr CheckCorner
			bvs .NotLoaded			; check if fruit is in a corner			 
            sta FruitPosition ; if so place fruit here
			lda GameState2
			and #$F0			; reset lower nibble person move count
			sta GameState2		
            lda GameState
            ORA #$01			; set fruit loaded
            sta GameState
			lda #WHITE
			sta StickFigureColor
			lda #0
			sta StayMoveHist
.NotLoaded			
            clv
            rts   
.HandleManualMove
			; check if in corner; if yes, jmp to escapeposition
			lda FruitPosition
			jsr UnpackA
			jsr CheckCornerZero
			bvs .EscapePositionRep2		; escape automaticaly if in a corner
			lda NewDirectionPerson
			bne .ProcessDirection
			jmp .NoMove				; newdirectionperson = 0, no move
			; check NewDirectionPerson; if no direction, stay put
.EscapePositionRep2
			jmp .EscapePosition
.ProcessDirection ; A still holds direction			
.left       cmp #DIR_LEFT
            bne .right
            dex
            bpl .continue
            bmi .collision
.right      cmp #DIR_RIGHT
            bne .up
            inx
            cpx #16
            bne .continue
            beq .collision
.up         cmp #DIR_UP
            bne .down
            dey
            bpl .continue
            bmi .collision
.down       cmp #DIR_DOWN
			bne .nomove2
			iny
            cpy #16
            beq .collision
.continue   
            jsr CheckPlayField      ;2nd level +0.5 check that new location is empty
            bvs .collision
.continue_move		
		;	jsr PackXY
		;	sta FruitPosition
			lda NewDirectionPerson
			cmp #DIR_LEFT
			bne .rightS
			lda FruitPosition
			jsr UnpackA
			dex
			jsr PackXY
			sta FruitPosition
			ldx #$01
			jmp .fin
.rightS		
			cmp #DIR_RIGHT
			bne .upS
			lda FruitPosition
			jsr UnpackA
			inx
			jsr PackXY
			sta FruitPosition
			ldx #$00
			jmp .fin
.upS
			cmp #DIR_UP
			bne .downS
			lda FruitPosition
			jsr UnpackA
			dey
			jsr PackXY
			sta FruitPosition
			ldx #$02
			jmp .fin
.downS
			cmp #DIR_DOWN
			bne .fin
			lda FruitPosition
			jsr UnpackA
			iny
			jsr PackXY
			sta FruitPosition
			ldx #$03
.fin			
			jsr TrackFruitMoves
			rts
.nomove2
			jmp .NoMove
			rts
.collision
			jmp .NoMove
			rts

UpdateMotionDelay SUBROUTINE
            lda EscapeCount          ; Load EscapeCount
            cmp #5                   ; Compare EscapeCount to 7
            bcc .UseDefaultDelay     ; If EscapeCount <= 7, use default delay

            sec                      ; Subtract 7 from EscapeCount
            sbc #5                   ; A = EscapeCount - 7
            sta Temp                 ; Store the result temporarily

            lda #10                  ; Load base value (10)
            sec                      ; Subtract (EscapeCount - 7)
            sbc Temp                 ; A = 10 - (EscapeCount - 7)
            bne .ClampDelay          ; If result >= 1, skip clamping

            lda #1                   ; Clamp delay to minimum of 1
.ClampDelay:
            sta MotionDelay         ; Store the calculated delay
            rts

.UseDefaultDelay:
            lda #10                  ; Default delay (10)
            sta MotionDelay         ; Store the default delay
            rts

GrowBody SUBROUTINE
            lda Direction           ; convert direction to movement value
            bpl .vertical
            and #%01000000
            bpl .store
.vertical   asl
            asl

.store      ldx FreeOffset          ; store new movement value
            beq .done
.shift      lsr
            lsr
            dex
            bne .shift
.done       ldx FreeIndex
            ora Body,x
            sta Body,x

            inc FreeOffset
            lda FreeOffset
            cmp #4
            bne .return
            lda #0
            sta FreeOffset
            inc FreeIndex
            ldx FreeIndex
            sta Body,x
.return     rts

TrimTail SUBROUTINE
			; 2nd level from move
            lda TailLoc           ; find movement value at tail
            ldx TailOffset
            beq .done
.shift      asl
            asl
            dex
            bne .shift
.done       and #%11000000
            pha

            lda TailPosition      ; clear tail position
            jsr UnpackA			  ; 3rd level
            jsr UpdatePlayField	  ; 3rd level + 0.5

            jsr UnpackA           ; 3rd level update tail position
            pla
            bne .right
            dex
.right      cmp #MOV_RIGHT
            bne .up
            inx
.up         cmp #MOV_UP
            bne .down
            dey
.down       cmp #MOV_DOWN
            bne .update
            iny
.update     jsr PackXY			 ; 3rd level
            sta TailPosition

            inc TailOffset
            lda TailOffset
            cmp #4
            bne .return
            lda #0
            sta TailOffset

            ldx FreeIndex
            dec FreeIndex
            lda Body,x
            dex
.reorder    ldy Body,x              ; shift backwards overwriting tail byte
            sta Body,x
            tya
            dex
            bpl .reorder

.return     rts

GameOver SUBROUTINE
			lda GameState
            and #$FB		;clear enable joystick flag
			ora #$80		;set game over bit
            sta GameState
			
			lda #RED
			sta COLUPF
			
            lda CrashIndex
            sta Sound	
			
            lda Score             ; Game Over!
            cmp HiScore
            bcc .NoHiScore
            sta HiScore					
					
.NoHiScore  
            lda #0
            sta NewDirection
			sta NewDirectionPerson


;			ldx #16
;;.ResetPF    		
;			dex
;            sta DisplayLeft,x
;            sta DisplayRight,x
;            bne .ResetPF			
			
			jmp StartOfFrame
;            jmp Restart

GameOver2 SUBROUTINE
 
            lda #0
            sta Score
            sta NewDirection


			ldx #16
.ResetPF    		
			dex
            sta DisplayLeft,x
            sta DisplayRight,x
            bne .ResetPF	
			
            jmp Restart

;check if fruit spawning near a corner		
CheckCorner SUBROUTINE
    cmp #$00         ; Check if A == $00			1
    beq .SetOverflow ; If equal, set overflow
    cmp #$01         ; Check if A == $01			2
    beq .SetOverflow ; If equal, set overflow
    cmp #$10         ; Check if A == $10			2
    beq .SetOverflow ; If equal, set overflow
    cmp #$20         ; Check if A == $10			3
    beq .SetOverflow ; If equal, set overflow
    cmp #$11         ; Check if A == $10			3
    beq .SetOverflow ; If equal, set overflow
    cmp #$02         ; Check if A == $10			3
    beq .SetOverflow ; If equal, set overflow
    cmp #$0F         ; Check if A == $0F			1
    beq .SetOverflow ; If equal, set overflow
    cmp #$1F         ; Check if A == $1F			2
    beq .SetOverflow ; If equal, set overflow
    cmp #$0E         ; Check if A == $0E			2
    beq .SetOverflow ; If equal, set overflow
    cmp #$0D         ; Check if A == $0E			3
    beq .SetOverflow ; If equal, set overflow
    cmp #$1E         ; Check if A == $0E			3
    beq .SetOverflow ; If equal, set overflow
    cmp #$2F         ; Check if A == $0E			3
    beq .SetOverflow ; If equal, set overflow
    cmp #$F0         ; Check if A == $F0			1
    beq .SetOverflow ; If equal, set overflow
    cmp #$F1         ; Check if A == $F1			2
    beq .SetOverflow ; If equal, set overflow
    cmp #$E0         ; Check if A == $E0			2
    beq .SetOverflow ; If equal, set overflow
    cmp #$D0         ; Check if A == $E0			3
    beq .SetOverflow ; If equal, set overflow
    cmp #$E1         ; Check if A == $E0			3
    beq .SetOverflow ; If equal, set overflow
    cmp #$F2         ; Check if A == $E0			3
    beq .SetOverflow ; If equal, set overflow
    cmp #$FE         ; Check if A == $FE			2
    beq .SetOverflow      ; If equal, set overflow
    cmp #$EF         ; Check if A == $EF			2
    beq .SetOverflow      ; If equal, set overflow
    cmp #$DF         ; Check if A == $EF			3
    beq .SetOverflow      ; If equal, set overflow
    cmp #$EE         ; Check if A == $EF			3
    beq .SetOverflow      ; If equal, set overflow
    cmp #$FD         ; Check if A == $EF			3
    beq .SetOverflow      ; If equal, set overflow
    cmp #$FF         ; Check if A == $FF			1
    bne .Return      ; If not equal to any, return	
.SetOverflow
	bit .Return
.Return
    rts
	
;check if fruit spawning in a corner		
CheckCornerZero SUBROUTINE
    cmp #$00         ; Check if A == $00
    beq .SetOverflow ; If equal, set overflow
    cmp #$0F         ; Check if A == $0F
    beq .SetOverflow ; If equal, set overflow
    cmp #$F0         ; Check if A == $F0
    beq .SetOverflow ; If equal, set overflow
    cmp #$FF         ; Check if A == $FF
    bne .Return      ; If not equal to any, return	
.SetOverflow
	bit .Return
.Return
    rts

; CheckPlayField routine
CheckPlayField SUBROUTINE
            pha
            cpx #8
            bcc .continue
            ; add 17 to y to access right hand side of screen!
            tya
            clc
            adc #17
            tay
            ; reallign x relative to right hand side of screen
            txa
            sec
            sbc #8
            tax
.continue   lda BitMask,x
            and DisplayLeft,y   ; is bit already set?
            bne .collision
            pla
            clv
            rts
.collision  pla
            bit .return         ; set overflow flag
.return     rts

; UpdatePlayField routine
UpdatePlayField SUBROUTINE
            pha
            cpx #8
            bcc .continue
            ; add 17 to y to access right hand side of screen!
            tya
            clc
            adc #17
            tay
            ; reallign x relative to right hand side of screen
            txa
            sec
            sbc #8
            tax
.continue   lda BitMask,x
            eor DisplayLeft,y
            sta DisplayLeft,y
            pla
            rts

; Pack X and Y into A
PackXY SUBROUTINE
            txa
            asl
            asl
            asl
            asl
            sta Temp
            tya
            ora Temp
            rts

; UnpackA into X and Y
UnpackA SUBROUTINE
            sta Temp
            lsr
            lsr
            lsr
            lsr
            tax
            lda #$0F
            and Temp
            tay
            lda Temp
            rts

; Get next random number
GetRandom SUBROUTINE
            lda RandomNum   ; 3
            lsr             ; 2
            bcc .NoEor      ; 2
            eor #$D4        ; 3
.NoEor      sta RandomNum   ; 3
            rts             ; 6

; SetHorizPos routine
; A = X coordinate
; RESP0+X = reset register to strobe
SetHorizPos SUBROUTINE
            cpx #2          ; 2; carry flag will be set for balls and missiles
            adc #0          ; 2; (adding 1 to account for different timings)
            sec             ; 2
            sta WSYNC       ; 3
.loop       sbc #15         ; 2
            bcs .loop       ; 3 ; usually 1 run
            eor #7          ; 2
            asl             ; 2
            asl             ; 2
            asl             ; 2
            asl             ; 2
            sta.a HMP0,X    ; 4 ; force absolute addressing for timing!
;			sta HMP0,X    ; 4 
            sta RESP0,X     ; 4
            rts             ; 6 => 31 cycles

            ALIGN  256

;===============================================================================
; R O M - C O D E (Data)
;===============================================================================
SpriteData

Char0    .byte $00,$38,$44,$64,$54,$4C,$44,$38
Char1    .byte $00,$38,$10,$10,$10,$10,$30,$10
Char2    .byte $00,$7C,$40,$20,$18,$04,$44,$38
Char3    .byte $00,$38,$44,$04,$18,$08,$04,$7C
Char4    .byte $00,$08,$08,$7C,$48,$28,$18,$08
Char5    .byte $00,$38,$44,$04,$04,$78,$40,$7C
Char6    .byte $00,$38,$44,$44,$78,$40,$20,$1C
Char7    .byte $00,$20,$20,$20,$10,$08,$04,$7C
Char8    .byte $00,$38,$44,$44,$38,$44,$44,$38
Char9    .byte $00,$70,$08,$04,$3C,$44,$44,$38
CharA    .byte $00,$42,$42,$42,$7E,$42,$24,$18   
CharB    .byte $00,$7C,$42,$42,$42,$7C,$42,$7C   
CharC    .byte $00,$7C,$40,$40,$40,$40,$40,$7C 
CharD    .byte $00,$7C,$42,$42,$42,$42,$42,$7C  
CharE    .byte $00,$7E,$40,$40,$40,$7C,$40,$7E  
CharF    .byte $00,$40,$40,$40,$40,$7C,$40,$7E 

Face:
			.byte %11110000
			.byte %11110000
			.byte %01100000
			.byte %01100000
			.byte %11110000
			.byte %10010000
			.byte %11110000
FaceClose   .byte %11110000
			.byte %11110000
			.byte %01100000
			.byte %01100000
			.byte %11110000
			.byte %11110000
			.byte %11110000
StickFigure .byte %01110000
			.byte %01010000
			.byte %01110000
			.byte %00100000
			.byte %01110000
			.byte %00100000
			.byte %00100000
			.byte %00000000
FaceOpen	.byte %11110000
			.byte %01100000
			.byte %11110000
			.byte %10010000
			.byte %10010000
			.byte %10010000
			.byte %11110000
			.byte %00000000				

Effects
ChimeEffect 	.byte 6,4,6,3,6,2,6,1,6,0,0
CrashEffect 	.byte 8,31,8,31,8,31,8,31,8,31,8,31,8,31,8,31,8,31,8,31,8,31,0
EscapeEffect	.byte 10,3,12,3,10,3,14,3,0 
RoarEffect  	.byte 15,3, 13,3, 11,4, 10,4, 9,5, 8,5, 7,5, 6,6, 5,6, 0

ChimeIndex = ChimeEffect - Effects
CrashIndex = CrashEffect - Effects
EscapeIndex = EscapeEffect - Effects
RoarIndex = RoarEffect - Effects

PFData   .byte %00001000,%10001000,%01001000,%11001000,%00101000,%10101000,%01101000,%11101000,%00011000,%10011000,%01011000,%11011000,%00111000,%10111000,%01111000,%11111000

BitMask  .byte %00000001, %00000010, %00000100, %00001000, %00010000, %00100000, %01000000, %10000000

            ORG $FFFA



            .word Reset          ; NMI

            .word Reset          ; RESET

            .word Reset          ; IRQ



      END
