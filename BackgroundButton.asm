;Key
;This is a disassembly of sprite 80 in SMW - Key.
;By RussianMan. Please give credit if used.
;Requested by Hamtaro126.

;Modified to act as a wall button
;sprite by HammerBrother.
;extra_byte_1: $00 = Can press again, $01 = pressed permanently
;extra_byte_2: color for YXPPCCCT:
; $00 = Palette 0 (LM row number $08)
; $02 = Palette 1 (LM row number $09)
; $04 = Palette 2 (LM row number $0A)
; $06 = Palette 3 (LM row number $0B)
; $08 = Palette 4 (LM row number $0C)
; $0A = Palette 5 (LM row number $0D)
; $0C = Palette 6 (LM row number $0E)
; $0E = Palette 7 (LM row number $0F)
;extra_byte_3: Not used but you can use this as a Lunar Magic-configured value
; to reuse this sprite for different switch action.

;Settings
 !GFXTile = $00				;>tile to display when button not pressed (use only $00-$FF)
 !GFXTile2 = $01			;>tile to display when button pressed (use only $00-$FF)
 !GFXPage = 1				;>0 = Page 0, 1 = page 1 (don't use any other values).
 !PressedTimer = 15			;>How many frames the button remains pressed before popping out (1 second is 60 frames) (use only $01-$FF)
 
;Sound effects, see https://www.smwcentral.net/?p=viewthread&t=6665
 !SFX_SoundNumb = $0B		;>Sound effect number
 !SFX_Port = $1DF9		;>Use only $1DF9, $1DFA, or $1DFB. 

;Sprite table defines
 ;Best not to modify
  !ButtonState = !1528
  !ButtonPressedTimer = !1540
 ;Feel free to modify these
  !FrozenTile = $0165 ;>Tile that turns the tile into when touching liquids.



;Action to perform when switch is pressed
macro SwitchAction()
	SwitchAction:
	;Example: Toggle on/off switch
		LDA $14AF|!addr
		EOR #$01
		STA $14AF|!addr
		RTS ;>Keep this RTS here else game will crash.
endmacro

Print "INIT ",pc
	RTL

Print "MAIN ",pc
	PHB
	PHK
	PLB
	JSR Button
	PLB
	RTL

Button:
	%SubOffScreen()			
	JSR HandleGFX			;Handle graphics
	LDA $9D				;freeze flag
	BEQ .RunMain			;

	.RunMain
		;JSR .HandleStun		;useless, handles stun that causes glitches

	.ProcessStates
		LDA !ButtonState,x
		ASL
		TAX
		JMP (..States,x)

		..States
			dw Pressible		;>$00
			dw PressedPopLater	;>$01 ($02)
			dw PressedIndefinitely	;>$02 ($04)

;-------------------------------------------------------------------------------------------------
Pressible:
	LDX $15E9|!addr			;>Current sprite index (in some cases, better than PHX, <use X for something else>, PLX)
	LDA $16				;\If not pressing UP, then don't press
	BIT.b #%00001000		;|
	BEQ .No				;/
	;Check if player is in the button's hitbox.
	;Prevents you from activating both by standing in between when 2 are side by side.
		;Mario clipping (Box B)
			JSL $03B664
			LDA $94				;\override X position
			CLC				;|
			ADC #$08			;|
			STA $00				;|
			LDA $95				;|
			ADC #$00			;|
			STA $08				;/
			STZ $02				;>override Width to be 0 (this basically makes Player's hitbox a vertical line)
		;Sprite clipping (Box A)
			LDA !E4,x		;\X position
			;CLC			;|
			;ADC #$04		;|
			STA $04			;|
			LDA !14E0,x		;|
			;ADC #$00		;|
			STA $0A			;/
			LDA !D8,x		;\Y position
			CLC			;|
			ADC #$08		;|
			STA $05			;|
			LDA !14D4,x		;|
			ADC #$00		;|
			STA $0B			;/
			LDA #$0F		;\Width (must be 15px, not 16, due to hitboxes count as colliding if they are edge to edge touching but not overlapping)
			STA $06			;/
			LDA #$10		;\Height
			STA $07			;/
		;Contact?
			JSL $03B72B		;>Check if A and B touching?
			BCC .No
	;Pressing the button
		LDA #!SFX_SoundNumb	;\Sound effect
		STA !SFX_Port|!addr	;/
		JSR SwitchAction
		LDA !extra_byte_1,x	;\Determine 1-press only or reusuable
		BNE .PressedForever	;/
		.PressedTemporally
			LDA.b #!PressedTimer
			STA !ButtonPressedTimer,x
			LDA #$01
			BRA .SetButtonState
		.PressedForever
			LDA #$02
		.SetButtonState
			STA !ButtonState,x
	.No
	RTS				;
;-------------------------------------------------------------------------------------------------
PressedPopLater:
	LDX $15E9|!addr			;>Current sprite index (in some cases, better than PHX, <use X for something else>, PLX)
	LDA !ButtonPressedTimer,x	;\Timer expires? If not, do nothing, else revert the button
	BNE .No				;/
	STZ !ButtonState,x		;>Revert button.
	.No
PressedIndefinitely:
	LDX $15E9|!addr			;>Restore X index
	RTS
	
	%SwitchAction()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;Graphics routine
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

HandleGFX:
	%GetDrawInfo()			;
	
	LDA $00				;\X position
	CLC				;|
	ADC #$04			;|
	STA $0300|!Base2,y		;/
	LDA $01				;\Y position
	CLC				;|
	ADC #$04			;|
	STA $0301|!Base2,y		;/
	
	LDA !ButtonState,x
	BNE .PressedGFX
	
	.UnpressedGFX
		LDA #!GFXTile
		BRA .WriteTileNumber
	.PressedGFX
		LDA #!GFXTile2
	.WriteTileNumber
		STA $0302|!Base2,y		;>Tile number
	
	LDA !extra_byte_2,x
	AND.b #%00001110	;>Ignore XY flips, page (forcibly set to 0 or 1), and priority
	ORA.b #(%00100000|!GFXPage)	;>Force only some bits of the PP to be set (should not appear behind layers without priority.)
	STA $0303|!Base2,y		;>Tile properties (YXPPCCCT)

	LDY #$00			;tile size = 8x8
	LDA #$00			;tiles to display = 1
	JSL $01B7B3|!BankB		;
	RTS				;