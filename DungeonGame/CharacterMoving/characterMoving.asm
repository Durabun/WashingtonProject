    processor 6502
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include required files with VCS register memory mapping and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    include "vcs.h"
    include "macro.h"
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare variables from memory address $80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg.u Variables
    org $80
    
CharXPos        byte                ; character x position
CharYPos        byte                ; character y position
Temp            byte                ; temporary register
CharSpritePtr   word                ; sprite pointer
CharColorPtr    word                ; color pointer
Random          byte                ; random variables
FloorColor      byte                ; floor color
WallColor       byte                ; wall color
TreeSeg         byte                ; tree segment counter
RoomLocation    byte    

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start our ROM code at memory address $F000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg Code
    org $F000
    
Reset:
    CLEAN_START                 ; call macro to reset memory and registers
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init RAM Variables and TIA Registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #40
    sta CharXPos                ; store x = 40
    lda #50
    sta CharYPos                ; store y = 50
    lda #%11010100
    sta Random                  ; random variable = $D4
    lda #$35                    
    sta FloorColor              ; load floor color
    lda #$F9
    sta WallColor               ; load wall color
    lda #1
    sta TreeSeg                 ; load tree segment counter
    lda #5
    sta RoomLocation            ; load room location

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load sprite and color pointers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #<CharSprite
    sta CharSpritePtr            
    lda #>CharSprite
    sta CharSpritePtr+1
    
    lda #<CharSpriteColor
    sta CharColorPtr           
    lda #>CharSpriteColor
    sta CharColorPtr+1
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start main display loop and frame rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display VSYNC and VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VBLANK                  ; turn on VBLANK
    sta VSYNC                   ; turn on VSYNC
    REPEAT 3
        sta WSYNC               ; Display 3 lines of VSYNC
    REPEND
    lda #0                      
    sta VSYNC                   ; Turn off VSYNC
    REPEAT 35
        sta WSYNC               ; Display 33 lines of VBLANK    
    REPEND
    ; since we are within the VSYNC/VBLANK region, set the x position of sprite(s)
    lda CharXPos               ; load character x position 
    ldy #0                      ; set it to the p0 mode
    jsr SetObjectXPos          ; set the object's x position
    
    sta WSYNC
    sta HMOVE                    ; apply move  
    
    lda #0
    sta VBLANK
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Render the 96 visible scanlines of main game (due to 2 line kernel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameVisibleLine:
    lda FloorColor              ; load floor color
    sta COLUBK                  ; set floor color
    lda #%00000001
    sta CTRLPF                  ; enable playfield reflection
    lda #$F0
    sta COLUPF
    lda #%01111110
    sta PF1
    ldx #192                     ; set x to remaining 192 scanlines
    

    
    
    
.GameLineLoop:
    ; main game loop
    ;sta WSYNC
    
.AreWeInsideChar:               ; loop to see if we are within y coords of sprite
    txa                          ; transfer x value (current scanline) to A
    sec                          ; set carry flag
    sbc CharYPos                ; subtract current scanline from character position
    cmp #9                       ; see if it is within sprite height
    bcc .DrawP0Sprite           ; if so, jump to draw sprite
    lda #0                       ; else set A to 0
.DrawP0Sprite:
    tay                          ; transfer A to Y    
    lda (CharSpritePtr),Y      ; load sprite pointer from y to A
    sta WSYNC                   ; draw scanline
    sta GRP0                    ; set graphics of sprite
    lda (CharColorPtr),Y       ; load sprite color ptr from y to A
    sta COLUP0                  ; set color of sprite
    
    dex                          ; x--
    bne .GameLineLoop           ; jump to game loop if still drawing lines
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display Overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VBLANK                  ; turn VBLANK on again
    REPEAT 30
        sta WSYNC               ; display 30 recommended lines of VBLANK overscan
    REPEND
    lda #0
    sta VBLANK                  ; turn off VBLANK
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Control input checks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckP0Up:
    lda #%00010000               ; p0 joystick up
    bit SWCHA
    bne CheckP0Down             ; if bit pattern is not same...
    ; more logic
    ;inc JetYPos
    lda CharYPos
    cmp #130
    bpl CheckP0Down
.P0UpPressed:
    inc CharYPos
    
CheckP0Down:
    lda #%00100000
    bit SWCHA
    bne CheckP0Left
    ; more logic
    ;dec JetYPos
    lda CharYPos
    cmp #15
    bmi CheckP0Left
.P0DownPressed:
    dec CharYPos
    
CheckP0Left:
    lda #%01000000
    bit SWCHA
    bne CheckP0Right
    ; more logic
    ;dec JetXPos
    lda CharXPos
    cmp #20
    bmi CheckP0Right
.P0LeftPressed:
    dec CharXPos
    
CheckP0Right:
    lda #%10000000
    bit SWCHA
    bne EndInputCheck
    ; more logic
    ;inc JetXPos
    lda CharXPos
    cmp #115
    bpl CheckButtonPressed
.P0RightPressed:
    inc CharXPos
    
CheckButtonPressed:
    lda #%10000000                  ; if button is pressed
    bit INPT4
    bne EndInputCheck
       
EndInputCheck:
    ; skip because nothing was pressed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Update the coordinates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop back to start a brand new frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    jmp StartFrame              ; continue to display the next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine section
;; Set object x position
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetObjectXPos subroutine
    sta WSYNC                   ; start new scanline
    sec                         ; set carry flag
.Div15Loop
    sbc #15                     ; A - 15
    bcs .Div15Loop             ; repeat until A-15 < 0, otherwise
    eor #7                      ; XOR operation to get between -8 to 7
    asl
    asl
    asl
    asl                         ; shift byte to the left x4
    sta HMP0,Y                 ; store fine offset to HMxx
    sta RESP0,Y                ; fix object in position
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sprite lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CharSprite:
    .byte #%00000000;$00
    .byte #%00111100;$FE
    .byte #%10100101;$FC
    .byte #%01011010;$FA
    .byte #%00111100;$F8
    .byte #%11111111;$42
    .byte #%11111111;$42
    .byte #%01111110;$40
    .byte #%00111100;$40

CharSpriteColor:
    .byte #$00;
    .byte #$FE;
    .byte #$FC;
    .byte #$FA;
    .byte #$F8;
    .byte #$42;
    .byte #$42;
    .byte #$40;
    .byte #$40;
    
TreePF:
    .byte #%00011000
    .byte #%00111100
    .byte #%01111110
    .byte #%11111111
    .byte #%00111100
    .byte #%00111100
    .byte #%00000000
    .byte #%00000000
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete ROM sixe with exactly 4kb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC                   ; move to $FFFC
    word Reset                  
    word Reset