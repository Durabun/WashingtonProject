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
EnemyXPos       byte                ; enemy/relic x position
EnemyYPos       byte                ; enemy/relic y position
Temp            byte                ; temporary register
CharSpritePtr   word                ; sprite pointer
CharColorPtr    word                ; color pointer
EnemySpritePtr  word                ; Enemy sprite pointer
EnemyColorPtr   word                ; Enemy color pointer
Random          byte                ; random variables
FloorColor      byte                ; floor color
WallColor       byte                ; wall color
TreeSeg         byte                ; tree segment counter
RoomLocation    byte 
RoomColor       byte   
EnemySpawn      byte                ; spawn checker (0-no, 1-yes)

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
    lda #67
    sta CharXPos                ; store x = 40
    lda #73
    sta CharYPos                ; store y = 50
    lda #%11010100
    sta Random                  ; random variable = $D4
    lda #$F9
    sta WallColor               ; load wall color
    lda #1
    sta TreeSeg                 ; load tree segment counter
    lda #$50
    sta RoomLocation            ; load room location
    lda #45
    sta EnemyXPos
    sta EnemyYPos
    lda #0
    sta EnemySpawn              ; enemy spawn flag (0 - not spawned)
    
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
    
    lda #<EnemySprite
    sta EnemySpritePtr            
    lda #>EnemySprite
    sta EnemySpritePtr+1
    
    lda #<EnemySpriteColor
    sta EnemyColorPtr           
    lda #>EnemySpriteColor
    sta EnemyColorPtr+1
    
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
    REPEAT 34
        sta WSYNC               ; Display remaining lines of VBLANK    
    REPEND
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations done during the VBLANK region
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    lda CharXPos
    ldy #0
    jsr SetObjectXPos           ; set player0 horizontal position
    
    lda EnemyXPos
    ldy #1
    jsr SetObjectXPos           ; set player1 x coord
    
    sta WSYNC
    sta HMOVE                    ; apply move    
    
    lda #0
    sta VBLANK                  ; Turn off VBLANK
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Render the 96 visible scanlines of main game (2 pixel kernel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; due to 2lk,dimensions of the vertical screen is different
; 0 - 96
RoomGen:
    jsr SetFloorColor
    ldx #96
.GameLineLoop:
    ; main game loop
.AreWeInsideJetSprite:
    txa                         ; transfer x to accumulator
    sec                         ; set carry flag   
    sbc CharYPos                ; subtract sprite Y-coord
    cmp #9                        ; are we inside the sprite height? We want to compare the literal value in here (9)
    bcc .DrawSpriteP0           ; if true, call draw routine
    lda #0                      ; else, set lookup index to zero
.DrawSpriteP0:
    tay                         ; load Y so we can work with pointer
    lda (CharSpritePtr),Y        ; load player0 bitmat data
    sta WSYNC                   ; wait for scanline
    sta GRP0                    ; set graphics for p0
    lda (CharColorPtr),Y         ; load player0 for color
    sta COLUP0                  ; set color of player 0
    
    
.IsSpawned:
    lda EnemySpawn
    cmp #1
    bne .RenderRemaining
    lda #0
.AreWeInsideBomberSprite:
    txa                         ; transfer x to accumulator
    sec                         ; set carry flag   
    sbc EnemyYPos                ; subtract sprite Y-coord
    cmp #9                       ; are we inside the sprite height?
    bcc .DrawSpriteP1           ; if true, call draw routine
    lda #0                      ; else, set lookup index to zero
.DrawSpriteP1:
    tay                         ; load Y so we can work with pointer
    lda (EnemySpritePtr),Y        ; load player1 bitmat data
    sta WSYNC                   ; wait for scanline
    sta GRP1                    ; set graphics for p1
    lda (EnemyColorPtr),Y         ; load player0 for color
    sta COLUP1                  ; set color of player1
.RenderRemaining:
    lda EnemySpawn
    cmp #1
    beq .EndRender
    sta WSYNC
.EndRender:

    dex                         ; X--
    bne .GameLineLoop          ; repeat until 192 lines are done

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
;; Loop back to start a brand new frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    jmp StartFrame              ; continue to display the next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Begin subroutines
;; Set object x position
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; Calculate room floor color
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetFloorColor subroutine
    lda RoomLocation            ; load the current room counter
    clc                          ; clear carry flag
    adc #$04                    ;  add current room counter with $04
    sta COLUBK                  ; store into the BG color
    rts                         ;  return from subroutine
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to waste cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jsr takes 6 cycles
;; rts takes 6 cycles
;; tada! waste 12 cycles!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Sleep12Cycles subroutine
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
      
EnemySprite
        .byte #%00000000
        .byte #%00110110;$AE
        .byte #%00111110;$AE
        .byte #%01111110;$AE
        .byte #%10111111;$AE
        .byte #%10111001;$AE
        .byte #%01111001;$AE
        .byte #%00111111;$AE
        .byte #%00011100;$AE
        
EnemySpriteColor
        .byte #$00;
        .byte #$7C;
        .byte #$7C;
        .byte #$8C;
        .byte #$8C;
        .byte #$9C;
        .byte #$9C;
        .byte #$AE;
        .byte #$AE;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete ROM sixe with exactly 4kb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC                   ; move to $FFFC
    word Reset                  
    word Reset