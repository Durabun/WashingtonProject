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
    lda #70
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
    REPEAT 35
        sta WSYNC               ; Display 33 lines of VBLANK    
    REPEND
    ; since we are within the VSYNC/VBLANK region, set the x position of sprite(s)
    lda CharXPos               ; load character x position 
    ldy #0                      ; set it to the p0 mode
    jsr SetObjectXPos          ; set the object's x position
;    lda EnemyXPos              ; load enemy x position
;    ldy #1
;    jsr SetObjectXPos
    
    
    sta WSYNC
    sta HMOVE                    ; apply move  
    
    lda #0
    sta VBLANK
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Render the 192 visible scanlines of main game (due to 2 line kernel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RoomGen:
    jsr SetFloorColor
    ldx #192
.GameLoop:
    ; main game loop
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
.IsEnemySpawned:
    lda EnemySpawn
    cmp #0
    bne .AreWeInsideEnemy
.AreWeInsideEnemy:
    txa 
    sec
    sbc EnemyYPos
    cmp #9
    bcc .DrawEnemySprite
    lda #
.DrawEnemySprite:
    tay
    lda (EnemySpritePtr),Y
    sta WSYNC
    sta GRP1
    lda (EnemyColorPtr),Y
    sta COLUP1
;    
    dex                          ; x--
    bne .GameLoop               ; jump to game loop if still drawing lines

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
.P0UpPressed:
    inc CharYPos
    
CheckP0Down:
    lda #%00100000
    bit SWCHA
    bne CheckP0Left
.P0DownPressed:
    dec CharYPos
    
CheckP0Left:
    lda #%01000000
    bit SWCHA
    bne CheckP0Right
.P0LeftPressed:
    dec CharXPos
    
CheckP0Right:
    lda #%10000000
    bit SWCHA
    bne CheckButtonPressed
.P0RightPressed:
    inc CharXPos
    
CheckButtonPressed:
    lda #%10000000                  ; if button is pressed
    bit INPT4
    bne UpYCheck
        
 
UpYCheck:
    lda CharYPos                   ; load Y coordinate into A
    cmp #130                        ; compare to upper Y bound
    bmi LowYCheck                  ; if it not beyond, go to lower Y check
.UpExit:                           ; otherwise, sprite has left from the top
    lda RoomLocation               ; load room location check
    clc                             ; clear carry flag for addition
    adc #$30                        ; add $30 to the room counter
    sta RoomLocation               ; Update RoomLocation counter
    lda #18                         ; load 
    sta CharYPos
;    jsr CheckSpawn
;    tya
;    cmp #%00001101 
;    bpl .UpSetSpawn
;.UpSetSpawn:
;    lda #50
;    sta EnemyXPos
;    sta EnemyYPos
  
LowYCheck:
    lda CharYPos
    cmp #15
    bpl RightXCheck
.DownExit:
    lda RoomLocation
    sec
    sbc #$30
    sta RoomLocation
    lda #127
    sta CharYPos

RightXCheck:
    lda CharXPos
    cmp #115
    bmi LeftXCheck
.RightExit
    lda RoomLocation
    clc 
    adc #$10
    sta RoomLocation
    lda #23
    sta CharXPos

LeftXCheck:
    lda CharXPos
    cmp #20
    bpl EndPosCheck
.LeftExit:
    lda RoomLocation
    sec 
    sbc #$10
    sta RoomLocation
    lda #112
    sta CharXPos

EndPosCheck:
; Nothing to do here
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop back to start a brand new frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    jmp StartFrame              ; continue to display the next frame
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check if Enemy/relic has spawned in this room
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckSpawn subroutine
    lda Random
    asl
    eor Random
    asl 
    eor Random
    asl
    asl
    eor Random
    asl
    rol Random                  ; generates a new byte (between 0-255)
    
    ; lets take this byte and AND it with bit 15
    lda Random
    and #%00001111              ; generate byte that fits between 0-15
    tay
    rts
;    cmp #%00001101              ; compare it to byte of 13 (if rng is greater than 13)
;    bpl .SetSpawn
;    rts
;.SetSpawn
;    lda #1                      ; set spawn flag to 1
;    sta EnemySpawn             ; set EnemySpawn to 1 (an enemy has been spawned)
;    
;    lda Random
;    asl
;    eor Random
;    asl 
;    eor Random
;    asl
;    asl
;    eor Random
;    asl
;    rol Random                  ; generates a new byte (between 0-255)
;    
;    lsr                          ; divide by 2
 ;   lsr                          ; divide by 2
 ;   sta EnemyXPos
 ;;   lda #30     
 ;   clc 
;    adc EnemyXPos
;    sta EnemyXPos               ; set the random x coord
;    
;    lda Random
;    asl
;    eor Random
;    asl 
;    eor Random
;    asl
 ;   asl
 ;;   eor Random
 ;   asl
 ;   rol Random                  ; generates a new byte (between 0-255)
 ;   lsr                          ; divide by 2
 ;   sta EnemyYPos               ; set the random y coord
 ;   rts
    

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
        .byte #%00110110;$AE
        .byte #%00111110;$AE
        .byte #%01111110;$AE
        .byte #%10111111;$AE
        .byte #%10111001;$AE
        .byte #%01111001;$AE
        .byte #%00111111;$AE
        .byte #%00011100;$AE
        
EnemySpriteColor
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