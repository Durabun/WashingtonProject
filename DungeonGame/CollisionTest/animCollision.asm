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
RelicSpritePtr  word                ; Relic sprite pointer
RelicColorPtr   word                ; Relic color pointer
EnemySpritePtr  word                ; Enemy sprite pointer
EnemyColorPtr   word                ; Enemy color pointer
AnimOffset      byte                ; Relic animation offset
Random          byte                ; random variables
FloorColor      byte                ; floor color
WallColor       byte                ; wall color
TreeSeg         byte                ; tree segment counter
RoomLocation    byte 
RoomColor       byte   
Spawn           byte                ; spawn checker (0-no, 1-yes)
AnimCheck       byte                ; animation check
Relics          byte                 ; Relic score counter
EnemySpawn      byte                 ; SpawnFlag for enemy
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
    lda #35
    sta CharYPos                ; store y = 50
    lda #%11010100
    sta Random                  ; random variable = $D4
    lda #$50
    sta RoomLocation            ; load room location
    lda #1
    sta EnemyXPos
    sta EnemyYPos
    lda #0
    sta Spawn              ; enemy spawn flag (0 - not spawned)
    sta AnimOffset
    sta AnimCheck
    sta Relics
    sta EnemySpawn         ; enemy is not present while less than 7 relics are found

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load sprite and color pointers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #<CharSprite1
    sta CharSpritePtr            
    lda #>CharSprite1
    sta CharSpritePtr+1
    
    lda #<CharColor1
    sta CharColorPtr           
    lda #>CharColor1
    sta CharColorPtr+1
    
    lda #<Relic1Sprite
    sta RelicSpritePtr            
    lda #>Relic1Sprite
    sta RelicSpritePtr+1
    
    lda #<RelicSpriteColor
    sta RelicColorPtr           
    lda #>RelicSpriteColor
    sta RelicColorPtr+1
    
    lda #<EnemySprite1
    sta EnemySpritePtr
    lda #>EnemySprite1
    sta EnemySpritePtr+1
    
    lda #<EnemyColor1
    sta EnemyColorPtr
    lda #>EnemyColor1
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
;; Render the 96 visible scanlines of main game (2 line kernel)
;; It is a 2lk because I call WSYNC twice 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RoomGen:
    jsr SetFloorColor
    ldx #96
;    lda EnemySpawn
;    cmp #1
;    bne .GameLineLoop
    
.GameLineLoop:
    ; main game loop
.AreWeInsideCharSprite:
    txa                         ; transfer x to accumulator
    sec                         ; set carry flag   
    sbc CharYPos                ; subtract sprite Y-coord
    cmp #9                        ; are we inside the sprite height? We want to compare the literal value in here (9)
    bcc .DrawSpriteChar           ; if true, call draw routine
    lda #0                      ; else, set lookup index to zero
.DrawSpriteChar:
    clc
    adc AnimOffset
    tay                         ; load Y so we can work with pointer
    lda (CharSpritePtr),Y        ; load player0 bitmat data
    sta WSYNC                   ; wait for scanline
    sta GRP0                    ; set graphics for p0
    lda (CharColorPtr),Y         ; load player0 for color
    sta COLUP0                  ; set color of player 0
    
.IsSpawned:
    lda Spawn
    cmp #1
    bne .RenderRemaining
    lda #0
.AreWeInsideRelicSprite:
    txa                         ; transfer x to accumulator
    sec                         ; set carry flag   
    sbc EnemyYPos                ; subtract sprite Y-coord
    cmp #9                       ; are we inside the sprite height?
    bcc .DrawSpriteRelic           ; if true, call draw routine
    lda #0                      ; else, set lookup index to zero
.DrawSpriteRelic:
    clc
    adc AnimOffset
    tay                         ; load Y so we can work with pointer
    lda (RelicSpritePtr),Y        ; load player1 bitmat data
    sta WSYNC                   ; wait for scanline
    sta GRP1                    ; set graphics for p1
    lda (RelicColorPtr),Y         ; load player0 for color
    sta COLUP1                  ; set color of player1
.RenderRemaining:
    lda Spawn
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
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sprite coordinate checks
;; and room transition checks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
UpYCheck:
    lda CharYPos                   ; load Y coordinate into A
    cmp #65                       ; compare to upper Y bound
    bmi LowYCheck                  ; if it not beyond, go to lower Y check
.UpExit:                           ; otherwise, sprite has left from the top
    lda RoomLocation               ; load room location check
    clc                             ; clear carry flag for addition
    adc #$30                        ; add $30 to the room counter
    sta RoomLocation               ; Update RoomLocation counter
    lda #9                         ; load 
    sta CharYPos  
    ; Upon exiting the room, check if the enemy has spawned
    lda Spawn
    cmp #1
    bne .SpawnUpEnemy              ; if it is 0, then check to spawn`
    lda #0                          ; else, reset the spawn flag
    sta Spawn                 ; enemy de-spawns when entering a room if the flag was 1
.SpawnUpEnemy:                     ; Method to spawn the enemy sprite
    jsr SpawnCoords                ; Generate the enemy spawn coordinates
    jsr GenSpawn                   ; Generate the RNG value for the enemy
    cmp #%00001011                  ; Compare RNG to binary 11 (25% of spawn)
    bmi .UpEnd                     ; if the gen'd value is less, then end the spawn check
    lda #1                          ; else, load 1
    sta Spawn                 ; set spawn flag to 1 (which reveals the enemy)
.UpEnd:                            ; End of the up exit check
    
    
LowYCheck:
    lda CharYPos
    cmp #7
    bpl RightXCheck
.DownExit:
    lda RoomLocation
    sec
    sbc #$30
    sta RoomLocation
    lda #63
    sta CharYPos
    ;
    lda Spawn
    cmp #1
    bne .SpawnDownEnemy
    lda #0
    sta Spawn
.SpawnDownEnemy:
    jsr SpawnCoords
    jsr GenSpawn
    cmp #%00001011
    bmi .DownEnd
    lda #1
    sta Spawn
.DownEnd:

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
    jsr SpawnCoords
    ;
    lda Spawn
    cmp #1
    bne .SpawnRightEnemy
    lda #0
    sta Spawn
.SpawnRightEnemy:
    jsr SpawnCoords
    jsr GenSpawn
    cmp #%00001011
    bmi .RightEnd
    lda #1
    sta Spawn
.RightEnd:

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
    jsr SpawnCoords
    ;
    lda Spawn
    cmp #1
    bne .SpawnLeftEnemy
    lda #0
    sta Spawn
.SpawnLeftEnemy:
    jsr SpawnCoords
    jsr GenSpawn
    cmp #%00001011
    bmi .LeftEnd
    lda #1
    sta Spawn
.LeftEnd:

EndPosCheck:
; Do nothing here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Increment animation flag
;; and change offset every few frames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
AnimationFlag:
    lda AnimCheck               ; load the animation check flag
    cmp #20                      ; Compare flag to #20 (every 20 fps)   
    beq .UpdateAnim             ; if the flag is equal to #20:
    inc AnimCheck               ; else, increment the animation flag
    jmp StartFrame              ; jump to the beginning of the fram loop
.UpdateAnim:                    ; jump here to update the animation
    jsr Animation               ; subroutine to update the animation offset
    lda #0                       ; load 0 into A
    sta AnimCheck               ; reset the animation flag
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collision Checks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckCollisionP0Relic:          ; collision check between p0 and relics
    lda #%10000000               ; collision bit 7
    bit CXPPMM                  ; check CXPPMM bit with above pattern    
    bne .CollisionP0Relic          ; collision between p0 and p1
    jmp EndCollisionCheck     ; skip to bottom
.CollisionP0Relic:             ; if there is a collision
    inc Relics                  ; increment relic score
    lda #0                      ; load 0 into A
    sta Spawn              ; save to spawn flag (make the relic disappear)
    jmp EndCollisionCheck      ; jump to the end of the collision check
    
CheckCollisionP0P1:            ; collision between player and enemy
    lda #%10000000               ; collision bit 7
    bit CXPPMM                  ; check the bit
    bne .CollisionP0P1          ; if it is collided, jump to this method
    jmp EndCollisionCheck       ; otherwise, jump to the end
.CollisionP0P1:
    ; this would be a game over
    
EndCollisionCheck:
    sta CXCLR                   ; fallback
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check relic scores
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckRelics:                   ; check relic score
    lda Relics                 ; load score into A
    cmp #7                     ; is it equal to 7?
    beq .setMonster           ; if yes, go to setMonster method
.setMonster:                   
    lda #0                     ; load #0 into A
    sta Relics                 ; clear relic score
    lda #1                     ; load #1 into A
    sta EnemySpawn             ; set enemy flag to 1 (enemy appears!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop back to start a brand new frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    jmp StartFrame              ; continue to display the next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Begin subroutines
;; Check if enemy/relic has spawned
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Animation subroutine           ; animation offset subroutine (idle)
    lda AnimOffset              ; load the animation offset
    cmp #0                      ; if offset == 0:
    beq .Offset                 ; jump to Offset method
    lda #0                       ; else if the offset is #9, load #0
    sta AnimOffset              ; save to offset
    jmp .AnimEnd                ; then jump to the end
.Offset                         ; offset method
    lda #9                       ; load #9
    sta AnimOffset              ; save to the offset (so now it is #9)
    jmp .AnimEnd                ; jump to the end of the animation subroutine
.AnimEnd
    rts                          ; return
    
 

GenSpawn subroutine            ; subroutine to check the spawn status
    lda Random                  ; load RNG seed
    asl
    eor Random
    asl 
    eor Random
    asl
    asl
    eor Random
    asl 
    rol Random                  ; generates a new byte (between 0-255)
    and #%00001111              ; generate byte that fits between 0-15
    rts                         ; returm with y register    

SpawnCoords subroutine
    lda Random                  ; Do RNG routine for x coord
    asl
    eor Random
    asl
    eor Random
    asl
    asl
    eor Random
    asl 
    rol Random
    
    lsr
    lsr
    sta EnemyYPos                   ; save RNG to y coord
    lda #2
    clc
    adc EnemyYPos
    sta EnemyYPos
    
    lda Random
    asl
    eor Random
    asl
    eor Random
    asl
    asl
    eor Random
    asl
    rol Random
    
    lsr
    lsr
    sta EnemyXPos
    lda #15
    clc
    adc EnemyXPos
    sta EnemyXPos                   ; save RNG to x coord
    rts

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
CharSprite1
    .byte #%00000000;
    .byte #%00100100;$F0
    .byte #%01011010;$F2
    .byte #%00111100;$F2
    .byte #%00111100;$00
    .byte #%01100110;$00
    .byte #%00011000;$0E
    .byte #%01111110;$00
    .byte #%00111100;$00
CharSprite2
    .byte #%00000000;
    .byte #%00100100;$F0
    .byte #%00011000;$F2
    .byte #%01111110;$F2
    .byte #%00111100;$00
    .byte #%01100110;$00
    .byte #%00011000;$0E
    .byte #%01111110;$00
    .byte #%00111100;$00
CharColor1
    .byte #$00;
    .byte #$F0;
    .byte #$F2;
    .byte #$F2;
    .byte #$00;
    .byte #$00;
    .byte #$0E;
    .byte #$00;
    .byte #$00;
CharColor2
    .byte #$00;
    .byte #$F0;
    .byte #$F2;
    .byte #$F2;
    .byte #$00;
    .byte #$00;
    .byte #$0E;
    .byte #$00;
    .byte #$00;    
Relic1Sprite
    .byte #%00000000;
    .byte #%00001001;$40
    .byte #%01011100;$40
    .byte #%00001000;$42
    .byte #%01110111;$42
    .byte #%00100010;$42
    .byte #%00010101;$44
    .byte #%01010100;$44
    .byte #%00001000;$44
Relic2Sprite
    .byte #%00000000;
    .byte #%01001010;$40
    .byte #%00011100;$40
    .byte #%00001000;$42
    .byte #%01110111;$42
    .byte #%00100010;$42
    .byte #%00010100;$44
    .byte #%00010101;$44
    .byte #%00101000;$44       
RelicSpriteColor
        .byte #$00;
        .byte #$40;
        .byte #$40;
        .byte #$42;
        .byte #$42;
        .byte #$42;
        .byte #$46;
        .byte #$46;
        .byte #$46;
RelicSpriteColor2
        .byte #$00;
        .byte #$40;
        .byte #$40;
        .byte #$42;
        .byte #$42;
        .byte #$42;
        .byte #$46;
        .byte #$46;
        .byte #$46;
EnemySprite1
    .byte #%00000000;
    .byte #%00111000;--
    .byte #%01011100;--
    .byte #%10011101;--
    .byte #%00111110;--
    .byte #%10111110;--
    .byte #%00100010;--
    .byte #%01001001;--
    .byte #%00010100;--
EnemySprite2
    .byte #%00000000;
    .byte #%10111000;--
    .byte #%01011101;--
    .byte #%00011100;--
    .byte #%10011100;--
    .byte #%00111110;--
    .byte #%00100010;--
    .byte #%01001001;--
    .byte #%01010101;--
EnemyColor1
    .byte #$00;
    .byte #$08;
    .byte #$08;
    .byte #$0A;
    .byte #$0A;
    .byte #$0C;
    .byte #$0C;
    .byte #$0E;
    .byte #$0E;
EnemyColor2
    .byte #$00;
    .byte #$08;
    .byte #$08;
    .byte #$0A;
    .byte #$0A;
    .byte #$0C;
    .byte #$0C;
    .byte #$0E;
    .byte #$0E;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete ROM sixe with exactly 4kb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC                   ; move to $FFFC
    word Reset                  
    word Reset