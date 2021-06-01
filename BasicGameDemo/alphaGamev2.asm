;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Alpha build of project Washington
;; Programmed by Jon Eugenio
;; v0.1
;; 05/26/2021
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    
BGColor         byte                ; register containing BG color address
CharXPos        byte                ; character x position
CharYPos        byte                ; character y position
EnemyXPos       byte                ; enemy/relic x position
EnemyYPos       byte                ; enemy/relic y position
CharSpritePtr   word                ; sprite pointer
CharColorPtr    word                ; color pointer
RelicSpritePtr  word                ; Relic sprite pointer
RelicColorPtr   word                ; Relic color pointer
EnemySpritePtr  word                ; Enemy sprite pointer
EnemyColorPtr   word                ; Enemy color pointer
AnimOffset      byte                ; Relic animation offset
Random          byte                ; random variables
RoomLocation    byte 
RoomColor       byte   
Spawn           byte                ; spawn checker (0-no, 1-yes), for both relics and ghosts
AnimCheck       byte                ; animation check
Relics          byte                ; Relic score counter
Power           byte                ; power meter
EnemySpawn      byte                ; SpawnFlag for ghost SPECIFICALLY
Score           byte                ; Grand score for the entire game
gameOver        byte                ; Game over flag 
exit            byte                ; Exit flag  
statusCol       byte                ; color of status screen
iconXPos        byte                ; x position of the icon status
scoreXPos       byte                ; x position of the relic score
statusOffset    byte                ; offset for the status bar info
ChaseCounter    byte                ; tracks how many rooms before chase ends  
DiffOffset      byte                ; ghost gets faster the longer the game goes on  
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
    lda #10
    sta iconXPos                 ;       store the x coordinate of the icon
    lda #115                        
    sta scoreXPos                ;       store the x coordinate of the round score
    lda #67                       ; (2) 
    sta CharXPos                 ; (3)   store as char x coord             
    lda #25                       ; (2)
    sta CharYPos                 ; (3)   store as char y coord    
    lda #%11010100                ; (2)
    sta Random                   ; (3)   store as random byte 
    lda #$50                     ; (2) 
    sta RoomLocation            ; (3)    store as starting room location
    lda #1                       ; (2)    
    sta EnemyXPos               ; (3)    store as enemy x coord
    sta EnemyYPos               ; (3)    store as enemy y coord
    sta DiffOffset              ;
    lda #0                       ; (2)   
    sta Spawn                   ; (3)    set spawn flag
    sta AnimOffset              ; (3)    set animation offset flag
    sta AnimCheck               ; (3)    set animation check flag
    sta Relics                  ; (3)    set relic score to 0
    sta Power                   ; (3)    set power meter to 0
    sta EnemySpawn              ; (3)    set enemy spawn flag  
    sta Score                   ; (3)    set total score to 0  
    sta gameOver                ; (3)    set game over flag to 0 (not game over)
    sta exit                    ; (3)    set exit door flag
    sta statusOffset           ; (3)    offset for the status bar
    sta ChaseCounter           ; (3)    counter to determine chase end
    lda #$0F
    sta statusCol              ; (3)    color of the status bar
    ; this entire initialization takes a total of
    ; 12 + 36 = 48 clock (machine) cycles

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load sprite and color pointers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #<CharSprite1           ; (2) load lo-bit
    sta CharSpritePtr           ; (3) store in pointer    
    lda #>CharSprite1           ; (2) load hi-bit
    sta CharSpritePtr+1         ; (3) store in pointer
    
    lda #<CharColor1            ; (2) load lo-bit for color
    sta CharColorPtr            ; (3) store in pointer
    lda #>CharColor1            ; (2) load hi-bit
    sta CharColorPtr+1          ; (3) store in pointer
    
    lda #<Relic1Sprite          ; (2)
    sta RelicSpritePtr          ; (3)        
    lda #>Relic1Sprite          ; (2)
    sta RelicSpritePtr+1        ; (3)
    
    lda #<RelicSpriteColor      ; (2)
    sta RelicColorPtr           ; (3)    
    lda #>RelicSpriteColor      ; (2)
    sta RelicColorPtr+1         ; (3)
    
    lda #<EnemySprite1          ; (2)
    sta EnemySpritePtr          ; (3)
    lda #>EnemySprite1          ; (2)
    sta EnemySpritePtr+1        ; (3)    
    
    lda #<EnemyColor1           ; (2)
    sta EnemyColorPtr           ; (3)
    lda #>EnemyColor1           ; (2)
    sta EnemyColorPtr+1         ; (3)
    ; just loading all of the sprites and colors into pointers
    ; this takes 60 clock cycles

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start main display loop and frame rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display VSYNC and VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2                       ; (2)
    sta VBLANK                  ; (3) turn on VBLANK
    sta VSYNC                   ; (3) turn on VSYNC
    REPEAT 3
        sta WSYNC               ; (3*3) Display 3 lines of VSYNC
    REPEND
    lda #0                      ; (2)
    sta VSYNC                   ; (3) Turn off VSYNC
    REPEAT 32
        sta WSYNC               ; (3*34) Display remaining lines of VBLANK    
    REPEND
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations done during the VBLANK region
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;    lda CharXPos                ; (4) load char x coord into A
    lda scoreXPos               ; load the score sprite x coord
    ldy #0                       ; (3) load 0 into y
    jsr SetObjectXPos           ; (6) set player0 horizontal position
    ; the subroutine takes at least 31 cycles
    ; at least 42 cycles, 33 WSYNC lines
;    lda EnemyXPos
    lda iconXPos                ; load the icon sprite x coord
    ldy #1
    jsr SetObjectXPos           ; set player1 x coord
    ; 42 machine cycles, 34 WSYNC lines
    ; at least 84 cycles (depending where the sprite is)
    sta WSYNC                    ; (3) 35 WSYNC lines
    sta HMOVE                    ; (3) apply move    
    sta WSYNC                    ; (3) 36 WSYNC lines
    lda #0                       ; (2)
    sta VBLANK                  ; (3) Turn off VBLANK
    ; takes up 144 machine cycles
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Render the 96 visible scanlines of main game (2 line kernel)
;; It is a 2lk because I call WSYNC twice 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StatusRender:
; top of status bar
    lda #$61
    sta COLUBK
    sta WSYNC ;(1)
    lda #$63
    sta COLUBK
    sta WSYNC ;(2)
    lda #$65
    sta COLUBK
    sta WSYNC ;(3)
    lda #$67
    sta COLUBK
    sta WSYNC ;(4)
    lda #$69
    sta COLUBK
    sta WSYNC ;(5)
    lda #$6B
    sta COLUBK
    sta WSYNC ;(6)
    lda #$6D
    sta COLUBK
    sta WSYNC ;(7)
    lda #$6F
    sta COLUBK
    sta WSYNC ;(8)
    sta COLUBK
    sta WSYNC ;(9)
    sta WSYNC ;(10)
; render the middle of the status bar
    sta WSYNC ;(11)
    lda #9
    clc
    adc statusOffset
    tay
    lda Digits,Y
    sta GRP0
    lda Symbols,Y
    sta GRP1
    sta WSYNC ; (12)
    lda #8
    clc
    adc statusOffset
    tay
    lda Digits,Y
    sta GRP0
    lda Symbols,Y
    sta GRP1
    sta WSYNC ;(13)
    lda #7
    clc
    adc statusOffset
    tay
    lda Digits,Y
    sta GRP0
    lda Symbols,Y
    sta GRP1
    sta WSYNC ;(14)
    lda #6
    clc
    adc statusOffset
    tay
    lda Digits,Y
    sta GRP0
    lda Symbols,Y
    sta GRP1
    sta WSYNC ;(15)
    lda #5
    clc
    adc statusOffset
    tay
    lda Digits,Y
    sta GRP0
    lda Symbols,Y
    sta GRP1
    sta WSYNC ;(16)
    lda #4
    clc
    adc statusOffset
    tay
    lda Digits,Y
    sta GRP0
    lda Symbols,Y
    sta GRP1
    sta WSYNC ;(15)
    lda #3
    clc
    adc statusOffset
    tay
    lda Digits,Y
    sta GRP0
    lda Symbols,Y
    sta GRP1
    sta WSYNC ;(16)
    lda #2
    clc
    adc statusOffset
    tay
    lda Digits,Y
    sta GRP0
    lda Symbols,Y
    sta GRP1
    sta WSYNC ;(17)
    lda #1
    clc
    adc statusOffset
    tay
    lda Digits,Y
    sta GRP0
    lda Symbols,Y
    sta GRP1
    sta WSYNC ;(18)
    lda #0
    clc
    adc statusOffset
    tay
    lda Digits,Y
    sta GRP0
    lda Symbols,Y
    sta GRP1
    sta WSYNC ;(19)
    sta WSYNC ;(20)
    ; Reset the coordinates of the players here
    lda CharXPos                ; (4) load char x coord into A
    ldy #0                       ; (3) load 0 into y
    jsr SetObjectXPos           ; (6) set player0 horizontal position
    sta WSYNC ; (22)
    lda EnemyXPos
    ldy #1
    jsr SetObjectXPos           ; set player1 x coord
    sta WSYNC ; (24)
    ; 42 machine cycles, 34 WSYNC lines
    ; at least 84 cycles (depending where the sprite is)
    sta WSYNC ;(25)                   ; (3) 35 WSYNC lines
    sta HMOVE                    ; (3) apply move    
    sta WSYNC ;(26)                   ; (3) 36 WSYNC lines
    
    lda #$6F
    sta COLUBK
    sta WSYNC ;(27)
    lda #$6D
    sta COLUBK
    sta WSYNC ;(28)
    lda #$6B
    sta COLUBK
    sta WSYNC ;(29)
    lda #$69
    sta COLUBK
    sta WSYNC ;(30)
    lda #$60
    sta COLUBK
    sta WSYNC ;(31)
    sta WSYNC ;(32)

RoomGen:
    jsr SetFloorColor           ; (6+15) call setFloorColor method 
    sta COLUBK
    ldx #80                      ; (2) load the 96th line into X
.MainGameLoop:                  ; main game loop
    lda gameOver                ; (4) load game over flag
    cmp #0                       ; (2) compare to #1
    bne .renderGameOver         ; render the game over screen if flag == 1
    ; otherwise continue
    lda EnemySpawn              ; (4) load EnemySpawn flag
    cmp #0                       ; (2) compare to #1
    beq .RelicLoop              ; (2+1) if flag == 1, go to the ghost loop (ghost is spawned)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.GhostLoop:                     ; if the ghost is present, this is the mode
    jsr AreWeInsideChar        ; (6+46 max) cycles, with 14 cycles going into the next line
.IsGhostSpawned:               ; method to see if the ghost is spawned
    lda Spawn                   ; (4) load Spawn flag
    cmp #1                      ; (2) compare to #1
    bne .GhostRenderRemaining ; (2+1) if flag is != 1, render remaining lines
    lda #0                      ; (2) otherwise proceed with rendering ghost
.AreWeInsideGhostSprite:      
    txa                         ; (2) transfer x to accumulator
    sec                         ; (2) set carry flag   
    sbc EnemyYPos              ; (4) subtract sprite Y-coord
    cmp #9                      ; (2) are we inside the sprite height?
    bcc .DrawSpriteGhost       ; (2+1) if true, call draw routine
    lda #0                      ; (2) else, set lookup index to zero
.DrawSpriteGhost:
    clc                         ; (2) clear carry flag
    adc AnimOffset             ; (4) add A to animation offset
    tay                         ; (2) load Y so we can work with pointer
    lda (EnemySpritePtr),Y    ; (5?) load player1 bitmat data
    sta WSYNC                  ; (3) wait for scanline
    sta GRP1                   ; (3) set graphics for p1
    lda (EnemyColorPtr),Y     ; (5?) load player0 for color
    sta COLUP1                 ; (3) set color of player1
    jmp .EndRender
.GhostRenderRemaining:        ; if there isnt something spawned,
    sta WSYNC                  ; (3) draw line
    jmp .EndRender             ; (3) jump to endRender anyway
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; Game loop for relic mode   
.RelicLoop: 
    lda #0
    sta Power
    jsr AreWeInsideChar        ; (6+46) subroutine for drawing the character sprite
.IsSpawned:                     ; check if relic has spawned
    lda Spawn                   ; (4) load Spawn flag
    cmp #1                      ; (2) is spawn == 1? (is relic spawned?)
    bne .RenderRemaining       ; (2+1) if no, then render remaining lines
    lda #0                      ; (3) else load 0 in A and proceed to render relic
.AreWeInsideRelicSprite:
    txa                         ; (2) transfer x to accumulator
    sec                         ; (2) set carry flag   
    sbc EnemyYPos              ; (4) subtract sprite Y-coord
    cmp #9                      ; (2) are we inside the sprite height?
    bcc .DrawSpriteRelic       ; (2+1) if true, call draw routine
    lda #0                      ; (2) else, set lookup index to zero
.DrawSpriteRelic:               
    clc                         ; (2)
    adc AnimOffset             ; (4) add A to animation offset
    tay                         ; (2) load Y so we can work with pointer
    lda (RelicSpritePtr),Y     ; (5?) load player1 bitmat data
    sta WSYNC                   ; (3)wait for scanline
    sta GRP1                    ; (3) set graphics for p1
    lda (RelicColorPtr),Y      ; (5?) load player0 for color
    sta COLUP1                  ; (3) set color of player1
    jmp .EndRender
.RenderRemaining:              ; if there is no entity, render remaining lines
    sta WSYNC                   ; (3)
    jmp .EndRender
.renderGameOver:               ; render the game over screen if the game over flag is active
    lda #0
    sta statusOffset
    sta Relics
    lda #$FF                    ; (2) load a game over color 
    sta COLUBK                  ; (3) set as the BG color
    sta WSYNC                   ; (3) draw scanline
    sta WSYNC                   ; (3) draw scanline
.EndRender:
    dex                         ; (2) X--
    bne .MainGameLoop          ; (2+1) repeat until 96 lines are done
    sta WSYNC                   ; (3) we draw one less scanline in the main loop
    ; and then draw it here 
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display Overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2                       ; (2)
    sta VBLANK                  ; (3) turn VBLANK on again
    REPEAT 30
        sta WSYNC               ; (3*30) display 30 recommended lines of VBLANK overscan
    REPEND
    lda #0                      ; (2)
    sta VBLANK                 ; (3) turn off VBLANK
    ; takes 103 machine cycles

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Control input checks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckP0Up:
    lda #%00010000               ; p0 joystick up
    bit SWCHA
    bne CheckP0Down             ; if bit pattern is not same...
.P0UpPressed:
    lda gameOver
    cmp #1
    beq CheckP0Down
    inc CharYPos
    
CheckP0Down:
    lda #%00100000
    bit SWCHA
    bne CheckP0Left
.P0DownPressed:
    lda gameOver
    cmp #1
    beq CheckP0Left
    dec CharYPos
    
CheckP0Left:
    lda #%01000000
    bit SWCHA
    bne CheckP0Right
.P0LeftPressed:
    lda gameOver
    cmp #1
    beq CheckP0Right
    dec CharXPos
    
CheckP0Right:
    lda #%10000000
    bit SWCHA
    bne CheckButtonPressed
.P0RightPressed:
    lda gameOver
    cmp #1
    beq CheckButtonPressed
    inc CharXPos
    
CheckButtonPressed:
    lda #%10000000                  ; if button is pressed
    bit INPT4
    bne UpYCheck
.P0ButtonPressed:
    lda gameOver
    cmp #1
    beq .ResetGame
    lda EnemySpawn
    cmp #0
    beq .EndButton
    lda Spawn
    cmp #0
    beq .EndButton
    lda Power
    cmp #0
    beq .EndButton
    jsr PowerPush
    jsr PowerPush
    jsr PowerPush
    jsr PowerPush
    jsr PowerPush
    dec Power
    jmp .EndButton
.ResetGame:
    lda #0
    sta gameOver
.EndButton:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sprite coordinate checks
;; and room transition checks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
UpYCheck:
    lda CharYPos                   ; load Y coordinate into A
    cmp #72                       ; compare to upper Y bound
    bmi LowYCheck                  ; if it not beyond, go to lower Y check
.UpExit:                           ; otherwise, sprite has left from the top
    lda RoomLocation               ; load room location check
    clc                             ; clear carry flag for addition
    adc #$30                        ; add $30 to the room counter
    sta RoomLocation               ; Update RoomLocation counter
    lda #3                         ; load 
    sta CharYPos 
    
    ; Upon exiting the room, check if the enemy has spawned
    lda Spawn
    cmp #1
    bne .SpawnUpEnemy              ; if it is 0, then check to spawn`
    lda #0                          ; else, reset the spawn flag
    sta Spawn                       ; enemy de-spawns when entering a room if the flag was 1
    sta exit
    
.SpawnUpEnemy:                     ; Method to spawn the enemy sprite
    jsr SpawnCoords                ; Generate the enemy spawn coordinates
    jsr GenSpawn                   ; Generate the RNG value for the enemy
    cmp #%00001011                  ; Compare RNG to binary 11 (25% of spawn)
    bmi .UpEnd                     ; if the gen'd value is less, then end the spawn check
    ;
    lda #1                          ; else, load 1
    sta Spawn                 ; set spawn flag to 1 (which reveals the enemy)
.UpEnd:                            ; End of the up exit check
    lda EnemySpawn                ; load ghost flag
    cmp #1                         ; check if the ghost is spawned    
    bne LowYCheck                 ; if not, go to the next check
    inc ChaseCounter              ; otherwise, increment the chase counter
    
    
LowYCheck:
    lda CharYPos
    cmp #1
    bpl RightXCheck
.DownExit:
    lda RoomLocation
    sec
    sbc #$30
    sta RoomLocation
    lda #70
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
    lda EnemySpawn
    cmp #1
    bne RightXCheck
    inc ChaseCounter

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
    lda EnemySpawn
    cmp #1
    bne LeftXCheck
    inc ChaseCounter

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
    lda EnemySpawn
    cmp #1
    bne EndPosCheck
    inc ChaseCounter

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
CheckCollision:                 ; check the collision state
    lda EnemySpawn              ; load enemy spawn flag
    cmp #1                       ; check if flag == 1
    bne .CheckCollisionRelic    ; jump to the relic collision check
    lda exit
    cmp #1
    bne .CheckCollisionGhost
.CheckCollisionDoor:
    lda #%10000000
    bit CXPPMM
    bne .CollisionDoor
    jmp .EndCollisionCheck
.CollisionDoor:
    lda #0
    sta Spawn
    sta EnemySpawn
    jmp StartFrame
.CheckCollisionGhost:          ; else check the collision with the ghost
    lda #%10000000              ; collision bit 7
    bit CXPPMM                  ;check CXPPMM with above bit
    bne .CollisionGhost         ; collision happens with ghost
    jmp .EndCollisionCheck      ; else end the collision check
.CollisionGhost:                ; otherwise, update the game with the ghost collision
    ; game over goes here         ; the game should end
    jsr GameOver
    lda #1
    sta gameOver
    jmp StartFrame
.CheckCollisionRelic:           ; relic collision check
    lda #%10000000                ; load bit
    bit CXPPMM                    ; compare
    bne .CollisionRelic          ; if collision happens, jump here
    jmp .EndCollisionCheck       ; otherwise end the collision check
.CollisionRelic:                 ; relic collision update
    inc Relics                    ; inc relic score
    inc Score
    inc Power
    lda #0                        ; load 0 into A
    sta Spawn                     ; store 0 into Spawn flag (shut it off)
    lda statusOffset
    clc
    adc #9
    sta statusOffset
    
    ;jmp .EndCollisionCheck       ; then end the collision check
.EndCollisionCheck:
    sta CXCLR                     ; fallback
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check relic scores
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckRelics:                   ; check relic score
    lda Relics                 ; load score into A
    cmp #7                     ; is it equal to 7?
    bne .EndRelicCheck       ; if no, then end the relic score check
.setMonster:                  ; once enough relics have been spawned, the monster appears        
    lda #0                     ; load #0 into A
    sta Relics                 ; clear relic score
    lda #7                     ; load 7 into A
    sta Power                  ; save to Power (the character has 7 power due to the relics)
    lda #1                     ; load #1 into A
    sta EnemySpawn             ; set enemy flag to 1 (enemy appears!)
.EndRelicCheck:
    ; pass
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Update ghost check
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
UpdateGhost:
    lda EnemySpawn              ; Load enemy spawn flag
    cmp #1                       ; compare to 1
    bne .EndGhostUpdate         ; if spawn =! 1 jump to end (enemy is not spawned)
    lda Spawn                    ; otherwise, check if ghost is rendered
    cmp #1                       ; compare to 1 (is ghost active AND rendered?)
    bne .EndGhostUpdate         ; if not, jump to the end
    REPEAT 2                    ; repeat it 7 times (to make the ghost faster)
        jsr GhostChase          ; otherwise call GhostChase subroutine
    REPEND                          
.EndGhostUpdate                 ; end ghost update

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game state check
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StateCheck:
    ; include something here to change speed of the ghost
.ChaseCheck:
    lda ChaseCounter            ; load the chase counter
    cmp #33                      ; compare it to 33
    bpl .ResetState             ; if it is greater, go to reset the game state
    jmp .EndStateCheck          ; otherwise, end the state check
.ResetState
    inc Score
    lda #0                       ; load 0 into A
    sta EnemySpawn              ; reset the ghost spawn
    sta Spawn                   ; reset the relic spawn
    sta ChaseCounter            ; reset the chase counter
    sta Relics                  ; (3)    set relic score to 0
    sta Power                   ; (3)    set power meter to 0 
    sta statusOffset           ; reset status offset
.EndStateCheck                 ; end the check
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop back to start a brand new frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    jmp StartFrame             ; (3) jump to start of frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Begin subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set object x position
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetObjectXPos subroutine
    sta WSYNC                   ; (3) start new scanline
    sec                         ; (2) set carry flag
.Div15Loop
    sbc #15                     ; (2) A - 15
    bcs .Div15Loop             ; (2 + 1) repeat until A-15 < 0, otherwise
    eor #7                      ; (2) XOR operation to get between -8 to 7
    asl                         ; (2)
    asl                         ; (2)
    asl                         ; (2)
    asl                         ; (2) shift byte to the left x4
    sta HMP0,Y                 ; (5) store fine offset to HMxx
    sta RESP0,Y                ; (5) fix object in position
    rts                         ; (6)
    ; this subroutine to set the x coordinate of the object
    ; takes 5 + (5 per loop) + 26 clock cycles
    ; subroutine prints 1 WSYNC line per call

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculate room floor color
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetFloorColor subroutine
    lda RoomLocation            ; (4) load the current room counter
    clc                          ; (2) clear carry flag
    adc #$08                     ; (2) add current room counter with $04
;    sta COLUBK                  ; (3) store into the BG color
    rts                          ; (6) return from subroutine
    ; subroutine adds a constant to the current room color code
    ; this takes 17 cycles to complete

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Is the current scanline within the sprite?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
AreWeInsideChar subroutine    ; subroutine for rendering character sprite
    txa                         ; (2) transfer x to accumulator
    sec                         ; (2) set carry flag   
    sbc CharYPos               ; (4) subtract sprite Y-coord
    cmp #9                      ; (2) are we inside the sprite height? We want to compare the literal value in here (9)
    bcc .DrawSpriteChar        ; (2+1) if true, call draw routine
    lda #0                      ; (2) else, set lookup index to zero
.DrawSpriteChar                ; branch here if the scanline is within the sprite
    clc                         ; (2) clear carry flag
    adc AnimOffset             ; (4) add the animation offset
    tay                         ; (2) load Y so we can work with pointer
    lda (CharSpritePtr),Y      ; (5?) load player0 bitmat data
    sta WSYNC                   ; (3) wait for scanline
    sta GRP0                    ; (3) set graphics for p0
    lda (CharColorPtr),Y       ; (5?) load player0 for color
    sta COLUP0                  ; (3) set color of player 0
    rts                         ; (6)
    ; (20) if the sprite is not present
    ; (46) if it branches because the sprite is present
    ; note that a line is drawn before the full 44 scanlines
    ; after drawing, (17) cycles are spent

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Animation offset subroutine
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
    rts   
 
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
    lda Random                  ; Do RNG routine for y coord
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
    lda #10
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
;; Update ghost position (chase the player)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GhostChase subroutine          ; subroutine to update ghost position
; assuming this is invoked only when ghost is rendered
.UpdateXCoord
    lda EnemyXPos               ; load enemy x position
    cmp CharXPos                ; compare to player x position
    bpl .decX                   ; enemyX > playerX?
.incX                           ; if no, increment enemy X
    inc EnemyXPos               ; enemy x ++
    jmp .UpdateYCoord          ; jump to UpdateYCoord
.decX                           
    dec EnemyXPos               ; EnemyX --
.UpdateYCoord                   
    lda EnemyYPos               ; load enemy y position
    cmp CharYPos                ; compare to player y position
    bpl .decY                   ; if enemyY > playerY, decrement it
.incY                           ; else, increment y
    inc EnemyYPos   
    jmp .EndChase               ; jump to end the chase
.decY
    dec EnemyYPos               ; enemy y --
.EndChase:
    rts                          ; return

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Power push activated when button is pressed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PowerPush subroutine
; Exactly the same as Ghost Chase but in the opposite direction
.UpdateXCoord
    lda EnemyXPos               ; load enemy x position
    cmp CharXPos                ; compare to player x position
    bpl .incX                   ; enemyX > playerX?
.decX                           ; if no, decrement enemy X
    dec EnemyXPos               ; enemy x --
    jmp .UpdateYCoord          ; jump to UpdateYCoord
.incX                           
    inc EnemyXPos               ; EnemyX ++
.UpdateYCoord                   
    lda EnemyYPos               ; load enemy y position
    cmp CharYPos                ; compare to player y position
    bpl .incY                   ; if enemyY > playerY, increment it
.decY                           ; else, decrement y
    dec EnemyYPos   
    jmp .EndPush               ; jump to end the chase
.incY
    inc EnemyYPos               ; enemy y --
.EndPush:
    rts                          ; return
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Over
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
GameOver subroutine
    ; reset EVERYTHING
    lda #67                       ; (2) 
    sta CharXPos                 ; (3)   store as char x coord             
    lda #35                       ; (2)
    sta CharYPos                 ; (3)   store as char y coord    
    lda #$50                     ; (2) 
    sta RoomLocation            ; (3)    store as starting room location
    lda #1                       ; (2)    
    sta EnemyXPos               ; (3)    store as enemy x coord
    sta EnemyYPos               ; (3)    store as enemy y coord
    lda #0                       ; (2)   
    sta Spawn                   ; (3)    set spawn flag
    sta AnimOffset              ; (3)    set animation offset flag
    sta AnimCheck               ; (3)    set animation check flag
    sta Relics                  ; (3)    set relic score to 0
    sta Power                   ; (3)    set power meter to 0
    sta EnemySpawn              ; (3)    set enemy spawn flag  
    sta Score                   ; (3)    set total score to 0 
    sta statusOffset
    sta ChaseCounter
    rts

DrawDoor subroutine
    tay 
    lda ExitDoor,Y
    sta WSYNC
    sta GRP1
    lda DoorColor,Y
    sta COLUP1
    rts
    
    
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
Symbols
    ;empty
    .byte #%00000000;
    .byte #%11111111;$F4
    .byte #%10000001;$F6
    .byte #%10000001;$F8
    .byte #%10000001;$FA
    .byte #%10000001;$FC
    .byte #%10000001;$FE
    .byte #%10000001;$FA
    .byte #%11111111;$F6
    ;LeftHand
    .byte #%00000000;
    .byte #%00011100;$FA
    .byte #%00111100;$FC
    .byte #%01111100;$FE
    .byte #%10111110;$FC
    .byte #%10111111;$FA
    .byte #%00101010;$F8
    .byte #%00010101;$FA
    .byte #%00001010;$FE
    ;Ankh
    .byte #%00000000;
    .byte #%00111100;$FA
    .byte #%00011000;$FC
    .byte #%10011001;$FE
    .byte #%11111111;$FC
    .byte #%10011001;$FA
    .byte #%00100100;$F8
    .byte #%00100100;$FA
    .byte #%00011000;$FE
    ;Goat
    .byte #%00000000;
    .byte #%00011000;$FA
    .byte #%00011000;$FC
    .byte #%10111101;$FE
    .byte #%01011010;$FC
    .byte #%00111100;$FA
    .byte #%00011000;$F8
    .byte #%00100100;$FA
    .byte #%00100100;$FE
    ;CrackedBell
    .byte #%00000000;
    .byte #%00011000;$FA
    .byte #%11110111;$FC
    .byte #%01111010;$FE
    .byte #%01110110;$FC
    .byte #%00101100;$FA
    .byte #%00110100;$F8
    .byte #%00011000;$FA
    .byte #%00011000;$FE
    ;Skull
    .byte #%00000000;
    .byte #%00101010;$FA
    .byte #%00101010;$FC
    .byte #%00111110;$FE
    .byte #%01111111;$FC
    .byte #%11101011;$FA
    .byte #%11001001;$F8
    .byte #%01111110;$FA
    .byte #%00111100;$FE
    ;Oroboros
    .byte #%00000000;
    .byte #%00111110;$FA
    .byte #%01000110;$FC
    .byte #%01000011;$FE
    .byte #%01010011;$FC
    .byte #%00010011;$FA
    .byte #%11110011;$F8
    .byte #%01011110;$FA
    .byte #%00111100;$FE
    ;RightHand
    .byte #%00000000;
    .byte #%00111000;$FA
    .byte #%00111100;$FC
    .byte #%00111110;$FE
    .byte #%01111101;$FC
    .byte #%11111101;$FA
    .byte #%01010100;$F8
    .byte #%10101000;$FA
    .byte #%01010000;$FE

ExitDoor
    .byte #%00000000;
    .byte #%01110111;$FA
    .byte #%01110111;$FC
    .byte #%01010101;$FE
    .byte #%01010101;$FC
    .byte #%01110111;$FA
    .byte #%01110111;$F8
    .byte #%00110110;$FA
    .byte #%01010101;$FE
    
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
SymbolColors
    .byte #$00;
    .byte #$FA;
    .byte #$FC;
    .byte #$FE;
    .byte #$FC;
    .byte #$FA;
    .byte #$F8;
    .byte #$FA;
    .byte #$FE;
DoorColor
    .byte #$00;
    .byte #$60;
    .byte #$62;
    .byte #$64;
    .byte #$66;
    .byte #$66;
    .byte #$68;
    .byte #$68;
    .byte #$6A;

Digits
    ; 0
    .byte #%00000000;
    .byte #%00110000;$F4
    .byte #%01001000;$F6
    .byte #%01000100;$F8
    .byte #%01000100;$FA
    .byte #%01000100;$FC
    .byte #%01000100;$FE
    .byte #%00100100;$FA
    .byte #%00011000;$F6
    ; 1
    .byte #%00000000;
    .byte #%01111000;$F4
    .byte #%00110000;$F6
    .byte #%00110000;$F8
    .byte #%00011000;$FA
    .byte #%00011000;$FC
    .byte #%00001100;$FE
    .byte #%00101100;$FA
    .byte #%00011100;$F6
    ; 2
    .byte #%00000000;
    .byte #%01111100;$F4
    .byte #%00100010;$F6
    .byte #%00010000;$F8
    .byte #%00001000;$FA
    .byte #%01000110;$FC
    .byte #%01000010;$FE
    .byte #%00100010;$FA
    .byte #%00011100;$F6
    ; 3
    .byte #%00000000;
    .byte #%01111100;$F4
    .byte #%10000100;$F6
    .byte #%01000010;$F8
    .byte #%00000100;$FA
    .byte #%00001100;$FC
    .byte #%01000010;$FE
    .byte #%01000010;$FA
    .byte #%00111100;$F6
    ; 4
    .byte #%00000000;
    .byte #%00010000;$F4
    .byte #%00010000;$F6
    .byte #%00001000;$F8
    .byte #%11111000;$FA
    .byte #%01000100;$FC
    .byte #%00100100;$FE
    .byte #%00010010;$FA
    .byte #%00110010;$F6
    ; 5
    .byte #%00000000;
    .byte #%01111000;$F4
    .byte #%10000100;$F6
    .byte #%00000100;$F8
    .byte #%00001000;$FA
    .byte #%01111000;$FC
    .byte #%10000000;$FE
    .byte #%01000000;$FA
    .byte #%00111110;$F6
    ; 6
    .byte #%00000000;
    .byte #%00111000;$F4
    .byte #%01000100;$F6
    .byte #%10000010;$F8
    .byte #%11000100;$FA
    .byte #%10111000;$FC
    .byte #%10000000;$FE
    .byte #%01100000;$FA
    .byte #%00011100;$F6
    ; 7
    .byte #%00000000;
    .byte #%00100000;$F4
    .byte #%00010100;$F6
    .byte #%00001000;$F8
    .byte #%00010100;$FA
    .byte #%00100100;$FC
    .byte #%00000010;$FE
    .byte #%00000010;$FA
    .byte #%00111111;$F6

DigitColors
    ; 0
    .byte #$00;
    .byte #$F4;
    .byte #$F6;
    .byte #$F8;
    .byte #$FA;
    .byte #$FC;
    .byte #$FE;
    .byte #$FA;
    .byte #$F6;
    ; 1
    .byte #$00;
    .byte #$F4;
    .byte #$F6;
    .byte #$F8;
    .byte #$FA;
    .byte #$FC;
    .byte #$FE;
    .byte #$FA;
    .byte #$F6;
    ; 2
    .byte #$00;
    .byte #$F4;
    .byte #$F6;
    .byte #$F8;
    .byte #$FA;
    .byte #$FC;
    .byte #$FE;
    .byte #$FA;
    .byte #$F6;
    ; 3
    .byte #$00;
    .byte #$F4;
    .byte #$F6;
    .byte #$F8;
    .byte #$FA;
    .byte #$FC;
    .byte #$FE;
    .byte #$FA;
    .byte #$F6;
    ; 4
    .byte #$00;
    .byte #$F4;
    .byte #$F6;
    .byte #$F8;
    .byte #$FA;
    .byte #$FC;
    .byte #$FE;
    .byte #$FA;
    .byte #$F6;
    ; 5
    .byte #$00;
    .byte #$F4;
    .byte #$F6;
    .byte #$F8;
    .byte #$FA;
    .byte #$FC;
    .byte #$FE;
    .byte #$FA;
    .byte #$F6;
    ; 6
    .byte #$00;
    .byte #$F4;
    .byte #$F6;
    .byte #$F8;
    .byte #$FA;
    .byte #$FC;
    .byte #$FE;
    .byte #$FA;
    .byte #$F6;
    ; 7
    .byte #$00;
    .byte #$F4;
    .byte #$F6;
    .byte #$F8;
    .byte #$FA;
    .byte #$FC;
    .byte #$FE;
    .byte #$FA;
    .byte #$F6;
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete ROM sixe with exactly 4kb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC                   ; move to $FFFC
    word Reset                  
    word Reset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Acknowledgments
;; Gustavo from Udemy
;; Alienbill for his sprite and sound editors
;; Folks at AtariAge
;; and my friends and family
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;