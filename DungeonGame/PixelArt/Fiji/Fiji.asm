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
    
BGColor             byte        ; register containing BG color address
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start our ROM code at memory address $F000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg Code
    org $F000
    
Reset:
    CLEAN_START                 ; call macro to reset memory and registers
    
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
    REPEAT 37
        sta WSYNC               ; Display remaining lines of VBLANK    
    REPEND
    
    lda #0
    sta VBLANK                  ; Turn off VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Render the 96 visible scanlines of main game (2 line kernel)
;; It is a 2lk because I call WSYNC twice 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; code here
    lda #$FF
    sta BGColor
    lda #%00000001
    sta CTRLPF
    lda #%11111111
    sta PF0
    ;lda #%11111111
    ;sta PF1
    lda #%00110000
    sta PF2
MainLoop:
    ldx #192
.BGLoop:
    lda BGColor
    sta COLUBK
    ;lda PlayField1
    ;sta PF1
    sta WSYNC
    lda BGColor
    sec
    sbc #$01
    sta BGColor
    dex
    bne .BGLoop
    
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
PlayField1:
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
    .byte $%00001111
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete ROM sixe with exactly 4kb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC                   ; move to $FFFC
    word Reset                  
    word Reset