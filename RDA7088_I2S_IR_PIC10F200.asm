;*******************************************************************************
; FILE:      PIC10F200+RDA7088+IR+I2S.asm                                      *
; CONTENTS:  RDA7088N + I²C + I²S + NEC IR REMOTE Control                      *
; EDITOR:    mit41301                                                          *
; UPDATED:   12/12/22                                                          *
;*******************************************************************************
 include "p10F200.inc"
 list    p=10F200

 __idlocs H'F200'
 __CONFIG _MCLRE_OFF & _CP_OFF & _WDT_OFF;
; __CONFIG H'0FEB'

;*******************************************************************************
; File register usage                                                          *
;*******************************************************************************

iRAM  set  h'10'
;********************************************************************
		cblock iRAM	
;********************************************************************
i           ;EQU    010h    ;Delay variable
j           ;EQU    011h    ;Delay variable
bit_count   ;EQU    012h    ;Counter of processed bits in I2C
i2c_data    ;EQU    013h    ;Data to receive/transmit via I2C
port        ;EQU    014h    ;Helper register to implement I2C
ack         ;EQU    015h    ;Acknowledgment received from the device
volume      ;EQU    016h    ;Radio volume level
frequency_l ;EQU    017h    ;Frequency low byte
frequency_h ;EQU    018h    ;Frequency high byte
;********************************************************************
_count      ;EQU    019h    ;Saved value of the timer
_byte_count ;EQU    01Ah    ;Counter of processed bytes
_bit_count  ;EQU    01Bh    ;Counter of processed bits
_ir_data:4  ;EQU    01Ch    ;First IR byte
;********************************************************************

RAM_		
		endc

     	if RAM_ > H'020'
     	error "File register usage overflow"
     	endif
;###############################################################################
SCL        EQU    GP1    ;SCL pin of the I²C Bus
SDA        EQU    GP2    ;SDA pin of the I²C Bus
IRx        EQU    GP3    ;INPUT ONLY PIN(IR Rx 38kHz)
;###############################################################################
;volume_init EQU 0x03 | 0x80; Initial Volume = 3 // [0..15]
volume_init EQU 0x03 | 1<<7 ;0x80; Initial Volume = 3 // [0..15]

freq  EQU d'919'	;   91.9 MHz
;freq  EQU d'943'   ;   94.3 MHz
;freq  EQU d'1040'	;  104.0 MHz

;freqB EQU (freq - d'870') ; BAND = 0 : 87~108 MHz (US/Europe)
freqB EQU (freq - 366)     ; BAND = 0 : 87~108 MHz (US/Europe)
freqH EQU (freqB >> 2)     ; Frequency = Channel Spacing(kHz) x CHAN + 87.0 MHz
freqL EQU ((freqB&3) << 6) ; Shift channel selection for matching register 0x03
	
	ORG    0x00
 
    andlw   ~1
    movwf   OSCCAL

INIT:
    MOVLW  ~((1<<T0CS)|(1<<NOT_GPPU)|(1<<PSA))
    OPTION                ;Enable GPIO2 and pull-ups

    MOVLW  0x0F           ;Save 0x0F into 'port' register
    MOVWF  port           ;It's used to switch SDA/SCL pins direction
    TRIS   GPIO           ;Set all pins as inputs

    MOVLW  0xFF           ;Perform 200 ms delay
    CALL   DELAY          ;to let the power stabilize
    MOVLW  0xFF           ;Perform 400 ms delay
    CALL   DELAY          ;to let the power stabilize

    CLRF   GPIO           ;Clear GPIO to set all pins to 0

;DEFAULT STARTING:        ;Reading the stored data from EEPROM

 	movlw volume_init ;0x83
 	movlw volume
 	movlw freqL ;0x40 ;91.90 MHz
 	movwf frequency_l
 	movlw freqH ;0x0C ;91.90 MHz
 	movwf frequency_h
;
    MOVLW  0xC0           ;Implement AND operation between 0xC0
    ANDWF  frequency_l, F ;and 'frequency_l' to clear its last 6 bits
    BSF    frequency_l, 4 ;Set bit 4 (Tune) to adjust the frequency

;START_RADIO:              ;Start FM radio
    CALL   I2C_START      ;Issue I2C Start condition
    MOVLW  0x20           ;Radio chip address for sequential writing is 0x20
    CALL   I2C_WRITE_BYTE ;Write the radio address via i2C
    MOVLW  0xD0           ;Write high byte into radio register 0x02/ BASS BOOST EN 
    CALL   I2C_WRITE_BYTE
    MOVLW  0x01           ;Write  low byte into radio register 0x02
    CALL   I2C_WRITE_BYTE
    MOVF   frequency_h, W ;Write high byte into radio register 0x03
    CALL   I2C_WRITE_BYTE
    MOVF   frequency_l, W ;Write  low byte into radio register 0x03
    CALL   I2C_WRITE_BYTE
;-----------------------------------------------------------------------------
;          IV
;-----------------------------------------------------------------------------
    MOVLW  b'00000010'    ;LOAD W Reg with 0x04 Register High byte 
    CALL   I2C_WRITE_BYTE ;Write high byte into radio register 0x04
    MOVLW  b'01000000'    ;LOAD W Reg with 0x04 Register Low byte 
    CALL   I2C_WRITE_BYTE ;Write high byte into radio register 0x04
;-----------------------------------------------------------------------------
;          V
;-----------------------------------------------------------------------------
    MOVLW  b'00000000'    ;Write high byte into radio register 0x05
    CALL   I2C_WRITE_BYTE
    MOVLW  b'00000000'    ;Write low byte into radio register 0x05
    CALL   I2C_WRITE_BYTE
;-----------------------------------------------------------------------------
;          VI
;-----------------------------------------------------------------------------
    MOVLW  b'00000010'    ;Write high byte into radio register 0x06
    CALL   I2C_WRITE_BYTE
    MOVLW  b'01110000'    ;Write low byte into radio register 0x06
    CALL   I2C_WRITE_BYTE
;-----------------------------------------------------------------------------
    CALL   I2C_STOP       ;Issue I2C Stop condition
;=============================================================================

    MOVLW  0x0F           ;Implement AND operation between 0xC0
    ANDWF  volume, F      ;and 'volume' to clear its higher 4 bits
    BSF    volume, 7      ;Set bit 7  to select correct LNA input
    GOTO   SET_VOLUME     ;And go to the 'SET_VOLUME' label

LOOP:                     

INCREASE_VOLUME:          ;Otherwise start 'INCREASE_VOLUME'
    INCF   volume, F      ;Increment the 'volume' register
    BTFSC  volume, 4      ;If bit 4 becomes set (volume = 0b10010000)
    DECF   volume, F      ;then decrement the 'volume' to get 0b10001111
    GOTO   SET_VOLUME     ;and go to the 'SET_VOLUME' label
DECREASE_VOLUME:          ;Decrease the volume here
    DECF   volume, F      ;Decrement the 'volume' register
    BTFSS  volume, 7      ;If bit 7 becomes 0 (volume = 0b01111111)
    INCF   volume, F      ;then increment the 'volume' to get 0b10000000
SET_VOLUME:               ;Set the radio volume
    CALL   I2C_START      ;Issue I2C start condition
    MOVLW  0x22           ;Radio chip address for random writing is 0x22
    CALL   I2C_WRITE_BYTE ;Write the radio address via I2C
    MOVLW  0x05           ;Set the register number to write to (0x05)
    CALL   I2C_WRITE_BYTE ;And write it via I2C
    MOVLW  0x88           ;Set the high byte of 0x05 register (default value)
    CALL   I2C_WRITE_BYTE ;And write it via i2C
    MOVF   volume, W      ;Set the 'volume' as low byte of 0x05 register
    CALL   I2C_WRITE_BYTE ;And write it via I2C
    CALL   I2C_STOP       ;Issue Stop condition

    GOTO   _LOOP           ;And return to the 'LOOP' label

;CHANNEL_SEEK:             ;CHANNEL_SEEK up | down

CH_UP:
    CALL   I2C_START      ;Issue I2C Start condition
    MOVLW  0x20           ;Radio chip address for sequential writing is 0x20
    CALL   I2C_WRITE_BYTE ;Write the radio address via I2C

    MOVLW  0xC3           ;Otherwise set 0xC3 as high byte of 0x02 register
    CALL   I2C_WRITE_BYTE ;And write it via I2C
    MOVLW  0x01           ;Set 0x01 as low byte of 0x02 register
    CALL   I2C_WRITE_BYTE ;And write it via I2C
    CALL   I2C_STOP       ;Issue I2C Stop condition
    GOTO   _LOOP           ;And return to the '_LOOP' labe;

SEEK_DOWN:                ;Seek the station down

    CALL   I2C_START      ;ADDEDIssue I2C Start condition
    MOVLW  0x20           ;ADDEDRadio chip address for sequential writing is 0x20
    CALL   I2C_WRITE_BYTE ;ADDEDWrite the radio address via I2C

    MOVLW  0xC1           ;Set 0xC1 as high byte of 0x02 register
    CALL   I2C_WRITE_BYTE ;Ending of previous transaction
    MOVLW  0x01           ;Set 0x01 as low byte of 0x02 register
    CALL   I2C_WRITE_BYTE ;And write it via I2C
    CALL   I2C_STOP       ;Issue I2C Stop condition
    GOTO   _LOOP           ;And return to the '_LOOP' labe;

;-------------Helper subroutines---------------------------------------------
SDA_HIGH:                 ;Set SDA pin high
    BSF    port, SDA      ;Set 'sda' bit in the 'port' to make it input
    MOVF   port, W        ;Copy 'port' into W register
    TRIS   GPIO           ;And set it as TRISGPIO value
    RETLW  0

SDA_LOW:                  ;Set SDA pin low
    BCF    port, SDA      ;Reset 'sda' bit in the 'port' to make it output
    MOVF   port, W        ;Copy 'port' into W register
    TRIS   GPIO           ;And set it as TRISGPIO value
    RETLW  0

SCL_HIGH:                 ;Set SCL pin high
    BSF    port, SCL      ;Set 'scl' bit in the 'port' to make it input
    MOVF   port, W        ;Copy 'port' into W register
    TRIS   GPIO           ;And set it as TRISGPIO value
    RETLW  0

SCL_LOW:                  ;Set SCL pin low
    BCF    port, SCL      ;Reset 'scl' bit in the 'port' to make it output
    MOVF   port, W        ;Copy 'port' into W register
    TRIS   GPIO           ;And set it as TRISGPIO value
    RETLW  0
;-------------I2C start condition--------------
I2C_START:
    CALL   SCL_HIGH       ;Set SCL high
    CALL   SDA_LOW        ;Then set SDA low
    RETLW  0
;-------------I2C stop condition---------------
I2C_STOP:
    CALL   SDA_LOW        ;Set SDA low
    CALL   SCL_HIGH       ;Set SCL high
    CALL   SDA_HIGH       ;Then set SDA highs and release the bus
    RETLW  0
;------------I2C write byte--------------------
I2C_WRITE_BYTE:
    MOVWF  i2c_data       ;Load 'i2c_data' from W register
    MOVLW  8              ;Load value 8 into 'bit_count'
    MOVWF  bit_count      ;to indicate we're going to send 8 bits
I2C_WRITE_BIT:            ;Write single bit to I2C
    CALL   SCL_LOW        ;Set SCL low, now we can change SDA
    BTFSS  i2c_data, 7    ;Check the MSB of 'i2c_data'
    GOTO   I2C_WRITE_0    ;If it's 0 then go to the 'I2C_WRITE_0' label
I2C_WRITE_1:              ;Else continue with 'I2C_WRITE_1'
    CALL   SDA_HIGH       ;Set SDA high
    GOTO   I2C_SHIFT      ;And go to the 'I2C_SHIFT' label
I2C_WRITE_0:
    CALL   SDA_LOW        ;Set SDA low
I2C_SHIFT:
    CALL   SCL_HIGH       ;Set SCL high to start the new pulse
    RLF    i2c_data, F    ;Shift 'i2c_data' one bit to the left
    DECFSZ bit_count, F   ;Decrement the 'bit_count' value, check if it's 0
    GOTO   I2C_WRITE_BIT  ;If not then return to the 'I2C_WRITE_BIT'
I2C_CHECK_ACK:            ;Else check the acknowledgement bit
    CALL   SCL_LOW        ;Set I2C low to end the last pulse
    CALL   SDA_HIGH       ;Set SDA high to release the bus
    CALL   SCL_HIGH       ;Set I2C high to start the new pulse
    MOVF   GPIO, W        ;Copy the GPIO register value into the 'ack'
    MOVWF  ack            ;Now bit 'sda' of the 'ack' will contain ACK bit
    CALL   SCL_LOW        ;Set SCL low to end the acknowledgement bit
    RETLW  0
;------------I2C read byte--------------------
I2C_READ_BYTE:
    MOVLW  8              ;Load value 8 into 'bit_count'
    MOVWF  bit_count      ;to indicate we're going to receive 8 bits
    CLRF   i2c_data       ;Clear the 'i2c_data' register
I2C_READ_BIT:             ;Read single bit from the I2C
    RLF    i2c_data, F    ;Shift the 'i2c_data' register one bit to the left
    CALL   SCL_LOW        ;Set SCL low to prepare for the new bit
    CALL   SCL_HIGH       ;Set SCL high to read the bit value
    BTFSC  GPIO, SDA      ;Check the 'sda' bit in the GPIO register
    BSF    i2c_data, 0    ;if it's 1 then set the LSB of the 'i2c_data'
    DECFSZ bit_count, F   ;Decrement the 'bit_count' value, check if it's 0
    GOTO   I2C_READ_BIT   ;If not, then return to the 'I2C_READ_BIT'
    CALL   SCL_LOW        ;Set SCL low to end the last pulse
    RETLW  0              ;Otherwise return from the subroutine
;----------I2C send ACK----------------------
I2C_ACK:
    CALL   SDA_LOW        ;Set SDA low to issue ACK condition
    CALL   SCL_HIGH       ;Set SCL high to start the new pulse
    CALL   SCL_LOW        ;Set SCL low to end the pulse
    CALL   SDA_HIGH       ;Set SDA high to release the bus
    RETLW  0
;----------I2C send NACK----------------------
I2C_NACK:
    CALL   SDA_HIGH       ;Set SDA low to issue NACK condition
    CALL   SCL_HIGH       ;Set SCL high to start the new pulse
    CALL   SCL_LOW        ;Set SCL low to end the pulse
    RETLW  0
;-------------Delay subroutine--------------
DELAY:                    ;Start DELAY subroutine here  
    MOVWF  i              ;Copy the value to the register i
    MOVWF  j              ;Copy the value to the register j
DELAY_LOOP:               ;Start delay loop
    DECFSZ i, F           ;Decrement i and check if it is not zero
    GOTO   DELAY_LOOP     ;If not, then go to the DELAY_LOOP label
    DECFSZ j, F           ;Decrement j and check if it is not zero
    GOTO   DELAY_LOOP     ;If not, then go to the DELAY_LOOP label
    RETLW  0              ;Else return from the subroutine
;##############################################################################
;---IR REMOTE CONTROL 38kHz ROUTINE--------------------------------------------
;##############################################################################
_LOOP:                    ;Main loop of the program
;---------------Wait for the preamble positive pulse---------------------
    BTFSC GPIO, IRx       ;Wait while 'ir' pin goes down
    GOTO _LOOP           ;If it's high then return to 'LOOP'
    CLRF  TMR0           ;Otherwise clear the timer register
    BTFSS GPIO, IRx       ;And wait while 'ir' is low
    GOTO  $-1
    MOVF  TMR0, W        ;Copy the TMR0 value into the W register
    MOVWF _count         ;and save the value into the 'count' register
    MOVLW d'30'          ;Load 30 into W (256 us x 32 = 7.7 ms)
    SUBWF _count, W      ;And subtract W from 'count'
    BTFSS STATUS, C      ;If 'count' < 30 (pulse is shorter than 7.7 ms)
    GOTO  _LOOP          ;then return to 'LOOP'
    MOVLW d'45'          ;Load 45 into W (256 us x 45 = 11.5 ms)
    SUBWF _count, W      ;And subtract W from 'count'
    BTFSC STATUS, C      ;If 'count' > 45 (pulse is longer than 8 ms)
    GOTO _LOOP           ;then return to 'LOOP'
;---------------Check the preamble negative pulse---------------------
    CLRF TMR0            ;Otherwise clear the timer register
    BTFSC GPIO, IRx       ;And wait while 'ir' is high
    GOTO $-1
    MOVF TMR0, W         ;Copy the TMR0 value into the W register
    MOVWF _count         ;and save the value into the 'count' register
    MOVLW d'13'          ;Load 15 into W (256 us x 13 = 3.3 ms)
    SUBWF _count, W      ;And subtract W from 'count'
    BTFSS STATUS, C      ;If 'count' < 13 (pulse is shorter than 3.3 ms)
    GOTO _LOOP           ;then return to 'LOOP'
    MOVLW d'20'          ;Load 20 into W (256 us x 20 = 5.1 ms)
    SUBWF _count, W      ;And subtract W from 'count'
    BTFSC STATUS, C      ;If 'count' > 20 (pulse is longer than 5.1 ms)
    GOTO _LOOP           ;then return to 'LOOP'
;---------------Receive the command bytes-----------------------------
    CLRF _byte_count     ;Clear the 'byte_count' register    
    MOVLW _ir_data       ;Load the address of the 'ir_data' into W
    MOVWF FSR            ;and save it to the indirect pointer register
_RECEIVE_BYTE:
    CLRF _bit_count      ;Clear the 'bit_count' register
    CLRF INDF            ;Clear the indirectly addressed register
_RECEIVE_BIT:
    RRF INDF, F          ;Shift the INDF register to the right
;---------------Receive the positive pulse of the bit-----------------
    CLRF TMR0           ;Otherwise clear the timer register
    BTFSS GPIO, IRx      ;And wait while 'ir' is low
    GOTO $-1
    MOVF TMR0, W        ;Copy the TMR0 value into the W register
    MOVWF _count         ;and save the value into the 'count' register
    MOVLW 1             ;Load 1 into W (256 us x 1 = 0.26 ms)
    SUBWF _count, W      ;And subtract W from 'count'
    BTFSS STATUS, C     ;If 'count' < 1 (pulse is shorter than 0.26 ms)
    GOTO _LOOP           ;then return to 'LOOP'
    MOVLW 3             ;Load 3 into W (256 us x 3 = 0.77 ms)
    SUBWF _count, W      ;And subtract W from 'count'
    BTFSC STATUS, C     ;If 'count' > 3 (pulse is longer than 0.77 ms)
    GOTO _LOOP           ;then return to 'LOOP'
;---------------Receive the negative pulse of the bit-----------------
    CLRF TMR0           ;Otherwise clear the timer register
    BTFSC GPIO, IRx     ;And wait while 'ir' is high
    GOTO $-1
    MOVF TMR0, W        ;Copy the TMR0 value into the W register
    MOVWF _count        ;and save the value into the 'count' register
    MOVLW 4;/////////// ;Load 5 into W (256 us x 4 = 1.1 ms)
    SUBWF _count, W      ;And subtract W from 'count'
    BTFSS STATUS, C     ;If 'count' < 4 (pulse is shorter than 1.1 ms)
    GOTO _NEXT_BIT       ;then go to the 'NEXT_BIT' label
    MOVLW 8             ;Load 8 into W (256 us x 8 = 2 ms)
    SUBWF _count, W      ;And subtract W from 'count'
    BTFSC STATUS, C     ;If 'count' > 8 (pulse is longer than 2 ms)
    GOTO  _LOOP         ;then go to the 'LOOP' label
    BSF   INDF, 7       ;Set the MSB of the INDF register
_NEXT_BIT:
    INCF _bit_count, F   ;Increment the 'bit_count' register
    BTFSS _bit_count, 3  ;Check if 'bit_count' becomes 8
    GOTO _RECEIVE_BIT    ;If it's not, then return to 'RECEIVE_BIT' label
    INCF _byte_count, F  ;Increment the 'byte_count' register
    BTFSC _byte_count, 2 ;Check if 'byte_count' becomes 4    
    GOTO _CHECK_DATA     ;If it is then go to 'CHECK_DATA' label
    INCF FSR, F          ;Increment the indirect addressing pointer
    GOTO _RECEIVE_BYTE   ;and go to 'RECEIVE_BYTE' label

_CHECK_DATA:
    COMF _ir_data+1, W   ;Negate the second received byte
    XORWF _ir_data, W    ;And implement the XOR between 1st and 2nd bytes
    BTFSS STATUS, Z      ;If the result is not 0 (bytes are not equal)
    GOTO _LOOP           ;Then return to the 'LOOP' label
    COMF _ir_data+3, W   ;Negate the fourth received byte
    XORWF _ir_data+2, W  ;And implement the XOR between 3rd and 4th bytes
    BTFSS STATUS, Z      ;If the result is not 0 (bytes are not equal)
    GOTO _LOOP           ;Then return to the 'LOOP' label

    MOVLW 0x05           ;Check the VOL- button (code 0x05)
    XORWF _ir_data+2, W  ;If command is not 0x05
    BTFSS STATUS, Z
    GOTO $+2             ;then skip the next line
	goto DECREASE_VOLUME

    MOVLW 0x06           ;Check the VOL+ button (code 0x06)
    XORWF _ir_data+2, W  ;If command is not 0x06
    BTFSS STATUS, Z
    GOTO $+2             ;then skip the next line
	goto INCREASE_VOLUME

    MOVLW 0x02           ;Check the << CH- button (code 0x02)
    XORWF _ir_data+2, W  ;If command is not 0x02
    BTFSS STATUS, Z
    GOTO $+2             ;then skip the next line
	goto SEEK_DOWN 

    MOVLW 0x03           ;Check the W button (code 0x03)
    XORWF _ir_data+2, W  ;If command is not 0x03
    BTFSS STATUS, Z
    GOTO $+2    ;$+2   ;then skip the next three lines
	goto CH_UP ;////////////////////////

    GOTO _LOOP           ;and go to the 'SET_OUTPUT' label

    END                   ;/* END of the Program */