;	<< SD Monitor >>
;	by R. L. Shoemaker  30-Nov-23

; This monitor program is for use with a Z180 SBC board with an SD card.
; All console I/O is done using the Z180's internal serial port connected to
; a USB serial port adapter.
; NOTE: The start address for this program is currently 9000H so that it can
; be used to load and debug a 32K CP/M 2.2 system and it's SD card BIOS.

; Memory locations and sizes for a 32K CP/M system are:
MEMSIZE	EQU	32					;CP/M system size in Kbytes
BIAS	EQU	(MEMSIZE-20)*1024	;offset from a 20K system
CCP		EQU	3400H+BIAS			;base address of the CCP
BIOS	EQU	CCP+1600H			;base of the BIOS
BIOSLEN	EQU	05FFH				;max length of the BIOS
SIZE	EQU	BIOS+BIOSLEN-CCP	;size of CP/M 2.2
NSECTS	EQU	SIZE/128			;number of sectors to load

; Start location for CP/M 2.2 
COLD_BOOT EQU BIOS

; Start location for the Z180 system monitor
Z180MON	  EQU 0F000H

; Clocked Serial I/O Port Addresses
Z180_CNTR	EQU	0CAH	;CSI/O Control/Status Register (R/W)
Z180_TRDR	EQU	0CBH	;CSI/O Transmit/Receive Data Register
SD_CARD_CS	EQU	0CH		;Output port for the SD card adapter
						;  bit 2 is the chip select line (0=on, 1=off)
SD_CARD_LED	EQU	0EH		;Output port for the SD card LED
						;  bit 2 turns the SD card LED ON or OFF
; Constants
CR		EQU	0DH		;ASCII code for carriage return
LF		EQU	0AH	    ;ASCII code for line feed
NULL	EQU 00H		;ASCII code for NUL
TAB		EQU	09H		;ASCII code for TAB
BS		EQU 08H		;ASCII code for backspace
SPACE	EQU 20H		;ASCII code for space
ESC		EQU 1BH		;ASCII code for escape key
MAXTRACKS  EQU 160	;total number of tracks on disks 0 and 1
MAXSECTORS EQU 26	;number of sectors on each track

; Z180 internal serial port register addresses 
Z180_STAT0	EQU	0C4H	;Status Register for serial port Chan0
Z180_TDR0	EQU	0C6H	;Transmit Data Register for serial port Chan0
Z180_RDR0	EQU	0C8H	;Receive Data Register for serial port Chan0

;=============================================================================
; Addresses for a 32KB CP/M 2.2 system
CCP:	EQU 6400H		;CCP base address for a 32 KB system
BDOS:	EQU 6C06H		;BDOS entry point
BIOS:	EQU 7A00H		;BIOS base address
CDISK:	EQU 0004H		;address containing current disk number 0 = A, 1 = B
DISKS:	EQU 02			;number of disks in the system

MEMSIZE	EQU	32					;CP/M system size in Kbytes
BIAS	EQU	(MEMSIZE-20)*1024	;offset from a 20K system
CCP		EQU	3400H+BIAS			;base address of the CCP
BIOS	EQU	CCP+1600H			;base of the BIOS

;=============================================================================
	ORG	9000H
BEGIN: LD HL,SIGNONmsg	;say hello
	CALL PRINT
	CALL INIT_SD_CARD

;========================== START OF MONITOR LOOP- ============================
MLOOP: LD DE,MLOOP	;start of the main 'work' loop
	PUSH DE	     	;set up a return to MLOOP. Note that an unbalanced
	CALL CRLF		;  return or pop will put [DE] in PC
	LD	 C,'>'
	CALL CO
	CALL CO
	
ST1: CALL TI	 		;get a console character and convert to upper case if a-z
						;  entering ESC will exit the program
	CP	 'B'			;boot the computer into CP/M
	JP	 Z,BOOT
	CP	 'D'			;display a block of memory
	JP	 Z,DISP
	CP	 'F'			;format the SD card
	JP	 Z,FORMAT
	CP	 'G'			;get CP/M from the SD card and load it into memory
	JP	 Z,GET_SYS	
	CP	 'P'			;put CP/M from memory onto the SD card
	JP	 Z,PUT_SYS
	CP	 'R'			;read one sector from the SD card
	JP	 Z,READ_SECTOR
	CP	 'W'			;write one sector onto the SD card
	JP	 Z,WRITE_SECTOR	
	CP	 'T'			;write one track onto the SD card
	JP	 Z,WRITE_TRACK
	CP	 'K'			;read one track from the SD card
	JP	 Z,READ_TRACK
	CP	 ESC			;exit program and return to system monitor
	JP	 Z180MON		;  if Escape key pressed
	JP	 MLOOP


;==============================================================================
;							    MONITOR COMMANDS
;==============================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BOOT COMMAND - Boots the computer into CP/M 2.2 after the code has been loaded
;	into memory by GET_SYS.
;	USAGE: B[CR]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
BOOT:
	JP COLD_BOOT		;jump to BIOS cold start entry

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DISP- Displays the contents of memory from <addr1> to <addr2> in both hex
;	and ASCII with the starting location displayed at the beginning of each line.
;	Parameters must be entered as hexadecimal numbers.
;   USAGE: D<addr1> <addr2>[CR]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DISP: CALL GET2HEX	;get 2 parameters in HL and DE
	LD	 A,L		;round off addresses to XX00H
	AND	 0F0H
	LD	 L,A
	LD	 A,E		;final address lower half
	AND	 0F0H
	ADD	 A,10H		;finish to end 0f line
DS0: CALL CRLF		;CRLF and print address
	CALL HLPRNT
DS1: CALL PSPACE    ;space over
	LD	 A,(HL)
	CALL APRNT
	CALL HILO		;test for end of range, quit if range exceeded
	JP	 C,MLOOP
	LD	 A,L
	AND	 0FH
	JR	 NZ,DS1
	LD	 C,TAB		;insert tab between hex and ASCII
	CALL CO
	LD	 B,4H		;also 4 spaces
TA11: LD C,SPACE
	CALL CO
	DJNZ TA11	
	LD	 B,16		;now print 16 ASCII characters
	PUSH DE			;temporarily save DE
	LD	 DE,0010H
	SBC	 HL,DE
	POP	 DE
T11: LD	 A,(HL)
	AND	 7FH
	CP	 ' ' 		;filter out control characters
	JR	 NC,T33
T22: LD	 A,'.'
T33: CP	 07CH
	JR	 NC,T22
	LD	 C,A		;set up to display ASCII
	CALL CO
	INC	 HL
	DJNZ T11		;repeat for entire line
	JR	DS0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FORMAT COMMAND - Formats an SD card to emulate a 77 (4DH) track floppy disk
;	with 26 (1AH) 128-byte sectors per track. Each sector is filled with bytes
;	of 0E5H.
;	USAGE: F[CR]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FORMAT:
	LD 	 HL,FmtBuff  	;point to 128-byte buffer and fill with 0E5H
	LD	 A,0E5H
	LD	 B,00
FILL_LOOP:
	LD	 (HL),A
	INC	 HL
	LD	 (HL),A
	INC	 HL
	DJNZ FILL_LOOP
	
	LD 	 A,00 			;set start track = 0
	LD 	 (track),A
TRK_LOOP:
 	LD	 A,01			;set start sector = 1
	LD 	 (sector),A
	CALL CRLF
SEC_LOOP:
	LD 	 HL,FmtBuff		;set start address to SecBuffer	
	LD 	 (DMAaddr),HL
	CALL PRINTI			;display the track to be formatted
	DB	 '  Formatting Trk=','$'
	LD	 A,(track)
	CALL APRNT
	CALL PRINTI			;and the sector to be formatted
	DB	 '  Sec=','$'
	LD	 A,(sector)
	CALL APRNT
	CALL CRLF
	CALL WRITE			;fill the sector with E5
	LD 	 A,(sector)
	CP 	 MAXSECTORS		;all sectors done?
	JP 	 Z,NXT_TRACK	;if so, go to next track
	INC	 A				;else fill next sector
	LD 	 (sector),A
	JR 	 SEC_LOOP
NXT_TRACK:
 	CALL CRLF
	LD 	 A,(track)		;all tracks done?
	CP 	 MAXTRACKS
	RET	 Z				;if so, all done
	INC  A				;else fill next track
	LD 	 (track),A
	JR 	 TRK_LOOP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GET_SYS COMMAND - Reads the code for the entire CP/M system into memory
;	from track 0, sectors 2 - 26, track 1, sectors 1 - 26, and track 2,
;	sectors 1 -5 on the SD card. The code will be loaded into memory at a
;	start address of <addr>.
;	<addr> = 6400H for a 32K system.
;	USAGE: G<addr>[CR]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GET_SYS:
	LD	 C,01
	CALL HEXSP
	POP	 HL				;set start address to start of CP/M
	LD	 (DMAaddr),HL
	LD	 A,00			;track 0 will be the starting track
	LD	 (track),A
	LD	 A,02			;sector 2 will be the start sector for track 0
	LD	 (sector),A
	CALL CRLF

RD_TRK0:
	CALL DISPLAY_TRKSEC
	CALL READ
	LD	 A,(sector)
	CP	 MAXSECTORS		;last sector of track 0 reached?
	JP	 Z,TRK1			;yes, start reading track 1
	INC	 A				;no, go to next sector of track 0
	LD	 (sector),A
	CALL NEXT_DMA		;increment memory address for next sector to read
	JP	 RD_TRK0		;read the next sector

TRK1: 
	CALL NEXT_DMA		;increment memory address for next sector to read
	LD A,01				;now read track 1
	LD	 (track),A
	LD	 A,01			;start with sector 1 for track 1
	LD	 (sector),A
	
RD_TRK1:
	CALL DISPLAY_TRKSEC
	CALL READ
	LD	 A,(sector)
	CP	 MAXSECTORS		;last sector of track 1 reached?
	JP	 Z,TRK2			;yes, start reading track 2
	INC	 A				;no, go to next sector
	LD	 (sector),A
	CALL NEXT_DMA		;increment memory address for next sector to read
	JP	 RD_TRK1		;read the next sector

TRK2: 
	CALL NEXT_DMA		;increment memory address for next sector to read
	LD	 A,02			;now read track 2
	LD	 (track),A
	LD	 A,01			;start with sector 1 for track 2
	LD	 (sector),A
	
RD_TRK2:
	CALL DISPLAY_TRKSEC
	CALL READ
	LD	 A,(sector)
	CP	 5				;last sector to load reached?
	JP	 Z,DONE			;yes, done loading CP/M
	INC	 A				;no, go to next sector of track 2
	LD	 (sector),A
	CALL NEXT_DMA		;increment memory address for next sector to read
	JP	 RD_TRK2		;read the next sector

DONE:
	LD	 HL,CPM_LOADmsg
	CALL PRINT	
	RET
	
NEXT_DMA:
	LD	 HL,(DMAaddr)	;increment the memory address
	LD	 DE,128
	ADD	 HL,DE
	LD	 (DMAaddr),HL
	RET

DISPLAY_TRKSEC:
	CALL PRINTI			;display the track to be read
	DB	 '  Reading Trk=','$'
	LD	 A,(track)
	CALL APRNT
	CALL PRINTI			;and the sector to be read
	DB	 '  Sec=','$'
	LD	 A,(sector)
	CALL APRNT
	CALL CRLF
	RET
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PUT_SYS COMMAND - Writes a memory image of CP/M 2.2 onto the SD card.
;	The total size of CP/M 2.2 with a full length BIOS is 1C00H bytes,
;	so we need to write 56 128-byte sectors onto the drive. Track 0 sector 1
;	is reserved for a boot loader, so we store the CP/M code on track 0
;	sectors 2 - 26, on track 1 sectors 1 - 26, and track 2 sectors 1 - 5.
;	The starting address of the memory image is at <addr>.
;	<addr> = 6400H for a 32K system.
;	USAGE: P<addr>[CR]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PUT_SYS:
	LD	 C,01
	CALL HEXSP
	POP	 HL				;set start address to start of CP/M
	LD	 (DMAaddr),HL
	LD	 A,00			;track 0 will be the starting track
	LD	 (track),A
	LD	 A,02			;sector 2 will be the start sector for track 0
	LD	 (sector),A
	CALL CRLF
	
WR_TRK0:
	CALL DISP_TRKSEC1
	CALL WRITE
	LD	 A,(sector)
	CP	 MAXSECTORS		;last sector of track reached?
	JP	 Z,WR_TRK1		;yes, start writing track 1
	INC	 A				;no, go to next sector of track 0
	LD	 (sector),A
	CALL NEXT_DMA
	JP	 WR_TRK0		;write the next sector

WR_TRK1:
	CALL NEXT_DMA		;increment memory address for next sector to write
	LD	 A,01			;now write track 1
	LD	 (track),A
	LD	 A,01			;start with sector 1 for track 1
	LD	 (sector),A


WR1:
	CALL DISP_TRKSEC1
	CALL WRITE
	LD A,(sector)		;set the sector to write
	CP	 MAXSECTORS		;last sector reached?
	JP	 Z,WR_TRK2		;yes, all done
	INC	 A				;no, go to next sector of track 1
	LD	 (sector),A
	CALL NEXT_DMA		;increment memory address for next sector to write
	JP	 WR1			;write the next sector	

WR_TRK2:
	CALL NEXT_DMA		;increment memory address for next sector to write
	LD	 A,02			;now write track 2
	LD	 (track),A
	LD	 A,01			;start with sector 1 for track 2
	LD	 (sector),A

WR2:
	CALL DISP_TRKSEC1
	CALL WRITE
	LD A,(sector)		;set the sector to write
	CP	 5				;last sector reached?
	JP	 Z,WRT_DONE		;yes, all done
	INC	 A				;no, go to next sector of track 2
	LD	 (sector),A
	CALL NEXT_DMA		;increment memory address for next sector to write
	JP	 WR2

DISP_TRKSEC1:
	CALL PRINTI			;display the track to be written
	DB	 '  Writing Trk=','$'
	LD	 A,(track)
	CALL APRNT
	CALL PRINTI			;and the sector to be written
	DB	 '  Sec=','$'
	LD	 A,(sector)
	CALL APRNT
	CALL CRLF
	RET

WRT_DONE:
	LD	 HL,CPM_WRTmsg
	CALL PRINT	
	RET

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; READ_SECTOR - Read the specified disk sector into memory, starting at the
;	specified DMA address. For an SD card, the sector size is 512 bytes.
;	However, for this version of the monitor, only the first 128 bytes
;	of each sector will contain valid data. These bytes are read
;	into memory, and dummy reads are performed for the remaining 384 bytes.
;	Parameters must be entered as hexadecimal numbers.
;	Z flag set on success.
;   USAGE: R<track> <sector> <DMAaddr>[CR]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
READ_SECTOR: 
	CALL GET3HEX
	LD	 A,L
	LD	 (track),A
	CP	 MAXTRACKS + 1	;valid track number?
	JP	 NC,TRK_ERR		;abort if not
	LD	 A,E
	CP	 A,00			;floppy disk sector numbers start with 1, not 0
	JP	 Z,SEC_ERR
	LD	 (sector),A
	CP	 MAXSECTORS + 1	;valid sector number?
	JP	 NC,SEC_ERR		;abort if not
	LD	 (DMAaddr),BC
	CALL CRLF
R1:	CALL PRINTI			;display the track and sector to be read
	DB	 'Reading ','$'
	CALL PRINT_TRK_SEC
	CALL PRNRD
	
	CALL READ			;read one sector into memory
	CP	 A,00
	RET	 NZ
	
	LD	 HL,SEC_RD1msg	;sector read successful: show sector number
	CALL PRINT			;  and memory address used
	LD	 HL,(SecNum)
	CALL HLPRNT
	LD	 HL,SEC_RD2msg
	CALL PRINT
	LD	 HL,(DMAaddr)
	CALL HLPRNT
	CALL CRLF
	XOR	 A,A			;read successful so return zero
	RET	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; READ - This is the core routine that reads one disk sector into memory,
;	starting at the memory address stored in DMAaddr. For an SD card, the
;	sector size is 512 bytes. However, for this version of the monitor, only
;	the first 128 bytes of each sector contain valid data. These bytes are read
;	into memory, and dummy reads are performed for the remaining 384 bytes.
;	Entry:	logical disk number in 'diskno'
;			logical disk track number in 'track'
;			logical disk sector number in 'sector'
;			Destination address in 'DMAaddr'
;   Exit:  A = 00 if the operation completes properly
;		   A = 01 if an error has occurred
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
READ:
	LD	 BC,(sector)
	CALL SECTRAN  		;compute the physical SD card sector number to read
	LD	 (SecNum),HL	;and store the physical sector number
	CALL DRIVE_CS_ON	;enable the SD card chip select line
READ1:
	LD	 A,51H			;send CMD17 (read one 512-byte block)
	CALL WRITE_SPI
	LD	 A,0			;send 1st byte of the command argument
	CALL WRITE_SPI
	LD	 A,0			;send 2nd byte of the command argument
	CALL WRITE_SPI
	LD	 HL,(SecNum)	;send the sector number to read in the 3rd byte		
	LD	 A,L
	CALL WRITE_SPI
	LD	 A,H			;and the 4th byte of the command argument
	CALL WRITE_SPI
	LD	 A,0FFH			;send a dummy CRC byte
	CALL WRITE_SPI
	LD	 A,0FFH			;and send the SD card 8 extra clock cycles to complete
	CALL WRITE_SPI		;  the transmit operation before the clock shuts down
	CALL READ_SPI		;get the response token in A	
	CP	 A,00			;and check its value
	JP	 Z,START_TOK	;if A = 00, sector read command was received correctly
	LD	 HL,RD_CMD_ERRmsg	;if A != 00, have an error receiving command
	CALL CMD_FAIL		;print failure message, turn off CS, and return NZ
	JR	 NZ,RD_ERR

START_TOK:
	LD	B,00			;poll SD card until we receive the start token (0FEH)
POLL: CALL READ_SPI
	CP	 A,0FFH
	JP	 Z,POLL
	CP	 A,0FEH				;was start token received?
	JP	 Z,READ_DATA		;if yes, read the 512 bytes of sector data
	DJNZ POLL				;else keep trying until we see 0FEH
	LD	 HL,SEC_RD_ERRmsg	;error: no start token found
	CALL CMD_FAIL			;print failure message, turn off CS, and return NZ
	JR	 NZ,RD_ERR

READ_DATA:
	LD	 HL,(DMAaddr)	;put the start address in HL
	LD	 B,80H
RD128:					;read 128 bytes from the drive into the buffer
	CALL READ_SPI		;read a data byte from SD card
	LD	 (HL),A			;and store it in RAM
	INC	 HL
 	DJNZ RD128
IGNORE:
	LD 	 B,80H			;read the second 128 data bytes and ignore them
R2: CALL READ_SPI	
	DJNZ R2
	LD 	 B,80H			;read the third 128 data bytes and ignore them
R3: CALL READ_SPI
	DJNZ R3
	LD 	 B,80H			;read the fourth 128 data bytes and ignore them
R4: CALL READ_SPI
	DJNZ R4	
	CALL READ_SPI		;read the 16-bit CRC 
	CALL READ_SPI	
	CALL READ_SPI		;need one extra!
	CALL DRIVE_CS_OFF	;disable the SD card chip select line
	CALL GET_CARD_STATUS
	JR	 NZ,RD_ERR
	XOR	 A,A			;read successful, so return A = 0			
	RET

RD_ERR:
	LD	 A,01
	RET

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; WRITE_SECTOR - Write one SD disk sector with 128 bytes of data pointed to
;	by DMAaddr. The remainder of the SD card's 512-byte sector is filled with
;	0E5H bytes. The sector to write is specified by the track and sector
;	variables. Z flag set on success
;	Parameters must be entered as hexadecimal numbers.
;   USAGE: W<track> <sector> <DMAaddr>[CR]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
WRITE_SECTOR:
	CALL GET3HEX
	LD	 A,L
	LD	 (track),A
	CP	 MAXTRACKS + 1	;valid track number?
	JP	 NC,TRK_ERR		;abort if not
	LD	 A,E
	CP	 A,00			;floppy disk sector numbers start with 1, not 0
	JP	 Z,SEC_ERR
	LD	 (sector),A
	CP	 MAXSECTORS + 1	;valid sector number?
	JP	 NC,SEC_ERR		;abort if not
	LD	 (DMAaddr),BC
W1:	CALL PRINTI			;display track and sector that data will be put into
	DB	 'Writing ','$'
	CALL PRINT_TRK_SEC
	CALL PRNWRT
	
	CALL WRITE			;read one sector into memory
	CP	 A,00
	RET	 NZ
	
	LD	 HL,SEC_WRT1msg	;sector write successful: show sector number
	CALL PRINT			;  and memory address used
	LD	 HL,(DMAaddr)
	CALL HLPRNT
	LD	 HL,SEC_WRT2msg
	CALL PRINT
	LD	 HL,(SecNum)
	CALL HLPRNT
	CALL CRLF
	XOR	 A,A			;writing successful so return zero
	RET
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; WRITE - This is the core routine that fills one disk sector with
;	128 bytes of data pointed to by DMAaddr. The remainder of the
;	512-byte sector is filled with 0E5H bytes.
;	Entry:	logical disk number in 'diskno'
;			logical disk track number in 'track'
;			logical disk sector number in 'sector'
;			Source address for data in 'DMAaddr'
;   Exit:  A = 00 if the operation completes properly
;		   A = 01 if an error has occurred
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
WRITE:
	LD	 BC,(sector)	
	CALL SECTRAN  		;compute the physical SD card sector number to write
	LD	 (SecNum),HL	;and store the physical sector number
	CALL DRIVE_CS_ON	;enable the SD card chip select line
	LD	 A,58H			;send CMD24 (write one 512-byte block)
	CALL WRITE_SPI
	LD	 A,0			;send 1st byte of the command argument
	CALL WRITE_SPI
	LD	 A,0			;send 2nd byte of the command argument
	CALL WRITE_SPI
	LD	 HL,(SecNum)	;send the sector number to write in the 3rd byte		
	LD	 A,L
	CALL WRITE_SPI
	LD	 A,H			;and the 4th byte of the command argument
	CALL WRITE_SPI
	LD	 A,0FFH			;send a dummy CRC byte
	CALL WRITE_SPI
	LD	 A,0FFH			;and send the SD card 8 extra clock cycles to complete
	CALL WRITE_SPI		;  the transmit operation before the clock shuts down
	CALL READ_SPI		;get the response token in A	
	CP	 A,00			;and check its value
	JP	 Z,WRITE_DATA	;if A = 00, sector write command was received correctly
	LD	 HL,WR_CMD_ERRmsg	;if A != 00, have an error receiving command
	CALL CMD_FAIL		;print failure message, turn off CS, and return NZ
	JP	 NZ,RD_ERR

WRITE_DATA:
	LD	 A,0FEH			;send a Start Block token to the SD card
	CALL WRITE_SPI
	LD	 HL,(DMAaddr)	;get start address of the RAM block to write
	LD	 B,80H
WRITE_128:
	LD	 A,(HL)			;get a data byte to write
	CALL WRITE_SPI		;and send it to the SD card
	INC  HL
	DJNZ WRITE_128		;repeat 128 times
	LD	 B,80H
REPEAT_128:
	LD	 A,0E5H			;get an E5 data byte to write
	CALL WRITE_SPI		;and send it to the SD card
	INC  HL
	DJNZ REPEAT_128		;repeat 128 times	
	LD	 B,00
REPEAT_256:
	LD	 A,0E5H			;get an E5 data byte to write
	CALL WRITE_SPI		;and send it to the SD card
	DJNZ REPEAT_256		;repeat another 256 times
	
	LD	A,0FFH			;send a dummy 16-bit CRC
	CALL	WRITE_SPI
	CALL	WRITE_SPI
	
	LD B,00				;poll SD card until a data response token received
RESP_TOK:
	CALL READ_SPI
	CP	 A,0FFH
	JP	 Z,RESP_TOK
	AND	A,1FH			;data response token received? (must be xxx0101)
	CP	A,05H
	JP	 Z,WAIT_TILL_DONE	;if yes, wait until card has finished writing the data
	DJNZ RESP_TOK			;else keep trying until we receive an 05
	LD	HL,WR_ERRmsg		;error: no data response token received
	CALL CMD_FAIL			;print failure message, turn off CS, and return NZ
	JR	 NZ,WR_ERR

WAIT_TILL_DONE:
	LD	 B,25			;  this may take up to 250 msec
WAIT2:
	LD A,10				;delay for 10 msec
	CALL MSEC_DLY
	CALL READ_SPI		;check if card is still busy?
	CP	 A,00
	JP	 NZ,WRITE_DONE	;no, writing is complete
	DJNZ WAIT2			;yes, keep checking
WRITE_ERR:
	LD	 HL,NOT_DONEmsg ;error: Sector Write did not complete
	CALL CMD_FAIL		;print failure message, turn off CS, and return NZ
	JR	 NZ,WR_ERR

WRITE_DONE:
	CALL GET_CARD_STATUS
	JR	 NZ,WR_ERR
	XOR	 A,A			;write successful, so return A = 0			
	RET

WR_ERR:
	LD	 A,01
	RET
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; READ_TRACK COMMAND - Read one 26-sector track from the CF drive into
;	memory. The start address for storing the data in memory is <addr>.
;	Parameters must be entered as hexadecimal numbers.
;	USAGE: K<track> <addr>[CR]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
READ_TRACK:
	LD	 C,02
	CALL HEXSP
	POP	 HL				;set start address
	LD	 (DMAaddr),HL
	POP	 HL				;set track to read
	LD	 A,L
	LD	 (track),A
	LD	 A,01			;set sector 1 as the start sector
	LD	 (sector),A
	CALL CRLF
	
RDM1:
	CALL R1				;read one sector
	LD	 A,(sector)
	CP	 MAXSECTORS		;last sector reached?
	JP	 Z,RDONE		;yes, all done
	INC	 A				;no, go to next sector of track
	LD	 (sector),A
	LD	 HL,(DMAaddr)	;increment the memory address
	LD	 DE,128
	ADD	 HL,DE
	LD	 (DMAaddr),HL
	JP	 RDM1			;read the next sector

RDONE: RET	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; WRITE_TRACK COMMAND - Write one 26 sector track from memory onto
;	the CF drive. The start address for the memory data is <addr>.
;	Parameters must be entered as hexadecimal numbers.
;	USAGE: T<track> <addr>[CR]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
WRITE_TRACK:
	LD	 C,02
	CALL HEXSP
	POP	 HL				;set start address
	LD	 (DMAaddr),HL
	POP	 HL				;set track to write
	LD	 A,L
	LD	 (track),A
	LD	 A,01			;set sector 1 as the start sector
	LD	 (sector),A
	CALL CRLF

WRM1:
	CALL W1				;write one sector
	LD	 A,(sector)
	CP	 MAXSECTORS		;last sector reached?
	JP	 Z,WDONE		;yes, all done
	INC	 A				;no, go to next sector of track
	LD	 (sector),A
	LD	 HL,(DMAaddr)	;increment the memory address to write
	LD	 DE,128
	ADD	 HL,DE
	LD	 (DMAaddr),HL
	JP	 WRM1			;write the next sector

WDONE: RET
	
;------------------------------------------------------------------------------	
; SECTRAN - Calculate the physical sector number for an SD card from the
;	logical floppy disk track and sector numbers.
;	The logical SSSD disk has 77 tracks (numbered 0-76) and 26 128-byte sectors
;	per track (numbered 1-26).
;	The SD card appears simply as a set of sequentially numbered sectors
;	(only sectors numbered 0 - 2001 are needed for the logical disk).
;		Entry:	Track number in 'track'
;				Sector number in BC
;   	Exit:   HL = physical sector number 
SECTRAN:
	LD	 (sector),BC
	LD	 HL,(track)		;get track number in HL
	LD	 H,00			;track is a one byte value
	ADD	 HL,HL			;HL = 2x track
	LD	 B,H			;save 2x track in BC
	LD	 C,L
	ADD	 HL,HL			;HL = 4x track
	ADD	 HL,HL			;HL = 8x track
	LD	 D,H			;save 8x track in DE
	LD	 E,L
	ADD	 HL,HL			;HL = 16x track
	ADD	 HL,DE			;HL = 24x track
	ADD	 HL,BC			;HL = 26x track
	LD	 DE,(sector)	;finally add in the logical disk sector number
	LD	 D,00			;sector is a one byte value
	DEC	 DE				;logical disk sector numbers start at 1, not 0
	ADD	 HL,DE
	LD	 (SecNum),HL	;and store the physical sector number
	RET

;=============================================================================
; 						SD CARD INITIALIZATION ROUTINES
;=============================================================================
;------------------------------------------------------------------------------
; INIT_SD_CARD - Initialize the on-board SD card for use as a mass storage
;	device.
INIT_SD_CARD:     
	LD	 A,00				;SD card type not yet determined
	LD	 (SD_TYPE),A	
	LD	 A,06H				;initially set the SPI clock to slow speed (3125 bps)
	OUT0 (Z180_CNTR),A
	CALL SD_RESET			;send the reset command (CMD0) to the SD card
	JP	 NZ,BAD_SD_RESET	;if SD_RESET returns non-zero the reset has failed
	LD	 HL,RESET_OKmsg		;show SD card initialization success
	CALL PRINT
	CALL GET_TYPE			;determine whether the SD card is a Type 2
	LD	 HL,CF_TYPE_ERRmsg	;a Type 2 SD Card was NOT detected
	CALL NZ,CMD_FAIL		;show error message and abort the initialization
	RET	 NZ
	LD	 HL,CARD_TYPE2msg	;a Type 2 SD Card was detected
	CALL PRINT	
	CALL TYPE_2_ACTIVATE	;Type 2 SD cards need special activation
	LD	 HL,CMD55_FAILmsg	;if SD Card activation failed (CMD55 or ACMD41)
	CALL NZ,CMD_FAIL_RESP	;display error message
	RET	 NZ					;and abort
	LD	 HL,CMD41_55_OKmsg	;else show initialization success
	CALL PRINT

	CALL STOP_CRC_CHECK		;turn off CRC checking
	LD	 HL,CRC_ERRORmsg	;display error trying to turn off CRC checking
	CALL NZ,CMD_FAIL_RESP
	RET	 NZ

	CALL SET_SEC_SIZE		;set Sector size to 512 bytes
	LD	 HL,SIZE_ERRORmsg	;display error setting set sector size
	CALL NZ,CMD_FAIL_RESP

	LD	 A,00H				;set the SPI clock to high speed (200,000 bps) 
	OUT0 (Z180_CNTR),A
	RET

BAD_SD_RESET:
	LD	 HL,RESET_ERRmsg	;display error resetting SD card
	CALL CMD_FAIL_RESP
	RET			;and abort

;------------------------------------------------------------------------------
; SD_RESET - Send the SD card reset command (CMD0). If the SD card does
;	not return an 01, indicating it is in the IDLE state, repeat the command
;	up to 256 times before failing and returning non-zero. 
SD_RESET:
		LD B,00
INIT2: 	CALL DRIVE_CS_OFF	;the SD card's chip select line must be disabled
							;  when sending empty clock cycles
		LD	 C,12			;transmit 12x8 empty clock cycles. The SD card uses
INIT1: 	LD	 A,0FFH			;  the SPI bus clock to drive some of its internal
		CALL WRITE_SPI		;  processing
		DEC	 C
		JP	 NZ,INIT1	
		CALL DRIVE_CS_ON	;re-enable the SD card adapter's chip select line
		LD	 HL,CMD0		;point HL to the reset command (CMD0)
		CALL SEND_SD_CMD	;  and send it to the SD card
		CALL READ_SPI		;get the response token from the SD card in [A]
		CALL DRIVE_CS_OFF	;disable the SD card's chip select line
		CP	 A,01			;if the response token = 01 the SD card is in the
		RET  Z				;  IDLE state and has been successfully reset
		DJNZ INIT2			;retry the reset command up to 256 times
		XOR	 A				;if we got here the reset command has failed
		DEC	 A				;  so return with the Z flag cleared (i.e. NZ)
		RET

;------------------------------------------------------------------------------
; GET_TYPE - Try to determine the SD card type. To do this, send a CMD8. If the
;	five byte response token is 0x01000001AA, it's a Type 2 card. Return with
;	the Z flag set if so, otherwise return NZ.
GET_TYPE:
	LD	B,40			;try repeatedly to determine card type
	CALL DRIVE_CS_ON	;enable the SD card chip select line
GET_TYPE1:
	LD	 HL,CMD_8		;send CMD8 to get card voltage
	CALL SEND_SD_CMD	
	CALL READ_SPI		;get the five byte response token	 
	CP	 A,01H			;if first value in the token = 01, we probably have
	JP	 Z,SD_TYPE_2	;  a Type 2 card, so go and confirm that
	DJNZ GET_TYPE1		;if token != 01, loop back and check again
	CALL DRIVE_CS_OFF	;disable the SD card chip select line
	XOR	 A				;failed to detect a Type 2 card
	DEC	 A
	RET					;so return NZ
SD_TYPE_2:				;confirm that we have a Type 2 card
	CALL READ_SPI		;2nd value in the response token should be 00
	CALL READ_SPI		;3rd value in the response token should be 00
	CALL READ_SPI		;4th value in the response token should be 01
	CALL READ_SPI		;5th value in the response token should be AAH	
	CALL DRIVE_CS_OFF	;disable the SD card chip select line
	CP	 A,0AAH			;see if the 5th value = AAH
	JP	 NZ,NOT_2		;if it's not AAH, card type is unknown so abort
	LD	 A,02			;if it is AAH, the SD card is definitely Type 2
	LD	 (SD_TYPE),A	;store an 02 in the SD_TYPE variable
	XOR	 A,A				
	RET					;and return Z
NOT_2:
	XOR	A,A				;return NZ to abort the initialization
	DEC	A
	RET

;------------------------------------------------------------------------------
; TYPE_2_ACTIVATE - Activate a Type 2 SD card. To do this send a CMD55, which
;	indicates that the next byte sent will be an application specific command,
;	followed by a CMD41, which starts the Type 2 SD card's activation
;	process. These 2 commands need to be sent repeatedly.
TYPE_2_ACTIVATE:
	LD	B,40			;try repeatedly to start the activation process
ACT1:
	CALL DRIVE_CS_ON	;enable the SD card chip select line
	LD	 HL,CMD_55		;send application specific command prefix
	CALL SEND_SD_CMD			
	CALL READ_SPI		;get the response token value in A
	CP	 A,01			;check its value
	JP	 Z,ACT2			;if A = 01, the response was correct - send CMD41 next	
	DJNZ ACT1			;else go back and try again	
	CALL DRIVE_CS_OFF	;disable the SD card chip select line
	XOR	 A				;could never get a correct response to CMD55
	DEC	 A
	RET					;return NZ to indicate failure
ACT2:
	CALL DRIVE_CS_OFF	;disable the SD card chip select line
	CALL DRIVE_CS_ON	;then re-enable it (apparently this is required!)
	LD	 HL,ACMD_41		;send ACMD41 to start the SD card's activation process	
	CALL SEND_SD_CMD			
	CALL READ_SPI		;get the response token value in A
	CALL DRIVE_CS_OFF	;disable the SD card chip select line
	CP	 A,00			;if A = 00, the response was correct
	RET	 Z				;return Z to indicate success
	DJNZ ACT1			;else go back and try the procedure again
	XOR	A				;could never get a correct response to ACMD41
	DEC	A
	RET					;return NZ to indicate failure

;------------------------------------------------------------------------------
; STOP_CRC_CHECK - Turn off the SD card's optional CRC check using CMD59
STOP_CRC_CHECK:
	CALL DRIVE_CS_ON	;enable the SD card chip select line
	LD	 HL,CMD_59
	CALL SEND_SD_CMD	;send CMD59 to turn off CRC checking
	CALL READ_SPI		;get the response token value in A
	CALL DRIVE_CS_OFF	;disable the SD card chip select line
	CP	 A,00			;if A = 00, the command succeeded
	RET	 Z				;so return with Z flag set
	XOR	 A				;else return NZ to indicate failure
	DEC	 A
	RET

;------------------------------------------------------------------------------
; SET_SEC_SIZE - Set the SD card's sector size to 512 bytes
SET_SEC_SIZE:
	CALL DRIVE_CS_ON	;enable the SD card chip select line
	LD	 HL,CMD_16
	CALL SEND_SD_CMD	;send CMD16 to set the sector size to 512 bytes
	CALL READ_SPI		;get the response token value in A
	CALL DRIVE_CS_OFF	;disable the SD card chip select line
	CP	 A,00			;if A = 00, the command succeeded
	RET	 Z				;so return with Z flag set
	XOR	 A				;else return NZ to indicate failure
	DEC	 A
	RET

;------------------------------------------------------------------------------
; GET_CARD_STATUS - Get the contents of the SD card's 16-bit status register.
;	The status will be returned as a 16-bit (2 byte) response token.
;	A value of 00 00 indicates no errors.
GET_CARD_STATUS:
	CALL DRIVE_CS_ON	;enable the SD card chip select line
	LD	 HL,CMD_13		;send CMD13 (Send Status) to get contents of the
	CALL SEND_SD_CMD	;  SD card's 16-bit status register	
	CALL READ_SPI		;get the 1st response token in A	
	CP	 A,00			;and check its value
	JP	 Z,STATUS1		;if A = 00, 1st byte of status reg shows no errors
	LD	 HL,STATUS_ERRmsg	;if A != 00, have an error in the status register
	CALL CMD_FAIL		;print failure message, turn off CS, and return NZ
	RET	
STATUS1:
	CALL READ_SPI		;get the 2nd response token in A	
	CP	 A,00			;and check its value
	JP	 Z,STATUS2		;if A = 00, 2nd byte of status reg shows no errors
	LD	 HL,STATUS_ERRmsg	;if A != 00, have an error in the status register
	CALL CMD_FAIL		;print failure message, turn off CS, and return NZ
	RET	
STATUS2:
	LD	 A,0FFH			;flush with extra 0FF's
	CALL WRITE_SPI	
	LD	 A,0FFH			;flush with extra 0FF's
	CALL WRITE_SPI
	CALL DRIVE_CS_OFF	;disable the SD card chip select line
	XOR	 A
	RET					;return Z if no errors

;==============================================================================
;								SPI COMMANDS
;==============================================================================
;------------------------------------------------------------------------------
; SEND_SD_CMD - This routine sends commands to the SD Card. An SD card command
;	consists of a 6-byte sequence that is sent to the SD card. The first byte
;	must be 40H + command number. Thus to send command 8, the first byte must
;	be 48H. The next 4 bytes are the command argument, and the last byte is
;	a 7-bit cyclic redundancy checksum (CRC) plus a stop bit of 1 so that the
;	last byte always has bit 0 = 1.
;	Only the [A] & [HL] registers are altered

SEND_SD_CMD:
	LD	 A,(HL)		 ;send the command number from CMD table
	CALL WRITE_SPI
	INC	 HL
	LD	 A,(HL)		;send first byte of the command argument 
	CALL WRITE_SPI
	INC	 HL
	LD	 A,(HL)		;send second byte of the command argument  
	CALL WRITE_SPI
	INC	 HL
	LD	 A,(HL)		;send third byte of the command argument  
	CALL WRITE_SPI
	INC	 HL
	LD	 A,(HL)		;send fourth byte of the command argument  
	CALL WRITE_SPI
	INC	 HL
	LD	 A,(HL)		;send the CRC byte
	CALL WRITE_SPI

	INC	 HL
	LD	 A,0FFH		;finally we must send a dummy byte that provides 8 clock
	CALL WRITE_SPI	;  cycles for the SD card to complete the transmit
	RET				;  operation before the clock is shut down
	
;------------------------------------------------------------------------------	
; WRITE_SPI - Send one byte to the SD card over the SPI interface. The bits in
;	the byte must be mirrored because the SD card expects the first bit of the
;	byte to be the MSB and the last bit to be the LSB.
;	On exit all registers are unchanged.
WRITE_SPI:
	PUSH AF
	PUSH BC
	CALL MIRROR			;mirror the bits in the byte
	CALL SD_WAITTX		;wait until any previous transmit operation is done
	OUT0 (Z180_TRDR),C	;put the byte to send into the Transmit/Receive Data reg
	IN0	 A,(Z180_CNTR)	;set bit 4 in the Control/Status reg to enable the TX
	SET	 4,A
	OUT0 (Z180_CNTR),A	;transmission of the byte is now started
	POP	 BC
	POP	 AF
	RET				

;------------------------------------------------------------------------------	
; READ_SPI - Receive one byte from the SD card over the SPI interface. On
;	exit the received byte is in A. All other registers are preserved.
READ_SPI:
	PUSH BC
	CALL SD_WAITTX		;wait until any TX operation is completed
	IN0	 A,(Z180_CNTR)	;set bit 5 in the Control/Status reg to enable the RX
	SET	 5,A
	OUT0 (Z180_CNTR),A
	CALL SD_WAITRX		;wait till a byte received in the Transmit/Receive Data reg
	IN0	 A,(Z180_TRDR)	;get the received byte
	CALL MIRROR			;mirror the bits in the byte
	LD	 A,C			;store the result in A
	POP	 BC
	RET

;------------------------------------------------------------------------------	
; SD_WAITTX - Poll the Transmit Enable bit (bit 4) in the Control/Status register
;	until it is clear. When a byte transmission operation is in progress this
;	bit will be 1. When the transmit operation is completed, the bit is
;	automatically cleared to zero.
SD_WAITTX:
	IN0	A,(Z180_CNTR)	;see if a byte transmission is currently in progress
	BIT	4,A				;if yes, the Transmit Enable bit will be set to 1
	JR	NZ,SD_WAITTX	;so loop back
	RET					;else return when the byte transmission is completed

;------------------------------------------------------------------------------	
; SD_WAITRX - Poll the Receive Enable bit (bit 5) in the Control/Status register
;	until it is clear. When a byte receive operation is in progress this
;	bit will be 1. When the receive operation is completed, the bit is
;	automatically cleared to zero.	
SD_WAITRX:
	IN0	A,(Z180_CNTR)	;see if a byte receive operation is currently in progress
	BIT	5,A				;if yes, the Receive Enable bit will be set to 1
	JR	NZ,SD_WAITRX	;so loop backk
	RET					;else return when the byte receive operation is completed
	
;------------------------------------------------------------------------------	
; MIRROR - Reverse the order of the bits in a byte so that the MSB becomes the
;	LSB and vice versa
;	On Entry:  A = byte to be mirrored 
;	On Exit:   C = the mirrored byte	
;	The result is in the C register
MIRROR:
	LD	 C,A	;if initial byte = 76543210
	RLCA
	RLCA		;we now have A = 54321076
	XOR	 C
	AND  0AAH
	XOR  C		;now have A = 56341270
	LD	 C,A
	RLCA
	RLCA
	RLCA		;now have A = 41270563
	RRC	 C
	XOR  C
	AND  066H
	XOR  C		;and finally A = 01234567
	LD	 C,A	;return the result in C
	RET		

;------------------------------------------------------------------------------					
; DRIVE_CS_OFF - Turn off the chip select signal on the SD card adapter.
;	No registers are altered.
DRIVE_CS_OFF:	
	PUSH AF
	LD	 A,04H				;bit 2 of the output port for the SD card adapter
	OUT0 (SD_CARD_CS),A		;  controls the chip select line(0=ON, 1=off)
	OUT0 (SD_CARD_LED),A	;same for SD Card's LED (0=ON, 1=off)
	POP	 AF
	RET

;------------------------------------------------------------------------------					
; DRIVE_CS_ON - Turn on the chip select signal on the SD card adapter.
;	No registers are altered.
DRIVE_CS_ON:	
	PUSH AF
	LD	 A,00H				;bit 2 of the output port for the SD card adapter
	OUT0 (SD_CARD_CS),A		;  controls the chip select line(0=ON, 1=off)
	OUT0 (SD_CARD_LED),A	;same for SD Card's LED (0=ON, 1=off)
	POP	 AF
	RET

;==============================================================================
;						   SUPPORT AND ERROR ROUTINES
;==============================================================================
;---------------------------------------------------------------
; MSEC_DLY - delay for the specified number of milliseconds.
;	On entry: A = number of millisec to delay
;	On exit:  A = 0
MSEC_DLY:
	  PUSH BC
DLY2: LD   B,50H
DLY3: PUSH IY
	  POP  IY
	  DJNZ DLY3
	  DEC  A
	  JR   NZ,DLY2 
	  POP  BC
	  RET

;------------------------------------------------------------------------------
; CMD_FAIL_RESP - Print a failure message. A pointer to the message must be
;	in [HL] on entry, and [A} must contain the response token whose
;	value will be displayed in the message.
CMD_FAIL_RESP:
	CALL DRIVE_CS_OFF	;always turn off CS on the SD Card
	CALL PRINT			;print the failure message
	CALL SHOW_BITS		;display the response token as a binary number
	XOR	 A				;return NZ
	DEC	 A
	RET	

;------------------------------------------------------------------------------
; CMD_FAIL - Print a failure message. A pointer to the message must be
;	in [HL] on entry.
CMD_FAIL:
	CALL DRIVE_CS_OFF	;always turn off CS on the SD Card
	CALL PRINT			;print the failure message
	XOR	 A				;return NZ
	DEC	 A
	RET
TRK_ERR: LD HL,TRKERRmsg	;bad track number entered
	CALL PRINT
	JP 	 ERROR

SEC_ERR: LD HL,SECERRmsg	;bad sector number entered
	CALL PRINT
	JP 	 ERROR

;------------------------------------------------------------------------------
; PSPACE - Prints a space on the console
PSPACE:  LD   C,' '	;print a space on the console
	  JP   CO

;------------------------------------------------------------------------------
; SHOW_BITS - Display the byte in A as an 8-bit binary number on the console.
SHOW_BITS:
	LD	 E,A		;move the byte into E
	LD	 B,8    	;display it as an 8-bit binary number
	CALL PSPACE		;space over
BIT2: SLA E			;shift a bit into CY
	LD	 A,18H		;load ASCII '0' divided by 2
	ADC	 A,A	    ;make into '0' or '1'
	LD	 C,A		;print it
	CALL CO
	DJNZ BIT2
	CALL CRLF
	RET
	
;==============================================================================
;								CONSOLE I/O ROUTINES
;==============================================================================

;---------------------------------------------------------------
; CO - Output a character to Z180 serial port A
;	Entry: C = character to send
;   Exit:  A = the character sent. All other registers unchanged
;---------------------------------------------------------------	
CO:	IN0	 A,(Z180_STAT0)	;check Z180 serial port 0 status register
	AND	 02H			;see if Transmit Data Register is empty
	JR	 Z,CO			;it's not empty, keep checking		
	LD	 A,C			;it is empty, output character
	OUT0 (Z180_TDR0),A
	RET

;---------------------------------------------------------------
; CI - Input a character from the console keyboard.
;	Entry: none
;   Exit:  A = character received. All other registers unchanged
;---------------------------------------------------------------	
CI:	IN0	 A,(Z180_STAT0)	;check Z180 serial port 0 status register
	AND	 80H			;see if Receive Data Register is full	
	JR	 Z,CI			;receiver buffer empty, check again
	IN0	 A,(Z180_RDR0)	;not empty, input the character
	RET
		
;---------------------------------------------------------------
; CSTS - Check the console status.
;	Entry: none
;   Exit:  A = 00 if no character from keyboard waiting
;		   All other registers unchanged
;		Else:
;			A = FF if a character waiting in the UART
;			All other registers unchanged
;---------------------------------------------------------------
CSTS: 	IN0	A,(Z180_STAT0)	;check Z180 serial port 0 status register
		AND	80H				;see if Receive Data Register is full
		LD	A,00			;return zero if no character
		JR	Z,NOCHAR
		CPL					;return 0FF otherwise
NOCHAR:	RET

;------------------------------------------------------------------------------
; TI - This is the keyboard input handling routine. It converts lower case to
;	upper case, ignores CR's, and Esc's
;   All other characters are echoed as they are received.
TI:	CALL CI		;get a character
	CP	 CR		;ignore if CR
	RET	 Z
	CP	 ESC	;ignore if ESC
	RET	 Z	
	CP	 'A'-1	;ASCII code is less than 'A', just echo it
	JR	 C,ECHO
	CP	 'z'+1	;ASCII code is greater than 'z', just echo it
	JR	 NC,ECHO
	AND	 5FH	;ASCII code in range 'A'-'z", convert to upper case 
ECHO: PUSH BC	;echo the character
	LD	 C,A
	CALL CO
	LD	 A,C
	POP	 BC
	RET

;------------------------------------------------------------------------------
; QCHK - Returns zero if A holds a space or a comma, and returns with carry set
;   if it's a CR
QCHK:  	CP 	 ' '	;return zero if delimiter
		RET	 Z
		CP	 ','
		RET	 Z
		CP	 CR		;return with carry set if CR
		SCF	
		RET	 Z
		CCF		 	;else return non-zero, no carry
		RET

;------------------------------------------------------------------------------
; PRINT - Prints a '$'-terminated string on the console. A pointer to the
;	message must be	in HL on entry.
;   Registers A,C,HL modified
PRINT: LD A,(HL)	;pick up character from message pointer (HL)
	INC	 HL
	CP	 '$'		;is it a '$'?
	RET	 Z			;done if yes
	CP	 NULL		;is it a NULL?
	RET	 Z			;done if yes
	LD	 C,A		;else print it
	CALL CO
	JR	 PRINT		;and go back for more

;------------------------------------------------------------------------------
; PRINTI - Prints a '$'-terminated string given in the next line of code
;	on the console.
;   Registers A,C,HL modified
PRINTI:	EX	 (SP),HL	;put address of next instruction in HL
		CALL PRINT
		EX	 (SP),HL	;Move updated return address back to stack
		RET

;------------------------------------------------------------------------------
; CRLF - Prints a carriage return-line feed on the console.
CRLF: PUSH BC
	LD	 C,LF
	CALL CO
	LD	 C,CR
	CALL CO
	POP	 BC
	RET

;---------------------------------------------------------------
; PRINT_TRK_SEC - Print the track and sector as decimal values
PRINT_TRK_SEC: 
	CALL PRINTI
	DB	 'Track = ','$'
	CALL INIT_STRING
	LD	 H,00				;put track number into HL
	LD	 A,(track)
	LD	 L,A
	LD	 DE,DEC_STRING
	CALL Num2Dec
	LD	 HL,DEC_STRING+3	;skip leading zeros
	CALL PRINT

	CALL PRINTI
	DB	 '  Sector = ','$'
	CALL INIT_STRING	
	LD	 H,00				;put sector number into HL
	LD	 A,(sector)
	LD	 L,A
	LD	 DE,DEC_STRING
	CALL Num2Dec
	LD	 HL,DEC_STRING+3	;skip leading zeros
	CALL PRINT
	RET
	
PRNRD: CALL PRINTI
	DB	 '  into address ','$'	
	LD	 HL,(DMAaddr)
	CALL HLPRNT
	CALL CRLF	
	RET
	
PRNWRT: CALL PRINTI
	DB	 ' from start address ','$'	
	LD	 HL,(DMAaddr)
	CALL HLPRNT
	CALL CRLF	
	RET

INIT_STRING: LD	 A,'$'
	LD	 B,06
NXTBYT: LD HL,DEC_STRING
	LD	 (HL),A
	INC	 HL
	DJNZ NXTBYT
	RET

;------------------------------------------------------------------------------		
; Num2Dec - Converts a 16-bit integer to an ASCII decimal number
;	Entry: HL = binary number to convert, DE = address of ASCII string
;	Exit:  ASCII string pointed to by DE		
Num2Dec:
		LD	 BC,-10000
		CALL Num1
		LD	 BC,-1000
		CALL Num1
		LD	 BC,-100
		CALL Num1
		LD	 C,-10
		CALL Num1
		LD	 C,B		
Num1:	LD	 A,'0' - 1
Num2:	INC	 A
		ADD	 HL,BC
		JR	 C,Num2
		SBC	 HL,BC
		LD	 (DE),A
		INC	 DE
		RET
		
;------------------------------------------------------------------------------
; HEXSP - This is the main "parameter-getting" routine. It takes hex values
;	entered at the console, separated by a space or comma, and places them
;	on the stack as 16-bit binary values.
;	On entry the C register must contain the number of hex values expected.
;	If a carriage return is entered instead of an expected hex value, it places
;	a 0000 on the stack. Entering a non-hex character causes HEXSP to abort.
HEXSP: LD   HL,0	;initialize HL to zero
EX0:   CALL TI	    ;get something from console
EX1:   LD   B,A	    ;save it in B
	CALL ASC2HEX	;convert ascii to hex
	JR	 C,EX2	    ;illegal character dectected if carry set
	ADD	 HL,HL	    ;multiply by 16
	ADD	 HL,HL
	ADD	 HL,HL
	ADD	 HL,HL
	OR	 L	     	;or in a nibble
	LD	 L,A
	JR	 EX0	    ;get more nibbles
EX2: EX	(SP),HL	    ;save on the stack
	PUSH HL	     	;replace the return
	LD	 A,B	    ;test the delimiter
	CALL QCHK
	JR	 NC,EX3	    ;jump if CR entered
	DEC	 C	     	;should go to zero
	RET	 Z	     	;return if it does
EX3: JP	 NZ,ERROR   ;something wrong
	DEC	 C	     	;do this again?
	JR	NZ,HEXSP	;yes
	RET		     	;else return
EXF: LD	 C,1
	LD	 HL,0
	JR	 EX1

ERROR:
	LD	 HL,INP_ERRmsg	;if A != 00, have an input error
	CALL CMD_FAIL		;print failure message, turn off CS, and return NZ
	RET					;then abort
;------------------------------------------------------------------------------
; ASC2HEX - Qualify the ASCII character in A as representing a valid hex digit,
;   and convert it to hexadecimal. Returns with the carry flag set if it is not
;   a hex digit (0 thru F)
ASC2HEX: SUB '0'	;qualify the character
	RET	 C	     	;no good if <0
	CP	 'G'-'0'	;is it >F?
	CCF		     	;also no good
	RET	 C
	CP	 10	     	;is it a number?
	CCF
	RET	 NC	     	;return clean if so
	SUB	 'A'-'9'-1	;adjust and filter out ":" thru "@"
	CP	 0AH
	RET

;------------------------------------------------------------------------------		
; GET2HEX - Gets two parameters from the console, places the 1st parameter in HL
;   and the 2nd in DE, then CRLFs
GET2HEX: LD	C,02
	CALL HEXSP
	POP	DE		;put 2nd param in DE
	POP	HL		;and 1st param in HL
	JP	CRLF

;------------------------------------------------------------------------------	
; GET3HEX - Gets 3 parameters from the console, places the 1st parameter in HL,
;   the 2nd in DE, the 3rd in BC, then CRLFs
GET3HEX:	LD C,03
	CALL	HEXSP
	CALL	CRLF
	POP	BC
	POP	DE
	POP	HL
	RET
	
;------------------------------------------------------------------------------
; HLPRNT - Prints the contents of the HL register on the console
HLPRNT: LD  A,H	;print HL on the console
	CALL APRNT
	LD	 A,L
;------------------------------------------------------------------------------
; APRNT - Prints the contents of the A register on the console
APRNT: PUSH	AF
	RRCA	
	RRCA	
	RRCA	
	RRCA	
	CALL LAD
	POP	 AF
LAD: CALL CONV
	JP	 CO
;------------------------------------------------------------------------------
; CONV - Converts the low nibble in A to an ASCII character and returns it in C
CONV: AND 0FH
	ADD	 A,90H
	DAA	
	ADC	 A,40H
	DAA	
	LD	 C,A
	RET

;------------------------------------------------------------------------------	
; HILO - Tests for end address of a range. The Carry flag is set if the range
;   has been exceeded.
HILO: INC	HL	;increment HL
	LD	A,H	    ;test for crossing 64K border
	OR	L
	SCF
	RET	Z	    ;return with carry set if A = 0
	LD	A,E	    ;now test HL vs. DE
	SUB	L
	LD	A,D
	SBC	A,H
	RET		    ;return with Carry set if HL > DE

;==============================================================================
;								MESSAGES
;==============================================================================
SIGNONmsg: 		DB	 CR,LF,'SD Card Monitor 30-NOV-23',CR,LF,'$'
RESET_ERRmsg:	DB	'SD Card reset to idle state failed: R1 = $'
RESET_OKmsg:	DB	'SD Card reset to idle state',CR,LF,'$'
CF_TYPE_ERRmsg: DB	'SD Card NOT Type 2. Initialization failed',CR,LF,'$'
CARD_TYPE2msg:	DB	'SD card Type 2 detected',CR,LF,'$'
CMD41_55_OKmsg: DB	'SDHC/SDXC card support enabled',CR,LF
                DB	'SD Card initialization complete',CR,LF,'$'
CMD55_FAILmsg:	DB	'SDHC/SDXC card support failed: R1 = $'
CRC_ERRORmsg:	DB	'Error turning off CRC checking',CR,LF,'$'
SIZE_ERRORmsg:	DB	'Sector size error',CR,LF,'$'
NO_INITmsg:	 	DB	'Error: SD Card not initialized',CR,LF,'$'
STATUS_ERRmsg:	DB  'Error in the SD card Status Register',CR,LF,'$'
INP_ERRmsg:		DB  '  Console input error',CR,LF,'$'
CPM_LOADmsg		DB	'CP/M loaded into memory',CR,LF,'$'
CPM_WRTmsg		DB  'CP/M written to disk',CR,LF,'$'
TRKERRmsg:		DB	'Invalid track number, abort',CR,LF,LF,'$'
SECERRmsg:		DB	'Invalid sector number, abort',CR,LF,LF,'$'
NOT_DONEmsg:	DB  'Sector Write did not complete',CR,LF,'$'
WRT_DONEmsg:	DB  'Sector Write successful',CR,LF,'$'
RD_CMD_ERRmsg:  DB	'Read Sector command failed: R1 = $'
WR_CMD_ERRmsg:  DB	'Write Sector command failed: R1 = $'
SEC_RD_ERRmsg:  DB	'Read Sector error: No 0FEH Flag',CR,LF,'$'
WR_ERRmsg:		DB  'Write Sector error: Response token not received',CR,LF,'$'
SEC_RD1msg		DB  'Physical sector $'	
SEC_RD2msg		DB  ' read into address $'
SEC_WRT1msg		DB  'Start address $'
SEC_WRT2msg		DB  ' written to physical sector $'

;==============================================================================
;								SD CARD COMMANDS
;==============================================================================	
CMD0:		DB	40H,00H,00H,00H,00H,95H,0FFH	;Reset the SD Card
CMD_8:		DB	48H,00H,00H,01H,0AAH,87H,0FFH	;Check operating voltage range
												;  (illegal command for Type 1 cards)
CMD_13:		DB	4DH,00H,00H,02H,00H,081H,0FFH	;Get the SD card's status register		
CMD_16:		DB	50H,00H,00H,02H,00H,081H,0FFH	;Set the SD card sector size to 512 Bytes
CMD_17:		DB	51H,00H,00H,00H,00H,0FFH,0FFH	;Read one sector from the SD card
CMD_24:		DB	58H,00H,00H,00H,00H,0FFH,0FFH	;Write one sector to the SD card
ACMD_41:	DB	69H,40H,00H,00H,00H,077H,0FFH	;Start the SD card's activation process
CMD_55:		DB	77H,00H,00H,00H,00H,065H,0FFH	;Prefix for a following application specific command 
CMD_59:		DB	7BH,00H,00H,00H,00H,0FDH,0FFH	;Turn off CRC checking

;==============================================================================
;								LOCAL DATA STORAGE
;==============================================================================											
SD_TYPE	 DW	0	;SD Card's Type number
SecNum	 DW	0	;16-bit physical sector number to read or write
SecCount DW	0	;number of sectors to read or write

track:	 DB	0	;logical disk track to read or write
sector:	 DB	0	;logical disk sector to read or write
diskno:	 DB	0	;disk number 0-1
DMAaddr: DW 0	;start address in RAM memory for sector data

DEC_STRING	DB '$$$$$$'

FmtBuff	  DS 128

	END
