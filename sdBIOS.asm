;	<< sdBIOS.asm >>
;	by R. L. Shoemaker  5-Dec-23

; The BIOS here is written for a Z180 SBC board with an SD card. All console
; I/O is done using the Z180's internal serial port connected to a USB serial
; port adapter. The BIOS code plus scratch RAM areas must fit in a 600H block
; of memory. The BIOS here uses 46CH bytes, and the scratch RAM area uses
; 0DEH bytes leaving 0B6H bytes unallocated.
; A maximum size BIOS requires that 56 sectors be loaded onto the disk, while
; only 51 sectors are available in tracks 0 and 1. An additional 5 sectors
; must be loaded into track 2, sectors 1-5. To account for this, the number
; of reserved tracks in the Disk Parameter Blocks (dpblk0 and dpblk1) have
; been increased from 2 to 3.

; The BIOS treats the SD card as if it consists of two logical SSSD 8"
; floppy disks, each having 77 tracks with 26 128-byte sectors per track.
; Logical to physical sector tranlation is done in the simplest way possible
; by only using the first 128 bytes of each 512-byte SD card sector. This
; has the advantage of not requiring any sector blocking/deblocking code
; to be written. The wasted space is irrelevant since SD cards have
; Gigabytes of storage

;=============================================================================
; The MSIZE variable specifies (in decimal kilobytes) the amount of contiguous
; RAM memory available starting at 0000. To create a copy of CP/M for a
; different memory memory size, the MSIZE variable is the only thing that needs
; to be modified.
; For a system with 32K of RAM, setting MEM = 32 will cause CP/M to be
; assembled starting at 6400H and ending at 7FFFH.
MSIZE	EQU	32					;memory size in Kbytes

BIAS	 EQU (MSIZE-20)*1024	;offset from a 20K system
CCP		 EQU 3400H+BIAS			;base of the CCP
CCP_LEN	 EQU 0800H				;fixed length of the CCP
BDOS	 EQU CCP + CCP_LEN		;base of the BDOS
BDOS_LEN EQU 0E00H				;fixed length of the BDOS
BIOS	 EQU CCP+1600H			;base of the BIOS
BIOS_LEN EQU 05FFH				;max length of the BIOS
SIZE	 EQU BIOS+BIOS_LEN-CCP  ;size of CP/M 2.2
NSECTS	 EQU SIZE/128			;number of sectors to load

BDOS_ENTRY:	EQU BDOS + 0006		;BDOS entry point

DISKS:	EQU 02			;number of disks in the system

;==============================================================================
; Page Zero reserved memory locations for CP/M 2.2
WRMBOOT	EQU 00-02	 ;Jump instruction to the warm boot entry point
IOBYTE	EQU	03		 ;I/O definition byte (ignored in the version here)
CDISK	EQU	04		 ;Current disk number 0 = A, 1 = B, and user number
ENTRY	EQU	05-07	 ;Jump instruction to the BDOS entry point
RST38	EQU 38H-3AH  ;Restart vector 7 (for use by debuggers)
SCRATCH	EQU 40H-4FH  ;Scratch RAM reserved for use by the BIOS
TFCB	EQU	5CH-7CH  ;Default file control block (FCB) location
TBUFF	EQU	80H-0FFH ;Default file buffer and command line storage location
TBASE	EQU	0100H	 ;Start address for transient programs

;==============================================================================
; Control character equates
CNTRLC	EQU	3		;control-c
CNTRLE	EQU	05H		;control-e
BS		EQU	08H		;backspace
TAB		EQU	09H		;tab
LF		EQU	0AH		;line feed
FF		EQU	0CH		;form feed
CR		EQU	0DH		;carriage return
CNTRLP	EQU	10H		;control-p
CNTRLR	EQU	12H		;control-r
CNTRLS	EQU	13H		;control-s
CNTRLU	EQU	15H		;control-u
CNTRLX	EQU	18H		;control-x
CNTRLZ	EQU	1AH		;control-z (end-of-file mark)
DEL	EQU	7FH		;rubout

;=============================================================================
; Clocked Serial I/O Port Addresses
Z180_CNTR	EQU	0CAH	;CSI/O Control/Status Register (R/W)
Z180_TRDR	EQU	0CBH	;CSI/O Transmit/Receive Data Register
SD_CARD_CS	EQU	0CH		;Output port for the SD card adapter
						;  bit 2 is the chip select line (0=on, 1=off)
SD_CARD_LED	EQU	0EH		;Output port for the SD card LED
						;  bit 2 turns the SD card LED ON or OFF
;=============================================================================
; Z180 internal serial port register addresses 
Z180_STAT0	EQU	0C4H	;Status Register for serial port Chan0
Z180_TDR0	EQU	0C6H	;Transmit Data Register for serial port Chan0
Z180_RDR0	EQU	0C8H	;Receive Data Register for serial port Chan0

;=============================================================================
;=============================================================================
; START OF BIOS CODE
;=============================================================================
;=============================================================================
	ORG	CCP		;set the origin of CP/M to be the base address of the CCP
	ORG	BIOS			;BIOS origin for a 32K system
;======================= Jump vectors for BIOS subroutines ====================
;=============================================================================
	JP	BOOT		;cold start
WBOOT_ENTRY:
	JP	WBOOT		;warm start
	JP	CONST		;console status
	JP	CONIN		;console character in
	JP	CONOUT		;console character out
	JP	LIST		;list character out
	JP	PUNCH		;punch character out
	JP	READER		;reader character out
	JP	HOME		;move head to home position
	JP	SELDSK		;select disk
	JP	SETTRK		;set track number
	JP	SETSEC		;set sector number
	JP	SETDMA		;set dma address
	JP	READ		;read disk
	JP	WRITE		;write disk
	JP	LISTST		;return list status
	JP	SECTRAN		;sector translate

;=============================================================================
; Disk parameter tables for a two-drive system with 8" SSSD floppy disks
; Three tracks are reserved on disk 0 for the CP/M system code.

; Disk Parameter Header for disk 00
dpbase:	DW	0000h, 0000h
		DW	0000h, 0000h
		DW	dirbf, dpblk0
		DW	chk00, all00
		
; Disk Parameter Header for disk 01
		DW	0000h, 0000h
		DW	0000h, 0000h
		DW	dirbf, dpblk1
		DW	chk01, all01

; Disk parameter block for disk 00
dpblk0:	DW	26	 ;SPT: number of sectors per track
		DB	3	 ;BSH: block shift factor (= 3 for 1K block size)
		DB	7	 ;BLM: block mask (= 7 for 1K block size)
		DB	0	 ;EXM: extent mask
		DW	239	 ;DSM: largest block number on the disk
		DW	63	 ;DRM: (max number of directory entries) - 1
		DB	0C0H ;AL0: directory allocation bitmap, first byte
		DB	 00H ;AL1: directory allocation bitmap, second byte
		DW	16	 ;CKS: checksum vector size (DRM+1)/4
		DW	3	 ;OFF: number of reserved tracks 

; Disk parameter block for disk 01
dpblk1:	DW	26	 ;SPT: number of sectors per track
		DB	3	 ;BSH: block shift factor (= 3 for 1K block size)
		DB	7	 ;BLM: block mask (= 7 for 1K block size)
		DB	0	 ;EXM: extent mask
		DW	239	 ;DSM: largest block number on the disk
		DW	63	 ;DRM: (max number of directory entries) - 1
		DB	0C0H ;AL0: directory allocation bitmap, first byte
		DB	 00H ;AL1: directory allocation bitmap, second byte
		DW	16	 ;CKS: checksum vector size (DRM+1)/4
		DW	80	 ;OFF: number of reserved tracks
					  ;(NOTE: logical disk 1 starts on track 80)
					   
;==============================================================================
; BOOT - This does a cold boot of CP/M 2.2. It is the entry point from from
;	the ColdStart_Loader in track 0, sector 1. The ColdStart_Loader has
;	already loaded BDOS, CCP, and CBIOS
;==============================================================================		
BOOT:
	XOR A
	LD	 (CDISK),A	;select disk zero
	JP	 GOCPM		;initialize and go to cp/m

;==============================================================================
; WBOOT - does a warm boot of CP/M 2.2. This loads only the BDOS and the CCP,
; 	starting at track 0, sector 2. The CBIOS should still be in memory
;==============================================================================
WBOOT: 
	LD SP, 0080H	;use space below buffer for stack
	LD 	 C, 00		;select disk 0	
	CALL SELDSK
	CALL HOME		;set the track number to 00

	LD 	 B,NSECTS	;B has the # of sectors to load
	LD 	 C,00		;C has the current track number
	LD 	 D,02		;D has the next sector to read
	LD	 HL,CCP 	;HL has the base of CP/M (initial load point)
LD1: PUSH BC		;save sector count and current track
	PUSH DE			;save next sector to read
	PUSH HL			;save DMA address
	LD 	 C,D		;set the first sector to load
	CALL SETSEC
	POP	 BC			;restore the DMA address in BC
	PUSH BC			;replace on stack for later
	CALL SETDMA		;set the DMA address
	CALL READ		;read one sector from the disk
	CP	 00			;any errors?
	JP	 NZ,WBOOT	;retry the entire boot if an error occurs

	POP	 HL			;get DMA address in HL
	LD	 DE, 128
	ADD	 HL,DE		;increment DMA address by 128
	POP	 DE			;get current sector #
	POP	 BC			;get # of sectors remaining and current track
	DEC	 B			;decrement # of sectors remaining
	JP	 Z,GOCPM	;if none left, boot CP/M

	INC	 D			;was last sector on track read?
	LD 	 A,D		;if yes, change tracks
	CP	 27
	JP	 C,LD1		;else go back and load next sector
	INC	 C			;increment the current track number
	LD 	 D,01		;begin with first sector of next track

	PUSH BC			;save register state, and change tracks
	PUSH DE
	PUSH HL
	CALL SETTRK		;track address set from register C
	POP	 HL
	POP	 DE
	POP	 BC
	JP	 LD1		;go back for another sector

;--------------------------------------------------------------------
; GOCPM - Load operation complete, set parameters and jump to CP/M
GOCPM: 
	LD A,0C3H		;set a jump to the warm boot routine
	LD	 (0000),A	;  at location 0000
	LD	 HL, WBOOT_ENTRY
	LD	 (0001),HL
	LD	 (0005),A	;set a jump to the BDOS entry point at location 0005
	LD	 HL, BDOS_ENTRY
	LD	 (0006),HL
	LD	 BC,0080h	;set the default DMA address to 0080H
	CALL SETDMA
	LD	 HL,SIGNONmsg
	CALL PRNT		;display the sign-on message
	
	LD	 A,(CDISK)	;get current disk number
	CP	 DISKS		;see if it's a valid disk number
	JP	 C,DISKOK	;if so, jump to the CCP
	LD	 A,00		;else change to disk 0
DISKOK:	LD C,A
	JP	 CCP		;jump to the CCP with current disk number in C

SIGNONmsg:
	DEFB CR,LF,'CP/M v2.2',CR,LF,'$'
	
;------------------------------------------------------------------------------
; PRNT - Prints a '$'-terminated string on the console. A pointer to the
;	message must be	in HL on entry.
;   Registers A,C,HL modified
PRNT: LD A,(HL)	;pick up character from message pointer (HL)
	INC	 HL
	CP	 '$'		;is it a '$'?
	RET	 Z			;done if yes
	LD	 C,A		;else print it
	CALL CO
	JR	 PRNT		;and go back for more
	
;--------------------------------------------------------------------
; CONST - Check the console status.
;	Entry: none
;   Exit:  A = 00 if no character from keyboard waiting
;		   All other registers unchanged
;		Else:
;			A = FF if a character waiting in the UART
;			All other registers unchanged
;--------------------------------------------------------------------
CONST:
	CALL CSTS
	RET

;---------------------------------------------------------------
; CONIN - Input a character from the console keyboard.
;	Entry: none
;   Exit:  A = character received. All other registers unchanged
;---------------------------------------------------------------
CONIN:
	CALL CI
	AND	7fh		;strip parity bit
	CP	 'A'-1	;ASCII code is less than 'A', leave char unchanged
	RET  C
	CP	 'z'+1	;ASCII code is greater than 'z', leave char unchanged
	RET	 NC
	AND	 5FH	;ASCII code in range 'A'-'z", convert to upper case 
	RET

;---------------------------------------------------------------
; CONOUT - Output a character to the console
;	Entry: C = character to send
;   Exit:  A = the character sent. All other registers unchanged
;---------------------------------------------------------------
CONOUT:
	CALL CO
	RET

;---------------------------------------------------------------
; LIST - Send a character to the list device. This is a
;	dummy routine
;---------------------------------------------------------------
LIST:
	LD 	A,C	  	;just move character from C to A
	RET		  	;null subroutine

;---------------------------------------------------------------
; LISTST - Return the status of the list device
;	(0 if not ready, 1 if ready). This is a dummy routine
;---------------------------------------------------------------
LISTST:
	XOR	A	 	;always return 00
	RET

;---------------------------------------------------------------
; PUNCH - Send a character to the punch device. This is a
;	dummy routine
;---------------------------------------------------------------
PUNCH:
	LD 	A,C	  	;just move character from C to A
	RET		  	;null subroutine

;---------------------------------------------------------------
; READER - Get a character from the reader device. This is a
;	dummy routine
;   Exit:  A = end of file character (1AH)
;---------------------------------------------------------------
READER:
	LD	 A,1AH
	AND	 7FH	;remember to strip parity bit
	RET

;--------------------------------------------------------------------
; HOME - Move head to track 00 of the current drive
HOME: 
	LD C,00		;select track 0
	CALL SETTRK
	RET			;we will move to track 00 on first read or write

;--------------------------------------------------------------------
; SELDSK - Select the disk to use for reads or writes and return
;	a pointer to the Disk Parameter Header table
;	Entry: C = the disk number to select (0 or 1)
;   Exit:  the 'diskno' variable is set
;		   HL = pointer to the Disk Parameter Header table
;--------------------------------------------------------------------
SELDSK:	
	LD	 HL, 0000h	;error return code
	LD	 A,C		;check the disk number
	LD	 (diskno),A
	CP	 DISKS		;must be 0 or 1
	RET	 NC			;return with carry clear if > 1
	LD	 A,(diskno)
	LD 	 L,A		;set L to disk number: 0 or 1
	LD 	 H,00		;high order byte is 00
	ADD	 HL,HL		;*2
	ADD	 HL,HL		;*4
	ADD	 HL,HL		;*8
	ADD	 HL,HL		;*16 (size of each header)
	LD	 DE, dpbase
	ADD	 HL,DE		;HL = dpbase + (16*diskno)
	RET

;--------------------------------------------------------------------
; SETTRK - Select a disk track to use for reads or writes
;	Entry: C = the disk track to select
;   Exit:  the 'track' variable is set
;--------------------------------------------------------------------
SETTRK:
	LD 	A,C
	LD	(track),A
	RET

;--------------------------------------------------------------------
; SETSEC - Select a disk sector to use for reads or writes
;	Entry: C = the disk sector to select
;   Exit:  the 'sector' variable is set
;--------------------------------------------------------------------
SETSEC:
	LD 	A,C
	LD	(sector),A
	RET

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
;------------------------------------------------------------------------------
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

;--------------------------------------------------------------------
; SETDMA - Set the DMA address for sector reads or writes	
;	Entry:  Translation table pointed to by DE
;			BC = the DMA address to use
;   Exit:   The 'DMAaddr' variable is set
;--------------------------------------------------------------------
SETDMA:
	LD 	L,C			;set HL to the DMA address
	LD 	H,B
	LD	(DMAaddr),HL	;and store it in DMAaddr
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
	JP	 NZ,WRT_DONE	;no, writing is complete
	DJNZ WAIT2			;yes, keep checking
WRITE_ERR:
	LD	 HL,NOT_DONEmsg ;error: Sector Write did not complete
	CALL CMD_FAIL		;print failure message, turn off CS, and return NZ
	JR	 NZ,WR_ERR

WRT_DONE:
	CALL GET_CARD_STATUS
	JR	 NZ,WR_ERR
	XOR	 A,A			;write successful, so return A = 0			
	RET

WR_ERR:
	LD	 A,01
	RET

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

;------------------------------------------------------------------------------
; CMD_FAIL - Print a failure message. A pointer to the message must be
;	in [HL] on entry.
CMD_FAIL:
	CALL DRIVE_CS_OFF	;always turn off CS on the SD Card
	CALL PRNT			;print the failure message
	XOR	 A				;return NZ
	DEC	 A
	RET	

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

;============================ CONSOLE I/O ROUTINES ============================
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

;================================== MESSAGES ==================================
RD_CMD_ERRmsg:  DB	'Sector Read command failed',CR,LF,'$'
SEC_RD_ERRmsg:  DB	'Sector Read error: No 0FEH Flag',CR,LF,'$'
NOT_DONEmsg:	DB  'Sector Write did not complete',CR,LF,'$'
WR_CMD_ERRmsg:  DB	'Sector Write command failed',CR,LF,'$'
WR_ERRmsg:		DB  'Sector Write error: no response token',CR,LF,'$'
STATUS_ERRmsg:	DB  'Error in Status Register',CR,LF,'$'

;=============================== SD CARD COMMAND ==============================
CMD_13:	 DB	4DH,00H,00H,02H,00H,081H,0FFH	;Get the SD card's status register

;=============================== BIOS VARIABLES ===============================
track:	 DB	0		;logical disk track to read or write
sector:	 DB	0		;logical disk sector to read or write
diskno:	 DB	0		;disk number (0-1)
DMAaddr: DW 0		;RAM memory address for disk operations
SecNum:  DW 0		;physical SD card sector number

;========================= Scratch RAM area for BDOS use ======================
begdat:	EQU	$	 	;beginning of scratch RAM area
dirbf:	DW	0000,0000,0000,0000,0000,0000,0000,0000 ;scratch directory area
		DW	0000,0000,0000,0000,0000,0000,0000,0000 ;128 bytes
		DW	0000,0000,0000,0000,0000,0000,0000,0000
		DW	0000,0000,0000,0000,0000,0000,0000,0000				
		DW	0000,0000,0000,0000,0000,0000,0000,0000
		DW	0000,0000,0000,0000,0000,0000,0000,0000
		DW	0000,0000,0000,0000,0000,0000,0000,0000
		DW	0000,0000,0000,0000,0000,0000,0000,0000		
		
all00:	DW	0000,0000,0000,0000,0000,0000,0000,0000 ;allocation vector 0
		DW	0000,0000,0000,0000,0000,0000,0000		;31 bytes
		DB	00
all01:	DW	0000,0000,0000,0000,0000,0000,0000,0000 ;allocation vector 1
		DW	0000,0000,0000,0000,0000,0000,0000		;31 bytes
		DB	00
		
chk00:	DW	0000,0000,0000,0000,0000,0000,0000,0000 ;check vector 0 (16 bytes)
chk01:	DW	0000,0000,0000,0000,0000,0000,0000,0000 ;check vector 1 (16 bytes)

enddat:	EQU	$	 		;end of scratch RAM area
datsiz:	EQU	$-begdat;	;size of scratch RAM area

	END
