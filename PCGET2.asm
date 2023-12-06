;	<< PCGET2 >>
;	by R. L. Shoemaker  5-Dec-23

; This program is 977 bytes long

; PCGET - This CP/M program receives a file from a PC via a serial 
; port and writes it to a file on the CP/M system. The file transfer uses
; the XMODEM protocol. Just prior to running this program, the file to be
; loaded from the PC should be already set up for transfer in a terminal
; program like TeraTerm by clicking on 'File=>Transfer=>XMODEM=>Send...',
; entering the file name to transfer in the pop-up window, and clicking
; on 'Open'.
; The program is written for use with a Z180 SBC board, and is based primarily
; on the XModem command in my Z180Mon.asm monitor program.

LOAD_ADDRESS EQU 2000H
WARM_BOOT	 EQU 0000

;==============================================================================
; Z180 internal serial port register addresses 
Z180_STAT0	EQU	0C4H	;Status Register for serial port Chan0
Z180_TDR0	EQU	0C6H	;Transmit Data Register for serial port Chan0
Z180_RDR0	EQU	0C8H	;Receive Data Register for serial port Chan0

;==============================================================================
; BDOS equates (for CP/M 2.2)
RDCON	EQU	1	;Wait for a character from the serial port; echo it to the 
				;  screen, and return it in A
WRCON	EQU	2	;Send the character in E to the screen
PRINTS	EQU	9	;Display a $-terminated string. DE = address of the string 
CONST	EQU	11	;Check console status, returns A=0 if no characters waiting,
				;  nonzero if a character is waiting.
OPEN	EQU	15	;Open a file to read OR read/write. DE = FCB address, returns
				;  A = FF if file not found
CLOSE	EQU	16	;Close a file and write any pending data. DE = FCB address,
				;  returns A = FF if error 
SRCHF	EQU	17	;Search for the first occurrence of the specified file.
				;DE = FCB address, returns A = FF if error, A = 0-3 if success. 
ERASE	EQU	19	;Deletes all directory entries matching the specified filename.
				;  DE = FCB address, returns A = FF if error 
READ	EQU	20	;Read one 128-byte sector. DE = FCB address, returns A = 0 if 
				;  read OK, A = 1 if end of file
WRITE	EQU	21	;Write one 128-byte sector. DE = FCB address, returns A = 0 if
				;  write OK, error code if A non-zero
MAKE	EQU	22	;Create a file. DE = FCB address, returns A = FF if the
				;  directory is full.

;=============================================================================				
; CP/M system addresses
BDOS		EQU	0005		;address of a jump to the BDOS
FCB			EQU	5CH			;address of the default File Control Block
PARAM1		EQU	FCB+1		;command line parameter 1 IN FCB
PARAM2		EQU	PARAM1+16	;command line parameter 2
REC_BUFF 	EQU 0080H 		;address of the default file buffer

;==============================================================================
; Constants for XModem
SOH		EQU	1		;a byte of 01 indicates start of header
EOT		EQU	4		;a byte of 04 indicates end of transmission
ACK		EQU	6		;Acknowledge
NAK		EQU	15H		;Not Acknowledge
CAN		EQU	24		;Cancel Xmodem transfer


;==============================================================================
; ASCII codes
CR		EQU	0DH		;ASCII code for carriage return
LF		EQU	0AH	    ;ASCII code for line feed
NULL	EQU 00H     ;ASCII code for NULL
CTRLC	EQU	3		;Control-C

;=============================================================================
;=============================================================================
; START OF PROGRAM CODE
;=============================================================================
;=============================================================================
	ORG	100H
	
BEGIN:
	LD 	 HL,0000
	ADD  HL,SP			;save a copy of CP/M's stack pointer in OLDSP
	LD   (OLDSP),HL
	LD   SP,STKTOP		;set SP to top of the local stack
	LD	 HL,SIGNONmsg		;print the signon message
	CALL PRINT
	LD	 A,(PARAM1)		;set A = first char of first parameter on command line
	CP 	 A,' '			;was a file name specified as the 1st parameter?
	JP	 NZ,SET_UP_XFER	;if yes, set up for the transfer
	LD	 HL,USAGEmsg	;else print error message and exit
	CALL PRINT
	JP	 EXIT

SET_UP_XFER:
	CALL ERASE_OLD_FILE	;if existing file with same name, erase it
	CALL MAKE_NEW_FILE	;initialize and open a new file
	LD	 HL,SENDmsg		;waiting for XModem program to start
	CALL PRINT
	LD	 A,40			;make sure line is clear for 20 seconds		
	CALL GET_BYTE		;while XModem program on PC is being started
	JP	 NC,CANCEL		;abort if garbage character received
	CALL XM_INIT

XM_LP:
	CALL XM_RECEIVE		;receive the next packet
	JR	 XM_LP			;loop until EOT Received		
		
;------------------------------------------------------------------------------
;XM_INIT -Tell PC to start an XModem transfer and receive the first packet	
;	from the PC	
XM_INIT:
	LD	 A,01			;initialize the packet number
	LD	 (PackNum),A
	LD	 A,NAK			;send a NAK to terminal to start the transfer
	CALL PUT_CHAR
RECV_LP:
	CALL GET_HDR		;get the packet header
	JR 	 GET_DATA		;header good, get rest of packet

;------------------------------------------------------------------------------
;GET_HDR - Reads in a packet header from the PC. It validates the header and
;	returns with Z set if the header is good. If an End-of-Text symbol (EOT)
;	is found as the first byte of the header, it jumps to GOT_EOT.
;	The routine aborts the program if neither an SOH or an EOT is received.
GET_HDR: 
	CALL CI			;test first byte in header
	CP	 SOH
	JR	 Z,GET_BNUM	;if SOH received, get packet # next
	CP	 EOT
	SCF
	JP	 Z,GOT_EOT	;if EOT received, transfer complete
	JP	 CANCEL		;else abort		
GET_BNUM:
	CALL CI			;get the packet #
	LD	 B,A		;save it in B
	CALL CI			;get the complement of the packet #
	CPL
	CP	 B			;is the packet # valid?
	JP	 NZ,CANCEL	;no, abort
	RET

;------------------------------------------------------------------------------
; XM_RECEIVE - This is the main loop that receives XModem packets from the PC,
;	verifies that the packet is valid, and writes the data in each packet
;	to the disk. On entry, PackNum = current packet number.
;  NOTE: XM_INIT jumps to GET_DATA in the middle of this routine
XM_RECEIVE: 
	LD	 A,ACK			;send ACK to start receiving next packet
	CALL PUT_CHAR
	CALL GET_HDR
	
GET_DATA: 
	LD	 C,A			;put packet # in C
	LD	 A,(PackNum)	;compare with expected packet number #
	CP	 C
	JR	 Z,PKTNUM_OK	;get rest of packet if packet # is correct
	JP	 CANCEL			;cancel if packet # isn't correct
PKTNUM_OK:
	LD	 B,128			;128 data bytes per packet
	LD	 C,00			;clear the checksum
	LD	 HL,0080H		;point to beginning of the default disk buffer
	LD	 (NextByte),HL	;and save that address in NextByte
DATA_LOOP:
	CALL CI				;get a data byte
	LD	 (HL),A			;store it in memory
	LD	 A,(HL)			;update the checksum
	ADD	 A,C
	LD	 C,A
	INC	 HL				;advance to next memory location
	DEC	 B				;decrement # of bytes left in packet
	JR   NZ,DATA_LOOP	;some bytes left, go back for more
	CALL CI				;end of packet reached, get checksum
	CP	 C
	JP	 NZ,BAD_CHKSUM	;abort if checksum not valid
	LD 	 DE,FCB			;write the 128-byte block to disk
	LD 	 C,WRITE
	CALL BDOS
	OR 	 A				;was the sector write successful?
	JP 	 NZ,WRT_ERROR
	LD	 A,(PackNum)		;advance to next packet
	INC	 A
	LD	 (PackNum),A
	RET
	
;------------------------------------------------------------------------------
;BAD_CHKSUM - Checksum not valid, cancel transfer and exit program
BAD_CHKSUM:
	LD	 HL,CHKSUMmsg
	CALL PRINT
	JP	 CANCEL

;------------------------------------------------------------------------------
;WRT_ERROR - Error writing sector to disk, cancel transfer and exit program	
WRT_ERROR:
	LD	 HL,SECTORmsg
	CALL PRINT
	JP	 CANCEL
	
;------------------------------------------------------------------------------
;CANCEL - Cancels the transfer, closes the open file, and exits the program.
CANCEL:
	LD	 A,CAN
	CALL PUT_CHAR
	CALL PUT_CHAR
	CALL PURGE
	LD	 HL,CANCELmsg
	CALL PRINT
	LD	 DE,FCB
	LD	 C,CLOSE
	CALL BDOS
	JP	 WARM_BOOT						

;------------------------------------------------------------------------------
;GOT_EOT - Acknowleges receipt of an End-of-Text symbol from the PC, closes
;	the open file, and cleanly exits the program.
GOT_EOT:
	LD	 A,NAK		;NAK the EOT
	CALL PUT_CHAR
	CALL CI			;wait for 2nd EOT
	CP	 EOT
	JR	 Z,FINISH
	CALL CANCEL	
FINISH:
	LD	 A,ACK		;ACK the 2nd EOT
	CALL PUT_CHAR
	LD	 HL,FINISHmsg
	CALL PRINT
	LD	 DE,FCB		;close the open file
	LD	 C,CLOSE
	CALL BDOS
	JP	 EXIT				

;------------------------------------------------------------------------------
; EXIT - Restore the CP/M stack and terminate the program.
EXIT:
	LD	 HL,(OLDSP)	;restore the CP/M system stack
	LD	 SP,HL
	RET				;return to the CCP

;============================= FILE I/O ROUTINES ==============================
;------------------------------------------------------------------------------
; ERASE_OLD_FILE - Erase any existing file on the disk having the same name as
;	the file specified on the command line.
ERASE_OLD_FILE:
	LD 	 DE,FCB
	LD 	 C,SRCHF	;does a file having the same name exist?
	CALL BDOS		;BDOS returns A = FF if file not found
	INC  A
	RET  Z			;no match, so just return
	LD	 DE,FCB		;else erase the existing file
	LD 	 C,ERASE
	CALL BDOS
	RET

;------------------------------------------------------------------------------
; MAKE_NEW_FILE - Create a new file on the disk using the file name specified
;	on the command line. If the current directory is full, the program will be
;	aborted.
MAKE_NEW_FILE:
	LD 	 DE,FCB		;create a new file
	LD 	 C,MAKE
	CALL BDOS		;BDOS returns A=FF if directory is full
	INC  A
	JP	 Z,MAKE_ERR
	RET
MAKE_ERR:
	LD	 HL,NO_MAKEmsg
	CALL PRINT
	JP	 EXIT

;==============================================================================
;=========================== CONSOLE I/O ROUTINES =============================
	
;------------------------------------------------------------------------------
; PRINT - Prints a '$'-terminated string on the console. A pointer to the
;	message must be	in HL on entry.
;   Registers A,C,HL modified
PRINT: LD A,(HL)		;pick up character from message pointer (HL)
	INC	 HL
	CP	 '$'		;is it a '$'?
	RET	 Z			;done if yes
	CP	 NULL		;is it a NULL?
	RET	 Z			;done if yes
	LD	 C,A		;else print it
	CALL CO
	JR	 PRINT		;and go back for more
	
;------------------------------------------------------------------------------
; CO - Output a character to Z180 serial port A
;	Entry: C = character to send
;   Exit:  A = the character sent. All other registers unchanged	
CO:	IN0	 A,(Z180_STAT0)	;check Z180 serial port 0 status register
	AND	 02H			;see if Transmit Data Register is empty
	JR	 Z,CO			;it's not empty, keep checking		
	LD	 A,C			;it is empty, output character
	OUT0 (Z180_TDR0),A
	RET

;------------------------------------------------------------------------------
; PUT_CHAR - Output a character to the console
;		A = character to send
;		All registers preserved
PUT_CHAR:
	PUSH AF
	PUSH BC		;Save registers
	LD	 C,A
	CALL CO
	POP  BC
	POP  AF
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

;------------------------------------------------------------------------------
; CSTS - Check the console status.
;	Entry: none
;   Exit:  A = 00 if no character from keyboard waiting
;		   All other registers unchanged
;		Else:
;			A = FF if a character waiting in the UART
;			All other registers unchanged
CSTS:
 	IN0	A,(Z180_STAT0)	;check Z180 serial port 0 status register
	AND	80H				;see if Receive Data Register is full
	LD	A,00			;return zero if no character
	JR	Z,NOCHAR
	CPL					;return 0FF otherwise
NOCHAR:
	RET

;------------------------------------------------------------------------------
; CI - Input a character from the console keyboard.
;	Entry: none
;   Exit:  A = character received. All other registers unchanged	
CI:
	IN0	 A,(Z180_STAT0)	;check Z180 serial port 0 status register
	AND	 80H			;see if Receive Data Register is full	
	JR	 Z,CI			;receiver buffer empty, check again
	IN0	 A,(Z180_RDR0)	;not empty, input the character
	RET

;------------------------------------------------------------------------------
; GET_BYTE - Gets a byte within a time limit
;  Entry: A contains # of 1/2 seconds to wait before returning
;  Exit:  CY=1, No Char (Time Out)
;		  CY=0, A = Char
GET_BYTE:
	PUSH	DE
	PUSH BC
	LD	 D,A		;put # of 1/2 seconds into D
GET1:
	LD	 BC,25		;inner loop count down until timeout
GET2:
	CALL CHK_BYTE	;see if a data byte is available
	JP	 NC,GOT_BYTE
	DJNZ GET2
	DEC	 C
	JR   NZ,GET2
	DEC	 D
	JR   NZ,GET1
	SCF				;carry set to indicate timeout
GOT_BYTE:
	POP BC
	POP	DE
	RET

;------------------------------------------------------------------------------
; CHK_BYTE - Check if a data byte is available, input the byte
;	if one is available
;		Exit: CY=0, A = data byte
;			  CY=1, no data byte available
;		All other registers unchanged
CHK_BYTE:
	CALL CSTS
	JR	 Z,NO_CHAR
	CALL CI
	CCF
	RET
NO_CHAR:
	SCF
	RET

;------------------------------------------------------------------------------
;PURGE - Clears all incoming bytes until the serial input line
;  is clear for 2 seconds
PURGE: 
	LD	 A,4		;2 seconds for time out
	CALL GET_BYTE
	JR	 NC,PURGE
	RET

;==============================================================================
;================================= MESSAGES ===================================		
SIGNONmsg:	DB	CR,LF,'PCGET File Transfer',CR,LF,'$'
USAGEmsg:	DB	'Usage error: need a file name',CR,LF,'$'
SENDmsg:	DB	'Send the file now using XMODEM...',CR,LF,'$'
NO_MAKEmsg:	DB	CR,LF,'Cannot make file - directory full',CR,LF,'$'
CHKSUMmsg:	DB  'Bad checksum',CR,LF,'$'
SECTORmsg	DB	'Error writing sector to disk',CR,LF,'$'
CANCELmsg:	DB	'Transfer Canceled',CR,LF,'$'
ABORTmsg:	DB	CR,LF,'Transfer Aborted',CR,LF,'$'
FINISHmsg:	DB 	CR,LF,'Transfer Complete','$'

;==============================================================================
;============================= LOCAL VARIABLES ================================
PackNum:	DW  0000
NextByte:	DW  0000
OLDSP		DW  0000	;storage for the CP/M system stack pointer
STACK		DS	64		;reserved space for a local stack
STKTOP:

	END
