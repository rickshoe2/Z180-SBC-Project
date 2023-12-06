;	<< FILEDUMP >>
;	by R. L. Shoemaker  29-Nov-23

;  FILEDUMP - This program reads an input file from disk,and prints the
;	contents of the file on the console.
;	Program length is 282 bytes.
;=============================================================================
; BDOS equates (for CP/M 2.2)
CONIN	EQU	1	;Wait for a character from the serial port; echo it to the 
				;  screen, and return it in A
CONOUT	EQU	2	;Send the character in E to the screen
RAWIO	EQU 6
PSTRING	EQU	9	;Display a $-terminated string. DE = address of the string 
CONSTS	EQU	11	;Check console status, returns A=0 if no characters waiting,
				;  nonzero if a character is waiting.
FOPEN	EQU	15	;Open a file to read or read/write. DE = FCB address, returns
				;  A = FF if file not found
FCLOSE	EQU	16	;Close a file and write any pending data. DE = FCB address,
				;  returns A = FF if error 
SRCHF	EQU	17	;Search for the first occurrence of the specified file.
				;DE = FCB address, returns A = FF if error, A = 0-3 if success. 
ERASE	EQU	19	;Deletes all directory entries matching the specified filename.
				;  DE = FCB address, returns A = FF if error 
READREC	EQU	20	;Read one 128-byte record from an opened file.
				;  DE = FCB address, returns A = 0 if read OK, A = 1 if EOF
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

;=============================================================================
; File Control Block definitions
FCBDN EQU FCB+0 ;disk name
FCBFN EQU FCB+1 ;file name
FCBFT EQU FCB+9 ;disk file type (3 characters)
FCBRL EQU FCB+12 ;file's current reel number
FCBRC EQU FCB+15 ;file's record count (0 to 128)
FCBCR EQU FCB+32 ;current (next) record number (0 to 127)
FCBLN EQU FCB+33 ;FCB length

;=============================================================================
; ASCII codes
CR		EQU	0DH		;ASCII code for carriage return
LF		EQU	0AH	    ;ASCII code for line feed
NULL	EQU 00H     ;ASCII code for NULL
SPACE	EQU 20H		;ASCII code for a space
TAB		EQU 09		;ASCII code for a tab
ESC		EQU 1BH		;ASCII code for escape key
CTRLC	EQU	03		;Control-C

;=============================================================================
;=============================================================================
; START OF PROGRAM CODE
;=============================================================================
;=============================================================================
	ORG	100H
	
BEGIN:
	LD 	 HL,0000
	ADD  HL,SP			;save a copy of CP/M's stack pointer in STACK
	LD   (OLDSP),HL
	LD   SP,STKTOP		;set to top of the local stack
	LD	 DE,SIGNONmsg	;say hello
	LD	 C,PSTRING
	CALL BDOS
	CALL OPEN_FILE		;open the file for reading
NEW_REC:
	CALL READ_REC
	CALL PRINT_REC	
	JP	 NEW_REC

;------------------------------------------------------------------------------
; READ_REC - Read a 128-byte record from the disk. On entry,
;	must have DE = FCB address. Returns A = 0 if read is OK,
;	A = 1 if end of file.
;------------------------------------------------------------------------------	
READ_REC:
	PUSH HL
	PUSH DE
	PUSH BC
	LD	 DE,FCB		;use the info in the FCB to read one sector
	LD	 C,READREC
	CALL BDOS
	POP  BC
	POP  DE
	POP  HL
	OR	 A
	JP 	 NZ,DONE
	RET

;------------------------------------------------------------------------------
; PRINT_REC - Print the contents of the 128-byte buffer pointed to by REC_BUFF
;	on the console in hex
;------------------------------------------------------------------------------
PRINT_REC:
	LD	 HL,REC_BUFF
	LD	 C,08
	CALL CRLF

NULINE:
	LD	 B,16
	CALL CRLF
NUBYTE:
	LD	 D,(HL)
	CALL PBYTE
	CALL PSPACE
	INC	 HL
	DEC	 B
	JP	 NZ,NUBYTE
NUTEXT:
	LD	 B,04		;insert 4 spaces
TX0: LD A,SPACE
	CALL PCHAR
	DJNZ TX0	
	LD	 B,16		;now print 16 ASCII characters
	PUSH DE
	LD	 DE,0010H	;go back 16 bytes in the buffer
	SBC	 HL,DE
	POP	 DE
TX1: LD	 A,(HL)
	AND	 7FH
	CP	 ' ' 		;filter out control characters
	JR	 NC,TX3
TX2: LD	 A,'.'
TX3: CP	 07CH
	JR	 NC,TX2
	CALL PCHAR		;display byte as ASCII characters
	INC	 HL
	DJNZ TX1		;repeat for entire line	
	DEC	 C
	JP	 NZ,NULINE
	CALL CRLF
	CALL KEY_CHK		;check to see if the escape key was pressed
	CP	 ESC
	JP	 Z,DONE			;abort the program if yes
	RET
	
;============================= SUPPORT ROUTINES ===============================
;------------------------------------------------------------------------------
; DONE - Clean up the stack and terminate the program.
;------------------------------------------------------------------------------
DONE:
	LD	 HL,(OLDSP)	;restore the CP/M system stack
	LD	 SP,HL
	RET				;return to the CCP
	
;------------------------------------------------------------------------------
; OPEN_FILE - Open a file for reading or writing. DE must point
;	to the file's File Control Block on entry. An error code is
;	returned in A (there is a read error if A = FF).
;------------------------------------------------------------------------------
OPEN_FILE:	
	LD	 DE,FCB
	LD	 C,FOPEN
	CALL BDOS
	INC	 A			;if A = FF on exit, it's now 00, meaning no file was found
	JR   Z,NO_FILE
	RET
	
NO_FILE:
	LD	 DE,NO_FILEmsg
	LD	 C,PSTRING
	CALL BDOS
	JP	 DONE

;=========================== CONSOLE I/O ROUTINES =============================

;------------------------------------------------------------------------------
; PBYTE - Print the byte in register A as a pair of hex digits on the console.	
;------------------------------------------------------------------------------
PBYTE:
	LD	 A,D		;save the byte in A
	RRCA			;rotate the high nibble into the low nibble
	RRCA
	RRCA
	RRCA
	CALL PNIBBLE 	;and print the high nibble
	LD	 A,D		;restore the byte in A
	CALL PNIBBLE	;and print the low nibble
	RET

;------------------------------------------------------------------------------
; PNIBBLE - Print the low nibble of the byte in register A on the console.	
;------------------------------------------------------------------------------
PNIBBLE:
	AND	 A,0FH 		;mask off the high nibble of the byte
	CP	 A,0AH		;is the hex digit in the nibble A-F?
	JP	 NC,P10		;yes, print it as an alphabetic character
	ADD	 A,'0'		;no, convert to a numeric ASCII code in the range 0-9
	JP	 PRN
P10: ADD A,'A' - 10	;convert to an alphabetic ASCII code
PRN: CALL PCHAR
	 RET

;------------------------------------------------------------------------------
; PCHAR - Print the character in register A on the console.
;	All registers, including A, remain unchanged.
;------------------------------------------------------------------------------
PCHAR:
	PUSH HL
	PUSH DE
	PUSH BC
	LD	 C,CONOUT	;use the Write Console BDOS routine to output the character
	LD	 E,A
	CALL BDOS
	POP  BC
	POP  DE
	POP  HL
	RET

;------------------------------------------------------------------------------
; PSPACE - Print a space on the console
;	All registers except A remain unchanged.
;------------------------------------------------------------------------------
PSPACE:
	LD	 A,SPACE
	CALL PCHAR
	RET

;------------------------------------------------------------------------------
; CRLF - Send a CR and an LF to the console.
;	All registers except A remain unchanged.
;------------------------------------------------------------------------------
CRLF:
	LD	 A,CR
	CALL PCHAR
	LD	 A,LF
	CALL PCHAR
	RET
	
;------------------------------------------------------------------------------
; KEY_CHK - Check console status to see if a key has been pressed,
;	and abort the program if so.
;------------------------------------------------------------------------------
KEY_CHK:
	PUSH HL
	PUSH DE
	PUSH BC
	LD	 C,CONSTS
	CALL BDOS
	POP  BC
	POP  DE
	POP  HL
	RET
	
;================================== Messages ==================================
SIGNONmsg: 	DB	CR,LF,'DUMP-HEX' ,'$'
NO_FILEmsg  DB	CR,LF,'No such filename$',CR,LF,'$'

;---------------------------   RAM Data Area   --------------------------------
OLDSP	DW  0	;storage for the CP/M system stack pointer
STACK	DS	64	;reserved space for a local stack
STKTOP:

	END


