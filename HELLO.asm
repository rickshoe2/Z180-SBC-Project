;	<< HELLO >>
;	by R. L. Shoemaker  5-Dec-23

;  HELLO - This program simply prints out a hello message on the console.
;	Program length is 143 bytes.

;=============================================================================
; BDOS equates (for CP/M 2.2)
RDCON	EQU	1	;Wait for a character from the serial port; echo it to the 
				;  console, and return it in A
WRCON	EQU	2	;Send the character in E to the console
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
BDOS	EQU	0005		;address of a jump to the BDOS
FCB		EQU	5CH			;address of the default File Control Block
PARAM1	EQU	FCB+1		;command line parameter 1 in FCB
PARAM2	EQU	PARAM1+16	;command line parameter 2

;=============================================================================
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

MAIN:
	LD HL,0000		;store system SP in variable OLDSP
	ADD	 HL,SP
	LD	 (OLDSP),HL
	LD	 SP,STKTOP	;set a new SP to be STKTOP

	CALL CRLF
	LD	 DE,MSG
	CALL PRINT_STR		;print the Hello World message

FINISH:
	LD	 HL,(OLDSP)	;restore the system SP
	LD	 SP,HL
	RET				;and return to CP/M
	
;============================ CONSOLE I/O ROUTINES ============================

;------------------------------------------------------------------------------
; PRINT_STR - Prints a '$'-terminated string on the console. A pointer to the
;	message must be	in DE on entry.
;	All registers except A remain unchanged.
PRINT_STR:
	PUSH HL
	PUSH BC
	LD	 C,PRINTS
	CALL BDOS
	POP  BC
	POP  HL
	RET
;------------------------------------------------------------------------------
; PUT_CHAR - Print the character in register A on the console.
;	All registers except A remain unchanged.
;------------------------------------------------------------------------------	
PUT_CHAR:
	PUSH HL			;save registers
	PUSH DE
	PUSH BC
	LD	 E,A		;put char to print in E
	LD	 C,WRCON	;call the console output routine in the BDOS
	CALL BDOS
	POP  BC			;restore registersx
	POP  DE
	POP  HL
	RET

;------------------------------------------------------------------------------
; CRLF - Send a CR and an LF to the console.
;	All registers except A remain unchanged.
;------------------------------------------------------------------------------
CRLF:
	LD	 A,CR
	CALL PUT_CHAR
	LD	 A,LF
	CALL PUT_CHAR
	RET

;================================== MESSAGES ==================================
MSG:	DEFB 'Hello from CP/M 2.2','$'

;========================= STORAGE FOR RAM VARIABLES ==========================
OLDSP  DS 2		;system SP stored here
STACK  DS 3FH	;reserve space for a local stack
STKTOP DS 1		;top of new stack	

	END