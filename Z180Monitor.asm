;	<< Z180 Monitor v2.0 >>
;	by R. L. Shoemaker  30-Nov-23

; This Z180 Monitor program is designed to run on a Z180 SBC board.
; The board has 1 Megabyte of physical memory. The lower 512KB is ROM
; and the upper 512KB is RAM. The MMU's default settings after reset cause
; the Z80 core's logical 64KB address space (0000H to FFFFH) to correspond
; one-to-one with the first 64KB of the 1024KB (00000H to FFFFFH)
; physical address space. 
; The external XTAL clock operates at 18.432 MHz, with the CPU clock configured
; to run at PHI = 9.216 MHz with 3 memory wait states, and 3 I/O wait states.
; All interrupts are disabled.

;==============================================================================
; BAUD_RATE defines the baud rate used by the Z180's internal serial port.
;	If BAUD_RATE = 00, the baud rate generator prescale factor is set to 10
; 	and the divide ratio to 16. The Baud rate is then 57,600.
; 	If BAUD_RATE = 20H, the Baud rate is set to 19,200
BAUD_RATE	EQU 20H	;set the Baud rate to 19,200 here

;==============================================================================
; STACK_TOP defines the top of the monitor's system stack. The monitor itself
;	begins at F000, and the 20H bytes between EFE0 and F000 are reserved for a
;	register storage area used by the GOTO command.
STACK_TOP EQU 0EFE0H

;==============================================================================
; Z180 internal register addresses
IO_BASE	  	EQU	0C0H
Z180_DCNTL	EQU	IO_BASE + 32H		;DMA/wait control
Z180_RCR	EQU	IO_BASE + 36H		;Refresh Control Register
Z180_CMR	EQU	IO_BASE + 1EH		;Clock Multiplier Register (for z8s180)
Z180_CCR	EQU	IO_BASE + 1FH		;CPU Control Register (for z8s180)
Z180_ICR	EQU	IO_BASE + 3FH		;CPU Control Register (for z8s180)
INITIAL_ICR	EQU 3FH					;I/O Control Register's initial address
									;  upon reset
; Z180 Memory Management Unit register addresses
Z180_CBR	EQU	IO_BASE + 38H		; MMU Common Base Register
Z180_BBR	EQU	IO_BASE + 39H		; MMU Bank Base Register
Z180_CBAR	EQU	IO_BASE + 3AH		; MMU Common/Bank Area Register
 
; Z180 internal serial port register addresses 
Z180_CNTLA0	EQU	IO_BASE + 00H	;Control Register A for serial port Chan0
Z180_CNTLB0	EQU	IO_BASE + 02H	;Control Register B for serial port Chan0
Z180_STAT0	EQU	IO_BASE + 04H	;Status Register for serial port Chan0
Z180_TDR0	EQU	IO_BASE + 06H	;Transmit Data Register for serial port Chan0
Z180_RDR0	EQU	IO_BASE + 08H	;Receive Data Register for serial port Chan0
Z180_ASEXT0	EQU	IO_BASE + 12H	;Extension Control Register for serial port Chan0

; Z180 Clocked Serial I/O Port Addresses
Z180_CNTR	EQU	0CAH	;CSI/O Control/Status Register (R/W)
Z180_TRDR	EQU	0CBH	;CSI/O Transmit/Receive Data Register
SD_CARD_CS	EQU	0CH		;Output port for the SD card adapter
						;  bit 2 is the chip select line (0=on, 1=off)
SD_CARD_LED	EQU	0EH		;Output port for the SD card LED
						;  bit 2 turns the SD card LED ON or OFF
						
;=============================================================================
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
						
;=============================================================================
; Constants
CR		EQU	0DH		;ASCII code for carriage return
LF		EQU	0AH	    ;ASCII code for line feed
TAB		EQU	09H		;ASCII code for TAB
BS		EQU 08H		;ASCII code for backspace
SPACE	EQU 20H		;ASCII code for space
NULL	EQU 00H		;ASCII code for NUL
COMMA   EQU 2CH     ;ASCII code for comma
RST6	EQU 0030H   ;software interrupt RST6 jumps to this address
MAX		EQU 7		;number of required FFs to mark EOF for LOAD command
ESC		EQU 1BH		;ASCII code for escape key
MAXTRACKS  EQU 77	;number of tracks on the disk
MAXSECTORS EQU 26	;number of sectors on the disk

; Constants for XModem
SOH		EQU	1		;a byte of 01 indicates start of header
EOT		EQU	4		;a byte of 04 indicates end of transmission
ACK		EQU	6		;Acknowledge
NAK		EQU	15H		;Not Acknowledge
CAN		EQU	24		;Cancel Xmodem transfer

;=============================================================================
; Offsets in memory area for saved registers
RSSP	EQU	00		;offset of SP in register save area
RSAF	EQU	02		;offset of AF in register save area
RSBC	EQU	04		;offset of BC in register save area
RSDE	EQU	06		;offset of DE in register save area
RSHL	EQU	08		;offset of HL in register save area
RPC		EQU	0AH		;offset of PC in register save area
RSIX	EQU	0CH		;offset of IX in register save area
RSIY	EQU	0EH		;offset of IY in register save area
RSIR	EQU	10H		;offset of IR in register save area
RSAF2	EQU	12H		;offset of AF' in register save area
RSBC2	EQU	14H		;offset of BC' in register save area
RSDE2	EQU	16H		;offset of DE' in register save area
RSHL2	EQU	18H		;offset of HL' in register save area

;=============================================================================
; Low memory locations reserved for restart from a breakpoint. CP/M doesn't
; use locations 40H to 50H
STK_TOP	EQU 0040H	;contains address of monitor's stack top
OLD_SP  EQU 0042H
IX_SAV	EQU 0044H
SP_SAV	EQU 0046H
GOADDR	EQU 0048H	;program start address used by GOTO command
BRAKPT	EQU 004AH	;breakpoint address
IBYTE	EQU 004CH	;storage for instruction at breakpoint
JMPLOC	EQU 004DH	;a jump instruction (C3H) is stored here
PC_SAV	EQU 004EH

; Variables for XModem  (CP/M does not use locations 08H to 2FH)
NXT_BYTE EQU 002CH 	;address of next byte to store
BLKNUM	 EQU 002EH	;storage for the XModem block number

MON_START	EQU	0F000h	;final location of this monitor = F000H

	ORG	0000H

;=============================================================================
;=============================================================================
; Z180 INITIALIZATION
;=============================================================================
;=============================================================================
; NOTE: Use OUT0 and IN0 to access the Z180's internal registers as specified
;       in the Zilog Z180's User Manual
	IM	 1					;set interrupt mode 1
	DI						;disable all interrupts
	
	LD	 A,IO_BASE			;change the base address for the Z180's internal
	OUT0 (INITIAL_ICR),A	;  registers from 00 (the reset value) to C0H

	XOR	 A					;disable refresh (not really required)
	OUT0 (Z180_RCR),A
	
	LD	 A,00				;disable the CPU clock multiplier
	OUT0 (Z180_CMR),A
	LD	 A,00				;set the CPU clock speed to XTAL/2 (= 9.2 MHz)
	OUT0 (Z180_CCR),A
	
	LD	 A,0F0H				;set 3 wait states for memory, 3 wait states for I/O
	OUT0 (Z180_DCNTL),A

; The code below splits the Z80 core's logical address space into a 32K
; Bank Area (logical addresses 0000-7FFF), and a 32K Common Area (logical
; addresses 8000-FFFF). Because the Bank Base Address remains at 00,
; logical addresses of 0000-7FFF are mapped to physical addresses of
; 00000-07FFF (which are in the ROM memory). Thus we continue to run from
; ROM at this point in the code.
	LD	 A,80H			;split the logical address space into a 32K Bank Area
	OUT0 (Z180_CBAR),A	; (0000-7FFF) and a 32K Common Area (8000-FFFF)
	LD	 A,00			;keep the Bank Base address at 00
	OUT0 (Z180_BBR),A		
	LD	 A,0F0H			;change the Common Base address from 00 to 80H
	OUT0 (Z180_CBR),A

; We now have RAM in the upper 32K of the Z80 core's logical address space.
; so we can move the Zap180 monitor's code into that RAM at F000-FFFF, and
; initialize the stack top to be at EFE0 (the 20H area EFE0-EFFF will be used
; for register storage by the GOTO command).  

	LD	 SP,0EFE0H		;set SP to the desired stack location

	LD	 HL,MON_IMG		;copy the monitor code from ROM into RAM at logical
	LD	 DE,MON_START	;  address F000H
	LD	 BC,MON_LEN
	LDIR
	JP	MON_START		;then jump to the monitor!

	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0  ;clean up PROM for easier
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	 ;reading of the Hex file
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	
;=============================================================================
;=============================================================================
; START OF MONITOR CODE
;=============================================================================
;=============================================================================
MON_IMG	EQU	$			;the monitor image starts here
	.phase	MON_START	;it is also the start location for the monitor code

	JP BEGIN	    ;go around I/O vectors
	 
jSTART:	 JP START	;jump to start of main work loop
jSP_RET: JP SP_RET	;restore SP and jump to start of work loop
jERROR:	 JP ERROR	;print *,restore SP, jump back to main work loop
jCI:  	 JP CI	    ;jump to console input routine
jCO:  	 JP CO		;jump to console output routine
jCSTS:	 JP CSTS	;jump to console status routine	
jPRINT:	 JP PRINT	;display message on console. HL=msg address,'$'=msg end
jPRINTI: JP PRINTI	;display message given in next line on console,'$'=msg end
jHLPRNT: JP HLPRNT	;display (HL) on console
jAPRNT:  JP APRNT	;display the byte in the A register
jCRLF:	 JP CRLF	;send a CRLF to the console
jHEXSP:	 JP HEXSP	;put hex values from console on the stack, C = # of values
jMS_DLY: JP MS_DELAY ;millisecond software delay routine, delay length in A
jSAVREG: JP SAVREG	;save all Z80 registers and return to Zapple

BEGIN:
	LD	 HL,STACK_TOP	;save the stack top address in IX
	PUSH HL
	POP	 IX				;IX must always be preserved!
	LD	 BC,20H			;clear register storage area
	LD	 A,00
CLR: LD	 (HL),A
	INC	 HL
	DJNZ CLR

	CALL INIT			;initialize internal serial port A
	CALL PRINTI			;output sign-on msg
	DEFB	'Z180 Monitor v2.0$'
	CALL PRINTI			;print current stack location
	DEFB	CR,LF,'SP=$'
	PUSH IX				;restore stack pointer in HL
	POP	 HL
	CALL HLPRNT			;print contents of HL
	CALL CRLF			;then CRLF
	CALL CSTS			;check if garbage at keyboard
	CALL NZ,CI			;if so flush it

; The code below changes the Bank Base address to 80H. This causes logical
; addresses of 0000-7FFF to be mapped to physical addresses of
; 80000-87FFF (which are in the RAM memory). Thus the Z180's entire logical
; address space of 0000-FFFF will now be mapped to the RAM memory physical
; address space of 80000-8FFFF, and all code in the ROM chip will now
; become inaccessible.
	LD	 A,80H			;change the Bank Base address to 80H by writing to
	OUT0 (Z180_BBR),A	;  the Bank Base Register

; Put a copy of the stack top address into low memory
	LD	 (STK_TOP),IX
	
	CALL INIT_SD_CARD	;initialize the SD card

;==============================================================================	
;========================== START OF MAIN MONITOR LOOP- ======================
START: LD DE,START	;start of the main 'work' loop
	PUSH DE	     	;set up a return to START. Note that an extra unbalanced
	CALL CRLF		;  pop will put [DE] in PC
	LD	 C,'>'
	CALL CO
	
ST1: CALL TI	 	;get a console character
	AND	 7FH	    ;ignore nulls
	JR	 Z,ST1
	
	SUB	'A'			;ignore characters below A and above Z
	RET	M
	CP	'Z'-'A'+1
	RET	NC
	ADD	A,A		    ;multiply A by 2
	LD	C,A
	LD	B,00
	LD	HL,TBL		;point to start of command branch table
	ADD	HL,BC		;add in offset into table
	LD	A,(HL)
	INC	HL
	LD	H,(HL)
	LD	L,A
	JP	 (HL)	    ;go execute the command

;==============================================================================
;============================= COMMAND BRANCH TABLE ==========================
TBL:	
	DEFW	ERROR	    ;A - ASSIGN I/O DEVICE
	DEFW	BOOT	    ;B - BOOT SYSTEM INTO CP/M 2.2
	DEFW	ERROR	    ;C - CHECK RAM MEMORY FOR ERRORS
	DEFW	DISP	    ;D - DISPLAY MEMORY ON CONS. IN HEX
	DEFW	ERROR	    ;E - END OF FILE TAG FOR HEX DUMPS
	DEFW	FILL	    ;F - FILL MEMORY WITH A CONSTANT
	DEFW	GOTO	    ;G - GOTO [ADDR]<,>BREAKPOINTS (2)
	DEFW	HEXLOAD	    ;H - HEX MATH. <SUM>,<DIFFERENCE>
	DEFW	ERROR		;I - USER DEFINED, INSERT VECTOR
	DEFW	ERROR	    ;J - NON-DESTRUCTIVE MEMORY TEST
	DEFW	ERROR       ;K - USER DEFINED, INSERT VECTOR
	DEFW	LOAD	    ;L - LOAD A BINARY FILE INTO MEMORY
	DEFW	MOVE	    ;M - MOVE BLOCKS OF MEMORY
	DEFW	ERROR	    ;N - PUNCH NULLS ON PUNCH DEVICE
	DEFW	ERROR	    ;O - USER DEFINED, INSERT VECTOR
	DEFW	PUTSYS	    ;P - PUT NEW CP/M 2.2 IMAGE ONTO SD CARD
	DEFW	QUERY	    ;Q - QI(N)=DISP. N; QO(N,V)=OUT N,V
	DEFW	REGDISP	    ;R - DISPLAY CPU REGISTERS
	DEFW	SUBS	    ;S - SUBSTITUTE &/OR EXAMINE MEMORY
	DEFW	TYPE	    ;T - TYPE MEMORY AS ASCII TEXT
	DEFW	USER	    ;U - VECTOR TO USER PROGRAM AT ADDRESS 100H
	DEFW	VERIFY	    ;V - COMPARE MEMORY AGAINST MEMORY
	DEFW	WHERE	    ;W - FIND SEQUENCE OF BYTES IN RAM
	DEFW	XMODEM	    ;X - READ A FILE FROM A PC USING XMODEM
	DEFW	ERROR	    ;Y - USER DEFINED, INSERT VECTOR
	DEFW	ERROR	    ;Z - PRINT ADDRESS OF HIGHEST R/W LOCATION

;==============================================================================
;======================= START OF SYSTEM DEPENDENT ROUTINES ===================
;---------------------------------------------------------------
; INIT -  Configure the Z180 internal serial port channel 0
;	Entry: none
;   Exit:  registers A and B modified
;---------------------------------------------------------------
INIT:
	LD	 A,64H				;enable xmitter and receiver, set 8 bits, no parity
	OUT0 (Z180_CNTLA0),A
	LD	 A,BAUD_RATE		;if BAUD_RATE = 00, Baud rate will be 57,600 
	OUT0 (Z180_CNTLB0),A	;if BAUD_RATE = 20H, Baud rate will be 19,200
	LD	 A,60H				;disable DCD and CTS for serial port 0
	OUT0 (Z180_ASEXT0),A 
	LD	 B,00				;a delay is required after configuring the serial port
	DJNZ $					;before actually trying to send data. 
	DJNZ $
	RET

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

;=============================================================================
;============================== Z180 MONITOR COMMANDS =========================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BOOT COMMAND - Boots the computer into CP/M 2.2 by reading track 0,
;	sectors 2 - 26, track 1, sectors 1 - 26, and track 2, sectors 1 -5
;	into memory at the	appropriate location, e.g. 6400H for a 32K system.
;	system. It then jumps to the BIOS cold start entry.
;	USAGE: B<addr>[CR]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
BOOT:
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
	LD	 A,01			;now read track 1
	LD	 (track),A
	LD	 A,01			;start with sector 1 for track 1
	LD	 (sector),A
	
RD_TRK1:
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
	JP COLD_BOOT		;jump to BIOS cold start entry

NEXT_DMA:
	LD	 HL,(DMAaddr)	;increment the memory address
	LD	 DE,128
	ADD	 HL,DE
	LD	 (DMAaddr),HL
	RET

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DISPLAY COMMAND - displays the contents of memory from <addr1> to <addr2>
;	in hex with the starting location displayed at the beginning of each line.
;   USAGE: D<addr1> <addr2>[CR]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DISP: CALL GET2HEX		;get parameters in HL and DE
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
	CALL HILOX		;test for end of range, return to START if range exceeded
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
; ERROR COMMAND - prints a '*' to announce an error, then restores the stack
;	pointer to its startup value and jumps to START.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ERROR: LD HL,MON_START	; put top of user RAM in HL
	LD	BC,1FH			;subtract size of register storage area
	SBC	 HL,BC
	LD   SP,HL			;set SP to RAMtop minus register storage (i.e. xxE0)
	LD	 C,'*'	   		;announce error
	CALL CO
	JP	 START	    	;go back to work

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILL COMMAND - fills a memory block from <addr1> to <addr2> with a byte value.
;   USAGE: F<addr1> <addr2> <byte>[CR]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FILL:	CALL GET3HEX	;get 3 parameters
FIL0:	LD	 (HL),C		;store the byte
		CALL HILO		;increment pointer and see if done
		JR	 NC,FIL0
		POP	 DE			;restore stack in case it got bombed
		JP	 START
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GOTO COMMAND - executes a program with or without a breakpoint. When this
;   command is executed, SP points to the address of START, so a RET at the end
;   of the program being executed will return to there.
;   USAGE: 	G[CR]
;			G<start addr>[CR]
;			G<start addr> <brkpt addr>[CR]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

GOTO: CALL TI			;get a character in A
	CALL QCHK			;return 0 if char is space or comma, carry set if CR
    LD   E,A            ;save the character in E
	JP	 C,RESTART    	;restart program if only CR entered
	JR	 Z,BRKPT    	;comma entered, get breakpoint
	CALL EXF	    	;get start address on the stack 
	POP	 HL				;and put it in HL
	LD	 (GOADDR),HL	;store the start address in GOADDR
	LD	 A,B			;last char typed after address is in B
	CP	 CR	     		;if last character was a CR,all done
	JR	 NZ,BRKPT       ;else get breakpoint address
	JP (HL) 			;execute the program, no breakpoint
	
BRKPT: CALL HEXSP1		;get breakpoint address on stack
	POP	 HL	     		;put breakpoint address in HL
	LD	 (BRAKPT),HL	;store the breakpoint address in BRAKPT
	LD	 A,H     		;make sure breakpoint address is not 0
	OR	 L
	JR	 NZ,OK
	JP	 ERROR			;error if address is 0
OK: LD	 A,(HL)	     	;pick up instruction byte at breakpoint
	LD	 (IBYTE),A		;store it in IBYTE
	LD	 A,0F7H	     	;replace instruction byte with RST6 software interrupt
	LD	 (HL),A

    LD	 A,0C3H			;set a jump to SAVREG when RST6 occurs
	LD	 (RST6),A
	LD	 HL,SAVREG
	LD	 (RST6+1),HL

    LD   A,E            ;was comma entered as first character?
    CP   COMMA
    JP   Z,RESTART      ;yes, start from stored PC
	LD	HL,(GOADDR)	    ;else get start address from GOADDR
	JP	(HL)			;execute the program with breakpoint set

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; HEX COMMAND - Loads an Intel Hex file into memory at the load address
;	specified in the file.
;	USAGE: H[CR}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
HEXLOAD: JP 0F806H

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LOAD COMMAND - loads a binary file into memory. The file must start
;   with four consecutive FFs, and end with at least seven FFs in a row.
;   USAGE: L<addr>[CR]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LOAD: 	CALL HEXSP1		;get a beginning load address
		POP	 HL
		CALL CRLF
		LD	 D,0FFH		;start-of-file tag
LOAD0:	LD	 B,04		;find at least four consecutive FFs
LOAD1:	CALL CI
		CP	 D			;does input byte = FF?
		JR	 NZ,LOAD0
		DJNZ LOAD1
LOAD2:	CALL CI			;four found, now wait for non-FF
		CP	 D
		JR	 Z,LOAD2
		LD	 (HL),A		;first real data byte
LOAD3:	INC	 HL
		CALL CI			;get next byte
		CP	 D
		JR	Z,ELOAD		;if byte=0FFH, possible end of file
		LD	 (HL),A
		JR	 LOAD3		;else keep going
ELOAD:	LD	 E,1		;initialize count of FFs
ELOAD0:	CALL CI
		CP	 D			;is next byte 0FFH also?
		JR	 NZ,ELOAD1  ;no, store previous FF and this byte also
		INC	 E			;increment count of consecutive FFs
		LD	 A,MAX		;look for EOF
		CP	 E			;found max?
		JR	 NZ,ELOAD0	;nope
		CALL PRINTI
		DEFB 'End=$'
		CALL HLPRNT		;yep, print end address
		RET				;go back to Zapple
ELOAD1:	LD	 (HL),D
		INC	 HL
		DEC	 E			;restore count
		JR	 NZ,ELOAD1
		LD	 (HL),A		;real byte
		JR LOAD3
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MOVE COMMAND - moves a block of memory from <addr1> thru <addr2> to the
;	the address starting at <addr3>.  
;   USAGE: M<start addr> <end addr> <destination start addr>[CR]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MOVE: CALL GET3HEX	;get 3 parameters, start address in HL
MO1:  LD   A,(HL)	;move one byte to destination address given by BC
	  LD   (BC),A
	  INC  BC	    ;increment destination pointer
	  CALL HILOX	;increment source addr (HL) and see if end addr (DE) exceeded
	  JR   MO1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PUTSYS COMMAND - Writes a memory image of CP/M 2.2 onto the SD card.
;	The total size of CP/M 2.2 with a full length BIOS is 1C00H bytes,
;	so we need to write 56 128-byte sectors onto the drive. Track 0 sector 1
;	is reserved for a boot loader, so we store the CP/M code on track 0
;	sectors 2 - 26, on track 1 sectors 1 - 26, and track 2 sectors 1 - 5.
;	The starting address of the memory image is at <addr>.
;	<addr> = 6400H for a 32K system.
;	USAGE: P<addr>[CR]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PUTSYS:
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
	CALL WRITE
	LD A,(sector)		;set the sector to write
	CP	 5				;last sector reached?
	JP	 Z,WR_DONE		;yes, all done
	INC	 A				;no, go to next sector of track 2
	LD	 (sector),A
	CALL NEXT_DMA		;increment memory address for next sector to write
	JP	 WR2

WR_DONE:
	LD	 HL,CPM_WRTmsg
	CALL PRINT	
	RET





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; QUERY I/O PORT COMMAND - reads a byte from an input port and displays it,
;   or sends a byte to an output port.
;	USAGE: QO<port>,<byte>[CR]   or   QI<port>[CR]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
QUERY: CALL TI		;get next character
	CP	 'O'
	JR	 Z,QOUT		;do output operation if 'O'
	CP	 'I'		;do input operation if 'I'
	JP	 NZ,ERROR	;else it's an error
	CALL HEXSP1		;get input port number
	POP	 BC
	IN	 E,(C)		;input a byte
BITS: LD	 B,8    ;display it as a binary number
	CALL PSPACE		;space over
QUE2: SLA	 E		;shift a bit into CY
	LD	 A,18H		;load ASCII '0' divided by 2
	ADC	 A,A	    ;make into '0' or '1'
	LD	 C,A		;print it
	CALL CO
	DJNZ QUE2
	RET
	
QOUT: CALL HEXSP2	;get output port number and byte to output
	POP	 DE
	POP	 BC
	OUT	 (C),E		;output it
	RET
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; REGDISP COMMAND - Displays the contents of all Z80 registers on the console.
;	USAGE: R[CR}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
REGDISP: LD  IX,(STK_TOP)
	LD   B,13		;display contents of all 13 registers
REG_LP:	LD	 HL,REG_ORDR
	LD	 A,B
	DEC	 A			;register # is in A
	CALL ADD_HL_A 	;pick up an offset from REGORDER
	LD	 A,(HL)
	OR	 A			;set flags. preserve offset in A in subsequent code
	CALL M,NEW_LINE
	AND	 0FH
	CALL GET_NAME 	;get address of reg name in HL
	PUSH AF			;print reg name, preserve A
	CALL PRINT
	POP	 AF
	CALL PRINT_REG	;print contents of register
	CALL PRN_NXT
	DEFB '  ','$'
	DJNZ REG_LP
	CALL SP_RET		    ;all done
	
; Start a new line on the console. The A register is preserved 
NEW_LINE: CALL PRN_NXT
	DEFB CR,LF,'$'
	RET		
; Get address of the register name in HL
GET_NAME: PUSH AF
	RLCA
	RLCA
	LD	 HL,NAMES
	CALL ADD_HL_A
	POP	 AF
	RET
; Print register contents
PRINT_REG: CALL	PRN_NXT
	DEFB '=','$'
	LD	 HL,(STK_TOP)	;get start of saved register area in HL
	RLCA				;multiply value in A by 2
	CALL ADD_HL_A		;HL = where to find Register
	CALL LD_HL_HL		;HL = (HL)				
	CALL HLPRNT     	;print HL on the console
    LD   C,' '	    	;followed by a space
	CALL CO
	RET
; Print the string given in the next line of code
PRN_NXT: EX	 (SP),HL ;put address of next instruction in HL
	CALL PRNT
	EX	 (SP),HL	 ;move updated return address back to stack
	RET
PRNT: PUSH AF
	CALL PRINT
	POP	 AF
	RET		
; Add the value in A to HL
ADD_HL_A: ADD A,L
	LD  L,A
	RET NC
	INC H
	RET
; Load contents of location pointed to by HL into HL
LD_HL_HL: LD A,(HL)
	INC	HL
	LD  H,(HL)
	LD  L,A
	RET
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SUBSTITUTE COMMAND - allows examination and modification of memory on a byte
;	by byte basis. It takes one address parameter,followed by a space. The data
;   at that location will be displayed. To change it, a new value is entered,
;   and a following space displays the next byte. A CR terminates the command.
;   A Backspace backs up the pointer and displays the previous location.
;   USAGE: S<addr><SP>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SUBS:	CALL HEXSP1		;get the starting address
		POP	 HL
SUB0:	LD	 A,(HL)		;get a byte from memory
		CALL APRNT	    ;display it
		CALL COPCK	    ;modify it?
		RET	 C	     	; no, all done
		JR	 Z,SUB1	    ;don't modify, skip ahead
		CP	 BS	     	;backup one byte?
		JR	 Z,SUB2		;yes
		PUSH HL	     	;else save pointer
		CALL EXF	    ;get new value
		POP	 DE	     	;value is in E
		POP	 HL			;restore HL
		LD	(HL),E	    ;modify memory
		LD	 A,B	    ;test for delimiter
		CP	 CR
		RET	 Z	     	;done if CR
SUB1:	INC	 HL			;next byte
SUB3:	LD	 A,L	    ;8 bytes on this line yet?
		AND	 07H
		CALL Z,LFADR	;yes, start new line
		JR	 SUB0
SUB2:	DEC	 HL	     	;decrement pointer
		JR	 SUB3	    ;and print data there

COPCK: 	LD	C,'-'	     ;print the prompt for the SUBST command
		CALL CO	
PCHK:  	CALL TI			;get a character in A
		CALL QCHK		;return 0 if A = space or comma, carry set if CR
		RET

LFADR: CALL	CRLF	;CRLF before HLSP
	CALL HLPRNT
	LD   C,' '	;print a space on the console
	JP   CO
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TYPE COMMAND - Displays the contents of a block of memory as ASCII text
;	USAGE: T<addr1> <addr2>[CR]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
TYPE: LD C,02		;get 2 parameters
	CALL HEXSP
	POP	 DE			;put 2nd param in DE
	POP	 HL			;and 1st param in HL
TYP1: CALL CRLF
	CALL HLPRNT		;print HL and a space
	LD	 C,SPACE
	CALL CO
	LD	 B,60
TYP2: LD A,(HL)		;get a character
	AND	7FH			;mask off high bit
	CP	' '			;is it a control code?
	JR	NC,TYP4
TYP3: LD A,'.'		;change char to a period if a Control code
TYP4: CP 7EH		;or if the DEL character
	JR	 NC,TYP3
	LD	 C,A
	CALL CO
	CALL HILOX		;quit if end of range reached 
	DJNZ TYP2		;start new line when B = 0
	JR	 TYP1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; USER COMMAND - This jumps to any user program that starts at location 0100H
;	USAGE:	U[CR}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
USER: JP  0100H

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; VERIFY COMMAND - Verifies that the contents of one memory block are identical
;   to another block of memory. 
;   USAGE: V<start addr> <end addr> <start addr of 2nd memory block>[CR]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
VERIFY:	CALL GET3HEX ;get 3 parameters, start address in HL
VERI0:	LD	 A,(BC)	;get a byte of 2nd memory block (BC)
	CP	 (HL)		;compare with byte in 1st block
	JR	 Z,VERI1	;continue if OK
	PUSH BC			;else display address of error
	CALL CERR	
	POP	 BC
VERI1: INC	BC		;increment 2nd block address
	CALL HILOX		;inc 1st block addr (HL),see if end addr (DE) exceeded
	JR	 VERI0

; Display the current location pointed to by (HL), the value at that
; location, and the contents of the accumulator.
CERR: LD	B,A	     ;save A
	CALL HLPRNT	     ;display HL
	CALL PSPACE
	LD	 A,(HL)
	CALL APRNT	     ;print contents of location pointed to by HL
	CALL PSPACE	     ;space over
	LD	 A,B
	CALL APRNT	     ;print the accumulator
	JP	 CRLF	     ;CRLF and return
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; WHERE COMMAND - searches memory for a spcified sequence of bytes.
;	As many bytes as desired may be entered, separated by commas. The entire
;	memory is searched starting from 0000, and all starting addresses of each
;	occurance of the search string are printed on the console.
;   USAGE: W<byte1>,<byte2>,<byte3>,...[CR]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
WHERE: LD	D,0		;search string byte count
WHER0: CALL	HEXSP1	;get one byte on the stack
	POP	 HL	     	;pick it up
	LD	 H,L	    ;stick it in the high byte of HL
	PUSH HL	     	;put it on the stack
	INC	 SP	     	;adjust stack
	INC	 D	     	;increment byte count
	LD	 A,B	    ;test delimiter
	SUB	 CR
	JR	 NZ,WHER0	;more to go
	LD	 B,A	    ;load BC and H with zeros
	LD	 C,A
	LD	 H,A
	LD	 L,D    	;get byte count in L
	DEC	 L	     	;-1
	ADD	 HL,SP	    ;bytes stored in stack
	PUSH HL
	PUSH BC
FINDC: PUSH	BC	    ;save that pointer
	CALL CRLF
	POP	 BC	     	;restore
FIND: POP HL	    ;HL=search address
	POP	 IX	     	;X=search byte pointer
	LD	 E,D	    ;reset counter
	LD	 A,(IX+0)   ;get the first search byte
	CPIR		    ;compare, increment, and repeat
	JP	 PO,DONE2	;odd parity=done
	PUSH IX	     	;save pointers
	PUSH HL
FOUND: DEC E
	JR	 Z,TELL	    ;found all
	LD	 A,(IX+-1)  ;look at next match
	CP	 (HL)	    ;test next
	JR	 NZ,FIND	;no match
	INC	 HL	     	;bump pointers
	DEC	 IX
	JR	 FOUND	    ;test next match
TELL: POP HL
	PUSH HL
	DEC	 HL
	PUSH BC	  		;save search count limit
	CALL HLPRNT	    ;tell console
	POP	 BC	     	;restore
	JR	 FINDC
DONE2: INC SP
	DEC	 E	     	;reset stack
	JR	 NZ,DONE2
	RET
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; XMODEM COMMAND - Reads a file sent via a serial port from a PC terminal
;	program like TeraTerm  and places it in RAM at a specified location.
;	The file must be sent via the XModem protocol.
;   USAGE: X<addr>[CR]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
XMODEM:	LD	 C,01			;get a beginning load address
		CALL HEXSP
		POP	 HL
		CALL CRLF
		LD	 (NXT_BYTE),HL	;save the load address
		LD	 HL,SIGNON		;print the signon message
		CALL PRINT
		CALL CRLF
		LD	 A,01			;initialize the block number
		LD	 (BLKNUM),A
		LD	 HL,WAITMSG		;waiting for XModem program to start
		CALL PRINT
		LD	 A,40			;make sure line is clear for 20 seconds		
		CALL GET_BYTE		;while XModem program on PC is being started
		JR	 NC,CANCEL		;abort if garbage character received
		LD	 HL,STARTMSG
		CALL PRINT
		CALL XM_INIT

XM_LP:  CALL XM_RECV		;receive the next packet
		JR	 XM_LP			;loop until EOT Received		
		
;-------------------------------------------------------------------------
;XM_INIT -Tell PC to start an XModem transfer and receive first packet		
XM_INIT:	LD	 A,01			;initialize the block number
		LD	 (BLKNUM),A
		LD	 A,NAK			;send a NAK to terminal to start the transfer
		CALL PUT_CHAR
RECV_LP: CALL GET_HDR		;get the packet header
		JR 	 GET_DATA		;header good, get rest of packet
	
;--------------------- XMODEM RECEIVE --------------------------
;Entry:	XM_INIT jumps to GET_DATA in the middle of the routine
;		NXT_BYTE = next memory location to write the file to
;		BLKNUM = current block number
;------------------------------------
XM_RECV: LD A,ACK			;send ACK to start receiving next packet
		CALL PUT_CHAR
		CALL GET_HDR
		
		
GET_DATA: LD C,A			;put block # in C
		LD	 A,(BLKNUM)		;compare with expected block number #
		CP	 C
		JR	 Z,BLKNUM_OK	;get rest of packet if block # is correct
		JP	 CANCEL			;cancel if block # isn't correct

BLKNUM_OK:	LD B,128		;128 bytes per block
		LD	 C,0			;clear the checksum
		LD	 HL,(NXT_BYTE)	;save HL, has the address where block is to go
BLK_LP:	CALL CI				;get a data byte
		LD	 (HL),A			;store it in memory
		LD	 A,(HL)			;update the checksum
		ADD	 A,C
		LD	 C,A
		INC	 HL				;advance to next memory location
		DEC	 B				;decrement # of bytes left in packet
		JR   NZ,BLK_LP		;some bytes left, go back for more
		LD	 (NXT_BYTE),HL	;update RAM storage address
		CALL CI				;end of packet reached, get checksum
		CP	 C
		JR	 NZ,CANCEL		;abort if checksum not valid
		
		;If we were transfering to a FILE, this is where we would write the
		;sector and reset HL to the same 128 byte sector buffer.
		;CALL	WRITE_SECTOR
		
		LD	 A,(BLKNUM)		;advance to next packet
		INC	 A
		LD	 (BLKNUM),A
		RET				
						
;---------------------------------------------------------------
;GET_HDR - Get a valid packet header
;	Exit: CY clear and A = B = block # if valid header found
;		  Jump to GOT_EOT if end of text character received
;		  Abort if neither SOH or EOT received
GET_HDR: CALL CI		;test first byte in header
		CP	 SOH
		JR	 Z,GET_BLK	;if SOH received, get block # next
		CP	 EOT
		SCF
		JR	 Z,GOT_EOT	;if EOT received, transfer complete
		JP	 CANCEL		;else abort
		
GET_BLK: CALL CI		;get the block #
		LD	 B,A		;save it in B
		CALL CI			;get the complement of the block #
		CPL
		CP	 B			;is the block # valid?
		JP	 NZ,CANCEL	;no, abort
		RET						
						
						
;---------------------------------------------------------------
;CANCEL - Cancel transfer and abort on all errors
CANCEL: LD A,CAN
		CALL PUT_CHAR
		CALL PUT_CHAR
		CALL PURGE
		LD	 HL,CANCELMSG
		CALL PRINT
		JP	ERROR						

GOT_EOT: LD A,NAK		;NAK the EOT
	CALL PUT_CHAR
	CALL CI				;wait for 2nd EOT
	CP	 EOT
	JR	 Z,FINISH
	CALL CANCEL
	
FINISH: LD A,ACK		;ACK the 2nd EOT
	CALL PUT_CHAR
	LD	 HL,FINISHMSG
	CALL PRINT
	JP	 SP_RET					

;============================== END OF COMMANDS ===============================

;==============================================================================
;====================== SD CARD INITIALIZATION ROUTINES =======================

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
	CALL GET_TYPE			;determine whether the SD card is a Type 2
	LD	 HL,CF_TYPE_ERRmsg	;a Type 2 SD Card was NOT detected
	CALL NZ,CMD_FAIL		;show error message and abort the initialization
	RET	 NZ
	CALL TYPE_2_ACTIVATE	;Type 2 SD cards need special activation
	LD	 HL,CMD55_FAILmsg	;if SD Card activation failed (CMD55 or ACMD41)
	CALL NZ,CMD_FAIL_RESP	;display error message
	RET	 NZ					;and abort
	LD	 HL,INIT_OKmsg		;else show initialization success
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
	RET						;and abort

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

;==============================================================================
;======================= SD CARD READ/WRITE ROUTINES ==========================
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
	CALL MS_DELAY
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
;============================== SPI COMMANDS ==================================
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
;========================= CONSOLE OUTPUT ROUTINES ============================
	
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
; PRINTI - Prints a '$'-terminated string given in the next line of code
;	on the console.
;   Registers A,C,HL modified
PRINTI:	EX	 (SP),HL	;put address of next instruction in HL
		CALL PRINT
		EX	 (SP),HL	;Move updated return address back to stack
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
; PUT_CHAR - Output a character to the console
;		A = character to send
;		All other registers preserved
PUT_CHAR: PUSH AF
	PUSH BC		;Save registers
	LD	C,A
	CALL CO
	POP BC
	POP AF
	RET

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
; PSPACE - Prints a space on the console
PSPACE:  LD   C,' '	;print a space on the console
	  JP   CO
	  
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
;=========================== CONSOLE INPUT ROUTINES ===========================
;------------------------------------------------------------------------------
; TI - This is the keyboard input handling routine. It converts lower case to
;	upper case, ignores CR's, and aborts if ctrl-C is entered
;   All other characters are echoed as they are received.
TI:	CALL CI	;get a character
	CP	 CR		;ignore if CR
	RET	 Z
	CP	'C'-40H	;abort if ctrl-C entered 
	JP	Z,SP_RET	
	CP	 'A'-1	;ASCII code is less than 'A', just echo it
	JR	 C,ECHO
	CP	 'z'+1	;ASCII code is greater than 'z', just echo it
	JR	 NC,ECHO
	AND	 5FH	;ASCII code in range 'A'-'z", convert to upper case 
ECHO: PUSH BC
	LD	 C,A
	CALL CO
	LD	 A,C
	POP	 BC
	RET

HEXSP1:	LD	C,01	;get 1 parameter from console and put it on the stack
		JP HEXSP
		
HEXSP2:	LD	C,02	;get 2 parameters from console and put them on the stack
		JP HEXSP

;------------------------------------------------------------------------------
; HEXSP - This is the main "parameter-getting" routine. It takes hex values
;	entered at the console, separated by a space or comma, and places them
;	on the stack as 16-bit binary values.
;	On entry the C register must contain the number of hex values expected.
;	If a carriage return is entered instead of an expected hex value, it places
;	a 0000 on the stack. Entering a non-hex character causes HEXSP to abort.
HEXSP: LD   HL,0	 ;initialize HL to zero
EX0:  CALL TI	     ;get something from console
EX1:  LD   B,A	     ;save it in B
	CALL ASC2HEX	     ;convert ascii to hex
	JR	 C,EX2	     ;illegal character dectected if carry set
	ADD	 HL,HL	     ;multiply by 16
	ADD	 HL,HL
	ADD	 HL,HL
	ADD	 HL,HL
	OR	 L	     	 ;or in a nibble
	LD	 L,A
	JR	 EX0	     ;get more nibbles
EX2: EX	(SP),HL	     ;save on the stack
	PUSH HL	     	 ;replace the return
	LD	 A,B	     ;test the delimiter
	CALL QCHK
	JR	 NC,EX3	     ;jump if CR entered
	DEC	 C	     	;should go to zero
	RET	 Z	     	;return if it does
EX3: JP	 NZ,ERROR   ;something wrong
	DEC	 C	     	;do this again?
	JR	NZ,HEXSP	    ;yes
	RET		     	;else return
EXF: LD	 C,1
	LD	 HL,0
	JR	 EX1

;------------------------------------------------------------------------------
; ASC2HEX - Qualify the ASCII character in A as representing a valid hex digit,
;   and convert it to hexadecimal. Returns with the carry flag set if it is not
;   a hex digit (0 thru F)
ASC2HEX:	SUB	'0'	    ;qualify the character
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
; GET2HEX - get two parameters from the console, place them in DE & HL,
;	and then CRLF
GET2HEX: LD	C,02
	CALL HEXSP
	POP	DE		;put 2nd param in DE
	POP	HL		;and 1st param in HL
	JP	CRLF
	
;------------------------------------------------------------------------------	
; GET3HEX - Gets 3 parameters from the console, places the 1st parameter in HL,
;   the 2nd in DE, and the 3rd in BC
GET3HEX:	LD C,03
	CALL	HEXSP
	CALL	CRLF
	POP	BC
	POP	DE
	POP	HL
	RET
	
;------------------------------------------------------------------------------
; GET_BYTE - Gets a byte within a time limit
;  Entry: A contains # of 1/2 seconds to wait before returning
;  Exit:  CY=1, No Char (Time Out)
;		  CY=0, A = Char
GET_BYTE: PUSH	DE
		PUSH BC
		LD	 D,A		;put # of 1/2 seconds into D
GET1:	LD	 BC,25		;inner loop count down until timeout
GET2:	CALL CHK_BYTE	;see if a data byte is available
		JP	 NC,GOT_BYTE
		DJNZ GET2
		DEC	 C
		JR   NZ,GET2
		DEC	 D
		JR   NZ,GET1
		SCF				;carry set to indicate timeout
GOT_BYTE: POP BC
		POP	DE
		RET

;==============================================================================	
;============================== SUPPORT ROUTINES ==============================
;------------------------------------------------------------------------------
; MS_DELAY - delay for the specified number of milliseconds.
;	On entry: A = number of millisec to delay
;	On exit:  A = 0
MS_DELAY:
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
; HILOX - Tests for end address of a range. The Carry flag is set if the range
;   has been exceeded. Used by the DISPLAY, MOVE, TEST, and VERIFY commands.
HILOX: CALL	HILO
	RET	 NC	     ;done, address is within range
	POP	 DE	     ;else go one level back on the stack
	RET			;
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
; SP_RET - This routine restores IX and SP to their startup values and jumps to
;   the main monitor work loop.
SP_RET: LD	 IX,(STK_TOP)
		LD SP,IX
		JP	 START	    ;go back to work

;---------------------------------------------------------------
;PURGE - Clears all incoming bytes until the serial input line
;  is clear for 2 seconds
PURGE: LD	A,4		;2 seconds for time out
	CALL GET_BYTE
	JR	 NC,PURGE
	RET

;---------------------------------------------------------------
; CHK_BYTE - Check if a data byte is available, input the byte
;	if one is available
;		Exit: CY=0, A = data byte
;			  CY=1, no data byte available
;		All other registers unchanged
CHK_BYTE: CALL CSTS
		JR	 Z,NO_CHAR
		CALL CI
		CCF
		RET
NO_CHAR: SCF
		RET
	
;==============================================================================
; SAVREG - This routine saves all the Z80 registers in the register storage
;   area, restores the code byte at the breakpoint, and then jumps to the
;	register display routine. 
SAVREG: LD	 (IX_SAV),IX	;save current IX
		LD	 IX,(STK_TOP)	;get stack top into IX			
		LD	 (IX+RSHL),L	;save HL in register storage area
		LD	 (IX+RSHL+1),H
		POP	 HL				;get PC from address put on stack by RST6
        DEC  HL             ;adjust it
		LD	 (PC_SAV),HL	;save PC in low memory
		LD	 (IX+RPC),L		;and in register storage area
		LD	 (IX+RPC+1),H
		LD	 (SP_SAV),SP	;and save SP in low memory
		LD	 HL,(SP_SAV)	;and in register storage area
		LD	 (IX+RSSP),L
		LD	 (IX+RSSP+1),H

        LD   SP,RSDE+2      ;put register storage offset into SP
        ADD  IX,SP          ;add offset into IX
        LD   SP,IX          ;and put result into SP
	    PUSH	DE
		PUSH	BC
		PUSH	AF
	
		EX	 AF,AF'		    ;save alternate register set	
		EXX
        LD   IX,(STK_TOP)   ;get start of reg storage in IX
        LD   SP,RSHL2+2     ;put register storage offset into SP
        ADD  IX,SP          ;add offset into IX
        LD   SP,IX          ;and put result into SP
		PUSH HL             ;save alternate registers
		PUSH DE
		PUSH BC
		PUSH AF
		EX	 AF,AF'
		EXX				;back to original register set
		LD	 A,I		;fetch IR
		LD	 B,A
		LD	 A,R
		LD	 C,A
		PUSH BC			;save IR
		
		LD 	 BC,RSIR	;set stack to save IX and IY
		LD	 HL,(STK_TOP)
		ADD	 HL,BC
		LD	 SP,HL
		LD	 IX,(IX_SAV) ;get original IX, IY is unchanged
		PUSH IY			 ;save them in register storage area
		PUSH IX
		LD   SP,(SP_SAV) ;all registers saved

        CALL PRINTI
        DEFB CR,LF,'Break @','$'
        LD   HL,(BRAKPT)    ;restore instruction byte at breakpoint
        LD   A,(IBYTE)
        LD  (HL),A
        CALL HLPRNT        ;print breakpoint address
        JP REGDISP       ;and display registers
	
;==============================================================================
; RESTART - This routine is executed when a program halted by a breakpoint is
; restarted using the G,<breakpt>[CR] or the G[CR} command. IR and the
; alternate register set are not restored in this version
RESTART: LD  A,0C3H
        LD   (JMPLOC),A
        LD	 HL,(STK_TOP)
        LD   BC,0002
        ADD  HL,BC
		LD	 SP,HL      ;set SP to point at RSAF in register storage area
		POP	 AF			;restore the registers
		POP	 BC
        POP  DE
        POP  HL
        POP  IX         ;put saved PC into the jump instruction at RST6
        LD   (JMPLOC+1),IX
        POP  IX
        POP  IY
		LD	 SP,(SP_SAV) ;restore SP
        JP   JMPLOC      ;and start code execution at the stored PC location		
		
;------------------------------------------------------------------------------
; Table of offsets used by the register display routine
REG_ORDR DEFB	0		;Registers to dump (Numbers shifted left)
		DEFB	5		;MSB will indicate a NEW LINE
		DEFB	8
		DEFB	7
		DEFB	6 + 80H
		DEFB	12
		DEFB	11
		DEFB	10
		DEFB	9 + 80H
		DEFB	4
		DEFB	3
		DEFB	2
		DEFB	1 + 80H	;First Register to Dump

;------------------------------------------------------------------------------
; Table of register names -	used by the register display routine	
NAMES	DEFB	'SP ','$'	;0
		DEFB	'AF ','$'	;1
		DEFB	'BC ','$'	;2
		DEFB	'DE ','$'	;3
		DEFB	'HL ','$'	;4
		DEFB	'PC ','$'	;5
		DEFB	'IX ','$'	;6
		DEFB	'IY ','$'	;7
		DEFB	'IR ','$'	;8
		DEFB	'AF',27H,'$' ;9
		DEFB	'BC',27H,'$' ;10
		DEFB	'DE',27H,'$' ;11
		DEFB	'HL',27H,'$' ;12

;==============================================================================		
;============================== SD CARD COMMANDS ==============================
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
;================================= MESSAGES ===================================		
SIGNON:		DEFB	CR,LF,'XModem File Transfer $'
ADDRESSMSG:	DEFB	CR,LF,'Enter file start address: $'
WAITMSG:	DEFB	'20 second delay for Xmodem start',CR,LF,'$'
STARTMSG:	DEFB	'Starting transfer',CR,LF,'$'
CANCELMSG:	DEFB	'Transfer Canceled',CR,LF,'$'
FINISHMSG:	DEFB 	CR,LF,'Transfer Complete','$'

RESET_ERRmsg:	DB	'SD Card reset to idle state failed $'
CF_TYPE_ERRmsg: DB	'SD Card NOT Type 2. Initialization failed',CR,LF,'$'
INIT_OKmsg:		DB	'SD Card initialized',CR,LF,'$'
CMD55_FAILmsg:	DB	'SDHC/SDXC card support failed $'
CRC_ERRORmsg:	DB	'Error turning off CRC checking',CR,LF,'$'
SIZE_ERRORmsg:	DB	'Sector size error',CR,LF,'$'
NO_INITmsg:	 	DB	'Error: SD Card not initialized',CR,LF,'$'
STATUS_ERRmsg:	DB  'Error in the SD card Status Register',CR,LF,'$'
TRKERRmsg:		DB	'Invalid track number, abort',CR,LF,LF,'$'
SECERRmsg:		DB	'Invalid sector number, abort',CR,LF,LF,'$'
RD_CMD_ERRmsg:  DB	'Read Sector command failed: R1 = $'
WR_CMD_ERRmsg:  DB	'Write Sector command failed: R1 = $'
SEC_RD_ERRmsg:  DB	'Read Sector error: No 0FEH Flag',CR,LF,'$'
WR_ERRmsg:		DB  'Write Sector error: Response token not received',CR,LF,'$'
NOT_DONEmsg:	DB  'Sector Write did not complete',CR,LF,'$'
CPM_LOADmsg		DB	CR,LF,'CP/M loaded into memory, booting...',CR,LF,'$'
CPM_WRTmsg		DB  'CP/M written to disk',CR,LF,'$'

;==============================================================================
;============================= LOCAL VARIABLES ================================											
SD_TYPE	 DW	0	;SD Card's Type number
SecNum	 DW	0	;16-bit physical sector number to read or write
SecCount DW	0	;number of sectors to read or write

track:	 DB	0	;logical disk track to read or write
sector:	 DB	0	;logical disk sector to read or write
diskno:	 DB	0	;disk number 0-1
DMAaddr: DW 0	;start address in RAM memory for sector data

MON_LEN		EQU	($ - MON_START)

	.dephase

	END
