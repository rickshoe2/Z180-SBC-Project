# Z180-SBC-Project
This repository contains a powerful machine language monitor (Z180Monitor) for the Z180 SBC board, as well as a set of other programs which can be used to create a copy of CP/M 2.2 and put it onto the board's SD card.
The Z180Monitor can be burned into ROM, and uses the on-board USB serial port running at 19,200 Baud for communication. It assumes that it is connected to a PC running a terminal program (I use TeraTerm). The monitor includes the usual Zapple-type commands (DISP,FILL,HEXLOAD,MOVE,QUERY,SUBS,TYPE,VERIFY,WHERE) as well as a GOTO command that can be used to step through a running program, stopping at breakpoints wherever you wish to examine registers and memory, and then resuming the program until the next breakpoint. It also has an XMODEM command that allows you to download files from the PC into memory where they can be executed and debugged with the GOTO command. Finally, there is a PUTSYS command that allows you to put a CP/M 2.2 image onto the Z180 board's SD card and a BOOT command that will boot into CP/M.
There is also an SD_Monitor program that allows you to format the SD card as a pair of logical SSSD 8" floppy disks having 77 tracks with 26 sectors/track each. It also has PUT_SYS and GET_SYS commands that allow you to put a CP/M image in memory onto the SD card or get CP/M from the SD card and load it into memory. In addition, it has READ_SECTOR, WRTIE_SECTOR, READ_TRACK, and WRITE_TRACK commands to read or write disk sectors and tracks.
A copy of CP/M 2.2 with a customized BIOS is in the repository as both source and object code as well as PCGET2 which allows you to download programs directly into CP/M from the PC using the XModem protocol.
