;               HCC FIG generic 8086 FORTH
; $Id$
; Copyright (2000): Albert van der Horst, HCC FIG Holland by GNU Public License

REM Generates a bootable disk from a TASM/MASM forth FigForth src in %1
REM It must be configured as USEDOS and BOOTED .

REM THE FOLLOWING IS TENTATIVE.
ECHO FLOPPY A: WILL BECOME A FORTH BOOTABLE DISK
ECHO ALL DATA WILL BE LOST!
ECHO HIT ^C IF YOU WANT TO STOP.
PAUSE

Tasm/la/zi %1
REM Equally possible:
REM masm %1     
tlink/v %1
REM Or tlink/t %1 generating a .com file

REM or NASM16 %1.asm -b %1.com
echo N > \tmp\fminput.$$$
echo N >> \tmp\fminput.$$$
echo N >> \tmp\fminput.$$$
Format a:/u <\tmp\fminput.$$$

echo N %1.exe> \tmp\dbinput.$$$
echo L >> \tmp\dbinput.$$$
REM WRITE BOOTSECTOR
echo W cs:0 0 0 1>> \tmp\dbinput.$$$
REM WRITE BOOTABLE FILE
echo N A:FORTH.COM> \tmp\dbinput.$$$
echo W cs:000>> \tmp\dbinput.$$$
echo Q>> \tmp\dbinput.$$$
DEBUG                < \tmp\dbinput.$$$

REM SUCCESFULL ENDING
A:
ATTRIB +S +R +H FORTH.COM
C:
EXIT


