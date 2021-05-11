..\bin\msdos ..\bin\as09xx monitor.asm -l > monitor.lst
..\bin\mot2bin monitor.s19
..\bin\bin2hexw monitor.bin monitor.hex
COPY monitor.hex C:\kitahard\EP2C5_MINI\mc6809\ROMS\6809\moni09.HEX
type monitor.lst
timeout 3
