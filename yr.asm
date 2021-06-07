; This software is copyright 2021 by David S. Madole.
; You have permission to use, modify, copy, and distribute
; this software so long as this copyright notice is retained.
; This software may not be used in commercial applications
; without express written permission from the author.
;
; The author grants a license to Michael H. Riley to use this
; code for any purpose he sees fit, including commercial use,
; and without any need to include the above notice.


           ; Include BIOS and kernal API entry points

           include include/bios.inc
           include include/kernel.inc


           ; Define non-published API elements

version    equ     0400h
himem      equ     0442h
d_type     equ     0444h
d_msg      equ     0447h
d_readkey  equ     0454h
d_input    equ     0457h


           ; Pin and polarity definitions from BIOS suitable for Pico/Elf

#define    SERP    bn2
#define    SERN    b2
#define    SERSEQ  req
#define    SERREQ  seq


           ; ASCII control characters used in X/YMODEM

soh        equ     1                   ; starts 128 byte packet
stx        equ     2                   ; starts 1024 byte packet
etx        equ     3                   ; cancel entire transfer
eot        equ     4                   ; signals end of file
ack        equ     6                   ; affirms good packet
nak        equ     21                  ; rejects bad packet
can        equ     24                  ; cancel entire transfer
crc        equ     67 ; ('C')          ; requests crc not checksum


           ; Flag bits in RE.0 user by X/YMODEM protocol receiver

cksum      equ     1                   ; use checksum instead of crc
donak      equ     2                   ; send nak instead of c to nak
batch      equ     4                   ; batch mode transfer started
first      equ     8                   ; have seen the first data packet
fopen      equ     16
flags      equ     255                 ; all flag bits set


           ; Lengths of 128 and 1024 byte packets with sequence and crc

len128     equ     2 + 128 + 2         ; each has two sequence bytes before
len1kb     equ     2 + 1024 + 2        ; and two crc bytes after data


           ; Executable program header

           org     2000h - 6
           dw      start
           dw      end-start
           dw      start

start:     org     2000h
           br      scanopts


           ; Build information

           db      6+80h              ; month
           db      7                  ; day
           dw      2021               ; year
           dw      2                  ; build
text:      db      'Written by David S. Madole',0


           ; A note about the +255 stuff that will be found in this code:
           ;
           ; Executing a loop with a counter more than 8 bits on the 1802
           ; is painful because you have to separately test the high and
           ; low bytes of a 16-bit register to determine completion like:
           ;
           ;   loop: inc r8
           ;         glo r8
           ;         bnz loop
           ;         ghi r8
           ;         bnz loop
           ;
           ; If you instead adjust the loop counter so that your end count
           ; is 255 rather than 0, then you can only test the high byte, as
           ; it will go to 0 when you go from 256 to 255:
           ;
           ;   loop: inc r8
           ;         ghi r8
           ;         bnz loop
           ;
           ; Since instructions have been removed from the loop that are
           ; executed many times, this can be a significant performance
           ; improvement. Often times the adjustment for this can be put
           ; as changes to constants on operations that are being done
           ; anyway, and so the adjustment comes without any cost. Even in
           ; other cases, it can be worth adjusting the count separately,
           ; as the six instructions to make the adjustment wil usually
           ; be far more than offset by removal of those two instructions
           ; executed repeatedly in the loop.


           ; General register usage:
           ;
           ; System-defined:
           ;   R0   - Reserved for DMA
           ;   R1   - Reserved for interrupts
           ;   R2   - Stack pointer
           ;   R3   - Program counter
           ;   R4   - SCALL program counter
           ;   R5   - SRET program counter
           ;   R6   - SCALL return address
           ;   RE.0 - SCALL overwrites with D
           ;   RE.1 - Baud rate constant
           ;
           ; API register  usage:
           ;
           ;   R7   - Flags for (o_open)
           ;   RA   - Command line pointer (on exec)
           ;   RC   - Data length for (o_write)
           ;   RD   - File descriptor pointer (o_open, o_write, o_close)
           ;   RF   - Filename (o_open) or buffer (o_write) pointer
           ;
           ; Additional application usage:
           ;
           ;   R8
           ;   R9
           ;   RB.0 - Status flags
           ;   RB.1 - Uncompressed baud rate


           ; Main code starts

scanopts:  ldi     0
           plo     rb

skipspac:  ldn     ra                 ; skip any spaces on command line,
           lbz     nooption           ; point to null or first non-space
           inc     ra
           smi     ' '
           lbz     skipspac

           dec     ra                 ; back up to first non-space
           smi     '-'-' '
           lbnz     nooption

           inc     ra
           lda     ra
           smi     'x'
           lbnz    argsfail

           glo     rb
           ori     cksum
           plo     rb
           lbr     skipspac

           ; Check if d_readkey points to BIOS, if so we will interpret RE.1
           ; as a BIOS-compatible baud rate, otherwise as nitro-compatible.

nooption:  ldi     high (d_readkey+1)
           phi     rf
           ldi     low (d_readkey+1)
           plo     rf

           ldn     rf                  ; If below f800h then use nitro rates
           smi     0f8h
           lbnf    uncomprs

           ; BIOS UART bit time measurement is 8 times the value in RE.1 plus
           ; 32 as measured in machine cycles. We need to convert this to
           ; 1 times the value plus 24 to be nitro compatible.

           ghi     re                  ; start by shifting to get bits 7-1
           shr                         ; then add 1 which adds 8 cycles
           adi     1
           plo     r7

           ldi     0                   ; zero nitro baud rate register
           phi     rb

multiply:  ghi     rb                  ; multiply the BIOS rate by 8 by
           adi     8                   ; repeated addition since it's small
           lbdf    baudfail            ; if we overflow the rate is too low
           phi     rb
           dec     r7
           glo     r7
           lbnz     multiply

           lbr     receive

           ; If nitro is in use, the baud rate constant in RE.1 is stored in 
           ; compressed format uncompress here from 7 to 8 bits.

uncomprs:  ghi     re                  ; uncompress the stored delay value,
           shr
           smi     1                   ; shift right then subtract one, save
           str     r2                  ; on stack for below and re for use
           phi     rb

           smi     63                  ; if value is less than 63 leave as-is
           lbnf    receive

           shl                         ; otherwise multiply by 2 then add to
           add                         ; saved value giving final range, save
           phi     rb

           ; Start X/YMODEM receive here

receive:   ldi     high startmsg
           phi     rf
           ldi     low startmsg
           plo     rf

           sep     scall
           dw      o_msg

           ldi     1                   ; put expected packet number on stack
           str     r2


           ; At start, and in case of error needing retry, flush any data
           ; already being received. We don't have a buffer to clear, but
           ; if the sender is sending anything, wait for it to finish.

rcverror:  ldi     200
           phi     rd
flshloop:  SERN    rcverror
           dec     rd
           ghi     rd
           lbnz    flshloop

sendnak:   ldi     high buffer         ; put address of packet buffer into rf
           phi     rf
           ldi     low buffer
           plo     rf

           glo     rb                  ; if we are using CRC and packet 1 not
           ani     cksum + donak       ; received yet, then send C otherwise
           lbnz    noreqcrc            ; send nak to trigger first packet

           ldi     crc
           plo     re
           lbr     getnext

noreqcrc:  ldi     nak
           plo     re


           ; This is the main entry point to receiving a packet. This either
           ; falls through from sendnak or is jumped to from sendack. The 
           ; two possible packet lengths are loaded into separate registers
           ; here in advance because there are not enough instruction cycles
           ; available to do this between characters later once the packet
           ; has started flowing.

getnext:   ldi     high (len128+255)   ; number of data bytes plus check
           phi     r8                  ; chars assuming a 1024 byte packet
           ldi     low (len128+255)
           plo     r8

           ldi     high (len1kb+255)   ; number of data bytes plus check
           phi     rc                  ; chars assuming a 1024 byte packet
           ldi     low (len1kb+255)
           plo     rc

           glo     rb                  ; if checksum not set then use above
           ani     cksum
           lbz     rxpacket

           dec     r8                  ; otherwise decrease counts by one
           dec     rc

           lbr     rxpacket            ; jump to time-sensitive code

timeout:   lbr     sendnak

           ; The entire packet is now received into the buffer, excepting the
           ; initial SOH ot STX character that is no longer needed. Now we
           ; check the validity of the packet and if any error jump to
           ; rcverror which will flush the buffer and send a negative ack.
           ;
           ; From this point on, timing is not critical as X/YMODEM are half-
           ; duplex protocols so the sender is waiting for an acknowledgement
           ; of the packet received before sending anything else. We still
           ; try to be as efficient as possible to speed transfers up.

received:  glo     rf                  ; recover length of data received,
           smi     low (buffer+4-255)  ; adjusting for length of packet id
           plo     r8                  ; bytes and check bytes
           ghi     rf
           smbi    high (buffer+4-255)
           phi     r8

           ldi     high buffer         ; reset buffer pointer
           phi     rf
           ldi     low buffer
           plo     rf

           sex     rf

           lda     rf                  ; check the sequence number and the
           xor                         ; complement of it, nak if mismatch
           xri     0ffh
           lbnz    rcverror

           dec     rf                  ; compare sequence to expected one,
           ldn     r2                  ; if they match then keep packet
           sm
           lbz     expected

           shr                         ; if not a re-send of the the previous
           lbnz    rcverror            ; packet either, then go nak it

           glo     rb                  ; if the previous one, and not the
           ani     first               ; first packet weve seen, then ack
           lbnz    sendack

expected:  inc     rf                  ; skip sequence number and complement
           inc     rf

           glo     rb                  ; unless checksum is selected use crc
           ani     cksum               ; algorithm for check
           lbz     checkcrc

           ; Calculate a simple modulo 256 sum across all of the data bytes
           ; and compares against the checksum received with the packet.

           inc     r8                  ; adjust for only one check byte

           ldi     0
           plo     rd
           sex     rf

sumloop:   glo     rd
           add
           plo     rd

           inc     rf
           dec     r8
           ghi     r8
           lbnz    sumloop

           glo     rd
           xor
           lbnz    rcverror

           lbr     chkbatch

           ; Calculate CRC-16/XMODEM across all of the data bytes and compare
           ; to the CRC received with the packet. This uses a pre-calculated
           ; table for speed, see later in the code for the tables and how
           ; they are derived. The algorithm for calculating the CRC is:
           ;
           ;   crc = 0x0000;
           ;   for (i = 0; i < strlen(bytes); ++i) {
           ;     j = (crc >> 8) ^ bytes[i];
           ;     crc = (crc << 8) ^ table[j];
           ;   }

checkcrc:  ldi     0                   ; clear crc and lsbs of table pointers
           plo     rd
           phi     rd

           ldi     high crctablo
           phi     r9
           ldi     high crctabhi
           phi     rc

crcloop:   ghi     rd                  ; j = (crc >> 8) ^ bytes[i];
           sex     rf
           xor
           plo     r9
           plo     rc

           glo     rd                  ; crc = (crc << 8) ^ table[j];
           sex     rc
           xor
           phi     rd
           ldn     r9
           plo     rd

           inc     rf                  ; r8 pre-adjusted to only check msb
           dec     r8
           ghi     r8
           lbnz    crcloop

           ghi     rd                  ; if no match on either, send nak
           sex     rf
           xor
           lbnz    rcverror

           glo     rd
           inc     rf
           xor
           dec     rf
           lbnz    rcverror


           ; We know the packet is valid now, check if its a YMODEM packet 0

chkbatch:  glo     rf                  ; recover length of data received,
           smi     low (buffer+2)      ; adjusting for length of packet id
           plo     rc
           ghi     rf
           smbi    high (buffer+2)
           phi     rc

           ldi     high buffer         ; put address of packet buffer into rf
           phi     rf
           ldi     low buffer
           plo     rf

           ldn     rf                  ; if sequence is not zero, get next
           lbnz    writepkt 

           glo     rb                  ; if we have already received a data
           ani     first               ; packet for this file, get next
           lbnz    writepkt

           inc     rf                  ; skip sequence number bytes
           inc     rf

           ldn     rf                  ; get first data byte, if zero then
           lbz     recvdone            ; this is an eof packet, go finish

           ; Process a YMODEM batch packet that is in the buffer, possibly
           ; opening the output file, and parsing sent file length so we can
           ; truncate any padding from the last packet when we get there.

batchpkt:  ghi     rf                  ; leave rf pointing to start of name,
           phi     r9                  ; that's where open expects it anyway
           glo     rf
           plo     r9

skipnam1:  lda     r9                  ; skip over filename to space or maybe
           lbz     endfname            ; a zero but it should be a space
           smi     ' '
           lbnz    skipnam1

           dec     r9                  ; overwrite space with null and leave
           str     r9                  ; r9 pointing to length just after it
           inc     r9

           ; If a file name is available on the command line, then don't open
           ; the file now, we will open it later when we first write.

endfname:  ldn     ra                  ; if cmd line name, then don't open
           lbnz    skipopen

           ldi     high fildes         ; load pointer to fildes to rd
           phi     rd
           ldi     low fildes
           plo     rd

           dec     r2                  ; protect packet number on stack

           ldi     1 + 2               ; file flags = create + truncate
           plo     r7

           sep     scall               ; open and/or create the file
           dw      o_open

           inc     r2                  ; re-expose packet number on stack

           glo     rb                  ; set file open flag
           ori     fopen
           plo     rb

skipopen:  glo     rb                  ; mark batch mode, we also need to
           ori     batch               ; switch back to 'C' instead of nak
           ani     flags-donak
           plo     rb

           ; Convert the ASCII decimal file length in the batch header to
           ; 32-bit unsigned binary using the "double dabble" method.

           ldi     high length
           phi     rd
           ldi     low length
           plo     rd

           ldi     0
           plo     r8

tobinary:  ldn     r9                  ; convert digits in-place from ASCII
           smi     48                  ; to binary e.g. from 48-47 to 0-9,
           lbnf    notdigit            ; at the same time, count how many
           str     r9
           smi     10
           lbdf    notdigit
           inc     r9
           inc     r8
           lbr     tobinary

notdigit:  glo     r8                  ; if there were no digits, no point
           lbz     rtlength            ; doing anything else, othewise save
           phi     r8

rewindin:  dec     r9                  ; subtract r8 from r9, this is gross
           dec     r8                  ; but its only for a handful of bytes
           glo     r8
           lbnz    rewindin

           ldi     32                  ; total number of output bit shifts
           plo     rc

           ; The bit-by-bit conversion loops back to here across 32 bits

nextbit:   ghi     r8                  ; reset counter of input digits
           plo     r8

           shl                         ; clear df (since r8 < 128)

shiftasc:  ldn     r9                  ; right-shift the digits, since we
           lbnf    zerobit             ; have 4 bit values in an 8 bit byte
           ori     16                  ; we copy df into the 5th bit
zerobit:   shr
           str     r9
           inc     r9
           dec     r8
           glo     r8
           lbnz    shiftasc

           ldi     4                   ; reset counter of output bytes
           plo     r8

shifti32:  ldn     rd                  ; next right-shift the 32 binary
           shrc                        ; output bits, shifting in the bit
           str     rd                  ; that was shifted out of the digits
           inc     rd
           dec     r8
           glo     r8
           lbnz    shifti32

           dec     rd                  ; repoint to msb of output value
           dec     rd
           dec     rd
           dec     rd

           ghi     r8                  ; reset counter of input digits
           plo     r8

fixupasc:  dec     r9                  ; adjust each digit value, if 8 or
           ldn     r9                  ; higher then subtract 3, and do it
           smi     8                   ; backwards so we reset r9 to first
           lbnf    below8              ; digit at the same time
           adi     5
           str     r9
below8:    dec     r8
           glo     r8
           lbnz    fixupasc

           dec     rc                  ; keep looping 32 times so the lsb
           glo     rc                  ; ends up in the right place
           lbnz    nextbit

           ; End of conversion of ASCII decimal length input

           ; End of conversion of ASCII decimal length input. This leaves r9
           ; pointing just past the last digit and rd just past the length.
           ; Next convert the file timestamp from octal to 32-bit binary.

rtlength:  ghi     r8                  ; if there was a number, then keep it
           lbnz    ackymode

           ldi     0ffh                ; otherwise set to the maximum length
           str     rd
           inc     rd
           str     rd
           inc     rd
           str     rd
           inc     rd
           str     rd

ackymode:  ldi     high type
           phi     r7
           ldi     low type
           plo     r7

           ldi     ack                 ; because this is a batch packet,
           plo     re
           sep     r7                  ; send the ack here


           ldi     100                 ; this delay helps to not have to nak
           phi     r8                  ; packet 1 with teraterm ymodem, not
delaynak:  dec     r8                  ; sure why, determined empirically
           ghi     r8
           lbnz    delaynak

           lbr     sendnak


           ; regular packet here

sendack:   ldi     high buffer         ; put address of packet buffer into rf
           phi     rf
           ldi     low buffer
           plo     rf

           ldn     r2                  ; increment expected packet number
           adi     1
           str     r2

           ldi     ack                 ; send an ack since packet was good
           plo     re

           glo     rb                  ; flag we have received a data packet
           ori     first
           plo     rb

           lbr     getnext


           ; At this point we have validated that the packet in the buffer is
           ; the correct one and that the check code is valid so save to disk.

writepkt:  ldi     high (length+3)     ; point to length of file in memeory
           phi     rd
           ldi     low (length+3)
           plo     rd

           sex     rd

           glo     rc                  ; subtract data size from file length
           sd
           stxd
           ghi     rc
           sdb
           stxd
           ldi     0                   ; 32 bits minus 16 bits so just carry
           sdb
           stxd
           ldi     0
           sdb
           str     rd

           lbdf    nopaddng            ; if we didn't pass the end, write

           inc     rd                  ; otherwise move pointer back to lsb
           inc     rd
           inc     rd

           glo     rc                  ; subtract the amount we overran from
           add                         ; the data length, the overrun is the
           plo     rc                  ; amount of padding on the packet
           ldi     0
           stxd                        ; but add since the overrun is negative
           ghi     rc
           adc
           phi     rc
           ldi     0                   ; 32 bits plus 16 bits so just carry
           stxd
           stxd
           str     rd

nopaddng:  dec     r2                  ; protect packet number on stack

           ldi     high fildes         ; load pointer to fildes to rd
           phi     rd                  ; byte count is already in rc
           ldi     low fildes
           plo     rd

           glo     rb                  ; if we already opened the file
           ani     fopen               ; don't do again, just write data
           lbnz    fileopen

           ldn     ra
           lbnz    gotaname

           ldi     high tempname
           phi     rf
           ldi     low tempname
           plo     rf

           br      openfile

gotaname:  ghi     ra                  ; copy pointer to file name in command
           phi     rf                  ; line to rf where open needs it
           glo     ra
           plo     rf

skipnam2:  ldn     ra                  ; skip over filename to zero or space
           lbz     openfile
           inc     ra
           smi     ' '
           lbnz    skipnam2

           dec     ra                  ; overwrite first space with zero
           str     ra
           inc     ra
           
skipspc2:  ldn     ra                  ; skip over any trailing spaces
           lbz     openfile
           inc     ra
           smi     ' '
           lbz     skipspc2

           dec     ra                  ; backup to non-space

openfile:  ldi     1 + 2               ; file flags = create + truncate
           plo     r7

           sep     scall               ; open the output file
           dw      o_open

fileopen:  ldi     high (buffer+2)     ; get pointer to data in buffer
           phi     rf                  ; rc was previously set to length
           ldi     low (buffer+2)
           plo     rf

           sep     scall               ; write buffer to output file
           dw      o_write

           inc     r2                  ; expose packet number on stack

           glo     rb
           ori     fopen               ; mark as file already opened
           plo     rb

           lbr     sendack


           ; ending points

recvdone:  ldi     high type
           phi     r7
           ldi     low type
           plo     r7

           ldi     ack                 ; send ack in resonse to eot
           plo     re
           sep     r7

           ldi     high complete
           phi     rf
           ldi     low complete
           plo     rf

           lbr     finflush

controlc:  ldi     high ctrlcmsg
           phi     rf
           ldi     low ctrlcmsg
           plo     rf

           lbr     closefil

cancel:    ldi     high cancelms
           phi     rf
           ldi     low cancelms
           plo     rf

finflush:  ldi     200
           phi     rd
outputfls: SERN    finflush
           dec     rd
           ghi     rd
           lbnz    outputfls

closefil:  glo     rb
           ani     fopen
           lbz     outputmsg

           ldi     high fildes
           phi     rd
           ldi     low fildes
           plo     rd

           sep     scall
           dw      o_close

outputmsg: sep     scall
           dw      o_msg

           sep     sret                ; return to operating system




startmsg:  db      'Waiting for X/YMODEM... ',0
cancelms:  db      ' ...cancelled',13,10,0
complete:  db      ' ...completed',13,10,0
ctrlcmsg:  db      ' ^C',13,10,0

baudfail:  ldi     high baudmesg
           phi     rf
           ldi     low baudmesg
           plo     rf

           sep     scall
           dw      o_msg
           sep     sret

baudmesg:  db      'Baud rate is too low', 13,10,0

argsfail:  ldi     high argsmesg
           phi     rf
           ldi     low argsmesg
           plo     rf

           sep     scall
           dw      o_msg
           sep     sret

argsmesg:  db      'Usage: yr [-x] filename ...',13,10,0

openfail:  ldi     high openmesg
           phi     rf
           ldi     low openmesg
           plo     rf

           sep     scall
           dw      o_msg
           sep     sret

openmesg:  db      'Open output file failed',13,10,0

writfail:  sep     scall
           dw      o_close

           ldi     high writmesg
           phi     rf
           ldi     low writmesg
           plo     rf

           sep     scall
           dw      o_msg
           sep     sret

writmesg:  db      'Write output file failed',13,10,0








           ; Code following this point is location-dependent. The CRC factor
           ; tables are because they must be page aligned, and the code that
           ; follows them is timing sensitive and needs to use short branch
           ; instructions. Starting them at a page boundary after the end of
           ; everything else seems the easiest way to handle this, especially
           ; since Rc/asm doesn't catch short branch page violations.

           org     $ + 0ffh & 0ff00h   ; Align to a memory page boundary


           ; The following is a table of pre-calculated CRC factors that
           ; was produced by the following code, stored with the low-order
           ; and high-order bytes broken out into separate tables to save
           ; a couple of instructions in the inner calculation loop. These
           ; are page aligned so we can just set the pointer lsb to index.
           ;
           ;   poly = 0x1021;
           ;   for (i = 0; i < 256; i++) {
           ;     k = (i << 8);
           ;     for (j = 0; j < 8; j++) {
           ;       k = (k << 1) ^ (k >> 15 ? poly : 0);
           ;     }
           ;     table[i] = k;
           ;   }

crctablo:  db      000h, 021h, 042h, 063h, 084h, 0a5h, 0c6h, 0e7h
           db      008h, 029h, 04ah, 06bh, 08ch, 0adh, 0ceh, 0efh
           db      031h, 010h, 073h, 052h, 0b5h, 094h, 0f7h, 0d6h
           db      039h, 018h, 07bh, 05ah, 0bdh, 09ch, 0ffh, 0deh
           db      062h, 043h, 020h, 001h, 0e6h, 0c7h, 0a4h, 085h
           db      06ah, 04bh, 028h, 009h, 0eeh, 0cfh, 0ach, 08dh
           db      053h, 072h, 011h, 030h, 0d7h, 0f6h, 095h, 0b4h
           db      05bh, 07ah, 019h, 038h, 0dfh, 0feh, 09dh, 0bch
           db      0c4h, 0e5h, 086h, 0a7h, 040h, 061h, 002h, 023h
           db      0cch, 0edh, 08eh, 0afh, 048h, 069h, 00ah, 02bh
           db      0f5h, 0d4h, 0b7h, 096h, 071h, 050h, 033h, 012h
           db      0fdh, 0dch, 0bfh, 09eh, 079h, 058h, 03bh, 01ah
           db      0a6h, 087h, 0e4h, 0c5h, 022h, 003h, 060h, 041h
           db      0aeh, 08fh, 0ech, 0cdh, 02ah, 00bh, 068h, 049h
           db      097h, 0b6h, 0d5h, 0f4h, 013h, 032h, 051h, 070h
           db      09fh, 0beh, 0ddh, 0fch, 01bh, 03ah, 059h, 078h
           db      088h, 0a9h, 0cah, 0ebh, 00ch, 02dh, 04eh, 06fh
           db      080h, 0a1h, 0c2h, 0e3h, 004h, 025h, 046h, 067h
           db      0b9h, 098h, 0fbh, 0dah, 03dh, 01ch, 07fh, 05eh
           db      0b1h, 090h, 0f3h, 0d2h, 035h, 014h, 077h, 056h
           db      0eah, 0cbh, 0a8h, 089h, 06eh, 04fh, 02ch, 00dh
           db      0e2h, 0c3h, 0a0h, 081h, 066h, 047h, 024h, 005h
           db      0dbh, 0fah, 099h, 0b8h, 05fh, 07eh, 01dh, 03ch
           db      0d3h, 0f2h, 091h, 0b0h, 057h, 076h, 015h, 034h
           db      04ch, 06dh, 00eh, 02fh, 0c8h, 0e9h, 08ah, 0abh
           db      044h, 065h, 006h, 027h, 0c0h, 0e1h, 082h, 0a3h
           db      07dh, 05ch, 03fh, 01eh, 0f9h, 0d8h, 0bbh, 09ah
           db      075h, 054h, 037h, 016h, 0f1h, 0d0h, 0b3h, 092h
           db      02eh, 00fh, 06ch, 04dh, 0aah, 08bh, 0e8h, 0c9h
           db      026h, 007h, 064h, 045h, 0a2h, 083h, 0e0h, 0c1h
           db      01fh, 03eh, 05dh, 07ch, 09bh, 0bah, 0d9h, 0f8h
           db      017h, 036h, 055h, 074h, 093h, 0b2h, 0d1h, 0f0h

crctabhi:  db      000h, 010h, 020h, 030h, 040h, 050h, 060h, 070h
           db      081h, 091h, 0a1h, 0b1h, 0c1h, 0d1h, 0e1h, 0f1h
           db      012h, 002h, 032h, 022h, 052h, 042h, 072h, 062h
           db      093h, 083h, 0b3h, 0a3h, 0d3h, 0c3h, 0f3h, 0e3h
           db      024h, 034h, 004h, 014h, 064h, 074h, 044h, 054h
           db      0a5h, 0b5h, 085h, 095h, 0e5h, 0f5h, 0c5h, 0d5h
           db      036h, 026h, 016h, 006h, 076h, 066h, 056h, 046h
           db      0b7h, 0a7h, 097h, 087h, 0f7h, 0e7h, 0d7h, 0c7h
           db      048h, 058h, 068h, 078h, 008h, 018h, 028h, 038h
           db      0c9h, 0d9h, 0e9h, 0f9h, 089h, 099h, 0a9h, 0b9h
           db      05ah, 04ah, 07ah, 06ah, 01ah, 00ah, 03ah, 02ah
           db      0dbh, 0cbh, 0fbh, 0ebh, 09bh, 08bh, 0bbh, 0abh
           db      06ch, 07ch, 04ch, 05ch, 02ch, 03ch, 00ch, 01ch
           db      0edh, 0fdh, 0cdh, 0ddh, 0adh, 0bdh, 08dh, 09dh
           db      07eh, 06eh, 05eh, 04eh, 03eh, 02eh, 01eh, 00eh
           db      0ffh, 0efh, 0dfh, 0cfh, 0bfh, 0afh, 09fh, 08fh
           db      091h, 081h, 0b1h, 0a1h, 0d1h, 0c1h, 0f1h, 0e1h
           db      010h, 000h, 030h, 020h, 050h, 040h, 070h, 060h
           db      083h, 093h, 0a3h, 0b3h, 0c3h, 0d3h, 0e3h, 0f3h
           db      002h, 012h, 022h, 032h, 042h, 052h, 062h, 072h
           db      0b5h, 0a5h, 095h, 085h, 0f5h, 0e5h, 0d5h, 0c5h
           db      034h, 024h, 014h, 004h, 074h, 064h, 054h, 044h
           db      0a7h, 0b7h, 087h, 097h, 0e7h, 0f7h, 0c7h, 0d7h
           db      026h, 036h, 006h, 016h, 066h, 076h, 046h, 056h
           db      0d9h, 0c9h, 0f9h, 0e9h, 099h, 089h, 0b9h, 0a9h
           db      058h, 048h, 078h, 068h, 018h, 008h, 038h, 028h
           db      0cbh, 0dbh, 0ebh, 0fbh, 08bh, 09bh, 0abh, 0bbh
           db      04ah, 05ah, 06ah, 07ah, 00ah, 01ah, 02ah, 03ah
           db      0fdh, 0edh, 0ddh, 0cdh, 0bdh, 0adh, 09dh, 08dh
           db      07ch, 06ch, 05ch, 04ch, 03ch, 02ch, 01ch, 00ch
           db      0efh, 0ffh, 0cfh, 0dfh, 0afh, 0bfh, 08fh, 09fh
           db      06eh, 07eh, 04eh, 05eh, 02eh, 03eh, 00eh, 01eh


           ; Since X/YMODEM do not have flow control except packet-by-packet
           ; and we can't rely on a hardware buffer for incoming data, some
           ; of the code is very timing critical. Once a packet starts being
           ; received, we have only a few instructions during the stop bit
           ; time to process the data. At maximum baud rate, this is about
           ; eight two-cycle instructions of budget. This makes it necessary
           ; to use short branch instructions, as does the UART code's need
           ; to look at the EF lines, which only have short branch tests.

rxpacket:  ldi     high timeout        ; return address if timeout occurs
           phi     r9
           ldi     low timeout
           plo     r9

retrysoh:  ldi     high type
           phi     r7
           ldi     low type
           plo     r7

           ldi     255                 ; set timeout timer to max value
           phi     rd

           sep     r7                  ; send start char from buffer

waitsoh:   sep     r7                  ; get first byte of packet

           smi     soh                 ; if it is soh, go get a 128 byte packet
           bz      read128

           smi     stx - soh           ; if it is stx, go get a 1024 byte packet
           bz      read1024

           smi     etx - stx           ; if it is etx, then cancel
           lbz     controlc

           smi     eot - etx           ; if it is eot, go process end of file
           bz      endfile

           smi     can - eot           ; if it is not can, keep looking
           lbz     cancel

           lbr     rcverror


           ; End of file

endfile:   glo     rb
           ani     fopen
           bz      fnotopen

           ldi     high fildes         ; load pointer to fildes
           phi     rd
           ldi     low fildes
           plo     rd

           dec     r2

           sep     scall               ; close output file
           dw      o_close

           inc     r2

           glo     rb
           ani     flags-fopen
           plo     rb

fnotopen:  glo     rb                  ; if not ymodem mode we are done
           ani     batch
           lbz     recvdone

           ldi     1                   ; reset expected sequence number
           str     r2

           glo     rb                  ; clear except checksum flag
           ani     cksum
           plo     rb

           lbr     ackymode


           ; Start receving the main part of the packet now that its started.
           ;
           ; Timing is critical in this code to avoid missing back-to-back
           ; characters. As noted previously, this is why there are two copies
           ; of this code for the two packet lengths: they reference different
           ; pre-initialized length counter variables.

read128:   glo     rb                  ; once weve seen soh only send nak
           ori     donak
           plo     rb

loop128:   sep     r7                  ; receive expected packets to buffer
           inc     rf                  ; and advance pointer

           ldi     50                 ; reset timeout approx 1 sec at 4 Mhz
           phi     rd

           dec     r8                  ; loop counter has been +255 adjusted
           ghi     r8
           bnz     loop128

           ghi     rb
           br      checkbuf


read1024:  glo     rb                  ; once wve seen stx only send nak
           ori     donak
           plo     rb

loop1024:  sep     r7                  ; receive expected packets to buffer
           inc     rf                  ; and advance pointer

           ldi     50                 ; reset timeout approx 1 sec at 4 Mhz
           phi     rd

           dec     rc                  ; loop counter has been +255 adjusted
           ghi     rc
           bnz     loop1024

           ghi     rb
checkbuf:  SERN    rcverrsh
           smi     1 
           SERN    rcverrsh
           bnz     checkbuf

           lbr     received
rcverrsh:  lbr     rcverror

; Input
;
; r7   - program counter
; r9   - timeout return address
; ra   - timeout counter
; re.0 - half bit delay
; re.1 - full bit delay
; rf   - pointer to buffer


         ; Top of bit receive loop

readspac:  smi     0                   ; Jumps here to set DF if a 1 bit

readmark:  ldn     rf                  ; Get current received character and
           shrc                        ;  shift right to put new bit at MSB,
           str     rf                  ;  move stack pointer to delay value

           bdf     readmore            ;  to character, branch if not done

         ; Character receive is done

readdone:  SERN    readdone            ; Wait for stop bit

readretn:  sep     r3                  ;  and return to caller

         ; Receive entry point is here

read:      SERN    readstrt
           dec     rd
           SERN    readstrt
           ghi     rd
           SERN    readstrt
           ghi     rd
           SERN    readstrt
           ghi     rd
           SERN    readstrt
           ghi     rd
           SERN    readstrt
           ghi     rd
           SERN    readstrt
           ghi     rd
           SERN    readstrt
           ghi     rd
           SERN    readstrt
           ghi     rd
           SERN    readstrt
           bnz     read

readtimo:  ghi     r9
           phi     r3
           glo     r9
           plo     r3

           br      readretn

readstrt:  ldi     07fh                ; Preload character with all 1's, we
           str     rf                  ;  will look for a 0 to know when done

           ghi     rb                  ; Get half for delay to middle of start
           shr

readdly1:  smi     4
           bdf     readdly1
           sdi     low (readjmp1-1)    ; Calculate jump based on remainder of
           plo     r7                  ;  timing loop, which is -1 to -4

readjmp1:  skp                         ; Delay 5 cycles from here
           skp                         ;  4 cycles from here
           lskp                        ;  3 cycles from here
           ldi     0                   ;  2 cycles from here

         ; Character bit receive loop resumes here

readmore:  ghi     rb                  ; Get timing constant and move SP back
readdly2:  smi     4                   ; Delays for cycles equal to the next
           bdf     readdly2            ;  higher multiple of 4 of value of D
           sdi     low (readjmp2-1)    ; Calculate jump based on remainder of
           plo     r7                  ;  timing loop, which is -1 to -4

readjmp2:  skp                         ; Delay 5 cycles from here
           skp                         ;  4 cycles from here
           lskp                        ;  3 cycles from here
           ldi     0                   ;  2 cycles from here

           SERP    readspac            ; If not EF2 then bit is space, go set
           br      readmark            ;  DF, otherwise a mark, leave DF clear

         ; Go back to top of bit receive loop


; Output
;
; r7   - program counter
; re.1 - full bit delay
; rf   - pointer to buffer

typestop:  SERREQ                      ; We are done sending, set the stop bit
           br      readretn            ;  then return to the caller

type:      ghi     rb                  ; Delay for one bit time before start

typedly1:  smi     4                   ;  bit so we can be called back-to-
           bdf     typedly1            ;  back without a start bit violation

           ghi     rb                  ; Get delay value, advance stack to
           SERSEQ                      ;  point to output character, then

typedly2:  smi     4                   ;  start sending start bit and delay
           bdf     typedly2            ;  for one bit in 4 cycle chunks

           sdi     low (typejmp2-1)    ; Calculate jump for remainder left
           smi     0                   ;  from delay loop, set DF to shift a
           plo     r7                  ;  1 in to know when send is complete

           ; Main loop jumps back to here

typejmp2:  skp                         ; Delay 5 cycles from here
           skp                         ;  4 cycles from here
           lskp                        ;  3 cycles from here
           ldi     0                   ;  2 cycles from here

           glo     re                  ; Get character value to send,
           shrc                        ;  shift out lowest bit, update stack
           plo     re                 ;  to bit time delay value

           bz      typestop            ; If all bits clear, we are done, 
           bdf     type1bit            ;  otherwise jump if sending a 1 bit

           SERSEQ                      ; Send a zero bit
           ghi     rb                  ;  advance the stack pointer back
typedly3:  smi     4                   ;  to character value, then delay
           bdf     typedly3            ;  for one bit time

           sdi     low (typejmp2-1)    ; Calculate the delay jump, this also
           plo     r7                  ;  returns to the start of the loop

type1bit:  SERREQ                      ; Send a one bit, this block of code
           ghi     rb                  ;  is the same as for the zero bit
typedly4:  smi     4                   ;  above but is factored out twice
           bdf     typedly4            ;  to meet timing constraints

           sdi     low (typejmp2-1)    ; Calculate the delay jump, this also
           plo     r7                  ;  returns to the start of the loop


           ; This will hold the length of the file being received if we get
           ; send one through a YMODEM header packet, this is a 32-bit 
           ; unsigned value. Pre-set to the maximum value in case all we are
           ; doing is an XMODEM which will not send the length info.

tempname:  db      'yr-received-file',0

length:    db      0ffh,0ffh,0ffh,0ffh

fildes:    db      0,0,0,0
           dw      dta
           db      0,0
           db      0
           db      0,0,0,0
           dw      0,0
           db      0,0,0,0

end:       ; This is the end of what will actually be in the executable file.
           ; The structures below will end up in memory just following the 
           ; loaded code. This is considered safe to do for modest amounts
           ; of data (perhaps less than 2K?) at least for now.

dta:       ds      512
buffer:    ds      len1kb

