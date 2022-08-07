
yr.bin: yr.asm include/bios.inc include/kernel.inc
	asm02 -l -b yr.asm

clean:
	-rm -f yr.bin yr.lst

