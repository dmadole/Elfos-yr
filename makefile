PROJECT = yr

$(PROJECT).prg: $(PROJECT).asm include/bios.inc include/kernel.inc
	rcasm -l -v -x -d 1802 $(PROJECT) > $(PROJECT).lst 2>&1
	cat $(PROJECT).lst
	hextobin $(PROJECT)

crc: crc.c
	cc -o crc crc.c

clean:
	-rm -f $(PROJECT).bin
	-rm -f $(PROJECT).prg
	-rm -f $(PROJECT).lst
	-rm -f crc

