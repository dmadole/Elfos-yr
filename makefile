PROJECT = yr

$(PROJECT).prg: $(PROJECT).asm bios.inc kernel.inc
	rcasm -l -v -x -d 1802 $(PROJECT) > $(PROJECT).lst
	cat $(PROJECT).lst
	hextobin $(PROJECT)

crc: crc.c
	cc -o crc crc.c

clean:
	-rm -f $(PROJECT).bin
	-rm -f $(PROJECT).prg
	-rm -f $(PROJECT).lst

