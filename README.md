I have gotten my new program “yr” close enough that I am ready to have others try it out who are interested…

This is a new file receiver for Elf/OS that supports both XMODEM (including 1K and CRC options) and YMODEM.

YMODEM brings some particular advantages, one of which is that it transmits the file size with the file, so that the receiving end can remove the padding bytes. This way files transferred to Elf/OS come through with the correct sizes, and also will produce the correct CRCs. YMODEM also passes the filename you don’t need to specify it (unless you want to change the name), and it supports batch transfer of multiple files at once.

Besides working with nitro at all the baud rates it supports (approximately 2400 to 21000 at 4Mhz) I have also made this compatible with the BIOS-based soft UART as well, when running at rates that are supported by nitro (it will not work at the lowest rates BIOS supports). In both cases it uses the nitro UART, but I have made it able to translate BIOS baud rate constants to ones compatible with the nitro UART.

Used with both turbo and nitro and at 19200 baud, this will move data at about 1.7KB per second.

USAGE:

yr accepts one command line option “-x” which uses the old checksum instead of CRC. It can be used for compatibility with old XMODEM clients that do not support CRC.

If being used to receive XMODEM, you should supply one filename on the command line. If you do not supply a filename and send via XMODEM, it will save your file as “yr-receive-file” and you can then rename it.

If being used to receive YMODEM, you can leave the command line blank and it will save the files using the names provided by the sending system. You can also specify one or more filenames, separated with spaces, and those will be used instead of the received names. If more files are transmitted than names provided, once the command line names run out, it will start using the names provided by the sending system. Multiple files can be received in one invocation with YMODEM.

NOTE:

There is a new release of nitro that corrects an off-by-one situation that causes a slight inaccuracy in baud rates. It doesn’t normally cause a noticeable issue with the console, but it can create errors with file transfers, so it is highly recommended to use this release if you are going to use nitro with yr.

The release builds are only for machines with Pico/Elf like soft UART ports. If you would like a build for something else, let me know.
