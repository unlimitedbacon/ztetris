include .knightos/variables.make

# This is a list of files that need to be added to the filesystem when installing your program
ALL_TARGETS:=$(BIN)ztetris $(APPS)ztetris.app $(SHARE)icons/ztetris.img

# This is all the make targets to produce said files
$(BIN)ztetris: main.asm
	mkdir -p $(BIN)
	$(AS) $(ASFLAGS) main.asm $(BIN)ztetris

$(APPS)ztetris.app: config/ztetris.app
	mkdir -p $(APPS)
	cp config/ztetris.app $(APPS)ztetris.app

$(SHARE)icons/ztetris.img: config/ztetris.png
	mkdir -p $(SHARE)icons
	kimg -c config/ztetris.png $(SHARE)icons/ztetris.img

include .knightos/sdk.make
