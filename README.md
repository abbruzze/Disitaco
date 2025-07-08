![workflow status](https://github.com/abbruzze/Disitaco/actions/workflows/scala.yml/badge.svg)
[![Release](https://img.shields.io/github/v/release/abbruzze/Disitaco)](https://github.com/abbruzze/Disitaco/releases)
[![Language](https://img.shields.io/github/languages/top/abbruzze/Disitaco)]()
[![Downloads](https://img.shields.io/github/downloads/abbruzze/Disitaco/total)](https://github.com/abbruzze/Disitaco/releases/latest)

<p align="center">
  <img src="images/disitacoREADMELogoT.png" alt="Disitaco logo" width="384" height="384">
</p>

# Disitaco ver 1.0
Scala PC XT emulator

<p align="center">
  <img src="images/boot.gif" alt="msdos 6.22 booting process">
</p>

## Emulator main features
-----------
- 8088 CPU (instruction-exact), testet against [ProcessorTests](https://github.com/SingleStepTests/ProcessorTests/tree/main/8088)
- Intel 8237 DMA controller
- Intel 8253 PIT
- Intel 8259 PIC
- Intel 8255 PPI
- INS 8250 UART
- MC146818 RTC
- Lotech EMS
- Speaker
- AdLib audio card
- CGA, MDA, HDA video cards
- Floppy controller with 1.44M, 720K, 360K, 320K, 180K and 160K floppy images
- Hard disk controller with 10M and 24M hard disk images
- Option ROMs support
- Mouse support (serial)
- 8088 Debugger
- Keyboard

### Bios
Disitaco requires a BIOS to run. The default BIOS is the [GLa BIOS](https://glabios.org/) already included in the `rom` directory. 
You can also use the original IBM 5160 BIOS, split in U18 and U19 rom files. The configuration file allows you to choose which BIOS to use.

### Storage
Disitaco supports floppy and hard disk images. Images can be created with the `makedisk` command line utility.
Disitaco supports the Xebec hard disk controller and can be configured with the `option.rom` properties in the configuration file.
The hard disks images size can be 10M or 24M.
The emulator is already shipped with a 10M hard disk image, `H_C.img`, formatted with DOS 6.22, that can be used with the Xebec controller.
A Virtual 1.44 floppy disk can be created with the content of a local directory: in this case the floppy disk size must be set to 1.44M in the configuration file.

### Video
Disitaco supports CGA, MDA and HDA video cards. The CGA card can be configured to use the alternative character set and to display on a composite monitor.
The emulator's frame size will be adjusted to the video card's resolution automatically.

### Mouse
Disitaco supports a serial mouse. The mouse can be configured to use 3 buttons and the scale factor for the X and Y axis.
Must be enabled in the configuration file and activated using the 'Mouse capture' menu item under Tools.
To deactivate the mouse, click the mouse wheel button with CTRL pressed.

### Audio
Disitaco supports the speaker and the AdLib audio card. The speaker can be configured with a sampling frequency.

### Keyboard
Disitaco supports the PC XT keyboard. The keyboard, at the moment, supports the Italian layout only.

### Serial
Other than the serial mouse, Disitaco supports two out of the box serial port devices. 
The first one is used to exchange files with the host, using the tool `hostftp.exe` shipped with the hard drive image, the second one can be used to connect a terminal to an external TCP/IP source.

## Usage
Go to https://github.com/abbruzze/Disitaco/releases/latest and download and unzip on your computer the latest version.
Be sure to have a jre (17 or above, best performance on latest versions) in the path and launch in the bin directory:
- On Windows: **disitaco.bat**
- On Linux: **disitaco.sh**


## Configuration
The emulator's configuration is stored in `config/disitaco.config` file.
Follows the default configuration file content. You can copy it to `config/disitaco.config` and modify it to suit your needs.

```properties
# Default Disitaco properties
# all path that does not start with / or <D>: (where <D> is a drive letter on Windows) are relative to Disitaco's installation home
# Windows path: every backslash character must be doubled: e.g. c:\temp => c:\\temp

# clock frequency in Hz
clock = 4770000

# 8088's speed correction factor: if > 1 slow down cpu
cpu.correctionFactor = 1.0

# bios can be configured either with 'bios' property that must reference a single bios rom or with 'bios.u18' and 'bios.u19' properties that must reference the u18 and u19 rom respectively
bios = rom/GLABIOS_0.2.5_8T.ROM
#bios.u18 = rom/BIOS_5160_10JAN86_U18_62X0851_27256_F800.BIN
#bios.u19 = rom/BIOS_5160_10JAN86_U19_62X0854_27256_F000.BIN

# graphic card: mda (monochrome), cga (color), hda (hercules)
video.card = cga

#total memory in Kbytes <= 640
memory = 640

# audio enabled/disabled
audio.enabled = true
# audio buffer in millis
audio.audioBufferMillis = 1

# speaker sampling frequency in Hz
speaker.samplingFreq = 48000
# speaker volume: 0 to 100
speaker.volume = 70

# AdLib audio card enabled
adlib.enabled = false
# AdLib volume: 0 to 100
adlib.volume = 100

# CGA char rom
cga.char.rom = rom/IBM_5788005_AM9264_1981_CGA_MDA_CARD.BIN
# CGA character set: true means alternative
cga.altCharSet = false
# CGA on composite monitor
cga.composite = false

# Lotech EMS
ems.lotech.enabled = true
ems.lotech.port = 260
ems.lotech.address = E0000

# floppy flushing: if enabled writes to floppy images will be written on host images, discarded otherwise
floppy.flushing = true

# floppies image paths
# if path is a directory will be created a virtual 1.44M floppy in sync with the given directory
#floppy.a.image =
#floppy.b.image =

# floppy geometry: 160k, 180k, 320k, 360k, 720k, 1200k, 1440k, 2880k
# 320k can read 160k, 720k can read 360k, 1440k can read 720k
floppy.a.geometry = 720k
floppy.b.geometry = 1440k

# mouse
mouse.serial.enabled = true
mouse.serial.3buttons = true
mouse.serial.com = 1
mouse.serial.scale.x = 0.5
mouse.serial.scale.y = 0.5

# debugger
debugger.openAtStartup = false

# turbo (warp mode)
# reading from port disable warp mode. writing to port enable warp mode
turbo.port = 68

# host ftp
# use hostftp.exe to transfer files to host
hostftp.enabled = true
hostftp.com = 2

# fast int13 - bypass real floppy/hd controller increasing performance
fastInt13.enabled = true

# Option roms
# 3 mandatory entries, 1 optional entry:
# option.rom.<N>.name = Option Rom label
# option.rom.<N>.address = Address where the rom will be loaded. E.g. D0000
# option.rom.<N>.path = rom path
# option.rom.<N>.refs = additional info if any (optional)

# declare the size of the hard disks: 10 or 24 can be configured (expressed in Mb)
# Use EmptyDiskMaker utility to create an image of 10404k or 25432k
hd.c.size = 10
hd.d.size = 10

# HD refs syntax: <hd image path for drive C>[,<hd image path for drive D>]
# hard disks will be recognized if the option rom contains the Xebec string
option.rom.1.name = Xebec HD
option.rom.1.address = D0000
option.rom.1.path = rom/IBM_XEBEC_5000059_1982.BIN
option.rom.1.refs = rom/H_C.img
# RTC
option.rom.2.name = GLaTICK
option.rom.2.address = D2000
option.rom.2.path = rom/GLaTICK_0.8.5_AT.ROM
```
