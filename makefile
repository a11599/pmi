#------------------------------------------------------------------------------
# Protected mode interface (PMI) makefile
#------------------------------------------------------------------------------

# Compiler options

nasm_dos_opts = -i "src" -f obj
nasm_pe_opts = -i "src" -f win32

# Build mode
# Set to "release" in command line parameter to create a release build.
# Example for full recompilation of the release version:
# wmake build=release full

build = debug

# Target stub EXE file without extension

pmi = build build\$(build) build\$(build)\obj &
	build\$(build)\pmi.exe

# List of runtime library object files

rtl = build build\$(build) build\$(build)\rtl &
	build\$(build)\rtl\env_arg.obj &
	build\$(build)\rtl\irq.obj &
	build\$(build)\rtl\keyboard.obj &
	build\$(build)\rtl\log.obj &
	build\$(build)\rtl\profiler.obj &
	build\$(build)\rtl\string.obj &
	build\$(build)\rtl\systimer.obj &
	build\$(build)\rtl\timer.obj

# Validate build target environment value

build_ok = 0
!ifeq build debug
%exceptions = 1
build_ok = 1
!endif
!ifeq build release
%exceptions =
build_ok = 1
!endif
!ifneq build_ok 1
pmi = abort
rtl = abort
!endif

# Append \ at the end of nasm/watcom path variables if not empty

!ifneq nasm_dir
nasm_dir = $(nasm_dir)\
!endif
!ifneq watcom_dir
watcom_dir = $(watcom_dir)\
!endif

# Build PMI and RTL

incremental: $(pmi) $(rtl)
full: clean $(pmi) $(rtl)

# Create binary distribution package

dist: .SYMBOLIC
	$(watcom_dir)wmake full
	$(watcom_dir)wmake build=release full
	@if not exist dist mkdir dist
	@if not exist dist\debug mkdir dist\debug
	@if not exist dist\debug\rtl mkdir dist\debug\rtl
	@if not exist dist\release mkdir dist\release
	@if not exist dist\release\rtl mkdir dist\release\rtl
	@copy build\debug\pmi.exe dist\debug
	@copy build\debug\rtl\*.obj dist\debug\rtl
	@copy build\release\pmi.exe dist\release
	@copy build\release\rtl\*.obj dist\release\rtl
	@copy README.md dist

# Cleanup

clean: .SYMBOLIC .MULTIPLE
	@if exist build\$(build)\obj del /q build\$(build)\obj\*.*
	@if exist build\$(build)\obj rmdir build\$(build)\obj
	@if exist build\$(build)\rtl del /q build\$(build)\rtl\*.*
	@if exist build\$(build)\rtl rmdir build\$(build)\rtl
	@if exist build\$(build) del /q build\$(build)\*.*


#------------------------------------------------------------------------------
# Build PMI stub
#------------------------------------------------------------------------------

# List of PMI stub objects

pmi_objs = &
	build\$(build)\obj\pmi.obj &
	build\$(build)\obj\kernel.obj &
	build\$(build)\obj\memory.obj &
	build\$(build)\obj\file.obj &
	build\$(build)\obj\program.obj &
	build\$(build)\obj\dma.obj

# Abort if unknown build environment is given

abort:
	echo "$(build)" is not a valid build target.
	@%abort

# Create directory for binary files

build: .SYMBOLIC .ALWAYS
	@if not exist build mkdir build

build\$(build): build .SYMBOLIC .ALWAYS
	@if not exist build\$(build) mkdir build\$(build)

build\$(build)\obj: build\$(build) .SYMBOLIC .ALWAYS
	@if not exist build\$(build)\obj mkdir build\$(build)\obj

# Binary build and link

build\$(build)\pmi.exe: $(pmi_objs) build\$(build)
	@%create build\$(build)\obj\pmi.lnk
	@%write build\$(build)\obj\pmi.lnk NAME build\$(build)\pmi
	@%write build\$(build)\obj\pmi.lnk OPTION dosseg
	@%write build\$(build)\obj\pmi.lnk OPTION map=build\$(build)\obj\pmi.map
	@%write build\$(build)\obj\pmi.lnk OPTION packcode=0
	@%write build\$(build)\obj\pmi.lnk OPTION packdata=0
	@%write build\$(build)\obj\pmi.lnk FORM dos
	@%write build\$(build)\obj\pmi.lnk FILE {$(pmi_objs)}
	$(watcom_dir)wlink @build\$(build)\obj\pmi.lnk

# .obj file dependencies with included external files and build instructions

build\$(build)\obj\dma.obj: src\pmi\dma.asm &
	src\pmi\config.inc &
	src\pmi\api\pmi.inc &
	src\pmi\api\kernel.inc &
	src\pmi\structs\dma.inc

	$(nasm_dir)nasm $(nasm_dos_opts) $[@ -o $^@

build\$(build)\obj\file.obj: src\pmi\file.asm &
	src\pmi\config.inc &
	src\pmi\api\pmi.inc &
	src\pmi\api\kernel.inc &
	src\pmi\api\memory.inc

	$(nasm_dir)nasm $(nasm_dos_opts) $[@ -o $^@

build\$(build)\obj\kernel.obj: src\pmi\kernel.asm &
	src\pmi\config.inc &
	src\pmi\api\pmi.inc &
	src\pmi\api\memory.inc &
	src\pmi\api\file.inc &
	src\pmi\api\dma.inc &
	src\pmi\api\program.inc &
	src\pmi\consts\kernel.inc &
	src\pmi\structs\kernel.inc &
	src\pmi\structs\memory.inc

	$(nasm_dir)nasm $(nasm_dos_opts) $[@ -o $^@

build\$(build)\obj\memory.obj: src\pmi\memory.asm &
	src\pmi\config.inc &
	src\pmi\api\pmi.inc &
	src\pmi\api\kernel.inc &
	src\pmi\consts\kernel.inc &
	src\pmi\consts\memory.inc &
	src\pmi\structs\memory.inc

	$(nasm_dir)nasm $(nasm_dos_opts) $[@ -o $^@

build\$(build)\obj\pmi.obj: src\pmi\pmi.asm &
	src\pmi\config.inc &
	src\pmi\api\pmi.inc &
	src\pmi\api\kernel.inc &
	src\pmi\consts\kernel.inc &
	src\pmi\structs\kernel.inc

	$(nasm_dir)nasm $(nasm_dos_opts) $[@ -o $^@

build\$(build)\obj\program.obj: src\pmi\program.asm &
	src\pmi\config.inc &
	src\pmi\api\pmi.inc &
	src\pmi\api\kernel.inc &
	src\pmi\api\memory.inc &
	src\pmi\api\file.inc &
	src\pmi\consts\memory.inc &
	src\pmi\structs\memory.inc &
	src\pmi\structs\program.inc

	$(nasm_dir)nasm $(nasm_dos_opts) $[@ -o $^@


#------------------------------------------------------------------------------
# Build PMI runtime library
#------------------------------------------------------------------------------

# Create directory for RTL .obj files

build\$(build)\rtl: build build\$(build) .SYMBOLIC .ALWAYS
	@if not exist build\$(build)\rtl mkdir build\$(build)\rtl

# .inc file dependencies

src\rtl\api\log.inc: &
	src\rtl\consts\log.inc

	$(watcom_dir)wtouch src\rtl\api\log.inc

src\rtl\api\string.inc: &
	src\rtl\consts\string.inc

	$(watcom_dir)wtouch src\rtl\api\string.inc

# .obj file dependencies with included external files and build instructions

build\$(build)\rtl\env_arg.obj: src\rtl\env_arg.asm &
	src\pmi\api\pmi.inc

	$(nasm_dir)nasm $(nasm_pe_opts) $[@ -o $^@

build\$(build)\rtl\irq.obj: src\rtl\irq.asm &
	src\pmi\api\pmi.inc

	$(nasm_dir)nasm $(nasm_pe_opts) $[@ -o $^@

build\$(build)\rtl\keyboard.obj: src\rtl\keyboard.asm &
	src\pmi\api\pmi.inc &
	src\rtl\api\irq.inc &
	src\rtl\consts\keyboard.inc &
	src\rtl\kblayout\us.inc

	$(nasm_dir)nasm $(nasm_pe_opts) $[@ -o $^@

build\$(build)\rtl\log.obj: src\rtl\log.asm &
	src\pmi\api\pmi.inc &
	src\pmi\structs\memory.inc &
	src\pmi\consts\memory.inc &
	src\pmi\structs\program.inc &
	src\rtl\api\string.inc &
	src\rtl\consts\log.inc

	$(nasm_dir)nasm $(nasm_pe_opts) $[@ -o $^@

build\$(build)\rtl\profiler.obj: src\rtl\profiler.asm &
	src\pmi\api\pmi.inc &
	src\rtl\api\systimer.inc

	$(nasm_dir)nasm $(nasm_pe_opts) $[@ -o $^@

build\$(build)\rtl\string.obj: src\rtl\string.asm &
	src\rtl\consts\string.inc

	$(nasm_dir)nasm $(nasm_pe_opts) $[@ -o $^@

build\$(build)\rtl\systimer.obj: src\rtl\systimer.asm &
	src\pmi\api\pmi.inc &
	src\rtl\api\irq.inc

	$(nasm_dir)nasm $(nasm_pe_opts) $[@ -o $^@

build\$(build)\rtl\timer.obj: src\rtl\timer.asm &
	src\pmi\api\pmi.inc &
	src\rtl\api\irq.inc

	$(nasm_dir)nasm $(nasm_pe_opts) $[@ -o $^@
