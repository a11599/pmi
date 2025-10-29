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

pmi = build build$(ps)$(build) build$(ps)$(build)$(ps)obj &
	build$(ps)$(build)$(ps)pmi.exe

# List of runtime library object files

rtl = build build$(ps)$(build) build$(ps)$(build)$(ps)rtl &
	build$(ps)$(build)$(ps)rtl$(ps)env_arg.obj &
	build$(ps)$(build)$(ps)rtl$(ps)irq.obj &
	build$(ps)$(build)$(ps)rtl$(ps)keyboard.obj &
	build$(ps)$(build)$(ps)rtl$(ps)log.obj &
	build$(ps)$(build)$(ps)rtl$(ps)profiler.obj &
	build$(ps)$(build)$(ps)rtl$(ps)string.obj &
	build$(ps)$(build)$(ps)rtl$(ps)systimer.obj &
	build$(ps)$(build)$(ps)rtl$(ps)timer.obj

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

# Build PMI and RTL

incremental: $(pmi) $(rtl)
full: clean $(pmi) $(rtl)

# Create binary distribution package

dist: .SYMBOLIC
	$(watcom_bin_dir)wmake full
	$(watcom_bin_dir)wmake build=release full
	@if not exist dist mkdir dist
	@if not exist dist$(ps)debug mkdir dist$(ps)debug
	@if not exist dist$(ps)debug$(ps)rtl mkdir dist$(ps)debug$(ps)rtl
	@if not exist dist$(ps)release mkdir dist$(ps)release
	@if not exist dist$(ps)release$(ps)rtl mkdir dist$(ps)release$(ps)rtl
	@$(copy) build$(ps)debug$(ps)pmi.exe dist$(ps)debug
	@$(copy) build$(ps)debug$(ps)rtl$(ps)*.obj dist$(ps)debug$(ps)rtl
	@$(copy) build$(ps)release$(ps)pmi.exe dist$(ps)release
	@$(copy) build$(ps)release$(ps)rtl$(ps)*.obj dist$(ps)release$(ps)rtl
	@$(copy) README.md dist

# Cleanup

clean: .SYMBOLIC .MULTIPLE
	@if exist build$(ps)$(build)$(ps)obj $(del) build$(ps)$(build)$(ps)obj$(ps)*.*
	@if exist build$(ps)$(build)$(ps)obj rmdir build$(ps)$(build)$(ps)obj
	@if exist build$(ps)$(build)$(ps)rtl $(del) build$(ps)$(build)$(ps)rtl$(ps)*.*
	@if exist build$(ps)$(build)$(ps)rtl rmdir build$(ps)$(build)$(ps)rtl
	@if exist build$(ps)$(build) $(del) build$(ps)$(build)$(ps)*.*


#------------------------------------------------------------------------------
# Build PMI stub
#------------------------------------------------------------------------------

# List of PMI stub objects

pmi_objs = &
	build$(ps)$(build)$(ps)obj$(ps)pmi.obj &
	build$(ps)$(build)$(ps)obj$(ps)kernel.obj &
	build$(ps)$(build)$(ps)obj$(ps)memory.obj &
	build$(ps)$(build)$(ps)obj$(ps)file.obj &
	build$(ps)$(build)$(ps)obj$(ps)program.obj &
	build$(ps)$(build)$(ps)obj$(ps)dma.obj

# Abort if unknown build environment is given

abort:
	@echo "$(build)" is not a valid build target.
	@%abort

# Create directory for binary files

build: .SYMBOLIC .ALWAYS
	@if not exist build mkdir build

build$(ps)$(build): build .SYMBOLIC .ALWAYS
	@if not exist build$(ps)$(build) mkdir build$(ps)$(build)

build$(ps)$(build)$(ps)obj: build$(ps)$(build) .SYMBOLIC .ALWAYS
	@if not exist build$(ps)$(build)$(ps)obj mkdir build$(ps)$(build)$(ps)obj

# Binary build and link

build$(ps)$(build)$(ps)pmi.exe: $(pmi_objs) build$(ps)$(build)
	@%create build$(ps)$(build)$(ps)obj$(ps)pmi.lnk
	@%write build$(ps)$(build)$(ps)obj$(ps)pmi.lnk NAME build$(ps)$(build)$(ps)pmi
	@%write build$(ps)$(build)$(ps)obj$(ps)pmi.lnk OPTION dosseg
	@%write build$(ps)$(build)$(ps)obj$(ps)pmi.lnk OPTION map=build$(ps)$(build)$(ps)obj$(ps)pmi.map
	@%write build$(ps)$(build)$(ps)obj$(ps)pmi.lnk OPTION packcode=0
	@%write build$(ps)$(build)$(ps)obj$(ps)pmi.lnk OPTION packdata=0
	@%write build$(ps)$(build)$(ps)obj$(ps)pmi.lnk FORM dos
	@%write build$(ps)$(build)$(ps)obj$(ps)pmi.lnk FILE {$(pmi_objs)}
	$(watcom_bin_dir)wlink @build$(ps)$(build)$(ps)obj$(ps)pmi.lnk

# .obj file dependencies with included external files and build instructions

build$(ps)$(build)$(ps)obj$(ps)dma.obj: src$(ps)pmi$(ps)dma.asm &
	src$(ps)pmi$(ps)config.inc &
	src$(ps)pmi$(ps)api$(ps)pmi.inc &
	src$(ps)pmi$(ps)api$(ps)kernel.inc &
	src$(ps)pmi$(ps)structs$(ps)dma.inc

	$(nasm_bin) $(nasm_dos_opts) $[@ -o $^@

build$(ps)$(build)$(ps)obj$(ps)file.obj: src$(ps)pmi$(ps)file.asm &
	src$(ps)pmi$(ps)config.inc &
	src$(ps)pmi$(ps)api$(ps)pmi.inc &
	src$(ps)pmi$(ps)api$(ps)kernel.inc &
	src$(ps)pmi$(ps)api$(ps)memory.inc

	$(nasm_bin) $(nasm_dos_opts) $[@ -o $^@

build$(ps)$(build)$(ps)obj$(ps)kernel.obj: src$(ps)pmi$(ps)kernel.asm &
	src$(ps)pmi$(ps)config.inc &
	src$(ps)pmi$(ps)api$(ps)pmi.inc &
	src$(ps)pmi$(ps)api$(ps)memory.inc &
	src$(ps)pmi$(ps)api$(ps)file.inc &
	src$(ps)pmi$(ps)api$(ps)dma.inc &
	src$(ps)pmi$(ps)api$(ps)program.inc &
	src$(ps)pmi$(ps)consts$(ps)kernel.inc &
	src$(ps)pmi$(ps)structs$(ps)kernel.inc &
	src$(ps)pmi$(ps)structs$(ps)memory.inc

	$(nasm_bin) $(nasm_dos_opts) $[@ -o $^@

build$(ps)$(build)$(ps)obj$(ps)memory.obj: src$(ps)pmi$(ps)memory.asm &
	src$(ps)pmi$(ps)config.inc &
	src$(ps)pmi$(ps)api$(ps)pmi.inc &
	src$(ps)pmi$(ps)api$(ps)kernel.inc &
	src$(ps)pmi$(ps)consts$(ps)kernel.inc &
	src$(ps)pmi$(ps)consts$(ps)memory.inc &
	src$(ps)pmi$(ps)structs$(ps)memory.inc

	$(nasm_bin) $(nasm_dos_opts) $[@ -o $^@

build$(ps)$(build)$(ps)obj$(ps)pmi.obj: src$(ps)pmi$(ps)pmi.asm &
	src$(ps)pmi$(ps)config.inc &
	src$(ps)pmi$(ps)api$(ps)pmi.inc &
	src$(ps)pmi$(ps)api$(ps)kernel.inc &
	src$(ps)pmi$(ps)consts$(ps)kernel.inc &
	src$(ps)pmi$(ps)structs$(ps)kernel.inc

	$(nasm_bin) $(nasm_dos_opts) $[@ -o $^@

build$(ps)$(build)$(ps)obj$(ps)program.obj: src$(ps)pmi$(ps)program.asm &
	src$(ps)pmi$(ps)config.inc &
	src$(ps)pmi$(ps)api$(ps)pmi.inc &
	src$(ps)pmi$(ps)api$(ps)kernel.inc &
	src$(ps)pmi$(ps)api$(ps)memory.inc &
	src$(ps)pmi$(ps)api$(ps)file.inc &
	src$(ps)pmi$(ps)consts$(ps)memory.inc &
	src$(ps)pmi$(ps)structs$(ps)memory.inc &
	src$(ps)pmi$(ps)structs$(ps)program.inc

	$(nasm_bin) $(nasm_dos_opts) $[@ -o $^@


#------------------------------------------------------------------------------
# Build PMI runtime library
#------------------------------------------------------------------------------

# Create directory for RTL .obj files

build$(ps)$(build)$(ps)rtl: build build$(ps)$(build) .SYMBOLIC .ALWAYS
	@if not exist build$(ps)$(build)$(ps)rtl mkdir build$(ps)$(build)$(ps)rtl

# .inc file dependencies

src$(ps)rtl$(ps)api$(ps)log.inc: &
	src$(ps)rtl$(ps)consts$(ps)log.inc

	$(watcom_bin_dir)wtouch src$(ps)rtl$(ps)api$(ps)log.inc

src$(ps)rtl$(ps)api$(ps)string.inc: &
	src$(ps)rtl$(ps)consts$(ps)string.inc

	$(watcom_bin_dir)wtouch src$(ps)rtl$(ps)api$(ps)string.inc

# .obj file dependencies with included external files and build instructions

build$(ps)$(build)$(ps)rtl$(ps)env_arg.obj: src$(ps)rtl$(ps)env_arg.asm &
	src$(ps)pmi$(ps)api$(ps)pmi.inc

	$(nasm_bin) $(nasm_pe_opts) $[@ -o $^@

build$(ps)$(build)$(ps)rtl$(ps)irq.obj: src$(ps)rtl$(ps)irq.asm &
	src$(ps)pmi$(ps)api$(ps)pmi.inc

	$(nasm_bin) $(nasm_pe_opts) $[@ -o $^@

build$(ps)$(build)$(ps)rtl$(ps)keyboard.obj: src$(ps)rtl$(ps)keyboard.asm &
	src$(ps)pmi$(ps)api$(ps)pmi.inc &
	src$(ps)rtl$(ps)api$(ps)irq.inc &
	src$(ps)rtl$(ps)consts$(ps)keyboard.inc &
	src$(ps)rtl$(ps)kblayout$(ps)us.inc

	$(nasm_bin) $(nasm_pe_opts) $[@ -o $^@

build$(ps)$(build)$(ps)rtl$(ps)log.obj: src$(ps)rtl$(ps)log.asm &
	src$(ps)pmi$(ps)api$(ps)pmi.inc &
	src$(ps)pmi$(ps)structs$(ps)memory.inc &
	src$(ps)pmi$(ps)consts$(ps)memory.inc &
	src$(ps)pmi$(ps)structs$(ps)program.inc &
	src$(ps)rtl$(ps)api$(ps)string.inc &
	src$(ps)rtl$(ps)consts$(ps)log.inc

	$(nasm_bin) $(nasm_pe_opts) $[@ -o $^@

build$(ps)$(build)$(ps)rtl$(ps)profiler.obj: src$(ps)rtl$(ps)profiler.asm &
	src$(ps)pmi$(ps)api$(ps)pmi.inc &
	src$(ps)rtl$(ps)api$(ps)systimer.inc

	$(nasm_bin) $(nasm_pe_opts) $[@ -o $^@

build$(ps)$(build)$(ps)rtl$(ps)string.obj: src$(ps)rtl$(ps)string.asm &
	src$(ps)rtl$(ps)consts$(ps)string.inc

	$(nasm_bin) $(nasm_pe_opts) $[@ -o $^@

build$(ps)$(build)$(ps)rtl$(ps)systimer.obj: src$(ps)rtl$(ps)systimer.asm &
	src$(ps)pmi$(ps)api$(ps)pmi.inc &
	src$(ps)rtl$(ps)api$(ps)irq.inc

	$(nasm_bin) $(nasm_pe_opts) $[@ -o $^@

build$(ps)$(build)$(ps)rtl$(ps)timer.obj: src$(ps)rtl$(ps)timer.asm &
	src$(ps)pmi$(ps)api$(ps)pmi.inc &
	src$(ps)rtl$(ps)api$(ps)irq.inc

	$(nasm_bin) $(nasm_pe_opts) $[@ -o $^@
