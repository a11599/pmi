#------------------------------------------------------------------------------
# Protected mode interface (PMI) makefile
#------------------------------------------------------------------------------

# Compiler options

nasm_dos_opts = -i "src" -f obj
nasm_pe_opts = -i "src" -f win32

# Target file names and directories

dist_bin = bin				# PMI binary EXE file target directory
dist_rtl = rtl				# Runtime library object file target dir

# Target stub EXE file without extension

pmi = pmi
pmi_dbg = pmi_dbg

# List of runtime library object files

rtl = $(dist_rtl) &
	$(dist_rtl)\env_arg.obj &
	$(dist_rtl)\irq.obj &
	$(dist_rtl)\keyboard.obj &
	$(dist_rtl)\log.obj &
	$(dist_rtl)\profiler.obj &
	$(dist_rtl)\string.obj &
	$(dist_rtl)\systimer.obj &
	$(dist_rtl)\timer.obj

# Build targets
#
# debug (default)
# Incremental build with exception dump enabled as pmi_dbg.exe, keeps object
# files.
#
# release
# Clean build without exception dump as pmi.exe. Deletes PMI object files after
# the build.

debug: enable_debug $(dist_bin)\$(pmi_dbg).exe $(rtl)
release: clean $(dist_bin)\$(pmi).exe clean_pmi_obj $(rtl)


#------------------------------------------------------------------------------
# Build PMI stub
#------------------------------------------------------------------------------

# List of PMI stub objects

pmi_objs = &
	obj\pmi.obj &
	obj\kernel.obj &
	obj\memory.obj &
	obj\file.obj &
	obj\program.obj &
	obj\dma.obj

# Enable debug build

enable_debug: .SYMBOLIC
	set EXCEPTIONS=1

# Cleanup

clean: clean_pmi_obj clean_rtl_obj .SYMBOLIC
	set EXCEPTIONS=
	if exist $(dist_bin)\$(pmi).exe del $(dist_bin)\$(pmi).exe

clean_pmi_obj: .SYMBOLIC .MULTIPLE
	if exist obj del obj\*.obj >nul
	if exist obj del obj\*.lnk >nul

clean_rtl_obj: .SYMBOLIC .MULTIPLE
	if exist rtl del rtl\*.obj >nul

# Create directory for binary files

$(dist_bin): .SYMBOLIC .ALWAYS
	@if not exist $(dist_bin) mkdir $(dist_bin)

# Binary build and link

$(dist_bin)\$(pmi_dbg).exe: obj $(pmi_objs) $(dist_bin)
	@%write obj\$(pmi_dbg).lnk NAME $(dist_bin)\$(pmi_dbg)
	@%write obj\$(pmi_dbg).lnk OPTION dosseg
	@%write obj\$(pmi_dbg).lnk OPTION map=obj\$(pmi_dbg).map
	@%write obj\$(pmi_dbg).lnk OPTION packcode=0
	@%write obj\$(pmi_dbg).lnk OPTION packdata=0
	@%write obj\$(pmi_dbg).lnk FORM dos
	@%write obj\$(pmi_dbg).lnk FILE {$(pmi_objs)}
	$(wlink) @obj\$(pmi_dbg).lnk

$(dist_bin)\$(pmi).exe: obj $(pmi_objs) $(dist_bin)
	@%write obj\$(pmi).lnk NAME $(dist_bin)\$(pmi)
	@%write obj\$(pmi).lnk OPTION dosseg
	@%write obj\$(pmi).lnk OPTION map=obj\$(pmi).map
	@%write obj\$(pmi).lnk OPTION packcode=0
	@%write obj\$(pmi).lnk OPTION packdata=0
	@%write obj\$(pmi).lnk FORM dos
	@%write obj\$(pmi).lnk FILE {$(pmi_objs)}
	$(wlink) @obj\$(pmi).lnk

# Create obj directory for .obj files

obj: .SYMBOLIC .ALWAYS
	@if not exist obj mkdir obj

# .obj file dependencies with included external files and build instructions

obj\dma.obj: src\pmi\dma.asm &
	src\pmi\config.inc &
	src\pmi\api\pmi.inc &
	src\pmi\api\kernel.inc &
	src\pmi\structs\dma.inc

	$(nasm) $(nasm_dos_opts) $[@ -o $^@

obj\file.obj: src\pmi\file.asm &
	src\pmi\config.inc &
	src\pmi\api\pmi.inc &
	src\pmi\api\kernel.inc &
	src\pmi\api\memory.inc

	$(nasm) $(nasm_dos_opts) $[@ -o $^@

obj\kernel.obj: src\pmi\kernel.asm &
	src\pmi\config.inc &
	src\pmi\api\pmi.inc &
	src\pmi\api\memory.inc &
	src\pmi\api\file.inc &
	src\pmi\api\dma.inc &
	src\pmi\api\program.inc &
	src\pmi\consts\kernel.inc &
	src\pmi\structs\kernel.inc &
	src\pmi\structs\memory.inc

	$(nasm) $(nasm_dos_opts) $[@ -o $^@

obj\memory.obj: src\pmi\memory.asm &
	src\pmi\config.inc &
	src\pmi\api\pmi.inc &
	src\pmi\api\kernel.inc &
	src\pmi\consts\kernel.inc &
	src\pmi\consts\memory.inc &
	src\pmi\structs\memory.inc

	$(nasm) $(nasm_dos_opts) $[@ -o $^@

obj\pmi.obj: src\pmi\pmi.asm &
	src\pmi\config.inc &
	src\pmi\api\pmi.inc &
	src\pmi\api\kernel.inc &
	src\pmi\consts\kernel.inc &
	src\pmi\structs\kernel.inc

	$(nasm) $(nasm_dos_opts) $[@ -o $^@

obj\program.obj: src\pmi\program.asm &
	src\pmi\config.inc &
	src\pmi\api\pmi.inc &
	src\pmi\api\kernel.inc &
	src\pmi\api\memory.inc &
	src\pmi\api\file.inc &
	src\pmi\consts\memory.inc &
	src\pmi\structs\memory.inc &
	src\pmi\structs\program.inc

	$(nasm) $(nasm_dos_opts) $[@ -o $^@


#------------------------------------------------------------------------------
# Build PMI runtime library
#------------------------------------------------------------------------------

# Create rtl directory for .obj files

$(dist_rtl): .SYMBOLIC .ALWAYS
	@if not exist $(dist_rtl) mkdir $(dist_rtl)

# .inc file dependencies

src\rtl\api\log.inc: &
	src\rtl\consts\log.inc

	wtouch src\rtl\api\log.inc

src\rtl\api\string.inc: &
	src\rtl\consts\string.inc

	wtouch src\rtl\api\string.inc

# .obj file dependencies with included external files and build instructions

$(dist_rtl)\env_arg.obj: src\rtl\env_arg.asm &
	src\pmi\api\pmi.inc

	$(nasm) $(nasm_pe_opts) $[@ -o $^@

$(dist_rtl)\irq.obj: src\rtl\irq.asm &
	src\pmi\api\pmi.inc

	$(nasm) $(nasm_pe_opts) $[@ -o $^@

$(dist_rtl)\keyboard.obj: src\rtl\keyboard.asm &
	src\pmi\api\pmi.inc &
	src\rtl\api\irq.inc &
	src\rtl\consts\keyboard.inc &
	src\rtl\kblayout\us.inc

	$(nasm) $(nasm_pe_opts) $[@ -o $^@

$(dist_rtl)\log.obj: src\rtl\log.asm &
	src\pmi\api\pmi.inc &
	src\pmi\structs\memory.inc &
	src\pmi\consts\memory.inc &
	src\pmi\structs\program.inc &
	src\rtl\api\string.inc &
	src\rtl\consts\log.inc

	$(nasm) $(nasm_pe_opts) $[@ -o $^@

$(dist_rtl)\profiler.obj: src\rtl\profiler.asm &
	src\pmi\api\pmi.inc &
	src\rtl\api\systimer.inc

	$(nasm) $(nasm_pe_opts) $[@ -o $^@

$(dist_rtl)\string.obj: src\rtl\string.asm &
	src\rtl\consts\string.inc

	$(nasm) $(nasm_pe_opts) $[@ -o $^@

$(dist_rtl)\systimer.obj: src\rtl\systimer.asm &
	src\pmi\api\pmi.inc &
	src\rtl\api\irq.inc

	$(nasm) $(nasm_pe_opts) $[@ -o $^@

$(dist_rtl)\timer.obj: src\rtl\timer.asm &
	src\pmi\api\pmi.inc &
	src\rtl\api\irq.inc

	$(nasm) $(nasm_pe_opts) $[@ -o $^@
