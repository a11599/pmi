;==============================================================================
; Protected mode interface - Executable program manager
;==============================================================================

	cpu 386

	group pmi code bss

segment code public use16 class=CODE align=16
segment code

%include "pmi/config.inc"
%include "pmi/api/pmi.inc"
%include "pmi/api/kernel.inc"
%include "pmi/api/memory.inc"
%include "pmi/api/file.inc"
%include "pmi/consts/memory.inc"
%include "pmi/structs/memory.inc"
%include "pmi/structs/program.inc"

%define PMI_FN(fn) pmi_fn + pmi_fns. %+ fn
%define PCB_SIGNATURE 'PCB_'


;------------------------------------------------------------------------------
; Load and run a PE executable file.
;------------------------------------------------------------------------------
; -> EBX - Position within the file where the executable starts
;    ESI - Linear address of ASCIIZ file name of the PE executable
;    EDI - Linear address of ASCIIZ arguments
;    DS - Flat data segment
;    ES - Flat data segment
;------------------------------------------------------------------------------
; <- CF - Set if error
;    AH - PMI or MS-DOS error code if CF set, otherwise 0
;    AL - Exit code or 0 if CF set
;------------------------------------------------------------------------------
; The program starts with the following setup:
; - EAX, ECX, EDX, EBP: 0
; - EBX: Linear address of MS-DOS environment variables
; - ESI: Linear address of ASCIIZ file name of the PE executable
; - EDI: Linear address of ASCIIZ arguments
; - CS:EIP: Program entry point
; - SS:ESP: Stack automatically allocated (full stack reserve pre-allocated)
; - GS: PMI public API jump table segment
; - CS: Flat 32-bit code segment (base: 0, limit: 4 GB)
; - DS, ES, FS, SS: Flat data segment (base: 0, limit: 4 GB)
;------------------------------------------------------------------------------

	[bits 32]

global program_start
program_start:
	push eax
	push ebx
	push ecx
	push edx
	push esi
	push edi
	push ebp
	push ds
	push es
	push fs
	push gs

	cld

	; Initialize variables

	mov ds, cs:[sel_data_krnl32]
	mov [temp_pcb + pcb.args], edi
	mov [temp_pcb + pcb.file], esi
	mov [file_ofs], ebx
	mov dword [temp_pcb + pcb.mem_addr], 0
	mov dword [sect_h_addr], 0
	mov dword [file_handle], 0

	; Open file

	mov ebx, esi
	call fopen
	jc .error
	mov ds, cs:[sel_data_flat]
	call far [cs:PMI_FN(file_get_buf)]
	mov ds, cs:[sel_data_krnl32]
	jc .error
	mov [file_buf], eax

	; Read first 64 bytes to find start of PE executable. The loader
	; supports stubless and MZ stub PE files as well as Phar Lap TNT PL
	; files (which are PE with a different signature so Windows won't try to
	; execute it).

	mov ecx, 0x40
	mov edi, [file_buf]
	call fread
	jc .error
	xor esi, esi			; ESI: PE header offset
	sub edi, [kernel_addr]
	cmp dword [edi], 'PE'		; Stubless PE
	je .seek_pe_opt_hdr
	cmp dword [edi], 'PL'		; Stubless Phar Lap TNT PE
	je .seek_pe_opt_hdr
	cmp word [edi], 'MZ'		; PE/PL with DOS MZ EXE stub
	jne .not_pe

	mov ecx, [edi + 0x3c]		; Seek to PE header
	mov esi, ecx			; ESI: PE header offset
	call fseek
	jc .error
	mov ecx, 0x18			; Read 24 bytes COFF header
	add edi, [kernel_addr]
	call fread
	jc .error
	sub edi, [kernel_addr]
	cmp dword [edi], 'PE'		; PE with MZ EXE stub
	je .parse_header
	cmp dword [edi], 'PL'		; Phar Lap TNT PE with MZ EXE stub
	je .parse_header
	jmp .not_pe

.seek_pe_opt_hdr:
	mov ecx, 0x18			; Seek to optional header for stubless
	call fseek			; PE/PL
	jc .error

.parse_header:
	mov dword [reloc_addr], 0
	movzx eax, word [edi + 0x06]
	test eax, eax
	jz .not_pe			; No sections, nothing to run
	mov [num_sections], eax
	mov ax, [edi + 0x16]		; Check characteristics
	test ax, 0x0002
	jz .not_pe			; Not an executable
	test ax, 0x3000
	jnz .not_pe			; DLL or system file
	test al, 0x01			; With stripped relocation, the image
	jz .read_opt_header		; must be loaded to the preferred base
	mov dword [reloc_addr], -1	; address

.read_opt_header:

	; Read optional header

	movzx ecx, word [edi + 0x14]
	test ecx, ecx
	jz .not_pe			; No optional header: not an executable
	add edi, [kernel_addr]
	call fread
	jc .error
	sub edi, [kernel_addr]
	cmp word [edi], 0x10b
	jne .not_pe			; Not a PE32 executable

	mov eax, [edi + 0x10]		; Save important info for later
	mov [entry_point], eax
	mov eax, [edi + 0x1c]
	mov [image_base], eax
	mov eax, [edi + 0x20]
	mov [sect_alignment], eax
	mov eax, [edi + 0x38]
	mov [size_of_image], eax
	mov eax, [edi + 0x3c]
	mov [size_of_headers], eax
	mov eax, [edi + 0x48]
	mov [stack_size], eax

	cmp cx, 0x90			; Save addresses of directory data
	jb .find_real_base
	mov ebx, [edi + 0x5c]		; We don't use most of them, but they
	cmp ebx, 1			; are saved so we can free memory
	jb .find_real_base		; occupied by these section after
	mov eax, [edi + 0x60]		; loading the PE image
	mov [edata_addr], eax
	cmp ebx, 2
	jb .find_real_base
	mov eax, [edi + 0x68]
	mov [idata_addr], eax
	cmp ebx, 3
	jb .find_real_base
	mov eax, [edi + 0x70]
	mov [rsrc_addr], eax
	cmp ebx, 4
	jb .find_real_base
	mov eax, [edi + 0x78]
	mov [pdata_addr], eax
	cmp ebx, 6
	jb .find_real_base
	mov eax, [edi + 0x88]		; Get relocation table address and size,
	mov [reloc_addr], eax		; this is the only data that we actually
	mov eax, [edi + 0x8c]		; need and use
	mov [reloc_size], eax
	cmp ebx, 7
	jb .find_real_base
	mov eax, [edi + 0x78]
	mov [debug_addr], eax
	cmp ebx, 10
	jb .find_real_base
	mov eax, [edi + 0x78]
	mov [tls_addr], eax
	cmp ebx, 15
	jb .find_real_base
	mov eax, [edi + 0x78]
	mov [cormeta_addr], eax

.find_real_base:
	mov eax, [size_of_headers]	; Get actual base of code and data for
	mov ebx, [sect_alignment]	; memory allocation
	dec ebx
	add eax, ebx
	inc ebx
	xor edx, edx
	div ebx
	mul ebx
	mov [code_base], eax
	mov ecx, [size_of_image]
	sub ecx, eax			; ECX: actual amount of memory for image
	mov [image_mem_size], ecx
	mov edx, eax
	add edx, [image_base]		; EDX: actual base address of image

	; Allocate memory for PE executable image and section headers. First try
	; to allocate memory at the preferred base address so we can prevent
	; doing relocation. If that fails, allocate any suitable memory.

	mov ds, cs:[sel_data_flat]
	call mem_alloc_addr		; Allocate wanted memory for PE image
	mov ds, cs:[sel_data_krnl32]
	jc .alloc_reloc_code
	mov [temp_pcb + pcb.code_base], edx
	mov [temp_pcb + pcb.mem_addr], eax
	jmp .alloc_sect_hdr

.alloc_reloc_code:
	cmp dword [reloc_addr], -1	; Relocations stripped, cannot load to
	je .not_pe			; abritrary memory address
	mov al, PMI_MEM_HI_LO
	mov ds, cs:[sel_data_flat]
	call far [cs:PMI_FN(mem_alloc)]	; Allocate any memory for PE image
	mov ds, cs:[sel_data_krnl32]
	jc .error
	mov [temp_pcb + pcb.code_base], eax
	mov [temp_pcb + pcb.mem_addr], eax

.alloc_sect_hdr:
	mov al, PMI_MEM_HI_LO		; Allocate memory for section header
	mov ecx, [num_sections]
	lea ecx, [ecx * 4 + ecx]
	shl ecx, 3			; ECX: memory needed for section header
	mov ds, cs:[sel_data_flat]
	call far [cs:PMI_FN(mem_alloc)]
	mov ds, cs:[sel_data_krnl32]
	mov [sect_h_addr], eax
	jc .error

	; Read section headers and each section from PE image

	mov edi, eax
	call fread			; Section header follows optional header
	jc .error
	mov esi, edi
	sub esi, [kernel_addr]		; ESI: pointer to section headers
	mov edx, [num_sections]		; EDX: number of sections

.load_section_loop:
	mov ecx, [esi + 0x14]		; Go to start of section
	jecxz .load_section_next	; Not on disk (uninitialized data only)
	call fseek
	jc .error
	mov ecx, [esi + 0x10]		; Size of raw data (to load for section)
	mov edi, [esi + 0x0c]		; Calculate section linear address
	sub edi, [code_base]
	add edi, [temp_pcb + pcb.code_base]
	call fread
	jc .error

.load_section_next:
	add esi, 0x28			; Next section
	dec edx
	jnz .load_section_loop

	call fclose			; Close PE file

	; Apply relocations when necessary

	mov ebp, [temp_pcb + pcb.code_base]
	sub ebp, [code_base]
	mov edi, ebp			; EDI: actual image base
	sub ebp, [image_base]		; EBP: image base offset delta
	jz .free_tables

	mov esi, [reloc_addr]
	sub esi, [code_base]
	add esi, [temp_pcb + pcb.code_base]
	sub esi, [kernel_addr]		; ESI: pointer to relocations
	sub edi, [kernel_addr]		; EDI: pointer to actual image
	mov ecx, [reloc_size]		; ECX: size of relocation table

.reloc_block_loop:
	mov ebx, [esi]			; EBX: relocation block base
	mov edx, [esi + 4]		; EDX: size of relocation block
	sub ecx, edx
	add esi, 8
	sub edx, 8
	jz .reloc_block_next

	push ecx

.reloc_loop:
	mov ax, [esi]
	mov cl, ah
	and eax, 0xfff			; EAX: offset within page
	shr cl, 4			; CL: relocation type
	jz .reloc_next			; Padding, ignore
	add eax, ebx			; EAX: target to relocate
	test cl, 0xfc
	jnz .not_pe			; Unsupported relocation type
	cmp cl, 2
	ja .reloc_dword			; Most common type, branch early
	jb .reloc_highword

	add [edi + eax], bp		; Relocate 16-bit offset (low word)
	jmp .reloc_next

.reloc_highword:
	shrd ecx, ebp, 16
	add [edi + eax], cx		; Relocate 16-bit offset (high word)

.reloc_dword:
	add [edi + eax], ebp		; Relocate 32-bit offset

.reloc_next:
	add esi, 2
	sub edx, 2
	jnz .reloc_loop

	pop ecx

.reloc_block_next:
	test ecx, ecx			; Process next relocation block, if any
	jnz .reloc_block_loop

.free_tables:

	; Find out which sections are unnecessary

	mov edx, [num_sections]		; EDX: number of sections
	mov esi, [sect_h_addr]
	sub esi, [kernel_addr]		; ESI: pointer to first section header

.unused_sect_loop:
	mov eax, [esi + 0x0c]		; EAX: section base
	mov ecx, DROP_SECTS_COUNT	; Check against known unneeded sections

.drop_sect_loop:
	cmp eax, [drop_sects + ecx * 4 - 4]
	je .unused_sect
	dec ecx
	jnz .drop_sect_loop
	jmp .unused_sect_next

.unused_sect:
	mov byte [esi], 0		; Mark section as unused

.unused_sect_next:
	add esi, 0x28
	dec edx
	jnz .unused_sect_loop

	; Find first used section. Sections addresses are adjacent and in
	; ascending address, so we just scan from backwards until we find a
	; section that is used.

	mov edx, [num_sections]
	mov esi, [sect_h_addr]
	sub esi, [kernel_addr]
	lea eax, [edx * 4 + edx - 5]
	shl eax, 3
	add esi, eax			; ESI: pointer to last section header
	xor ecx, ecx			; ECX: base of first unused section

.find_used_section_loop:
	cmp byte [esi], 0
	jne .found_used			; Section used, shrink memory block
	mov ecx, [esi + 0x0c]		; Base of current section
	sub esi, 0x28
	dec edx
	jnz .find_used_section_loop
	jmp .not_pe			; None used, nothing to execute

.found_used:
	jecxz .free_sect_hdr		; Nothing to free
	sub ecx, [code_base]		; ECX: size of memory to keep allocated

	; Free segment headers and unneeded PE section memory

	cmp ecx, [image_mem_size]
	jae .free_sect_hdr		; Nothing to free
	mov ebx, [temp_pcb + pcb.mem_addr]
	sub ebx, mcb.strucsize
	mov dh, MEM_SPLIT_NOR
	mov ds, cs:[sel_data_flat]
	call mem_split_block		; Free memory block of unused sections
	mov ebx, eax
	mov ecx, [eax + mcb.next]
	call mem_merge_blocks		; Merge with next block when possible
	mov ds, cs:[sel_data_krnl32]

.free_sect_hdr:
	mov eax, [sect_h_addr]
	mov ds, cs:[sel_data_flat]
	call far [cs:PMI_FN(mem_free)]	; Free section header memory block
	mov ds, cs:[sel_data_krnl32]
	mov dword [sect_h_addr], 0

	; Initialize runtime environment

	mov dword [temp_pcb + pcb.signature], PCB_SIGNATURE
	mov eax, [current_pcb]
	mov dword [temp_pcb + pcb.caller], eax
	mov [temp_pcb + pcb.ret_stack], esp
	mov [temp_pcb + pcb.ret_stack + 4], ss

	mov al, PMI_MEM_HI_LO		; Allocate memory for PCB instance
	mov ecx, pcb.strucsize
	mov ds, cs:[sel_data_flat]
	call far [cs:PMI_FN(mem_alloc)]
	mov ds, cs:[sel_data_krnl32]
	jc .error
	mov edi, eax			; EDI: PCB instance address

	mov al, PMI_MEM_HI_LO		; Allocate memory for stack - this
	mov ecx, [stack_size]		; normally allocates memory below PCB
	mov ds, cs:[sel_data_flat]	; so there is less chance for corruption
	call far [cs:PMI_FN(mem_alloc)]
	mov ds, cs:[sel_data_krnl32]
	jc .error_stack
	mov [temp_pcb + pcb.stack_addr], eax
	lea ebp, [eax + ecx]		; EBP: top of stack for program

	mov ebx, edi			; EBX: PCB instance address
	mov esi, temp_pcb
	mov ecx, pcb.strucsize / 4
	rep movsd			; Copy temporary PCB to instance

	; Transfer control to program

	mov [current_pcb], ebx		; Set current PCB to new program
	mov esp, ebp			; Use program stack

	mov eax, [entry_point]
	add eax, [temp_pcb + pcb.code_base]
	sub eax, [code_base]		; EAX: linear address of entry point
	push dword [sel_code_flat]	; Program runs in flat memory model
	push eax			; Entry point offset

	movzx ebx, word [env_seg]	; Initialize registers
	shl ebx, 4			; EBX: pointer to environment variables
	xor ecx, ecx
	xor edx, edx
	xor ebp, ebp
	mov edi, [temp_pcb + pcb.args]	; ESI: pointer to ASCIIZ arguments
	mov esi, [temp_pcb + pcb.file]	; EDI: pointer to ASCIIZ filename
	mov gs, [sel_pmi_fn]		; GS: PMI public API jump table
	mov ax, [sel_data_flat]
	mov ds, ax			; DS: flat data segment
	mov es, ax			; ES: flat data segment
	mov fs, ax			; FS: flat data segment
	xor eax, eax
	retf				; Jump to program

.not_pe:
	mov eax, PMI_E_INV_PE

.error_stack:
	push eax
	mov eax, edi
	mov ds, cs:[sel_data_flat]
	call far [cs:PMI_FN(mem_free)]	; Free PCB instance memory
	mov ds, cs:[sel_data_krnl32]
	pop eax

.error:

	; Cleanup after error, free allocated memory blocks and close PE file
	; EAX: PMI or MS-DOS error code

	push eax
	mov eax, [temp_pcb + pcb.mem_addr]
	test eax, eax
	jz .error_free_sect_hdr
	mov ds, cs:[sel_data_flat]
	call far [cs:PMI_FN(mem_free)]	; Free PE image memory block
	mov ds, cs:[sel_data_krnl32]

.error_free_sect_hdr:
	mov eax, [sect_h_addr]
	test eax, eax
	jz .error_free_done
	mov ds, cs:[sel_data_flat]
	call far [cs:PMI_FN(mem_free)]	; Free section header memory block
	mov ds, cs:[sel_data_krnl32]

.error_free_done:
	pop eax

	shl eax, 8			; AH: error code, AL: 0
	push eax
	call fclose			; Close file
	pop eax
	stc

.exit:
	pop gs
	pop fs
	pop es
	pop ds
	pop ebp
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	mov [esp], ax			; Update low word of EAX on stack
	pop eax
	retf


;------------------------------------------------------------------------------
; Terminates the currently running program.
;------------------------------------------------------------------------------
; -> AL - Exit code
;------------------------------------------------------------------------------

	[bits 32]

program_terminate:
	mov ds, cs:[sel_data_krnl32]
	mov [exit_code], al
	cmp dword [current_pcb], 0
	je .stop_kernel

	mov esi, [current_pcb]
	sub esi, [kernel_addr]		; ESI: pointer to PCB
	cmp dword [esi + pcb.signature], PCB_SIGNATURE
	jne .panic			; Invalid PCB, panic
	lss esp, [esi + pcb.ret_stack]	; Restore caller stack frame
	mov edi, temp_pcb
	add edi, [kernel_addr]		; EDI: pointer to temporary PCB
	mov ecx, pcb.strucsize / 4
	rep movsd			; Copy to temporary PCB

	mov ds, cs:[sel_data_flat]
	mov eax, cs:[temp_pcb + pcb.mem_addr]
	call far [cs:PMI_FN(mem_free)]	; Free program memory block
	mov eax, cs:[temp_pcb + pcb.stack_addr]
	call far [cs:PMI_FN(mem_free)]	; Free program stack
	mov eax, cs:[current_pcb]
	call far [cs:PMI_FN(mem_free)]	; Free program control block
	mov ds, cs:[sel_data_krnl32]

	mov eax, [temp_pcb + pcb.caller]
	mov [current_pcb], eax

	clc
	movzx ax, byte [exit_code]
	jmp program_start.exit

.panic:
	mov byte [exit_error], PMI_E_MEM_INVL

.stop_kernel:
	push dword [sel_code_krnl16]	; Shutdown kernel
	push kernel_shutdown
	retf


;------------------------------------------------------------------------------
; Setup program operations.
;------------------------------------------------------------------------------
; -> DS - Flat data selector
;------------------------------------------------------------------------------

	[bits 32]

global program_setup
program_setup:
	push ds
	mov ds, cs:[sel_data_krnl32]
	mov eax, cs
	mov dword [PMI_FN(terminate)], program_terminate
	mov dword [PMI_FN(terminate) + 4], eax
	mov dword [PMI_FN(execute)], program_start
	mov dword [PMI_FN(execute) + 4], eax
	pop ds

	retn


;------------------------------------------------------------------------------
; Open PE file.
;------------------------------------------------------------------------------
; -> EBX - Linear address of ASCIIZ filename
; <- CF - Set if error
;    EAX - PMI or MS-DOS error code if CF set
;    Destroys EAX, EBX, ECX
;------------------------------------------------------------------------------

	[bits 32]

fopen:
	mov al, PMI_FILE_READ		; Open for read
	push ds
	mov ds, cs:[sel_data_flat]
	call far [cs:PMI_FN(file_open)]
	pop ds
	jc .exit
	mov [file_handle], eax		; Save file handle when successful
	xor ecx, ecx
	call fseek

.exit:
	retn


;------------------------------------------------------------------------------
; Read bytes from currently open PE file.
;------------------------------------------------------------------------------
; -> ECX - Number of bytes to read from file
;    EDI - Linear address of buffer to receive data
; <- CF - Set if error
;    EAX - PMI or MS-DOS error code if CF set
;    Destroys EAX, EBX
;------------------------------------------------------------------------------

	[bits 32]

fread:
	mov ebx, cs:[file_handle]
	push ds
	mov ds, cs:[sel_data_flat]
	call far [cs:PMI_FN(file_read)]
	pop ds
	jc .exit
	cmp eax, ecx
	jne f_not_pe
	clc

.exit:
	retn

f_not_pe:
	mov eax, PMI_E_INV_PE
	stc
	retn


;------------------------------------------------------------------------------
; Close currently open PE file.
;------------------------------------------------------------------------------
; <- CF - Set if error
;    EAX - MS-DOS error code if CF set
;    Destroys EBX
;------------------------------------------------------------------------------

	[bits 32]

fclose:
	mov ebx, cs:[file_handle]
	test ebx, ebx
	jz .exit
	push ds
	mov ds, cs:[sel_data_flat]
	call far [cs:PMI_FN(file_close)]
	pop ds

.exit:
	retn


;------------------------------------------------------------------------------
; Seek pointer within currently open PE file.
;------------------------------------------------------------------------------
; -> ECX - New position
; <- CF - Set if error
;    EAX - Error code if CF is set
;    Destroys EAX, EBX, ECX
;------------------------------------------------------------------------------

	[bits 32]

fseek:
	mov al, PMI_SEEK_START		; Seek from start
	mov ebx, cs:[file_handle]
	test ebx, ebx
	jz f_not_pe
	add ecx, cs:[file_ofs]		; Add file offset
	push ds
	mov ds, cs:[sel_data_flat]
	call far [cs:PMI_FN(file_set_pos)]
	pop ds
	jc .exit
	cmp eax, ecx
	jne f_not_pe
	clc

.exit:
	retn


;==============================================================================
; Data area
;==============================================================================

global current_pcb

		align 4
current_pcb	dd 0			; Pointer to current PCB


;------------------------------------------------------------------------------
; Uninitialized data
;------------------------------------------------------------------------------

segment bss public use16 class=BSS align=4
segment bss

file_handle	resd 1			; PE file handle
file_ofs	resd 1			; File offset to PE executable
file_buf	resd 1			; Pointer to file buffer

num_sections	resd 1			; Number of sections
sect_alignment	resd 1			; Section alignment
entry_point	resd 1			; Image entry point
stack_size	resd 1			; Size of the stack for the program
image_base	resd 1			; Preferred base address
image_mem_size	resd 1			; Size of PE image loaded to memory
size_of_image	resd 1			; Total memory required to load image
size_of_headers	resd 1			; File offset of executable image start
sect_h_addr	resd 1			; Linear address of section headers

code_base	resd 1			; RVA of code base
reloc_size	resd 1			; Relocation table size
drop_sects:
reloc_addr	resd 1			; Relocation table address
edata_addr	resd 1			; Addresses of unneeded sections
idata_addr	resd 1
rsrc_addr	resd 1
pdata_addr	resd 1
debug_addr	resd 1
tls_addr	resd 1
cormeta_addr	resd 1
		DROP_SECTS_COUNT EQU ($ - drop_sects) / 4

temp_pcb	resb pcb.strucsize
exit_code	resb 1
