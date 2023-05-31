;==============================================================================
; Protected mode interface - Kernel
;==============================================================================

	cpu 386

	group pmi code bss

segment code public use16 class=CODE align=16
segment code

%include "pmi/config.inc"
%include "pmi/api/pmi.inc"
%include "pmi/api/memory.inc"
%include "pmi/api/file.inc"
%include "pmi/api/dma.inc"
%include "pmi/api/program.inc"
%include "pmi/consts/kernel.inc"
%include "pmi/structs/kernel.inc"
%include "pmi/structs/memory.inc"

%define	DPMI_TERM_STACK_SIZE 0x200
%define PMI_FN(fn) pmi_fn + pmi_fns. %+ fn


;------------------------------------------------------------------------------
; Start protected mode kernel.
;------------------------------------------------------------------------------
; -> EBX - Base address of available conventional memory
;    ECX - Size of available conventional memory
;    ES - PSP segment
; <- CF - Set if error
;    AH - Error code if CF set (PMI_E_*), otherwise 0
;    AL - Exit code or 0 if CF set
;------------------------------------------------------------------------------
; When compiled with DEBUG_EXCEPTIONS:
; <- ES:DI - Pointer to exception_dump_data structure when AH = PMI_E_EXCEPTION
;------------------------------------------------------------------------------
; Under DPMI, if the selectors used by PMI cannot be allocated, the kernel will
; terminate straight to DOS without returning to the caller. The error message
; defined in msg_err_dpmi global variable is printed to stdout using
; DOS function 0x09 before terminating.
;------------------------------------------------------------------------------

	[bits 16]

global kernel_start
kernel_start:
	push ebx
	push ecx
	push edx
	push esi
	push ebp
	push ds
	push fs
	push gs

	push eax
	push es
	push edi

	;----------------------------------------------------------------------
	; Real mode initialization

	mov ax, cs
	mov ds, ax			; DS: PMI segment

	mov [psp_seg], es		; Save PSP segment
	mov ax, es:[0x2c]
	mov [env_seg], ax		; Save environment segment
	mov eax, ebx			; Align conventional memory to paragraph
	add ebx, 0xf
	and ebx, 0xfffffff0
	sub eax, ebx
	sub ecx, eax
	and ecx, 0xfffffff0
	mov [cmb_base], ebx
	mov [cmb_size], ecx

	mov ah, 0x0f			; Save current video mode
	int 0x10
	mov [startup_vidmode], al
	mov [pmi_stack], sp		; Save current stack for exiting
	mov [pmi_stack + 2], ss

	call detect_host		; Detect host environment
	call setup_host_rm		; Setup host environment - real mode
	jc .error_host_setup
	call setup_pointers		; Initialize pointers
	call flush_kbd_buf		; Flush keystrokes from keyboard buffer

	cli				; Disable interrupts

	call setup_interrupts		; Initialize interrupts
	mov bl, [irq0_base]		; Remap PIC base interrupt vectors
	mov bh, [irq8_base]
	call remap_pic

	sti				; Enable interrupts

	lss sp, [rm_stack]		; Switch to real mode stack
	push dword [sel_code_krnl16]
	push dword .pm_setup
	jmp [rm2pm]

.error_host_setup:
	pop edi				; Restore EDI and ES
	pop es

	xor al, al			; Error setting up protected mode host
	jmp kernel_shutdown.error	; Return to caller

.pm_setup:

	;----------------------------------------------------------------------
	; 16-bit protected mode setup

	mov ds, cs:[sel_data_krnl32]
	call setup_host_pm		; Setup host environment - pmode

	push dword cs:[sel_code_krnl32]
	push dword .pm32_setup
	o32 retf

	[bits 32]

.pm32_setup:

	;----------------------------------------------------------------------
	; 32-bit protected mode setup

	mov ds, cs:[sel_data_flat]
	call mem_setup			; Initialize memory management
	call file_setup			; Initialize file operations
	call dma_setup			; Initialize DMA services
	call program_setup		; Initialize executable program manager

	; Attempt to run self (as stub)

	movzx edi, word cs:[psp_seg]	; Get arguments
	shl edi, 4
	movzx ebx, byte [edi + 0x80]	; Argument length
	add edi, 0x81
	mov byte [edi + ebx], 0		; Zero-terminate arguments
	movzx esi, word cs:[env_seg]
	shl esi, 4			; ESI: environment pointer
	dec esi

.search_file_loop:
	inc esi
	cmp word [esi], 0
	jne .search_file_loop
	add esi, 4
	xor ebx, ebx
	push cs
	call program_start
	mov ds, cs:[sel_data_krnl32]
	mov [exit_error], ah

	; All programs terminated, shutdown kernel

	push dword cs:[sel_code_krnl16]
	push kernel_shutdown
	retf


;------------------------------------------------------------------------------
; Shutdown protected mode kernel and return to caller of kernel_start.
;------------------------------------------------------------------------------
; -> AL - Exit code
;------------------------------------------------------------------------------

	[bits 16]

global kernel_shutdown
kernel_shutdown:
	mov ds, cs:[sel_data_krnl32]	; DS: PMI flat data segment
	mov [exit_code], al		; Save exit code

	call dma_shutdown		; Shutdown DMA services
	call shutdown_host_pm		; Restore host environment - pmode

	; Go back to real mode

	push word [kernel_seg]
	push word .rm_cleanup
	jmp far dword [pm2rm]

.rm_cleanup:
	mov ax, cs
	mov ds, ax			; DS: PMI segment
	lss sp, [pmi_stack]		; Restore initial stack

	cli

	mov bl, [old_irq0_base]		; Restore PIC mappings
	mov bh, [old_irq8_base]
	call remap_pic
	call restore_interrupts		; Restore interrupts

	sti

	call shutdown_host_rm		; Restore host environment - real mode
	call flush_kbd_buf		; Flush keystrokes from keyboard buffer

	cmp byte [exit_error], 0
	jne .panic

	pop edi
	pop es
	pop eax

	xor ah, ah			; Error code (0, no error)
	mov al, [exit_code]

	clc

.exit:
	pop gs
	pop fs
	pop ds
	pop ebp
	pop esi
	pop edx
	pop ecx
	pop ebx
	retn

.panic:
	pop edi				; Restore EDI and ES
	pop es

	mov ah, 0x0f			; Restore startup video mode
	int 0x10
	mov ah, al
	mov al, [startup_vidmode]
	cmp al, ah
	je .check_exception
	xor ah, ah
	int 0x10

.check_exception:
	%ifdef DEBUG_EXCEPTIONS
	cmp byte [exit_error], PMI_E_EXCEPTION
	jne .terminate
	mov ax, cs
	mov es, ax
	mov di, exception_dump
	%endif

.terminate:
	xor al, al			; Exit code (0)
	mov ah, [exit_error]		; Error code

.error:
	pop ebx				; Discard AX from stack, keep high word
	mov bx, ax
	mov eax, ebx
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Exit to DOS.
;------------------------------------------------------------------------------
; -> AL - Exit code
;------------------------------------------------------------------------------
; The main thread must call this instead of DOS function 0x4c in order to
; enable a clean termination of the DPMI client. The reason is that there is
; no method to switch back permanently to real/V86 mode once protected mode
; was initialized via DPMI. Hence the termination routines are only simulated
; via a real mode procedure call. This method recognizes this case and will
; either do a normal DOS terminate call or return back to protected mode and
; make a DOS terminate call through the DPMI host.
;------------------------------------------------------------------------------

global exit_to_dos
exit_to_dos:
	push .terminate
	.terminate_proc EQU $ - 2
	retn

.terminate:
	mov ah, 0x4c
	int 0x21


;------------------------------------------------------------------------------
; Detect protected mode host environment.
;------------------------------------------------------------------------------
; -> DS - PMI segment
; <- [env_flags] - Set from detected environment
;    Destroys everything except segment registers
;------------------------------------------------------------------------------

	[bits 16]

detect_host:
	%define .env_flags word [env_flags]

	; Get DOS version

	mov ax, 0x3000
	int 0x21
	rol ax, 8			; AH: major version, AL: minor version
	mov [dos_ver], ax

	mov .env_flags, 0

	; Check if loaded to upper memory

	mov ax, cs
	cmp ax, 0xa000
	jb .check_dpmi
	or .env_flags, ENV_MEM_LOADHI

.check_dpmi:

	; Detect DPMI host presence

	mov ax, 0x1687
	int 0x2f
	test ax, ax
	jnz .check_vcpi			; DPMI host not present
	test bl, 1
	jz .check_vcpi			; 16-bit DPMI host
	or .env_flags, ENV_DPMI
	test dh, dh
	jz .dpmi			; DPMI 0.9 host
	or .env_flags, ENV_DPMI1

.dpmi:
	mov [dpmi_rm2pm], di		; DPMI real to protected mode switch ptr
	mov [dpmi_rm2pm + 2], es
	mov [dpmi_host_mem], si		; Paragraphs required by DPMI host

.check_vcpi:

	; Detect VCPI host presence

	mov ax, 0x3d00			; Try to open 'EMMXXXX0'
	mov dx, emmxxxx0
	int 0x21
	jc .check_xms			; No EMS manager installed
	mov bx, ax			; File handle
	mov ax, 0x4400			; Check if it's a device driver
	int 0x21
	jc .no_vcpi
	test dl, 0x80
	jz .no_vcpi			; Not a device driver
	mov ax, 0x4407			; Check device driver output status
	int 0x21
	jc .no_vcpi
	test al, al
	jz .no_vcpi			; Not ready
	mov ah, 0x3e			; Close file
	int 0x21

	mov ax, 0xde00			; Check VCPI host availability
	int 0x67
	test ah, ah
	jnz .check_xms
	or .env_flags, ENV_VCPI
	jmp .check_xms

.no_vcpi:
	mov ah, 0x3e			; Close file
	int 0x21

.check_xms:

	; Detect XMS

	mov ax, 0x4300
	int 0x2f
	cmp al, 0x80
	jne .check_v86			; No XMS manager installed
	or .env_flags, ENV_XMS
	mov ax, 0x4310
	int 0x2f
	mov [xmm], bx			; XMS manager API entry point
	mov [xmm + 2], es

.check_v86:

	; Check if running in a V86 task

	smsw ax
	test al, 0x01
	jz .check_fast_a20
	or .env_flags, ENV_CPU_V86

.check_fast_a20:

	; Check fast A20 gate control support

	mov ax, 0x2403
	int 0x15
	jc .done
	test ah, ah
	jnz .done
	test bl, 0x02
	jz .done
	or .env_flags, ENV_A20_FAST

.done:
	retn


;------------------------------------------------------------------------------
; Initialize various pointers for protected and real mode.
;------------------------------------------------------------------------------
; -> DS - PMI segment
; <- Destroys everything except segment registers
;------------------------------------------------------------------------------

	[bits 16]

setup_pointers:
	xor eax, eax
	mov ax, cs
	mov [kernel_seg], ax		; Kernel code/data segment
	mov [r_pm2rm.kernel_seg], ax
	shl eax, 4
	mov [kernel_addr], eax		; Kernel code/data linear address
	or [gdt_code_krnl32 + 2], eax	; Kernel 32-bit code selector base
	or [gdt_data_krnl32 + 2], eax	; Kernel 32-bit data selector base
	or [gdt_code_krnl16 + 2], eax	; Kernel 16-bit code selector base

	add [vcpi_rm2pm_data + v_rm2pm_data.gdt_reg_addr], eax
	add [vcpi_rm2pm_data + v_rm2pm_data.idt_reg_addr], eax
	add [vcpi_rm2pm_addr], eax

	add [gdt_base], eax		; GDT linear address
	add eax, pmi_fn
	or [gdt_pmi_fn + 2], eax	; PMI public API jump table structure

	mov eax, [host_mem_addr]
	add [idt_base], eax		; IDT linear address
	add [vcpi_stack], eax		; VCPI temporary mode switch stack
	add eax, v_priv_data.tss
	or [gdt_tss + 2], eax		; TSS selector base

	xor eax, eax
	mov ax, [pmi_stack + 2]
	shl eax, 4
	or [gdt_stack_rm + 2], eax	; 16-bit PMI stack selector base

	mov ax, 0x2523			; Install custom Ctrl-Break handler
	mov dx, dos_ctrl_break_handler
	int 0x21
	mov ax, 0x2524			; Install custom critical error handler
	mov dx, dos_critical_error_handler
	int 0x21

	retn


;------------------------------------------------------------------------------
; Disable DOS Control-Break abort.
;------------------------------------------------------------------------------

dos_ctrl_break_handler:
	iret				; Ignore Ctrl-Break


;------------------------------------------------------------------------------
; Disable critical error handler, send error code straight back to application.
;------------------------------------------------------------------------------

dos_critical_error_handler:
	add sp, 8			; Discard DOS return address and AX
	mov ax, 0x05			; Set error code in AX to access denied
	mov bp, sp
	or word [bp + 20], 0x0001	; Set CF in application return stack
	pop bx				; Restore application registers
	pop cx
	pop dx
	pop si
	pop di
	pop bp
	pop ds
	pop es
	iret				; Return to application (not DOS)


;------------------------------------------------------------------------------
; Setup protected mode host environment in real mode.
;------------------------------------------------------------------------------
; -> DS - PMI segment
; <- CF - Set if error
;    AH - Error code if CF set
;    FS - Host private data segment
;    Destroys everything except segment registers
;------------------------------------------------------------------------------

	[bits 16]

setup_host_rm:
	push es
	push gs

	%define .env_flags word [env_flags]

	cld

	test .env_flags, ENV_CPU_V86
	jz .no_pm_host
	test .env_flags, ENV_DPMI | ENV_VCPI
	jz .err_v86

	mov byte [a20_type], A20_ENABLED

	%if (DPMI_PREFERRED > 0)
	test .env_flags, ENV_DPMI
	jnz .dpmi			; DPMI preferred
	%else
	test .env_flags, ENV_VCPI
	jz .dpmi			; VCPI preferred
	%endif

	;----------------------------------------------------------------------
	; VCPI host present

	mov byte [pm_host_type], PMI_HOST_VCPI
	mov ecx, v_priv_data.strucsize
	mov edx, 2			; 1 page table + 1 page directory
	call alloc_host_mem
	jc .exit

	; Clear page directory and first page table with zeroes

	mov dword [pt_count], 1
	mov ebx, [page_mem_addr]
	shr ebx, 4
	mov es, bx
	xor di, di			; ES:DI: page directory, first page tbl
	mov cx, 0x2000 / 4
	xor eax, eax
	rep stosd			; Init page dir and table with zeroes
	mov gs, bx			; GS: page directory segment
	add bx, 0x100
	mov es, bx			; ES: page table segment

	; Get VCPI host protected mode interface

	xor di, di			; ES:DI: pointer to VCPI page table
	mov si, gdt_code_vcpi
	mov ax, 0xde01
	int 0x67			; Get VCPI protected mode interface
	mov [pte_start], di		; Save first available page table entry
	mov [vcpi_host], ebx
	mov dword [vcpi_host + 4], SEL_CODE_VCPI
	xor al, al			; AL: identity mapping flag
	mov ebx, 0x9f000		; EBX: linear address counter

	; Clear "available" bits in page table entries above 640 KB

.clear_pte_avail_loop:
	and byte es:[di + 1], 0xf1	; Bits 9-11 of PTE: bits 1-3 in 2nd byte
	sub di, 4
	cmp di, 640
	jae .clear_pte_avail_loop

	; Clear "available" bits in page table entries and check identity
	; mapping below 640 KB

.clear_pte_avail_idm_loop:
	and byte es:[di + 1], 0xf1	; Bits 9-11 of PTE: bits 1-3 in 2nd byte
	mov edx, es:[di]
	and dx, 0xf000
	cmp ebx, edx			; Check if PTE linear = physical address
	setne ah			; AH: 1 if linear <> physical address
	or al, ah			; AL: 1 if any mismatch below 640 KB
	sub ebx, 0x1000
	sub di, 4
	jnc .clear_pte_avail_idm_loop

	test al, al
	jnz .setup_page_dir
	or .env_flags, ENV_MEM_IDENT	; Identity mapping

.setup_page_dir:

	; Setup page directory

	mov bx, es
	shr bx, 6			; BX: index into PTE for page tbl start
	mov eax, es:[bx]		; EAX: physical address of page table
	mov gs:[0], eax
	mov bx, gs
	shr bx, 6
	mov ebx, es:[bx]
	and ebx, 0xfffff000		; EAX: physical address of page dir
	mov [page_dir_phaddr], ebx
	mov [vcpi_rm2pm_data + v_rm2pm_data.cr3], ebx

	; Setup VCPI TSS

	mov ax, fs			; Initialize TSS
	mov es, ax
	mov di, v_priv_data.tss		; ES:DI: VCPI TSS
	xor eax, eax
	mov cx, 0x68 / 4
	rep stosd			; Fill TSS with all zeroes
	mov fs:[v_priv_data.tss + 0x1c], ebx
	mov word fs:[v_priv_data.tss + 0x66], 0x68

	; Set host-specific procedure entry points

	mov eax, SEL_CODE_KRNL16
	mov word [rm2pm], v_rm2pm
	mov dword [pm2rm], v_pm2rm
	mov [pm2rm + 4], eax
	mov eax, SEL_CODE_KRNL32
	mov dword [PMI_FN(call_rm)], rv_call_rm
	mov [PMI_FN(call_rm) + 4], eax
	mov dword [PMI_FN(get_irq_hndlr)], rv_get_irq_handler
	mov [PMI_FN(get_irq_hndlr) + 4], eax
	mov dword [PMI_FN(set_irq_hndlr)], rv_set_irq_handler
	mov [PMI_FN(set_irq_hndlr) + 4], eax
	mov dword [PMI_FN(get_env_info)], rvd_get_env_info
	mov [PMI_FN(get_env_info) + 4], eax

	; Save current PIC mappings

	mov ax, 0xde0a
	int 0x67
	mov [old_irq0_base], bl
	mov [old_irq8_base], cl

	; Set real mode stack

	mov ax, v_priv_data.rv_priv_data + rv_priv_data.rm_stacktop
	mov word [rm_stack], ax
	mov [rm_stack + 2], fs

	; Set protected mode stack

	xor eax, eax
	mov ax, [pmi_stack + 2]
	shl eax, 4
	xor ebx, ebx
	mov bx, [pmi_stack]
	add eax, ebx
	and eax, 0xfffffffc		; Make sure stack is dword-aligned
	mov [pm_stack], eax
	mov dword [pm_stack + 4], SEL_DATA_FLAT

	; Try to allocate extended memory with XMS when possible

	test .env_flags, ENV_XMS
	jz .vcpi_memory
	call alloc_xms_memory
	jecxz .vcpi_memory
	mov [xmb_base], ebx
	mov [xmb_size], ecx
	or .env_flags, ENV_MEM_XMS

.check_no_paging:

	; Disable paging when loaded low and conventional memory is identity
	; mapped (physical address = linear address below 640 KB)

	test .env_flags, ENV_MEM_IDENT
	jz .paging
	test .env_flags, ENV_MEM_LOADHI
	jz .no_paging

.paging:

	; Enable paging, map allocated XMS to linear address space

	or .env_flags, ENV_MEM_PAGING
	test .env_flags, ENV_MEM_XMS
	jz .done
	mov ebx, [xmb_base]
	mov ecx, [xmb_size]
	call alloc_map_vcpi_memory
	mov [xmb_base], ebx
	mov [xmb_size], ecx

.no_paging:
	mov dword [pm2rm], v_pm2rm_nopg
	mov dword [vcpi_rm2pm_data + v_rm2pm_data.eip], v_rm2pm.setup_pm_nopg
	jmp .done

.vcpi_memory:

	; Allocate memory from VCPI

	xor ecx, ecx
	call alloc_map_vcpi_memory
	jecxz .check_no_paging		; No extended mem, may turn off paging?
	mov [xmb_base], ebx
	mov [xmb_size], ecx
	or .env_flags, ENV_MEM_VCPI | ENV_MEM_PAGING
	jmp .done

.dpmi:

	;----------------------------------------------------------------------
	; DPMI host present

	mov byte [pm_host_type], PMI_HOST_DPMI
	xor ecx, ecx
	mov cx, [dpmi_host_mem]
	shl ecx, 4
	add ecx, DPMI_TERM_STACK_SIZE	; For DPMI termination stack
	xor edx, edx
	call alloc_host_mem		; Reserve memory for DPMI host

	; Need to shrink the program memory block to play nice with some DPMI
	; hosts during setup, will re-allocate the largest block later when
	; DPMI extended memory is already allocated and locked in setup_host_pm

	xor eax, eax
	mov ax, [psp_seg]
	mov es, ax
	shl eax, 4
	mov ebx, [cmb_base]
	sub ebx, eax
	shr ebx, 4
	mov ah, 0x4a
	int 0x21
	mov dword [cmb_base], 0
	mov dword [cmb_size], 0

	; Set host-specific procedure entry points - only offsets, selectors
	; are added later

	mov word [rm2pm], d_rm2pm
	mov dword [pm2rm], d_pm2rm
	mov dword [PMI_FN(call_rm)], d_call_rm
	mov dword [PMI_FN(get_irq_hndlr)], d_get_irq_handler
	mov dword [PMI_FN(set_irq_hndlr)], d_set_irq_handler
	mov dword [PMI_FN(get_env_info)], rvd_get_env_info

	; IRQ base vectors are fixed at 08h and 70h under DPMI

	mov byte [irq0_base], 0x08
	mov byte [irq8_base], 0x70

	; Set real mode stack for initial protected mode switch (use PMI stack)

	mov eax, [pmi_stack]
	mov [rm_stack], eax

	; Set protected mode stack (only offset, selector will be allocated
	; after entering protected mode)

	xor eax, eax
	mov ax, [pmi_stack + 2]
	shl eax, 4
	xor ebx, ebx
	mov bx, [pmi_stack]
	add eax, ebx
	and eax, 0xfffffffc		; Make sure stack is dword-aligned
	mov [pm_stack], eax

	; DPMI termination temporary stack

	mov eax, [host_mem_addr]
	add eax, DPMI_TERM_STACK_SIZE
	mov [dpmi_term_stack], eax

	; Set memory allocation mode, actual allocation is in protected mode
	; setup

	or .env_flags, ENV_MEM_DPMI
	jmp .done

.no_pm_host:

	;----------------------------------------------------------------------
	; No protected mode host present

	mov byte [pm_host_type], PMI_HOST_RAW
	mov ecx, rv_priv_data.strucsize
	xor edx, edx
	call alloc_host_mem		; Reserve memory for private data
	jc .exit

	; Set host-specific procedure entry points

	mov eax, SEL_CODE_KRNL16
	mov word [rm2pm], r_rm2pm
	mov dword [pm2rm], r_pm2rm
	mov [pm2rm + 4], eax
	mov eax, SEL_CODE_KRNL32
	mov dword [PMI_FN(call_rm)], rv_call_rm
	mov [PMI_FN(call_rm) + 4], eax
	mov dword [PMI_FN(get_irq_hndlr)], rv_get_irq_handler
	mov [PMI_FN(get_irq_hndlr) + 4], eax
	mov dword [PMI_FN(set_irq_hndlr)], rv_set_irq_handler
	mov [PMI_FN(set_irq_hndlr) + 4], eax
	mov dword [PMI_FN(get_env_info)], rvd_get_env_info
	mov [PMI_FN(get_env_info) + 4], eax

	; Set real mode stack

	mov ax, rv_priv_data.rm_stacktop
	mov word [rm_stack], ax
	mov [rm_stack + 2], fs

	; Set protected mode stack

	xor eax, eax
	mov ax, [pmi_stack + 2]
	shl eax, 4
	xor ebx, ebx
	mov bx, [pmi_stack]
	add eax, ebx
	and eax, 0xfffffffc		; Make sure stack is dword-aligned
	mov [pm_stack], eax
	mov dword [pm_stack + 4], SEL_DATA_FLAT

	; CPU in real mode, use XMS when available

	test .env_flags, ENV_XMS	; Try XMS when available
	jz .raw
	mov byte [a20_type], A20_XMS
	call enable_a20
	jc .done			; Cannot enable A20, no extended memory
	call alloc_xms_memory
	jecxz .raw_memory		; Could not allocate XMS, try raw
	mov [xmb_base], ebx
	mov [xmb_size], ecx
	or .env_flags, ENV_MEM_XMS
	jmp .done

.raw:
	; No XMS, try INT 15h extended memory services

	call enable_a20
	jc .done			; Cannot enable A20, no extended memory

.raw_memory:
	call alloc_raw_memory
	jecxz .done
	mov [xmb_base], ebx
	mov [xmb_size], ecx
	or .env_flags, ENV_MEM_RAW
	jmp .done

.err_v86:
	mov ah, PMI_E_V86		; No VCPI or DPMI in V86 mode
	stc
	jmp .exit

.done:
	clc

.exit:
	pop gs
	pop es
	retn


;------------------------------------------------------------------------------
; Setup protected mode host environment in protected mode.
;------------------------------------------------------------------------------
; -> DS - PMI flat data segment
;    ES, SS - Flat data segment
; <- Destroys everything except segment registers
;------------------------------------------------------------------------------

	[bits 16]

setup_host_pm:
	%define .env_flags word [env_flags]
	%define .DPMI_MAX_MEM DPMI_MAX_MEM_MB * 1024 *1024
	%define .DPMI_LOW_RSV_KB DPMI_LOW_RSV_KB * 1024

	;----------------------------------------------------------------------
	; DPMI memory allocation

	test .env_flags, ENV_MEM_DPMI
	jz .exit

	; Allocate extended memory block

	call alloc_dpmi_memory
	jecxz .dpmi_alloc_cmb
	mov [xmb_base], ebx
	mov [xmb_size], ecx

.dpmi_alloc_cmb:

	; Allocate largest conventional memory block

	sub esp, pmi_rm_call.strucsize

	mov byte [esp + pmi_rm_call.flags], PMI_CALL_INT
	mov byte [esp + pmi_rm_call.int], 0x21
	mov ah, 0x48			; Get largest available memory block
	mov bx, 0xffff
	call far dword [cs:PMI_FN(call_rm)]
	jnc .dpmi_mem_done		; Should always fail, something fishy
	xor ecx, ecx
	mov cx, bx
	shl ecx, 4			; ECX: conventional memory block size

	mov eax, ecx			; Leave some conventional memory free
	shr eax, 2			; to allow some breathing space for the
	cmp eax, .DPMI_LOW_RSV_KB	; DPMI host
	jbe .limit_mem
	mov eax, .DPMI_LOW_RSV_KB

.limit_mem:
	sub ecx, eax
	and cl, 0xf0			; Round down to paragraphs

	mov ah, 0x48			; Allocate memory block
	mov ebx, ecx
	shr ebx, 4
	call far dword [cs:PMI_FN(call_rm)]
	jc .dpmi_mem_done		; Shouldn't fail, something wrong
	xor ebx, ebx
	mov bx, ax
	shl ebx, 4			; EBX: conventional memory block base
	mov [cmb_base], ebx
	mov [cmb_size], ecx

.dpmi_mem_done:
	add esp, pmi_rm_call.strucsize

.exit:
	retn


;------------------------------------------------------------------------------
; Reserve conventional memory for host private data.
;------------------------------------------------------------------------------
; -> ECX - Amount of memory needed
;    EDX - Number of 4 KB pages to allocate (page-aligned)
; <- CF - Set if error
;    AH - Error code if CF set
;    FS - Host private data segment if CF is not set
;    EAX, EBX, ECX - Destroyed
;------------------------------------------------------------------------------

	[bits 16]

alloc_host_mem:
	mov ebx, [cmb_base]		; Reserve memory for private data
	mov [host_mem_addr], ebx
	mov eax, ebx
	shr eax, 4
	mov [host_mem_seg], ax
	mov fs, ax			; FS: host private data segment
	test edx, edx
	jnz .alloc_pages

	add ecx, 0xf			; No pages, calculate new low mem base
	and ecx, 0xfffffff0		; ECX: paragraph-aligned size
	add ebx, ecx
	jmp .done

.alloc_pages:
	lea ebx, [ebx + ecx + 0xfff]
	and ebx, 0xfffff000		; Align to page boundary (4 KB)
	mov [page_mem_addr], ebx	; Save allocated page linear address
	mov ecx, edx			; Calculate low memory new base
	shl ecx, 12			; ECX: size of page memory
	add ebx, ecx

.done:
	mov eax, [cmb_base]		; Calculate new low memory size
	add eax, [cmb_size]
	sub eax, ebx
	js .no_ram
	mov [cmb_size], eax
	mov [cmb_base], ebx		; Save new low memory base
	clc

.exit:
	retn

.no_ram:
	mov ah, PMI_E_MEM_LOW
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Restore protected mode host environment before returning to DOS in real mode.
;------------------------------------------------------------------------------
; -> DS - PMI segment
; <- Destroys everything except segment registers
;------------------------------------------------------------------------------

	[bits 16]

shutdown_host_rm:
	%define .env_flags word [env_flags]

	; Free VCPI allocated memory

	test .env_flags, ENV_MEM_VCPI
	jz .free_xms
	call free_vcpi_memory

.free_xms:

	; Free XMS memory block

	test .env_flags, ENV_MEM_XMS
	jz .free_raw
	call free_xms_memory

.free_raw:

	; Free raw extended memory

	test .env_flags, ENV_MEM_RAW
	jz .disable_a20
	call free_raw_memory

.disable_a20:

	; Disable A20 when enabled by PMI

	call disable_a20

	retn


;------------------------------------------------------------------------------
; Restore protected mode host environment before terminating the application in
; protected mode.
;------------------------------------------------------------------------------
; -> DS - PMI flat data segment
; <- Destroys everything except segment registers
;------------------------------------------------------------------------------

shutdown_host_pm:
	%define .env_flags word [env_flags]

	; Free DPMI memory block (required by DPMI 0.9 host)

	test .env_flags, ENV_MEM_DPMI
	jz .exit
	call free_dpmi_memory

.exit:
	retn


;==============================================================================
; Extended memory handling
;==============================================================================

;------------------------------------------------------------------------------
; Raw - Check if A20 line is enabled.
;------------------------------------------------------------------------------
; <- AL - 1 if A20 is enabled, 0 if A20 is disabled
;    Destroys everything except segment registers
;------------------------------------------------------------------------------

check_a20:
	push ds
	push es

	xor ax, ax
	mov es, ax			; ES: 0x0
	dec ax
	mov ds, ax			; DS: 0xffff

	; Write inverted value of 0xffff:0x500 to 0x0:0x4f0. If A20 is disabled,
	; reading 0xffff:0x500 returns the inverted value. Finally, restore
	; original value in 0x0:0x04f0, although this is not strictly necessary
	; since it's the "Inter-Application Communication Area".

	pushf				; Disable interrupts
	cli

	mov ax, [ds:0x500]
	mov bx, [es:0x4f0]
	not ax
	mov es:[0x4f0], ax
	cmp [ds:0x500], ax		; Values match if A20 is disabled
	setne al
	mov [es:0x4f0], bx

	popf				; Restore interrupt state

	pop es
	pop ds
	retn


;------------------------------------------------------------------------------
; Raw - Enable A20 address line.
;------------------------------------------------------------------------------
; -> DS - PMI segment
; <- CF - Set if A20 line could not be enabled, clear otherwise
;    Destroys everything except segment registers
;------------------------------------------------------------------------------

	[bits 16]

enable_a20:
	cmp byte [a20_type], A20_ENABLED
	je .done			; A20 already enabled

	; Use XMS manager when possible

	cmp byte [a20_type], A20_XMS
	jne .raw
	mov ah, 0x05			; Enable A20
	call far [xmm]
	test ax, ax
	jnz .done
	jmp .error			; Unsuccessful, no extended memory

.raw:
	call check_a20
	test al, al
	jz .fast
	mov byte [a20_type], A20_ENABLED
	jmp .done			; A20 already enabled, leave it as-is

.fast:

	; Try I/O port 92h bit 1

	test word [env_flags], ENV_A20_FAST
	jz .bios
	in al, 0x92
	test al, 0x02
	jnz .bios			; A20 already enabled in register
	or al, 0x02			; Set A20 enable bit
	and al, 0xfe			; Clear reset bit to make sure...
	out 0x92, al
	call check_a20
	test al, al
	jz .bios			; Unsuccessful
	mov byte cs:[a20_type], A20_FAST
	jmp .done

.bios:

	; Try PS/2 BIOS extended services

	mov ax, 0x2401			; Enable A20 via BIOS
	int 0x15
	call check_a20
	test al, al
	jz .kbd				; Unsuccessful
	mov byte cs:[a20_type], A20_BIOS
	jmp .done

.kbd:

	; Try keyboard controller at last

	pushf				; Disable interrupts
	cli

	call wait_kbd_command
	mov al, 0xad			; Disable keyboard
	out 0x64, al
	call flush_kbd_data
	call wait_kbd_command
	mov al, 0xd0			; Read from output port
	out 0x64, al
	call wait_kbd_data
	in al, 0x60
	mov ah, al
	call wait_kbd_command
	mov al, 0xd1			; Write to output port
	out 0x64, al
	call wait_kbd_command
	mov al, ah
	or al, 0x02			; Set A20 control (bit 1)
	out 0x60, al
	call wait_kbd_command
	mov al, 0xae			; Enable keyboard
	out 0x64, al
	call wait_kbd_command
	call flush_kbd_data

	popf				; Restore interrupt state

	call check_a20
	test al, al
	jz .error
	mov byte cs:[a20_type], A20_KBD_CTRLR

.done:
	clc

.exit:
	retn

.error:
	mov byte cs:[a20_type], A20_NA
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Raw - Disable A20 address line.
;------------------------------------------------------------------------------
; -> DS - PMI segment
; <- Destroys everything except segment registers
;------------------------------------------------------------------------------

	[bits 16]

disable_a20:
	mov al, [a20_type]

	cmp al, A20_KBD_CTRLR
	je .kbd
	cmp al, A20_BIOS
	je .bios
	cmp al, A20_FAST
	je .fast
	cmp al, A20_XMS
	jne .exit

	; Use XMS manager

	mov ah, 0x06			; Local disable A20 line
	call far [xmm]
	jmp .exit

.fast:

	; Use I/O port 92h bit 1

	in al, 0x92
	test al, 0x02			; A20 already disabled in register
	jz .exit
	and al, 0xfc			; Clear A20 and reset bits
	out 0x92, al
	jmp .exit

.bios:

	; Use PS/2 BIOS extended services

	mov ax, 0x2400			; Disable A20 via BIOS
	int 0x15
	jmp .exit

.kbd:

	; Use keyboard controller

	pushf				; Disable interrupts
	cli

	call wait_kbd_command
	mov al, 0xad			; Disable keyboard
	out 0x64, al
	call flush_kbd_data
	call wait_kbd_command
	mov al, 0xd0			; Read from output port
	out 0x64, al
	call wait_kbd_data
	in al, 0x60
	mov ah, al
	call wait_kbd_command
	mov al, 0xd1			; Write to output port
	out 0x64, al
	call wait_kbd_command
	mov al, ah
	and al, 0xfd			; Clear A20 control (bit 1)
	out 0x60, al
	call wait_kbd_command
	mov al, 0xae			; Enable keyboard
	out 0x64, al
	call wait_kbd_command
	call flush_kbd_data

	popf				; Restore interrupt state

.exit:
	retn


;------------------------------------------------------------------------------
; Wait for the keyboard controller to get ready to accept a command.
;------------------------------------------------------------------------------
; Destroys: AL
;------------------------------------------------------------------------------

	[bits 16]

wait_kbd_command:
	in al, 0x64
	test al, 2
	jnz wait_kbd_command
	retn


;------------------------------------------------------------------------------
; Wait for the keyboard controller to get ready to accept a data byte.
;------------------------------------------------------------------------------
; Destroys: AL
;------------------------------------------------------------------------------

	[bits 16]

wait_kbd_data:
	in al, 0x64
	test al, 1
	jz wait_kbd_data
	retn


;------------------------------------------------------------------------------
; Wait for the keyboard controller to get ready to accept a data byte.
;------------------------------------------------------------------------------
; Destroys: AL
;------------------------------------------------------------------------------

	[bits 16]

flush_kbd_data:
	in al, 0x64
	test al, 1
	jz .exit
	in al, 0x60
	jmp flush_kbd_data

.exit:
	retn


;------------------------------------------------------------------------------
; Flush the keystroke buffer. This is necessary to get rid of ghost keystrokes
; generated by the 8042 "Read from output port" command when enabling the A20
; line.
;------------------------------------------------------------------------------

	[bits 16]

flush_kbd_buf:
	push ax

.flush:
	mov ah, 0x01
	int 0x16
	jz .exit
	xor ah, ah
	int 0x16
	jmp .flush

.exit:
	pop ax
	retn


;------------------------------------------------------------------------------
; Raw - Allocate extended memory block.
;------------------------------------------------------------------------------
; -> DS - PMI segment
; <- EBX - Extended memory block base address (paragraph aligned) or zero if
;          XMS is not available
;    ECX - Extended memory block size (paragraph aligned) or zero if XMS not
;          available
;    Destroys everything except segment registers
;------------------------------------------------------------------------------

	[bits 16]

alloc_raw_memory:
	push es

	xor ax, ax
	mov es, ax			; ES: zeropage

	mov ah, 0x88			; Get size of extended memory
	int 0x15
	test ax, ax
	jz .no_ext_memory

	xor ecx, ecx
	mov cx, ax			; ECX: size of extended memory
	shl ecx, 10
	mov edx, 0x100000		; EDX: base of extended memory

	; Check for VDISK allocations (not supported, not worth the hassle and
	; probably not even used by anything these days)

	les bx, es:[0x19 * 4]		; ES:BX: INT 19h vector
	cmp dword es:[bx + 0x12], 'SIDV'
	jne .check_vdisk_ext
	cmp byte es:[bx + 0x16], 'K'
	je .no_ext_memory

.check_vdisk_ext:
	xor ax, ax			; Check VDISK allocation in extended
	dec ax				; memory if INT 19h absent
	mov es, ax
	cmp dword es:[0x13], 'SDIV'
	jne .use_ext_memory
	cmp byte es:[0x17], 'K'
	jne .no_ext_memory

.use_ext_memory:

	; Hook into INT 15h

	xor ax, ax
	mov es, ax
	mov eax, es:[0x15 * 4]		; Save old INT 15h handler address
	mov [int15_prev_handler], eax
	mov ax, cs
	shl eax, 16
	mov ax, int15_handler
	mov es:[0x15 * 4], eax

.exit:
	pop es
	retn

.no_ext_memory:
	xor ebx, ebx
	xor ecx, ecx
	jmp .exit


;------------------------------------------------------------------------------
; Raw - Free extended memory.
;------------------------------------------------------------------------------
; -> DS - PMI segment
; <- Destroys everything except segment registers
;------------------------------------------------------------------------------

free_raw_memory:
	push es

	xor ax, ax			; Restore original INT 15h handler
	mov es, ax
	mov eax, [int15_prev_handler]
	mov es:[0x15 * 4], eax

	pop es
	retn


;------------------------------------------------------------------------------
; Raw - Handler for INT 15h function 88h to return no available extended
; memory.
;------------------------------------------------------------------------------

	[bits 16]

int15_handler:
	cmp ah, 0x88			; Intercept get extended memory call
	je .fn88
	jmp 0x1234:0x1234
	int15_prev_handler EQU $ - 4

.fn88:
	xor ax, ax			; All extended memory allocated
	iret


;------------------------------------------------------------------------------
; Raw, VCPI - Allocate the largest XMS memory block.
;------------------------------------------------------------------------------
; -> DS - PMI segment
; <- EBX - Extended memory block base address (paragraph aligned) or zero if
;          XMS is not available
;    ECX - Extended memory block size (paragraph aligned) or zero if XMS not
;          available
;    Destroys everything except segment registers
;------------------------------------------------------------------------------

	[bits 16]

alloc_xms_memory:
	mov ah, 0x08			; Get largest block
	call far [xmm]
	test bl, 0x80
	jnz .no_xms_memory
	test ax, ax
	jz .no_xms_memory

	mov dx, ax			; Allocate largest block
	xor ecx, ecx
	mov cx, ax
	shl ecx, 10			; ECX: size of memory block in bytes
	mov ah, 0x09
	call far [xmm]
	cmp ax, 1
	jne .no_xms_memory
	mov bp, dx			; BP: block handle

	mov ah, 0x0c			; Lock memory block
	call far [xmm]
	cmp ax, 1
	jne .no_lock			; Cannot lock memory block
	mov [xmb_handle], bp		; Save memory block handle
	shl edx, 16
	mov dx, bx
	mov ebx, edx
	add ebx, 0xf			; Align address to paragraph boundary
	and ebx, 0xfffffff0		; EBX: memory block base address
	sub edx, ebx			; Align size to paragraph boundary
	sub ecx, edx
	and ecx, 0xfffffff0		; ECX: memory block size

.exit:
	retn

.no_lock:
	mov ah, 0x0a			; Free memory block
	mov dx, bp
	call far [xmm]

.no_xms_memory:
	xor ebx, ebx
	xor edx, edx
	jmp .exit


;------------------------------------------------------------------------------
; Raw, VCPI - Free XMS memory block.
;------------------------------------------------------------------------------
; -> DS - PMI segment
; <- Destroys everything except segment registers
;------------------------------------------------------------------------------

	[bits 16]

free_xms_memory:
	mov dx, [xmb_handle]
	test dx, dx
	jz .exit
	mov word [xmb_handle], 0
	mov ah, 0x0d			; Unlock memory block
	call far [xmm]
	mov ah, 0x0a			; Free memory block
	call far [xmm]

.exit:
	retn


;------------------------------------------------------------------------------
; VCPI - Allocate or map already allocated memory to VCPI linear address space.
;------------------------------------------------------------------------------
; -> DS - PMI segment
; -> EBX - Extended memory block physical base address (paragraph aligned)
;    ECX - Extended memory block size (paragraph aligned) or zero to allocate
;          pages from VCPI memory
; <- EBX - Extended memory block linear base address (paragraph aligned) or
;          zero if allocation/mapping was unsuccessful
;    ECX - Extended memory block size (paragraph aligned) or zero if
;          allocation/mapping was unsuccessful
;    Destroys everything except segment registers
;------------------------------------------------------------------------------
; This should only be called after alloc_host_mem was called to allocate the
; page directory and the first page table. cmb_base must be the same as it was
; set to by alloc_host_mem to ensure the continuity of page tables in linear
; memory address space.
;------------------------------------------------------------------------------

	[bits 16]

alloc_map_vcpi_memory:
	push es
	push fs
	push gs

	%if (VCPI_MAX_MEM_MB > 0)

	cld

	%if (VCPI_MIN_LOW_KB > (IO_BUF_SIZE + mcb.strucsize) >> 10)
	%define .VCPI_MIN_LOW VCPI_MIN_LOW_KB * 1024
	%else
	%define .VCPI_MIN_LOW (IO_BUF_SIZE + mcb.strucsize)
	%endif

	xor edx, edx
	mov eax, [cmb_size]		; Check remaining free conventional mem
	sub eax, .VCPI_MIN_LOW
	js .no_extra_pt			; Low on memory, no extra PT to alloc
	shr eax, 12
	mov edx, eax
	shl edx, 22			; EDX: maximum memory in potential PTs

.no_extra_pt:
	mov eax, 0x1000
	sub ax, [pte_start]
	shl eax, 10			; EAX: memory mappable into first PT
	add edx, eax			; EDX: maximum memory in potential PTEs
	cmp edx, VCPI_MAX_MEM_MB * 1024 * 1024
	jbe .alloc_or_map
	mov edx, VCPI_MAX_MEM_MB * 1024 * 1024
	add edx, eax			; EDX: maximum limited VCPI memory

.alloc_or_map:

	; If extended memory block is given in input, use its physical address
	; to map into the linear range. Otherwise allocate memory from VCPI.

	jecxz .alloc_pages
	mov eax, ebx			; Align base to page boundary
	add ebx, 0xfff
	and bx, 0xf000			; EBX: physical address of page
	sub eax, ebx
	mov bl, 0x07			; User/Writable/Present
	add ecx, eax			; Align size to page boundary
	and cx, 0xf000			; ECX: size of memory block
	cmp edx, ecx
	jae .add_pages
	mov ecx, edx			; ECX: limit to maximum PTE capacity
	jmp .add_pages

.alloc_pages:

	; Create a map of pages below 16 MB so we can order them in the page
	; table so we can have a change for continuous DMA-relevant extended
	; memory. The map uses 4 KB of temporary conventional memory at the top
	; of available heap. The map is a simple 1-byte flag of 0/1 values where
	; 1 represents an allocated page at the physical address of the
	; flag * 4096.

	mov eax, [cmb_base]
	add eax, [cmb_size]
	shr eax, 4
	sub eax, 0x100
	mov ds, ax			; DS: map for DMA-relevant pages
	mov es, ax			; ES: map for DMA-relevant pages
	xor di, di
	xor eax, eax
	mov cx, 0x1000 / 4
	rep stosd			; Zero out DMA page map

	mov ecx, edx			; ECX: maximum amount to allocate
	xor ebx, ebx			; EBX: no base, get from VCPI

	; EBX: page table entry for first page of the block or 0 to allocate
	; ECX: size of memory block or maximum amount to allocate from VCPI pool
	; EDX: page table entry when allocating from VCPI pool
	; SI: segment of current page table
	; DI: offset of current page table entry
	; EBP: memory mapped/allocated so far
	; DS: DMA page map when allocating, otherwise PMI segment
	; ES:DI: pointer to current page table entry
	; FS: first page table segment (to get physical addresses of new PTs)
	; GS: page directory segment (to add new page tables to directory)

.add_pages:
	mov esi, cs:[page_mem_addr]
	shr esi, 4
	mov gs, si			; GS: page directory segment
	add si, 0x100			; SI: page table segment
	mov fs, si			; FS: first page table segment
	mov es, si
	mov di, cs:[pte_start]		; ES:DI: next page table entry
	xor ebp, ebp			; EBP: memory allocated so far

.add_page_loop:

	; Add existing page from allocated extended memory block or allocate a
	; page from VCPI pool and then add that to the page table.

	cmp ebp, ecx
	jae .fill_pt			; Maximum memory allocated
	cmp di, 0x1000
	jb .add_page
	call .next_pt

.add_page:
	test ebx, ebx
	jz .alloc_page
	mov es:[di], ebx		; Use page from extended memory block
	add ebx, 0x1000			; Increase physical address for next PTE
	jmp .add_page_next

.alloc_page:

	; Allocate page and add to the next page table entry or DMA page map
	; when it's below 16 MB

	mov ax, 0xde04
	int 0x67			; Allocate page from VCPI
	test ah, ah
	jnz .fill_pt			; No more, zero out rest of page table
	cmp edx, 0x1000000		; DMA relevant?
	jae .add_vcpi_page

	mov eax, edx			; Set flag in DMA page map
	shr eax, 12
	mov byte [eax], 1
	jmp .add_dma_page_next		; Don't add page to page table

.add_vcpi_page:
	and dh, 0xf0			; Clear available and reserved bits
	mov dl, 0x07			; User/Writable/Present
	mov es:[di], edx

.add_page_next:

	; Continue with next page table entry

	add di, 4

.add_dma_page_next:
	add ebp, 0x1000			; Increase allocated memory amount
	jmp .add_page_loop

.fill_pt:
	test ebx, ebx
	jnz .fill_pt_tail

	; Move DMA pages to the start of the extended memory block

	mov bx, 0x1000
	xor cx, cx
	xor eax, eax			; EAX: number of DMA pages

.count_dma_pages_loop:
	mov cl, [bx - 1]
	add ax, cx
	dec bx
	jnz .count_dma_pages_loop

	mov bx, di			; ES:BX: old last page table entry + 4
	mov dx, ax
	shl dx, 2
	jz .fill_pt_tail		; No DMA pages
	add di, dx			; ES:DI: new last page table entry + 4

	push es				; Allocate new page tables for shifting
	push di

.alloc_dma_pte:
	cmp di, 0x1000
	jbe .move_ptes
	push di
	call .next_pt			; Allocate new page table
	pop di
	sub di, 0x1000
	jmp .alloc_dma_pte

.move_ptes:
	pop di				; Restore last unused PTE pointer
	pop es
	push es
	push di

	mov ecx, ebp
	shr ecx, 12
	sub ecx, eax			; ECX: number of PTEs to move
	jz .add_dma_pages

	sub di, 4			; Pre-decrement, since they point after
	sub bx, 4			; last used PTE
	js .move_pte_prev_pt

.move_pte_loop:
	mov edx, es:[bx]
	mov es:[di], edx		; Move PTE
	dec ecx
	jz .add_dma_pages
	sub di, 4
	sub bx, 4
	jns .move_pte_loop

.move_pte_prev_pt:
	mov dx, es
	sub dx, 0x100
	mov es, dx			; ES: previous page table
	add bx, 0x1000
	add di, 0x1000
	jmp .move_pte_loop

.add_dma_pages:

	xor esi, esi			; DS:ESI: DMA page map
	mov cx, 0x1001			; CX: number of DMA pages + 1 (pre-decr)
	xor eax, eax			; EAX: page physical address

.add_dma_page_loop:
	dec cx
	jz .dma_fill_pt_tail
	inc si
	cmp byte [si - 1], 0
	je .add_dma_page_loop
	lea eax, [esi - 1]
	shl eax, 12			; Calculate physical address
	mov al, 0x07			; User/Writable/Present
	mov es:[bx], eax		; Set page table entry for DMA page
	add bx, 4			; Next PTE
	jmp .add_dma_page_loop

.dma_fill_pt_tail:
	pop di				; Restore first unused PTE pointer
	pop es

	mov ax, di			; Adjust ES:DI to point to the first
	dec ax				; unused page table entry such that DI
	and ax, 0xf000			; won't cross the 0x1000 offset
	sub di, ax
	shr ax, 4
	mov dx, es
	add ax, dx
	mov es, ax

.fill_pt_tail:

	; Zero out remaining part of page table if cannot allocate any more
	; VCPI memory or the end of extended memory block was reached

	mov cx, 0x1000			; Zero out remaining part of page table
	sub cx, di
	jz .done			; Already full, nothing to fill
	shr cx, 2
	xor eax, eax
	rep stosd

.done:
	mov ax, cs
	mov ds, ax			; DS: PMI segment

	; Calculate (new) extended memory base and size and return to caller

	mov ecx, ebp			; ECX: extended memory block size
	jecxz .no_ext_memory
	xor ebx, ebx
	mov bx, [pte_start]
	shl ebx, 10			; EBX: extended memory block base
	jmp .exit

; Allocate memory for a new page table in conventional memory
; -> SI - Last page table segment
; <- SI - Last page table segment adjusted
;    ES:EDI - Pointer to start of allocated page
;    Destroys EDX

.next_pt:

	; Advance to next page table when current is full. Stop adding further
	; pages if the minimum limit of conventional memory or the maximum
	; amount of VCPI memory was reached.

	add si, 0x100			; Adjust to next page table
	mov es, si
	add dword cs:[cmb_base], 0x1000	; Reserve space for page table
	sub dword cs:[cmb_size], 0x1000
	inc dword cs:[pt_count]

	; Add new page to page directory

	mov di, si
	shr di, 6			; BX: index into PTE for page tbl start
	mov edx, fs:[di]		; EAX: physical address of page table
	mov edi, cs:[pt_count]
	mov gs:[edi * 4 - 4], edx	; Set page tbl physical address in dir

	xor di, di			; Continue allocating pages
	retn

	%endif

.no_ext_memory:
	xor ebx, ebx
	xor ecx, ecx

.exit:
	pop gs
	pop fs
	pop es
	retn


;------------------------------------------------------------------------------
; VCPI - Free allocated memory.
;------------------------------------------------------------------------------
; -> DS - PMI segment
; <- Destroys everything except segment registers
;------------------------------------------------------------------------------

	[bits 16]

free_vcpi_memory:
	push ds

	mov esi, [page_mem_addr]
	shr esi, 4			; SI: page table segment
	mov ecx, [pt_count]		; ECX: number of page tables
	mov bx, [pte_start]		; BX: pointer to page table entry

.free_vcpi_pt_loop:
	add si, 0x100			; Skip PD or next PT
	mov ds, si			; DS: page table segment

.free_vcpi_page_loop:
	mov edx, [bx]
	and dx, 0xf000			; Clear page bits
	test edx, edx
	jz .free_next_vcpi_page
	mov ax, 0xde05
	int 0x67			; Free VCPI page

.free_next_vcpi_page:
	add bx, 4
	cmp bx, 0x1000
	jb .free_vcpi_page_loop
	sub bx, 0x1000
	dec ecx
	jnz .free_vcpi_pt_loop

	pop ds
	retn


;------------------------------------------------------------------------------
; DPMI - Allocate and lock extended memory in protected mode.
;------------------------------------------------------------------------------
; -> DS - PMI flat data segment
;    ES, SS - Flat data segment
; <- EBX - Extended memory block linear base address (paragraph aligned) or
;          zero if allocation/mapping was unsuccessful
;    ECX - Extended memory block size (paragraph aligned) or zero if
;          allocation/mapping was unsuccessful
;    Destroys everything except segment registers
;------------------------------------------------------------------------------

	[bits 16]

alloc_dpmi_memory:
	%define .DPMI_EXT_RSV_MB DPMI_EXT_RSV_MB * 1024 * 1024

	mov ax, 0x0604			; Get size of page
	mov ebx, 0x1000			; Default page size is 4 KB
	int 0x31
	jc .get_free_mem_info		; Should not fail. Use default page size
	shl ebx, 16
	mov bx, cx

.get_free_mem_info:
	mov ax, 0x0500			; Get free memory information
	sub esp, 48			; Save memory info in stack area
	mov edi, esp
	int 0x31
	mov eax, [esp + 0x08]		; Get maximum locked page allocation
	cmp eax, -1
	jne .alloc_pages
	mov ecx, [esp]			; No info, allocate all available memory
	jmp .limit_mem

.alloc_pages:
	mul ebx				; Convert pages to bytes
	mov ecx, eax

.limit_mem:
	add esp, 48
	mov eax, ecx			; Leave some extended memory unallocated
	shr eax, 4			; to give breathing room for the DPMI
	cmp eax, .DPMI_EXT_RSV_MB	; server
	jbe .reserve_mem
	mov eax, .DPMI_EXT_RSV_MB

.reserve_mem:
	sub ecx, eax
	and cl, 0xf0			; Round down to paragraphs
	cmp ecx, .DPMI_MAX_MEM		; Limit maximum allocated memory
	jbe .alloc_mem
	mov ecx, .DPMI_MAX_MEM

.alloc_mem:

	; Allocate largest memory block

	mov ax, 0x0501
	shld ebx, ecx, 16
	mov edx, ecx			; EDX: maximum lockable memory
	int 0x31			; BX:CX: linear address of memory block
	jc .no_ext_mem
	mov [xmb_handle], di
	mov [xmb_handle + 2], si

	; Lock memory block (prevent swapping)

	mov ax, 0x0600
	mov di, dx
	shld esi, edx, 16
	int 0x31
	jc .no_ext_mem

	shl ebx, 16
	mov bx, cx			; EBX: base of memory block
	mov ecx, edx			; ECX: size of memory block

.exit:
	retn

.no_ext_mem:
	xor ebx, ebx
	xor ecx, ecx
	jmp .exit


;------------------------------------------------------------------------------
; DPMI - Free allocated extended memory in protected mode.
;------------------------------------------------------------------------------
; -> DS - PMI flat data segment
; <- Destroys everything except segment registers
;------------------------------------------------------------------------------

	[bits 16]

free_dpmi_memory:
	mov ax, 0x0601			; Free memory block
	mov edi, [xmb_handle]
	test edi, edi
	jz .exit
	shld esi, edi, 16
	int 0x31

.exit:
	retn


;==============================================================================
; Mode switch procedures
;==============================================================================

;------------------------------------------------------------------------------
; Raw - Switch to protected mode.
;------------------------------------------------------------------------------
; Do not call this function, it won't return to the caller. Use a near or far
; jump from real mode with the 32-bit target address pushed to the stack frame.
;------------------------------------------------------------------------------
; -> Stack frame:
;    - (dword) Jump target code segment selector
;    - (dword) Jump target code offset
; <- EAX, EBX, ECX, EDX, ESI, EDI, EBP, 16-bit flags - Preserved
;    CS:EIP - Control transferred to jump target
;    SS:ESP - Protected mode stack
;    DS, ES, FS, GS - Flat data segment selector (base: 0, segment limit: 4 GB)
;------------------------------------------------------------------------------

	[bits 16]
	align 4

r_rm2pm:
	pushf				; Disable interrupts
	cli

	mov cs:[.eax], eax		; Save registers
	mov ax, cs
	mov ds, ax
	pop word [.flags]
	pop dword [.eip]
	pop dword [.cs]

	mov [rm_stack], sp		; Save real mode stack top
	mov [rm_stack + 2], ss

	lidt [idt_register]		; Load protected mode IDTR
	lgdt [gdt_register]		; Load GDTR

	mov eax, cr0			; Enter protected mode
	or al, 1
	mov cr0, eax
	jmp dword SEL_CODE_KRNL32:.setup_pm

	align 2
.flags	dw 0

	align 4
.eax	dd 0
.eip	dd 0
.cs	dd 0

	[bits 32]

.setup_pm:
	mov ax, SEL_DATA_FLAT		; Set flat data selectors
	mov ds, ax
	mov es, ax
	mov fs, ax
	mov gs, ax
	lss esp, cs:[pm_stack]		; Load protected mode stack

	push dword cs:[.cs]		; Push return address
	push dword cs:[.eip]

	mov ax, cs:[.flags]		; Restore flags, make sure NT is clear
	and ah, 0xbf
	push ax
	o16 popf

	mov eax, cs:[.eax]		; Restore registers

	retf				; Jump to target address


;------------------------------------------------------------------------------
; VCPI - Switch to protected mode.
;------------------------------------------------------------------------------
; Do not call this function, it won't return to the caller. Use a near or far
; jump from real mode with the 32-bit target address pushed to the stack frame.
;------------------------------------------------------------------------------
; -> Stack frame:
;    - (dword) Jump target code segment selector
;    - (dword) Jump target code offset
; <- EAX, EBX, ECX, EDX, ESI, EDI, EBP, 16-bit flags - Preserved
;    CS:EIP - Control transferred to jump target
;    SS:ESP - Protected mode stack
;    DS, ES, FS, GS - Flat data segment selector (base: 0, segment limit: 4 GB)
;------------------------------------------------------------------------------

	[bits 16]
	align 4

v_rm2pm:
	pushf				; Disable interrupts
	cli

	mov cs:[.eax], eax		; Save registers
	mov cs:[.esi], esi
	mov ax, cs
	mov ds, ax
	pop word [.flags]
	pop dword [.eip]
	pop dword [.cs]

	mov [rm_stack], sp		; Save real mode stack top
	mov [rm_stack + 2], ss

	mov esi, [vcpi_rm2pm_addr]
	mov ax, 0xde0c
	int 0x67			; VCPI switch to protected mode

	align 2
.flags	dw 0

	align 4
.eax	dd 0
.esi	dd 0
.eip	dd 0
.cs	dd 0

	[bits 32]

.setup_pm_nopg:
	mov eax, cr0
	and eax, 0x7fffffff
	mov cr0, eax			; Disable paging
	mov eax, cr3
	and eax, 0xfff
	mov cr3, eax			; Flush TLB

.setup_pm:
	mov ax, SEL_DATA_FLAT		; Set flat data selectors
	mov ds, ax
	mov es, ax
	mov fs, ax
	mov gs, ax
	lss esp, cs:[pm_stack]		; Load protected mode stack

	pushfd				; Keep low 12 bits of flags, clear
	mov ax, cs:[.flags]		; NT and IOPL
	and ah, 0xf
	mov [esp], ax

	mov eax, cs:[.eax]		; Restore registers
	mov esi, cs:[.esi]

	push dword cs:[.cs]		; Push return address
	push dword cs:[.eip]

	iretd				; Jump to target address


;------------------------------------------------------------------------------
; DPMI - Initial switch to protected mode.
;------------------------------------------------------------------------------
; Do not call this function, it won't return to the caller. Use a near or far
; jump from real mode. This is only used once by kernel_start under DPMI, so we
; don't bother preserving registers. It also jumps to a predefined target.
;------------------------------------------------------------------------------
; -> DS - PMI segment
;    Stack frame (ignored):
;    - (dword) Ignored
;    - (dword) Ignored
; <- CS:EIP - Control transferred to jump target
;    SS:ESP - Protected mode stack
;    DS, ES, FS, GS - Flat data segment selector (base: 0, segment limit: 4 GB)
;------------------------------------------------------------------------------

	[bits 16]
	align 4

d_rm2pm:
	add esp, 8			; Ignore stack frame

	; Switch to 16-bit protected mode

	mov ax, [host_mem_seg]
	add ax, DPMI_TERM_STACK_SIZE / 16
	mov es, ax
	mov ax, 1			; 32-bit program
	call far [dpmi_rm2pm]
	jnc .setup_selectors

	; Cannot switch to protected mode, roll back kernel setup

	mov byte [exit_error], PMI_E_DPMI_INIT
	jmp kernel_shutdown.rm_cleanup

.setup_selectors:
	mov [sel_stack_rm], ss		; Real-mode compatible stack selector
	mov [sel_code_krnl16], cs	; Kernel 16-bit code segment
	mov [sel_data_krnl32], ds	; Kernel 32-bit data segment
	mov word [pm_exit_exception.sel_code_krnl16], cs

	; Convert DS to 32-bit data segment

	xor ebx, ebx
	mov ax, 0x0008
	mov bx, ds
	xor cx, cx
	dec cx
	mov dx, cx
	int 0x31			; Set limit to 4 GB
	jc .panic
	inc ax
	lar ecx, ebx
	shr ecx, 8
	and cx, ~SEG_DS_EXP_DOWN
	or cx, SEG_PRESENT | SEG_DS | SEG_DS_WRITABLE | SEG_DS_BIG | DESC_PAGE_GRAN
	int 0x31			; Set access rights
	jc .panic
	mov ds, bx			; Reload DS, just in case

	; Create selectors for sel_code_flat, sel_data_flat, sel_code_krnl32 and
	; sel_pmi_fn

	mov ax, 0x0003
	int 0x31			; Get selector increment
	jc .panic
	mov bp, ax			; BP: selector increment

	xor ax, ax
	mov cx, 4
	int 0x31			; Allocate 4 selectors
	jc .panic
	mov si, ax			; SI: first selector

	mov ax, 0x0007			; Set base
	mov bx, si
	xor cx, cx
	xor dx, dx
	int 0x31			; Set base to 0 for sel_code_flat
	jc .panic
	add bx, bp
	int 0x31			; Set base to 0 for sel_data_flat
	jc .panic
	add bx, bp
	mov dx, [kernel_seg]
	shld cx, dx, 4
	shl dx, 4
	int 0x31			; Set base of sel_code_krnl32
	jc .panic
	add bx, bp
	add dx, pmi_fn
	adc cx, 0
	int 0x31			; Set base of sel_pmi_fn
	jc .panic

	inc ax				; Set limit
	mov bx, si
	xor cx, cx
	dec cx
	mov dx, cx
	int 0x31			; Set limit to 4 GB for sel_code_flat
	jc .panic
	add bx, bp
	int 0x31			; Set limit to 4 GB for sel_data_flat
	jc .panic
	add bx, bp
	int 0x31			; Set limit to 4 GB for sel_code_krnl32
	jc .panic
	add bx, bp
	xor cx, cx
	mov dx, pmi_fns.strucsize
	int 0x31			; Set limit to table size for sel_pmi_fn
	jc .panic

	inc ax				; Set access rights
	mov bx, si
	mov [sel_code_flat], bx
	lar ecx, ebx
	shr ecx, 8
	or cx, SEG_PRESENT | SEG_CS | SEG_CS_READABLE | SEG_CS_32BIT | DESC_PAGE_GRAN
	int 0x31			; Set access rights for sel_code_flat
	jc .panic
	add bx, bp
	mov [sel_data_flat], bx
	lar ecx, ebx
	shr ecx, 8
	and cx, ~SEG_DS_EXP_DOWN
	or cx, SEG_PRESENT | SEG_DS | SEG_DS_WRITABLE | SEG_DS_BIG | DESC_PAGE_GRAN
	int 0x31			; Set access rights for sel_data_flat
	jc .panic
	add bx, bp
	mov [sel_code_krnl32], bx
	lar ecx, ebx
	shr ecx, 8
	or cx, SEG_PRESENT | SEG_CS | SEG_CS_READABLE | SEG_CS_32BIT | DESC_PAGE_GRAN
	int 0x31			; Set access rights for sel_code_krnl32
	jc .panic
	add bx, bp
	mov [sel_pmi_fn], bx
	lar ecx, ebx
	shr ecx, 8
	or cx, SEG_PRESENT | SEG_DS
	int 0x31			; Set access rights for sel_pmi_fn
	jc .panic

	; Set selectors in far pointers

	mov eax, [sel_data_flat]
	mov [pm_stack + 4], eax
	mov eax, [sel_code_krnl16]
	mov [pm2rm + 4], eax
	mov eax, [sel_code_krnl32]
	mov [PMI_FN(call_rm) + 4], eax
	mov [PMI_FN(get_irq_hndlr) + 4], eax
	mov [PMI_FN(set_irq_hndlr) + 4], eax
	mov [PMI_FN(get_env_info) + 4], eax

	; Setup PMI flat selectors

	lss esp, [pm_stack]		; Load protected mode stack
	mov ax, [sel_data_flat]		; Set flat data selectors
	mov ds, ax
	mov es, ax
	mov fs, ax
	mov gs, ax

	; Transfer control back to kernel_start protected mode setup

	jmp kernel_start.pm_setup

.panic:
	extern msg_err_dpmi
	%define .reg(name) [esp + d_call_rm_data. %+ name]

	push es				; Print DPMI init error message
	sub esp, d_call_rm_data.strucsize
	mov word .reg(edx), msg_err_dpmi
	mov word .reg(eax), 0x0900
	mov ax, [kernel_seg]
	mov .reg(ds), ax
	mov word .reg(sp), 0
	mov word .reg(ss), 0
	mov ax, 0x0300			; DPMI: simulate real mode interrupt
	mov bx, 0x21
	xor cx, cx
	mov dx, ss
	mov es, dx
	mov edi, esp			; ES:EDI: pointer to d_call_rm_data
	int 0x31
	add esp, d_call_rm_data.strucsize
	pop es

	mov ax, 0x4cff			; Terminate to DOS
	int 0x21


;------------------------------------------------------------------------------
; Raw - Switch to real mode.
;------------------------------------------------------------------------------
; Do not call this function, it won't return to the caller. Do a far jump using
; sel_code_krnl16 selector with the real mode target address pushed to the stack
; frame.
;------------------------------------------------------------------------------
; -> Stack frame:
;    - (word) Jump target code segment
;    - (word) Jump target code offset
; <- EAX, EBX, ECX, EDX, ESI, EDI, EBP, 16-bit flags - Preserved
;    CS:IP - Control transferred to jump target
;    SS:SP - Real mode stack
;    DS, ES, FS, GS - Undefined
;------------------------------------------------------------------------------

	[bits 16]
	align 4

r_pm2rm:
	pushf				; Disable IRQs during mode change
	cli

	mov ds, cs:[sel_data_krnl32]	; Save registers
	mov [.eax], eax
	pop word [.flags]
	pop word [.ip]
	pop word [.cs]

	mov [pm_stack], esp		; Save protected mode stack top
	mov [pm_stack + 4], ss

	lidt cs:[rm_idt_register]	; Restore real mode IDTR

	mov ss, cs:[sel_stack_rm]	; Real mode compatible stack selector
	movzx esp, word cs:[pmi_stack]	; Usable stack in case of exception

	mov eax, cr0			; Go back to real mode
	and al, 0xfe
	mov cr0, eax
	jmp word 0x1234:.setup_rm	; Finish switch to real mode
	.kernel_seg EQU $ - 2

	align 2
.flags	dw 0
.ip	dw 0
.cs	dw 0

	align 4
.eax	dd 0

.setup_rm:
	lss sp, cs:[rm_stack]		; Load real mode stack

	mov eax, cs:[.eax]		; Restore registers

	push word cs:[.cs]		; Push return address
	push word cs:[.ip]

	push word cs:[.flags]		; Restore interrupt state and flags
	popf

	retf				; Jump to target address


;------------------------------------------------------------------------------
; VCPI - Switch to V86 mode.
;------------------------------------------------------------------------------
; Do not call this function, it won't return to the caller. Do a far jump using
; sel_code_krnl16 selector with the real mode target address pushed to the stack
; frame.
;------------------------------------------------------------------------------
; -> Stack frame:
;    - (word) Jump target code segment
;    - (word) Jump target code offset
; <- EAX, EBX, ECX, EDX, ESI, EDI, EBP, 16-bit flags - Preserved
;    CS:IP - Control transferred to jump target
;    SS:SP - Real mode stack
;    DS, ES, FS, GS - Undefined
;------------------------------------------------------------------------------

	[bits 16]
	align 4

v_pm2rm_nopg:
	pushf				; Disable IRQs during mode change
	cli

	mov ds, cs:[sel_data_krnl32]	; Save registers
	mov [v_pm2rm.eax], eax
	pop word [v_pm2rm.flags]	; Need to do this before enabling
	pop word [v_pm2rm.ip]		; paging, otherwise ESP may point to
	pop word [v_pm2rm.cs]		; unmapped linear memory

	; Enable paging when low memory is identity mapped

	mov eax, cr3
	and eax, 0xfff
	or eax, [page_dir_phaddr]
	mov cr3, eax			; Set CR3 to our page directory
	mov eax, cr0
	or eax, 0x80000000
	mov cr0, eax			; Enable paging
	jmp v_pm2rm.go_rm

v_pm2rm:
	pushf				; Disable IRQs during mode change
	cli

	mov ds, cs:[sel_data_krnl32]	; Save registers
	mov [.eax], eax
	pop word [.flags]
	pop word [.ip]
	pop word [.cs]

.go_rm:
	mov [pm_stack], esp		; Save protected mode stack top
	mov [pm_stack + 4], ss

	mov ax, SEL_DATA_FLAT		; Setup temporary stack
	mov ds, ax
	mov ss, ax
	mov esp, cs:[vcpi_stack]
	sub esp, 16			; Skip GS, FS, DS, ES
	xor eax, eax
	mov ax, cs:[rm_stack + 2]
	push eax			; SS
	mov ax, cs:[rm_stack]
	push eax			; ESP
	sub esp, 4			; Skip EFLAGS (reserved)
	mov ax, cs:[kernel_seg]
	push eax			; CS: kernel real mode segment
	push dword .setup_rm		; IP: target address
	mov ax, 0xde0c
	call far dword [cs:vcpi_host]	; VCPI switch to V86 mode

	align 2
.flags	dw 0
.ip	dw 0
.cs	dw 0

	align 4
.eax	dd 0

.setup_rm:
	mov eax, cs:[.eax]		; Restore registers

	push word cs:[.flags]		; Restore interrupt state and flags
	push word cs:[.cs]		; Push return address
	push word cs:[.ip]

	iret				; Jump to target address


;------------------------------------------------------------------------------
; DPMI - Terminal switch to V86/real mode.
;------------------------------------------------------------------------------
; Do not call this function, it won't return to the caller. Do a far jump using
; sel_code_krnl16 selector. This is only used once by kernel_shutdown so we
; won't bother preserving registers.
;------------------------------------------------------------------------------
; -> Stack frame:
;    - (word) Ignored
;    - (word) Ignored
; <- EAX, EBX, ECX, EDX, ESI, EDI, EBP, 16-bit flags - Preserved
;    CS:IP - Control transferred to jump target
;    SS:SP - Real mode stack
;    DS, ES, FS, GS - Undefined
;------------------------------------------------------------------------------

	[bits 16]

d_pm2rm:
	mov ds, cs:[sel_data_krnl32]
	mov ss, cs:[sel_data_flat]
	mov esp, cs:[dpmi_term_stack]

	; Switch to real mode temporarily and continue at
	; kernel_shutdown.rm_cleanup. This will return to the main program and
	; finally terminate via exit_to_dos, which will capture this request and
	; force a return to protected mode.

	mov word [exit_to_dos.terminate_proc], .terminate
	sub esp, pmi_rm_call.strucsize
	mov byte [esp + pmi_rm_call.flags], PMI_CALL_FAR
	mov ax, [kernel_seg]
	mov [esp + pmi_rm_call.ds], ax
	mov [esp + pmi_rm_call.cs], ax
	mov word [esp + pmi_rm_call.ip], .rm_cleanup
	call far dword [PMI_FN(call_rm)]
	add esp, pmi_rm_call.strucsize

	; Terminate application from protected mode cleanly (through DPMI host)

	mov ah, 0x4c
	int 0x21

.rm_cleanup:

	; Now in real mode temporarily, save return stack address for
	; exit_to_dos function so we can return back to protected mode instead,
	; then jump to kernel_shutdown.rm_cleanup to continue with the
	; application termination procedure.

	mov [.ret_stack], sp
	mov [.ret_stack + 2], ss
	jmp kernel_shutdown.rm_cleanup

.ret_stack:
	dd 0

.terminate:
	lss sp, cs:[.ret_stack]		; Load original SS:SP for .rm_cleanup
	retf				; Return to protected mode


;------------------------------------------------------------------------------
; Raw, VCPI - Call a real mode procedure (interrupt or far call) from protected
; mode.
;------------------------------------------------------------------------------
; -> FLAGS - Passed to real mode procedure
;    EAX, EBX, ECX, EDX, ESI, EDI, EBP - Passed to real mode procedure
;    Stack frame:
;    - (pmi_rm_call) Data structure for the real mode call
; <- FLAGS - Returned from real mode procedure
;    EAX, EBX, ECX, EDX, ESI, EDI, EBP - Returned from real mode procedure
;    Stack frame:
;    - (pmi_rm_call) Data structure for the real mode call updated with results
;------------------------------------------------------------------------------

	[bits 32]

rv_call_rm:
	push ds
	push es
	push fs
	push gs
	%assign .frame 8 + 16		; Return address, DS, ES, FS, GS

	o16 pushf			; Disable interrupts and save flags
	cli

	mov ds, cs:[sel_data_krnl32]	; Save registers
	mov [.eax], eax
	pop word [.flags]

	%assign .i 0			; Copy pmi_rm_call structure
	%rep pmi_rm_call.strucsize / 4
	mov eax, [esp + .frame + .i]
	mov [.regs + .i], eax
	%assign .i .i + 4
	%endrep

	; Go to real mode

	push word cs:[kernel_seg]
	push word .rm
	jmp far [cs:pm2rm]

	align 2
.flags	dw 0

	align 4
.regs	db pmi_rm_call.strucsize dup (0)
.eax	dd 0

	[bits 16]

.rm:

	; Setup common registers

	mov es, cs:[.regs + pmi_rm_call.es]
	mov fs, cs:[.regs + pmi_rm_call.fs]
	mov gs, cs:[.regs + pmi_rm_call.gs]
	mov eax, cs:[.regs + pmi_rm_call.ip]

	test byte cs:[.regs + pmi_rm_call.flags], PMI_CALL_INT
	jz .call_proc

	; Call interrupt handler

	xor eax, eax			; Set interrupt handler address in
	mov ds, ax			; .cs and .ip of pmi_rm_call structure
	mov al, cs:[.regs + pmi_rm_call.int]
	mov eax, [eax * 4]

	push word cs:[.flags]		; Flags for simulated INT
	and word cs:[.flags], 0xfdff	; Clear IF for simulated INT

.call_proc:
	push cs
	push .call_done			; Return to .call_done

	push eax			; Int handler / target address for retf

	mov eax, cs:[.eax]		; Restore registers
	mov ds, cs:[.regs + pmi_rm_call.ds]
	push word cs:[.flags]		; Restore flags and interrupt state
	popf

	retf				; Jump to target / interrupt handler

.call_done:
	pushf				; Save updated flags, disable interrupts
	cli

	pop word cs:[.flags]		; Save registers
	mov cs:[.eax], eax

	; Update registers with result in pmi_rm_call copy

	mov cs:[.regs + pmi_rm_call.ds], ds
	mov cs:[.regs + pmi_rm_call.es], es
	mov cs:[.regs + pmi_rm_call.fs], fs
	mov cs:[.regs + pmi_rm_call.gs], gs

	; Go back to protected mode

	push dword SEL_CODE_KRNL32
	push dword .done
	jmp [cs:rm2pm]

	[bits 32]

.done:

	; Update original pmi_rm_call (use dword move to copy 2 segment
	; registers at once)

	mov word [esp + .frame + pmi_rm_call.error_code], 0
	mov eax, cs:[.regs + pmi_rm_call.ds]
	mov [esp + .frame + pmi_rm_call.ds], eax
	mov eax, cs:[.regs + pmi_rm_call.fs]
	mov [esp + .frame + pmi_rm_call.fs], eax

	mov eax, cs:[.eax]		; Restore registers

	push word cs:[.flags]		; Restore flags and interrupt state
	o16 popf

	pop gs
	pop fs
	pop es
	pop ds
	retf


;------------------------------------------------------------------------------
; DPMI - Call a real mode procedure (interrupt or far call) from protected
; mode.
;------------------------------------------------------------------------------
; -> FLAGS - Passed to real mode procedure
;    EAX, EBX, ECX, EDX, ESI, EDI, EBP - Passed to real mode procedure
;    Stack frame:
;    - (pmi_rm_call) Data structure for the real mode call
; <- FLAGS - Returned from real mode procedure
;    EAX, EBX, ECX, EDX, ESI, EDI, EBP - Returned from real mode procedure
;    Stack frame:
;    - (pmi_rm_call) Data structure for the real mode call updated with results
;------------------------------------------------------------------------------

	[bits 32]

d_call_rm:
	push ds
	push es
	push fs
	push gs
	pushf

	%define .reg(name) [esp + d_call_rm_data. %+ name]
	%assign .frame 8 + 20 + d_call_rm_data.strucsize
					; Return address, DS, ES, FS, GS, flags

	; Create and set translation service register structure for DPMI

	sub esp, d_call_rm_data.strucsize
	mov .reg(edi), edi
	mov .reg(esi), esi
	mov .reg(ebp), ebp
	mov .reg(ebx), ebx
	mov .reg(edx), edx
	mov .reg(ecx), ecx
	mov .reg(eax), eax
	mov ax, [esp + d_call_rm_data.strucsize]
	mov .reg(flags), ax
	mov word [esp + .frame + pmi_rm_call.error_code], 0
	mov ax, [esp + .frame + pmi_rm_call.es]
	mov .reg(es), ax
	mov ax, [esp + .frame + pmi_rm_call.ds]
	mov .reg(ds), ax
	mov ax, [esp + .frame + pmi_rm_call.fs]
	mov .reg(fs), ax
	mov ax, [esp + .frame + pmi_rm_call.gs]
	mov .reg(gs), ax
	mov bx, [esp + .frame + pmi_rm_call.cs]
	mov .reg(cs), bx
	mov dx, [esp + .frame + pmi_rm_call.ip]
	mov .reg(ip), dx
	mov word .reg(sp), 0
	mov word .reg(ss), 0

	; Call real mode procedure using DPMI translate services

	mov ax, 0x0301			; DPMI: call far real mode procedure
	test byte [esp + .frame + pmi_rm_call.flags], PMI_CALL_INT
	jz .call_proc
	dec al				; DPMI: simulate real mode interrupt
	mov bl, [esp + .frame + pmi_rm_call.int]

.call_proc:
	xor bh, bh
	xor cx, cx
	mov dx, ss
	mov es, dx
	mov edi, esp			; ES:EDI: pointer to d_call_rm_data
	int 0x31
	jc .error

	; Set registers from DPMI translation service register structure

	mov ax, .reg(es)
	mov [esp + .frame + pmi_rm_call.es], ax
	mov ax, .reg(ds)
	mov [esp + .frame + pmi_rm_call.ds], ax
	mov ax, .reg(fs)
	mov [esp + .frame + pmi_rm_call.fs], ax
	mov ax, .reg(gs)
	mov [esp + .frame + pmi_rm_call.gs], ax
	mov ax, .reg(flags)
	mov [esp + d_call_rm_data.strucsize], ax
	mov edi, .reg(edi)
	mov esi, .reg(esi)
	mov ebp, .reg(ebp)
	mov ebx, .reg(ebx)
	mov edx, .reg(edx)
	mov ecx, .reg(ecx)
	mov eax, .reg(eax)

.exit:
	add esp, d_call_rm_data.strucsize

	popf
	pop gs
	pop fs
	pop es
	pop ds
	retf

.error:
	mov word [esp + .frame + pmi_rm_call.error_code], ax
	jmp .exit


;==============================================================================
; Hardware interrupt management
;==============================================================================

;------------------------------------------------------------------------------
; Raw, VCPI - Setup interrupts for real and protected mode.
;------------------------------------------------------------------------------
; -> DS - PMI segment
;    FS - Host private data segment
; <- Destroys everything except segment registers
;------------------------------------------------------------------------------

	[bits 16]

setup_interrupts:
	in al, 0xa1			; Save current PIC masks
	mov ah, al
	in al, 0x21
	mov [pic_masks], ax

	cmp byte [pm_host_type], PMI_HOST_DPMI
	je .exit

	push es

	; Save real mode IDTR

	sidt [rm_idt_register]

	; Setup IDT

	mov di, rv_priv_data.idt	; FS:DI: IDT descriptor pointer
	xor bx, bx			; BX: IRQ handler offset
	xor cl, cl			; CL: interrupt number
	mov dl, [irq0_base]		; DL: master PIC base interrupt
	mov dh, [irq8_base]		; DH: slave PIC base interrupt
	mov bp, cs:[sel_code_krnl32]	; BP: 32-bit kernel code selector

.idt_loop:

	; 0x00 - 0x1f: Exception

	cmp cl, 0x20
	ja .check_irq

	%ifdef DEBUG_EXCEPTIONS
	xor bh, bh
	mov ax, pm_int_exceptions
	mov bl, cl
	shl bx, 3
	add ax, bx
	%else
	mov ax, pm_int_exception
	%endif
	mov si, (GATE_PRESENT | GATE_TRAP) << 8
	cmp cl, 0x20
	jb .set_descriptor

.check_irq:

	; 0xa0 - 0xaf: IRQs 0-15

	mov ch, cl
	mov bl, cl
	and ch, 0xf8			; PIC base interrupt always 8-aligned
	cmp ch, dl
	je .master_irq
	cmp ch, dh
	jne .empty

	sub bl, dh			; IRQs 8-15
	add bl, 8
	jmp .irq

.master_irq:
	sub bl, dl			; IRQs 0-7

.irq:
	xor bh, bh
	mov ax, pm_irq_reflectors
	shl bx, 2
	add ax, bx
	mov si, (GATE_PRESENT | GATE_INTERRUPT) << 8
	jmp .set_descriptor

.empty:

	; Otherwise set dummy handler

	mov ax, pm_int_empty
	mov si, (GATE_PRESENT | GATE_TRAP) << 8

.set_descriptor:
	mov word fs:[di], ax
	mov word fs:[di + 2], bp
	mov word fs:[di + 4], si
	mov word fs:[di + 6], 0

	add di, 8
	inc cl
	jnz .idt_loop

	; Initialize real mode IRQ reflectors

	lea bx, [rm_irq_reflector.call_irq + 1]
	mov al, dl			; IRQs 0-7
	call .set_rm_irq_reflectors
	mov al, dh			; IRQs 8-15
	call .set_rm_irq_reflectors

	; Install real mode redirectors

	xor ax, ax
	mov es, ax			; ES: interrupt vector table
	movzx di, dl			; IRQs 0-7
	shl di, 2			; ES:DI: interrupt vector entries
	mov si, old_irq_ptrs		; SI: pointer to backup area
	mov bx, cs
	shl ebx, 16
	mov bx, rm_irq_redirectors	; EBX: IRQ redirector pointer
	call .set_rm_irq_redirectors

	movzx di, dh			; IRQs 8-15
	shl di, 2			; ES:DI: interrupt vector entries
	call .set_rm_irq_redirectors

	pop es

.exit:
	retn

; Patch interrupt numbers for rm_irq_reflector.call_irq entries
; -> DS:BX - Pointer to first interrupt number to patch
;    AL - Interrupt number of first IRQ
; <- BX, AL - Adjusted by 8 entries
;    CL - 0

.set_rm_irq_reflectors:
	mov cl, 8

.set_rm_irq_reflector_loop:
	mov [bx], al			; Set interrupt number in call stub
	inc al
	add bx, 4			; Adjust pointer
	dec cl
	jnz .set_rm_irq_reflector_loop
	retn

; Setup real mode IRQ redirectors
; -> ES:DI - Pointer to interrupt vector of first IRQ
;    DS:SI - Pointer to old interrupt vector save structure
;    EBX - Pointer to first interrupt redirector
; <- DI, SI, EBX - Adjusted by 8 vectors
;    CL - 0

.set_rm_irq_redirectors:
	mov cl, 8			; CL: counter

.set_rm_irq_redirector_loop:
	mov eax, es:[di]		; EAX: old interrupt handler
	mov [si], eax			; Save it
	mov es:[di], ebx		; Install new interrupt handler
	add si, 4			; Adjust pointers
	add bx, 4
	add di, 4
	dec cl
	jnz .set_rm_irq_redirector_loop
	retn


;------------------------------------------------------------------------------
; Raw, VCPI - Restore interrupts before terminating.
;------------------------------------------------------------------------------
; Not called when terminating under DPMI.
;------------------------------------------------------------------------------
; -> DS - PMI segment
; <- Destroys everything except segment registers
;------------------------------------------------------------------------------

	[bits 16]

restore_interrupts:
	mov ax, [pic_masks]		; Restore original PIC masks
	out 0x21, al
	mov al, ah
	out 0xa1, al

	mov al, 0x36			; Reset timer IRQ rate to 18.2 Hz
	out 0x43, al
	xor al, al
	out 0x40, al
	out 0x40, al

	cmp byte [pm_host_type], PMI_HOST_DPMI
	je .exit

	push es

	cld

	xor ax, ax
	mov es, ax			; ES: zeropage

	; Restore real mode redirectors

	movzx di, byte [irq0_base]	; IRQs 0-7
	shl di, 2			; ES:DI: interrupt vector entries
	mov si, old_irq_ptrs		; SI: pointer to backup area
	mov cx, 8			; CX: counter
	rep movsd

	; Restore real mode redirectors

	movzx di, byte [irq8_base]	; IRQs 8-15
	shl di, 2			; ES:DI: interrupt vector entries
	mov cx, 8			; CX: counter
	rep movsd

	pop es

.exit:
	retn


;------------------------------------------------------------------------------
; Raw, VCPI - Remap IRQ vectors.
;------------------------------------------------------------------------------
; -> BL - New base vector for master PIC (IRQs 0-7)
;    BH - New base vector for slave PIC (IRQs 8-15)
;    DS - PMI segment
; <- Destroys everything except segment registers
;------------------------------------------------------------------------------

	[bits 16]

remap_pic:
	cmp byte [pm_host_type], PMI_HOST_DPMI
	je .exit

	pushf
	cli				; Disable interrupts

	in al, 0x21			; Save masks
	mov cl, al
	in al, 0xa1
	mov ch, al
	jmp $ + 2

	mov al, 0x11			; Start programming both PICs
	out 0x20, al
	out 0xa0, al
	jmp $ + 2

	mov al, bl			; Set new vectors
	out 0x21, al
	mov al, bh
	out 0xa1, al
	jmp $ + 2

	mov al, 0x04			; Setup master-slave connection
	out 0x21, al
	mov al, 0x02
	out 0xa1, al
	jmp $ + 2

	mov al, 0x01			; Write a command to PICs
	out 0x21, al
	out 0xa1, al
	jmp $ + 2

	mov al, cl			; Restore masks
	out 0x21, al
	mov al, ch
	out 0xa1, al

	cmp byte cs:[pm_host_type], PMI_HOST_VCPI
	jne .done
	mov ax, 0xde0b			; Inform VCPI host about the remapping
	xor cx, cx
	mov cl, bh
	xor bh, bh
	int 0x67

.done:
	popf				; Restore interrupt state

.exit:
	retn


;------------------------------------------------------------------------------
; Raw, VCPI - Real mode IRQ redirectors.
;------------------------------------------------------------------------------
; IRQs are remapped to prevent conflicts with CPU exceptions and to enable easy
; redirection to protected mode when a protected mode handler is installed
; without changing the real mode interrupt vector. These redirectors are
; installed as handlers for the remapped interrupts in real mode to redirect
; IRQs back to their original handler.
;------------------------------------------------------------------------------

	[bits 16]
	align 4

rm_irq_redirectors:

	; Stub for each IRQ. Pushes rm_irq_redirectors + IRQ * 4 + .stubsize to
	; the stack and jumps to rm_irq_redirector. As it is, each redirector
	; stub takes 3 bytes, but stubs are aligned at a dword boundary.

	%define .stub call rm_irq_redirector

	.stub
	.stubsize EQU $ - rm_irq_redirectors
	align 4

	%rep 15
	.stub
	align 4
	%endrep

	%if ($ - rm_irq_redirectors > 16 * 4)
	%error "rm_irq_redirectors: stub is larger, than 4 bytes!"
	%endif

rm_irq_redirector:
	push ds
	push bp
	push ebx

	xor bx, bx
	mov bp, sp
	mov ds, bx			; DS: zeropage
	mov bx, [bp + 8]		; BX: rm_irq_redirectors + IRQ * 4 + 3
	sub bx, rm_irq_redirectors	; BX: IRQ * 4 + 3
	cmp bl, 8 * 4 + rm_irq_redirectors.stubsize
	jae .irq8
	add bx, cs:[rm_irq0_vector]	; BX: interrupt * 4 + 3
	jmp .call_irq

.irq8:
	add bx, cs:[rm_irq8_vector]
	sub bx, 8 * 4			; BX: interrupt * 4 + 3

.call_irq:
	mov ebx, [bx - rm_irq_redirectors.stubsize]
	mov ds, [bp + 6]		; Restore DS

	; Replace return address and saved DS with far pointer to actual real
	; mode IRQ handler from vectors 8 - 15. DS is restored earlier, so we
	; won't have to pop it off the stack.

	mov [bp + 6], ebx		; EBX: current IRQ handler far pointer

	pop ebx
	pop bp
	retf				; Jump to actual IRQ handler


;------------------------------------------------------------------------------
; Raw, VCPI - Real to protected mode IRQ reflectors.
;------------------------------------------------------------------------------
; Replaces the real mode IRQ redirector when the protected mode handler of the
; IRQ is hooked. Then the IRQs happening in real mode will switches to protected
; mode and call the protected mode IRQ handler. The real mode IRQ handler will
; only be executed if the protected mode handler chains back to the previous
; protected mode handler (which by default redirects IRQs to their real mode
; handlers).
;------------------------------------------------------------------------------

	[bits 16]
	align 4

rm_irq_reflectors:

	; Stub for each IRQ. Pushes rm_irq_reflectors + IRQ * 4 + .stubsize to
	; the stack and jumps to rm_irq_reflector. As it is, each reflector stub
	; takes 3 bytes, but stubs are aligned at a dword boundary.

	%define .stub call word rm_irq_reflector

	.stub
	.stubsize EQU $ - rm_irq_reflectors
	align 4

	%rep 15
	.stub
	align 4
	%endrep

	%if ($ - rm_irq_reflectors > 16 * 4)
	%error "rm_irq_reflectors: stub is larger, than 4 bytes!"
	%endif

rm_irq_reflector:
	push ds				; Segment registers are trashed by
	push es				; mode switch
	push fs
	push gs
	push bx

	mov bx, sp
	mov bx, ss:[bx + 10]
	mov cs:[.addr], bx

	; Go to protected mode

	push dword SEL_CODE_KRNL16
	push dword .pm
	jmp [cs:rm2pm]

	[bits 16]
	align 4

.pm:

	; Call interrupt handler. Flags and BX are trashed in the process, but
	; it does not matter since it's an IRQ and it cannot assume anything
	; anyways.

	mov bx, 0x1234			; BX: rm_irq_reflectors + IRQ * 4 + 3
	.addr EQU $ - 2
	sub bx, rm_irq_reflectors	; BX: IRQ * 4 + 3
	lea bx, [.call_irq + bx - rm_irq_reflectors.stubsize]
	jmp bx

	align 4

.call_irq:

	; Interrupt invocation stub for each IRQ. Each stub consists of exactly
	; 4 bytes.

	%rep 16
	int 0x12			; Actual number set by setup_interrupts
	jmp .done_irq
	align 4
	%endrep

	%if (($ - .call_irq) > 16 * 4)
	%error "rm_irq_reflectors: .call_irq entries are larger, than 4 bytes!"
	%endif

.done_irq:

	; Go back to real mode

	push word cs:[kernel_seg]
	push word .rm
	jmp [cs:pm2rm]			; 16-bit segment, no need for far jmp

	[bits 16]
	align 4

.rm:
	pop bx
	pop gs
	pop fs
	pop es
	pop ds
	add sp, 2			; Discard pm_irq_reflectors address
	iret


;------------------------------------------------------------------------------
; Raw, VCPI - Protected to real mode IRQ reflectors.
;------------------------------------------------------------------------------
; Default handlers for protected mode IRQs. Reflects the IRQ to their original
; real mode handlers.
;------------------------------------------------------------------------------

	[bits 32]
	align 4

pm_irq_reflectors:

	; Stub for each IRQ. Pushes pm_irq_reflectors + IRQ * 4 + .stubsize to
	; the stack and jumps to pm_irq_reflector. As it is, each stub takes
	; exactly 4 bytes. A 16-bit operand is used to reduce the opcode size.
	; This can be done safely because the kernel segment is less, than 64k.
	; The expense is an additional clock cycle due to the prefix.

	%define .stub call word pm_irq_reflector

	.stub
	.stubsize EQU $ - pm_irq_reflectors
	align 4

	%rep 15
	.stub
	align 4
	%endrep

	%if ($ - pm_irq_reflectors > 16 * 4)
	%error "pm_irq_reflectors: stub is larger, than 4 bytes!"
	%endif

pm_irq_reflector:
	push ds				; Segment registers are trashed by
	push es				; mode switch
	push fs
	push gs
	push eax

	xor eax, eax			; Save return address for IRQ number
	mov ax, [esp + 20]		; calculation
	mov ds, cs:[sel_data_krnl32]
	mov [.addr], eax

	; Go to real mode

	push word cs:[kernel_seg]
	push word .rm
	jmp far [cs:pm2rm]

	align 4
.addr:	dd 0

	[bits 16]

.rm:

	; Call interrupt handler. Flags and EAX are trashed in the process, but
	; it does not matter since it's an IRQ and it cannot assume anything
	; anyways.

	xor ax, ax
	mov ds, ax			; DS: zeropage
	mov eax, cs:[.addr]		; AX: pm_irq_reflectors + IRQ * 4 + 4
	sub ax, pm_irq_reflectors	; AX: IRQ * 4 + 4
	cmp al, 8 * 4 + pm_irq_reflectors.stubsize
	jae .irq8
	add ax, cs:[rm_irq0_vector]	; AX: interrupt * 4 + 4
	jmp .call_irq

.irq8:
	add ax, cs:[rm_irq8_vector]
	sub ax, 8 * 4			; AX: interrupt * 4 + 4

.call_irq:
	pushf				; Simulate IRQ interrupt
	call far [eax - pm_irq_reflectors.stubsize]

	; Go back to protected mode

	push dword SEL_CODE_KRNL32
	push dword .pm
	jmp [cs:rm2pm]

	[bits 32]
	align 4

.pm:
	pop eax
	pop gs
	pop fs
	pop es
	pop ds
	add esp, 2			; Discard pm_irq_reflectors address
	iret


;------------------------------------------------------------------------------
; Raw, VCPI - Default dummy protected mode interrupt handler.
;------------------------------------------------------------------------------

	[bits 32]

pm_int_empty:
	iret


;------------------------------------------------------------------------------
; Raw, VCPI, DPMI - Spurious IRQ check prologue macro for IRQ 7 and 15
;------------------------------------------------------------------------------
; -> %1 - IRQ number (7 or 15)
;------------------------------------------------------------------------------

%macro	pm_spurious_irq 1

	%if (%1 = 7)
	%assign pic_base 0x20
	%else
	%assign pic_base 0xa0
	%endif

	push eax

	mov al, 0x0b			; Check if IRQ 7 / 15 is being serviced
	out pic_base, al
	in al, pic_base
	test al, 0x80
	jz .spurious			; Nope, spurious IRQ

	pop eax				; Yes, jump to handler
.irq_handler:
	jmp 0x1234:0x5678abcd
.in_use:
	db 0				; Set to 1 if prologue installed

.spurious:
	%if (%1 = 15)
	mov al, 0x20			; Send EOI to first PIC for spurious
	out 0x20, al			; IRQ 15
	%endif

	pop eax
	iretd

%endmacro


;------------------------------------------------------------------------------
; Raw, VCPI, DPMI - Spurious IRQ check prologue macro for IRQ 7
;------------------------------------------------------------------------------

	[bits 32]
	align 4

pm_spurious_irq7:
	pm_spurious_irq 7


;------------------------------------------------------------------------------
; Raw, VCPI, DPMI - Spurious IRQ check prologue macro for IRQ 15
;------------------------------------------------------------------------------

	[bits 32]
	align 4

pm_spurious_irq15:
	pm_spurious_irq 15


;------------------------------------------------------------------------------
; Raw, VCPI - Get info for a selector.
;------------------------------------------------------------------------------
; -> ES:EDI - GDT base
;    SI - Selector
; <- EAX - Segment limit
;    EBX - Segment base
;    CX - Selector flags (most significant bits) and access byte (low byte)
;    Destroys everything except segment registers
;------------------------------------------------------------------------------

	[bits 32]

%ifdef DEBUG_EXCEPTIONS

get_selector_info:
	test si, si
	je .invalid
	mov bx, [gdt_register]
	cmp si, bx
	ja .invalid

	and esi, 0xfff8
	mov ah, es:[edi + esi + 7]
	mov al, es:[edi + esi + 4]
	shl eax, 16
	mov ax, es:[edi + esi + 2]
	mov ebx, eax			; EBX: base
	mov al, es:[edi + esi + 6]
	and ax, 0xf
	shl eax, 16
	mov ax, es:[edi + esi]		; EAX: limit
	mov cx, es:[edi + esi + 5]
	and ch, 0xf0			; CX: flags
	test cx, DESC_PAGE_GRAN
	jz .exit
	shl eax, 12			; Page granular, extend limit
	or ax, 0xfff
	jmp .exit

.invalid:
	xor eax, eax
	xor ebx, ebx
	xor cx, cx

.exit:
	retn

	%endif

;------------------------------------------------------------------------------
; Raw, VCPI - Exception handlers.
;------------------------------------------------------------------------------
; Terminates the application right away.
;------------------------------------------------------------------------------

	[bits 32]

	%ifdef DEBUG_EXCEPTIONS

	align 8

pm_int_exceptions:
	%define .dump(data) exception_dump + exception_dump_data. %+ data
	%define .dumpreg(reg) mov [.dump(reg)], reg

	%macro pm_int_exception_stub 1
	%if (%1 <= 5 || %1 = 7)
	iret				; Ignore exceptions 0 - 5 and 7
	%else
	%if (%1 != 8 && (%1 < 10 || %1 > 14) && %1 != 17)
	push 0				; Push dummy fault code to stack
	%endif
	push %1
	jmp word pm_int_exception
	%endif
	%endmacro

	%assign .exception 0
	%rep 32
	pm_int_exception_stub .exception
	align 8
	%assign .exception .exception + 1
	%endrep

	%if ($ - pm_int_exceptions > 8 * 32)
	%error "pm_int_exceptions: stub is larger, than 8 bytes!"
	%endif

	align 4

pm_int_exception:
	o16 push ds			; Save segment registers
	mov ds, cs:[sel_data_krnl32]
	pop word [.dump(ds)]
	mov word [.dump(es)], es
	mov word [.dump(fs)], fs
	mov word [.dump(gs)], gs
	mov word [.dump(ss)], ss

	.dumpreg(eax)			; Save general registers
	.dumpreg(ebx)
	.dumpreg(ecx)
	.dumpreg(edx)
	.dumpreg(esi)
	.dumpreg(edi)
	.dumpreg(ebp)

	pop eax				; Save exception number
	mov byte [.dump(exception)], al
	pop eax				; Save fault code
	mov [.dump(fault_code)], eax
	pop eax				; Save EIP
	mov [.dump(eip)], eax
	pop eax				; Save CS
	mov [.dump(cs)], ax
	pop eax				; Save eflags
	mov [.dump(eflags)], eax

	mov [.dump(esp)], esp		; Save ESP (nothing else left on stack)
	mov cl, 64 / 4
	lea ebx, [.dump(stack)]
	xor esi, esi

.copy_stack_loop:
	mov eax, [esp + esi]
	mov [ebx + esi], eax
	add esi, 4
	dec cl
	jnz .copy_stack_loop

	mov es, [sel_data_flat]		; Save selector base, limit and flags
	mov edi, [gdt_base]
	mov si, [.dump(cs)]
	call get_selector_info
	mov [.dump(cs_base)], ebx
	mov [.dump(cs_limit)], eax
	mov [.dump(cs_flags)], cx
	mov si, [.dump(ds)]
	call get_selector_info
	mov [.dump(ds_base)], ebx
	mov [.dump(ds_limit)], eax
	mov [.dump(ds_flags)], cx
	mov si, [.dump(es)]
	call get_selector_info
	mov [.dump(es_base)], ebx
	mov [.dump(es_limit)], eax
	mov [.dump(es_flags)], cx
	mov si, [.dump(fs)]
	call get_selector_info
	mov [.dump(fs_base)], ebx
	mov [.dump(fs_limit)], eax
	mov [.dump(fs_flags)], cx
	mov si, [.dump(gs)]
	call get_selector_info
	mov [.dump(gs_base)], ebx
	mov [.dump(gs_limit)], eax
	mov [.dump(gs_flags)], cx
	mov si, [.dump(ss)]
	call get_selector_info
	mov [.dump(ss_base)], ebx
	mov [.dump(ss_limit)], eax
	mov [.dump(ss_flags)], cx

	%else				; DEBUG_EXCEPTIONS not defined

pm_int_exception:
	mov ds, cs:[sel_data_krnl32]

	%endif

pm_exit_exception:
	mov byte [exit_error], PMI_E_EXCEPTION
	xor al, al
	jmp SEL_CODE_KRNL16:kernel_shutdown
	.sel_code_krnl16 EQU $ - 2


;------------------------------------------------------------------------------
; Raw, VCPI - Get current spurious IRQ handler.
;------------------------------------------------------------------------------
; -> AL - IRQ number
; <- CF - Set if spurious IRQ handler not installed or not IRQ 7 / 15
;    CX:EDX - Selector:offset of the current handler
;------------------------------------------------------------------------------

	[bits 32]

rvd_get_spurious_irq:
	push ebx
	push esi

	mov ebx, pm_spurious_irq7.irq_handler
	mov esi, pm_spurious_irq7.in_use
	cmp al, 7
	je .spurious			; IRQ 7: get custom handler
	mov ebx, pm_spurious_irq15.irq_handler
	mov esi, pm_spurious_irq15.in_use
	cmp al, 15
	jne .not_spurious		; Not IRQ 15

.spurious:
	cmp byte cs:[esi], 0
	je .not_spurious		; Spurious prologue not installed

	mov edx, cs:[ebx + 1]		; Custom IRQ handler offset
	mov cx, cs:[ebx + 4]		; Custom IRQ handler selector
	clc

.exit:
	pop esi
	pop ebx
	retn

.not_spurious:
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Raw, VCPI - Get current IRQ handler.
;------------------------------------------------------------------------------
; -> AL - IRQ number
; <- CF - Set if error
;    EAX - Error code if CF set
;    AL - Actual IRQ number of the handler
;    CX:EDX - Selector:offset of the current handler
;------------------------------------------------------------------------------

	[bits 32]

rv_get_irq_handler:
	cmp al, 0x0f
	ja irq_error

	call rvd_get_spurious_irq
	jnc .exit

	push esi
	push ds
	mov ds, cs:[sel_data_flat]

	; Get IRQ handler from IDT

	cmp al, 2			; IRQ 2 triggers on IRQ 9
	jne .get_vector
	mov al, 9

.get_vector:
	xor edx, edx			; EDX: interrupt number
	mov dl, al
	add dl, cs:[irq0_base]
	cmp al, 8
	jb .get_idt
	mov dl, al
	add dl, cs:[irq8_base]
	sub dl, 8

.get_idt:
	mov esi, cs:[idt_base]
	lea esi, [esi + edx * 8]

	mov edx, [esi + 4]
	mov esi, [esi]
	mov dx, si
	shr esi, 16
	mov cx, si

	pop ds
	pop esi
	clc

.exit:
	retf

irq_error:
	mov eax, PMI_E_INV_IRQ
	stc
	retf


;------------------------------------------------------------------------------
; DPMI - Get current IRQ handler.
;------------------------------------------------------------------------------
; -> AL - IRQ number
; <- CF - Set if error
;    EAX - Error code if CF set
;    AL - Actual IRQ number of the handler
;    CX:EDX - Selector:offset of the current handler
;------------------------------------------------------------------------------

	[bits 32]

d_get_irq_handler:
	cmp al, 0x0f
	ja irq_error

	call rvd_get_spurious_irq
	jnc .exit

	push eax

	; Get IRQ handler from DPMI function

	cmp al, 2			; IRQ 2 triggers on IRQ 9
	jne .get_vector
	mov al, 9
	mov [esp], al			; Update AL in saved EAX

.get_vector:
	push ebx

	mov bl, al			; BL: interrupt number
	add bl, cs:[irq0_base]
	cmp al, 8
	jb .get_int
	mov bl, al
	add bl, cs:[irq8_base]
	sub bl, 8

.get_int:
	mov ax, 0x0204
	int 0x31

	pop ebx
	pop eax
	clc

.exit:
	retf


;------------------------------------------------------------------------------
; Raw, VCPI - Mask spurious IRQ 7 / 15 from handlers.
;------------------------------------------------------------------------------
; -> AL - IRQ number
;    CX:EDX - Selector:offset of the new handler
;    DS - Kernel flat data segment
; <- CX:EDX - Selector:offset of the new handler or spurious IRQ prologue
;------------------------------------------------------------------------------

	[bits 32]

rvd_set_spurious_irq:
	push ebx
	push esi
	push edi

	cmp cx, [sel_code_krnl32]
	jne .setup
	movzx ebx, al
	shl ebx, 2
	add ebx, pm_irq_reflectors	; EBX: original PM IRQ reflector handler
	cmp ebx, edx
	jne .setup

	; Restoring original PM IRQ reflector handler, uninstall spurious IRQ
	; prologue

	mov esi, pm_spurious_irq7.in_use
	cmp al, 7
	je .uninstall
	mov esi, pm_spurious_irq15.in_use
	cmp al, 15
	jne .done

.uninstall:
	mov byte [esi], 0
	jmp .done

.setup:

	; Adding a custom IRQ7 / 15 handler, replace handler address with
	; appropriate prologue. The prologue itself runs in flat memory model
	; and will either do a far jmp to the real handler (if it's somehow
	; using a different selector) or a relative near jmp.

	mov ebx, pm_spurious_irq7.irq_handler
	mov esi, pm_spurious_irq7.in_use
	mov edi, pm_spurious_irq7
	cmp al, 7
	je .set_target
	mov ebx, pm_spurious_irq15.irq_handler
	mov esi, pm_spurious_irq15.in_use
	mov edi, pm_spurious_irq15
	cmp al, 15
	jne .done

.set_target:
	cmp cx, [sel_code_flat]
	je .near_jmp

	mov byte [ebx], 0xea		; 32-bit far jmp
	mov [ebx + 1], edx		; 32-bit absolute offset
	mov [ebx + 5], cx		; 16-bit selector
	jmp .install

.near_jmp:
	mov byte [ebx], 0xe9		; 32-bit relative near jmp
	sub edx, ebx
	sub edx, 5			; 5 bytes for 32-bit relative near jmp
	sub edx, [kernel_addr]		; EDX: relative offset to custom handler
	mov [ebx + 1], edx		; 32-bit relative offset

.install:
	mov byte [esi], 1		; Mark prologue as being used
	mov cx, [sel_code_flat]		; Add spurious IRQ detection prologue
	add edi, [kernel_addr]
	mov edx, edi

.done:
	pop edi
	pop esi
	pop ebx
	retn


;------------------------------------------------------------------------------
; Raw, VCPI - Install IRQ handler.
;------------------------------------------------------------------------------
; -> AL - IRQ number
;    CX:EDX - Selector:offset of the new handler
; <- CF - Set if error
;    EAX - Error code if CF set
;    AL - Actual IRQ number where the handler was installed
;------------------------------------------------------------------------------

	[bits 32]

rv_set_irq_handler:
	cmp al, 0x0f
	ja irq_error

	push ebx
	push ecx
	push edx
	push esi
	push edi
	push ds

	mov ds, cs:[sel_data_krnl32]
	call rvd_set_spurious_irq
	mov ds, cs:[sel_data_flat]

	; Set IRQ handler in IDT

	mov ebx, edx
	cmp al, 2			; IRQ 2 triggers on IRQ 9
	jne .set_vector
	mov al, 9

.set_vector:
	xor edx, edx			; EDX: interrupt number
	mov dl, al
	add dl, cs:[irq0_base]
	cmp al, 8
	jb .set_idt
	mov dl, al
	add dl, cs:[irq8_base]
	sub dl, 8

.set_idt:
	mov esi, cs:[idt_base]
	lea esi, [esi + edx * 8]

	shl ecx, 16
	mov cx, bx			; ECX: low 16 bits of offset
	mov edi, [esi + 4]
	and edi, 0xffff
	and ebx, 0xffff0000
	or edi, ebx			; EDI: high 16 bits of offset

	pushf				; Disable interrupts
	cli

	mov [esi], ecx
	mov [esi + 4], edi

	popf				; Restore interrupt state

	; Set the real mode redirector (for original PM IRQ reflector handler)
	; or real mode reflector (for custom PM IRQ handler)

	mov bx, cx
	shr ecx, 16			; CX:EBX: new IRQ handler
	cmp cx, cs:[sel_code_krnl32]
	jne .set_rm_reflector
	movzx esi, al
	shl esi, 2
	add esi, pm_irq_reflectors	; ESI: original PM IRQ reflector handler
	cmp esi, ebx
	jne .set_rm_reflector
	mov ebx, cs:[kernel_seg]	; Set real mode redirector
	movzx esi, al			; ESI: IRQ number
	shl ebx, 16
	lea ebx, [ebx + rm_irq_redirectors + esi * 4]
	jmp .set_rm_irq_handler

.set_rm_reflector:
	mov ebx, cs:[kernel_seg]	; Set real mode reflector
	movzx esi, al			; ESI: IRQ number
	shl ebx, 16
	lea ebx, [ebx + rm_irq_reflectors + esi * 4]

.set_rm_irq_handler:
	mov [edx * 4], ebx

	pop ds
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	clc
	retf


;------------------------------------------------------------------------------
; DPMI - Install IRQ handler.
;------------------------------------------------------------------------------
; -> AL - IRQ number
;    CX:EDX - Selector:offset of the new handler
; <- CF - Set if error
;    EAX - Error code if CF set
;    AL - Actual IRQ number where the handler was installed
;------------------------------------------------------------------------------

	[bits 32]

d_set_irq_handler:
	cmp al, 0x0f
	ja irq_error

	push eax
	push ds
	mov ds, cs:[sel_data_krnl32]
	call rvd_set_spurious_irq
	pop ds

	; Set IRQ handler by DPMI function

	cmp al, 2			; IRQ 2 triggers on IRQ 9
	jne .set_vector
	mov al, 9
	mov [esp], al			; Update AL in saved EAX

.set_vector:
	push ebx

	mov bl, al			; BL: interrupt number
	add bl, cs:[irq0_base]
	cmp al, 8
	jb .set_int
	mov bl, al
	add bl, cs:[irq8_base]
	sub bl, 8

.set_int:
	mov ax, 0x0205
	int 0x31

	pop ebx
	pop eax
	retf


;------------------------------------------------------------------------------
; Raw, VCPI, DPMI - Get PMI environment information.
;------------------------------------------------------------------------------
; <- EBX - Linear address of pmi_env_info structure.
;------------------------------------------------------------------------------

	[bits 32]

rvd_get_env_info:
	%define .info(field) env_info + pmi_env_info. %+ field

	push ds
	mov ds, cs:[sel_data_krnl32]

	mov ebx, [cmb_base]
	mov [.info(cmb_base)], ebx
	mov ebx, [cmb_size]
	mov [.info(cmb_size)], ebx
	mov ebx, [xmb_base]
	mov [.info(xmb_base)], ebx
	mov ebx, [xmb_size]
	mov [.info(xmb_size)], ebx
	xor ebx, ebx
	mov bx, [psp_seg]
	shl ebx, 4
	mov [.info(psp_addr)], ebx
	mov ebx, [current_pcb]
	mov [.info(pcb_addr)], ebx
	mov bx, [dos_ver]
	mov [.info(dos_ver)], bx
	mov bl, [pm_host_type]
	mov [.info(pm_host_type)], bl
	mov bl, [startup_vidmode]
	mov [.info(startup_vmode)], bl
	mov ebx, [io_buf_addr]
	mov [.info(io_buf_addr)], ebx
	mov dword [.info(io_buf_size)], IO_BUF_SIZE

	mov ebx, env_info
	add ebx, [kernel_addr]

	pop ds
	retf


;==============================================================================
; Data area
;==============================================================================

global exit_error
global pmi_fn
global kernel_seg
global kernel_addr
global page_mem_addr
global env_seg
global sel_code_flat
global sel_code_krnl16
global sel_data_krnl32
global sel_data_flat
global sel_pmi_fn
global cmb_base
global cmb_size
global xmb_base
global xmb_size
global env_flags
global pm_host_type
global dos_ver

;------------------------------------------------------------------------------
; Macro to create a GDT entry.
;------------------------------------------------------------------------------
; -> %1 - Linear base address (dword)
;    %2 - Limit (dword) in bytes or 4 KB
;    %3 - Flags (word)
;------------------------------------------------------------------------------

%macro		gdt_entry 3

		dw (%2 & 0xffff), (%1 & 0xffff)
		db ((%1 >> 16) & 0xff)
		dw (%3 & 0xf0ff) + ((%2 >> 8) & 0x0f00)
		db (%1 >> 24)

%endmacro

		alignb 8
gdt		dd 0, 0			; Global Descriptor Table
		SEL_CODE_FLAT EQU $ - gdt
gdt_code_flat	gdt_entry 0, 0xfffff, SEG_PRESENT | SEG_CS | SEG_CS_READABLE | SEG_CS_32BIT | DESC_DPL_0 | DESC_PAGE_GRAN
		SEL_DATA_FLAT EQU $ - gdt
gdt_data_flat	gdt_entry 0, 0xfffff, SEG_PRESENT | SEG_DS | SEG_DS_WRITABLE | SEG_DS_BIG | DESC_DPL_0 | DESC_PAGE_GRAN
		SEL_CODE_KRNL32 EQU $ - gdt
gdt_code_krnl32	gdt_entry 0, 0xfffff, SEG_PRESENT | SEG_CS | SEG_CS_READABLE | SEG_CS_32BIT | DESC_DPL_0 | DESC_PAGE_GRAN
		SEL_DATA_KRNL32 EQU $ - gdt
gdt_data_krnl32	gdt_entry 0, 0xfffff, SEG_PRESENT | SEG_DS | SEG_DS_WRITABLE | SEG_DS_BIG | DESC_DPL_0 | DESC_PAGE_GRAN
		SEL_CODE_KRNL16 EQU $ - gdt
gdt_code_krnl16	gdt_entry 0, 0xffff, SEG_PRESENT | SEG_CS | SEG_CS_READABLE | DESC_DPL_0
		SEL_STACK_RM EQU $ - gdt
gdt_stack_rm	gdt_entry 0, 0xffff, SEG_PRESENT | SEG_DS | SEG_DS_WRITABLE | DESC_DPL_0
		SEL_PMI_FN EQU $ - gdt
gdt_pmi_fn	gdt_entry 0, pmi_fns.strucsize - 1, SEG_PRESENT | SEG_DS | DESC_DPL_0
		SEL_TSS EQU $ - gdt
gdt_tss		gdt_entry 0, 0x67, SEG_PRESENT | SEG_TSS
		SEL_CODE_VCPI EQU $ - gdt
gdt_code_vcpi	dd 0, 0
		dd 0, 0			; VCPI reserved selector
		dd 0, 0			; VCPI reserved selector
		GDT_LIMIT EQU $ - gdt - 1

sel_code_flat	dd SEL_CODE_FLAT	; 32-bit flat memory model code
sel_data_flat	dd SEL_DATA_FLAT	; 32-bit flat memory model data
sel_code_krnl32	dd SEL_CODE_KRNL32	; 32-bit kernel-segment based code
sel_data_krnl32	dd SEL_DATA_KRNL32	; 32-bit kernel-segment based data
sel_code_krnl16	dd SEL_CODE_KRNL16	; 16-bit kernel-segment based code
sel_stack_rm	dd SEL_STACK_RM		; 16-bit stack
sel_pmi_fn	dd SEL_PMI_FN		; Data selector for PMI API functions
sel_code_vcpi	dd SEL_CODE_VCPI	; VCPI host code segment selector

vcpi_rm2pm_addr	dd vcpi_rm2pm_data	; Address of VCPI V86 to PM switch data
vcpi_stack	dd v_priv_data.stack	; VCPI temporary mode switch stack
pt_count	dd 0			; Number of page tables
xmb_handle	dd 0			; XMS/DPMI extended memory block handle
xmb_base	dd 0			; Base of available extended memory
xmb_size	dd 0			; Size of available extended memory

gdt_register	dw GDT_LIMIT		; GDT register
gdt_base	dd gdt

idt_register	dw 256 * 8		; Protected mode IDT register
idt_base	dd rv_priv_data.idt

vcpi_rm2pm_data	istruc v_rm2pm_data	; VCPI V86 to protected mode switch data
		at v_rm2pm_data.gdt_reg_addr, dd gdt_register
		at v_rm2pm_data.idt_reg_addr, dd idt_register
		at v_rm2pm_data.ldt_reg, dw 0
		at v_rm2pm_data.task_reg, dw SEL_TSS
		at v_rm2pm_data.eip, dd v_rm2pm.setup_pm
		at v_rm2pm_data.cs, dw SEL_CODE_KRNL32
		iend

pte_start	dw 0			; First available page table entry

rm_irq0_vector	dw 0x08 * 4		; Real mode master PIC base interrupt
rm_irq8_vector	dw 0x70 * 4		; Real mode slave PIC base interrupt
irq0_base	db 0xa0			; Real master PIC base interrupt
irq8_base	db 0xa8			; Real slave PIC base interrupt
old_irq0_base	db 0x08			; Old master PIC base interrupt
old_irq8_base	db 0x70			; Old slave PIC base interrupt

a20_type	db 0			; A20 handling method type
pm_host_type	db 0			; Protected mode host type

exit_error	db 0			; Termination exit error code
emmxxxx0	db 'EMMXXXX0', 0	; EMS driver name


;------------------------------------------------------------------------------
; Uninitialized data
;------------------------------------------------------------------------------

segment bss public use16 class=BSS align=4
segment bss

cmb_base	resd 1			; Base of available conventional memory
cmb_size	resd 1			; Size of available conventional memory
old_irq_ptrs	resd 16			; Old handler addresses for IRQs 0-15
kernel_addr	resd 1			; Kernel linear address
pmi_stack	resd 1			; Original stack before entering PM

rm_stack	resd 1			; Real mode stack
pm_stack	resd 2			; Protected mode stack

vcpi_host	resd 2			; VCPI host protected mode entry point
xmm		resd 1			; XMS manager API entry point
dpmi_rm2pm	resd 1			; DPMI real to protected mode switch ptr
dpmi_term_stack	resd 1			; DPMI termination stack linear address
dpmi_host_mem	resw 1			; Paragraphs required for DPMI host data
host_mem_seg	resw 1			; Host data memory segment
host_mem_addr	resd 1			; Host data memory linear address
page_mem_addr	resd 1			; Linear address of allocated pages
page_dir_phaddr	resd 1			; Physical address of page directory

pmi_fn		resb pmi_fns.strucsize	; PMI public functions
env_info	resb pmi_env_info.strucsize

pm2rm		resd 2			; Protected to real mode switch
rm2pm		resw 1			; Real to protected switch
rm_idt_register	resw 3			; Real mode IDT register
env_flags	resw 1			; Environment flags
kernel_seg	resw 1			; Kernel segment address
psp_seg		resw 1			; PSP segment address
env_seg		resw 1			; DOS environment segment address
dos_ver		resw 1			; Major/minor version of DOS
pic_masks	resw 1			; Original PIC IRQ masks

startup_vidmode	resb 1			; Video mode before starting application
exit_code	resb 1			; Exit code

		%ifdef DEBUG_EXCEPTIONS
exception_dump	resb exception_dump_data.strucsize
		%endif
