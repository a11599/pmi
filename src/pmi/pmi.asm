;==============================================================================
; Protected mode interface - DOS init/exit stub
;==============================================================================

	cpu 386

	group pmi code
	group dgroup stack

segment code public use16 class=CODE align=16
segment code

%include "pmi/config.inc"
%include "pmi/api/pmi.inc"
%include "pmi/api/kernel.inc"
%include "pmi/consts/kernel.inc"
%include "pmi/structs/kernel.inc"

	cpu 8086			; 8086-compatible code ----------------

..start:
	mov ax, cs
	mov ds, ax

	; Check CPU, at least a 386 is required

	pushf				; 8086/8088: highest 4 bits can't be
	pop bx				; cleared in flags
	mov ax, bx
	and ah, 0x0f
	push ax
	popf
	pushf
	pop ax
	and ah, 0x0f
	cmp ah, 0x0f
	je .no_386

	or ah, 0xf0			; 80286: highest 4 bits can't be set in
	push ax				; flags
	popf
	pushf
	pop ax
	test ah, 0xf0
	jz .no_386

	push bx				; Restore flags to original state
	popf				; Restore flags to original state

	cpu 386				; 386 code ----------------------------

	; Determine available conventional memory

	xor eax, eax
	mov ax, ss
	shl eax, 4
	xor ebx, ebx
	mov bx, sp
	add ebx, eax			; EBX: base address of low memory
	xor ecx, ecx
	mov cx, es:[0x02]
	shl ecx, 4
	sub ecx, ebx			; ECX: size of available low memory

	; Start protected mode kernel and the PE executable

	call kernel_start
	jc .error

	; PE executable terminated, back to real mode

.exit:
	jmp exit_to_dos

	cpu 8086			; 8086-compatible code ----------------

.no_386:
	xor ah, ah

.error:
	%ifdef DEBUG_EXCEPTIONS
	cmp ah, PMI_E_EXCEPTION
	jne .lookup_error_msg
	push ax
	call exception_print
	pop ax
	jmp .terminate
	%endif

.lookup_error_msg:
	mov bx, err_tab			; Lookup error message
	mov cx, ERR_TAB_ENTRIES

.error_msg_loop:
	cmp [bx], ah
	je .print_msg
	add bx, ERR_TAB_ENTRY
	dec cx
	jnz .error_msg_loop

.print_msg:
	mov ah, 0x09			; Print error message
	mov dx, [bx + 1]
	int 0x21

.terminate:
	mov al, -1
	jmp .exit

	cpu 386				; 386 code ----------------------------


;==============================================================================
; Exception dump
;==============================================================================

%ifdef DEBUG_EXCEPTIONS

;------------------------------------------------------------------------------
; Print $-terminated string to stdout
;------------------------------------------------------------------------------
; -> %1 - Offset of text to display
; <- Destroys AH, DX
;------------------------------------------------------------------------------

%macro	exc_print 1

	%ifnidni %1, dx
	lea dx, [%1]
	%endif
	mov ah, 0x09
	int 0x21

%endmacro


;------------------------------------------------------------------------------
; Convert 4-bit, 8-bit, 16-bit or 32-bit hexadecimal value to string
;------------------------------------------------------------------------------
; -> %1 - Value to display
;    %2 - String buffer receiving converted value
;    DS - String buffer segment
; <- Destroys EAX, CX, SI
;------------------------------------------------------------------------------

%macro	exc_hexn 2

	%ifnidni %1, al
	mov al, %1
	%endif
	%ifnidni %2, bx
	mov bx, %2
	%endif
	mov cl, 1
	call exception_hex2str

%endmacro

%macro	exc_hexb 2

	%ifnidni %1, al
	mov al, %1
	%endif
	%ifnidni %2, bx
	mov bx, %2
	%endif
	mov cl, 2
	call exception_hex2str

%endmacro

%macro	exc_hexw 2

	%ifnidni %1, ax
	mov ax, %1
	%endif
	%ifnidni %2, bx
	mov bx, %2
	%endif
	mov cl, 4
	call exception_hex2str

%endmacro

%macro	exc_hexd 2

	%ifnidni %1, eax
	mov eax, %1
	%endif
	%ifnidni %2, bx
	mov bx, %2
	%endif
	mov cl, 8
	call exception_hex2str

%endmacro


;------------------------------------------------------------------------------
; Dump exception
;------------------------------------------------------------------------------
; -> DS - PMI segment
;    ES:DI - Pointer to exception_dump_data structure
; <- Destroys everything except segment registers and ES:DI
;------------------------------------------------------------------------------

exception_print:
	%define .dump(data) [es:di + exception_dump_data. %+ data]

	; Segment register dump
	; -> %1 - Segment register
	;    %2 - Segment register name's first character

	%macro exception_print_selector 2
	exc_hexw .dump(%1), exc_seg_sel
	exc_hexd .dump(%1_base), exc_seg_base
	exc_hexd .dump(%1_limit), exc_seg_limit
	exc_hexb .dump(%1_flags), exc_seg_accb
	mov ax, .dump(%1_flags)
	shr ax, 12
	exc_hexn al, exc_seg_flags
	mov byte [exc_seg_rname], %2
	exc_print exc_seg_reg
	%endmacro

	; Exception header

	exc_print msg_err_exc		; Standard panic error message
	exc_print exc_crlf
	xor ebx, ebx
	mov bl, .dump(exception)
	mov dx, [ebx * 2 + exc_tab]	; Lookup exception name
	exc_print dx
	exc_hexb .dump(exception), exc_nr
	exc_hexw .dump(cs), exc_ptr
	exc_hexd .dump(eip), exc_ptr_ofs
	mov eax, .dump(cs_base)
	add eax, .dump(eip)
	exc_hexd eax, exc_addr
	exc_print exc_header

	; Fault code when present (not 0)

	mov ecx, .dump(fault_code)
	jecxz .dump_regs
	exc_hexd ecx, exc_fault_code
	exc_print exc_fault

.dump_regs:

	; Segment registers

	exception_print_selector cs, 'C'
	exception_print_selector ds, 'D'
	exception_print_selector es, 'E'
	exception_print_selector fs, 'F'
	exception_print_selector gs, 'G'
	exception_print_selector ss, 'S'

	; General registers

	exc_hexd .dump(eax), exc_eax
	exc_hexd .dump(ebx), exc_ebx
	exc_hexd .dump(ecx), exc_ecx
	exc_hexd .dump(edx), exc_edx
	exc_hexd .dump(esi), exc_esi
	exc_hexd .dump(edi), exc_edi
	exc_hexd .dump(ebp), exc_ebp
	exc_hexd .dump(esp), exc_esp
	exc_print exc_regs

	; Dump stack

	push di
	mov ax, ss			; Make room for buffer on stack
	mov ds, ax
	sub sp, 80			; DS:SP: string buffer
	mov ebp, .dump(esp)		; EBP: Original stack base (ESP)
	lea di, .dump(stack)		; ES:DI: stack dump pointer
	mov cx, 64			; CX: number of words in stack dump

.dump_stack_loop:
	test cl, 0xf
	jnz .dump_stackd_sep

	mov eax, ebp			; New stack dump line, show original ESP
	mov bx, sp			; DS:BX: string buffer start
	push cx
	mov cl, 8
	call exception_hex2str
	pop cx
	add bx, 8

.dump_stackd_sep:
	test cl, 0x3
	jnz .dump_stack_val
	mov word [bx], 0xb320		; Extra separator every 4 bytes
	inc bx
	inc bx

.dump_stack_val:
	mov byte [bx], ' '		; Value/offset separator
	inc bx

	mov al, es:[di]			; Stack dump value
	push cx
	mov cl, 2
	call exception_hex2str
	pop cx
	add bx, 2

	inc di				; Next byte
	dec cl
	test cl, 0xf
	jnz .dump_stack_next

	mov word [bx], 0x0a0d		; Print stack dump line (16 bytes)
	mov byte [bx + 2], '$'
	mov dx, sp
	mov ah, 0x09
	int 0x21
	add ebp, 0x10			; Adjust stack base pointer

.dump_stack_next:
	test cl, cl
	jnz .dump_stack_loop

	add sp, 80
	mov ax, cs
	mov ds, ax
	pop di

	retn


;------------------------------------------------------------------------------
; Print hexadecimal number to stdout
;------------------------------------------------------------------------------
; -> EAX - Number to convert to string
;    CL - Digits to print from right
;    DS:BX - String buffer receiving data
; <- Destroys EAX, CX, SI
;------------------------------------------------------------------------------

exception_hex2str:
	xor ch, ch
	add bx, cx

.convert_loop:
	dec bx
	mov si, ax			; Get last digit
	and si, 0xf
	mov ch, cs:[exc_hextab + si]	; Convert to hexadecimal string
	mov [bx], ch			; Store in string buffer
	shr eax, 4
	dec cl
	jnz .convert_loop

	retn

%endif


;==============================================================================
; Data area
;==============================================================================

err_tab		db 0
		dw msg_err_cpu
		ERR_TAB_ENTRY EQU $ - err_tab
		db PMI_E_V86
		dw msg_err_v86
		db PMI_E_MEM_LOW
		dw msg_err_mem
		db PMI_E_DPMI_INIT
		dw msg_err_dpmi
		db PMI_E_MEM_INVL
		dw msg_err_mem_inv
		db PMI_E_EXCEPTION
		dw msg_err_exc
		db PMI_E_INV_PE
		dw msg_err_pe
		ERR_TAB_ENTRIES EQU ($ - err_tab) / 3
		db 0
		dw msg_err_generic

msg_err_cpu	db ':( No 386', 13, 10, '$'
msg_err_exc	db ':( Game over', 13, 10, '$'
msg_err_v86	db ':( CPU in V86 mode and no VCPI or DPMI found', 13, 10, '$'
msg_err_mem	db ':( Insufficient memory', 13, 10, '$'
		global msg_err_dpmi	; Also used by kernel if DPMI init fails
msg_err_dpmi	db ':( Cannot initialize DPMI host', 13, 10, '$'
msg_err_mem_inv	db ':( Memory corrupted', 13, 10, '$'
msg_err_pe	db ':( Cannot load protected mode program', 13, 10, '$'
msg_err_generic	db ':( Cannot initialize protected mode', 13, 10, '$'

		%ifdef DEBUG_EXCEPTIONS

exc_tab		dw exc_00
		dw exc_01
		dw exc_unknown
		dw exc_unknown
		dw exc_04
		dw exc_05
		dw exc_06
		dw exc_07
		dw exc_08
		dw exc_09
		dw exc_0a
		dw exc_0b
		dw exc_0c
		dw exc_0d
		dw exc_0e
		dw exc_unknown
		dw exc_10
		dw exc_11
		dw exc_12
		dw exc_13
		dw exc_14
		dw exc_15
		dw exc_unknown
		dw exc_unknown
		dw exc_unknown
		dw exc_unknown
		dw exc_unknown
		dw exc_unknown
		dw exc_1c
		dw exc_1d
		dw exc_1e
		dw exc_unknown

exc_00		db 'Division error$'
exc_01		db 'Debug$'
exc_04		db 'Overflow$'
exc_05		db 'Bound range exceeded$'
exc_06		db 'Invalid opcode$'
exc_07		db 'Device not available$'
exc_08		db 'Double fault$'
exc_09		db 'Coprocessor segment overrun$'
exc_0a		db 'Invalid TSS$'
exc_0b		db 'Segment not present$'
exc_0c		db 'Stack-segment fault$'
exc_0d		db 'General protection fault$'
exc_0e		db 'Page fault$'
exc_10		db 'x87 floating-point$'
exc_11		db 'Alignment check$'
exc_12		db 'Machine check$'
exc_13		db 'x87 SIMD floating-point$'
exc_14		db 'Virtualization$'
exc_15		db 'Control protection$'
exc_1c		db 'Hypervisor injection$'
exc_1d		db 'VMM communication$'
exc_1e		db 'Security$'
exc_unknown	db 'Unknown$'

exc_hextab	db '0123456789ABCDEF'
exc_header	db ' exception ('
exc_nr		db '00) at '
exc_ptr		db '0000:'
exc_ptr_ofs	db '00000000, address '
exc_addr	db '00000000', 13, 10, '$'
exc_fault	db 'Fault code: '
exc_fault_code	db '00000000', 13, 10, '$'
exc_seg_reg	db 13, 10
exc_seg_rname	db 'CS: '
exc_seg_sel	db '0000   base: '
exc_seg_base	db '00000000   limit: '
exc_seg_limit	db '00000000   access byte: '
exc_seg_accb	db '00   flags: '
exc_seg_flags	db '0$'
exc_regs	db 13, 10, 10, 'EAX: '
exc_eax		db '00000000   EBX: '
exc_ebx		db '00000000   ECX: '
exc_ecx		db '00000000   EDX: '
exc_edx		db '00000000', 13, 10, 'ESI: '
exc_esi		db '00000000   EDI: '
exc_edi		db '00000000   EBP: '
exc_ebp		db '00000000   ESP: '
exc_esp		db '00000000', 13, 10, 10, 'Stack:'
exc_crlf	db 13, 10, '$'

		%endif


;==============================================================================
; Stack
;==============================================================================

segment stack stack class=STACK align=16
segment stack

	resb 4096
