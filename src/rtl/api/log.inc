;==============================================================================
; PMI runtime library - Logging
;------------------------------------------------------------------------------
; API
;==============================================================================

%include "rtl/consts/log.inc"

extern __log_start
extern __log_stop
extern __log_flags
extern __log_mem
extern __log_str_format


;------------------------------------------------------------------------------
; Logging functions.
;------------------------------------------------------------------------------
; -> %1 - Function name or log level constant (LOG_ERROR, LOG_WARN, LOG_INFO
;         or LOG_DEBUG)
;------------------------------------------------------------------------------
; %1 = start: Start logging.
; -> %2 - Size of log buffer (maximum number of characters + 1)
;    %3 - Logging flags
;    %4 - Pointer to ASCIIZ log file name
;------------------------------------------------------------------------------
; %1 = stop: Stop logging.
;------------------------------------------------------------------------------
; %1 = flags: Dump the low word (16-bits) of CPU flags.
;------------------------------------------------------------------------------
; %1 = mem: Dump the list of conventional and extended memory blocks.
;------------------------------------------------------------------------------
; %1 = LOG_ERROR, LOG_WARN, LOG_INFO or LOG_DEBUG: Write formatted string
;      to log file using str_format.
; -> %2 - String to write, input for str_format
;    %3... - Variables for str_format (register, constant or memory variable).
;------------------------------------------------------------------------------

%macro log 1-*

	%if LOG_LEVEL > 0

	%ifidni %1, start
	push eax
	push ebx
	push ecx

	%ifnidni %2, ecx
	push %2				; Transfer parameters to registers
	%endif
	%ifnidni %3, eax
	push %3
	%endif
	%if (%0 > 3)
	%ifnidni %4, ebx
	push %4
	pop ebx
	%endif
	%endif
	%ifnidni %3, eax
	pop eax
	%endif
	%ifnidni %2, ecx
	pop ecx
	%endif
	call __log_start

	pop ecx
	pop ebx
	pop eax

	%elifidni %1, stop
	call __log_stop

	%elifidni %1, flags
	call __log_flags

	%elifidni %1, mem
	call __log_mem

	%elif LOG_LEVEL >= %1
	%rotate 1

	[section .data]

		align 4
%%eax		dd 0
%%ebp		dd 0
%%esp		dd 0
%%message	db %1, 0

	[section .log]

%%print:
	mov [%%ebp], ebp
	mov [%%esp], esp
	pushfd
	push esi
	push ebp
	mov ebp, esp

	%rotate 1			; Get rid of format source string
	%rep (%0 - 1)			; Push placeholder values to stack

	; 8-bit register

	%ifidni %1, al
	mov [%%eax], eax
	movzx eax, al
	push eax
	mov eax, [%%eax]
	%elifidni %1, ah
	mov [%%eax], eax
	movzx eax, ah
	push eax
	mov eax, [%%eax]
	%elifidni %1, bl
	mov [%%eax], eax
	movzx eax, bl
	push eax
	mov eax, [%%eax]
	%elifidni %1, bh
	mov [%%eax], eax
	movzx eax, bh
	push eax
	mov eax, [%%eax]
	%elifidni %1, cl
	mov [%%eax], eax
	movzx eax, cl
	push eax
	mov eax, [%%eax]
	%elifidni %1, ch
	mov [%%eax], eax
	movzx eax, ch
	push eax
	mov eax, [%%eax]
	%elifidni %1, dl
	mov [%%eax], eax
	movzx eax, dl
	push eax
	mov eax, [%%eax]
	%elifidni %1, dh
	mov [%%eax], eax
	movzx eax, dh
	push eax
	mov eax, [%%eax]

	; 16-bit register

	%elifidni %1, ax
	mov [%%eax], eax
	movzx eax, ax
	push eax
	mov eax, [%%eax]
	%elifidni %1, bx
	mov [%%eax], eax
	movzx eax, bx
	push eax
	mov eax, [%%eax]
	%elifidni %1, cx
	mov [%%eax], eax
	movzx eax, cx
	push eax
	mov eax, [%%eax]
	%elifidni %1, dx
	mov [%%eax], eax
	movzx eax, dx
	push eax
	mov eax, [%%eax]
	%elifidni %1, si
	mov [%%eax], eax
	movzx eax, si
	push eax
	mov eax, [%%eax]
	%elifidni %1, di
	mov [%%eax], eax
	movzx eax, di
	push eax
	mov eax, [%%eax]
	%elifidni %1, bp
	mov [%%eax], eax
	movzx eax, word [%%ebp]
	push eax
	mov eax, [%%eax]
	%elifidni %1, sp
	mov [%%eax], eax
	movzx eax, word [%%esp]
	add eax, 4			; Skip far return address
	push eax
	mov eax, [%%eax]

	; 32-bit register

	%elifidni %1, eax
	push eax
	%elifidni %1, ebx
	push ebx
	%elifidni %1, ecx
	push ecx
	%elifidni %1, edx
	push edx
	%elifidni %1, esi
	push esi
	%elifidni %1, edi
	push edi
	%elifidni %1, ebp
	mov [%%eax], eax
	mov eax, [%%ebp]
	push eax
	mov eax, [%%eax]
	%elifidni %1, esp
	mov [%%eax], eax
	mov eax, [%%esp]
	add eax, 4			; Skip return address
	push eax
	mov eax, [%%eax]
	%elifidni %1, eip
	mov [%%eax], eax
	mov eax, [%%esp]
	mov eax, ss:[eax]		; Get return address
	sub eax, 5			; Point before log message
	push eax
	mov eax, [%%eax]

	; Segment register

	%elifidni %1, cs
	mov [%%eax], eax
	mov eax, cs
	and eax, 0xffff
	push eax
	mov eax, [%%eax]
	%elifidni %1, ds
	o32 push ds
	%elifidni %1, es
	o32 push es
	%elifidni %1, fs
	o32 push fs
	%elifidni %1, gs
	o32 push gs
	%elifidni %1, ss
	o32 push ss

	; Constant or memory pointer

	%else
	push dword %1

	%endif

	%rotate 1
	%endrep

	mov esi, %%message
	call __log_str_format

	mov esp, ebp
	pop ebp
	pop esi
	popfd
	ret

	__SECT__

	call %%print

	%endif

	%endif

%endmacro
