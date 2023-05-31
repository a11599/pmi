;==============================================================================
; PMI runtime library - Logging
;------------------------------------------------------------------------------
; -> LOG_LEVEL environment variable:
;    ERROR - Log errors
;    WARN - Log warnings
;    INFO - Log info messages
;    DEBUG - Log debug messages
;    Logging is disabled if the variable is not set or set to any other value.
;------------------------------------------------------------------------------
; These functions are not meant to be called directly. Use the log macro in
; api/rtl/log.inc instead.
;==============================================================================

	cpu 386

section .text

%include "pmi/api/pmi.inc"
%include "pmi/structs/memory.inc"
%include "pmi/consts/memory.inc"
%include "pmi/structs/program.inc"
%include "rtl/api/string.inc"
%include "rtl/consts/log.inc"


;------------------------------------------------------------------------------
; Start logging. No log messages will be written until this function is called.
;------------------------------------------------------------------------------
; -> EAX - Logging flags
;    EBX - Pointer to ASCIIZ filename for LOG_FILE
;    ECX - Length of the buffer (maximum number of characters + 1)
; <- CF - Set if error
;    EAX - Error code if CF set
;------------------------------------------------------------------------------

	align 4

global __log_start
__log_start:
	push eax

	mov [flags], eax

	lea eax, [ecx - 1]		; Character capacity = size - 1
	mov [buf_chars], eax
	mov al, PMI_MEM_LO_HI		; Allocate log buffer
	call pmi(mem_alloc)
	jc .error
	mov [buf_addr], eax

	mov eax, [flags]
	test al, LOG_FILE
	jz .console			; Log to console
	mov ah, PMI_FILE_WRITE | PMI_FILE_CREATE
	test al, LOG_APPEND
	jnz .append
	or ah, PMI_FILE_TRUNC		; Overwrite existing when not appending

.append:
	test al, LOG_AUTOCOMMIT
	jz .open
	or ah, PMI_FILE_COMMIT		; Enable autocommit

.open:
	xchg al, ah
	call pmi(file_open)
	jc .file_error
	mov [file_handle], eax

	test dword [flags], LOG_APPEND
	jz .done
	push ebx			; Seek to end of file when appending
	push ecx
	mov ebx, eax
	xor ecx, ecx
	mov al, PMI_SEEK_END
	call pmi(file_set_pos)
	pop ecx
	pop ebx

.done:
	clc
	pop eax

.exit:
	ret

.file_error:
	mov eax, [buf_addr]
	call pmi(mem_free)

.error:
	mov dword cs:[file_handle], 0
	add sp, 4			; Discard EAX from stack
	stc
	jmp .exit

.console:
	test al, LOG_STDERR
	jz .stderr
	mov dword [file_handle], 1	; File handle for standard output
	jmp .done

.stderr:
	mov dword [file_handle], 2	; File handle for standard error output
	jmp .done


;------------------------------------------------------------------------------
; Stop logging. Should be called before terminating application. No messages
; will be logged after this call.
;------------------------------------------------------------------------------

	align 4

global __log_stop
__log_stop:
	push eax
	push ebx

	mov ebx, [file_handle]
	cmp ebx, 4
	jbe .exit			; Not an actual file, no need to close
	call pmi(file_close)		; Close logfile

.exit:
	mov dword [file_handle], 0	; Stop logging
	pop ebx
	pop eax
	ret


;------------------------------------------------------------------------------
; Dump lower 16-bits of the CPU flags to the log.
;------------------------------------------------------------------------------

	align 4

global __log_flags
__log_flags:
	cmp dword [file_handle], 0	; Logging not started, exit
	je .noop

	push eax
	push ebx
	push ecx
	push esi
	push edi
	push ebp

	pushfd
	pop eax

	mov ebp, esp
	mov esi, 0x8000

.check_flags_loop:
	test eax, esi			; Test flag bit
	setnz bl
	push ebx			; Save flag value (0 or 1)
	shr esi, 1			; Next bit
	jnz .check_flags_loop

	mov esi, flags_fmt		; Format flags string
	mov edi, [buf_addr]
	mov ecx, [buf_chars]
	call str_format
	mov esp, ebp
	mov esi, edi
	call str_len			; ECX: length of string
	mov ebx, [file_handle]
	call pmi(file_write)

	pop ebp
	pop edi
	pop esi
	pop ecx
	pop ebx
	pop eax

.noop:
	ret


;------------------------------------------------------------------------------
; Log memory blocks. Useful as a debug tool to check proper memory releases.
;------------------------------------------------------------------------------

global __log_mem
__log_mem:
	push eax
	push ebx
	push ecx
	push edx
	push esi
	push edi
	push ebp

	call pmi(get_env_info)
	mov edi, ebx			; EDI: PMI environment info

	mov ebp, esp			; Log conventional memory blocks
	mov esi, mcbs_cmb
	call __log_str_format
	mov ebx, [edi + pmi_env_info.cmb_base]
	test ebx, ebx
	jz .xmb
	call .log_area_mcbs

.xmb:
	mov ebp, esp			; Log extended memory blocks
	mov esi, mcbs_xmb
	call __log_str_format
	mov ebx, [edi + pmi_env_info.xmb_base]
	test ebx, ebx
	jz .done
	call .log_area_mcbs

.done:
	pop ebp
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	pop eax
	ret

; Log memory blocks in memory area.
; -> EBX - Base address of memory block
;    EDI - pmi_env_info structure

.log_area_mcbs:
	mov edx, -1			; EDX: expected previous block pointer

.loop_mcb:

	; Log block details

	mov ebp, esp
	add ebx, mcb.strucsize
	push ebx			; Memory block start address
	sub ebx, mcb.strucsize
	cmp word [ebx + mcb.signature], MCB_SIGNATURE
	jne .chain_broken

	test word [ebx + mcb.status], MCB_USED
	jnz .used
	push mcbs_blk_free		; Usage status (free)
	jmp .log_size

.used:
	push mcbs_blk_used		; Usage status (used)

.log_size:
	mov eax, [ebx + mcb.size]
	push eax			; Block size
	mov esi, mcbs_blk
	call __log_str_format
	mov esp, ebp			; Discard string variables

	cmp edx, [ebx + mcb.prev]	; Validate pointer to previous block
	je .check_block_use
	push edx			; Expected previous pointer
	push dword [ebx + mcb.prev]	; Actual previous pointer
	mov esi, mcbs_broken_prv
	call __log_str_format
	mov esp, ebp			; Discard string variables

.check_block_use:
	test word [ebx + mcb.status], MCB_USED
	jz .check_next

	; Log purpose of allocated block

	lea edx, [ebx + mcb.strucsize]
	cmp edx, [edi + pmi_env_info.io_buf_addr]
	je .used_io_buf
	cmp edx, [buf_addr]
	je .used_log_buf
	mov esi, [edi + pmi_env_info.pcb_addr]

.check_pcb_loop:
	cmp edx, esi
	je .used_pcb
	cmp edx, [esi + pcb.mem_addr]
	je .used_app
	cmp edx, [esi + pcb.stack_addr]
	je .used_stack
	mov esi, [esi + pcb.caller]
	test esi, esi
	jnz .check_pcb_loop
	jmp .check_next

.used_stack:
	push dword [esi + pcb.file]	; Program file name
	mov esi, mcbs_blk_stack		; Used for application stack
	jmp .log_user

.used_app:
	push dword [esi + pcb.file]	; Program file name
	mov esi, mcbs_blk_app		; Used for application code and data
	jmp .log_user

.used_pcb:
	push dword [esi + pcb.file]	; Program file name
	mov esi, mcbs_blk_pcb		; Used for program control block
	jmp .log_user

.used_log_buf:
	mov esi, mcbs_blk_log		; Used for log string buffer
	jmp .log_user

.used_io_buf:
	mov esi, mcbs_blk_io		; Used for I/O buffer

.log_user:
	call __log_str_format
	mov esp, ebp

.check_next:
	cmp dword [ebx + mcb.next], -1
	je .exit			; No more memory blocks

	; Log memory hole (should not be any)

	lea eax, [ebx + eax + mcb.strucsize]
	cmp eax, [ebx + mcb.next]	; Check for memory hole
	je .next_mcb
	push eax			; Memory hole start address
	push mcbs_blk_hole		; Usage status (hole)
	mov edx, [ebx + mcb.next]
	sub edx, eax
	push edx			; Memory hole size
	call __log_str_format
	mov esp, ebp			; Discard string variables

.next_mcb:
	mov edx, ebx			; EDX: expected previous block pointer
	mov ebx, [ebx + mcb.next]	; EBX: move to next block
	jmp .loop_mcb

.chain_broken:
	mov esi, mcbs_broken		; Memory block chain broken
	call __log_str_format
	mov esp, ebp			; Discard string variables

.exit:
	ret


;------------------------------------------------------------------------------
; Print formatted text to the log.
;------------------------------------------------------------------------------
; -> ESI - Pointer to ASCIIZ string message
;    EBP - Pointer to just above first variable value on stack for str_format
;------------------------------------------------------------------------------

	align 4

global __log_str_format
__log_str_format:
	cmp dword [file_handle], 0	; Logging not started, exit
	je .noop

	push eax
	push ebx
	push ecx
	push esi
	push edi

	mov edi, [buf_addr]
	mov ecx, [buf_chars]
	call str_format
	mov esi, edi
	call str_len			; ECX: length of string
	mov ebx, [file_handle]
	call pmi(file_write)

.done:
	pop edi
	pop esi
	pop ecx
	pop ebx
	pop eax

.noop:
	ret


section .data

;==============================================================================
; Data area
;==============================================================================

flags		dd 0			; Logging flags
file_handle	dd 0			; Logging not initialized (0)
buf_addr	dd 0			; Linear address of log buffer
buf_chars	dd 0			; Character capacity of log buffer

flags_fmt	db '+---+---+------+---+---+---+---+---+---+---+---+---+---+---+---+', 13, 10
		db '| - | N | IOPL | O | D | I | T | S | Z | - | A | - | P | - | C |', 13, 10
		db '| {u8} | {u8} |  {u8}{u8}  | {u8} | {u8} | {u8} | {u8} | {u8} | {u8} | {u8} | {u8} | {u8} | {u8} | {u8} | {u8} |', 13, 10
		db '+---+---+------+---+---+---+---+---+---+---+---+---+---+---+---+', 13, 10, 0

mcbs_cmb	db 'Conventional memory blocks', 13, 10, 0
mcbs_xmb	db 'Extended memory blocks', 13, 10, 0
mcbs_blk	db '- 0x{X}: {s}, {u} bytes', 13, 10, 0
mcbs_blk_free	db 'free', 0
mcbs_blk_used	db 'used', 0
mcbs_blk_hole	db 'hole', 0
mcbs_blk_io	db '  I/O buffer for file services address translation', 13, 10, 0
mcbs_blk_log	db '  Log message formatting buffer', 13, 10, 0
mcbs_blk_pcb	db '  Program control block for {s}', 13, 10, 0
mcbs_blk_app	db '  Application code and data for {s}', 13, 10, 0
mcbs_blk_stack	db '  Application stack for {s}', 13, 10, 0
mcbs_broken	db '  !! Memory block chain broken at 0x{X}, cannot find proper header!', 13, 10, 0
mcbs_broken_prv	db '  !! Invalid previous block pointer, expecting 0x{X}, got 0x{X}', 13, 10, 0
