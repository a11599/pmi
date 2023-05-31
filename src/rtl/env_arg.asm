;==============================================================================
; PMI runtime library - DOS environment and argument functions
;==============================================================================

	cpu 386

section .text

%include "pmi/api/pmi.inc"


;------------------------------------------------------------------------------
; Setup environment and argument functions.
;------------------------------------------------------------------------------
; -> EBX - Linear address of MS-DOS environment variables
;    EDI - Linear address of ASCIIZ arguments
; <- CF - Set if error
;    EAX - Error code if CF set
;------------------------------------------------------------------------------

	align 4

global env_arg_setup
env_arg_setup:
	push ecx
	push esi
	push edi
	push eax

	mov [env_addr], ebx		; Save DOS environment linear address
	mov dword [args_addr], 0	; No arguments (yet)

	; Allocate memory for converted arguments

	mov ecx, -1			; ECX: length of argument string

.arg_len_loop:
	inc ecx
	cmp byte [edi + ecx], 0
	jne .arg_len_loop
	jecxz .done

	mov al, PMI_MEM_HI_LO
	add ecx, 2			; Room for terminating 0s
	call pmi(mem_alloc)
	jc .error
	mov [args_addr], eax
	mov esi, eax			; ESI: ASCIIZ list of arguments
	sub ecx, 2			; ECX: length of argument string

	; Convert argument string to a list of ASCIIZ strings. The list is
	; terminated with an empty string (byte 0x00).

.skip_spaces_loop:
	mov al, [edi]			; Get next character from arg string
	inc edi
	cmp al, ' '
	setbe ah
	test ah, ah
	loopnz .skip_spaces_loop, ecx	; Skip while space or control character
	jecxz .next_arg			; End of arguments

.save_arg_chars_loop:
	mov [esi], al			; Not space or control character, store
	mov al, [edi]			; Get next character
	inc esi
	inc edi
	cmp al, ' '
	setbe ah
	test ah, ah
	loopz .save_arg_chars_loop, ecx	; Store while not space or control char
	jnz .next_arg			; Space or control char: next argument
	mov [esi], al			; Store last non-space or control char
	inc esi

.next_arg:
	mov byte [esi], 0		; Store 0 as argument separator
	inc esi
	jecxz .done_args		; End of arguments
	jmp .skip_spaces_loop		; Find start of next argument

.done_args:
	mov byte [esi], 0		; Store final 0 as end of argument list

.done:
	pop eax
	clc

.exit:
	pop edi
	pop esi
	pop ecx
	ret

.error:
	add esp, 4			; Discard EAX from stack
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Close environment and argument functions. Discards argument buffer and any
; further calls to get arguments will return as if no arguments were provided.
; Environment variable-related functions will still be available.
;------------------------------------------------------------------------------
; <- CF - Set if error
;    EAX - Error code if CF set
;------------------------------------------------------------------------------

	align 4

global env_arg_close
env_arg_close:
	push eax

	mov eax, [args_addr]
	test eax, eax
	jz .done
	call pmi(mem_free)		; Release argument buffer
	jc .error

.done:
	pop eax
	clc

.exit:
	ret

.error:
	add esp, 4
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Get the value of an environment variable.
;------------------------------------------------------------------------------
; -> ESI - Linear address of ASCIIZ uppercase environment variable name
; <- CF - Set if variable not found
;    ESI - Linear address of ASCIIZ environment variable value if found
;------------------------------------------------------------------------------

	align 4

global env_get_value
env_get_value:
	push eax
	push ebx
	push edi
	push esi

	mov ebx, esi			; EBX: wanted variable name
	mov esi, [env_addr]		; ESI: DOS environment area
	test esi, esi
	jz .not_found

.check_env_var_loop:
	xor edi, edi			; EDI: index into wanted variable name

.check_env_name_loop:
	mov ah, [ebx + edi]		; AH: next char of wanted variable name
	mov al, [esi]			; AL: next character of variable name
	inc edi
	inc esi
	test al, al			; End of environment area: not found
	jz .not_found
	cmp al, '='			; End of environment variable name
	je .check_name
	cmp al, 'a'			; Convert variable name to uppercase
	jb .compare_name
	cmp al, 'z'
	ja .compare_name
	sub al, 'a' - 'A'

.compare_name:
	cmp al, ah			; Same character, continue with next
	je .check_env_name_loop

.skip_env_var:
	inc esi				; Skip to next 0 byte
	cmp byte [esi - 1], 0
	jne .skip_env_var
	jmp .check_env_var_loop

.check_name:
	test ah, ah			; Check end of wanted variable name
	jnz .skip_env_var		; Nope, no match

	add esp, 4			; Discard ESI from stack
	clc

.exit:
	pop edi
	pop ebx
	pop eax
	ret

.not_found:
	stc
	pop esi
	jmp .exit


;------------------------------------------------------------------------------
; Get the number of arguments.
;------------------------------------------------------------------------------
; <- CL - Number of arguments
;------------------------------------------------------------------------------

	align 4

global arg_get_count
arg_get_count:
	push esi

	mov esi, [args_addr]		; ESI: program arguments
	xor cl, cl			; CL: number of arguments
	test esi, esi
	jz .exit			; No arguments
	dec esi				; For pre-increment

.count_arg_loop:
	inc esi
	cmp byte [esi], 0
	je .exit			; End of arguments

.skip_arg_loop:
	inc esi				; Skip argument
	cmp byte [esi], 0
	jnz .skip_arg_loop
	inc cl				; Next argument
	jmp .count_arg_loop

.exit:
	pop esi
	ret


;------------------------------------------------------------------------------
; Get the value of an argument.
;------------------------------------------------------------------------------
; -> CL - Index of the argument to retrieve (0-based)
; <- CF - Set if argument not found
;    ESI - Linear address of ASCIIZ argument if found
;------------------------------------------------------------------------------

	align 4

global arg_get
arg_get:
	push ebx
	push esi

	mov esi, [args_addr]		; ESI: program arguments
	test esi, esi
	jz .not_found			; No arguments
	xor bl, bl			; BL: index of current argument

.find_arg_loop:
	cmp bl, cl			; Check argument index match
	je .check_arg			; ESI points to start of argument
	cmp byte [esi], 0
	jz .not_found			; End of argument area

.skip_arg_loop:
	inc esi				; Skip argument
	cmp byte [esi], 0
	jne .skip_arg_loop
	inc bl				; Next argument
	inc esi				; Skip ASCIIZ terminator
	jmp .find_arg_loop

.check_arg:
	cmp byte [esi], 0		; Check end of argument area
	je .not_found

	add esp, 4			; Discard ESI from stack
	clc

.exit:
	pop ebx
	ret

.not_found:
	stc
	pop esi
	jmp .exit


;------------------------------------------------------------------------------
; Get the value of a named argument.
;------------------------------------------------------------------------------
; -> ESI - Linear address of ASCIIZ argument name
; <- CF - Set if argument not found
;    ESI - Linear address of ASCIIZ argument value if found
;------------------------------------------------------------------------------

	align 4

global arg_get_value
arg_get_value:
	push eax
	push ebx
	push edi
	push esi

	mov ebx, esi			; EBX: wanted argument name
	mov esi, [args_addr]		; ESI: program arguments
	test esi, esi
	jz .not_found			; No arguments

.check_args_loop:
	xor edi, edi			; EDI: index into wanted argument name
	cmp byte [esi], 0
	je .not_found			; End of argument area, not found

.check_arg_loop:
	mov ah, [ebx + edi]		; AH: next char of wanted argument name
	mov al, [esi]			; AL: next character of argument name
	inc edi
	test al, al			; End of argument
	jz .check_arg
	inc esi
	cmp al, ':'			; End of argument name
	je .check_arg
	cmp al, ah			; Same character, continue with next
	je .check_arg_loop

.skip_arg:
	test al, al
	jnz .skip_arg_loop
	inc esi
	jmp .check_args_loop		; Argument already ended

.skip_arg_loop:
	inc esi				; Skip to next argument
	cmp byte [esi - 1], 0
	jne .skip_arg_loop
	jmp .check_args_loop

.check_arg:
	test ah, ah			; Check end of wanted variable name
	jnz .skip_arg			; Nope, no match

	add esp, 4			; Discard ESI from stack
	clc

.exit:
	pop edi
	pop ebx
	pop eax
	ret

.not_found:
	stc
	pop esi
	jmp .exit


;==============================================================================
; Data area
;==============================================================================

section .data

env_addr	dd 0			; Linear address of environment
args_addr	dd 0			; Linear address of argument list
