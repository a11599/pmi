;==============================================================================
; PMI runtime library - String operations
;==============================================================================

	cpu 386

section .text

%include "rtl/consts/string.inc"


;------------------------------------------------------------------------------
; Return the length of an ASCIIZ string.
;------------------------------------------------------------------------------
; -> ESI - Pointer to ASCIIZ string
; <- ECX - Length of the string
;------------------------------------------------------------------------------

	align 4

global str_len
str_len:
	mov ecx, -1			; Pre-increment

.find_len_loop:
	inc ecx
	cmp byte [esi + ecx], 0
	jne .find_len_loop

	ret


;------------------------------------------------------------------------------
; Reverse a string in place.
;------------------------------------------------------------------------------
; -> ESI - Pointer to ASCIIZ string
;------------------------------------------------------------------------------

	align 4

global str_reverse
str_reverse:
	push eax
	push ecx
	push esi
	push edi

	call str_len			; Get length of string
	lea edi, [esi + ecx]		; EDI: end of string + 1
	shr ecx, 1			; Reverse until half of string
	jz .exit

.next_char:
	dec edi				; Move backward end of string pointer
	mov al, [esi]			; Exchange characters
	xchg al, [edi]
	mov [esi], al
	inc esi				; Move forward start of string pointer
	dec ecx
	jnz .next_char

.exit:
	pop edi
	pop esi
	pop ecx
	pop eax
	ret


;------------------------------------------------------------------------------
; Copy a string to a target buffer.
;------------------------------------------------------------------------------
; -> ECX - Maximum length of the buffer
;    ESI - Source ASCIIZ string
;    EDI - Target buffer
;------------------------------------------------------------------------------

	align 4

global str_copy
str_copy:
	test ecx, ecx
	jz .return

	push eax
	push ecx
	push esi
	push edi

	cld

	dec ecx				; Leave room for terminating NUL
	jz .done

.copy_loop:
	mov al, [esi]			; Copy next character
	mov [edi], al
	inc esi
	inc edi
	test al, al			; End of string?
	jz .done
	dec ecx				; End of buffer?
	jnz .copy_loop

.done:
	mov byte [edi], 0		; Store terminating NUL

	pop edi
	pop esi
	pop ecx
	pop eax

.return:
	ret


;------------------------------------------------------------------------------
; Append a string at the end of another one in place.
;------------------------------------------------------------------------------
; -> ECX - Maximum length of the appended string
;    ESI - Source ASCIIZ string
;    EDI - Target ASCIIZ string to which the source will be appended in place
;------------------------------------------------------------------------------

	align 4

global str_append
str_append:
	push eax
	push ecx
	push edx
	push esi
	push edi

	xor edx, edx			; EDX: length of target string

.check_end:
	cmp byte [edi + edx], 0
	jz .append			; End of target string
	inc edx
	jmp .check_end

.append:
	cmp edx, ecx
	jae .exit
	sub ecx, edx			; ECX: characters left remaining
	add edi, edx			; EDI: end of target string

.next_char:
	mov al, [esi]			; Copy character
	mov [edi], al
	inc esi
	inc edi
	test al, al			; End of source string?
	jz .exit
	dec ecx				; End of buffer?
	jnz .next_char

.exit:
	pop edi
	pop esi
	pop edx
	pop ecx
	pop eax
	ret


;------------------------------------------------------------------------------
; Find the first occurence of a character in a string.
;------------------------------------------------------------------------------
; -> AH - Character to search for
;    ECX - Maximum number of characters to search
;    ESI - ASCIIZ string to find character within
; <- CF - Set if character was not found
;    EAX - Index of character's first occurence if CF is not set
;------------------------------------------------------------------------------

	align 4

global str_char_pos
str_char_pos:
	push ebx
	push ecx
	push esi
	push eax

	test ecx, ecx
	jz .not_found			; ECX is zero, nothing to search

	mov ebx, esi			; EBX: start of string

.find_loop:
	mov al, [esi]			; Next character
	cmp al, ah			; Match?
	je .found
	test al, al
	jz .not_found
	inc esi
	dec ecx				; Search limit reached?
	jnz .find_loop

.not_found:
	pop eax
	stc
	jmp .exit

.found:
	sub esi, ebx
	add esp, 4			; Discard EAX from stack
	mov eax, esi			; Return index
	clc

.exit:
	pop esi
	pop ecx
	pop ebx

.return:
	ret


;------------------------------------------------------------------------------
; Find the last occurence of a character in a string.
;------------------------------------------------------------------------------
; -> AH - Character to search for
;    ECX - Maximum number of characters to search
;    ESI - ASCIIZ string to find character within
; <- CF - Set if character was not found
;    EAX - Index of character's last occurence if CF is not set
;------------------------------------------------------------------------------

	align 4

global str_char_rpos
str_char_rpos:
	push ebx
	push ecx
	push edx
	push esi
	push eax

	test ecx, ecx
	jz .not_found			; ECX is zero, nothing to search

	mov edx, -1			; EDX: length of string

.find_len_loop:
	inc edx
	cmp byte [esi + edx], 0
	jne .find_len_loop

	mov ebx, esi			; EBX: start of string
	lea esi, [esi + edx - 1]	; ESI: last character in string
	cmp ecx, edx			; Limit maximum search count to length
	jbe .find_loop
	mov ecx, edx
	test ecx, ecx
	jz .not_found

.find_loop:
	mov al, [esi]			; Next character
	cmp al, ah			; Found character?
	je .found
	dec esi
	dec ecx				; Search limit reached?
	jnz .find_loop

.not_found:
	pop eax
	stc
	jmp .exit

.found:
	add esp, 4			; Discard EAX from stack
	mov eax, esi
	sub eax, ebx			; EAX: index of character
	clc

.exit:
	pop esi
	pop edx
	pop ecx
	pop ebx
	ret


;------------------------------------------------------------------------------
; Compares two ASCIIZ strings for equivalence.
;------------------------------------------------------------------------------
; -> ECX - Maximum number of characters to compare
;    ESI - Pointer to source ASCIIZ string
;    EDI - Pointer to target ASCIIZ string
; <- Flags: Set as if two unsigned integers were compared. Use unsigned branch
;           instructions such as jb, ja, je or jne.
;------------------------------------------------------------------------------

	align 4

global str_cmp
str_cmp:
	push eax
	push ecx
	push esi
	push edi

.compare_loop:
	mov al, [esi]
	cmp al, [edi]
	jne .exit
	test al, al
	jz .same
	inc esi
	inc edi
	dec ecx
	jnz .compare_loop

.same:
	cmp al, al

.exit:
	pop edi
	pop esi
	pop ecx
	pop eax
	ret


;------------------------------------------------------------------------------
; Convert a 32-bit number into decimal string.
;------------------------------------------------------------------------------
; -> EAX - Number to convert
;    BL - Mode, STR_CV_SIGNED or STR_CV_UNSIGNED
;    ESI - Buffer receiving ASCIIZ result string (min. 12 bytes)
;------------------------------------------------------------------------------

	align 4

global str_int
str_int:
	push eax
	push ecx
	push edx
	push esi

	cmp bl, STR_CV_UNSIGNED
	je .calculate_length

	test eax, 0x80000000		; Unsigned: start conversion
	jz .calculate_length
	mov byte [esi], '-'		; Add minus char to buffer
	inc esi
	neg eax				; Negate value

.calculate_length:

	; Calculate length of string after conversion

	xor ecx, ecx

.loop_calculate_length:
	mov edx, [intlentab + ecx * 4]
	test edx, edx
	jz .convert
	cmp eax, edx
	jbe .convert
	inc ecx
	jmp .loop_calculate_length

.convert:

	; Convert digits

	lea esi, [esi + ecx + 1]	; ESI: pointer to end of string + 1
	mov byte [esi], 0		; End of string
	mov ecx, 10

.next_digit:
	dec esi				; Next character
	xor edx, edx			; Prepare for division
	div ecx				; Unsigned divide by 10
	add dl, '0'			; Remainder to string
	mov [esi], dl			; Store in output buffer
	test eax, eax			; Division result <> 0: next digit
	jnz .next_digit

	pop esi
	pop edx
	pop ecx
	pop eax
	ret


;------------------------------------------------------------------------------
; Convert a 32-bit fixed-point number into a decimal string.
;------------------------------------------------------------------------------
; -> EAX - Number to convert
;    BL - Mode, 0 = unsigned, 1 = signed
;    BH - Number of decimals in the converted string (rounding applies)
;    ECX - Fixed point base (value representing 1)
;    ESI - Buffer receiving ASCIIZ result string
;------------------------------------------------------------------------------

	align 4

global str_fixed
str_fixed:
	push eax
	push ebx
	push ecx
	push edx
	push esi
	push edi
	push ebp

	;----------------------------------------------------------------------
	; Convert integer part

	cmp bl, STR_CV_UNSIGNED
	je .check_round

	test eax, 0x80000000		; Unsigned: start conversion
	jz .check_round
	mov byte [esi], '-'		; Add minus char to buffer
	inc esi
	neg eax				; Negate value

.check_round:
	xor edx, edx
	div ecx				; EAX: integer part
	mov ebp, edx			; EBP: fraction part
	test bh, bh
	jnz .calculate_length
	mov edx, ecx			; Round integer when no fraction part
	shr edx, 1			; EDX: base / 2
	cmp ebp, edx
	setae dl
	movzx edx, dl
	add eax, edx

.calculate_length:

	; Calculate length of integer part of string after conversion

	xor edi, edi

.loop_calculate_length:
	mov edx, [intlentab + edi * 4]
	test edx, edx
	jz .convert_int
	cmp eax, edx
	jbe .convert_int
	inc edi
	jmp .loop_calculate_length

.convert_int:

	; Convert integer digits

	add esi, edi			; ESI: last char of integer part
	push esi

	mov edi, 10

.next_int_digit:
	xor edx, edx			; Prepare for division
	div edi				; Unsigned divide by 10
	add dl, '0'			; Remainder to string
	mov [esi], dl			; Store in output buffer
	dec esi
	test eax, eax			; Division result <> 0: next digit
	jnz .next_int_digit
	pop esi				; ESI: last integer character

	;----------------------------------------------------------------------
	; Convert fraction part

	test ebp, ebp			; No fraction part, done
	jz .done

	; Calculate length of fraction part string after conversion

	test bh, bh
	jz .done
	cmp bh, 9			; BH: maximum number of decimal chars
	jbe .get_frac_len		; Limit to 9
	mov bh, 9

.get_frac_len:
	movzx edi, bh
	mov edx, [intlentab + edi * 4 - 4]
	inc edx				; EDX: 10 ^ number of decimal digits
	mov eax, ebp			; EAX: fraction part
	mul edx				; EDX:EAX: fraction * 10 ^ nb dec digits

	div ecx				; EAX: floor(10-base fraction)
	shr ecx, 1
	cmp edx, ecx			; Rounding
	setae dl
	movzx edx, dl
	add eax, edx			; EAX: round(10-base fraction)
	test eax, eax			; No 10-base fraction, done
	jz .done
	mov byte [esi + 1], '.'
	inc esi				; Skip decimal point
	add esi, edi			; ESI: last char of fraction part
	mov edi, esi			; EDI: last char of fraction part

	; Convert fraction digits

	mov ecx, 10
	xor ebp, ebp			; EBP: strip zero flag

.next_frac_digit:
	xor edx, edx			; Prepare for division
	div ecx				; Unsigned divide by 10
	test ebp, ebp
	jnz .save_frac_digit
	test dl, dl
	jz .skip_frac_digit
	inc ebp
	mov edi, esi			; DS:EDI: actual last char of string

.save_frac_digit:
	add dl, '0'			; Remainder to string
	mov [esi], dl			; Store in output buffer

.skip_frac_digit:
	dec esi
	dec bh				; Number of fraction characters left
	test eax, eax			; Division result <> 0: next digit
	jnz .next_frac_digit
	test bh, bh
	jz .frac_done

	; Fill fraction digits with 0 from left since that is missing from
	; the 10-base converted fraction value

.fill_frac_zero:
	mov byte [esi], '0'
	dec esi
	dec bh
	jnz .fill_frac_zero

.frac_done:
	mov esi, edi			; ESI: last char of string

.done:
	mov byte [esi + 1], 0		; End of string

	pop ebp
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	pop eax
	ret


;------------------------------------------------------------------------------
; Convert a 32-bit number into a hexadecimal string.
;------------------------------------------------------------------------------
; -> EAX - Number to convert
;    BL - STR_CV_LOWER = lowercase, STR_CV_UPPER = uppercase
;    BH - Number of nibbles to convert (1 - 8 digits from lowest nibble)
;    ESI - Buffer receiving ASCIIZ result string (min. BH + 1 bytes)
;------------------------------------------------------------------------------

	align 4

global str_hex
str_hex:
	test bh, bh
	jnz .start
	mov byte [esi], 0		; 0 nibbles: empty string
	ret

.start:
	push eax
	push ebx
	push ecx
	push esi

	cmp bh, 8			; Limit number of nibbles to 8
	jbe .setup
	mov bh, 8

.setup:
	xor ecx, ecx
	mov cl, bh			; ECX: number of loops
	add esi, ecx			; Adjust SI to end of buffer
	mov byte [esi], 0		; End of string
	test cl, cl
	jz .done

	mov bh, '9' - 'a' + 1		; Lowercase a - f
	cmp bl, STR_CV_LOWER
	je .next_nibble
	mov bh, '9' - 'A' + 1		; Uppercase A - F

.next_nibble:
	dec esi				; Move buffer pointer forward
	mov bl, al
	and bl, 0x0f			; BL: lowest nibble
	cmp bl, 9
	jbe .digit
	sub bl, bh			; Adjust for correct A-F ASCII code

.digit:
	add bl, '0'			; Convert to ASCII
	mov [esi], bl
	shr eax, 4
	dec ecx
	jnz .next_nibble

.done:
	pop esi
	pop ecx
	pop ebx
	pop eax
	ret


;------------------------------------------------------------------------------
; Compose a string from a source by replacing placeholder variables with actual
; data into a target string. This isn't very fast, but convenient when speed is
; not of great concern. Use individual data converters when speed is important.
;------------------------------------------------------------------------------
; -> ECX - Maximum character capacity of the result buffer
;    ESI - Source string with optional placeholder variables
;    EDI - Buffer receiving ASCIIZ result string (must be large enough)
;    EBP - Pointer to just above first variable value on stack
;------------------------------------------------------------------------------
; Source string may contain placeholder "variables" enclosed in curly brackets.
; The following placeholders are supported:
; - {u[8|16|32]}: Unsigned integer of 8/16/32 bits (default: 32 bits)
; - {i[8|16|32]}: Signed integer of 8/16/32 bits (default: 32 bits)
; - {x[8|16|32]}: Lowercase hexadecimal value of 8/16/32 bits (default: 32 bits)
; - {X[8|16|32]}: Uppercase hexadecimal value of 8/16/32 bits (default: 32 bits)
; - {q[8|16|32]:base[.precision]}: Unsigned fixed-point value of 8/16/32 bits
;   (default: 32 bits) having "base" (value corresponding to 1). Fraction
;   decimals are displayed up to "precision" digits (default: max. precision).
; - {w[8|16|32]:base[.precision]}: Same as above, but signed.
; - {c}: A single character.
; - {s[:length]}: A string up to "length" characters (default: entire string).
;   Placeholder value on stack is linear address of string.
; - {>}: Skip placeholder value without displaying anything.
;------------------------------------------------------------------------------
; For each of the placeholders, a value must be provided on the stack in the
; order of placeholders from top to bottom. The typical usage is:
;------------------------------------------------------------------------------
;	mov ebp, esp			; EBP: current stack top
;	push dword {value1}		; Push first placeholder value
;	push dword {value2}		; Push second placeholder value
;	mov esi, fmt_string		; ESI: string to format
;	mov edi, buffer			; EDI: target buffer for formatted str
;	call str_format			; Compose formatted string
;	mov esp, ebp			; Discard placeholder values
;------------------------------------------------------------------------------
; Each placeholder value should use 32 bits on the stack. However for smaller
; values, such as {c} or {i16} for example, only the low byte/word of the value
; is used and the rest is discarded. So if you want to format AL as a signed
; integer, you can safely push eax for {i8}; the upper 24 bits won't be used
; to print the integer value. For the high byte of registers, you need to
; either move it to another register, or if there is no spare register, you can
; do an xchg ah, al / push eax / xchg ah, al at the expense of some extra CPU
; cycles.
;------------------------------------------------------------------------------

	align 4

global str_format
str_format:
	%define .loc(i) esp + (i * 4)

	push eax
	push ebx
	push ecx
	push edx
	push esi
	push edi
	push ebp

	push edi			; [esp - 4]: current end of target ptr
	push ecx			; [esp]: maximum length of target

.printf_loop:
	mov al, [esi]
	inc esi
	cmp al, '{'
	je .variable
	cmp dword [.loc(0)], 1
	jae .store
	xor al, al			; End of buffer

.store:
	mov [edi], al
	inc edi
	inc dword [.loc(1)]		; Adjust current end of string pointer
	dec dword [.loc(0)]		; Decrease available characters
	test al, al
	jnz .printf_loop

.exit:
	add esp, 8			; Discard local variables

	pop ebp
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	pop eax
	ret

.done:
	mov byte [edi], 0		; Terminate string
	jmp .exit

.variable:

	; Variable

	mov al, [esi]
	inc esi
	cmp al, '{'			; {{: print opening curly bracket
	je .store
	sub ebp, 4			; Go to start of next variable on stack

	; Check variable type

	mov cl, STR_CV_UNSIGNED
	cmp al, 'u'			; Unsigned integer
	je .int
	mov cl, STR_CV_SIGNED
	cmp al, 'i'			; Signed integer
	je .int
	mov cl, STR_CV_LOWER
	cmp al, 'x'			; Lowercase hexadecimal
	je .hex
	mov cl, STR_CV_UPPER
	cmp al, 'X'			; Uppercase hexadecimal
	je .hex
	mov dl, STR_CV_UNSIGNED
	cmp al, 'q'			; Unsigned fixed-point
	je .fixed
	mov dl, STR_CV_SIGNED
	cmp al, 'w'			; Signed fixed-point
	je .fixed
	cmp al, 'c'			; Character
	je .char
	cmp al, 's'			; String
	je .string
	cmp al, '>'			; Next placeholder value
	je .skip_variable
	jmp .skip_variable

.int:

	;----------------------------------------------------------------------
	; {u|i[8|16|32]}: unsigned or signed integer of bit length

	mov ebx, 0xff * 256 + '}'	; Get integer bit length
	call str_parse_int
	jc .skip_variable
	test eax, eax			; {u|i}: 32 bit
	je .int_dword
	cmp eax, 32			; {u32|i32}: 32 bit
	je .int_dword
	test cl, cl
	jz .uint
	cmp eax, 16			; {u16}
	je .int_word
	cmp eax, 8			; {u8}
	je .int_byte
	jmp .skip_variable		; Unknown, skip variable

.uint:
	cmp eax, 16			; {i16}
	je .uint_word
	cmp eax, 8			; {i8}
	je .uint_byte
	jmp .skip_variable

.int_dword:
	mov eax, [ebp]			; {u|i[32]}: get as 32 bit from stack
	jmp .str_int

.int_word:
	movsx eax, word [ebp]		; {i16}: sign-extend into eax
	jmp .str_int

.int_byte:
	movsx eax, byte [ebp]		; {i8}: sign-extend into eax
	jmp .str_int

.uint_word:
	xor eax, eax
	mov ax, [ebp]			; {u16}: zero-extend into eax
	jmp .str_int

.uint_byte:
	xor eax, eax
	mov al, [ebp]			; {u8}: zero-extend into eax

.str_int:
	mov edx, esi			; Save string pointer
	mov bl, cl			; Convert number to string
	mov esi, edi
	cmp dword [.loc(0)], 12		; Need 12 bytes max. for conversion
	jb .end_of_buffer
	call str_int
	mov esi, edx			; Restore string pointer
	call .skip_dest_to_end		; Skip to end of result string
	jmp .skip_variable

.fixed:

	;----------------------------------------------------------------------
	; {q|w[8|16|32]:base[.precision]}: unsigned or signed fixed-point
	; decimal of bit length

	mov bx, 0xff * 256 + ':'	; Get integer bit length
	call str_parse_int
	jc .skip_variable
	test eax, eax			; {q|w}: 32 bit
	je .fixed_dword
	cmp eax, 32			; {q32|w32}: 32 bit
	je .fixed_dword
	test cl, cl
	jz .ufixed
	cmp eax, 16			; {q16}
	je .fixed_word
	cmp eax, 8			; {q8}
	je .fixed_byte
	jmp .skip_variable		; Unknown, skip variable

.ufixed:
	cmp eax, 16			; {w16}
	je .ufixed_word
	cmp eax, 8			; {w8}
	je .ufixed_byte
	jmp .skip_variable

.fixed_dword:
	mov eax, [ebp]			; {q|w[32]}: get as 32 bit from stack
	jmp .fixed_precision

.fixed_word:
	movsx eax, word [ebp]		; {w16}: sign-extend into eax
	jmp .fixed_precision

.fixed_byte:
	movsx eax, byte [ebp]		; {w8}: sign-extend into eax
	jmp .fixed_precision

.ufixed_word:
	xor eax, eax
	mov ax, [ebp]			; {q16}: zero-extend into eax
	jmp .fixed_precision

.ufixed_byte:
	xor eax, eax
	mov al, [ebp]			; {q8}: zero-extend into eax

.fixed_precision:
	mov ebx, eax

.skip_bit_length:
	mov al, [esi]			; Skip bit length specifier
	inc esi
	test al, al
	jz .done
	cmp al, ':'
	jne .skip_bit_length

	push ebx			; Save variable value

	mov ah, '.'			; Check presence of precision
	mov ecx, 0xff
	call str_char_pos
	mov dh, 0xff			; DH: fixed-point precision
	mov bh, 0xff			; BH: max length of base
	jc .fixed_base			; No precision specified, use maximum

	mov bh, al			; BH: max length of base
	mov ecx, esi			; Save string pointer
	add esi, eax
	inc esi
	push ebx
	mov ebx, 0xff * 256 + '}'	; Get precision
	call str_parse_int
	pop ebx
	mov esi, ecx			; Restore string pointer
	jc .fixed_base
	cmp eax, 0xff
	ja .fixed_base
	mov dh, al			; DH: requested precision

.fixed_base:
	mov bl, '}'			; Get fixed-point base
	call str_parse_int
	pop ebx				; Restore variable value into EBX
	jc .skip_variable
	test eax, eax			; No base, invalid, skip
	jz .skip_variable

	cmp dword [.loc(0)], 13		; Check if buffer has enough space
	jb .end_of_buffer
	movzx ecx, bh			; Limit precision to available space
	add ecx, 13
	cmp [.loc(0)], ecx
	jae .str_fixed
	mov ecx, [.loc(0)]
	sub ecx, 13
	mov dh, cl

.str_fixed:
	mov ecx, eax			; ECX: base
	mov eax, ebx			; EAX: variable value
	mov ebx, edx			; Convert number to string
	mov edx, esi			; Save string pointer
	mov esi, edi
	call str_fixed
	mov esi, edx			; Restore string pointer
	call .skip_dest_to_end		; Skip to end of result string
	jmp .skip_variable

.hex:

	;----------------------------------------------------------------------
	; {x|X[8|16|32]}: lowercase/uppercase hexadecimal of bit length

	mov ebx, 0xff * 256 + '}'	; Get hexadecimal bit length
	call str_parse_int
	mov bl, cl			; BL: lowercase/uppercase
	mov bh, 8			; BH: number of nibbles
	test eax, eax			; {x|X}: 32 bit
	je .str_hex
	cmp eax, 32			; {x32|X32}: 32 bit
	je .str_hex
	sub bh, 4
	cmp eax, 16			; {x16|X16}: 16 bit
	je .str_hex
	sub bh, 2
	cmp eax, 8			; {x8|X8}: 8 bit
	jne .skip_variable		; Unknown, skip variable

.str_hex:
	movzx eax, bh			; Check if buffer has enough space
	cmp [.loc(0)], eax
	jb .end_of_buffer
	mov eax, [ebp]
	mov edx, esi			; Save string pointer
	mov esi, edi
	call str_hex
	mov esi, edx			; Restore string pointer
	call .skip_dest_to_end		; Skip to end of result string
	jmp .skip_variable

.char:

	;----------------------------------------------------------------------
	; {c} - Single character

	cmp dword [.loc(0)], 1
	jb .end_of_buffer
	mov al, [ebp]
	mov [edi], al
	inc edi
	inc dword [.loc(1)]		; Adjust current end of string pointer
	dec dword [.loc(0)]		; Decrease available characters
	jmp .skip_variable

.string:

	;----------------------------------------------------------------------
	; {s[:length]} - String, variable value is pointer to string

	mov ecx, -1			; ECX: maximum number of chars to copy
	cmp byte [esi], ':'
	jne .limit_string_length
	inc esi
	mov ebx, 0xff * 256 + '}'	; Get maximum number of chars to copy
	call str_parse_int
	jc .skip_variable
	cmp eax, 0
	jl .skip_variable
	mov ecx, eax

.limit_string_length:
	cmp [.loc(0)], ecx		; Limit length to buffer space
	jae .copy_string
	mov ecx, [.loc(0)]

.copy_string:
	mov ebx, [ebp]

.copy_string_loop:
	mov al, [ebx]
	test al, al
	jz .copy_string_done		; End of string
	mov [edi], al			; Copy to target string
	inc ebx
	inc edi
	dec ecx
	jnz .copy_string_loop

.copy_string_done:
	call .update_remaining

.skip_variable:

	; Skip after end of variable (closing curly bracket)

	mov al, [esi]
	inc esi
	test al, al
	jz .done
	cmp al, '}'
	jne .skip_variable
	jmp .printf_loop

.end_of_buffer:
	xor al, al
	jmp .store


; Skip result string pointer to its end.
; -> EDI - Result ASCIIZ string pointer
; <- AL - Destroyed
;    EDI - Pointer to terminator NUL of result string

	align 4

.skip_dest_to_end:
	mov al, [edi]
	inc edi
	test al, al
	jnz .skip_dest_to_end
	dec edi

.update_remaining:

	; .loc indexes are 1 higher, because .loc(0) points to return address

	sub edi, [.loc(2)]		; EDI: length of string added
	sub [.loc(1)], edi		; Decrease character capacity
	add edi, [.loc(2)]		; EDI: end of target string
	mov [.loc(2)], edi		; Update current end of target string
	ret


;------------------------------------------------------------------------------
; Convert an ASCIIZ string containing a 32-bit signed decimal number into an
; actual number.
;------------------------------------------------------------------------------
; -> BL - Terminator character (set to 0 to terminate at the end of string only)
;    BH - Maximum number of characters to convert
;    ESI - Buffer containing decimal numeric string
; <- CF - Set if error (invalid characters found)
;    EAX - Converted number
;------------------------------------------------------------------------------

	align 4

global str_parse_int
str_parse_int:
	push ebx
	push ecx
	push edx
	push esi
	push eax

	xor ecx, ecx
	mov cl, bh
	xor edx, edx			; EDX: result number
	xor bh, bh			; BH: sign, 0 = positive, 1 = negative

	test ecx, ecx			; Count = 0, nothing to convert
	jz .done
	xor eax, eax			; Clear high bytes

	; Check sign (if any)

	mov al, [esi]			; Get first character
	inc esi
	cmp al, '+'
	je .convert_loop		; +: ignore
	cmp al, '-'
	jne .check_digit		; Neither +, nor -, treat as digit
	inc bh				; -: set sign flag

.convert_loop:
	mov al, [esi]			; Get next character
	inc esi

.check_digit:
	test al, al			; End of string
	jz .done
	cmp al, bl			; Terminator character reached?
	je .done
	cmp al, '0'			; Check for non-numeric characters
	jb .error
	cmp al, '9'
	ja .error

	; Convert digit

	cmp edx, 214748364
	ja .error			; Will overflow
	lea edx, [edx * 4 + edx]
	add edx, edx
	sub al, '0'			; Convert to number
	add edx, eax			; Result * 10 + current character
	cmp edx, 0x80000000		; Overflow
	jae .error
	dec ecx
	jnz .convert_loop

.done:
	add esp, 4			; Discard EAX from stack
	mov eax, edx
	test bh, bh
	jz .positive
	neg eax

.positive:
	clc

.exit:
	pop esi
	pop edx
	pop ecx
	pop ebx
	ret

.error:
	stc
	pop eax
	jmp .exit


;------------------------------------------------------------------------------
; Convert an ASCIIZ string containing a 32-bit signed floating point decimal
; number into an actual fixed point number.
;------------------------------------------------------------------------------
; -> BL - Terminator character (set to 0 to terminate at the end of string only)
;    BH - Maximum number of characters to convert
;    ECX - Fixed point base (value representing 1)
;    ESI - Buffer containing decimal numeric string
; <- CF - Set if error (invalid characters found)
;    EAX - Converted number
;------------------------------------------------------------------------------

	align 4

global str_parse_fixed
str_parse_fixed:
	push ebx
	push edx
	push esi
	push edi
	push ebp
	push eax
	push ecx

	test ecx, ecx
	jz .error			; Base can't be 0

	;----------------------------------------------------------------------
	; Setup values

	; Calculate overflow limits

	mov eax, 0x80000000
	xor edx, edx
	div ecx
	mov ebp, eax			; EBP: integer overflow limit
	test edx, edx
	setnz dl			; DL: 1 when remainder > 0, else 0
	movzx edx, dl
	add ebp, edx			; EBP: ceil(0x80000000 / ECX)

	; Initialize registers

	xor ecx, ecx
	mov cl, bh			; CL: number of chars
					; CH: number of decimal digits
	xor edx, edx			; EDX: result integer part
	xor edi, edi			; EDI: result fraction part
	xor bh, bh			; BH: flags
					;     bit 0: negative (0x01)
					;     bit 1: fractions (0x02)

	test cl, cl			; Count = 0, nothing to convert
	jz .done

	;----------------------------------------------------------------------
	; String conversion

	; Check sign (if any)

	mov al, [esi]			; Get first character
	inc esi
	cmp al, '+'
	je .convert_loop		; +: ignore
	cmp al, '-'
	jne .check_digit		; Neither +, nor -, treat as digit
	inc bh				; -: set sign flag

.convert_loop:
	mov al, [esi]			; Get next character
	inc esi

.check_digit:
	test al, al			; End of string
	jz .done
	cmp al, bl			; Terminator character reached?
	je .done
	cmp al, '.'			; Decimal point
	je .decimal
	cmp al, '0'			; Check for non-numeric characters
	jb .error
	cmp al, '9'
	ja .error

	; Convert digit

	movzx eax, al			; Zero-extend
	sub al, '0'			; Convert to number
	test bh, 0x02
	jz .convert_integer

	; Add to fraction part

	cmp edi, 214748364
	ja .frac_will_overflow		; Fraction will overflow
	je .frac_may_overflow		; Fraction may overflow
	cmp ch, 9
	je .frac_will_overflow		; Fraction will overflow

.frac_no_overflow:
	lea edi, [edi * 4 + edi]
	add edi, edi
	add edi, eax			; Fraction * 10 + current character
	inc ch
	jmp .next_digit

.convert_integer:

	; Add to integer part

	cmp edx, 214748364
	ja .error			; Will overflow, error
	lea edx, [edx * 4 + edx]
	add edx, edx
	add edx, eax			; Result * 10 + current character
	cmp edx, ebp
	jae .error			; Integer overflow, error

.next_digit:
	dec cl
	jnz .convert_loop

.done:

	;----------------------------------------------------------------------
	; Convert to fixed point base, combine integer and fraction parts of
	; the parsed number

	; Calculate fraction 10-base divider (10 ^ (number of decimals))

	mov ebp, 1			; EBP: 10 ^ (number of decimals)
	test ch, ch
	jz .apply_base

.frac_tenbase_loop:
	lea ebp, [ebp * 4 + ebp]
	add ebp, ebp			; EBP *= 10
	dec ch
	jnz .frac_tenbase_loop

.apply_base:

	; Apply fixed point base on integer part

	mov eax, edx
	pop ecx				; ECX: fixed point base
	mul ecx				; EAX: integer * base
	mov esi, eax			; ESI: fixed point value integer part
	test bh, 0x02
	jz .integer

	; Apply fixed point base on fraction part

	mov eax, edi
	mul ecx
	div ebp
	shr ebp, 1			; Rounding
	cmp edx, ebp
	setae dl
	movzx edi, dl
	add edi, eax			; EDI: fixed point value fraction part
	add esi, edi

.integer:

	; Final overflow test, return parsed number if no overflow

	cmp esi, 0x80000000
	jae .error_final		; 32-bit overflow
	add esp, 4			; Discard EAX from stack
	mov eax, esi
	test bh, 0x01
	jz .positive
	neg eax				; Negate value when negative

.positive:
	clc

.exit:
	pop ebp
	pop edi
	pop esi
	pop edx
	pop ebx
	ret

	;----------------------------------------------------------------------
	; Fraction part handling

.decimal:

	; Handle decimal point character in string. Set decimal flag to treat
	; the rest of the string as fraction part.

	test bh, 0x02
	jnz .error			; More, than one decimal point
	or bh, 0x02			; Set fractions flag
	dec cl
	jnz .convert_loop
	jmp .done

.frac_will_overflow:

	; Fraction part will overflow with current digit. Use current digit to
	; round fraction value and stop further parsing.

	cmp al, 5
	setae al
	movzx eax, al
	add edi, eax
	jmp .done

.frac_may_overflow:

	; Fraction part may overflow if current digit is > 7. Apply digit if
	; <= 7 or round up fraction part and stop further parsing.

	cmp al, 7
	jbe .frac_no_overflow
	inc edi
	jmp .done

.error:
	pop ecx

.error_final:
	stc
	pop eax
	jmp .exit


;------------------------------------------------------------------------------
; Convert an ASCIIZ string containing a 32-bit hexadecimal number into an
; actual number.
;------------------------------------------------------------------------------
; -> BL - Terminator character (set to 0 to terminate at the end of string only)
;    BH - Maximum number of characters to convert
;    ESI - Buffer containing hexadecimal numeric string
; <- CF - Set if error (invalid characters found)
;    EAX - Converted number
;------------------------------------------------------------------------------

	align 4

global str_parse_hex
str_parse_hex:
	push ebx
	push ecx
	push edx
	push esi
	push eax

	xor ecx, ecx
	mov cl, bh
	xor edx, edx			; EDX: result number
	xor bh, bh			; BH: sign, 0 = positive, 1 = negative

	test ecx, ecx			; Count = 0, nothing to convert
	jz .done

.convert_loop:
	mov al, [esi]			; Get next character
	inc esi
	test al, al			; End of string
	jz .done
	cmp al, bl			; Terminator character reached?
	je .done
	cmp al, '0'			; Check for non-numeric characters
	jb .error
	cmp al, '9'
	jbe .number
	cmp al, 'A'
	jb .error
	cmp al, 'F'
	jbe .hex
	cmp al, 'a'
	jb .error
	cmp al, 'f'
	ja .error
	sub al, 'a' - 10		; a - z: lowercase hexadecimal digit
	jmp .add_digit

.hex:
	sub al, 'A' - 10		; A - Z: uppercase hexadecimal digit
	jmp .add_digit

.number:
	sub al, '0'			; 0 - 9: number digit

.add_digit:

	; Convert digit

	test edx, 0xf0000000
	jnz .error			; Overflow
	shl edx, 4
	movzx eax, al			; Zero-extend
	add edx, eax			; Result * 10 + current character
	dec cx
	jnz .convert_loop

.done:
	add esp, 4			; Discard EAX from stack
	mov eax, edx
	clc

.exit:
	pop esi
	pop edx
	pop ecx
	pop ebx
	ret

.error:
	stc
	pop eax
	jmp .exit


section .data

;==============================================================================
; Data area
;==============================================================================

intlentab	dd 9, 99, 999, 9999, 99999, 999999,
		dd 9999999, 99999999, 999999999, 0
