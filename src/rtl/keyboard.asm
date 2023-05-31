;==============================================================================
; PMI runtime library - Keyboard handling
;==============================================================================

	cpu 386

section .text

%include "pmi/api/pmi.inc"
%include "rtl/api/irq.inc"
%include "rtl/api/systimer.inc"
%include "rtl/consts/keyboard.inc"

; Keyboard controller constants

KB_RESEND	EQU 0xfe		; Resend status code
KB_ACK		EQU 0xfa		; Acknowledged status code
KB_OVERRUN	EQU 0xff		; Keyboard overrun scan code

; Keyboard interface ports, registers and associated constants

PORT_A		EQU 0x60		; Keyboard scan code / control port
STATUS_PORT	EQU 0x64		; Keyboard controller status port
OUTPUT_BUF_FULL	EQU 0x01		; Output buffer full
INPUT_BUF_FULL	EQU 0x02		; Input buffer full

; BIOS keyboard data addresses and associated constants

KB_FLAG		EQU 0x417		; BIOS data: keyboard flags 1
KB_FLAG_1	EQU 0x418		; BIOS data: keyboard flags 2
KB_FLAG_2	EQU 0x497		; BIOS data: keyboard flags 3
KB_LEDS 	EQU 00000111b		; LED state bits
KB_FLAG_3	EQU 0x496		; BIOS data: keyboard flags 4
LC_E1		EQU 00000001b		; Within E1 scan code sequence
LC_E0		EQU 00000010b		; Within E0 scan code sequence
ALT_INPUT	EQU 0x419		; BIOS data: ALT-keypad input buffer

; Runtime library constants

BUFFER_SIZE	EQU 61			; Size of keyboard event buffer
SEND_LED_REQ	EQU 0x80		; Set LEDs command requested
SEND_LED_CMD	EQU 0x40		; Set LEDs command running
SEND_LED_DATA	EQU 0x20		; Set LEDs data running
TOGGLER_SHR	EQU 4			; Toggler shift right for switch flags


;------------------------------------------------------------------------------
; Install a keyboard IRQ handler that runs entirely in protected mode, to
; prevent mode switches to real mode when handling default actions.
;------------------------------------------------------------------------------
; <- CF - Set if error
;    EAX - Error code if CF set
;------------------------------------------------------------------------------

global kbd_start
kbd_start:
	push ecx
	push edx
	push edi
	push eax

	mov [data_sel], ds		; Save flat data selector for IRQ

	call systimer_start		; Start systimer
	jc .error

	; Initialize data area

	xor eax, eax			; Set all down flags to off
	mov ecx, 256 / 8 / 4
	mov edi, key_down
	rep stosd

	call init_modifiers		; Initialize modifiers from BIOS state
	mov byte [ALT_INPUT], 0		; Cancel pending BIOS Alt-Keypad input

	mov al, 1			; Save current handler
	call pmi(get_irq_hndlr)
	jc .error
	mov [old_irq1_hndlr], edx
	mov [old_irq1_hndlr + 4], cx
	mov cx, cs			; Set new handler
	mov edx, kbd_irq_handler
	call pmi(set_irq_hndlr)
	jc .error

	mov byte [irq1_installed], 1

	pop eax
	clc

.exit:
	pop edi
	pop edx
	pop ecx
	ret

.error:
	add esp, 4			; Discard EAX from stack
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Restore original keyboard IRQ handler (real mode redirector).
;------------------------------------------------------------------------------
; <- CF - Set if error
;    EAX - Error code if CF set
;------------------------------------------------------------------------------

global kbd_stop
kbd_stop:
	cmp byte [irq1_installed], 0
	je .noop

	push ecx
	push edx
	push eax

	call systimer_stop		; Stop systimer

	mov al, 1			; Restore original handler
	mov edx, [old_irq1_hndlr]
	mov cx, [old_irq1_hndlr + 4]
	call pmi(set_irq_hndlr)
	jc .error

	mov byte [irq1_installed], 0
	pop eax
	clc

.exit:
	pop edx
	pop ecx

.noop:
	ret

.error:
	add esp, 4			; Discard EAX from stack
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Set a specific keyboard layout.
;------------------------------------------------------------------------------
; -> EAX - Linear address of keyboard ASCII translation table
;    EBX - Linear address of keypad ASCII translation table
;------------------------------------------------------------------------------
; Set EAX and/or EBX to 0 to use the default US American layout for keyboard
; and/or keypad ASCII translation.
;------------------------------------------------------------------------------

global kbd_set_layout
kbd_set_layout:
	push eax
	push ebx

	test eax, eax
	jnz .check_keypad
	mov eax, kbd_layout_us

.check_keypad:
	test ebx, ebx
	jnz .set_layout
	mov ebx, kbd_layout_us_num

.set_layout:
	mov [kbd_ascii], eax
	mov [kbd_ascii_num], ebx

	pop ebx
	pop eax
	ret


;------------------------------------------------------------------------------
; Check if a keyboard event is available.
;------------------------------------------------------------------------------
; <- ZF - Set if no event available, clear otherwise
;------------------------------------------------------------------------------

global kbd_has_event
kbd_has_event:
	push eax

	mov eax, [kb_buffer_head]
	cmp eax, [kb_buffer_tail]

	pop eax
	ret


;------------------------------------------------------------------------------
; Return oldest pending keyboard event if available.
;------------------------------------------------------------------------------
; <- ZF - Set if no event available, clear otherwise
;    AL - ASCII code when printable character
;    AH - Keycode (KC_*)
;    EBX - Modifiers at the time of the event
;    DL - Keyboard event (KBD_EVT_*)
;    DH - Virtual keycode for non-printable characters
;------------------------------------------------------------------------------

global kbd_get_event
kbd_get_event:
	push ecx

	mov ecx, [kb_buffer_head]
	cmp ecx, [kb_buffer_tail]
	je .exit			; No event, ZF is set

	call get_key
	add ecx, 4
	cmp ecx, kb_buffer + BUFFER_SIZE * 4
	jb .done			; ZF clear when branching
	sub ecx, BUFFER_SIZE * 4	; Clears ZF

.done:
	mov [kb_buffer_head], ecx	; ZF always clear at this point

.exit:
	pop ecx
	ret


;------------------------------------------------------------------------------
; Return current value of keyboard modifiers.
;------------------------------------------------------------------------------
; <- EBX - Current value of keyboard modifiers.
;------------------------------------------------------------------------------

global kbd_get_modifiers
kbd_get_modifiers:
	mov ebx, [modifiers]
	ret


;------------------------------------------------------------------------------
; Return next event from keyboard buffer.
;------------------------------------------------------------------------------
; -> ECX - Keyboard buffer head
; <- AL - ASCII code when printable character
;    AH - Keycode (KC_*)
;    EBX - Modifiers at the time of the event
;    DL - Keyboard event (KBD_EVT_*)
;    DH - Virtual keycode for non-printable characters
;------------------------------------------------------------------------------

get_key:
	push edx
	push eax

	xor ebx, ebx
	mov eax, [ecx]			; AH: keycode
	shld ebx, eax, 16		; EBX: modifiers

	; Convert keycode to ASCII when possible

	cmp ah, KC(7, 0)
	jae .no_ascii			; No ASCII conversion for rows above 6
	mov al, ah
	and al, 0x1f			; AL: key column
	shr ah, 5			; AH: key row
	cmp al, 24
	jae .keypad
	cmp al, 16
	jae .no_ascii

	; Keypress on main keyboard area

	sub ah, 1			; Keyboard ASCII table starts at row 1
	js .no_ascii
	shl ah, 4			; Conversion table row is 16 bytes
	add al, ah
	and eax, 0xff			; EAX: index into conversion table

	mov edx, [kbd_ascii]		; Get conversion table base address
	call get_ascii_conv_table	; EDX: base of ASCII conversion table
	jc .no_ascii
	mov al, [edx + eax]		; AL: ASCII code
	jmp .check_vk

.keypad:

	; Keypress on keypad

	sub ah, 2			; Keypad ASCII table starts at row 2
	js .no_ascii
	sub al, 24			; Adjust for keytab column offset
	shl ah, 2			; Conversion table row is 4 bytes
	add al, ah
	and eax, 0xff			; EAX: index into conversion table

	mov edx, [kbd_ascii_num]	; Get conversion table base address
	call get_ascii_conv_table	; EDX: base of ASCII conversion table
	jc .no_ascii
	mov al, [edx + eax]		; AL: ASCII code
	jmp .check_vk

.no_ascii:
	xor al, al			; Keypress somewhere else, no ASCII code

.check_vk:
	mov edx, [ecx]
	mov ah, dh			; AH: keycode
	mov [esp], ax			; Update AL and AH on stack for pop

	; Calculate virtual keycodes

	mov dh, KC_SHIFT_LEFT
	cmp ah, KC_SHIFT_RIGHT
	je .done			; Right shift = left shift
	mov dh, KC_CTRL_LEFT
	cmp ah, KC_CTRL_RIGHT
	je .done			; Right control = left control
	mov dh, KC_ALT_LEFT
	cmp ah, KC_ALT_RIGHT
	je .done			; Right alt = left alt
	mov dh, KC_GUI_LEFT
	cmp ah, KC_GUI_RIGHT
	je .done			; Right GUI = left GUI
	mov dh, ah
	and dh, 0x1f
	cmp dh, 24
	mov dh, ah			; DH: original keycode
	jb .done			; Not keypad key
	test ebx, KBD_NUM_ON
	jz .vk_keypad			; Num lock off, convert
	test ebx, KBD_SHIFT
	jz .done			; Num lock without shift, done

.vk_keypad:
	mov dh, KC_HOME
	cmp ah, KC_KP_7
	je .done			; Keypad 7 = home
	mov dh, KC_CURSOR_UP
	cmp ah, KC_KP_8
	je .done			; Keypad 8 = cursor up
	mov dh, KC_PAGE_UP
	cmp ah, KC_KP_9
	je .done			; Keypad 9 = page up
	mov dh, KC_CURSOR_LEFT
	cmp ah, KC_KP_4
	je .done			; Keypad 4 = cursor left
	mov dh, KC_CURSOR_RIGHT
	cmp ah, KC_KP_6
	je .done			; Keypad 6 = cursor right
	mov dh, KC_END
	cmp ah, KC_KP_1
	je .done			; Keypad 1 = end
	mov dh, KC_CURSOR_DOWN
	cmp ah, KC_KP_2
	je .done			; Keypad 2 = cursor down
	mov dh, KC_PAGE_DOWN
	cmp ah, KC_KP_3
	je .done			; Keypad 3 = page down
	mov dh, KC_INSERT
	cmp ah, KC_KP_0
	je .done			; Keypad 0 = insert
	mov dh, KC_DELETE
	cmp ah, KC_KP_DOT
	je .done			; Keypad dot = delete
	mov dh, AH			; DH: original keycode

.done:
	mov [esp + 4], dx		; Update DL and DH on stack for pop
	pop eax
	pop edx
	ret


;------------------------------------------------------------------------------
; Find ASCII conversion table for a modifier combination.
;------------------------------------------------------------------------------
; -> EBX - Modifiers
;    EDX - Pointer to conversion table header
; <- CF - Set if no appropriate conversion table found
;    EDX - Pointer to conversion table or 0 if CF set
;------------------------------------------------------------------------------

get_ascii_conv_table:
	push eax

.find_table_loop:
	cmp byte [edx], 0
	je .no_table
	add edx, 12
	mov eax, ebx
	and eax, [edx - 12]
	cmp eax, [edx - 8]
	jne .find_table_loop

	mov edx, [edx - 4]
	clc

.exit:
	pop eax
	ret

.no_table:
	xor edx, edx
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Protected mode keyboard IRQ handler.
;------------------------------------------------------------------------------

kbd_irq_handler:
	push eax
	push ecx
	push ds

	mov ds, cs:[data_sel]

	; Get data from keyboard

	in al, STATUS_PORT		; Make sure there is data
	test al, INPUT_BUF_FULL
	jnz .done_eoi			; Nope, ignore IRQ
	in al, PORT_A			; Read character

	; Handle special codes and multi-byte scan code sequences

	cmp al, KB_RESEND		; Resend?
	je .resend
	cmp al, KB_ACK			; ACK?
	je .ack
	cmp al, KB_OVERRUN		; Buffer overrun?
	je .done
	mov ah, [KB_FLAG_3]
	test ah, LC_E0			; Within E0 marker scan code sequence?
	jnz .within_e0
	test ah, LC_E1			; Within E1 marker scan code sequence?
	jnz .within_e1
	cmp al, 0xe0			; General marker code?
	je .e0
	cmp al, 0xe1			; Pause key?
	je .e1

	; Single-byte scan code
	; AL: scan code

	xor ecx, ecx
	mov cl, al
	and cl, 0x7f			; CL: scan code without release flag
	cmp cl, SCANCODES_COUNT		; Unknown scan code?
	jae .done
	mov ah, [scancodes + ecx]	; Get keycode for scancode
	test al, 0x80
	jnz .key_release

.key_down:

	; Key pressed down, add to keyboard buffer when possible
	; AL: scan code
	; AH: keycode

	xor ecx, ecx			; Set key down bit
	mov cl, ah
	bts [key_down], ecx
	setnc al			; AL: 1 if pressed, 0 if typematic rpt

 	call get_modifier		; ECX: modifier bitmask
	test ecx, ecx
	jz .add_keystroke		; Not a modifier key
	test ecx, KBD_INSERT
	jnz .update_modifier		; Enable typematic for Insert key
	test [modifiers], ecx
	jnz .done			; Discard when modifier already down

.update_modifier:
	or [modifiers], ecx		; Update modifier flags
	and ecx, KBD_TOGGLER_KEY	; Is it a toggler?
	jz .update_bios_flags
	shr ecx, TOGGLER_SHR		; Flip toggle switch flag
	xor [modifiers], ecx

.update_bios_flags:
	push eax
	call set_bios_modifiers		; Update BIOS keyboard flags
	pop eax
	call add_to_buffer		; Add keypress to buffer
	call sync_leds			; Synchronize LEDs
	jmp .done

.add_keystroke:
	call add_to_buffer		; Add keypress to buffer
	jmp .done

.within_e0:

	; Multi-byte scan code with E0 marker
	; AL: scan code

	mov ecx, SCANCODES_E0_COUNT
	mov ah, al
	and ah, 0x7f			; AH: scan code without release flag

.e0_loop:
	cmp ah, [scancodes_e0 + ecx * 2 - 2]
	je .e0_found			; Scan code found in lookup table?
	dec ecx
	jnz .e0_loop
	cmp al, 0xe0
	je .done_eoi			; Print screen sequence
	cmp al, 0x2a			; Ignore faux left-shift presses
	je .done_eoi
	jmp .done			; Unknown E0 sequence

.e0_found:
	mov ah, [scancodes_e0 + ecx * 2 - 1]
	test al, 0x80			; Got keycode, handle down/release
	jz .key_down

.key_release:

	; Key released
	; AL: scan code
	; AH: keycode

	xor ecx, ecx			; Clear key down bit
	mov cl, ah
	btr [key_down], ecx
	mov al, KBD_EVT_UP

	call get_modifier		; ECX: modifier bitmask
	test ecx, ecx
	jz .add_keyrelease		; Not a modifier key
	not ecx
	and [modifiers], ecx		; Update modifier flags
	push eax
	call set_bios_modifiers		; Update BIOS keyboard flags
	pop eax

.add_keyrelease:
	call add_to_buffer		; Add key release to buffer
	jmp .done

.resend:

	; Resend set LEDs command or data

	mov ah, [send_state]
	test ah, 0x04			; Retry count exceeded?
	jnz .clear_send			; Yes, stop trying
	mov al, 0xed
	test ah, SEND_LED_CMD
	jnz .resend_byte		; Resend set LEDs command
	test ah, SEND_LED_DATA		; Resend LED state data?
	jz .clear_send			; Something else, ignore
	mov al, [KB_FLAG_2]
	and al, KB_LEDS			; AL: bits 0 - 2: LED status

.resend_byte:
	and ah, ~SEND_LED_REQ		; Clear LED state changed flag
	inc ah				; Increase retry count
	mov [send_state], ah
	call send_kbd_data
	jmp .done

.ack:

	; Acknowledge set LEDs command or data

	mov ah, [send_state]
	mov al, 0xed
	test ah, SEND_LED_CMD
	jnz .send_led_data		; Send set LEDs command data
	test ah, SEND_LED_REQ		; LED state changed?
	jz .clear_send			; Nope, done
	mov al, 0xed			; LED state changed, set again
	mov byte [send_state], SEND_LED_CMD
	call send_kbd_data
	jmp .done

.send_led_data:
	mov byte [send_state], SEND_LED_DATA
	mov al, [KB_FLAG_2]
	and al, KB_LEDS			; AL: bits 0 - 2: LED status
	call send_kbd_data
	jmp .done

.clear_send:

	; Set LEDs complete or retry failed 3 times

	mov byte [send_state], 0	; Cancel all command transfer
	jmp .done

.e0:

	; Start of E0 marker scan code sequence

	or byte [KB_FLAG_3], LC_E0
	jmp .done_eoi

.e1:

	; Start of E1 marker scan code sequence

	or byte [KB_FLAG_3], LC_E1
	jmp .done_eoi

.within_e1:

	; Multi-byte scan code with E1 marker (Pause)
	; AL: scan code

	mov ah, al
	and ah, 0x7f
	cmp ah, 0x1d
	je .done_eoi
	mov ah, KC_PAUSE
	cmp al, 0x45
	je .key_release
	cmp al, 0xc5
	je .key_down

.done:

	; Done processing, clear E0/E1 marker flags and any pending scan code
	; sequence checks

	and byte [KB_FLAG_3], ~(LC_E0 | LC_E1)
	mov byte [seq_length], 0

.done_eoi:
	irq_pic_eoi 1

	pop ds
	pop ecx
	pop eax
	iret


;------------------------------------------------------------------------------
; Returns modifier based on keycode.
;------------------------------------------------------------------------------
; -> AH - Keycode
; <- ECX - Modifier
;------------------------------------------------------------------------------

get_modifier:
	mov ecx, MODIFIER_TABLE_COUNT

.check_modifier_loop:
	cmp ah, [modifier_table + ecx * 4 - 4]
	je .found_modifier
	dec ecx
	jnz .check_modifier_loop
	xor ecx, ecx
	ret

.found_modifier:
	movzx ecx, word [modifier_table + ecx * 4 - 2]
	ret


;------------------------------------------------------------------------------
; Update BIOS modifier key flags based on internal state.
;------------------------------------------------------------------------------
; <- Destroys EAX and ECX
;------------------------------------------------------------------------------

%macro	bios_flag 2

	test ecx, %1
	setnz al
	%if (%2 > 0)
	shl al, %2
	%endif
	or ah, al

%endmacro

set_bios_modifiers:
	mov ecx, [modifiers]

	xor ah, ah			; Update flag at 0x417
	bios_flag KBD_INSERT_ON, 7
	bios_flag KBD_CAPS_ON, 6
	bios_flag KBD_NUM_ON, 5
	bios_flag KBD_SCROLL_ON, 4
	bios_flag KBD_ALT, 3
	bios_flag KBD_CTRL, 2
	bios_flag KBD_SHIFT_LEFT, 1
	bios_flag KBD_SHIFT_RIGHT, 0
	mov [KB_FLAG], ah

	xor ah, ah			; Update flag at 0x418
	bios_flag KBD_INSERT, 7
	bios_flag KBD_CAPS, 6
	bios_flag KBD_NUM, 5
	bios_flag KBD_SCROLL, 4
	bios_flag KBD_ALT_LEFT, 1
	bios_flag KBD_CTRL_LEFT, 0
	mov [KB_FLAG_1], ah

	mov ah, [KB_FLAG_3]		; Update flag at 0x496
	and ah, 0x0c
	bios_flag KBD_ALT_RIGHT, 3
	bios_flag KBD_CTRL_RIGHT, 2
	mov [KB_FLAG_3], ah

	ret


;------------------------------------------------------------------------------
; Initialize modifier flags based on BIOS state.
;------------------------------------------------------------------------------
; <- Destroys EAX, ECX, EDX
;------------------------------------------------------------------------------

%macro	mod_flag 2

	xor edx, edx
	test ah, 1 << %2
	setnz dl
	neg edx				; EBX: 0xffffffff if flag set, else 0
	and edx, %1
	or ecx, edx

%endmacro

init_modifiers:
	xor ecx, ecx

	mov ah, [KB_FLAG]		; Set from flags at 0x417
	mod_flag KBD_INSERT_ON, 7
	mod_flag KBD_CAPS_ON, 6
	mod_flag KBD_NUM_ON, 5
	mod_flag KBD_SCROLL_ON, 4
	mod_flag KBD_SHIFT_LEFT, 1
	mod_flag KBD_SHIFT_RIGHT, 0

	mov ah, [KB_FLAG_1]		; Set from flags at 0x418
	mod_flag KBD_INSERT, 7
	mod_flag KBD_CAPS, 6
	mod_flag KBD_NUM, 5
	mod_flag KBD_SCROLL, 4
	mod_flag KBD_ALT_LEFT, 1
	mod_flag KBD_CTRL_LEFT, 0

	mov ah, [KB_FLAG_3]		; Set from flags at 0x496
	mod_flag KBD_ALT_RIGHT, 3
	mod_flag KBD_CTRL_RIGHT, 2

	mov [modifiers], ecx

	ret


;------------------------------------------------------------------------------
; Synchronize keyboard LEDs with current toggle values.
;------------------------------------------------------------------------------
; <- Destroys EAX and ECX
;------------------------------------------------------------------------------

sync_leds:
	mov ecx, [modifiers]
	and ecx, KBD_LEDS		; CL: bits 0 - 2: LED status togglers
	mov al, [KB_FLAG_2]
	and al, KB_LEDS			; AL: bits 0 - 2: LED status
	cmp al, cl
	jne .set_leds			; Different, update keyboard LEDs

.exit:
	ret

.set_leds:
	mov al, cl			; AL: new LED status
	mov ah, [KB_FLAG_2]		; Update LED status bits in BIOS data
	and ah, 0xf8
	or ah, al
	mov [KB_FLAG_2], ah

	test byte [send_state], SEND_LED_CMD | SEND_LED_REQ
	jnz .exit			; Already setting LED state
	test byte [send_state], SEND_LED_DATA
	jnz .set_leds_again		; Sending LED data, set again after that

	mov al, 0xed			; Send set LEDs command
	mov byte [send_state], SEND_LED_CMD
	call send_kbd_data
	jmp .exit

.set_leds_again:
	or byte [send_state], SEND_LED_REQ
	jmp .exit


;------------------------------------------------------------------------------
; Save keyboard event to buffer.
;------------------------------------------------------------------------------
; -> AL - Keyboard event (KBD_EVT_*)
;    AH - Keycode
; <- Destroys EAX, ECX
;------------------------------------------------------------------------------

add_to_buffer:
	mov ecx, [kb_buffer_tail]	; Try to add to buffer
	add ecx, 4
	cmp ecx, kb_buffer + BUFFER_SIZE * 4
	jb .check_overflow
	sub ecx, BUFFER_SIZE * 4

.check_overflow:
	cmp ecx, [kb_buffer_head]	; Enough space in buffer?
	je .exit			; Nope, discard keypress
	xchg [kb_buffer_tail], ecx	; Update keyboard buffer tail address

	shl eax, 16			; EAX[0]: Keyboard event
	mov ax, [modifiers]		; EAX[1]: Keycode
	rol eax, 16			; EAX[2-3]: Modifiers
	mov [ecx], eax

.exit:
	ret


;------------------------------------------------------------------------------
; Send command or data byte to the keyboard.
;------------------------------------------------------------------------------
; -> AL - Command or data byte
;------------------------------------------------------------------------------
; Attempts to send for at least 19.5 milliseconds. If the keyboard does not
; become available during this period, transmit of command is aborted globally.
; Retries are instrumented asynchronously using systimer and won't block the
; main application/IRQ handler thread.
;------------------------------------------------------------------------------

send_kbd_data:
	push eax

	mov [cmd_byte], al		; Save byte to send
	mov byte [cmd_attempts], 20	; Initialize maximum attempt count
	mov eax, [cmd_callback]		; Clear retry timeout callback
	test eax, eax
	jz .send_byte
	call systimer_clear_timeout
	jmp .send_byte_clear_timeout

.resend_byte:
	push eax			; Entry point for timeout callback

.send_byte_clear_timeout:
	mov dword [cmd_callback], 0	; Clear timeout callback handle

.send_byte:
	in al, STATUS_PORT		; Keyboard ready to accept data?
	test al, OUTPUT_BUF_FULL
	jnz .schedule_retry		; No, try again later

	mov al, [cmd_byte]		; Transmit byte
	out PORT_A, al

.exit:
	pop eax
	ret

.schedule_retry:
	dec byte [cmd_attempts]		; Further attempts needed?
	jnz .set_timeout		; Yes, set timeout callback

.clear_send:
	mov byte [send_state], 0	; Cancel further transmits
	jmp .exit

.set_timeout:
	push ebx			; Request callback after 1/1024 second
	mov eax, 1
	mov ebx, .resend_byte
	call systimer_set_timeout
	pop ebx
	jc .clear_send			; No callback slot available, cancel

	mov [cmd_callback], eax		; Save callback handle
	jmp .exit


;==============================================================================
; Data area
;==============================================================================

section .data

kb_buffer_head	dd kb_buffer		; Pointer to top of keyboard buffer
kb_buffer_tail	dd kb_buffer		; Pointer to tail of keyboard buffer

kbd_ascii	dd kbd_layout_us	; Keycode -> ASCII table pointer
kbd_ascii_num	dd kbd_layout_us_num	; Keycode -> ASCII for keypad

%define mod(name) KC_ %+ name, KBD_ %+ name
modifier_table	dw mod(SCROLL)
		dw mod(NUM)
		dw mod(CAPS)
		dw mod(INSERT)
		dw mod(SHIFT_LEFT)
		dw mod(SHIFT_RIGHT)
		dw mod(CTRL_LEFT)
		dw mod(CTRL_RIGHT)
		dw mod(ALT_LEFT)
		dw mod(ALT_RIGHT)
		dw mod(GUI_LEFT)
		dw mod(GUI_RIGHT)
		MODIFIER_TABLE_COUNT EQU ($ - modifier_table) / 4

irq1_installed	db 0			; Keyboard IRQ handler install status
send_state	db 0			; Keyboard command send state

scancodes	db 0			; 0x00: Invalid scan code
		db KC_ESC		; 0x01: Esc
		; 0x02 - 0x0b: 1234567890
		db KC_1, KC_2, KC_3, KC_4, KC_5, KC_6, KC_7, KC_8, KC_9, KC_0
		db KC_MINUS		; 0x0c: -
		db KC_EQUALS		; 0x0d: =
		db KC_BACKSPACE		; 0x0e: Backspace
		db KC_TAB		; 0x0f: Tab
		; 0x10 - 0x19: QWERTYUIOP
		db KC_Q, KC_W, KC_E, KC_R, KC_T, KC_Y, KC_U, KC_I, KC_O, KC_P
		db KC_BRACKET_OPEN	; 0x1a: [
		db KC_BRACKET_CLS	; 0x1b: ]
		db KC_ENTER		; 0x1c: Enter
		db KC_CTRL_LEFT		; 0x1d: Left ctrl
		; 0x1e - 0x26: ASDFGHJKL
		db KC_A, KC_S, KC_D, KC_F, KC_G, KC_H, KC_J, KC_K, KC_L
		db KC_SEMICOLON		; 0x27: ;
		db KC_APOSTROPHE	; 0x28: '
		db KC_BACKTICK		; 0x29: `
		db KC_SHIFT_LEFT	; 0x2a: Left shift
		db KC_BACKSLASH		; 0x2b: \
		; 0x2c - 0x32: ZXCVBNM
		db KC_Z, KC_X, KC_C, KC_V, KC_B, KC_N, KC_M
		db KC_COMMA		; 0x33: ,
		db KC_DOT		; 0x34: .
		db KC_SLASH		; 0x35: /
		db KC_SHIFT_RIGHT	; 0x36: Right shift
		db KC_KP_ASTERISK	; 0x37: Keypad *
		db KC_ALT_LEFT		; 0x38: Left alt
		db KC_SPACE		; 0x39: Space
		db KC_CAPS		; 0x40: Caps lock
		; 0x3b - 0x44: F1 - F10
		db KC_F1, KC_F2, KC_F3, KC_F4, KC_F5
		db KC_F6, KC_F7, KC_F8, KC_F9, KC_F10
		db KC_NUM		; 0x45: Num lock
		db KC_SCROLL		; 0x46: Scroll lock
		db KC_KP_7		; 0x47: Keypad 7
		db KC_KP_8		; 0x48: Keypad 8
		db KC_KP_9		; 0x49: Keypad 9
		db KC_KP_MINUS		; 0x4a: Keypad -
		db KC_KP_4		; 0x4b: Keypad 4
		db KC_KP_5		; 0x4c: Keypad 5
		db KC_KP_6		; 0x4d: Keypad 6
		db KC_KP_PLUS		; 0x4e: Keypad +
		db KC_KP_1		; 0x4f: Keypad 1
		db KC_KP_2		; 0x50: Keypad 2
		db KC_KP_3		; 0x51: Keypad 3
		db KC_KP_0		; 0x52: Keypad 0
		db KC_KP_DOT		; 0x53: Keypad .
		db KC_SYSREQ		; 0x54: SysReq
		db 0			; 0x55: Invalid scan code
		db KC_INTL		; 0x56: International keyboard extra key
		db KC_F11		; 0x57: F11
		db KC_F12		; 0x58: F12
		SCANCODES_COUNT EQU $ - scancodes

scancodes_e0	db 0x5e, KC_POWER	; ACPI: Power
		db 0x5f, KC_SLEEP	; ACPI: Sleep
		db 0x63, KC_WAKE	; ACPI: Wake
		db 0x10, KC_MM_TRK_PREV	; MM: Previous track
		db 0x19, KC_MM_TRK_NEXT	; MM: Next track
		db 0x20, KC_MM_MUTE	; MM: Mute
		db 0x21, KC_MM_CALC	; MM: Calculator
		db 0x22, KC_MM_PLAY	; MM: Play
		db 0x24, KC_MM_STOP	; MM: Stop
		db 0x2e, KC_MM_VOL_DOWN	; MM: Volume -
		db 0x30, KC_MM_VOL_UP	; MM: Volume +
		db 0x32, KC_WWW_HOME	; WWW: Home
		db 0x65, KC_WWW_SEARCH	; WWW: Search
		db 0x66, KC_WWW_FAVS	; WWW: Favorites
		db 0x67, KC_WWW_RELOAD	; WWW: Refresh
		db 0x68, KC_WWW_STOP	; WWW: Stop
		db 0x69, KC_WWW_FORWARD	; WWW: Forward
		db 0x6a, KC_WWW_BACK	; WWW: Back
		db 0x6b, KC_MM_EXPLORER	; MM: My computer
		db 0x6c, KC_MM_EMAIL	; MM: E-mail
		db 0x6d, KC_MM_MEDIA_SEL; MM: Media select
		db 0x1c, KC_KP_ENTER	; Keypad enter
		db 0x1d, KC_CTRL_RIGHT	; Right control
		db 0x35, KC_KP_SLASH	; Keypad /
		db 0x38, KC_ALT_RIGHT	; Right alt
		db 0x47, KC_HOME	; Home
		db 0x48, KC_CURSOR_UP	; Cursor up
		db 0x49, KC_PAGE_UP	; Page up
		db 0x4b, KC_CURSOR_LEFT	; Cursor left
		db 0x4d, KC_CURSOR_RIGHT; Cursor right
		db 0x4f, KC_END		; End
		db 0x50, KC_CURSOR_DOWN	; Cursor down
		db 0x51, KC_PAGE_DOWN	; Page down
		db 0x52, KC_INSERT	; Insert
		db 0x53, KC_DELETE	; Delete
		db 0x5b, KC_GUI_LEFT	; Left GUI
		db 0x5c, KC_GUI_RIGHT	; Right GUI
		db 0x5d, KC_MENU	; Menu
		db 0x37, KC_PRINT_SCREEN; Print screen
		SCANCODES_E0_COUNT EQU ($ - scancodes_e0) / 2

pause_down	db 0xe1, 0x1d, 0x45
pause_rel	db 0xe1, 0x9d, 0xc5
		PAUSE_LENGTH EQU 3

%include "rtl/kblayout/us.inc"

section .bss

data_sel	resd 1			; Flat data segment selector
old_irq1_hndlr	resd 2			; Original keyboard IRQ handler
kb_buffer	resd BUFFER_SIZE	; Keyboard buffer
modifiers	resd 1			; Keyboard modifiers and toggles
cmd_callback	resd 1			; Keyboard command timeout callback
key_down	resb 256 / 8		; Bitmask for key down for each keycode
seq_ptr		resd 1			; Scan code sequence pointer
seq_length	resb 1			; Length of remaining scan code sequence
seq_keycode	resb 1			; Keycode of the sequence being checked
seq_scancode	resb 1			; First non-marker scan code in sequence
cmd_byte	resb 1			; Command byte to send
cmd_attempts	resb 1			; Command send attempts
