;==============================================================================
; PMI runtime library - Timer (IRQ 0) handling
;==============================================================================

	cpu 386

section .text

%include "pmi/api/pmi.inc"
%include "rtl/api/irq.inc"


;------------------------------------------------------------------------------
; Calculate PIT and actual interrupt rate for a requested timer interrupt rate.
;------------------------------------------------------------------------------
; -> EDX - Requested interrupt rate
; <- EAX - Actual interrupt rate (rounded to nearest integer)
;    BX - PIT reload value
;------------------------------------------------------------------------------

	align 4

global timer_calc_rate
timer_calc_rate:
	push ecx
	push edx
	push ebx

	; Handle edge cases

	cmp edx, 18			; Lowest rate
	ja .check_max_rate
	mov eax, 18
	xor ebx, ebx
	jmp .exit

.check_max_rate:
	cmp edx, 1193182		; Highest rate
	jb .calc_pit_rate
	mov eax, 1193182
	mov ebx, 1
	jmp .exit

.calc_pit_rate:

	; Calculate PIT rate

	mov ebx, edx			; EBX: requested rate
	xor edx, edx
	mov eax, 1193182		; EAX: PIT osc. frequency
	div ebx				; EAX: PIT rate

	; Round PIT rate

	mov ecx, ebx			; ECX: requested rate / 2
	shr ecx, 1
	cmp edx, ecx			; Remainder > requested rate / 2?
	setae dl
	movzx edx, dl			; EDX: 1 when yes, 0 otherwise
	add eax, edx
	mov ebx, eax			; EBX: PIT rate

	; Calculate real interrupt rate

	xor edx, edx
	mov eax, 1193182		; EAX: 1193182 (PIT osc. frequency)
	div ebx				; EAX: real interrupt rate

	; Round real interrupt rate

	mov ecx, ebx			; ECX: real interrupt rate / 2
	shr ecx, 1
	cmp edx, ecx			; Remainder > real interrupt rate / 2?
	setae dl
	movzx edx, dl			; EDX: 1 when yes, 0 otherwise
	add eax, edx

.exit:
	mov [esp], bx			; Set low word of EBX on stack
	pop ebx
	pop edx
	pop ecx
	ret


;------------------------------------------------------------------------------
; Set the rate of the timer interrupt.
;------------------------------------------------------------------------------
; -> BX - PIT reload value, as returned by timer_calc_rate
;------------------------------------------------------------------------------

	align 4

global timer_set_rate
timer_set_rate:
	push eax

	mov al, 0x34
	out 0x43, al
	mov al, bl
	out 0x40, al
	mov al, bh
	out 0x40, al

	pop eax
	ret


;------------------------------------------------------------------------------
; Restore timer interrupt rate to default ~18.2 Hz.
;------------------------------------------------------------------------------

	align 4

global timer_reset_rate
timer_reset_rate:
	push eax

	mov al, 0x36
	out 0x43, al
	xor al, al
	out 0x40, al
	out 0x40, al

	pop eax
	ret


;------------------------------------------------------------------------------
; Install an AT BIOS-compatible timer IRQ handler that runs entirely in
; protected mode, to prevent mode switches to real mode when handling default
; actions.
;------------------------------------------------------------------------------
; <- CF - Set if error
;    EAX - Error code if CF set
;------------------------------------------------------------------------------

global timer_start
timer_start:
	push ecx
	push edx
	push eax

	mov [data_sel], ds		; Save flat data selector for IRQ

	xor al, al			; Save current handler
	call pmi(get_irq_hndlr)
	jc .error
	mov [old_irq0_hndlr], edx
	mov [old_irq0_hndlr + 4], cx
	mov cx, cs			; Set new handler
	mov edx, timer_irq_handler
	call pmi(set_irq_hndlr)
	jc .error

	mov byte [irq0_installed], 1
	pop eax
	clc

.exit:
	pop edx
	pop ecx
	ret

.error:
	add esp, 4			; Discard EAX from stack
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Restore original timer IRQ handler (real mode redirector).
;------------------------------------------------------------------------------
; <- CF - Set if error
;    EAX - Error code if CF set
;------------------------------------------------------------------------------

global timer_stop
timer_stop:
	cmp byte [irq0_installed], 0
	je .noop

	push ecx
	push edx
	push eax

	xor al, al
	mov edx, [old_irq0_hndlr]
	mov cx, [old_irq0_hndlr + 4]
	call pmi(set_irq_hndlr)
	jc .error

	mov byte [irq0_installed], 0
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
; AT BIOS compatible protected mode timer IRQ handler.
;------------------------------------------------------------------------------

timer_irq_handler:
	push eax
	push ds

	mov ds, cs:[data_sel]

	; Update timer tick counter in BIOS data area

	mov eax, [0x46c]
	inc eax
	cmp eax, 0x1800b0
	jb .set_counter
	xor eax, eax
	mov byte [0x470], 1		; Set day overflow flag

.set_counter:
	mov [0x46c], eax

	; Stop floppy drive motor

	mov al, [0x440]
	test al, al
	jz .done
	dec al
	mov [0x440], al
	jnz .done

	push edx
	and byte [0x43f], 0xf0		; Turn off motor running bits
	mov al, 0x0c			; Stop floppy drive motor
	mov edx, 0x3f2
	out dx, al
	pop edx

.done:
	irq_pic_eoi 0

	pop ds
	pop eax
	iret


;==============================================================================
; Data area
;==============================================================================

section .data

irq0_installed	db 0			; Timer IRQ handler install status

section .bss

data_sel	resd 1			; Flat data segment selector
old_irq0_hndlr	resd 2			; Original timer IRQ handler
