;==============================================================================
; PMI runtime library - System timer
;------------------------------------------------------------------------------
; A timer (IRQ 0) independent solution that generates 1024 ticks per second.
; Uses the real time clock (IRQ 8) for now.
;==============================================================================

	cpu 386

section .text

%include "pmi/api/pmi.inc"
%include "rtl/api/irq.inc"

; Number of timeout callbacks. Maximum is 255. Setting to higher values may
; slow down the system timer and thus the entire application, so be
; conservative and don't overuse timeout callbacks.

CALLBACK_COUNT	EQU 32			; Number of timeout callbacks (max. 255)

; Hardware support indicator values

SYSTIMER_DUNNO	EQU -1			; Hardware support not tested yet
SYSTIMER_ON	EQU 1			; Hardware supports systimer
SYSTIMER_OFF	EQU 0			; Hardware doesn't support systimer

%macro	rtc_delay 0

	jmp $ + 2
	jmp $ + 2
	jmp $ + 2

%endmacro


;------------------------------------------------------------------------------
; Install an RTC IRQ handler to provide a system-level 1/1024 second resolution
; timer. Interrupts will be enabled after this call!
;------------------------------------------------------------------------------
; <- CF - Set if error
;    EAX - Error code if CF set (PMI_E_INV_IRQ if not supported by hardware)
;------------------------------------------------------------------------------

global systimer_start
systimer_start:
	push ecx
	push edx
	push edi
	push eax

	inc dword [refcount]		; Increase reference counter
	cmp dword [refcount], 1
	jne .done			; Already installed

	mov eax, PMI_E_INV_IRQ
	cmp byte [systimer_works], SYSTIMER_OFF
	je .error			; Not supported by hardware

	mov [data_sel], ds		; Save flat data selector for IRQ

	mov edi, timeouts		; Erase timeout callback area
	xor eax, eax
	mov ecx, CALLBACK_COUNT * 2
	rep stosd

	mov dword [systimer_ticks], 0	; Initialize variables
	mov dword [timeouts_active], 0

	mov cl, 8			; Save original IRQ 8 enabled status
	call irq_enabled
	jc .error
	setnz [old_irq8_enable]
	mov al, 8			; Save current handler
	call pmi(get_irq_hndlr)
	jc .error
	mov [old_irq8_hndlr], edx
	mov [old_irq8_hndlr + 4], cx
	mov cx, cs			; Set new handler
	mov edx, rtc_irq_handler
	call pmi(set_irq_hndlr)
	jc .error

	; Program the RTC - These are BIOS-friendly safe values and should be
	; safe even if the application terminates unexpectedly

	cli

	mov al, 0x8a			; Set 1024 Hz timer rate
	out 0x70, al
	rtc_delay
	in al, 0x71
	mov ah, al
	mov [old_rtc_reg_a], ah
	and ah, 0xf0
	or ah, 0x06
	mov al, 0x8a
	out 0x70, al
	rtc_delay
	mov al, ah
	out 0x71, al

	mov al, 0x8b			; Enable periodic interrupts
	out 0x70, al
	rtc_delay
	in al, 0x71
	mov ah, al
	mov [old_rtc_reg_b], ah
	and ah, 0x8f			; Disable other interrupts
	or ah, 0x40
	mov al, 0x8b
	out 0x70, al
	rtc_delay
	mov al, ah
	out 0x71, al

	mov al, 0x0c			; Ack pending interrupts, enable NMI
	out 0x70, al
	rtc_delay
	in al, 0x71

	mov cl, 8			; Enable IRQ 8
	call irq_enable

	sti

	; Check if RTC IRQ - and thus systimer - is actually supported by the
	; hardware. A systimer tick should occur within ~1 ms, wait up to 36.2
	; ms for the tick to happen.

	cmp byte [systimer_works], SYSTIMER_ON
	je .done
	mov eax, [0x46c]		; Check if RTC IRQ runs
	mov ebx, [systimer_ticks]

.check_timer_loop:
	cmp ebx, [systimer_ticks]
	jne .works			; Systimer tick occured, it works
	mov ecx, [0x46c]
	sub ecx, eax
	cmp ecx, 2			; Wait up to two timer ticks
	jb .check_timer_loop

	mov byte [systimer_works], SYSTIMER_OFF
	call systimer_stop		; Doesn't work, undo changes
	mov eax, PMI_E_INV_IRQ
	jmp .error_hw

.works:
	mov byte [systimer_works], SYSTIMER_ON

.done:
	pop eax
	clc

.exit:
	pop edi
	pop edx
	pop ecx
	ret

.error:
	dec dword [refcount]		; Decrease reference counter

.error_hw:
	add esp, 4			; Discard EAX from stack
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Restore original RTC IRQ handler (real mode redirector).
;------------------------------------------------------------------------------
; <- CF - Set if error
;    EAX - Error code if CF set
;------------------------------------------------------------------------------

global systimer_stop
systimer_stop:
	push ecx
	push edx
	push eax

	cmp dword [refcount], 1
	jb .noop			; Systimer not installed
	ja .done			; Reference counter > 1

	; Restore RTC settings

	pushf
	cli

	mov al, 0x8a			; Restore timer rate
	out 0x70, al
	rtc_delay
	mov al, [old_rtc_reg_a]
	out 0x71, al
	mov al, 0x8b			; Restore interrupt settings
	out 0x70, al
	rtc_delay
	mov al, [old_rtc_reg_b]
	out 0x71, al

	mov al, 0x0c			; Ack pending interrupts, enable NMI
	out 0x70, al
	rtc_delay
	in al, 0x71

	cmp byte [old_irq8_enable], 0	; Restore IRQ 8 enable state
	jne .uninstall_handler
	mov cl, 8
	call irq_disable

.uninstall_handler:
	mov al, 8			; Uninstall IRQ handler
	mov edx, [old_irq8_hndlr]
	mov cx, [old_irq8_hndlr + 4]
	call pmi(set_irq_hndlr)

	popf

.done:
	dec dword [refcount]		; Decrease reference counter

.noop:
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
; Set a timeout callback.
;------------------------------------------------------------------------------
; -> EAX - Number of 1/1024 seconds until the callback
;    EBX - Address of the callback procedure
; <- CF - Set if no more callback slots available
;    EAX - Handle of the callback or 0 if CF set
;------------------------------------------------------------------------------
; The callback is called after a guaranteed timeout of at least the number of
; 1/1024 second ticks specified in EAX. Callbacks are executed in an IRQ
; context where DS is set to the flat data segment and interrupts are enabled.
; The callbacks should save all registers they alter and obey all rules
; regarding normal PMI IRQ handlers. The callback may call systimer_set_timeout
; and systimer_clear_timeout when necessary. The callback handle will already
; be freed by the time the callback procedure is invoked.
;------------------------------------------------------------------------------

global systimer_set_timeout
systimer_set_timeout:
	push ecx

	mov ecx, CALLBACK_COUNT

.find_slot:
	cmp dword [timeouts + ecx * 8 - 8], 0
	je .set_timeout
	dec ecx
	jnz .find_slot

	xor eax, eax
	stc				; No available slots
	jmp .exit

.set_timeout:
	pushf
	cli

	add eax, [systimer_ticks]	; EAX: target tick count
	inc eax
	mov [timeouts + ecx * 8 - 4], ebx
	mov [timeouts + ecx * 8 - 8], eax
	inc dword [timeouts_active]

	popf
	shl eax, 8
	clc

.exit:
	mov al, cl			; EAX: bits 0-7: index, bits 8-31: tick
	pop ecx
	ret


;------------------------------------------------------------------------------
; Clear (remove) a timeout callback.
;------------------------------------------------------------------------------
; -> EAX - Handle of the callback
; <- CF - Set if handle is invalid (or callback has been triggered already)
;------------------------------------------------------------------------------

global systimer_clear_timeout
systimer_clear_timeout:
	push eax
	push ebx
	push ecx

	test al, al
	jz .error			; Invalid handle (0)
	cmp al, CALLBACK_COUNT
	ja .error			; Invalid handle (too large)

	xor ecx, ecx
	mov cl, al

	pushf
	cli

	mov ebx, [timeouts + ecx * 8 - 8]
	xor al, al
	shl ebx, 8
	cmp ebx, eax			; No match with stored tick
	jne .error_clear
	mov dword [timeouts + ecx * 8 - 8], 0
	dec dword [timeouts_active]

	popf
	clc

.exit:
	pop ecx
	pop ebx
	pop eax
	ret

.error_clear:
	popf

.error:
	stc
	jmp .exit


;------------------------------------------------------------------------------
; RTC IRQ handler.
;------------------------------------------------------------------------------

rtc_irq_handler:
	push eax
	push ds

	mov ds, cs:[data_sel]

	inc dword [systimer_ticks]	; Increase tick count

	mov al, 0x0c			; Acknowledge pending RTC interrupts
	out 0x70, al
	rtc_delay
	in al, 0x71
	irq_pic_eoi 8			; Acknowledge PIC IRQ

	cmp dword [timeouts_active], 0
	jne .check_timeouts

.done:
	pop ds
	pop eax
	iret

.check_timeouts:

	; Process pending timeouts

	mov eax, [systimer_ticks]
	sti				; Enable further IRQs

	push ebx
	push ecx
	mov ecx, CALLBACK_COUNT

.timeout_loop:
	dec ecx
	js .done_timeouts
	cmp [timeouts + ecx * 8], eax
	jne .timeout_loop
	mov ebx, [timeouts + ecx * 8 + 4]
	mov dword [timeouts + ecx * 8], 0
	dec dword [timeouts_active]
	call ebx
	jmp .timeout_loop

.done_timeouts:
	pop ecx
	pop ebx
	jmp .done


;==============================================================================
; Data area
;==============================================================================

section .data

global systimer_ticks
systimer_ticks	dd 0			; Ticks since systimer start
refcount	dd 0			; Timer IRQ handler install status
systimer_works	db SYSTIMER_DUNNO	; Timer hardware support indicator

section .bss

data_sel	resd 1			; Flat data segment selector
old_irq8_hndlr	resd 2			; Original timer IRQ handler
timeouts_active	resd 1			; Number of active timeout callbacks
old_rtc_reg_a	resb 1			; Original RTC register A
old_rtc_reg_b	resb 1			; Original RTC register B
old_irq8_enable	resb 1			; Original IRQ 8 enabled flag
timeouts	resd CALLBACK_COUNT * 2	; Timeout callbacks
