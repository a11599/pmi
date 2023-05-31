;==============================================================================
; PMI runtime library - Hardware IRQ handling
;==============================================================================

	cpu 386

section .text

%include "pmi/api/pmi.inc"


;------------------------------------------------------------------------------
; Check if an IRQ is enabled by the PIC.
;------------------------------------------------------------------------------
; -> CL - IRQ number
; <- CF - Set if error
;    EAX - Error code if CF set
;    ZF - Set if IRQ is disabled
;------------------------------------------------------------------------------

	align 4

global irq_enabled
irq_enabled:
	cmp cl, 15
	ja irq_error

	push eax
	push ecx

	; Master PIC mask bits must be checked in any case, masking IRQ 2 masks
	; all IRQs (8 - 15) of the slave PIC

	in al, 0x21
	cmp cl, 8
	jb .get_irq_mask
	test al, 0x04			; IRQ 2 masked -> slave PIC masked
	jnz .exit

	; IRQ 2 not masked, get mask bit of slave PIC for IRQs 8 - 15

	sub cl, 8
	in al, 0xa1

.get_irq_mask:

	; Get and test IRQ mask bit from PIC

	mov ah, 1
	shl ah, cl
	test al, ah

.exit:
	clc
	pop ecx
	pop eax
	ret

irq_error:
	mov eax, PMI_E_INV_IRQ
	stc
	ret


;------------------------------------------------------------------------------
; Disable IRQ (set mask bit). The PIC will ignore these IRQs and no interrupt
; will be raised.
;------------------------------------------------------------------------------
; -> CL - IRQ number
; <- CF - Set if error
;    EAX - Error code if CF set
;------------------------------------------------------------------------------

	align 4

global irq_disable
irq_disable:
	cmp cl, 15
	ja irq_error

	push eax
	push ecx
	push edx

	; Setup input for mask bit clearing

	xor edx, edx
	mov dl, 0x21
	cmp cl, 8
	jb .disable_irq
	sub cl, 8
	mov dl, 0xa1

.disable_irq:

	; Set mask bit of IRQ (disable)

	mov ah, 1
	shl ah, cl
	in al, dx
	or al, ah
	out dx, al

.exit:
	pop edx
	pop ecx
	pop eax
	ret


;------------------------------------------------------------------------------
; Enable IRQ (clear mask bit).
;------------------------------------------------------------------------------
; -> CL - IRQ number
; <- CF - Set if error
;    EAX - Error code if CF set
;------------------------------------------------------------------------------

	align 4

global irq_enable
irq_enable:
	cmp cl, 15
	ja irq_error

	push eax
	push ecx
	push edx

	; Setup input for mask bit clearing

	xor edx, edx
	mov dl, 0x21
	cmp cl, 8
	jb .enable_irq
	sub cl, 8
	mov dl, 0xa1

	; IRQ 8 - 15: check mask bit of IRQ 2

	in al, 0x21
	test al, 0x04
	jz .enable_irq			; IRQ 2 not masked -> slave PIC enabled
	mov ch, al
	mov al, 1			; Unmask only wanted IRQ in slave PIC
	shl al, cl
	not al
	out dx, al
	mov al, ch			; Unmask (enable) IRQ 2 (slave PIC)
	and al, ~(0x04)
	out 0x21, al
	jmp .exit

.enable_irq:

	; Clear mask bit of IRQ (enable)

	mov ah, 1
	shl ah, cl
	not ah
	in al, dx
	and al, ah
	out dx, al

.exit:
	pop edx
	pop ecx
	pop eax
	ret
