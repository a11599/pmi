;==============================================================================
; PMI runtime library - Hardware IRQ handling
;------------------------------------------------------------------------------
; API
;==============================================================================

extern irq_enabled
extern irq_disable
extern irq_enable


;------------------------------------------------------------------------------
; Macro to send end-of-interrupt signal to the PIC.
;------------------------------------------------------------------------------
; -> %1 - IRQ number
; <- Destroys AL
;------------------------------------------------------------------------------

%macro	irq_pic_eoi 1

	%ifnum %1

	; Optimize for numeric constants in %1

	mov al, 0x20
	%if (%1 > 7)
	out 0xa0, al			; Send EOI to slave PIC
	%endif
	out 0x20, al			; Always send EOI to master PIC

	%else

	; %1 is register or memory data

	cmp %1, 7			; Compare first, %1 may be AL
	mov al, 0x20
	jbe %%eoi_master
	out 0xa0, al			; Send EOI to slave PIC

%%eoi_master:
	out 0x20, al			; Always send EOI to master PIC

	%endif

%endmacro
