;==============================================================================
; Protected mode interface - Kernel
;------------------------------------------------------------------------------
; Structures
;==============================================================================

;------------------------------------------------------------------------------
; Host private data structure when running under VCPI or no protected mode host
;------------------------------------------------------------------------------

struc		rv_priv_data

.rm_stack	resb RM_STACK_SIZE	; Real mode stack
.rm_stacktop:
.idt		resb 256 * 8		; Protected mode IDT
.strucsize:

endstruc

;------------------------------------------------------------------------------
; Host private data structure when running under VCPI. Starts with an
; rv_priv_data substructure so the start of private data is the same in all
; protected mode setups where PMI acts as the protected mode host.
;------------------------------------------------------------------------------

struc		v_priv_data

.rv_priv_data	resb rv_priv_data.strucsize
		alignb 4
.tss		resb 0x68		; TSS
.stack		resb 0x100		; VCPI temporary mode-switch stack
.stacktop:
.strucsize:

endstruc

;------------------------------------------------------------------------------
; VCPI real to protected mode switch procedure input data structure
;------------------------------------------------------------------------------

struc		v_rm2pm_data

.cr3		resd 1			; Protected mode CR3 register
.gdt_reg_addr	resd 1			; GDTR
.idt_reg_addr	resd 1			; IDTR
.ldt_reg	resw 1			; Selector for LDTR
.task_reg	resw 1			; TR
.eip		resd 1			; Protected mode target EIP
.cs		resw 1			; Protected mode target CS selector

endstruc

;------------------------------------------------------------------------------
; DPMI real mode simulate interrupt far call procedure service register
; structure
;------------------------------------------------------------------------------

struc		d_call_rm_data

.edi		resd 1			; EDI
.esi		resd 1			; ESI
.ebp		resd 1			; EBP
		resd 1			; Reserved, ignored
.ebx		resd 1			; EBX
.edx		resd 1			; EDX
.ecx		resd 1			; ECX
.eax		resd 1			; EAX
.flags		resw 1			; 16-bit flags
.es		resw 1			; ES segment
.ds		resw 1			; DS segment
.fs		resw 1			; FS segment
.gs		resw 1			; GS segment
.ip		resw 1			; Real mode target IP for far call
.cs		resw 1			; Real mode target CS for far call
.sp		resw 1			; SP (not used)
.ss		resw 1			; SS (not used)
		alignb 4
.strucsize:

endstruc

;------------------------------------------------------------------------------
; Exception dump for exception-handler-enabled builds.
;------------------------------------------------------------------------------

struc		exception_dump_data

.fault_code	resd 1			; Exception fault code
.eflags		resd 1			; 32-bit flags
.cs		resw 1			; CS selector
.ds		resw 1			; DS selector
.es		resw 1			; ES selector
.fs		resw 1			; FS selector
.gs		resw 1			; GS selector
.ss		resw 1			; SS selector
.eax		resd 1			; EAX
.ebx		resd 1			; EBX
.ecx		resd 1			; ECX
.edx		resd 1			; EDX
.esi		resd 1			; ESI
.edi		resd 1			; EDI
.ebp		resd 1			; EBP
.esp		resd 1			; ESP
.eip		resd 1			; EIP
.stack		resb 64			; Copy of last 64 bytes from stack
.cs_base	resd 1			; CS selector base address
.ds_base	resd 1			; DS selector base address
.es_base	resd 1			; ES selector base address
.fs_base	resd 1			; FS selector base address
.gs_base	resd 1			; GS selector base address
.ss_base	resd 1			; SS selector base address
.cs_limit	resd 1			; CS selector limit in bytes
.ds_limit	resd 1			; DS selector limit in bytes
.es_limit	resd 1			; ES selector limit in bytes
.fs_limit	resd 1			; FS selector limit in bytes
.gs_limit	resd 1			; GS selector limit in bytes
.ss_limit	resd 1			; SS selector limit in bytes
.cs_flags	resw 1			; CS selector access byte and flags
.ds_flags	resw 1			; DS selector access byte and flags
.es_flags	resw 1			; ES selector access byte and flags
.fs_flags	resw 1			; FS selector access byte and flags
.gs_flags	resw 1			; GS selector access byte and flags
.ss_flags	resw 1			; SS selector access byte and flags
.exception	resb 1			; Exception number
.strucsize:

endstruc
