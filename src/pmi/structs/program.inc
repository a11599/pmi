;==============================================================================
; Protected mode interface - Executable program manager
;------------------------------------------------------------------------------
; Structures
;==============================================================================

;------------------------------------------------------------------------------
; Program control block, contains essential data for the current executable
;------------------------------------------------------------------------------

struc		pcb

.signature	resd 1			; Signature, used for health check
.caller		resd 1			; PCB of caller executable
.ret_stack	resd 2			; Stack pointer for return to caller
.mem_addr	resd 1			; Allocated memory block address
.stack_addr	resd 1			; Stack memory block address
.args		resd 1			; Pointer to ASCIIZ argument string
.file		resd 1			; Pointer to ASCIIZ filename string
.code_base	resd 1			; Actual code base address
.strucsize:

endstruc
