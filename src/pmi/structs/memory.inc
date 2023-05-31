;==============================================================================
; Protected mode interface - Memory management
;------------------------------------------------------------------------------
; Structures
;==============================================================================

;------------------------------------------------------------------------------
; Data structure of a memory control block
;------------------------------------------------------------------------------

struc		mcb

.signature	resw 1			; Signature, used for health check
.status		resw 1			; Block status flags
.size		resd 1			; Size of the block in bytes
.prev		resd 1			; Linear address of previous block or -1
.next		resd 1			; Linear address of next block or -1
		alignb 4
.strucsize:

endstruc
