;==============================================================================
; Protected mode interface - ISA DMA services
;------------------------------------------------------------------------------
; Structures
;==============================================================================

;------------------------------------------------------------------------------
; Data structure of a DMA channel register port mapping table
;------------------------------------------------------------------------------

struc		dma_ports

.page		resb 1			; DMA controller page register
.start_address	resb 1			; Start address
.count		resb 1			; Byte/word count
.channel_mask	resb 1			; Channel mask
.mode		resb 1			; DMA transfer mode
.flipflop_reset	resb 1			; Flip/flop reset register
		alignb 8		; Must be 8 bytes long!
.strucsize:

endstruc
