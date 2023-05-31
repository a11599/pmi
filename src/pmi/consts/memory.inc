;==============================================================================
; Protected mode interface - Memory management
;------------------------------------------------------------------------------
; Constants
;==============================================================================

MIN_BLOCK_SIZE	EQU 1024		; Minimum block split size
DMA_MAX_ADDRESS	EQU 0x01000000		; Maximum DMA memory address (16 MB)

; Memory control block signature

%define MCB_SIGNATURE 'MB'

; Memory control block status flags

MCB_USED	EQU 0x0001		; Block used

; Memory allocation control values

MEM_ALLOC_BTM	EQU 0			; Bottom-up memory allocation
MEM_ALLOC_TOP	EQU 1			; Top-down memory allocation
MEM_ALLOC_DMA	EQU 2			; Allocate memory for DMA

; Memory split control values

MEM_SPLIT_NOR	EQU 0			; Don't split if new block too small
MEM_SPLIT_FORCE	EQU 1			; Force split regardless new block size
