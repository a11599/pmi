;==============================================================================
; Protected mode interface
;------------------------------------------------------------------------------
; Public API
;==============================================================================

;------------------------------------------------------------------------------
; Error codes
;------------------------------------------------------------------------------

PMI_E_V86	EQU -1			; CPU in V86 mode and no VCPI or DPMI
PMI_E_MEM_LOW	EQU -2			; Not enough memory
PMI_E_DPMI_INIT	EQU -3			; DPMI host cannot enter protected mode
PMI_E_MEM_INVL	EQU -4			; Memory blocks corrupted
PMI_E_MEM_BLOCK	EQU -5			; Memory block not available
PMI_E_EXCEPTION	EQU -6			; Exception during runtime
PMI_E_INV_PE	EQU -7			; PE executable not found
PMI_E_INV_IRQ	EQU -8			; Invalid IRQ number
PMI_E_INV_DMA	EQU -9			; Invalid DMA channel

;------------------------------------------------------------------------------
; Flags for call_rm function
;------------------------------------------------------------------------------

PMI_CALL_FAR	EQU 0x00		; Call far real mode procedure
PMI_CALL_INT	EQU 0x01		; Call real mode interrupt

;------------------------------------------------------------------------------
; Memory allocation modes for mem_alloc
;------------------------------------------------------------------------------

PMI_MEM_LO	EQU 0			; Conventional memory only
PMI_MEM_LO_HI	EQU 1			; Conventional, then high memory
PMI_MEM_HI	EQU 2			; High memory only
PMI_MEM_HI_LO	EQU 3			; High, then conventional memory
PMI_MEM_DMA	EQU 4			; Memory block for ISA DMA
PMI_MEM_DMA_LO	EQU 5			; Conventional memory block for ISA DMA

;------------------------------------------------------------------------------
; File access modes
;------------------------------------------------------------------------------

PMI_FILE_READ	EQU 0x01		; Read access
PMI_FILE_WRITE	EQU 0x02		; Write access
PMI_FILE_CREATE	EQU 0x20		; Create file if not exists
PMI_FILE_TRUNC	EQU 0x40		; Truncate existing file (overwrite)
PMI_FILE_COMMIT	EQU 0x80		; Commit writes instantly (DOS 4.0+)

;------------------------------------------------------------------------------
; File seek origin position codes
;------------------------------------------------------------------------------

PMI_SEEK_START	EQU 0			; Seek relative to start of file
PMI_SEEK_CURPOS	EQU 1			; Seek relative to current position
PMI_SEEK_END	EQU 2			; Seek relative to to end of file

;------------------------------------------------------------------------------
; DMA transfer modes
;------------------------------------------------------------------------------

PMI_DMA_WRITE	EQU 0x04		; Peripheral writes to memory
PMI_DMA_READ	EQU 0x08		; Peripheral reads from memory
PMI_DMA_AUTO	EQU 0x10		; Auto restart at the end of block
PMI_DMA_BACK	EQU 0x20		; Decrement address after each operation
PMI_DMA_DEMAND	EQU 0x00		; On-demand DMA transfer
PMI_DMA_SINGLE	EQU 0x40		; Single DMA (byte-by-byte) transfer
PMI_DMA_BLOCK	EQU 0X80		; Block DMA (block of memory) transfer

;------------------------------------------------------------------------------
; Protected mode host types
;------------------------------------------------------------------------------

PMI_HOST_RAW	EQU 0			; PMI built-in protected mode host
PMI_HOST_VCPI	EQU 1			; VCPI host
PMI_HOST_DPMI	EQU 2			; DPMI host

;------------------------------------------------------------------------------
; Structure of far pointers to PMI public functions.
;------------------------------------------------------------------------------

struc		pmi_fns

.get_env_info	resd 2			; Get PMI environment information
.call_rm	resd 2			; Call real mode procedure
.get_irq_hndlr	resd 2			; Get current IRQ handler
.set_irq_hndlr	resd 2			; Set IRQ handler

.execute	resd 2			; Execute program
.terminate	resd 2			; Terminate program

.mem_alloc	resd 2			; Allocate memory block
.mem_free	resd 2			; Free memory block

.file_open	resd 2			; Open file
.file_close	resd 2			; Close file
.file_read	resd 2			; Read from file
.file_write	resd 2			; Write to file
.file_set_pos	resd 2			; Seek file position
.file_get_pos	resd 2			; Get current file position
.file_get_buf	resd 2			; Get conventional memory I/O buffer
.file_free_buf	resd 2			; Free conventional memory I/O buffer

.dma_start	resd 2			; Start DMA transfer
.dma_stop	resd 2			; Stop DMA transfer
.strucsize:

endstruc

;------------------------------------------------------------------------------
; Macro to call PMI public API functions from client applications. Use with
; call instruction and use label of members from pmi_fns structure.
;------------------------------------------------------------------------------
; Example:
;	call pmi(terminate)
;------------------------------------------------------------------------------

%define pmi(fn) far [gs:pmi_fns. %+ fn]

;------------------------------------------------------------------------------
; Data structure for real mode calls for call_rm to simulate a real mode
; interrupt or far call.
;------------------------------------------------------------------------------

struc		pmi_rm_call

.flags		resb 1			; Real mode procedure flags (PMI_CALL_*)
.int		resb 1			; Interrupt number when PMI_CALL_INT
.error_code	resw 1			; Error code if call failed
.ip		resw 1			; Offset when PMI_CALL_FAR
.cs		resw 1			; Segment address when PMI_CALL_FAR
.ds		resw 1			; DS segment register
.es		resw 1			; ES segment register
.fs		resw 1			; FS segment register
.gs		resw 1			; GS segment register
		alignb 4
.strucsize:

endstruc

;------------------------------------------------------------------------------
; Data structure for env_info service.
;------------------------------------------------------------------------------

struc		pmi_env_info

.cmb_base	resd 1			; Conventional memory area base address
.cmb_size	resd 1			; Conventional memory area size
.xmb_base	resd 1			; Extended memory area base address
.xmb_size	resd 1			; Extended memory area size
.psp_addr	resd 1			; Address of DOS PSP
.pcb_addr	resd 1			; Address of program control block
.io_buf_addr	resd 1			; Address of file services I/O buffer
.io_buf_size	resd 1			; Size of the file services I/O buffer
.dos_ver	resw 1			; DOS version number (major.minor)
.pm_host_type	resb 1			; Protected mode host type
.startup_vmode	resb 1			; Video mode before starting application
		alignb 4
.strucsize:

endstruc