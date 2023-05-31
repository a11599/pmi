;==============================================================================
; Protected mode interface - ISA DMA services
;------------------------------------------------------------------------------
; These don't strictly belong to the kernel since they are not depending on
; any kernel stuff for now. They are kept here in case PMI will support VDS
; under DPMI in the future. VDS is a hot mess and I don't have the energy to
; figure it out. For now, we just allocate conventional memory only and hope
; the DPMI server either identity maps this area or has a transparent page
; remapping implementation.
;==============================================================================

	cpu 386

	group pmi code

segment code public use16 class=CODE align=16
segment code

%include "pmi/config.inc"
%include "pmi/api/pmi.inc"
%include "pmi/api/kernel.inc"
%include "pmi/structs/dma.inc"

%define PMI_FN(fn) pmi_fn + pmi_fns. %+ fn
%define	port(register) dma_registers + dma_ports. %+ register


;------------------------------------------------------------------------------
; Start DMA transfer. Interrupts are disabled during this process so nothing
; can mess with the DMA controller while this is happening.
;------------------------------------------------------------------------------
; -> EBX - Physical address of DMA buffer (word aligned for channels 4-7)
;    ECX - Number of bytes to transfer (can't cross a 64 KB boundary)
;    DL - DMA channel number
;    DH - DMA mode (PMI_DMA_*)
; <- CF - Set if error
;    EAX - Error code if CF set
;------------------------------------------------------------------------------

	[bits 32]
	align 4

global dma_start
dma_start:
	cmp dl, 7
	jae dma_error

	push cs
	call dma_stop

	push eax
	push ebx
	push ecx
	push edx
	push esi
	push edi
	push ds

	mov ds, cs:[sel_data_krnl32]

	test dh, PMI_DMA_AUTO
	jz .check_direction
	xor eax, eax			; Mark DMA channel for running
	mov al, dl
	bts [dma_running], eax

.check_direction:
	test dh, PMI_DMA_BACK
	jz .calc_address
	add ebx, ecx			; Transfer backwards: start at block end

.calc_address:
	cmp dl, 3
	jbe .setup_channel
	shr ecx, 1			; Count must be in words for 16-bit
	mov esi, ebx			; WTF addressing for 16-bit channels
	shr esi, 1
	mov bx, si			; Offset is in words, but page in bytes!

.setup_channel:
	dec ecx				; 1 byte/word less for DMA count
	movzx esi, dl			; ESI: channel for register lookup table
	mov al, dl
	and al, 0x03
	and dh, 0xfc
	or al, dh
	mov edi, eax			; EDI: DMA mode + channel (low byte)

	pushf				; Disable interrupts
	cli

	; Disable channel before reprogramming the DMA controller

	and al, 0x03
	or al, 0x04
	movzx edx, byte [esi * 8 + port(channel_mask)]
	out dx, al

	; Set start address

	movzx edx, byte [esi * 8 + port(flipflop_reset)]
	mov al, 0xff
	out dx, al			; Reset flip-flop
	movzx edx, byte [esi * 8 + port(start_address)]
	mov al, bl
	out dx, al			; Low byte
	mov al, bh
	out dx, al			; High byte
	movzx edx, byte [esi * 8 + port(page)]
	shr ebx, 16
	mov al, bl
	out dx, al			; Page

	; Set mode

	movzx edx, byte [esi * 8 + port(mode)]
	mov eax, edi
	out dx, al

	; Set count

	movzx edx, byte [esi * 8 + port(flipflop_reset)]
	mov al, 0xff
	out dx, al			; Reset flip-flop again
	movzx edx, byte [esi * 8 + port(count)]
	mov al, cl
	out dx, al			; Low byte
	mov al, ch
	out dx, al			; High byte

	; Enable channel

	mov eax, edi
	and al, 0x03
	movzx edx, byte [esi * 8 + port(channel_mask)]
	out dx, al

	popf				; Restore interrupt state

	pop ds
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	pop eax
	retf


;------------------------------------------------------------------------------
; Stop DMA transfer (also disables channel).
;------------------------------------------------------------------------------
; -> DL - DMA channel number
; <- CF - Set if error
;    EAX - Error code if CF set
;------------------------------------------------------------------------------

	[bits 32]
	align 4

global dma_stop
dma_stop:
	cmp dl, 7
	jae dma_error

	push eax
	push esi
	push edx
	push ds

	mov ds, cs:[sel_data_krnl32]

	xor eax, eax			; Clear running DMA transfer flag
	mov al, dl
	btr [dma_running], eax

	movzx esi, dl			; ESI: channel for register lookup table
	mov al, dl
	and al, 0x03
	or al, 0x04
	movzx edx, byte [esi * 8 + port(channel_mask)]
	out dx, al

	clc

	pop ds
	pop edx
	pop esi
	pop eax
	retf

dma_error:
	mov eax, PMI_E_INV_DMA
	stc
	retf


;------------------------------------------------------------------------------
; Setup ISA DMA services.
;------------------------------------------------------------------------------
; -> DS - Flat data selector
;------------------------------------------------------------------------------

	[bits 32]

global dma_setup
dma_setup:
	push eax

	push ds
	mov ds, cs:[sel_data_krnl32]
	mov eax, cs
	mov dword [PMI_FN(dma_start)], dma_start
	mov dword [PMI_FN(dma_start) + 4], eax
	mov dword [PMI_FN(dma_stop)], dma_stop
	mov dword [PMI_FN(dma_stop) + 4], eax
	pop ds

	pop eax
	retn


;------------------------------------------------------------------------------
; Shutdown ISA DMA services.
;------------------------------------------------------------------------------
; -> DS - PMI data selector
;------------------------------------------------------------------------------

	[bits 16]

global dma_shutdown
dma_shutdown:
	push ax
	push bx
	push dx
	push esi

	xor dx, dx			; DX: DMA channel port address
	mov bx, -1			; BX: DMA channel (pre-increment)

	; Stop running auto-init DMA transfers

.stop_dma_loop:
	inc bx
	cmp bl, 7
	jae .done			; All 8 DMA channels checked, done
	bt [dma_running], bx		; Auto-init DMA running on channel?
	jnc .stop_dma_loop		; No, check next
	movzx esi, bl			; ESI: channel for register lookup table
	mov al, bl
	and al, 0x03
	or al, 0x04
	mov dl, [esi * 8 + port(channel_mask)]
	out dx, al			; Mask DMA channel
	jmp .stop_dma_loop		; Check next

.done:
	pop esi
	pop dx
	pop bx
	pop ax
	retn


;==============================================================================
; Data area
;==============================================================================

dma_running	db 0			; Running DMA channels with PMI_DMA_AUTO

dma_registers:	; DMA 0 (unusable)
		istruc dma_ports
		at dma_ports.page, db 0x87
		at dma_ports.start_address, db 0x00
		at dma_ports.count, db 0x01
		at dma_ports.channel_mask, db 0x0a
		at dma_ports.mode, db 0x0b
		at dma_ports.flipflop_reset, db 0x0c
		iend

		; DMA 1
		istruc dma_ports
		at dma_ports.page, db 0x83
		at dma_ports.start_address, db 0x02
		at dma_ports.count, db 0x03
		at dma_ports.channel_mask, db 0x0a
		at dma_ports.mode, db 0x0b
		at dma_ports.flipflop_reset, db 0x0c
		iend

		; DMA 2
		istruc dma_ports
		at dma_ports.page, db 0x81
		at dma_ports.start_address, db 0x04
		at dma_ports.count, db 0x05
		at dma_ports.channel_mask, db 0x0a
		at dma_ports.mode, db 0x0b
		at dma_ports.flipflop_reset, db 0x0c
		iend

		; DMA 3
		istruc dma_ports
		at dma_ports.page, db 0x82
		at dma_ports.start_address, db 0x07
		at dma_ports.count, db 0x08
		at dma_ports.channel_mask, db 0x0a
		at dma_ports.mode, db 0x0b
		at dma_ports.flipflop_reset, db 0x0c
		iend

		; DMA 4 (unusable)
		istruc dma_ports
		at dma_ports.page, db 0x8f
		at dma_ports.start_address, db 0xc0
		at dma_ports.count, db 0xc2
		at dma_ports.channel_mask, db 0xd4
		at dma_ports.mode, db 0xd6
		at dma_ports.flipflop_reset, db 0xd8
		iend

		; DMA 5
		istruc dma_ports
		at dma_ports.page, db 0x8b
		at dma_ports.start_address, db 0xc4
		at dma_ports.count, db 0xc6
		at dma_ports.channel_mask, db 0xd4
		at dma_ports.mode, db 0xd6
		at dma_ports.flipflop_reset, db 0xd8
		iend

		; DMA 6
		istruc dma_ports
		at dma_ports.page, db 0x89
		at dma_ports.start_address, db 0xc8
		at dma_ports.count, db 0xca
		at dma_ports.channel_mask, db 0xd4
		at dma_ports.mode, db 0xd6
		at dma_ports.flipflop_reset, db 0xd8
		iend

		; DMA 7
		istruc dma_ports
		at dma_ports.page, db 0x8a
		at dma_ports.start_address, db 0xcc
		at dma_ports.count, db 0xce
		at dma_ports.channel_mask, db 0xd4
		at dma_ports.mode, db 0xd6
		at dma_ports.flipflop_reset, db 0xd8
		iend
