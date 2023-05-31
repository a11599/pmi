;==============================================================================
; Protected mode interface - Memory management
;==============================================================================

	cpu 386

	group pmi code

segment code public use16 class=CODE align=16
segment code

%include "pmi/config.inc"
%include "pmi/api/pmi.inc"
%include "pmi/api/kernel.inc"
%include "pmi/consts/kernel.inc"
%include "pmi/consts/memory.inc"
%include "pmi/structs/memory.inc"

%define PMI_FN(fn) pmi_fn + pmi_fns. %+ fn


;------------------------------------------------------------------------------
; Allocate a paragraph-aligned memory block.
;------------------------------------------------------------------------------
; -> AL - Allocation mode (PMI_MEM_*)
;    ECX - Size of memory block in bytes
;    DS - Flat data selector
; <- CF - Set if error
;    EAX - Linear address of allocated memory or error code if CF is set
;    EBX - Physical address of memory block when allocating for DMA
;------------------------------------------------------------------------------

	[bits 32]
	align 4

global mem_alloc
mem_alloc:
	push ecx
	push edx
	push ebx

	mov dh, al			; DH: allocation mode
	cmp al, PMI_MEM_LO_HI
	jbe .alloc_cmb			; PMI_MEM_LO or PMI_MEM_LO_HI modes
	cmp al, PMI_MEM_HI_LO
	jbe .alloc_xmb			; PMI_MEM_HI or PMI_MEM_HI_LO modes

	; PMI_MEM_DMA: allocate memory block for ISA DMA. Always allocate
	; conventional memory under DPMI and hope the host has good DMA
	; virtualization...

	mov dl, MEM_ALLOC_DMA
	cmp byte cs:[pm_host_type], PMI_HOST_DPMI
	je .alloc_cmb_dma		; Allocate conventional mem under DPMI
	cmp al, PMI_MEM_DMA_LO
	je .alloc_cmb_dma		; Allocate conventional memory only
	mov ebx, cs:[xmb_base]		; Try extended memory first
	call allocate_memory_block
	jnc .exit_dma

.alloc_cmb_dma:
	mov ebx, cs:[cmb_base]		; Try conventional memory next
	call allocate_memory_block
	jc .error

.exit_dma:
	mov [esp], ebx			; Set EBX on stack
	jmp .exit

.alloc_cmb:

	; PMI_MEM_LO or PMI_MEM_LO_HI: allocate conventional memory block

	mov ebx, cs:[cmb_base]
	mov dl, MEM_ALLOC_BTM
	call allocate_memory_block
	jnc .exit
	cmp dh, PMI_MEM_LO_HI
	jne .error

.alloc_xmb:

	; Allocate extended memory block

	mov ebx, cs:[xmb_base]
	mov dl, MEM_ALLOC_TOP
	call allocate_memory_block
	jnc .exit
	cmp dh, PMI_MEM_HI_LO
	je .alloc_cmb			; Attempt conventional memory allocation

.no_mem:
	mov eax, PMI_E_MEM_LOW

.error:
	stc

.exit:
	pop ebx
	pop edx
	pop ecx
	retf


;------------------------------------------------------------------------------
; Release a previously allocated memory block.
;------------------------------------------------------------------------------
; -> EAX - Linear address of allocated memory
;    DS - Flat data selector
; <- CF - Set if error
;    EAX - Error code if CF is set
;------------------------------------------------------------------------------

	[bits 32]
	align 4

global mem_free
mem_free:
	push ebx
	push ecx
	push edx
	push eax

	lea ebx, [eax - mcb.strucsize]	; EBX: MCB address

	; Free memory block

	mov eax, PMI_E_MEM_INVL
	cmp word [ebx + mcb.signature], MCB_SIGNATURE
	jne .error			; MCB chain destroyed
	test word [ebx + mcb.status], MCB_USED
	jz .done			; Block already free
	and word [ebx + mcb.status], ~MCB_USED

	; Merge with neighbour blocks when possible

	mov ecx, [ebx + mcb.next]
	call mem_merge_blocks
	jc .error
	mov ecx, eax
	mov ebx, [eax + mcb.prev]
	call mem_merge_blocks
	jc .error

.done:
	pop eax
	clc

.exit:
	pop edx
	pop ecx
	pop ebx
	retf

.error:
	add sp, 4			; Discard EAX from stack
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Initialize memory management. Call this after conventional and extended
; memory areas are allocated by the kernel.
;------------------------------------------------------------------------------
; -> DS - Flat data selector
;------------------------------------------------------------------------------

	[bits 32]

global mem_setup
mem_setup:
	push eax
	push ebx
	push ecx

	push ds
	mov ds, cs:[sel_data_krnl32]
	mov eax, cs
	mov dword [PMI_FN(mem_alloc)], mem_alloc
	mov dword [PMI_FN(mem_alloc) + 4], eax
	mov dword [PMI_FN(mem_free)], mem_free
	mov dword [PMI_FN(mem_free) + 4], eax
	pop ds

	mov ebx, cs:[cmb_base]		; Initialize conventional memory
	mov ecx, cs:[cmb_size]
	test ebx, ebx
	jz .init_xmb
	call .init_block

.init_xmb:
	mov ebx, cs:[xmb_base]		; Initialize extended memory
	mov ecx, cs:[xmb_size]
	test ebx, ebx
	jz .done
	call .init_block

.done:
	pop ecx
	pop ebx
	pop eax

	retn

; Initialize memory block area.
; -> EBX - Linear address of paragraph-aligned memory area
;    ECX - Paragraph-aligned size of memory area in bytes
;    DS - Flat data selector

.init_block:
	mov word [ebx + mcb.signature], MCB_SIGNATURE
	mov word [ebx + mcb.status], 0
	sub ecx, mcb.strucsize		; Decrease by MCB size
	mov dword [ebx + mcb.size], ecx
	mov dword [ebx + mcb.prev], -1
	mov dword [ebx + mcb.next], -1

	retn


;------------------------------------------------------------------------------
; Allocate a paragraph-aligned memory block including a specific linear address
; range when possible.
;------------------------------------------------------------------------------
; -> EDX - Desired linear address of memory block
;    ECX - Size of memory block in bytes
;    DS - Flat data selector
; <- CF - Set if error, PMI_E_MEM_BLOCK when address range unavailable
;    EAX - Linear address of allocated memory or error code if CF is set
;------------------------------------------------------------------------------
; The actual allocated linear address may be different if the memory block
; cannot be split reasonably (the new blocks would be too small). The caller
; must use the actual linear address to free the block later. However, the
; desired address range is guaranteed to be available for the caller within the
; allocated memory block if the call is successful.
;------------------------------------------------------------------------------

	[bits 32]
	align 4

global mem_alloc_addr
mem_alloc_addr:
	push ebx
	push ecx
	push edx
	push esi

	mov ah, dl			; Align to paragraphs
	and dl, 0xf0			; EDX: bottom of wanted address range
	and ah, 0xf
	mov al, 0x10
	sub al, ah
	and eax, 0xf
	add ecx, eax
	lea ecx, [ecx + edx + 0xf]
	and cl, 0xf0			; ECX: top of wanted address range

	cmp edx, 0x100000		; Conventional or extended memory?
	mov ebx, cs:[xmb_base]
	jae .check_block_loop
	mov ebx, cs:[cmb_base]

.check_block_loop:
	cmp word [ebx + mcb.signature], MCB_SIGNATURE
	mov eax, PMI_E_MEM_INVL
	jne .error			; MCB chain corrupted
	test word [ebx + mcb.status], MCB_USED
	jnz .next_block			; Block already in use
	lea eax, [ebx + mcb.strucsize]	; EAX: available block start address
	cmp eax, edx
	jae .no_block			; Block above wanted linear address
	mov esi, [ebx + mcb.size]
	add esi, eax			; ESI: available block end address
	cmp esi, edx
	jbe .next_block			; Block below wanted address range
	cmp esi, ecx
	jae .split_block_lower		; Block found, split
	jmp .no_block			; Range interrupted by used block

.next_block:

	; Check next block

	mov ebx, [ebx + mcb.next]	; Next MCB
	cmp ebx, -1
	jne .check_block_loop
	jmp .no_block			; End of memory blocks, nothing found

.split_block_lower:
	mov esi, ecx			; ESI: wanted memory range top
	mov ecx, edx
	sub ecx, eax			; ECX: memory below wanted range
	sub ecx, mcb.strucsize		; Make room for new MCB
	jbe .split_block_upper		; Too small, don't split
	mov dh, MEM_SPLIT_NOR
	call mem_split_block		; Split lower part of memory block
	cmp eax, -1			; New block unreasonably small
	je .split_block_upper
	mov ebx, eax			; Use upper part of split block

.split_block_upper:
	mov ecx, esi
	sub ecx, ebx
	sub ecx, mcb.strucsize		; ECX: wanted range size from block
	mov dh, MEM_SPLIT_NOR
	call mem_split_block

	mov word [ebx + mcb.status], MCB_USED
	lea eax, [ebx + mcb.strucsize]	; EAX: linear address of allocated block
	clc

.exit:
	pop esi
	pop edx
	pop ecx
	pop ebx
	retn

.no_block:
	mov eax, PMI_E_MEM_BLOCK

.error:
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Allocate memory block in a specific memory block area.
;------------------------------------------------------------------------------
; -> EBX - Pointer to memory block area
;    ECX - Size of memory area to allocate in bytes (max. 64 KB for DMA)
;    DL - Memory allocation control value (MEM_ALLOC_*)
;    DS - Flat data selector
; <- CF - Set if allocation failed
;    EAX - Linear address of memory block reserved or error code if CF set
;    EBX - Physical address of memory block when allocating for DMA
;------------------------------------------------------------------------------

	[bits 32]
	align 4

allocate_memory_block:
	push ecx
	push edx
	push esi
	push ebp
	push ebx

	mov eax, PMI_E_MEM_LOW		; Empty memory block
	test ebx, ebx
	jz .error

	add ecx, 15
	and cl, 0xf0			; Align size to paragraph

	;----------------------------------------------------------------------
	; Find a memory block of suitable size
	; - MEM_ALLOC_BTM: Bottom-up, allocate starting from beginning of memory
	;   area
	; - MEM_ALLOC_TOP: Top-down, allocate starting from top of memory area
	; - MEM_ALLOC_DMA: Allocate block below 16 MB, not crossing DMA page
	;   boundary (64 KB)

	xor ebp, ebp			; EBP: address of matching block
	cmp dl, MEM_ALLOC_TOP		; Top-down: EBP = 0
	je .check_block
	not ebp				; Bottom-up/DMA: EBP = 0xffffff

.check_block:

	; Check if the memory block is large enough and available

	cmp word [ebx + mcb.signature], MCB_SIGNATURE
	mov eax, PMI_E_MEM_INVL
	jne .error			; MCB chain corrupted
	test word [ebx + mcb.status], MCB_USED
	jnz .next_block			; Block already in use
	cmp [ebx + mcb.size], ecx
	jb .next_block			; Block too small

	; Check allocation control

	cmp dl, MEM_ALLOC_DMA
	je .check_dma			; Allocating for DMA; special checks
	cmp dl, MEM_ALLOC_TOP
	je .top_down

	cmp ebx, ebp			; Bottom-up allocation
	ja .next_block			; Block address higher than found, skip
	jmp .save_block

.top_down:
	cmp ebx, ebp			; Top-down allocation
	jb .next_block			; Block address lower, than found, skip

.save_block:
	mov ebp, ebx			; Save possible block address

.next_block:

	; Check next block

	mov ebx, [ebx + mcb.next]	; Next MCB
	cmp ebx, -1
	jne .check_block

	; No more blocks, did we found something?

	mov eax, PMI_E_MEM_LOW
	test ebp, ebp			; Top-down: no block found
	jz .error
	cmp ebp, 0xffffffff		; Bottom-up/DMA: no block found
	je .error
	mov ebx, ebp

	; Yes, bottom-up allocation: split and use lower part of the block

	cmp dl, MEM_ALLOC_BTM
	je .split_lower

	; Top-down allocation: split and use upper part of the block

	lea eax, [ecx + mcb.strucsize]	; EAX: memory required for upper part
	cmp eax, [ebx + mcb.size]
	jae .allocate_current_block	; Block fits perfectly, don't split
	mov eax, [ebx + mcb.size]
	sub eax, ecx
	sub eax, mcb.strucsize
	mov ecx, eax			; ECX: size of lower split part
	cmp ecx, MIN_BLOCK_SIZE
	jb .allocate_current_block	; New lower part too small, allocate all
	mov dh, MEM_SPLIT_FORCE
	call mem_split_block		; Split block
	cmp eax, -1
	je .allocate_current_block	; No split, allocate current block
	jmp .allocate_block		; Split, allocate upper block

.check_dma:

	;----------------------------------------------------------------------
	; Check if memory block is suitable for DMA
	; - Must be below 16 MB
	; - Cannot cross 64 KB boundary (DMA controller page)
	; - Use first block which fits

	cmp ebx, DMA_MAX_ADDRESS
	jae .next_block			; Block not DMA addressable

	test word cs:[env_flags], ENV_MEM_PAGING
	jz .check_dma_linear

	; Paging enabled, check physical address for DMA compliance

	call find_dma_block
	cmp eax, -1
	je .next_block
	mov [esp], esi			; Update EBX on stack
	lea esi, [ebx + mcb.strucsize]
	cmp esi, eax			; DMA block at start of memory block
	je .split_lower
	jmp .split_page

.check_dma_linear:

	; Check if DMA page would be crossed

	lea eax, [ebx + mcb.strucsize]
	and eax, 0xffff0000
	add eax, 0x00010000		; EAX: start of next DMA page
	lea esi, [ebx + ecx + mcb.strucsize]
	cmp esi, eax			; ESI: end address of allocation
	jbe .split_lower		; DMA addressable, block found

	; DMA page crossed

	lea esi, [eax + ecx]		; ESI: end address of page-aligned block
	cmp esi, DMA_MAX_ADDRESS
	jae .next_block			; Block at next page non-DMA addressable

	; Check if block is large enough for DMA page-aligned allocation

	sub esi, mcb.strucsize
	sub esi, ebx			; ESI: minimum block size required
	cmp esi, [ebx + mcb.size]
	ja .next_block			; Block too small

.split_page:

	; Force split block to create a page-aligned upper block. The lower
	; block is used as padding for page-alignment and the upper part is
	; split further as necessary.

	push ecx
	mov ecx, eax
	sub ecx, ebx
	sub ecx, mcb.strucsize * 2	; ECX: size of lower (padding) block
	mov dh, MEM_SPLIT_FORCE
	call mem_split_block
	mov ebx, eax			; Use the page-aligned upper block
	pop ecx				; ECX: restore wanted block size

.split_lower:

	; Split block and use lower part for allocation

	mov dh, MEM_SPLIT_NOR
	call mem_split_block

.allocate_current_block:
	mov eax, ebx

.allocate_block:

	; Mark split block as used an return its address

	mov word [eax + mcb.status], MCB_USED
	add eax, mcb.strucsize		; Skip MCB

	cmp dl, MEM_ALLOC_DMA		; Physical address = linear address for
	jne .done			; non-paged DMA environments
	test word cs:[env_flags], ENV_MEM_PAGING
	jnz .done
	mov [esp], eax			; Update EBX on stack

.done:
	clc

.exit:
	pop ebx
	pop ebp
	pop esi
	pop edx
	pop ecx
	retn

.error:
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Find linear address range within memory block that maps to DMA-addressable
; physical memory.
;------------------------------------------------------------------------------
; -> EBX - MCB of memory block
;    ECX - Wanted DMA memory block size (max. 64 KB)
; <- EAX - Base of available address range or -1 if no suitable range was found
;    ESI - Physical address of DMA block or -1 if no suitable range was found
;------------------------------------------------------------------------------

find_dma_block:
	push edx
	push edi
	push ebp

	push ebx
	push ecx

	lea eax, [ebx + mcb.strucsize]
	mov ebp, [ebx + mcb.size]	; EBP: size of memory block
	mov esi, cs:[page_mem_addr]
	add esi, 0x1000			; ESI: pointer to first page table entry

	lea ebx, [eax + 0xfff]
	and bx, 0xf000
	sub ebx, eax			; EBX: number of bytes in first page
	shr eax, 10
	and al, 0xfc			; EAX: index into page table
	add esi, eax			; ESI: pointer to block's first PTE
	mov edi, ecx			; EDI: bytes still needed for block
	mov edx, [esi]
	and dx, 0xf000			; EDX: previous block physical address

.check_block:
	mov ecx, [esp]			; Restore wanted block size from stack
	cmp edx, DMA_MAX_ADDRESS
	jae .no_block			; Above DMA-suitable address space
	movzx eax, dx			; Check if block crosses DMA page
	add eax, ecx
	test eax, 0xffff0000
	jnz .next_page			; It does, find next page
	mov ecx, ebp			; ECX: possible DMA block offset

.check_continuity_loop:
	sub edi, ebx
	jle .found_block		; Contiguous block of pages found
	sub ebp, ebx
	jc .no_block			; End of block
	add esi, 4			; Next page table entry
	mov ebx, 0x1000			; Advance by 4K from now on
	mov eax, [esi]			; Check if pages are continuous
	and ax, 0xf000
	sub eax, edx
	add edx, eax
	cmp eax, 0x1000
	je .check_continuity_loop	; Check further continuity
	jmp .check_block

.next_page:
	lea eax, [edx + 0xffff]
	xor ax, ax			; EAX: physical address of next DMA page

.next_page_loop:
	sub ebp, ebx
	jc .no_block			; End of memory block
	add esi, 4			; Next page table entry
	mov ebx, 0x1000			; Advance by 4K from now on
	cmp [esi], eax
	jb .next_page_loop
	mov edx, [esi]
	and dx, 0xf000			; EDX: start of new possible DMA block
	mov edi, ecx			; EDI: size of wanted memory block
	jmp .check_block

.found_block:
	mov ebp, ecx			; EBP: bytes left after start of block

	pop ecx
	pop ebx

	cmp ebp, ecx			; Overrun can happen at the end of the
	jb .block_too_small		; block, check here to make sure

	mov eax, [ebx + mcb.size]
	sub eax, ebp			; EAX: offset of DMA block
	add eax, mcb.strucsize
	add eax, ebx			; EAX: linear address of DMA block
	mov edi, eax
	and di, 0xfff			; EDI: offset of DMA block within page
	mov edx, eax
	shr edx, 10
	and dl, 0xfc			; EDX: index into page table entries
	add edx, cs:[page_mem_addr]
	add edx, 0x1000
	mov esi, [edx]
	and si, 0xf000
	or esi, edi			; ESI: physical address of DMA block

.done:
	pop ebp
	pop edi
	pop edx
	retn

.no_block:
	pop ecx
	pop ebx

.block_too_small:
	mov eax, -1
	mov esi, eax
	jmp .done


;------------------------------------------------------------------------------
; Split memory block. The upper part of the block will be marked as free.
;------------------------------------------------------------------------------
; -> EBX - Address of memory block MCB to split
;    ECX - Size of lower split area in bytes, must be smaller than the size of
;          memory block being split. When forcing split, the block must
;          also have enough space for the new MCB.
;    DH - Memory split control value (MEM_SPLIT_*)
;    DS - Flat data selector
; <- EAX - Linear address of new upper memory block MCB or -1 if split was not
;          done since the new block would have been too small (not with
;          MEM_SPLIT_FORCE)
;------------------------------------------------------------------------------

	[bits 32]
	align 4

global mem_split_block
mem_split_block:
	push esi

	mov eax, [ebx + mcb.size]
	sub eax, ecx			; EAX: size of new upper block

	cmp dh, MEM_SPLIT_FORCE
	je .split			; Force memory block split
	cmp eax, MIN_BLOCK_SIZE + mcb.strucsize
	jae .split			; New block size reasonable, split

	mov eax, -1			; New block too small, don't split
	jmp .exit

.split:

	; EBX: B1 = lower block (block being split)
	; ESI: B2 = new upper block
	; EAX: B3 = next block

	mov [ebx + mcb.size], ecx	; B1.size = new lower block size

	; Create new upper block

	lea esi, [ebx + ecx + mcb.strucsize]
	sub eax, mcb.strucsize		; EAX: size of new upper block

	mov word [esi + mcb.signature], MCB_SIGNATURE
	mov word [esi + mcb.status], 0
	mov [esi + mcb.size], eax
	mov [esi + mcb.prev], ebx	; B2.prev = B1
	mov eax, [ebx + mcb.next]
	mov [esi + mcb.next], eax	; B2.next = B3

	; Adjust block pointers

	mov [ebx + mcb.next], esi	; B1.next = B2
	mov [eax + mcb.prev], esi	; B3.prev = B2

	mov eax, esi			; Return new upper block MCB

.exit:
	pop esi
	retn


;------------------------------------------------------------------------------
; Merge memory blocks (when possible).
;------------------------------------------------------------------------------
; -> EBX - Address of first memory block MCB to merge or -1
;    ECX - Address of second memory block MCB to merge or -1
;    DS - Flat data selector
; <- CF - Set if error
;    EAX - Error code if CF set or linear address of merged memory block
;------------------------------------------------------------------------------

	[bits 32]
	align 4

global mem_merge_blocks
mem_merge_blocks:
	push edx

	; Both EBX and ECX must point to valid MCBs and have the same status
	; for merge

	cmp ebx, -1
	je .done
	cmp ecx, -1
	je .done
	mov dx, [ebx + mcb.status]
	cmp [ecx + mcb.status], dx
	jne .done
	mov eax, PMI_E_MEM_INVL		; EAX: MCB chain destroyed error code
	cmp word [ebx + mcb.signature], MCB_SIGNATURE
	jne .error
	cmp word [ecx + mcb.signature], MCB_SIGNATURE
	jne .error

	; Check if blocks are really neighbours

	mov edx, ecx
	sub edx, [ebx + mcb.size]
	sub edx, mcb.strucsize
	cmp edx, ebx
	jne .done

	; Merge blocks

	mov edx, [ebx + mcb.size]
	add edx, [ecx + mcb.size]
	add edx, mcb.strucsize		; EDX: size of merged block
	mov [ebx + mcb.size], edx

	; EBX: B1 - first block
	; ECX: B2 - second block
	; EDX: B3 - next block or -1

	mov edx, [ecx + mcb.next]
	mov [ebx + mcb.next], edx	; B1.next = B3
	cmp edx, -1
	je .done			; No B3 (next) block
	cmp word [edx + mcb.signature], MCB_SIGNATURE
	jne .error			; Invalid B3 (next) block
	mov [edx + mcb.prev], ebx	; B3.prev = B1

.done:
	mov eax, ebx
	clc

.exit:
	pop edx
	retn

.error:
	stc
	jmp .exit

term:
jmp PMI_FN(terminate)
