;==============================================================================
; Protected mode interface - File operations
;==============================================================================

	cpu 386

	group pmi code

segment code public use16 class=CODE align=16
segment code

%include "pmi/config.inc"
%include "pmi/api/pmi.inc"
%include "pmi/api/kernel.inc"
%include "pmi/api/memory.inc"

%define PMI_FN(fn) pmi_fn + pmi_fns. %+ fn
%if (IO_BUF_SIZE > 0xfff0)
%define FILE_BUF_SIZE 0xfff0
%else
%define FILE_BUF_SIZE IO_BUF_SIZE
%endif


;------------------------------------------------------------------------------
; Open file.
;------------------------------------------------------------------------------
; -> AL - File access mode (PMI_FILE_*)
;    EBX - Linear address of ASCIIZ file name
;    DS - Flat data segment
; <- CF - Set if error
;    EAX - PMI or MS-DOS error code if CF set or file handle if no error
;------------------------------------------------------------------------------

	[bits 32]
	align 4

global file_open
file_open:
	push ebx
	push ecx
	push edx
	push esi

	cmp ebx, 0x100000
	jae .copy_name
	mov edx, ebx
	and edx, 0xf
	shr ebx, 4			; BX:DX: address of file name
	jmp .open

.copy_name:
	mov dl, al			; DL: file access mode
	push cs
	call file_get_io_buffer
	jc .error
	xor esi, esi			; ESI: character index

.copy_name_loop:
	mov dh, [ebx + esi]
	mov [eax + esi], dh
	test dh, dh
	jz .use_buffer_ptr
	inc esi
	dec ecx
	jnz .copy_name_loop
	mov ax, 2
	jmp .error			; File name too long for buffer

.use_buffer_ptr:
	mov ebx, eax
	shr ebx, 4
	xchg eax, edx			; AL: file access mode, EDX: IO buf addr
	and edx, 0xf

.open:
	sub esp, pmi_rm_call.strucsize
	mov byte [esp + pmi_rm_call.flags], PMI_CALL_INT
	mov byte [esp + pmi_rm_call.int], 0x21
	mov [esp + pmi_rm_call.ds], bx
	cmp word cs:[dos_ver], 0x0400
	jb .old_dos

	mov ah, 0x6c			; DOS 4.0+, extended open/create
	mov esi, edx			; SI: file name offset
	mov ebx, eax
	and ebx, 0x03			; Read/write access mode
	mov bl, cs:[file_mode_tab + ebx]
	or ebx, 0x2000			; Disable critical error handling
	test al, PMI_FILE_COMMIT
	setnz dl
	shl edx, 14			; DX: 0x4000 (PMI_FILE_COMMIT) or 0x00
	or ebx, edx			; Disable buffers if autocommit
	mov edx, 1			; 0x0001: Open existing file only
	test al, PMI_FILE_CREATE
	setnz cl
	shl cl, 4			; CL: 0x10 (PMI_FILE_CREATE) or 0x00
	or dl, cl			; 0x0011: Open existing or create new
	test al, PMI_FILE_TRUNC
	setnz cl			; CL: 0x01 (PMI_FILE_TRUNC) or 0x00
	add dl, cl			; 0x0012: Overwrite existing or create
	mov ecx, 0x20			; Set default file attribute
	call far [cs:PMI_FN(call_rm)]
	lea esp, [esp + pmi_rm_call.strucsize]
	jnc .done
	jmp .error

.old_dos:
	mov cl, al			; CL: File access mode
	and eax, 0x03
	mov al, cs:[file_mode_tab + eax]
	test cl, PMI_FILE_TRUNC		; Pre-DOS 4.0
	jnz .old_dos_create
	mov ah, 0x3d			; DOS: open file
	call far [cs:PMI_FN(call_rm)]
	jnc .old_dos_done
	cmp ax, 0x02
	jne .old_dos_fail		; Error other than file not found
	test cl, PMI_FILE_CREATE
	jnz .old_dos_create		; Create file when not found

.old_dos_fail:
	lea esp, [esp + pmi_rm_call.strucsize]
	stc
	jmp .error

.old_dos_create:
	mov ah, 0x3c			; DOS: create file
	mov ecx, 0x20			; Set default file attribute
	call far [cs:PMI_FN(call_rm)]

.old_dos_done:
	lea esp, [esp + pmi_rm_call.strucsize]
	jc .error

.done:
	movzx eax, ax			; EAX: file handle

.exit:
	pop esi
	pop edx
	pop ecx
	pop ebx
	retf

.error:
	movsx eax, ax			; EAX: error code
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Close file.
;------------------------------------------------------------------------------
; -> EBX - File handle
; <- CF - Set if error
;    EAX - MS-DOS error code if CF set
;------------------------------------------------------------------------------

	[bits 32]
	align 4

global file_close
file_close:
	push eax

	sub esp, pmi_rm_call.strucsize
	mov byte [esp + pmi_rm_call.flags], PMI_CALL_INT
	mov byte [esp + pmi_rm_call.int], 0x21
	mov ah, 0x3e			; DOS: close file
	call far [cs:PMI_FN(call_rm)]
	lea esp, [esp + pmi_rm_call.strucsize]
	jc .error

	pop eax

.exit:
	retf

.error:
	add esp, 4			; Discard EAX from stack
	movsx eax, ax
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Read from file.
;------------------------------------------------------------------------------
; -> EBX - File handle
;    ECX - Number of bytes to read from file
;    EDI - Linear address of buffer to receive data
;    DS - Flat data segment
;    ES - Flat data segment
; <- CF - Set if error
;    EAX - PMI or MS-DOS error code if CF set or number of bytes read
;------------------------------------------------------------------------------

	[bits 32]
	align 4

global file_read
file_read:
	push ecx
	push edx
	push esi
	push edi
	push ebp

	cld

	xor eax, eax			; Zero high word
	xor edx, edx			; EDX: number of bytes read
	mov ebp, ecx			; EBP: number of bytes to read

.loop_read:
	cmp edi, 0x100000
	jb .direct_read
	push cs				; Setup file I/O buffer
	call file_get_io_buffer
	jc .error

.loop_read_xmb:

	; Read chunk into conventional memory IO buffer

	mov ecx, ebp			; ECX: number of bytes to read
	cmp ecx, FILE_BUF_SIZE
	jbe .read_to_buffer
	mov ecx, FILE_BUF_SIZE

.read_to_buffer:
	push edx
	sub esp, pmi_rm_call.strucsize
	mov byte [esp + pmi_rm_call.flags], PMI_CALL_INT
	mov byte [esp + pmi_rm_call.int], 0x21
	mov eax, cs:[io_buf_addr]
	shr eax, 4			; EAX: high word clear, I/O buf segment
	mov [esp + pmi_rm_call.ds], ax	; I/O buffer segment
	xor edx, edx			; EDX: I/O buffer offset
	mov ah, 0x3f			; DOS: read chunk from file
	call far [cs:PMI_FN(call_rm)]
	lea esp, [esp + pmi_rm_call.strucsize]
	pop edx
	jc .error			; Operation failed
	add edx, eax			; Increase number of bytes read
	cmp eax, ecx
	jne .eof_buffer			; End of file
	sub ebp, eax			; Decrease number of bytes to read
	jmp .copy_from_buffer

.eof_buffer:
	xor ebp, ebp			; EOF, copy data to target, then return

.copy_from_buffer:

	; Copy from conventional memory IO buffer to target memory address, use
	; doubleword copy to speed things up, then copy rest of remaining bytes

	mov esi, cs:[io_buf_addr]	; ESI: IO buffer linear address
	mov ecx, eax
	shr ecx, 2			; ECX: number of bytes read / 4
	jz .copy_bytes
	rep movsd

.copy_bytes:
	mov ecx, eax
	and ecx, 0x3			; ECX: number of bytes left (0 - 3)
	jz .next_chunk_to_buffer
	rep movsb

.next_chunk_to_buffer:
	test ebp, ebp			; Read next chunk
	jz .done
	jmp .loop_read_xmb

.direct_read:

	; Read directly into target buffer if it is in conventional memory

	push edx
	sub esp, pmi_rm_call.strucsize
	mov byte [esp + pmi_rm_call.flags], PMI_CALL_INT
	mov byte [esp + pmi_rm_call.int], 0x21
	mov edx, edi
	shr edx, 4
	mov [esp + pmi_rm_call.ds], dx	; Target address segment
	mov edx, edi
	and edx, 0xf			; DX: target address offset
	mov ecx, ebp			; ECX: number of bytes to read
	cmp ecx, 0xfff0
	jbe .read_to_target
	mov ecx, 0xfff0

.read_to_target:
	mov ah, 0x3f			; DOS: read chunk from file
	call far [cs:PMI_FN(call_rm)]
	lea esp, [esp + pmi_rm_call.strucsize]
	pop edx
	jc .exit
	add edx, eax			; Increase number of bytes read
	add edi, eax			; Advance target address
	cmp eax, ecx
	jne .done			; End of file
	sub ebp, eax			; Decrease number of bytes to read
	jnz .loop_read			; Read next chunk, may be in high mem.

.done:
	mov eax, edx			; Return number of bytes read
	clc

.exit:
	pop ebp
	pop edi
	pop esi
	pop edx
	pop ecx
	retf

.error:
	movsx eax, ax
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Write to file.
;------------------------------------------------------------------------------
; -> EBX - File handle, set bit 16 to commit instantly
;    ECX - Number of bytes to write
;    ESI - Linear address of source buffer containing file data
;    DS - Flat data segment
;    ES - Flat data segment
; <- CF - Set if error
;    EAX - Number of bytes written or error code if CF is set
;------------------------------------------------------------------------------

	align 4
	[bits 32]

global file_write
file_write:
	push ecx
	push edx
	push esi
	push edi
	push ebp

	cld

	xor eax, eax			; Zero high word
	xor edx, edx			; EDX: number of bytes written
	mov ebp, ecx			; EBP: number of bytes to write

.loop_write:
	cmp esi, 0x100000
	jb .direct_write
	push cs				; Setup file IO buffer
	call file_get_io_buffer
	jc .error

.loop_write_xmb:
	mov ecx, ebp			; ECX: number of bytes to write
	cmp ecx, FILE_BUF_SIZE
	jbe .copy_to_buffer
	mov ecx, FILE_BUF_SIZE

.copy_to_buffer:

	; Copy from source memory address to conventional memory IO buffer, use
	; doubleword copy to speed things up, then copy rest of remaining bytes

	mov edi, cs:[io_buf_addr]	; EDI: conventional memory IO buffer
	mov eax, ecx
	shr ecx, 2			; ECX: number of bytes to write / 4
	jz .copy_bytes
	rep movsd

.copy_bytes:
	mov ecx, eax
	and ecx, 0x03			; ECX: number of bytes left (0 - 3)
	jz .write_from_buffer
	rep movsb

.write_from_buffer:

	; Write chunk from conventional memory IO buffer

	push edx
	sub esp, pmi_rm_call.strucsize
	mov byte [esp + pmi_rm_call.flags], PMI_CALL_INT
	mov byte [esp + pmi_rm_call.int], 0x21
	mov ecx, eax			; ECX: number of bytes to write
	mov eax, cs:[io_buf_addr]
	shr eax, 4			; EAX: high word clear, I/O buf segment
	mov [esp + pmi_rm_call.ds], ax	; I/O buffer segment
	xor edx, edx			; EDX: I/O buffer offset
	mov ah, 0x40			; DOS: write chunk to file
	call far [cs:PMI_FN(call_rm)]
	lea esp, [esp + pmi_rm_call.strucsize]
	pop edx
	jc .exit			; Operation failed
	add edx, eax			; Increase number of bytes written
	cmp eax, ecx
	jne .done			; Disk full?
	sub ebp, eax			; Decrease number of bytes to write

.next_chunk_from_buffer:
	test ebp, ebp			; Write next chunk
	jz .done
	jmp .loop_write_xmb

.direct_write:

	; Write directly from source buffer if it is in conventional memory

	push edx
	sub esp, pmi_rm_call.strucsize
	mov byte [esp + pmi_rm_call.flags], PMI_CALL_INT
	mov byte [esp + pmi_rm_call.int], 0x21
	mov edx, esi
	shr edx, 4
	mov [esp + pmi_rm_call.ds], dx	; Segment of target address
	mov edx, esi
	and edx, 0x0f			; DX: offset of target address

	mov ecx, ebp			; ECX: number of bytes to write
	cmp ecx, 0xfff0
	jbe .write_from_source
	mov ecx, 0xfff0

.write_from_source:
	mov ah, 0x40			; DOS: write chunk to file
	call far [cs:PMI_FN(call_rm)]
	lea esp, [esp + pmi_rm_call.strucsize]
	pop edx
	jc .exit
	add edx, eax			; Increase number of bytes written
	add esi, eax			; Advance source address
	cmp eax, ecx
	jne .done			; End of file
	sub ebp, eax			; Decrease number of bytes to write
	jnz .loop_write			; Write next chunk, may be in high mem.

.done:
	mov eax, edx			; Return number of bytes written
	clc

.exit:
	pop ebp
	pop edi
	pop esi
	pop edx
	pop ecx
	retf

.error:
	movsx eax, ax
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Set file read/write position (seek).
;------------------------------------------------------------------------------
; -> AL - Seek origin (PMI_SEEK_*)
;    EBX - File handle
;    ECX - New position
; <- CF - Set if error
;    EAX - Error code if CF is set or actual new position
;------------------------------------------------------------------------------

	[bits 32]
	align 4

global file_set_pos
file_set_pos:
	push ecx			; Keep this in sync with
	push edx			; sys_file_get_pos!

file_seek:
	sub esp, pmi_rm_call.strucsize
	mov byte [esp + pmi_rm_call.flags], PMI_CALL_INT
	mov byte [esp + pmi_rm_call.int], 0x21
	mov edx, ecx
	sar ecx, 16			; CX:DX: new position
	mov ah, 0x42			; DOS: seek file
	call far [cs:PMI_FN(call_rm)]
	lea esp, [esp + pmi_rm_call.strucsize]
	movzx eax, ax
	jc .exit

	shl edx, 16
	add eax, edx			; EAX: actual new position

.exit:
	pop edx
	pop ecx
	retf


;------------------------------------------------------------------------------
; Get the current read/write position of a file.
;------------------------------------------------------------------------------
; -> EBX - File handle
; <- CF - Set if error
;    EAX - Error code if CF is set or current position
;------------------------------------------------------------------------------

	[bits 32]
	align 4

global file_get_pos
file_get_pos:
	push ecx			; Keep this in sync with
	push edx			; sys_file_set_pos!

	mov al, PMI_SEEK_CURPOS
	xor ecx, ecx
	jmp file_seek


;------------------------------------------------------------------------------
; Returns the linear address and size of the temporary I/O buffer.
;------------------------------------------------------------------------------
; <- CF - Set if error
;    EAX - Linear address of temporary I/O buffer or error code if CF is set
;    ECX - Size of the buffer
;------------------------------------------------------------------------------

	[bits 32]
	align 4

global file_get_io_buffer
file_get_io_buffer:
	mov eax, cs:[io_buf_addr]
	test eax, eax
	jne .done			; Buffer already allocated

	mov al, PMI_MEM_LO		; Allocate buffer
	mov ecx, IO_BUF_SIZE
	push cs
	call mem_alloc
	jc .exit

	push ds
	mov ds, cs:[sel_data_krnl32]	; Save linear address
	mov [io_buf_addr], eax
	pop ds

.done:
	mov ecx, IO_BUF_SIZE

.exit:
	retf


;------------------------------------------------------------------------------
; Free temporary I/O buffer.
;------------------------------------------------------------------------------
; -> DS - Flat data segment
;------------------------------------------------------------------------------

	[bits 32]
	align 4

global file_free_io_buffer:
file_free_io_buffer:
	push eax

	mov eax, cs:[io_buf_addr]
	test eax, eax
	jz .done
	push cs
	call mem_free			; Free memory block

	push ds
	mov ds, cs:[sel_data_krnl32]
	mov dword [io_buf_addr], 0	; We don't have the buffer anymore
	pop ds

.done:
	pop eax
	retf


;------------------------------------------------------------------------------
; Setup file operations.
;------------------------------------------------------------------------------
; -> DS - Flat data selector
;------------------------------------------------------------------------------

	[bits 32]

global file_setup
file_setup:
	push ds
	mov ds, cs:[sel_data_krnl32]
	mov eax, cs
	mov dword [PMI_FN(file_open)], file_open
	mov dword [PMI_FN(file_open) + 4], eax
	mov dword [PMI_FN(file_close)], file_close
	mov dword [PMI_FN(file_close) + 4], eax
	mov dword [PMI_FN(file_read)], file_read
	mov dword [PMI_FN(file_read) + 4], eax
	mov dword [PMI_FN(file_write)], file_write
	mov dword [PMI_FN(file_write) + 4], eax
	mov dword [PMI_FN(file_set_pos)], file_set_pos
	mov dword [PMI_FN(file_set_pos) + 4], eax
	mov dword [PMI_FN(file_get_pos)], file_get_pos
	mov dword [PMI_FN(file_get_pos) + 4], eax
	mov dword [PMI_FN(file_get_buf)], file_get_io_buffer
	mov dword [PMI_FN(file_get_buf) + 4], eax
	mov dword [PMI_FN(file_free_buf)], file_free_io_buffer
	mov dword [PMI_FN(file_free_buf) + 4], eax
	pop ds

	retn


;==============================================================================
; Data area
;==============================================================================

global io_buf_addr

		align 4
io_buf_addr	dd 0			; Linear address of I/O buffer
file_mode_tab	db 0, 0, 1, 2		; PMI_FILE_* to DOS file access mode
