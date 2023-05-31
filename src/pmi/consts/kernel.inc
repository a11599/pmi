;==============================================================================
; Protected mode interface - Kernel
;------------------------------------------------------------------------------
; Constants
;==============================================================================

; Environment flags

ENV_DPMI	EQU 0x8000		; DPMI host present
ENV_DPMI1	EQU 0x4000		; DPMI 1.0 compliant host present
ENV_VCPI	EQU 0x2000		; VCPI host present
ENV_XMS		EQU 0x1000		; XMS memory manager present
ENV_MEM_DPMI	EQU 0x0800		; Using DPMI memory
ENV_MEM_VCPI	EQU 0x0400		; Using VCPI memory
ENV_MEM_XMS	EQU 0x0200		; Using XMS memory
ENV_MEM_RAW	EQU 0x0100		; Using raw extended memory
ENV_MEM_IDENT	EQU 0x0080		; Identity mapping under 640 KB in VCPI
ENV_MEM_LOADHI	EQU 0x0040		; Application loaded into upper memory
ENV_MEM_PAGING	EQU 0x0020		; Using paging under VCPI
ENV_A20_FAST	EQU 0x0008		; Fast A20 gate control available
ENV_CPU_V86	EQU 0x0001		; Running in a V86 task

; A20 mode

A20_NA		EQU 0			; A20 not available
A20_ENABLED	EQU 1			; A20 already enabled
A20_XMS		EQU 2			; XMS manager
A20_FAST	EQU 3			; I/O port 92h
A20_BIOS	EQU 4			; PS/2 BIOS services
A20_KBD_CTRLR	EQU 5			; 8042 keyboard controller

; Descriptor flags (generic for each type)

DESC_PAGE_GRAN	EQU 0x8000		; Granularity: 4 KB (page)
DESC_AVAILABLE	EQU 0x1000		; Available = 1
DESC_DPL_MASK	EQU 0x0060		; Destination privilege level bitmask
DESC_DPL_0	EQU 0x0000		; Destination privilege level 0
DESC_DPL_1	EQU 0x0020		; Destination privilege level 1
DESC_DPL_2	EQU 0x0040		; Destination privilege level 2
DESC_DPL_3	EQU 0x0060		; Destination privilege level 3

; Segment descriptors

SEG_PRESENT	EQU 0x0080		; Code/data segment: segment present
SEG_CS		EQU 0x0018		; Code segment
SEG_CS_32BIT	EQU 0x4000		; Code segment: 32 bit
SEG_CS_CONFRMNG	EQU 0x0004		; Code segment: conforming (caller CPL)
SEG_CS_READABLE	EQU 0x0002		; Code segment: readable
SEG_CS_ACCESSED	EQU 0x0001		; Code segment: accessed
SEG_DS		EQU 0x0010		; Data segment
SEG_DS_BIG	EQU 0x4000		; Data segment: big (use ESP for stack)
SEG_DS_EXP_DOWN	EQU 0x0004		; Data segment: expand down
SEG_DS_WRITABLE	EQU 0x0002		; Data segment: writable
SEG_DS_ACCESSED	EQU 0x0001		; Data segment: accessed
SEG_TSS		EQU 0x0009		; Task state segment
SEG_TSS_BUSY	EQU 0x0002		; Task state segment: busy (active)

; Gate descriptors

GATE_PRESENT	EQU 0x0080		; Gate: present
GATE_CALL	EQU 0x000c		; Call gate
GATE_TASK	EQU 0x0005		; Task gate
GATE_INTERRUPT	EQU 0x000e		; Interrupt gate (CLI on enter)
GATE_TRAP	EQU 0x000f		; Trap gate
