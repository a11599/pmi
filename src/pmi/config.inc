;==============================================================================
; Protected mode interface - configuration
;==============================================================================

;------------------------------------------------------------------------------
; Real mode stack size when running under VCPI or PMI internal protected mode
; host environment.
;------------------------------------------------------------------------------

RM_STACK_SIZE	EQU 0x200		; In bytes

;------------------------------------------------------------------------------
; Maximum memory allocated when running under VCPI and XMS memory is not
; available. Each additional MB uses 4 KB conventional memory. This is only the
; limit, if the VCPI server provides less extended memory, the kernel will
; also reserve less conventional memory for the VCPI page tables.
;------------------------------------------------------------------------------

VCPI_MAX_MEM_MB	EQU 64			; Maximum allocated VCPI memory in MB
VCPI_MIN_LOW_KB	EQU 256			; Minimum low memory to leave free in KB

;------------------------------------------------------------------------------
; Maximum memory allocated when running under DPMI. PMI allocates and locks the
; largest lockable block, or all available extended memory if this information
; is not provided by the host. PMI will reserve somewhat less, than available to
; leave some breathing room for the DPMI host:
; - 1/16 of extended memory up to DPMI_EXT_RSV_MB is left unallocated
; - 1/4 of conventional memory up to DPMI_LOW_RSV_KB is left unallocated
;------------------------------------------------------------------------------

DPMI_MAX_MEM_MB	EQU 64			; Maximum allocated DPMI memory in MB
DPMI_EXT_RSV_MB	EQU 2			; DPMI ext. memory not allocated in MB
DPMI_LOW_RSV_KB	EQU 64			; DPMI low memory not allocated in KB

;------------------------------------------------------------------------------
; Prefer DPMI mode over VCPI if both VCPI and DPMI are available. Used for
; development testing under DPMI, but left here in case someone has such
; preference.
;------------------------------------------------------------------------------

DPMI_PREFERRED	EQU 0			; 1: prefer DPMI, 0: prefer VCPI

;------------------------------------------------------------------------------
; Size of conventional memory buffer for file I/O and data translation.
;------------------------------------------------------------------------------

IO_BUF_SIZE	EQU 0xfff0		; I/O buffer size in bytes

;------------------------------------------------------------------------------
; Environment-variable specific setup for compilation from makefile, do not
; change
;------------------------------------------------------------------------------

%ifenv EXCEPTIONS
%define DEBUG_EXCEPTIONS
%endif
