;==============================================================================
; Protected mode interface - Kernel
;------------------------------------------------------------------------------
; API
;==============================================================================

extern kernel_start
extern kernel_shutdown
extern exit_to_dos

extern exit_error
extern pmi_fn
extern kernel_seg
extern kernel_addr
extern page_mem_addr
extern env_seg
extern sel_code_flat
extern sel_code_krnl16
extern sel_data_krnl32
extern sel_data_flat
extern sel_pmi_fn
extern cmb_base
extern cmb_size
extern xmb_base
extern xmb_size
extern env_flags
extern pm_host_type
extern dos_ver
