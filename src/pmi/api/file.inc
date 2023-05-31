;==============================================================================
; Protected mode interface - File operations
;------------------------------------------------------------------------------
; API
;==============================================================================

extern file_setup
extern file_open
extern file_close
extern file_read
extern file_write
extern file_set_pos
extern file_get_pos
extern file_get_io_buffer
extern file_free_io_buffer

extern io_buf_addr
