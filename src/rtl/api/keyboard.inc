;==============================================================================
; PMI runtime library - Keyboard handling
;------------------------------------------------------------------------------
; API
;==============================================================================

%include "rtl/consts/keyboard.inc"

extern kbd_start
extern kbd_stop
extern kbd_set_layout
extern kbd_has_event
extern kbd_get_event
extern kbd_get_modifiers
