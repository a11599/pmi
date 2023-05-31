;==============================================================================
; PMI runtime library - Keyboard handling
;------------------------------------------------------------------------------
; Constants
;==============================================================================

;------------------------------------------------------------------------------
; Keyboard events
;------------------------------------------------------------------------------

KBD_EVT_REPEAT	EQU 0			; Key auto-repeat (typematic)
KBD_EVT_DOWN	EQU 1			; Key pressed down
KBD_EVT_UP	EQU 2			; Key released

;------------------------------------------------------------------------------
; Keyboard modifier and toggle keys
;------------------------------------------------------------------------------

KBD_SCROLL_ON	EQU 0x0001		; Scroll lock on
KBD_NUM_ON	EQU 0x0002		; Num lock on
KBD_CAPS_ON	EQU 0x0004		; Caps lock on
KBD_LEDS	EQU 0x0007		; Mask for keyboard LED toggle switches
KBD_INSERT_ON	EQU 0x0008		; Insert mode on
KBD_SCROLL	EQU 0x0010		; Scroll lock down
KBD_NUM		EQU 0x0020		; Num lock down
KBD_CAPS	EQU 0x0040		; Caps lock down
KBD_INSERT	EQU 0x0080		; Insert key down
KBD_TOGGLER_KEY	EQU 0x00f0		; Mask to test for mode toggle keys
KBD_SHIFT_LEFT	EQU 0x0100		; Left shift key down
KBD_SHIFT_RIGHT	EQU 0x0200		; Right shift key down
KBD_SHIFT	EQU 0x0300		; Mask to test for any shift key
KBD_CTRL_LEFT	EQU 0x0400		; Left control key down
KBD_CTRL_RIGHT	EQU 0x0800		; Right control key down
KBD_CTRL	EQU 0x0c00		; Mask to test for any control key
KBD_ALT_LEFT	EQU 0x1000		; Left alt key down
KBD_ALT_RIGHT	EQU 0x2000		; Right alt key down
KBD_ALT		EQU 0x3000		; Mask to test for any alt key
KBD_GUI_LEFT	EQU 0x4000		; Left GUI key down
KBD_GUI_RIGHT	EQU 0x8000		; Right GUI key down
KBD_GUI		EQU 0xc000		; Mask to test for any GUI key
KBD_SHIFTERS	EQU 0xff00		; Mask to test for any shifter key

;------------------------------------------------------------------------------
; Keyboard key codes
;------------------------------------------------------------------------------
; A key code is an 8-bit value approximately representing the position of the
; key on a US keyboard. The upper 3 bits represent the row and lower 5 bits
; represent the column. Use the KC macro to refer to a keycode by row and
; column (only constants allowed), or use the KC_* constants to refer to a
; specific key.
; - Row 0 is reserved for dedicated shortcut keys such as multimedia buttons.
; - Columns 0 - 13 are used for the main keyboard buttons.
; - Columns 16 - 18 are used for the cursor and editing button section.
; - Columns 24 - 27 are for keypad buttons.
;------------------------------------------------------------------------------

%define KC(row, column) row * 32 + column

; Row 1

KC_ESC		EQU KC(1, 0)
KC_F1		EQU KC(1, 2)
KC_F2		EQU KC(1, 3)
KC_F3		EQU KC(1, 4)
KC_F4		EQU KC(1, 5)
KC_F5		EQU KC(1, 6)
KC_F6		EQU KC(1, 7)
KC_F7		EQU KC(1, 8)
KC_F8		EQU KC(1, 9)
KC_F9		EQU KC(1, 10)
KC_F10		EQU KC(1, 11)
KC_F11		EQU KC(1, 12)
KC_F12		EQU KC(1, 13)
KC_SYSREQ	EQU KC(1, 15)
KC_PRINT_SCREEN	EQU KC(1, 16)
KC_SCROLL	EQU KC(1, 17)
KC_PAUSE	EQU KC(1, 18)

; Row 2

KC_BACKTICK	EQU KC(2, 0)		; `
KC_1		EQU KC(2, 1)
KC_2		EQU KC(2, 2)
KC_3		EQU KC(2, 3)
KC_4		EQU KC(2, 4)
KC_5		EQU KC(2, 5)
KC_6		EQU KC(2, 6)
KC_7		EQU KC(2, 7)
KC_8		EQU KC(2, 8)
KC_9		EQU KC(2, 9)
KC_0		EQU KC(2, 10)
KC_MINUS	EQU KC(2, 11)		; -
KC_EQUALS	EQU KC(2, 12)		; =
KC_BACKSPACE	EQU KC(2, 13)
KC_INSERT	EQU KC(2, 16)
KC_HOME		EQU KC(2, 17)
KC_PAGE_UP	EQU KC(2, 18)
KC_NUM		EQU KC(2, 24)
KC_KP_SLASH	EQU KC(2, 25)		; Keypad /
KC_KP_ASTERISK	EQU KC(2, 26)		; Keypad *
KC_KP_MINUS	EQU KC(2, 27)		; Keypad -

; Row 3

KC_TAB		EQU KC(3, 0)
KC_Q		EQU KC(3, 1)
KC_W		EQU KC(3, 2)
KC_E		EQU KC(3, 3)
KC_R		EQU KC(3, 4)
KC_T		EQU KC(3, 5)
KC_Y		EQU KC(3, 6)
KC_U		EQU KC(3, 7)
KC_I		EQU KC(3, 8)
KC_O		EQU KC(3, 9)
KC_P		EQU KC(3, 10)
KC_BRACKET_OPEN	EQU KC(3, 11)		; [
KC_BRACKET_CLS	EQU KC(3, 12)		; ]
KC_BACKSLASH	EQU KC(3, 13)
KC_DELETE	EQU KC(3, 16)
KC_END		EQU KC(3, 17)
KC_PAGE_DOWN	EQU KC(3, 18)
KC_KP_7		EQU KC(3, 24)		; Keypad 7
KC_KP_8		EQU KC(3, 25)		; Keypad 8
KC_KP_9		EQU KC(3, 26)		; Keypad 9

; Row 4

KC_CAPS		EQU KC(4, 0)
KC_A		EQU KC(4, 1)
KC_S		EQU KC(4, 2)
KC_D		EQU KC(4, 3)
KC_F		EQU KC(4, 4)
KC_G		EQU KC(4, 5)
KC_H		EQU KC(4, 6)
KC_J		EQU KC(4, 7)
KC_K		EQU KC(4, 8)
KC_L		EQU KC(4, 9)
KC_SEMICOLON	EQU KC(4, 10)		; ;
KC_APOSTROPHE	EQU KC(4, 11)		; '
KC_ENTER	EQU KC(4, 13)
KC_KP_4		EQU KC(4, 24)		; Keypad 4
KC_KP_5		EQU KC(4, 25)		; Keypad 5
KC_KP_6		EQU KC(4, 26)		; Keypad 6
KC_KP_PLUS	EQU KC(4, 27)		; Keypad +

; Row 5

KC_SHIFT_LEFT	EQU KC(5, 0)
KC_Z		EQU KC(5, 1)
KC_X		EQU KC(5, 2)
KC_C		EQU KC(5, 3)
KC_V		EQU KC(5, 4)
KC_B		EQU KC(5, 5)
KC_N		EQU KC(5, 6)
KC_M		EQU KC(5, 7)
KC_COMMA	EQU KC(5, 8)		; ,
KC_DOT		EQU KC(5, 9)		; .
KC_SLASH	EQU KC(5, 10)		; /
KC_INTL		EQU KC(5, 12)		; Extra key on international keyboard
KC_SHIFT_RIGHT	EQU KC(5, 13)
KC_CURSOR_UP	EQU KC(5, 17)
KC_KP_1		EQU KC(5, 24)		; Keypad 1
KC_KP_2		EQU KC(5, 25)		; Keypad 2
KC_KP_3		EQU KC(5, 26)		; Keypad 3

; Row 6

KC_CTRL_LEFT	EQU KC(6, 0)
KC_GUI_LEFT	EQU KC(6, 1)
KC_ALT_LEFT	EQU KC(6, 2)
KC_SPACE	EQU KC(6, 6)
KC_ALT_RIGHT	EQU KC(6, 10)
KC_GUI_RIGHT	EQU KC(6, 11)
KC_MENU		EQU KC(6, 12)		; Apps / local menu
KC_CTRL_RIGHT	EQU KC(6, 13)
KC_CURSOR_LEFT	EQU KC(6, 16)
KC_CURSOR_DOWN	EQU KC(6, 17)
KC_CURSOR_RIGHT	EQU KC(6, 18)
KC_KP_0		EQU KC(6, 24)		; Keypad 0
KC_KP_DOT	EQU KC(6, 26)		; Keypad .
KC_KP_ENTER	EQU KC(6, 27)		; Keypad enter

; Row 0: dedicated function buttons

KC_MM_TRK_PREV	EQU KC(0, 0)		; MM: Previous track
KC_MM_TRK_NEXT	EQU KC(0, 1)		; MM: Next track
KC_MM_MUTE	EQU KC(0, 2)		; MM: Mute
KC_MM_PLAY	EQU KC(0, 3)		; MM: Play
KC_MM_STOP	EQU KC(0, 4)		; MM: Stop
KC_MM_VOL_DOWN	EQU KC(0, 5)		; MM: Volume -
KC_MM_VOL_UP	EQU KC(0, 6)		; MM: Volume +
KC_POWER	EQU KC(0, 8)		; ACPI: Power
KC_SLEEP	EQU KC(0, 9)		; ACPI: Sleep
KC_WAKE		EQU KC(0, 10)		; ACPI: Wake
KC_MM_CALC	EQU KC(0, 16)		; MM: Calculator
KC_MM_EXPLORER	EQU KC(0, 17)		; MM: My computer (explorer)
KC_MM_EMAIL	EQU KC(0, 18)		; MM: E-mail
KC_MM_MEDIA_SEL	EQU KC(0, 19)		; MM: Media select
KC_WWW_HOME	EQU KC(0, 24)		; WWW: Home
KC_WWW_SEARCH	EQU KC(0, 25)		; WWW: Search
KC_WWW_FAVS	EQU KC(0, 26)		; WWW: Favorites
KC_WWW_RELOAD	EQU KC(0, 27)		; WWW: Refresh/reload page
KC_WWW_STOP	EQU KC(0, 28)		; WWW: Stop page download
KC_WWW_FORWARD	EQU KC(0, 29)		; WWW: Forward
KC_WWW_BACK	EQU KC(0, 30)		; WWW: Back
