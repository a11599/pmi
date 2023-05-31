;==============================================================================
; PMI runtime library - Keyboard handling
;------------------------------------------------------------------------------
; US-American keyboard layout ASCII translation tables
;==============================================================================

kbd_layout_us:
	dd KBD_CAPS_ON | KBD_SHIFTERS, 0, .base
	dd KBD_CAPS_ON | KBD_SHIFTERS, KBD_CAPS_ON, .caps
	dd KBD_CAPS_ON | KBD_SHIFTERS, KBD_SHIFT_LEFT, .shift
	dd KBD_CAPS_ON | KBD_SHIFTERS, KBD_SHIFT_RIGHT, .shift
	dd KBD_CAPS_ON | KBD_SHIFTERS, KBD_CAPS_ON | KBD_SHIFT_LEFT, .caps_shift
	dd KBD_CAPS_ON | KBD_SHIFTERS, KBD_CAPS_ON | KBD_SHIFT_RIGHT, .caps_shift
	dd 0

.base:
	db  27,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
	db '`','1','2','3','4','5','6','7','8','9','0','-','=',  8,  0,  0
	db   9,'q','w','e','r','t','y','u','i','o','p','[',']','\',  0,  0
	db   0,'a','s','d','f','g','h','j','k','l',';', 39,  0, 13,  0,  0
	db   0,'z','x','c','v','b','n','m',',','.','/',  0,  0,  0,  0,  0
	db   0,  0,  0,  0,  0,  0,' ',  0,  0,  0,  0,  0,  0,  0,  0,  0

.caps:
	db  27,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
	db '`','1','2','3','4','5','6','7','8','9','0','-','=',  8,  0,  0
	db   9,'Q','W','E','R','T','Y','U','I','O','P','[',']','\',  0,  0
	db   0,'A','S','D','F','G','H','J','K','L',';', 39,  0, 13,  0,  0
	db   0,'Z','X','C','V','B','N','M',',','.','/',  0,  0,  0,  0,  0
	db   0,  0,  0,  0,  0,  0,' ',  0,  0,  0,  0,  0,  0,  0,  0,  0

.shift:
	db  27,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
	db '~','!','@','#','$','%','^','&','*','(',')','_','+',  8,  0,  0
	db   9,'Q','W','E','R','T','Y','U','I','O','P','{','}','|',  0,  0
	db   0,'A','S','D','F','G','H','J','K','L',':', 34,  0, 13,  0,  0
	db   0,'Z','X','C','V','B','N','M','<','>','?',  0,  0,  0,  0,  0
	db   0,  0,  0,  0,  0,  0,' ',  0,  0,  0,  0,  0,  0,  0,  0,  0

.caps_shift:
	db  27,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
	db '~','!','@','#','$','%','^','&','*','(',')','_','+',  8,  0,  0
	db   9,'q','w','e','r','t','y','u','i','o','p','{','}','|',  0,  0
	db   0,'a','s','d','f','g','h','j','k','l',':', 34,  0, 13,  0,  0
	db   0,'z','x','c','v','b','n','m','<','>','?',  0,  0,  0,  0,  0
	db   0,  0,  0,  0,  0,  0,' ',  0,  0,  0,  0,  0,  0,  0,  0,  0

kbd_layout_us_num:
	dd KBD_NUM_ON | KBD_SHIFTERS, 0, .base
	dd KBD_NUM_ON | KBD_SHIFTERS, KBD_NUM_ON, .num
	dd KBD_NUM_ON | KBD_SHIFTERS, KBD_NUM_ON | KBD_SHIFT_LEFT, .base
	dd KBD_NUM_ON | KBD_SHIFTERS, KBD_NUM_ON | KBD_SHIFT_RIGHT, .base
	dd 0

.base:
	db   0,'/','*','-'
	db   0,  0,  0,  0
	db   0,  0,  0,'+'
	db   0,  0,  0,  0
	db   0,  0,  0, 13

.num:
	db   0,'/','*','-'
	db '7','8','9',  0
	db '4','5','6','+'
	db '1','2','3',  0
	db '0',  0,'.', 13
