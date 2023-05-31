;==============================================================================
; PMI runtime library - Keyboard handling
;------------------------------------------------------------------------------
; Hungarian keyboard layout ASCII translation tables
;==============================================================================

kbd_layout_hu:
	dd KBD_CAPS_ON | KBD_SHIFTERS, 0, .base
	dd KBD_CAPS_ON | KBD_SHIFTERS, KBD_CAPS_ON, .caps
	dd KBD_CAPS_ON | KBD_SHIFTERS, KBD_SHIFT_LEFT, .shift
	dd KBD_CAPS_ON | KBD_SHIFTERS, KBD_SHIFT_RIGHT, .shift
	dd KBD_CAPS_ON | KBD_SHIFTERS, KBD_CAPS_ON | KBD_SHIFT_LEFT, .caps_shift
	dd KBD_CAPS_ON | KBD_SHIFTERS, KBD_CAPS_ON | KBD_SHIFT_RIGHT, .caps_shift
	dd KBD_CAPS_ON | KBD_SHIFTERS, KBD_ALT_RIGHT, .altgr
	dd KBD_CAPS_ON | KBD_SHIFTERS, KBD_ALT_RIGHT | KBD_CTRL_LEFT, .altgr
	dd KBD_CAPS_ON | KBD_SHIFTERS, KBD_ALT_RIGHT | KBD_CAPS_ON, .altgr
	dd KBD_CAPS_ON | KBD_SHIFTERS, KBD_ALT_RIGHT | KBD_CTRL_LEFT | KBD_CAPS_ON, .altgr
	dd 0

.base:
	db  27,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
	db '0','1','2','3','4','5','6','7','8','9',148,129,162,  8,  0,  0
	db   9,'q','w','e','r','t','z','u','i','o','p',147,163,150,  0,  0
	db   0,'a','s','d','f','g','h','j','k','l',130,160,  0, 13,  0,  0
	db   0,'y','x','c','v','b','n','m',',','.','-',  0,161,  0,  0,  0
	db   0,  0,  0,  0,  0,  0,' ',  0,  0,  0,  0,  0,  0,  0,  0,  0

.caps:
	db  27,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
	db '0','1','2','3','4','5','6','7','8','9',153,154,'O',  8,  0,  0
	db   9,'Q','W','E','R','T','Z','U','I','O','P',153,'U',154,  0,  0
	db   0,'A','S','D','F','G','H','J','K','L',144,143,  0, 13,  0,  0
	db   0,'Y','X','C','V','B','N','M',',','.','-',  0,'I',  0,  0,  0
	db   0,  0,  0,  0,  0,  0,' ',  0,  0,  0,  0,  0,  0,  0,  0,  0

.shift:
	db  27,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
	db 245, 39, 34,'+','!','%','/','=','(',')',153,154,'O',  8,  0,  0
	db   9,'Q','W','E','R','T','Z','U','I','O','P',153,'U',154,  0,  0
	db   0,'A','S','D','F','G','H','J','K','L',144,143,  0, 13,  0,  0
	db   0,'Y','X','C','V','B','N','M','?',':','_',  0,'I',  0,  0,  0
	db   0,  0,  0,  0,  0,  0,' ',  0,  0,  0,  0,  0,  0,  0,  0,  0

.caps_shift:
	db  27,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
	db 245, 39, 34,'+','!','%','/','=','(',')',148,129,162,  8,  0,  0
	db   9,'q','w','e','r','t','z','u','i','o','p',147,163,150,  0,  0
	db   0,'a','s','d','f','g','h','j','k','l',130,160,  0, 13,  0,  0
	db   0,'y','x','c','v','b','n','m','?',':','_',  0,161,  0,  0,  0
	db   0,  0,  0,  0,  0,  0,' ',  0,  0,  0,  0,  0,  0,  0,  0,  0

.altgr:
	db   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
	db   0,'~',  0,'^',  0,248,  0,'`',  0,  0,  0,  0,  0,  0,  0,  0
	db   0,'\','|',  0,  0,  0,  0,  0,  0,  0,  0,246,249,  0,  0,  0
	db   0,  0,  0,  0,'[',']',  0,  0,  0,  0,'$',225,  0,  0,  0,  0
	db   0,'>','#','&','@','{','}','<',';','>','*',  0,'<',  0,  0,  0
	db   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0

kbd_layout_hu_num:
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
	db '0',  0,',', 13
