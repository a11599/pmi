;==============================================================================
; PMI runtime library - Logging
;------------------------------------------------------------------------------
; Constants
;==============================================================================

;------------------------------------------------------------------------------
; Log level constants
;------------------------------------------------------------------------------

LOG_ERROR	EQU 1			; Error
LOG_WARN	EQU 2			; Warning
LOG_INFO	EQU 3			; Info message
LOG_DEBUG	EQU 4			; Debug message

;------------------------------------------------------------------------------
; Logging flags
;------------------------------------------------------------------------------

LOG_STDOUT	EQU 0x00		; Print log to standard output
LOG_STDERR	EQU 0x01		; Print log to standard error
LOG_FILE	EQU 0x02		; Print log to a file
LOG_AUTOCOMMIT	EQU 0x04		; Flush logfile after each write
LOG_APPEND	EQU 0x08		; Append to file instead overwrite

;------------------------------------------------------------------------------
; Set LOG_LEVEL constant from LOG_LEVEL environment variable
;------------------------------------------------------------------------------

%ifenv LOG_LEVEL
%defstr LOG_ENV_LEVEL %!LOG_LEVEL

%ifidni LOG_ENV_LEVEL, 'ERROR'
%define LOG_LEVEL LOG_ERROR
%elifidni LOG_ENV_LEVEL, 'WARN'
%define LOG_LEVEL LOG_WARN
%elifidni LOG_ENV_LEVEL, 'INFO'
%define LOG_LEVEL LOG_INFO
%elifidni LOG_ENV_LEVEL, 'DEBUG'
%define LOG_LEVEL LOG_DEBUG
%else
%define LOG_LEVEL 0
%endif

%else					; %ifenv LOG_LEVEL
%define LOG_LEVEL 0

%endif

;------------------------------------------------------------------------------
; Define LOG_ENABLED if LOG_LEVEL was defined and understood
;------------------------------------------------------------------------------

%if LOG_LEVEL > 0
%define LOG_ENABLED
%endif
