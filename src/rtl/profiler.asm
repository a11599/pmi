;==============================================================================
; PMI runtime library - Profiling tools
;------------------------------------------------------------------------------
; A simple toolkit to analyze CPU usage of code. Uses the timestamp counter on
; Pentium and newer CPUs or systimer on older architectures. Systimer's low
; resolution (~1 ms) only allows for a very low precision though.
;==============================================================================

	cpu 386

section .text

%include "pmi/api/pmi.inc"
%include "rtl/api/systimer.inc"

TSC_RESOLUTION	EQU 11			; TSC resolution bits per systimer ticks

PROF_INSTALLED	EQU 0x01
PROF_TSC_AVAIL	EQU 0x02
PROF_TSC_MEAS	EQU 0x04


;------------------------------------------------------------------------------
; Start the profiler.
;------------------------------------------------------------------------------
; <- CF - Set if error
;    EAX - Error code if CF set (PMI_E_INV_IRQ if not supported by hardware)
;------------------------------------------------------------------------------

global profiler_start
profiler_start:
	push ebx
	push ecx
	push eax

	test dword [profiler_flags], PROF_INSTALLED
	jnz .done

	call systimer_start		; Start systimer
	jc .error			; Failed: profiler not available

	test dword [profiler_flags], PROF_TSC_MEAS
	jnz .done
	test dword [profiler_flags], PROF_TSC_AVAIL
	jnz .measure_tsc

	cli				; Check for 486+
	pushfd
	pop ebx
	mov eax, ebx
	or eax, 0x200000
	push eax
	popfd
	pushfd
	pop eax
	sti
	xor eax, ebx
	jz .done

	cpu 586

	xor eax, eax			; Pentium, check RDTSC support
	cpuid
	cmp eax, 1
	jl .done
	mov eax, 1
	cpuid
	test edx, 0x10
	jz .done			; RDTSC not available

	or dword [profiler_flags], PROF_TSC_AVAIL

.measure_tsc:

	; Check TSC increments in a systimer tick

	cli

	xor eax, eax
	mov ebx, tsc_start_timer
	call systimer_set_timeout
	jc .error_rdtsc
	mov eax, 1
	mov ebx, tsc_stop_timer
	call systimer_set_timeout
	jc .error_rdtsc

	sti

.wait_tsc_cnt_loop:
	test dword [profiler_flags], PROF_TSC_MEAS
	jz .wait_tsc_cnt_loop

	mov eax, [tsc_stop_cnt]
	mov ebx, [tsc_stop_cnt + 4]
	sub eax, [tsc_start_cnt]
	sbb ebx, [tsc_start_cnt + 4]
	mov [tsc_cnt], eax
	mov [tsc_cnt + 4], ebx
	bsr ecx, ebx			; Calculate resolution shifter
	jz .test_cnt_low
	add ecx, 32 - TSC_RESOLUTION
	jmp .use_tsc

.test_cnt_low:
	bsr ecx, eax
	sub ecx, TSC_RESOLUTION
	jns .use_tsc
	xor ecx, ecx

.use_tsc:
	mov [tsc_shift_cnt], cl
	test cl, 0x20
	jnz .calc_high_tsc_cnt
	shrd eax, ebx, cl
	mov [tsc_cnt_low], eax
	jmp .done

.calc_high_tsc_cnt:
	shr ebx, cl			; Bits 5-7 of CL are ignored by the CPU
	mov [tsc_cnt_low], ebx

	cpu 386

.done:
	or dword [profiler_flags], PROF_INSTALLED
	clc
	pop eax

.exit:
	pop ecx
	pop ebx
	ret

.error_rdtsc:
	sti

.error:
	add esp, 4			; Discard EAX from stack
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Stop the profiler.
;------------------------------------------------------------------------------
; <- CF - Set if error
;    EAX - Error code if CF set
;------------------------------------------------------------------------------

global profiler_stop
profiler_stop:
	test dword [profiler_flags], PROF_INSTALLED
	jz .done

	and dword [profiler_flags], ~PROF_INSTALLED
	call systimer_stop

.done:
	clc
	ret


;------------------------------------------------------------------------------
; Get the current value of the profiler performance counter.
;------------------------------------------------------------------------------
; <- EAX - Current performance counter value or 0 if profiler not available.
;------------------------------------------------------------------------------

global profiler_get_counter
profiler_get_counter:
	mov eax, [profiler_flags]
	test eax, PROF_INSTALLED
	jz .no_counter			; Profiler not available

	test eax, PROF_TSC_AVAIL
	jnz .get_tsc			; TSC available (Pentium +)

	mov eax, [systimer_ticks]	; Return systimer tick count
	ret

.get_tsc:
	cpu 586

	push ecx
	push edx

	rdtsc				; Get TSC count
	mov cl, [tsc_shift_cnt]		; Reduce resolution to useful level
	test cl, 0x20
	jnz .tsc_high_cnt

	shrd eax, edx, cl

	pop edx
	pop ecx
	ret

.tsc_high_cnt:
	shr edx, cl			; Bits 5-7 of CL ignored by CPU
	mov eax, edx

	pop edx
	pop ecx
	ret

	cpu 386

.no_counter:
	xor eax, eax			; Profiler not available, return 0
	ret


;------------------------------------------------------------------------------
; Save TSC counter at the next systimer tick. Used to calculate number of TSC
; counts per systimer ticks.
;------------------------------------------------------------------------------

tsc_start_timer:
	push eax
	push edx

	cpu 586

	rdtsc				; Store TSC count at systimer tick start
	mov [tsc_start_cnt], eax
	mov [tsc_start_cnt + 4], edx

	cpu 386

	pop edx
	pop eax
	ret


;------------------------------------------------------------------------------
; Save TSC counter at the next + 1 systimer tick. Used to calculate number of
; TSC counts per systimer ticks.
;------------------------------------------------------------------------------

tsc_stop_timer:
	push eax
	push edx

	cpu 586

	rdtsc				; Store TSC count at systimer tick end
	mov [tsc_stop_cnt], eax
	mov [tsc_stop_cnt + 4], edx

	cpu 386

	or dword [profiler_flags], PROF_TSC_MEAS

	pop edx
	pop eax
	ret


;==============================================================================
; Data area
;==============================================================================

section .data

tsc_start_cnt	dq 0			; TSC count at start of systimer tick
tsc_stop_cnt	dq 0			; TSC count at end of systimer tick
profiler_flags	dd 0			; Profiler flags

section .bss

tsc_cnt		resq 1			; TSC count (hi-res) in a systimer tick
tsc_cnt_low	resd 1			; TSC count (low-res) in a systimer tick
tsc_shift_cnt	resb 1			; TSC count bitshift to right count
