This is PMI

PROTECTED MODE INTERFACE
========================

_a minimalist host environment for flat memory model 32-bit DOS applications_

- Compatible with DPMI and VCPI
- Built-in protected mode host when neither DPMI, nor VCPI is present
- Small (around 11.5k uncompressed without exception dump)
- Built for speed (can disable paging under VCPI when certain conditions are met)
- Runs applications in 32-bit flat memory model in PE executable format
- Written in assembly for assembly programs
- Small, concise API
- Comes with a small, useful runtime library for common operations


Table of contents
-----------------

- [What? Why?](#what-why)
- [32-bit protected mode primer](#32-bit-protected-mode-primer)
  - [What is protected mode?](#what-is-protected-mode)
  - [What is a flat memory model?](#what-is-a-flat-memory-model)
  - [What about that paging stuff?](#what-about-that-paging-stuff)
- [The PMI runtime environment](#the-pmi-runtime-environment)
  - [Memory](#memory)
  - [Interrupts](#interrupts)
  - [Stacks](#stacks)
  - [Application startup, termination](#application-startup-termination)
- [Creating a PMI application](#creating-a-pmi-application)
  - [The build environment](#the-build-environment)
    - [hello.asm](#helloasm)
    - [hello.lnk](#hellolnk)
  - [Building the application](#building-the-application)
- [PMI API](#pmi-api)
  - [Usage](#usage)
  - [Error handling](#error-handling)
  - [pmi.inc](#pmiinc)
  - [Kernel services](#kernel-services)
    - [`get_env_info`](#get_env_info)
    - [`call_rm`](#call_rm)
    - [`get_irq_hndlr`](#get_irq_hndlr)
    - [`set_irq_hndlr`](#set_irq_hndlr)
  - [Application management services](#application-management-services)
    - [`execute`](#execute)
    - [`terminate`](#terminate)
  - [Memory handling](#memory-handling)
    - [`mem_alloc`](#mem_alloc)
    - [`mem_free`](#mem_free)
  - [File operations](#file-operations)
    - [`file_open`](#file_open)
    - [`file_close`](#file_close)
    - [`file_read`](#file_read)
    - [`file_write`](#file_write)
    - [`file_set_pos`](#file_set_pos)
    - [`file_get_pos`](#file_get_pos)
    - [`file_get_buf`](#file_get_buf)
    - [`file_free_buf`](#file_free_buf)
  - [DMA services](#dma-services)
    - [`dma_start`](#dma_start)
    - [`dma_stop`](#dma_stop)
- [PMI runtime library](#pmi-runtime-library)
  - [`env_arg` module](#env_arg-module)
    - [`env_arg_setup`](#env_arg_setup)
    - [`env_get_value`](#env_get_value)
    - [`arg_get_count`](#arg_get_count)
    - [`arg_get`](#arg_get)
    - [`arg_get_value`](#arg_get_value)
  - [`irq` module](#irq-module)
    - [`irq_pic_eoi irq` macro](#irq_pic_eoi-irq-macro)
    - [`irq_enabled`](#irq_enabled)
    - [`irq_disable`](#irq_disable)
    - [`irq_enable`](#irq_enable)
  - [`keyboard` module](#keyboard-module)
    - [Keycodes](#keycodes)
    - [Modifiers](#modifiers)
    - [`kbd_start`](#kbd_start)
    - [`kbd_stop`](#kbd_stop)
    - [`kbd_set_layout`](#kbd_set_layout)
    - [`kbd_has_event`](#kbd_has_event)
    - [`kbd_get_event`](#kbd_get_event)
    - [`kbd_get_modifiers`](#kbd_get_modifiers)
  - [`log` module](#log-module)
    - [`log command`\[, parameters...\] macro](#log-command-parameters-macro)
      - [`log start`, logbufsize, flags\[, logfile\]](#log-start-logbufsize-flags-logfile)
      - [`log stop`](#log-stop)
      - [`log flags`](#log-flags)
      - [`log mem`](#log-mem)
      - [`log` level, "message"\[, parameters\]](#log-level-message-parameters)
  - [`profiler` module](#profiler-module)
    - [`profiler_start`](#profiler_start)
    - [`profiler_stop`](#profiler_stop)
    - [`profiler_get_counter`](#profiler_get_counter)
  - [`string` module](#string-module)
    - [`str_len`](#str_len)
    - [`str_reverse`](#str_reverse)
    - [`str_copy`](#str_copy)
    - [`str_append`](#str_append)
    - [`str_char_pos`](#str_char_pos)
    - [`str_char_rpos`](#str_char_rpos)
    - [`str_cmp`](#str_cmp)
    - [`str_int`](#str_int)
    - [`str_fixed`](#str_fixed)
    - [`str_hex`](#str_hex)
    - [`str_format`](#str_format)
    - [`str_parse_int`](#str_parse_int)
    - [`str_parse_fixed`](#str_parse_fixed)
    - [`str_parse_hex`](#str_parse_hex)
  - [`systimer` module](#systimer-module)
    - [`systimer_start`](#systimer_start)
    - [`systimer_stop`](#systimer_stop)
    - [`systimer_ticks`](#systimer_ticks)
    - [`systimer_set_timeout`](#systimer_set_timeout)
    - [`systimer_clear_timeout`](#systimer_clear_timeout)
  - [`timer` module](#timer-module)
    - [`timer_calc_rate`](#timer_calc_rate)
    - [`timer_set_rate`](#timer_set_rate)
    - [`timer_reset_rate`](#timer_reset_rate)
    - [`timer_start`](#timer_start)
    - [`timer_stop`](#timer_stop)
- [Technical details](#technical-details)
  - [Modes of operation](#modes-of-operation)
  - [Memory management](#memory-management)
  - [Exception handling](#exception-handling)
  - [Hardware interrupts](#hardware-interrupts)
  - [System registers, tables](#system-registers-tables)
- [Configure and build](#configure-and-build)
  - [Changing compile time parameters](#changing-compile-time-parameters)
  - [Building a custom PMI stub](#building-a-custom-pmi-stub)


# What? Why?

When the news are about AI writing code by itself, it's time to show the middle finger with some x86 retro assembly coding. This is a protected mode host to run your tidy 32-bit application in PE executable format in a flat memory address space. This means no segment registers to care about (except in IRQ handlers) and you can access the entire host memory with linear addresses.

This isn't new, in fact this was the standard at the end of the DOS era and pretty much the only standard in current operating systems. PMI was written for multiple reasons:

- 32-bit PE format support was either missing from most other protected mode hosts or supported the old DJGPP format only. I wanted to use Windows NT PE format.
- Many hosts that do support 32-bit PE format use CPU features that aren't emulated well by DosBox (and thus won't work reliably).
- Most hosts implement the DPMI API and have a built-in DOS extender. Many even support stuff such as virtual memory, DLL loading or some kind of Win32 API emulation (especially those that support 32-bit PE executables). I don't want any of that bloat.
- None of them can run under VCPI with paging disabled as far as I know. It was an old trick I used in my early-2000s DOS extenders and I wanted to do it again because it feels good.
- None of them have built-in support for DMA addressable memory block allocation.

PMI is small, it only adds about 11.5 kbytes to your application (or about 14 kbytes when exception crash dump is enabled). Writing a protected mode application is simple, just create a PE executable and use pmi.exe as the MS-DOS (MZ EXE) stub. The API is simple: your application is provided with a jump table at GS:0 that you can use to call PMI's services.


# 32-bit protected mode primer

This section is aimed at those who have experience with real mode segmented x86 assembly, but haven't coded in protected mode and/or under a flat memory model yet.

## What is protected mode?

Protected mode was the native operating mode of 80286 and newer processors (these days it's considered a legacy mode, with 64-bit long mode being the native mode and Intel even considering removing these modes). It can be used to restrict applications accessing or writing memory areas it shouldn't. It also has mechanisms to recover from application faults so it won't bring down the entire operating system. These are extremely useful features for modern multitasking operating systems, but largely useless for single-user, single-application systems like DOS.

Protected mode does have segments, but with a very different concept. Instead of pointing to an actual memory address, segment registers contain indexes (called selectors) to special tables called descriptors, which define the start address (base) and size (limit) of the segment to which the register points to. They also have several control flags to enable read/write, code execution, and so on, to achieve memory protection.

The true funkiness however started with the 80386, which was the first 32-bit processor of the x86 series. It has 32-bit registers and a 32-bit data bus, thus supporting up to 4 GB of memory. But the most important part is that the 80386's 32-bit protected mode can access the entire 4 GB address space without dealing with segments!

## What is a flat memory model?

I think you can see where we are heading to. Yes, it is possible to set the base and size (limit) of these selectors so that each segment starts at address 0 and ends at 4 GB, thus you can access any memory by referencing its absolute address. When all common segment registers (CS, DS, ES and SS) are set up like that, you don't ever need to worry about segments when accessing memory, no matter what register you use as a base or index. Want to display a 320x200 256-color VGA image? No problem, just set ESI to point to the image in memory, EDI to `0xa0000` (address of the VGA frame buffer), ECX to `16000` and do a `REP MOVSD`. The source image may be anywhere in memory.

Unfortunately, you can't really get the full potential within the realms of a traditional DOS .EXE file. Those were designed with real mode (traditional segmented memory access) in mind. .EXE files can have a self-contained protected mode host and can enter 32-bit protected mode and they can also setup flat memory data segments, but you cannot use them directly to access your program's code or data directly. .EXE files don't have relocation info for offsets within segments. That's why all flat model hosts/DOS extenders use some other executable format such as the OS/2 linear executable ("LE" or "LX") or the Windows portable executable ("PE"). My choice for PMI was 32-bit PE, which is a widespread format that is easy to write code for and has good compiler/linker support. PE was designed to run in the flat memory model from the ground up. PE files are compiled to run at a specific fixed address in memory, by default at 0x400000, or 4 MB. If PMI can allocate enough memory at this address (probably, if the machine has at least 8 GB RAM), the executable can be loaded very quickly. If the address is not available, the PE format includes all necessary relocations to alter the executable image's memory references so they will point to the correct memory address. This takes a little bit of time though (applications with lots of data access can have very large relocation tables). But PMI was written for assembly programs where you should use registers for as many operations as possible anyways.

## What about that paging stuff?

Paging was another new feature of the 80386. It's basically a way to translate memory from linear addresses (the addresses your program uses) to physical addresses (actual addresses of memory and memory-mapped devices). Paging is extremely useful for real operating systems and can be used to map any physical memory at any linear address. For example, if the PE executable was compiled to run at address 0x400000, the OS can set up paging to have memory mapped at this address to prevent relocation. Paging is also very useful to prevent memory fragmentation in multitasking environments.

There are two problems with paging:

- It's slow. Not by much, but still slower, than actual direct physical memory access, especially on old 80386 and 80486s. PMI was written for performance and absolutely targets these old platforms so it tries to avoid paging when possible.

- ISA DMA. The PC's ISA DMA controllers were never updated to support the address translation of the CPU. These pesky devices were difficult enough to program already, but now it's further complicated by the fact that they still need to be programmed with physical memory addresses while your application can see totally different ones. There are several techniques to mitigate the side effects of paging, but to play audio, you need a nice contiguous physical memory area for ISA sound cards.

# The PMI runtime environment

## Memory

The largest conventional and extended memory block is allocated by PMI before passing control to your application. Memory can be allocated from this area using the `mem_alloc` PMI service. The maximum amount of available extended memory is 64 MB. This should be plenty for retro assembly coding.

All memory allocated via DPMI is also locked (the host should either not swap it, or swap transparently). This also means that hardware interrupt handlers can access any memory allocated by PMI.

PMI does not provide a heap area to the application (no matter what is specified in the PE file).

## Interrupts

CPU exceptions are handled by PMI or the DPMI host when present. Exceptions will terminate the PMI application and return control back to DOS. The debug version of PMI dumps registers and the top 64 bytes of stack when not running under DPMI.

Hardware interrupts (IRQs) are supported in protected mode. By default, the IRQ is handled by its real mode (DOS) handler. When a protected mode handler is installed, the handler may either handle the IRQ completely in protected mode, or pass it back to real mode by jumping to the previous IRQ handler. IRQ handlers are always called with a 32-bit stack frame and should return using a 32-bit iret (iretd).

Hardware interrupt handlers are called directly, without any PMI code between the handler and the interrupt request, with the exception of IRQs 7 and 15. For these IRQs PMI will run a tiny piece of code which checks if the IRQ is spurious and ignores it in that case. Control will be transferred to the actual handler as quickly as possible.

Like in real mode, hardware interrupt handlers should not make any assumptions on segment registers when running under PMI. Save the data segment selector and set it in the IRQ handler if write access is needed. The same goes for the stack segment, if you need a flat selector in the stack, setup your own stack within the IRQ handler.

Hardware interrupt handlers should not call PMI services.

DOS Ctrl-Break (0x23) and critical error (0x24) handlers are redirected by PMI. The Ctrl-Break handler won't do anything (although the default ^C characters will be printed to stdout by DOS if you are doing any DOS console I/O). This won't happen when using the `keyboard` runtime library, because keystrokes won't reach the BIOS (and hence DOS). PMI's critical error handler sets the Carry flag and reports an access denied (0x05) error in AX to the caller (thus effectively failing the call).

PMI does not support software interrupts in protected mode. Existing real mode software interrupts can be called using the call_rm service.

## Stacks

PMI will allocate a stack for the application to use. The size of the stack depends on the reserved stack size specified in the PE header (PMI has no virtual memory and hence will always allocate the entire stack before control is transferred to the application).

During mode switches:

- Under DPMI: The DPMI host provides the necessary stacks. Usually there are separate stacks for real mode, protected mode and protected mode IRQ handlers. The real mode stack is guaranteed to be at least 512 bytes, while the stack for protected mode IRQ handlers is at least 4096 bytes. The protected mode stack is the normal application stack that was allocated by PMI.

- When running under PMI's internal host (VCPI or raw), PMI manages two stacks, one for real mode and one for protected mode. The switch happens transparently during mode switches. The protected mode stack is the one that was provided to the application. PMI uses the same protected mode stack to service IRQ handlers. PMI's real mode stack is 512 bytes by default.

The application may switch to a different stack at any time. The application may also switch to another stack while servicing an IRQ, but must return to the original stack before returning from the IRQ handler.

## Application startup, termination

Once PMI initialized the protected mode environment, it attempts to load the PE executable from the same .EXE file. PMI tries to allocate memory for the executable at its preferred base address (which is 0x400000 by default), starting from the offset where the actual compiled code/data begins. If this address is not available, PMI will relocate it to a suitable memory block (if the system has not enough memory, PMI terminates with an error message).

The executable gets control at its entry point with the following register values:

- EAX, ECX, EDX, EBP: 0
- EBX: Linear address of the DOS environment (a list of ASCIIZ key-value pairs, terminated with an empty string).
- ESI: Linear address of the PE executable's ASCIIZ filename. For the application that was started by PMI, this will contain the full path specification of the file. Otherwise it's the same that was given by the caller of the `execute` service.
- EDI: Linear address of ASCIIZ command line arguments.
- CS:EIP: Program entry point.
- SS:ESP: Top of stack allocated for the application.
- GS: PMI public API jump table segment (pmi_fns structure instance).
- CS: Flat 32-bit code segment (base: 0, size: 4 GB).
- DS, ES, FS, SS: Flat data and 32-bit stack segment (base: 0, size: 4 GB).

Applications shall use the `terminate` PMI service to exit. The `execute` service can be used to run another application. When an application terminates, control is given back to the application that started it. If there is no parent application, control is passed back to PMI which terminates to DOS.

When PMI terminates to DOS:

- The original video mode is restored when terminating due to an exception.
- The original PIC IRQ masks are restored.
- Timer frequency is reset to 18.2 Hz.
- Running DMA transfers on channels with auto restart DMA (`PMI_DMA_AUTO`) are stopped.

PMI applications cannot be installed as TSR programs. If your program needs to stay resident, then you probably need a more advanced DPMI-like extender anyways (or should not be using protected mode at all).


# Creating a PMI application

_Using NASM and the Watcom linker to create a "Hello world!" application._

## The build environment

PMI was meant for applications written in assembly language. My choice of assembler is [NASM](https://www.nasm.us/):

- It is free.
- It is in active development and has been for decades.
- It uses Intel syntax.
- It is cross-platform (my primary development platform is DosBox on Windows, but I also want it to compile under DOS).
- It has good local label support and a useful macro language.

The assembly source code is compiled to Win32 object format. This allows keeping the source code very clean from segment directives.

For building and linking I prefer using wmake and wlink from [Open Watcom](https://www.openwatcom.org/) tools:

- It is free.
- It is cross-platform (see reasons above).
- It can build PE executables for Phar Lap TNT DOS extender, which is just a standard Win32 PE with PL signature. This means Windows won't try to execute the PMI application and there is no need to patch the binary after the build process.

To create a "Hello world!" application, you need the assembly source code (hello.asm) and a linker directive file (hello.lnk). The contents of these two files are provided below.

### hello.asm

```nasm
        cpu 386

section .text

%include "pmi/api/pmi.inc"

global _main
_main:
        mov ebx, 0x01                   ; File handle for standard output
        mov ecx, HELLO_WORLD_SIZE       ; Length of "Hello world!"
        mov esi, hello_world            ; "Hello world!" string offset
        call pmi(file_write)            ; Write to standard output

        call pmi(terminate)             ; Terminate application

section .data

hello_world     db 'Hello world!', 13, 10
                HELLO_WORLD_SIZE EQU $ - hello_world
```

### hello.lnk

```
NAME hello
OPTION stub={path\to\pmi}\bin\pmi.exe
OPTION start=_main
OPTION stack=4096
FORM Windows NT TNT
FILE {hello.obj}
```

Replace `{path\to\pmi}` with the absolute or relative path of the PMI directory.

## Building the application

To compile, use the following command (replace `{path/to/pmi}` with the absolute or relative path of the PMI directory):
```
nasm -f win32 hello.asm -i "{path/to/pmi}/src"
```

To build the executable:
```
wlink @hello.lnk
```

Of course, you can (and should use) a makefile for larger projects. This was omitted here for simplicity. Check out the [tmodplay](/a11599/tmodplay) repository for a PMI application with a more complete DOS/Windows build environment setup.

The benefit of Win32 object format and PE flat memory executable is the obvious simplicity of the assembly source code. No need to use segment directives or groups. Just put all your code to `section .text`, data to `section .data` and uninitialized data to `section .bss`. The Win32 format automatically assumes and generates 32-bit code in all segments.

Linker options are also pretty simple. Set the stub option to pmi.exe, the start option to the global label for your application entry point, stack to the amount of wanted stack space. Use either `Windows NT` or `Windows NT TNT` form. PMI supports both, but you need to use `Windows NT TNT` if the application must be compatible with Windows DOS boxes (otherwise Windows will try to execute the PE itself which will probably generate a fault pretty soon).

The hello world application above contains `call pmi()` instructions. The `pmi()` macro is defined in `pmi.inc` and provides a convenient way to call PMI services from your application. See the chapter below on PMI's API and the `pmi.inc` include file.


# PMI API

## Usage

PMI provides a small, but useful set of services for 32-bit programs. The API is lightweight and kept to the minimum. All API services must be called using far calls. When the application starts, pointers to API services are provided in the GS segment selector.

API services have a snake_case name and are defined in the `pmi_fns`
structure:

| offset   | name               | description |
| -------- | ------------------ | ----------- |
| 0x00     | `get_env_info`     | Get PMI environment information |
| 0x08     | `call_rm`          | Call far real mode procedure or interrupt |
| 0x10     | `get_irq_hndlr`    | Get current hardware interrupt handler |
| 0x18     | `set_irq_hndlr`    | Set hardware interrupt handler |
| 0x20     | `execute`          | Load and run PMI application |
| 0x28     | `terminate`        | Terminate current PMI application |
| 0x30     | `mem_alloc`        | Allocate memory block |
| 0x38     | `mem_free`         | Free previously allocated memory block |
| 0x40     | `file_open`        | Open a file using DOS services |
| 0x48     | `file_close`       | Close a previously open file |
| 0x50     | `file_read`        | Read from an open file |
| 0x58     | `file_write`       | Write to an open file |
| 0x60     | `file_set_pos`     | Set read/write position of file (seek) |
| 0x68     | `file_get_pos`     | Get current file read/write position |
| 0x70     | `file_get_buf`     | Get address of conventional memory I/O buffer |
| 0x78     | `file_free_buf`    | Free conventional memory I/O buffer |
| 0x80     | `dma_start`        | Start DMA transfer in specific channel |
| 0x88     | `dma_stop`         | Stop DMA transfer in specific channel |

PMI services are not reentrant, but you should be fine as long as you avoid calling them from hardware interrupt handlers. All services expect flat memory model segments for DS and ES and a valid stack at SS:ESP with at least 256 bytes available. This is normally not an issue because there is no need to change any of these registers and the services should not be called from IRQ handlers anyways.

## Error handling

API services that may fail will set the carry flag to indicate an error condition and EAX will be set to a PMI, DOS or DPMI error code. Some services return a PMI or DOS error code in AH. There is no clash, because each error code has a unique recognizable characteristic:

- PMI error codes are negative.
- DOS error codes are 8-bit positive and <= 0x12.
- DPMI error codes are 16-bit and start at 0x8000.

List of PMI error codes returned to PMI applications:

| value | name              | description |
| ----- | ----------------- | ----------- |
| -2    | `PMI_E_MEM_LOW`   | Not enough memory to fulfill request. |
| -4    | `PMI_E_MEM_INVL`  | Memory blocks have been corrupted. Reason is most probably some bug in code which writes outside of the allocated memory block or buggy calculation of necessary memory block size (allocated memory is too small to fit data). |
| -7    | `PMI_E_INV_PE`    | PE executable not found or PE data cannot be parsed. The file is either not a Win32 PE file or it has some complex/unusual structure that PMI's limited PE parser cannot understand. |
| -8    | `PMI_E_INV_IRQ`   | The IRQ number is not valid (> 15). |
| -9    | `PMI_E_INV_DMA`   | The DMA channel number is not valid (> 7). |

There are also other error codes, but those are only used internally and won't be returned by the API.

## pmi.inc

The `pmi.inc` include file in the `src\pmi\api` folder contains definitions of PMI constants and structures for NASM. Constants are all uppercase, structures are all lowercase with snake_case naming.

The `pmi(fn)` macro can be used to conveniently call PMI services from the application as long as the GS register points to the PMI public API jump table. The macro doesn't contain the call instruction itself for the sake of better readability of the appliction source code.

_Example: Terminate PMI application._

```nasm
section .text

        %include pmi/api/pmi.inc

        call pmi(terminate)
```

## Kernel services

These services provide a way to call real mode procedures or interrupts and to get/set hardware IRQ handlers.

### `get_env_info`

Return information about the PMI and DOS environment.

Inputs:

None.

Outputs:

- EBX: Linear address of `pmi_env_info` structure.

The `pmi_env_info` structure consists of the following members:

| offset | value           | size | description |
| ------ | --------------- | ---- | ----------- |
| 0x00   | `cmb_base`      | 4    | Linear address of the conventional memory area. This is only provided for information purposes, always use PMI memory management services for memory block allocations. |
| 0x04   | `cmb_size`      | 4    | Size of the conventional memory area. |
| 0x08   | `xmb_base`      | 4    | Linear address of the extended memory area. This is only provided for information purposes, always use PMI memory management services for memory block allocations. |
| 0x0c   | `xmb_size`      | 4    | Size of the extended memory area. |
| 0x10   | `psp_addr`      | 4    | Linear address of the DOS PSP segment for the current PMI application. |
| 0x14   | `pcb_addr`      | 4    | Linear address of currently running PMI application's program control block. Definition of this structure is available in `src\structs\program.inc`. |
| 0x18   | `io_buf_addr`   | 4    | Linear address of I/O buffer used by file services for DOS address translation. 0 if the buffer is not currently allocated. |
| 0x1c   | `io_buf_size`   | 4    | Size of the I/O buffer used by file services for DOS address translation, if allocated (`io_buf_addr` is not zero). |
| 0x20   | `dos_ver`       | 2    | Version of DOS. High byte is major, low byte is minor version. Check this before relying on and DOS 2.0+ feature. |
| 0x22   | `pm_host_type`  | 1    | Type of the protected mode host environment, see `PMI_HOST_*` constants below. |
| 0x23   | `startup_vmode` | 1    | Video mode that was active before the PMI application was started. You can use this information when setting a different mode to restore the original mode before terminating your application (PMI won't do this unless it's terminating due to an exception or panic situation). |

Possible values for `pm_host_type` member:

| value | name            | description |
| ----- | --------------- | ----------- |
| 0     | `PMI_HOST_RAW`  | Protected mode as well as real-protected mode transitions are managed by PMI's internal host environment. |
| 1     | `PMI_HOST_VCPI` | Protected mode is managed by PMI's internal host environment. Real-protected mode transitions are handled by a VCPI compliant host (usually an expanded memory manager). |
| 2     | `PMI_HOST_DPMI` | Protected mode as well as real-protected mode transitions are managed by a DPMI compliant host. |

Please refer to the [Modes of operation](#modes-of-operation) chapter for technical details on each operating mode.

### `call_rm`

Call a far real mode procedure which shall return with retf or a real mode interrupt. General registers and flags are passed in-place, segment registers and input parameters are provided/passed back on the caller stack. The stack frame must point to an instance of a `pmi_rm_call` structure.

Inputs:

- EAX, EBX, ECX, EDX, ESI, EDI, EBP, flags: Passed to real mode procedure.
- SS:ESP: Pointer to `pmi_rm_call` structure.

Outputs:

- EAX, EBX, ECX, EDX, ESI, EDI, EBP, flags: Passed from real mode procedure.
- SS:ESP: `pmi_rm_call` structure updated with new segment register values.

The `pmi_rm_call` structure is 16 bytes long.

| offset | name         | size | description |
| ------ | ------------ | ---- | ----------- |
| 0x00   | `flags`      | 1    | Flags indicating the type of the real mode procedure. See `PMI_CALL_*` constants below. |
| 0x01   | `int`        | 1    | Interrupt number for `PMI_CALL_INT`. The interrupt is called with the interrupt flag cleared and hardware interrupts disabled. |
| 0x02   | `error_code` | 2    | PMI, MS-DOS or DPMI error code if theoperation fails. Currently this can only failunder DPMI. If successful, it's set to 0. |
| 0x04   | `ip`         | 2    | Offset of the real mode procedure for `PMI_CALL_FAR`. |
| 0x06   | `cs`         | 2    | Real mode segment of the real mode procedure for `PMI_CALL_FAR`. |
| 0x08   | `ds`         | 2    | Real mode segment for DS register. |
| 0x0a   | `es`         | 2    | Real mode segment for ES register. |
| 0x0c   | `fs`         | 2    | Real mode segment for FS register. |
| 0x0e   | `gs`         | 2    | Real mode segment for GS register. |

Possible values for flags:

| value | name           | description |
| ----- | -------------- | ----------- |
| 0x00  | `PMI_CALL_FAR` | Far real mode procedure that exits with RETF. |
| 0x01  | `PMI_CALL_INT` | Real mode interrupt. |

_Example: Read 4096 bytes from file into conventional memory buffer
allocated via mem_alloc service, pointed to by EAX._

```nasm
        %include pmi/api/pmi.inc

        sub esp, pmi_rm_call.strucsize
        mov byte [esp + pmi_rm_call.flags], PMI_CALL_INT
        mov byte [esp + pmi_rm_call.int], 0x21
        mov edx, eax
        and edx, 0xf                    ; DX: offset of buffer
        shr eax, 4
        mov [esp + pmi_rm_call.ds], ax  ; DS: segment of buffer
        mov cx, 4096                    ; CX: bytes to read
        mov bx, [file_handle]           ; BX: handle of previously open file
        mov ah, 0x3f                    ; AH: read from file via handle
        call pmi(call_rm)               ; Call real mode interrupt
        mov bx, [esp + pmi_rm_call.error_code]
        lea esp, [esp + pmi_rm_call.strucsize]
        jc .dos_error                   ; MOV and LEA didn't alter flags
        test bx, bx
        jnz .rm_error
```

### `get_irq_hndlr`

Get the current handler of a hardware interrupt (IRQ).

Inputs:

- AL: Number of hardware interrupt (IRQ).

Outputs:

- CF: Set if failed.
- EAX: Error code if CF set.
- AL: Number of hardware interrupt (IRQ) where the request is served. Same as input AL, except for IRQ 2. In that case AL will contain 9, since IRQ 2 is served on IRQ 9 due to chaining of the hardware interrupt controllers.
- CX:EDX: Selector:offset of the current interrupt handler. Jump to this address from your protected mode IRQ handler if you want to reflect the IRQ to real mode.

See the `set_irq_hndlr` service for example.

### `set_irq_hndlr`

Set the current handler of a hardware interrupt (IRQ).

Inputs:

- AL: Number of hardware interrupt (IRQ).
- CX:EDX: Selector:offset of the new interrupt handler. Normally CX is the same as CS for custom IRQ handlers.

Outputs:

- CF: Set if failed.
- EAX: Error code if CF set.
- AL: Number of hardware interrupt (IRQ) where the request is served. Same as input AL, except for IRQ 2. In that case AL will contain 9, since IRQ 2 is served on IRQ 9 due to chaining of the hardware interrupt controllers.

_Example: Install a custom handler for IRQ 0 (timer interrupt). Do some dummy update in video memory to provide visual feedback that it works and reflect back to the real mode handler. Wait for a keystroke, then restore the original IRQ handler._

```nasm
section .code

        %include pmi/api/pmi.inc

        mov [data_sel], ds              ; Save flat data selector for IRQ
        mov al, 0x0
        call pmi(get_irq_hndlr)         ; Get current IRQ 0 handler
        mov [irq0.old_handler], edx     ; Set address for chaining
        mov [irq0.old_handler + 4], cx
        mov cx, cs                      ; CX = CS: handler selector
        mov edx, irq0                   ; EDX: handler offset
        call pmi(set_irq_hndlr)         ; Set new handler

        sub esp, pmi_rm_call.strucsize
        mov byte [esp + pmi_rm_call.flags], PMI_CALL_INT
        mov byte [esp + pmi_rm_call.int], 0x16
        xor ah, ah
        call pmi(call_rm)               ; Wait for a keystroke
        lea esp, [esp + pmi_rm_call.strucsize]

        mov al, 0x0                     ; Restore old IRQ 0 handler
        mov edx, [irq0.old_handler]
        mov cx, [irq0.old_handler + 4]
        call pmi(set_irq_hndlr)

        ...

irq0:
        push ds
        mov ds, cs:[data_sel]
        inc byte [0xb8000]
        pop ds
        jmp 0x1234:0x12345678
        .old_handler EQU $ - 6

        ...

section .data

data_sel        dd 0
```

## Application management services

PMI programs can launch other applications or terminate using these services. The application that is launched by the PMI stub itself is the main application. Its termination will also terminate PMI and return control back to DOS.

### `execute`

Load and run a PE executable. The executable will start with the same environment as the "main" PE executable attached to the PMI stub.

Inputs:

- EBX: Position within the file where the executable starts. Useful for binary bundles when multiple application files are packed into a single file for easier distribution. If the file is a standalone PE file (with or without MZ .EXE stub), EBX should be set to 0. The MZ .EXE stub will be skipped.
- ESI: Linear address of the PE executable's ASCIIZ filename.
- EDI: Linear address of ASCIIZ command line arguments.

Outputs:

- CF: Set if failed.
- AH: Error code if CF set or 0 when successful.
- AL: Exit code if successful or 0 when failed.

_Example: Run `PART2.EXE`._

```nasm
section .text

        %include pmi/api/pmi.inc

        xor ebx, ebx
        mov esi, part_2_exe
        mov edi, null_str
        call pmi(execute)
        jc .execute_error               ; Could not run PART2.EXE
        test al, al
        jnz .runtime_error              ; PART2.EXE returned an error

        ...

section .data

part_2_exe      db 'part2.exe'          ; Terminator 0 follows at null_str

null_str        db 0
```

### `terminate`

Terminate the currently running PMI application. If this was the original application attached to the pmi.exe stub, control will be given back to DOS.

Inputs:

- AL: Program exit code.

Outputs:

None.

This service does not return to the caller.

## Memory handling

Applications with dynamic memory requirements must use these services to allocate and free memory. PMI will only release allocated memory when it returns back to DOS. Applications executed within another PMI program need to free memory themselves before terminating, otherwise the block won't be available for further use.

### `mem_alloc`

Allocate a conventional or extended memory block from the PMI memory
pool.

Inputs:

- AL: Allocation mode. See `PMI_MEM_*` constants below.
- ECX: Size of memory block to allocate in bytes.

Outputs:

- CF: Set if failed.
- EAX: Error code if CF set or linear address of allocated memory block.
- EBX: Physical address of memory block when allocating memory for DMA. Might be identical to linear address.

Possible values for allocation mode (AL):

| value | name             | description |
| ----- | ---------------- | ----------- |
| 0x00  | `PMI_MEM_LO`     | Allocate block from conventional memory pool. |
| 0x01  | `PMI_MEM_LO_HI`  | Allocate block from conventional memory pool when possible. If a suitable block is not available, allocate from extended memory. |
| 0x02  | `PMI_MEM_HI`     | Allocate block from extended memory pool. |
| 0x03  | `PMI_MEM_HI_LO`  | Allocate block from extended memory pool when possible. If a suitable block is not available, allocate from conventional memory. |
| 0x04  | `PMI_MEM_DMA`    | Allocate memory suitable for ISA DMA transfers. The block won't cross a 64 KB boundary and its physical address will be below 16 MB. When possible, the memory block will be allocated in extended memory. |
| 0x05  | `PMI_MEM_DMA_LO` | Same as `PMI_MEM_DMA`, but allocates from conventional memory only (physical address is below 1 MB). |

The returned memory blocks are guaranteed to start on at least a paragraph (16-byte) boundary.

When allocating conventional or DMA memory blocks, the algorithm uses a bottom-up allocation strategy, otherwise it uses a top-down approach. You should normally use `PMI_MEM_HI_LO` for all normal memory block reservations and `PMI_MEM_DMA` for DMA buffers. This reduces the chance that DMA-compliant memory blocks are consumed for non-DMA purposes.

### `mem_free`

Release a memory block previously allocated by `mem_alloc`.

Inputs:

- EAX: Linear address of memory block.

Outputs:

- CF: Set if failed.
- EAX: Error code if CF set.

Attempting to free a memory block which was already freed before will
return as successful, but won't do anything (as long as the block's
internal data structure is intact).

## File operations

PMI is not a DOS extender. File operations are only provided because most of these services had to be implemented for the PE loader anyways and it made sense to provide a minimum working set and expose them as generic services through the API.

All functions accept 32-bit pointers and values which is translated for DOS. Otherwise, the services behave exactly as their orresponding DOS functions.

### `file_open`

Open/create a file for reading and/or writing.

Inputs:

- AL: File access mode, see `PMI_FILE_*` constants below.
- EBX: Linear address of ASCIIZ filename.

Outputs:

- CF: Set if failed.
- EAX: Error code if CF set or file handle when successful.

Possible values for file access mode (AL) are combinations of:

| value | name              | description |
| ----- | ----------------- | ----------- |
| 0x01  | `PMI_FILE_READ`   | Open the file for read only. |
| 0x02  | `PMI_FILE_WRITE`  | Open the file for write only. |
| 0x20  | `PMI_FILE_CREATE` | Create the file if it does not exist yet. Prior to DOS 4.0, the file will be opened with `PMI_FILE_READ` and `PMI_FILE_WRITE` in this case. |
| 0x40  | `PMI_FILE_TRUNC`  | Overwrite the file if it exists. Prior to DOS 4.0, the file will be opened with `PMI_FILE_READ` and `PMI_FILE_WRITE` in this case. |
| 0x80  | `PMI_FILE_COMMIT` | Commit (flush) file after each write. This option requires DOS 4.0 or newer (it's not effective in older DOS versions). |

If neither `PMI_FILE_READ`, nor `PMI_FILE_WRITE` is specified, `PMI_FILE_READ` is assumed.

### `file_close`

Flush buffers and close a previously open file.

Inputs:

- EBX: File handle.

Outputs:

- CF: Set if failed.
- EAX: Error code if CF set.

### `file_read`

Read data from a file opened with `PMI_FILE_READ`.

Inputs:

- EBX: File handle.
- ECX: Number of bytes to read from the file.
- EDI: Linear address of buffer receiving file data.

Outputs:

- CF: Set if failed.
- EAX: Error code if CF set or number of bytes read from the file.

### `file_write`

Write data to a file opened with `PMI_FILE_WRITE`.

Inputs:

- EBX: File handle.
- ECX: Number of bytes to read from the file.
- ESI: Linear address of buffer containing data to write.

Outputs:

- CF: Set if failed.
- EAX: Error code if CF set or number of bytes written to the file.

### `file_set_pos`

Set the read/write position of a file (seek).

Inputs:

- AL: Origin from which the final position is calculated, see `PMI_SEEK_*` constants below.
- EBX: File handle.
- ECX: New read/write position, relative to the origin specified by AL. This is a signed 32-bit value, for example to move position back by 256 bytes, set AL to `PMI_SEEK_CURPOS` and ECX = -256.

Outputs:

- CF: Set if failed.
- EAX: Error code if CF set or new read/write position of the file.

Possible seek origin (AL) constants:

| value | name              | description |
| ----- | ----------------- | ----------- |
| 0x00  | `PMI_SEEK_START`  | Set position relative to start of file. |
| 0x01  | `PMI_SEEK_CURPOS` | Set position relative to current read/write position of the file. |
| 0x02  | `PMI_SEEK_END`    | Set position relative to end of file. |

If the resulting position extends past the end of file, the file will be extended.

### `file_get_pos`

Get the current read/write position of the file.

Inputs:

- EBX: File handle.

Outputs:

- CF: Set if failed.
- EAX: Error code if CF set or current read/write position of the file.

This service is equivalent to `file_set_pos` called with AL = `PMI_SEEK_CURPOS`, ECX = 0. It is provided for convenience.

### `file_get_buf`

File operations use a conventional memory buffer for translation when linear addresses are in extended memory. This buffer is 65520 bytes by default and may be useful for your application when having to call real mode procedures or interrupts.

The buffer is allocated when PMI starts and is not freed unless `file_free_buf` is called explicitly. Once the buffer is freed, it will be allocated again if `file_get_buf` or any other file operation that needs translation is called.

Inputs:

None.

Outputs:

- CF: Set if failed.
- EAX: Error code if CF set or linear address of buffer when successful.
- ECX: Size of the buffer.

This service can fail if the buffer was freed and conventional memory was allocated and not enough memory has been left to allocate the buffer.

### `file_free_buf`

The conventional memory buffer can be freed if your application is not doing any further file operations or needs more conventional memory temporarily. The buffer will be reallocated if `file_get_buf` is called or a file operation needs it to translate extended memory addresses for DOS.

Inputs:

None.

Outputs:

None.

## DMA services

DMA services are not depending on any PMI kernel functionality as of now. They are provided in the API if Virtual DMA Services will be used in the future.

### `dma_start`

Start DMA transfer on a specific DMA channel. You need to setup the
target peripheral before starting the transfer. Use `mem_alloc` with
`PMI_MEM_DMA` or `PMI_MEM_DMA_LO` allocation mode to reserve the DMA buffer.

Inputs:

- EBX: Physical address of the DMA buffer as returned by mem_alloc. The buffer must be word-aligned for 16-bit DMA transfers. Buffers from mem_alloc are aligned at least on paragraph boundary.
- ECX: Number of bytes to transfer from the DMA buffer. Must be an even number for 16-bit DMA transfers. Also EBX + ECX cannot cross a 64 KB boundary for 8-bit or 128 KB boundary for 16-bit DMA transfers.
- DL: DMA channel number (0 - 7).
- DH: DMA transfer mode flags, see `PMI_DMA_*` constants below.

Outputs:

- CF: Set if failed.
- EAX: Error code if CF set.

Possible values for DMA transfer mode flags (DH) are combinations of:

| value | name           | description |
| ----- | -------------- | ----------- |
| 0x04  | PMI_DMA_WRITE  | Data is read from peripheral and written to memory. |
| 0x08  | PMI_DMA_READ   | Data is read from memory and written to peripheral. |
| 0x10  | PMI_DMA_AUTO   | Restart DMA transfer automatically if the end of the DMA buffer is reached. Also known as auto-initialized DMA transfer. |
| 0x20  | PMI_DMA_BACK   | Transfer backwards: DMA transfer starts at the end of the buffer and decrements the address after each transfer. The DMA controller will automatically be programmed so that it starts at the end of the buffer (ie. DMA start address will be EBX + ECX). |
| 0x00  | PMI_DMA_DEMAND | Demand transfer mode. For peripherals which read data in bursts but may suspend transfer in between. Tape drives and newer floppy controllers with FIFO buffers. |
| 0x40  | PMI_DMA_SINGLE | Single transfer mode. For peripherals which cannot buffer data and need it one-by-one. This is the usual mode for for ISA sound cards for playback and recording. |
| 0x80  | PMI_DMA_BLOCK  | Block transfer mode. For peripherals which can buffer entire blocks of data, such as hard disk controllers. |

### `dma_stop`

Stop DMA transfer on a specific DMA channel. The DMA channel will be disabled after this operation until transfer is restarted with the `dma_start` service.

Inputs:

- DL: DMA channel number (0 - 7).

Outputs:

- CF: Set if failed.
- EAX: Error code if CF set.


# PMI runtime library

The PMI runtime library aims to provide a useful set of utilities for PMI applications. The runtime library is basically just a collection of 32-bit Windows format object files (modules) that you can optionally link with your PMI application and make use of the functions defined in that module.

Linkable Runtime library modules are located in the `rtl` folder and the respective API for each module is defined in `src\rtl\api` directory.

Unless otherwise noted, the runtime library functions require DS and ES to be set to the flat data selectors and GS:0 to point to the PMI API jump table.

## `env_arg` module

This module contains functions to work with command line arguments and DOS environment variables.

### `env_arg_setup`

Setup the module before use. The PMI application needs to call this function once before using any of its other functions.

Inputs:

- EBX: Linear address of MS-DOS environment variables as provided at application startup.
- EDI: Linear address of ASCIIZ arguments as provided at application startup.

Outputs:

- CF: Set if failed.
- EAX: Error code if CF set.

### `env_get_value`

Returns the value of a DOS environment variable.

Inputs:

- ESI: Linear address of ASCIIZ uppercase environment variable name to search for.

Outputs:

- CF: Set if environment variable not found.
- ESI: Linear address of ASCIIZ environment variable value if found.

### `arg_get_count`

Get the number of command line arguments.

Inputs:

None.

Outputs:

- CL: Number of command line arguments.

### `arg_get`

Get the value of a command line argument by argument position.

Inputs:

- CL: Index of command line argument (0-based).

Outputs:

- CF: Set if command line argument not found.
- ESI: Linear address of ASCIIZ command line argument value if found.

### `arg_get_value`

Get value of an argument by its name. Can be used to check for existence of an argument by name or to get value when argument is specified in name:value format. If the argument has a switch character (usually / in DOS), it must be included in its name.

Inputs:

- ESI: Linear address of ASCIIZ command line argument name, including switch character if any.

Outputs:

- CF: Set if command line argument not found.
- ESI: Linear address of ASCIIZ command line argument value if found.

## `irq` module

This module can be used to handle hardware interrupt related programming of the PIC (Programmable Interrupt Controller).

### `irq_pic_eoi irq` macro

Use this macro to send an EOI (end of interrupt) signal to the appropriate PIC(s) at the end of your interrupt handler routine.

This macro does not require any segment registers to be set to a specific value.

Macro parameters:

- `irq`: IRQ number (0 - 15), might be a constant, register or memory variable.

Outputs:

- AL: Destroyed.

### `irq_enabled`

Checks whether a hardware interrupt is enabled by the interrupt controller.

Inputs:

- CL: IRQ number (0 - 15).

Outputs:

- CF: Set if failed.
- EAX: Error code if CF set.
- ZF: Set if IRQ is disabled.

### `irq_disable`

Disable a hardware interrupt on the interrupt controller. Disabling IRQ 2 also disables IRQs 8-15.

Inputs:

- CL: IRQ number (0 - 15).

Outputs:

- CF: Set if failed.
- EAX: Error code if CF set.

### `irq_enable`

Enable a hardware interrupt on the interrupt controller.

Inputs:

- CL: IRQ number (0 - 15).

Outputs:

- CF: Set if failed.
- EAX: Error code if CF set.

If IRQ 2 was disabled and the IRQ to enable is between 8 and 15, IRQ 2 will be enabled and all IRQs between 8 and 15 except the one which is to be enabled (CL) will be disabled.

## `keyboard` module

Requires `irq` and `systimer` module.

The keyboard module implements a protected mode AT / PS/2 keyboard handler to prevent mode switches to real mode for handling keyboard events. The module has some interoperability with the BIOS to enable sharing modifier key (Shift/Ctrl/Alt) and keyboard switch (Num/Caps/Scroll lock) states.

### Keycodes

The keyboard module uses keycodes to represent a specific key on the keyboard. The keycode is an 8-bit value, where bits 0-4 correspond to the column and bits 5-7 correspond to the row of the key on a US ASCII standard keyboard, thus it tries to resemble the physical position of a key on an actual keyboard.

Translation of keycodes to ASCII codes is implemented via translation tables for specific keyboard layouts (see `kbd_set_layout` function).

Dedicated keys on keyboard:

| value | name              | key |
| ----- | ----------------- | --- |
| 0x00  | `KC_MM_TRK_PREV`  | Multimedia: previous track |
| 0x01  | `KC_MM_TRK_NEXT`  | Multimedia: next track |
| 0x02  | `KC_MM_MUTE`      | Multimedia: mute |
| 0x03  | `KC_MM_PLAY`      | Multimedia: play |
| 0x04  | `KC_MM_STOP`      | Multimedia: stop |
| 0x05  | `KC_MM_VOL_DOWN`  | Multimedia: volume down |
| 0x06  | `KC_MM_VOL_UP`    | Multimedia: volume up |
| 0x08  | `KC_POWER`        | ACPI: Power |
| 0x09  | `KC_SLEEP`        | ACPI: Sleep |
| 0x0a  | `KC_WAKE`         | ACPI: Wake from sleep |
| 0x10  | `KC_MM_CALC`      | Multimedia: calculator |
| 0x11  | `KC_MM_EXPLORER`  | Multimedia: file browser |
| 0x12  | `KC_MM_EMAIL`     | Multimedia: e-mail client |
| 0x13  | `KC_MM_MEDIA_SEL` | Multimedia: media select |
| 0x18  | `KC_WWW_HOME`     | Browser: homepage |
| 0x19  | `KC_WWW_SEARCH`   | Browser: search |
| 0x1a  | `KC_WWW_FAVS`     | Browser: favorites |
| 0x1b  | `KC_WWW_RELOAD`   | Browser: refresh/reload page |
| 0x1c  | `KC_WWW_STOP`     | Browser: stop |
| 0x1d  | `KC_WWW_FORWARD`  | Browser: forward |
| 0x1e  | `KC_WWW_BACK`     | Browser: back |

Keyboard row 1:

| value | name              | key |
| ----- | ----------------- | --- |
| 0x20  | `KC_ESC`          | Esc |
| 0x21  | `KC_F1`           | F1 |
| 0x22  | `KC_F2`           | F2 |
| 0x23  | `KC_F3`           | F3 |
| 0x24  | `KC_F4`           | F4 |
| 0x25  | `KC_F5`           | F5 |
| 0x26  | `KC_F6`           | F6 |
| 0x27  | `KC_F7`           | F7 |
| 0x28  | `KC_F8`           | F8 |
| 0x29  | `KC_F9`           | F9 |
| 0x2a  | `KC_F10`          | F10 |
| 0x2b  | `KC_F11`          | F11 |
| 0x2c  | `KC_F12`          | F12 |
| 0x2f  | `KC_SYSREQ`       | SysReq |
| 0x30  | `KC_PRINT_SCREEN` | Print Screen |
| 0x31  | `KC_SCROLL`       | Scroll Lock |
| 0x32  | `KC_PAUSE`        | Pause/Break |

Keyboard row 2:

| value | name              | key |
| ----- | ----------------- | --- |
| 0x40  | `KC_BACKTICK`     | ` |
| 0x41  | `KC_1`            | 1 |
| 0x42  | `KC_2`            | 2 |
| 0x43  | `KC_3`            | 3 |
| 0x44  | `KC_4`            | 4 |
| 0x45  | `KC_5`            | 5 |
| 0x46  | `KC_6`            | 6 |
| 0x47  | `KC_7`            | 7 |
| 0x48  | `KC_8`            | 8 |
| 0x49  | `KC_9`            | 9 |
| 0x4a  | `KC_0`            | 0 |
| 0x4b  | `KC_MINUS`        | - |
| 0x4c  | `KC_EQUALS`       | = |
| 0x4d  | `KC_BACKSPACE`    | Backspace |
| 0x50  | `KC_INSERT`       | Insert |
| 0x51  | `KC_HOME`         | Home |
| 0x52  | `KC_PAGE_UP`      | Page Up |
| 0x58  | `KC_NUM`          | Num Lock |
| 0x59  | `KC_KP_SLASH`     | Keypad / |
| 0x5a  | `KC_KP_ASTERISK`  | Keypad * |
| 0x5b  | `KC_KP_MINUS`     | Keypad - |

Keyboard row 3:

| value | name              | key |
| ----- | ----------------- | --- |
| 0x60  | `KC_TAB`          | Tab |
| 0x61  | `KC_Q`            | Q |
| 0x62  | `KC_W`            | W |
| 0x63  | `KC_E`            | E |
| 0x64  | `KC_R`            | R |
| 0x65  | `KC_T`            | T |
| 0x66  | `KC_Y`            | Y |
| 0x67  | `KC_U`            | U |
| 0x68  | `KC_I`            | I |
| 0x69  | `KC_O`            | O |
| 0x6a  | `KC_P`            | P |
| 0x6b  | `KC_BRACKET_OPEN` | [ |
| 0x6c  | `KC_BRACKET_CLS`  | ] |
| 0x6d  | `KC_BACKSLASH`    | \ |
| 0x70  | `KC_DELETE`       | Delete |
| 0x71  | `KC_END`          | End |
| 0x72  | `KC_PAGE_DOWN`    | Page Down |
| 0x78  | `KC_KP_7`         | Keypad 7 |
| 0x79  | `KC_KP_8`         | Keypad 8 |
| 0x7a  | `KC_KP_9`         | Keypad 9 |

Keyboard row 4:

| value | name              | key |
| ----- | ----------------- | --- |
| 0x80  | `KC_CAPS`         | Caps Lock |
| 0x81  | `KC_A`            | A |
| 0x82  | `KC_S`            | S |
| 0x83  | `KC_D`            | D |
| 0x84  | `KC_F`            | F |
| 0x85  | `KC_G`            | G |
| 0x86  | `KC_H`            | H |
| 0x87  | `KC_J`            | J |
| 0x88  | `KC_K`            | K |
| 0x89  | `KC_L`            | L |
| 0x8a  | `KC_SEMICOLON`    | ; |
| 0x8b  | `KC_APOSTROPHE`   | ' |
| 0x8c  | `KC_ENTER`        | Enter |
| 0x98  | `KC_KP_4`         | Keypad 4 |
| 0x99  | `KC_KP_5`         | Keypad 5 |
| 0x9a  | `KC_KP_6`         | Keypad 6 |
| 0x9b  | `KC_KP_PLUS`      | Keypad + |

Keyboard row 5:

| value | name              | key |
| ----- | ----------------- | --- |
| 0xa0  | `KC_SHIFT_LEFT`   | Left Shift |
| 0xa1  | `KC_Z`            | Z |
| 0xa2  | `KC_X`            | X |
| 0xa3  | `KC_C`            | C |
| 0xa4  | `KC_V`            | V |
| 0xa5  | `KC_B`            | B |
| 0xa6  | `KC_N`            | N |
| 0xa7  | `KC_M`            | M |
| 0xa8  | `KC_COMMA`        | , |
| 0xa9  | `KC_DOT`          | . |
| 0xaa  | `KC_SLASH`        | / |
| 0xac  | `KC_INTL`         | Extra key on 102+ key international keyboards |
| 0xad  | `KC_SHIFT_RIGHT`  | Right Shift |
| 0xb1  | `KC_CURSOR_UP`    | Cursor Up |
| 0xb8  | `KC_KP_1`         | Keypad 1 |
| 0xb9  | `KC_KP_2`         | Keypad 2 |
| 0xba  | `KC_KP_3`         | Keypad 3 |

Keyboard row 6:

| value | name              | key |
| ----- | ----------------- | --- |
| 0xc0  | `KC_CTRL_LEFT`    | Left Control |
| 0xc1  | `KC_GUI_LEFT`     | Left GUI (Windows) |
| 0xc2  | `KC_ALT_LEFT`     | Left Alt |
| 0xc6  | `KC_SPACE`        | Space |
| 0xca  | `KC_ALT_RIGHT`    | Right Alt |
| 0xcb  | `KC_GUI_RIGHT`    | Right GUI (Windows) |
| 0xcc  | `KC_MENU`         | (Local) Menu / Apps |
| 0xcd  | `KC_CTRL_RIGHT`   | Right Control |
| 0xd0  | `KC_CURSOR_LEFT`  | Cursor Left |
| 0xd1  | `KC_CURSOR_DOWN`  | Cursor Down |
| 0xd2  | `KC_CURSOR_RIGHT` | Cursor Right |
| 0xd8  | `KC_KP_0`         | Keypad 0 |
| 0xda  | `KC_KP_DOT`       | Keypad . |
| 0xdb  | `KC_KP_ENTER`     | Keypad Enter |

### Modifiers

The state of Shift, Control, Alt, GUI (Windows) keys and toggle keys Num Lock, Caps Lock and Scroll Lock are represented as flags of a 32-bit value.

| value      | name              | description |
| ---------- | ----------------- | ----------- |
| 0x00000001 | `KBD_SCROLL_ON`   | Scroll Lock active (on). |
| 0x00000002 | `KBD_NUM_ON`      | Num Lock active (on). |
| 0x00000004 | `KBD_CAPS_ON`     | Caps Lock active (on). |
| 0x00000008 | `KBD_INSERT_ON`   | Insert mode active (on). |
| 0x00000010 | `KBD_SCROLL`      | Scroll lock key held down. |
| 0x00000020 | `KBD_NUM`         | Num Lock key held down. |
| 0x00000040 | `KBD_CAPS`        | Caps Lock key held down. |
| 0x00000080 | `KBD_INSERT`      | Insert key held down. |
| 0x00000100 | `KBD_SHIFT_LEFT`  | Left Shift key held down. |
| 0x00000200 | `KBD_SHIFT_RIGHT` | Right Shift key held down. |
| 0x00000400 | `KBD_CTRL_LEFT`   | Left Control key held down. |
| 0x00000800 | `KBD_CTRL_RIGHT`  | Right Control key held down. |
| 0x00001000 | `KBD_ALT_LEFT`    | Left Alt key held down. |
| 0x00002000 | `KBD_ALT_RIGHT`   | Right Alt key held down. |
| 0x00004000 | `KBD_GUI_LEFT`    | Left GUI (Windows) key held down. |
| 0x00008000 | `KBD_GUI_RIGHT`   | Right GUI (Windows) key held down. |

In addition, the following masking constants are defined:

| value      | name              | description |
| ---------- | ----------------- | ----------- |
| 0x00000007 | `KBD_LEDS`        | Mask to extract only toggle keys having a LED on the keyboard. |
| 0x000000f0 | `KBD_TOGGLER_KEY` | Mask for toggler key held down states. |
| 0x00000300 | `KBD_SHIFT`       | Mask for held down state of any Shift key. |
| 0x00000c00 | `KBD_CTRL`        | Mask for held down state of any Control key. |
| 0x00003000 | `KBD_ALT`         | Mask for held down state of any Alt key. |
| 0x0000c000 | `KBD_GUI`         | Mask for held down state of any GUI (Windows) key. |
| 0x0000ff00 | `KBD_SHIFTERS`    | Mask for held down state of any Shift, Control, Alt or GUI (Windows) key. |

### `kbd_start`

Start the keyboard module. Real mode code which relies on DOS/BIOS keyboard services won't receive any keystrokes as long as the protected mode handler is installed. Use `kbd_stop` before calling a real mode procedure requiring keyboard input.

Inputs:

None.

Outputs:

- CF: Set if failed.
- EAX: Error code if CF set.

### `kbd_stop`

Stop the keyboard module and restore the original BIOS keyboard IRQ handler redirector.

Inputs:

None.

Outputs:

- CF: Set if failed.
- EAX: Error code if CF set.

### `kbd_set_layout`

Setup a specific keyboard layout. By default, the runtime library uses US ASCII keyboard (single-row Enter key) mappings.

Inputs:

- EAX: Linear address of keyboard ASCII translation table (set to 0 to use US ASCII), see definition below.
- EBX: Linear address of keypad ASCII translation table (set to 0 to use US ASCII), see definition below.

Outputs:

None.

The keyboard layout definition is specified in two ASCII translation tables, one for the main keyboard area and another one for the numeric keypad. The data tables are composed of a zero-terminated array of modifier mask, value and layout offset table doublewords.

| offset | size | description |
| ------ | ---- | ----------- |
| 0x00   | 4    | The value specified here is used as a mask for modifier flags. Current modifier flags are combined with a logical AND with the value specified at this offset. |
| 0x04   | 4    | The value specified here is used as a comparison value with the masked modifier value as calculated above. |
| 0x08   | 4    | Linear address of the keyboard layout table. For the main keyboard, this is a 6x16 byte matrix, where each entry represents ASCII values for keycodes 0x20-0x2f, 0x40-0x4f, 0x60-0x6f, 0x80-0x8f, 0xa0 - 0xaf and 0xc0-0xcf of a given modifier combination. For the numeric keypad, it's a 5x4 byte matrix, where each entry represents ASCII values for keycodes 0x58-0x5b, 0x78-0x7b, 0x98-0x9b, 0xb8-0xbb and 0xd8-0xdb of a given modifier combination. |
|        | N    | The structure above can be repeated to cover multiple modifier key combinations. |
| N*0x0c | 4    | 0 value terminating the table above. |

The keyboard layout table is a 96 byte array for the main keyboard area, where each byte represents ASCII values of given modifier key combinations for the following keycodes:

| offset      | keycode range |
| ----------- | ------------- |
| 0x00 - 0x0f | 0x20 - 0x2f   |
| 0x10 - 0x1f | 0x40 - 0x4f   |
| 0x20 - 0x2f | 0x60 - 0x6f   |
| 0x30 - 0x3f | 0x80 - 0x8f   |
| 0x40 - 0x4f | 0xa0 - 0xaf   |
| 0x50 - 0x5f | 0xc0 - 0xcf   |

For the keypad, the layout table is a 20 byte array, where each byte represents ASCII values of given modifier key combinations for the following keycodes:

| offset      | keycode range |
| ----------- | ------------- |
| 0x00 - 0x03 | 0x58 - 0x5b   |
| 0x04 - 0x07 | 0x78 - 0x7b   |
| 0x08 - 0x0b | 0x98 - 0x9b   |
| 0x0c - 0x0f | 0xb8 - 0xbb   |
| 0x10 - 0x14 | 0xd8 - 0xdb   |

### `kbd_has_event`

Checks whether there is a pending unhandled event in the keyboard event buffer.

Inputs:

None.

Outputs:

- ZF: Set if no event is available, clear otherwise.

### `kbd_get_event`

Removes and returns the oldest pending unhandled event from the keyboard event buffer when available.

Inputs:

None.

Outputs:

- ZF: Set if no event is available, clear otherwise.
- AL: ASCII code of the keyboard event. 0 if the key cannot be translated to an ASCII code.
- AH: Keycode (see `KC_*` constants).
- EBX: State of modifiers at the time of the event (see `KBD_*` constants).
- DL: Keyboard event (see `KBD_EVT_*` constants below).
- DH: Virtual keycode for some non-ASCII keys (see table below).

Possible values for keyboard events:

| value | name             | description |
| ----- | ---------------- | ----------- |
| 0     | `KBD_EVT_REPEAT` | Key held down, keypress generated via typematic repeat. |
| 1     | `KBD_EVT_DOWN`   | Key pressed down. |
| 2     | `KBD_EVT_UP`     | Key released. |

In addition to normal keycodes, this function also returns a virtual keycode which is either identical to the real keycode or a functional equivalent (from user's perspective) of the following keycodes:

| keycode          | virtual keycode   | condition |
| ---------------- | ----------------- | --------- |
| `KC_SHIFT_RIGHT` | `KC_SHIFT_LEFT`   | |
| `KC_CTRL_RIGHT`  | `KC_CTRL_LEFT`    | |
| `KC_ALT_RIGHT`   | `KC_ALT_LEFT`     | |
| `KC_GUI_RIGHT`   | `KC_GUI_LEFT`     | |
| `KC_KP_7`        | `KC_HOME`         | Num Lock inactive (off) or Num Lock active (on) and any of the Shift keys are held down. |
| `KC_KP_8`        | `KC_CURSOR_UP`    | Num Lock inactive (off) or Num Lock active (on) and any of the Shift keys are held down. |
| `KC_KP_9`        | `KC_PAGE_UP`      | Num Lock inactive (off) or Num Lock active (on) and any of the Shift keys are held down. |
| `KC_KP_4`        | `KC_CURSOR_LEFT`  | Num Lock inactive (off) or Num Lock active (on) and any of the Shift keys are held down. |
| `KC_KP_6`        | `KC_CURSOR_RIGHT` | Num Lock inactive (off) or Num Lock active (on) and any of the Shift keys are held down. |
| `KC_KP_1`        | `KC_END`          | Num Lock inactive (off) or Num Lock active (on) and any of the Shift keys are held down. |
| `KC_KP_2`        | `KC_CURSOR_DOWN`  | Num Lock inactive (off) or Num Lock active (on) and any of the Shift keys are held down. |
| `KC_KP_3`        | `KC_PAGE_DOWN`    | Num Lock inactive (off) or Num Lock active (on) and any of the Shift keys are held down. |
| `KC_KP_0`        | `KC_INSERT`       | Num Lock inactive (off) or Num Lock active (on) and any of the Shift keys are held down. |
| `KC_KP_DOT`      | `KC_DELETE`       | Num Lock inactive (off) or Num Lock active (on) and any of the Shift keys are held down. |

### `kbd_get_modifiers`

Get the current value of the keyboard modifiers flag.

Inputs:

None.

Outputs:

- EBX: Current state of modifiers (see `KBD_*` constants).

## `log` module

Requires `string` module.

This module provides basic logging capabilities for PMI applications. The log can be written to a file, standard output or standard error output.

The application may log at the following levels:

| value | name        | description |
| ----- | ----------- | ----------- |
| 0x01  | `LOG_ERROR` | Error-related (fatal) messages. |
| 0x02  | `LOG_WARN`  | Warning (non-fatal error) messages. |
| 0x03  | `LOG_INFO`  | Informational messages about program execution. |
| 0x04  | `LOG_DEBUG` | Detailed data about program execution for debugging. |

The `LOG_LEVEL` environment variable determines during compile time which of the application logs will be compiled into the binary and written to the log. Possible values are:

- None or environment variable not defined: logging disabled.
- `ERROR`: Enable logging of `LOG_ERROR` messages only.
- `WARN`: Enable logging of `LOG_ERROR` and `LOG_WARN` messages.
- `INFO`: Enable logging of `LOG_ERROR`, `LOG_WARN` and `LOG_INFO` messages.
- `DEBUG`: Enable logging of all messages.

Log messages above the specified log level won't be compiled into the .EXE file and won't take up memory and disk space. If logging is disabled, you don't have to link `rtl\log.obj` even if the application uses the `log` macro, since no calls will be made to the module's static code.

### `log command`[, parameters...] macro

This macro acts as a gatekeeper for all logging operations. It takes care of filtering messages above the wanted log level and makes sure to prevent calling any functions defined in `rtl\log.obj` if logging is disabled.

Macro parameters:

- `command`: A character sequence or one of the log level constants (`LOG_*`, see above).
- `parameters`: A list of command-specific parameters separated with comma.

See the following chapters for supported commands.

#### `log start`, logbufsize, flags[, logfile]

Start logging to a target. Log messages won't be printed until this command is issued. A buffer will be allocated for the log messages, preferably from conventional memory (to prevent buffer translation when writing the log to file or console).

Macro parameters:

- `logbufsize`: Size of the log buffer. The maximum length of a log entry is 1 character less, than the size of the buffer (due to the terminating NUL character).
- `flags`: Flags for logging, see `LOG_*` flags below.
- `logfile`: Linear address of ASCIIZ log file to which the log is written if `flags` contains `LOG_FILE`.

Outputs:

- CF: Set if failed.
- EAX: Error code if failed.

Possible values of `flags` are combination of:

| value | name             | description |
| ----- | ---------------- | ----------- |
| 0x00  | `LOG_STDOUT`     | Output log to standard output (console). |
| 0x01  | `LOG_STDERR`     | Output log to standard error output (console). |
| 0x02  | `LOG_FILE`       | Output log to a file. `logfile` parameter is required in this case. |
| 0x04  | `LOG_AUTOCOMMIT` | Don't buffer log contents, always flush the target file after a log entry is written. Requires DOS 4.0 or newer. |
| 0x08  | `LOG_APPEND`     | If the log file already exists, append entries insted of overwriting existing contents. |

#### `log stop`

Stop logging. Any further log messages will be ignored.

#### `log flags`

Dump the low 16-bit word of CPU flags to the log.

#### `log mem`

Dump conventional and extended memory block information to the log.

#### `log` level, "message"[, parameters]

Log a message to the log target specified in `log start`.

Macro parameters:

- `level`: Level associated with the log message (`LOG_ERROR`, `LOG_WARN`, `LOG_INFO` or `LOG_DEBUG`.)
- `message`: Message to print to the log. May be surrounded by curly brackets to include ASCII character codes, such as CR and LF. The format within the curly brackets should follow the declaration for a byte sequence (`db`). The message may contain placeholders as defined in `str_format`.
- `parameters`: Optional parameters for `str_format`. Can be register, constant or memory variable.

## `profiler` module

Requires `systimer` module.

The profiler module allows rough performance profiling of code. It's useful during development or to detect CPU overload situations. It uses `systimer`'s 1024 Hz resolution clock or the timestamp counter on Pentium and newer processors. The profiler's accuracy is in the millisecond range when using `systimer`, and nanosecond range when using the timestamp counter.

You should not make assumptions about the profiler's accuracy. Always compare the ticks elapsed during the routine under profiling against the ticks elapsed during a known timeframe (such as vertical sync, sound card IRQ or `systimer` tick).

The resolution of the timestamp counter can be configured with the `TSC_RESOLUTION` constant in `src\rtl\profiler.asm`. This defines the number of maximum bits used from the timestamp counter for each `systimer` tick. The default value is 11, which allows for about nanosecond resolution.

### `profiler_start`

Sets up and initializes the profiler. Call this at the start of the application.

Inputs:

None.

Outputs:

- CF: Set if failed.
- EAX: Error code if CF is set.

Error `PMI_E_INV_IRQ` is returned if the hardware does not support RTC periodic IRQs (and hence `systimer` and `profiler` are not available.)

This will also start `systimer`. If the application already uses `systimer`, the profiler itself won't cause any processing overhead. Otherwise the overhead of `systimer` applies.

### `profiler_stop`

Stops the profiler.

Inputs:

None.

Outputs:

- CF: Set if failed.
- EAX: Error code if CF is set.

### `profiler_get_counter`

Returns the current number of the profiler tick counter. The counter will roll over after a certain time. Always work with the difference of the current and previous readout. With the default setup, the difference won't roll over until 4 ^ (32 - `TSC_RESOLUTION` + 10) seconds. This is 2048 seconds with the default `TSC_RESOLUTION` configuration.

Inputs:

None.

Outputs:

- EAX: Current value of the profiler performance counter.

The returned value is 0 if the profiler is not available or has not been started.

## `string` module

This module provides a collection of useful utilities to work with ASCIIZ strings.

### `str_len`

Get the number of characters (length) of a string.

Inputs:

- ESI: Linear address of the ASCIIZ string.

Outputs:

- ECX: Number of characters (length) in the string.

### `str_reverse`

Reverse a string in place.

Inputs:

- ESI: Linear address of the ASCIIZ string.

Outputs:

None.

### `str_copy`

Copy a string to a target memory area.

Inputs:

- ECX: Maximum number of characters to copy from the source string. Set it to the size of the target buffer - 1 to prevent overflow if the source string is longer than the destination area.
- ESI: Linear address of the ASCIIZ string to copy.
- EDI: Linear address of the memory area to where the string shall be copied to.

Outputs:

None.

### `str_append`

Append a string to another one, in place.

Inputs:

- ECX: Maximum number of characters in the appended string. Set it to the size of the destination string buffer - 1 to prevent overflow if the concatenated string would be longer than the memory block.
- ESI: Linear address of the ASCIIZ string to append.
- EDI: Linear address of the ASCIIZ string to which the string shall be appended to.

Outputs:

None.

### `str_char_pos`

Get the position of the first occurence of a character in a string.

Inputs:

- AH: Character to search for.
- ECX: Maximum number of characters to search in the string. Set to -1 to search until the end of the string, regardless its size.
- ESI: Linear address of the ASCIIZ string to search.

Outputs:

- CF: Set if the character was not found, clear otherwise.
- EAX: 0-based index of the character's first occurence if found (CF is not set).

### `str_char_rpos`

Get the position of the last occurence of a character in a string.

Inputs:

- AH: Character to search for.
- ECX: Maximum number of characters to search in the string. Set to -1 to search until the end of the string, regardless its size.
- ESI: Linear address of the ASCIIZ string to search.

Outputs:

- CF: Set if the character was not found, clear otherwise.
- EAX: 0-based index of the character's last occurence if found (CF is not set).

### `str_cmp`

Compare strings by ASCII code.

Inputs:

- ECX: Maximum number of characters to compare. Set to -1 to compare the entire strings, regardless of their sizes.
- ESI: Linear address of the source ASCIIZ string to compare.
- EDI: Linear address of the target ASCIIZ string to compare.

Outputs:

- Flags: Set as if two unsigned integers were compared. Use unsigned branch instructions such as `JA`, `JB`, `JE` or `JNE` after the comparison.

### `str_int`

Convert a 32-bit signed or unsigned integer to a string.

Inputs:

- EAX: Integer to convert.
- BL: Conversion mode, see `STR_CV_SIGNED` and `STR_CV_UNSIGNED` constants below.
- ESI: Linear address of buffer receiving ASCIIZ result. It must be at least 11 bytes long for unsigned and 12 bytes for signed integers.

Outputs:

None.

Possible values for conversion mode:

| value | name              | description |
| ----- | ----------------- | ----------- |
| 0     | `STR_CV_UNSIGNED` | Input number is unsigned. |
| 1     | `STR_CV_SIGNED`   | Input number is signed. |

### `str_fixed`

Convert a 32-bit signed or unsigned fixed point number to a string.

Inputs:

- EAX: Number to convert.
- BL: Conversion mode, see `STR_CV_SIGNED` and `STR_CV_UNSIGNED` constants in `str_int`.
- BH: Number of decimals in converted string (last digit will be rounded).
- ECX: Fixed point base, value which represents 1.
- ESI: Linear address of buffer receiving ASCIIZ result. It must be long enough to accomodate the entire string. To be safe, it should be at least 13 bytes (sign, 10 characters integer part, decimal dot, terminator NUL) plus number of decimals specified in BH large.

Outputs:

None.

### `str_hex`

Convert a 32-bit value to a hexadecimal string.

Inputs:

- EAX: Integer to convert.
- BL: Letter casing, see `STR_CV_LOWER` and `STR_CV_UPPER` constants below.
- BH: Number of nibbles to convert from least significant to most significant (0 - 8). Set to 8 to convert the entire value.
- ESI: Linear address of buffer receiving ASCIIZ result. It must be at least 11 bytes long for unsigned and 12 bytes for signed integers.

Outputs:

None.

Possible values for conversion mode:

| value | name           | description |
| ----- | -------------- | ----------- |
| 0     | `STR_CV_LOWER` | Use lowercase letters in converted hexadecimal string. |
| 1     | `STR_CV_UPPER` | Use uppercase letters in converted hexadecimal string. |

### `str_format`

Copy a string with tokens to another memory area while replacing tokens with variables. Tokens are special character sequences enclosed in curly brackets, whose values are provided on the stack.

This function is not particularly optimized. Use individual format conversion functions if performance is important.

Inputs:

- ECX: Maximum number of characters to copy to the target buffer.
- ESI: Linear address of the source ASCIIZ string with tokens to copy.
- EDI: Linear address of buffer receiving ASCIIZ result. It must be long enough to accomodate the entire result string.
- EBP: Linear address of stack area pointing just above the first token value.

Outputs:

None.

Token values are 32-bit entries pushed to the stack. EBP should point to just above the entires. Token values must always be 32-bit, even if the printed value is 8 or 16-bit only. `str_format` will ignore the rest of the upper bits in these cases. For example to print AL, you can safely push EAX and use `{i8}` token to print the 8-bit integer in AL, regardless of the contents of the upper 24 bits in EAX.

Supported tokens in the source string:

- `{u[8|16|32]}`: An unsigned 8/16/32 bit integer number.
- `{i[8|16|32]}`: A signed 8/16/32 bit integer number.
- `{x[8|16|32]}`: Lowercase hexadecimal 8/16/32 bit number.
- `{X[8|16|32]}`: Uppercase hexadecimal 8/16/32 bit number.
- `{q[8|16|32]:base[.precision]}`: Unsigned fixed-point 8/16/32 bit number. `base` is the value representing 1 (must be a 10-base number). The optional `precision` defines the maximum number of decimal digits in the converted string (rounding is applied).
- `{w[8|16|32]:base[.precision]}`: Signed fixed-point 8/16/32 bit number. Parameters are identical to unsigned token version.
- `{c}`: A single character.
- `{s[:length]}`: Linear address of an ASCIIZ string. The optional `length` parameter specifies the maximum number of characters copied from the string to the target buffer.
- `{>}`: Skip token value on the stack without printing it to the target buffer.

_Example: Create a printable string for sound card initialization._

```nasm
section .code

%include "pmi/api/pmi.inc"
%include "rtl/api/string.inc"

%define BUF_LEN 512

        ...

        mov esi, hw_init_msg
        mov edi, buf
        mov ecx, BUF_LEN - 1
        mov ebp, esp
        mov al, [hw_irq]
        push hw_sb                      ; Token 1: address of hw_sb string
        push [hw_port]                  ; Token 2: I/O port address from memory
        push eax                        ; Token 3: IRQ from register low byte
        push 1                          ; Token 4: DMA from constant
        call str_format
        mov esp, ebp                    ; Discard tokens from stack

        ...

section .data

hw_init_msg     db 'Initializing {s} on port {X16}, IRQ {u8}, DMA {u8}', 0
hw_sb           db 'Sound Blaster', 0
hw_port         dw 0x220

section .bss

buf             resb BUF_LEN
```

### `str_parse_int`

Parse a signed integer from a string to a 32-bit integer.

Inputs:

- BL: Terminator character. If this character is found while parsing the number, the number parsed so far will be returned without considering the rest of the string. Set to 0 to parse to the end of the string.
- BH: Maximum number of characters to take into consideration for the conversion.
- ESI: Linear address of the numeric ASCIIZ string to parse.

Outputs:

- CF: Set if the number was invalid (contains a non-numeric character or overflows 32 bits).
- EAX: Parsed number if successful.

### `str_parse_fixed`

Parse a signed number from a string to a 32-bit fixed point value.

Inputs:

- BL: Terminator character. If this character is found while parsing the number, the number parsed so far will be returned without considering the rest of the string. Set to 0 to parse to the end of the string.
- BH: Maximum number of characters to take into consideration for the conversion.
- ECX: Fixed point base, value which represents 1.
- ESI: Linear address of the numeric ASCIIZ string to parse.

Outputs:

- CF: Set if the number was invalid (contains a non-numeric character or overflows 32 bits).
- EAX: Parsed fixed point number if successful.

### `str_parse_hex`

Parse a hexadecimal string to a 32-bit integer.

Inputs:

- BL: Terminator character. If this character is found while parsing the number, the number parsed so far will be returned without considering the rest of the string. Set to 0 to parse to the end of the string.
- BH: Maximum number of characters to take into consideration for the conversion.
- ESI: Linear address of the numeric ASCIIZ string to parse.

Outputs:

- CF: Set if the number was invalid (contains a non-hexadecimal character or overflows 32 bits).
- EAX: Parsed number if successful.

## `systimer` module

Requires `irq` module.

This module uses the real time clock IRQ (8) to generate a periodic tick 1024 times per second. It can be used to measure time for rough code profiling/CPU usage or to schedule asynchronous callbacks for a later time. The IRQ handler itself is tiny and does minimal processing as long as it doesn't have to run any callbacks. The processing overhead is quite small even on lowly 386s, but it is there nonetheless.

### `systimer_start`

Initializes the module, sets up the protected mode real-time clock IRQ handler. The module maintains a reference count, subsequent calls to this function will only increase the number of references.

Inputs:

None.

Outputs:

- CF: Set if failed.
- EAX: Error code if CF set.

Error `PMI_E_INV_IRQ` is returned if the hardware does not support RTC periodic IRQs (and hence `systimer` is not available.)

### `systimer_stop`

Stops the timer. The module maintains a reference count, the timer won't be stopped until the same amount of calls have been made to `systimer_stop` as to `systimer_start` previously.

Inputs:

None.

Outputs:

- CF: Set if failed.
- EAX: Error code if CF set.

### `systimer_ticks`

A 32-bit exported global variable which contains the number of 1/1024 second ticks since `systimer_start` was called. It rolls over after about 2.5 days. If your application is long running, make sure to handle this scenario.

### `systimer_set_timeout`

Register a callback procedure which is executed after a certain amount of systimer ticks elapsed.

Inputs:

- EAX: Number of ticks until the callback is executed.
- EBX: Linear address of the callback procedure.

Outputs:

- CF: Set if no callback slots are available.
- EAX: Handle of the callback (for `systimer_clear_timeout`) or 0 if CF is set.

This function only requires DS to be set to the flat data segment.

Callbacks are a scarce resource, make sure to not overuse them. By default, `systimer` provides 32 callback slots, but this can be increased up to 255 by chaning the `CALLBACK_COUNT` constant in `src\rtl\systimer.asm` and recompiling PMI. Note, that increasing the count will add some processing overhead to the IRQ 8 handler.

Callbacks are called from an IRQ context with interrupts enabled and hence should be designed to comply with all rules regarding IRQ handlers. `systimer` will set DS to flat data segment selector, but other segment registers will be undefined and the handler must set them accordingly when required. By the time the callback is invoked, it is already removed from the callback pool. Make sure the callback is not doing too much processing since it blocks the main thread of your application. It is recommended to set a trigger flag which is watched by the application's main loop and does the heavy lifting.

### `systimer_clear_timeout`

Cancel a scheduled callback procedure.

Inputs:

- EAX: Handle of the callback.

Outputs:

- CF: Set if the handle is invalid or it was already triggered.

This function only requires DS to be set to the flat data segment.

## `timer` module

Requires `irq` module.

This module provides functions to program the PC timer interrupt (IRQ 0) and channel 0 of the programmable interval timer.

### `timer_calc_rate`

The PIT can only generate timer interrupts at certain intervals. This function can be used to determine the nearest actual frequency for a requested interrupt frequency and to get the corresponding PIT reload value for `timer_set_rate`.

Inputs:

- EDX: Requested timer interrupt frequency (Hz).

Outputs:

- EAX: Actual nearest timer interrupt frequency rounded to nearest integer (Hz).
- BX: PIT reload value for `timer_set_rate`.

### `timer_set_rate`

Set the frequency of timer interrupts (IRQ 0).

Inputs:

- BX: PIT reload value as returned by `timer_calc_rate`.

Outputs:

None.

### `timer_reset_rate`

Reset the frequency of timer interrupts (IRQ 0) to the default ~18.2 Hz.

Inputs:

None.

Outputs:

None.

### `timer_start`

Starts an AT BIOS compatible protected mode timer IRQ handler to prevent mode switches to real mode for handling default IRQ 0 activities. This will prevent any custom real mode extensions/TSRs built on the timer interrupt from running.

Inputs:

None.

Outputs:

- CF: Set if failed.
- EAX: Error code if CF is set.

### `timer_stop`

Stops the protected mode timer IRQ handler and restores the original real mode IRQ redirector.

Inputs:

None.

Outputs:

- CF: Set if failed.
- EAX: Error code if CF is set.


# Technical details

## Modes of operation

Depending on the current DOS environment, PMI can operate in one of the following three modes:

- Raw: the processor is in real mode before PMI was started. PMI uses XMS or BIOS extended memory services to allocate the largest extended memory block and uses its own internal mode switch routines. If real mode code (such as DOS services) need to be executed, PMI switches the CPU back to real mode, runs the service, switches back to flat model protected mode and transfers control back to your application. Paging is never enabled in this mode. This is the preferred mode of operation since both real and protected mode are under full control of PMI.

- VCPI: the processor is in V86 mode and a VCPI host is available. This is a common scenario when an EMS manager such as EMM386 is installed. V86 is a special mode of the 80386 and newer processors that simulates a 80386 processor in real mode, while keeping a supervisor's (the VCPI host, like EMM386) finger at the heartbeat of the program running in V86 mode. The supervisor may intercept several actions, such as I/O port or memory access and do funky stuff like emulating hardware or swapping memory in and out of memory ranges. It's not possible to enter protected mode natively in V86 mode. VCPI is a standard interface that enables programs like PMI to request supervisor-level protected mode access and coexist with the current protected mode supervisor and it's V86 mode. Under VCPI, PMI uses XMS or VCPI memory allocation functions to allocate extended memory (XMS is preferred). VCPI normally requires paging, but PMI can disable paging under certain conditions (see below). When running in protected mode and paging disabled, performance is equivalent to raw mode. VCPI is slower in real (V86) mode and mode switches will take somewhat longer (largely depending on the VCPI host).

- DPMI: DPMI is the successor of VCPI. It's a more advanced host for protected mode programs. When the processor runs in V86 mode and VCPI is not available, but DPMI is, PMI will use it for setting up the protected mode environment for the PMI application. DPMI hosts can be wildly different. Some may be only thin wrappers above VCPI or a raw environment and may be very fast while others may be feature-rich solutions with virtual memory, multitasking and hardware virtualization. Needless to say, DPMI is the least preferred mode of operation for PMI. The sole purpose of DPMI support in PMI is to add compatibility with Windows DOS boxes.

A somewhat unique feature (at least I haven't heard of any other DOS extenders doing the same) is the ability to run without paging under VCPI. This mode is only possible if all of the following conditions are met:

- Extended memory can be allocated and locked using XMS memory manager.
- The VCPI host uses identity mapping for memory below 640 KB. In other words, the linear addresses are identical to physical addresses.
- The application is loaded to memory below 640 KB (not using loadhigh).

This might sound restrictive, but when EMS is provided using MS-DOS HIMEM.SYS and EMM386.EXE, the above conditions are met as long as the PMI application is not loaded into UMB (which DOS won't do unless explicitly requested).

## Memory management

PMI was written with performance in mind and was meant for single-tasking environments. It's greedy and will try to allocate all conventional and extended memory during startup to provide these two memory pools for PMI applications so it doesn't have to reach out to memory managers during runtime.

PMI in general supports up to 64 MB of extended memory. PMI was written for DOS assembly retro coding and 64 MB was deemed sufficient. It should not be too hard to add support for more, but PMI was deliberately kept minimalistic. If your application needs more, than 64 MB, you probably want to look somewhere else and use a more advanced DOS extender or consider switching to a different platform.

PMI supports the following methods of extended memory allocations:

- BIOS extended memory services: This mode uses INT 0x15/0x88 to get the amount of extended memory and hooks the interrupt to return 0 for any further requests. Only top-bottom allocation is supported, if bottom-up (VDISK) is detected, PMI will not use extended memory. This allocation method is only used if the CPU is in real mode.

- XMS: This mode uses the XMS manager's XMS 2.0 API to query, allocate and lock the largest extended memory block. If the process fails, PMI won't use extended memory. XMS 2.0 limits the size of the memory block to 64 MB. Adding support for XMS 3.0 should not be too hard, but it wasn't deemed necessary. This allocation method is used when the CPU is in real mode or V86 mode with a VCPI host.

- VCPI: This mode is used if a VCPI host is present and XMS allocation failed. VCPI memory allocation is much slower, than XMS since 4 KB pages are allocated one-by-one using an API call to the VCPI host. This mode also requires paging to be enabled (see below). The maximum amount of extended memory is limited to around 64 MB but this can be changed with PMI compile time configuration parameters. The VCPI host is used to switch between real (V86) and protected mode.

- DPMI: When running in DPMI mode, DPMI is used to query the largest lockable extended memory block which is then allocated and locked during PMI startup. This might be a slow procedure, depending on the DPMI host. The maximum amount of extended memory is limited to around 64 MB but this can be changed with PMI compile time configuration parameters. PMI will not reserve up to 1/16th of lockable region to leave some breathing room to the DPMI server and other applications in a multitasked environment. This can also be changed with a PMI compile time configuration parameter. The DPMI host is used to enter protected mode and then DPMI services are used to simulate real mode far calls and interrupts.

Paging is enabled in VCPI mode if the application is loaded to UMB or if XMS allocation fails. Paging slows down protected mode execution a bit, although it will still be much faster, than V86 mode. PMI stores all page tables in conventional memory, which uses 4 KB for each allocated 4 MB. PMI will make sure to add VCPI page tables under 16 MB physical memory address in ascending physical address to allow reservation of DMA buffers for the `mem_alloc` service. For DMA buffer allocations, the `mem_alloc` service will check page tables to meet ISA DMA compatibility requirements. Page tables are used optimally, the remaining page table entries in the first page table will also be used for extended memory mapping (VCPI host usually only populate about 1/4 of the first page table, leaving room for almost 3 MB of linear address space). Paging is also enabled if XMS allocation succeeds, but the application was loaded into UMB. Page table entries will be generated from the XMS block's physical address in this case. Under DPMI, paging may or may not be enabled, depending on the DPMI host.

Free conventional memory is also allocated for PMI applications during startup. Under DPMI however, PMI will leave up to 1/8 of available conventional memory free for the DPMI host and other applications in a multitasking environment.

## Exception handling

Exceptions are handled by the DPMI host when running under DPMI. Otherwise PMI will capture CPU exceptions and terminate the application for exceptions 6 and 8 - 31. PMI will also restore the video mode before exiting to the one that was active before startup.

PMI's debug version will also print an exception dump before exiting when not running under DPMI. The dump contains:

- The name and hexadecimal number of the exception.
- The CS:EIP pointer and its linear address at the time of the exception.
- The fault code of the exception (when present and not zero).
- Hexadecimal selector, base, limit and access byte of segment registers CS, DS, ES, FS, GS and SS.
- Hexadecimal values of general registers EAX, EBX, ECX, EDX, ESI, EDI, EBP and ESP.
- The top 64 bytes of the stack at the time of the exception.

Applications cannot install custom exception handlers.

## Hardware interrupts

PMI supports 16 hardware interrupts as defined for IBM PC/ATs. Hardware interrupts in DPMI mode are under the control of the DPMI host. From the perspective of the PMI application, the DPMI host's interrupt management is transparent and follows a similar pattern.

When running under PMI's internal protected mode host, hardware interrupts are mapped to interrupts 0xa0 - 0xaf in both protected and real mode. Under DPMI, interrupts are not remapped (or at least the remap is transparent). PMI applications should manage their hardware interrupt handlers using `set_irq_hndlr` and `get_irq_hndlr` services which makes this difference transparent.

Default protected mode IRQ handlers are reflectors, which switch back to real mode and invoke the real mode IRQ handler. While in real mode, it would not make sense to switch to protected mode just to switch back to real mode again to handle the IRQ. Therefore the normal handlers for remapped IRQ interrupts in real mode are simple redirectors which jump to the original handlers at 0x08 - 0x0f and 0x70 - 0x78 to behave as if the PICs were not remapped.

However once a protected mode IRQ handler is installed, these real mode interrupt handlers will be changed to point to a reflector, which switches to protected mode and invokes the protected mode IRQ handler. The custom IRQ handler can reflect the IRQ back to its original real mode handler by jumping to the original protected mode IRQ handler code. When the custom IRQ handler is uninstalled, the corresponding real mode reflector is also restored to the redirector code.

PMI installs a very tiny prologue for IRQs 7 and 15 to handle spurious IRQs. This prologue is also installed under DPMI even though many DPMI hosts already deal with the problem internally. Unfortunately, this is not mandated by the specification, so it wouldn't be safe to not check this scenario under DPMI as well. The overhead is minimal and the frequency of these IRQs is not very high so it's an acceptable tradeoff.

PMI won't setup flat memory model data segment registers for IRQ handlers automatically, the application must do it within the handler itself as needed. Remember that the PMI API requires flat data selectors for DS and ES and the PMI runtime library also requires GS:0 to point to the PMI API jump table. But you should not really call any of them from an IRQ handler.

## System registers, tables

PMI does not support selector manipulation for applications. It's not necessary due to the flat memory model these applications are executed in.

In raw and VCPI mode, PMI sets up only a GDT with the bare minimum of required selectors.

In raw and VCPI mode, PMI manages the protected mode IDT. The IDT itself has entries for all 256 interrupts, but only exceptions and hardware interrupt vectors are used, the rest are dummy pointers to a simple iretd. Hardware interrupt handlers are set up as 32-bit interrupt gates (interrupt flag will be clear on entering the handler), other interrupts are set up as 32-bit task gates (interrupt flag won't be changed).

Task switching is not used by PMI, but a task state segment is provided in VCPI mode for the VCPI host, as required by the VCPI API. How it is used depends on the VCPI host itself.

In DPMI mode, the DPMI host manages the GDT and LDT and it may use only the GDT or both GDT and LDT. PMI sets up the required selectors using DPMI services. IDT and task handling is under the DPMI host's control. PMI uses DPMI services to set hardware interrupt handlers.


# Configure and build

## Changing compile time parameters

PMI can be configured to better conform with the requirements of your application using a few compile time parameters. These parameters are defined in `src\pmi\config.inc`.

- `RM_STACK_SIZE`: Defines the size of the real mode stack in raw and VCPI mode. For DPMI compatibility, this should not really be different from the default `0x200` (512) bytes. If the real mode services don't need such a large stack, you can decrease this value to save a few bytes of conventional memory, although this is not really worth the hassle. If you don't care about DPMI compatibility, you can also increase this value to provide more stack space for real mode procedures.

- `VCPI_MAX_MEM_MB`: Defines the maximum amount of VCPI memory allocated during PMI startup, in megabytes. The default value is `64`. If the VCPI host can provide more, than specified in this parameter and there are available page table entries in the last page table, PMI will keep allocating memory until the entire page table is filled. VCPI hosts usually only use the first 1 MB of the initial page table, so if this value is set to 4 MB, PMI will typically allocate ~7 MB of extended memory. If your application doesn't need 64 MB of extended memory, it might be a good idea to reduce this parameter to improve startup time if XMS memory is not available in VCPI mode.

- `VCPI_MIN_LOW_KB`: The amount of conventional memory to leave free after allocating VCPI memory in kilobytes. This is only effective if paging must be used in VCPI mode, where each allocated 4 MB will consume 4 KB of conventional memory. This may limit the amount of available extended memory if the available conventional memory in DOS is too low (although this is highly unlikely). The default value is `256`.

- `DPMI_MAX_MEM_MB`: The maximum amount of memory allocated and locked in DPMI mode in megabytes. The default value is `64`. If you application does not require so much extended memory, it might be a good idea to reduce this amount to leave more room for other DPMI applications in a multitasking environment. It also reduces the chance of startup failures due to the DPMI host refusing to lock a large amount of extended memory.

- `DPMI_EXT_RSV_MB`: The maximum amount of extended memory which won't be allocated in DPMI mode, even if the DPMI host reports that it's possible. Value is in megabytes, the default is `2`. PMI will only reserve 15/16th of the reported lockable memory. This parameter controls the maximum amount that is not reserved (the 1/16th of reported lockable memory). Set this to 0 to reserve all lockable memory.

- `DPMI_LOW_RSV_KB`: The maximum amount of conventional memory which won't be allocated in DPMI mode, in kilobytes. Defaults to `64`. Similar to `DPMI_EXT_RSV_MB`, but for memory below 1 MB. PMI will only reserve 7/8th of available conventional memory and leave 1/8th, up to the amount defined in this parameter free. Set it to 0 to reserve all conventional memory.

- `DPMI_PREFERRED`: Flag to set host preference in V86 mode when both VCPI and DPMI hosts are available. When set to 0, VCPI mode will be used, when set to 1, PMI will use DPMI mode. Defaults to `0` (VCPI preferred).

- `IO_BUF_SIZE`: The size of the conventional memory I/O buffer used by file operations and the execute service, in bytes. The default value is `0xfff0` (65520). It can be reduced to save conventional memory at the expense of more real mode DOS calls for large file transfers. It can also be increased if your application needs a larger buffer for some reason, but file operations will never use more, than 65520 bytes for data translation.

## Building a custom PMI stub

PMI can be built under DOS and Windows. It should also be possible to build under other platforms, but the current build environment supports these two only.

To build a custom `pmi.exe` binary:

- Install [NASM](https://nasm.us).
- Install [Open Watcom](https://www.openwatcom.org/) tools.
- Copy `makeinit.sam` to `makeinit` and set the following parameters:
  - `nasm`: Path to nasm.exe (NASM binary).
  - `wlink`: Path to wlink.exe (Open Watcom linker).
  - If both of them are added to system `PATH`, you can leave `makeinit` as-is.
- Run `wmake` to create a debug-enabled build as `bin\pmi_dbg.exe` or `wmake release` to create a release build as `bin\pmi.exe`.

To test PMI:
- Copy `test_bat.sam` to `test.bat`
- Under DOS:
  - Run `make.bat` to create a debug-enabled build and execute it.
- Under Windows versions without proper DOS boxes:
  - Install DosBox-X.
  - Copy `emu\env_bat.sam` to `emu\env.bat` and adjust the `dosbox` environment variable to point to the DosBox-X binary according to your install location.
  - Run `makedb.bat` to create a debug-enabled build and execute it under DosBox-X.

PMI should display the following message:

```
:( Cannot load protected mode program
```

To test DPMI mode under DOS, set the `DPMI_PREFERRED` setting in `config.inc` to `1` and uncomment `lib\cwsdpmi` in `test.bat`. You can also try other DPMI hosts, but be aware that many of them have issues with DosBox-X, unrelated to PMI. CWSDPMI seems to work fine. If you run into issues, make sure to test on real hardware or a more accurate emulator such as [Bochs](https://bochs.sourceforge.io/) first.
