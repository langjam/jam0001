# smalang 0.1

.bss

CODE: .quad 0       # pointer to source program
HEAP_A: .quad 0     # pointer to heap A
HEAP_B: .quad 0     # pointer to heap B
VARENV: .quad 0     # pointer to env
VCOUNT: .quad 0     # number of entries in env
HALTED: .quad 0     # whether or not we've halted
CSTACK: .quad 0     # space to store return addresses for smalang calls
SSTACK: .quad 0     # space to store base pointers for stack shenanigans
OPCODE: .space 1024 # pointers for all ascii chars
IOBUF: .space 4096  # buffer for io formatting
QBEGIN: .quad 0     # tracks opening address of quoted region
QCOUNT: .quad 0     # tracks quote brackets

.data

MSG_OOM: .asciz "Out of memory!\n"
MSG_USAGE: .asciz "Usage: smal <source file>\n"
MSG_TOO_BIG: .asciz "File too big! Max source size is 1MB.\n"
MSG_TYPERR: .asciz "Type error!\n"
NEWLINE: .asciz "\n"
VAL_FN: .asciz "<native function>"
VAL_UFN: .asciz "<function>"

.text

# reserved registers
#  - rbx = instruction pointer
#  - r12 = value stack pointer
#  - r13 = base value stack
#  - r14 = bottom of heap
#  - r15 = "top" of heap (actually grows down)

mmap:   # rdi = size
    mov rax, 9      # mmap call
    add rdi, 8      # add room for ptr
    mov rsi, rdi
    xor rdi, rdi    # ptr = 0
    mov rdx, 3      # PROT_READ | PROT_WRITE
    mov r10, 34     # MAP_ANONYMOUS | MAP_PRIVATE
    mov r8, -1
    mov r9, 0
    push rdi
    syscall
    pop rdi
    mov [rax], rdi  # store size
    add rax, 8      # return ptr after
    ret

munmap: # rdi = ptr
    mov rax, 11     # munmap call
    sub rdi, 8      # ptr was offset by 8, this is the correct address
    mov rsi, [rdi]  # get size from start of page
    syscall
    ret

open:   # rdi = path
    mov rax, 2      # open
    xor rsi, rsi    # O_RDONLY
    syscall
    ret

panic_strlen:
    cmp BYTE PTR [rsi + rdx], 0
    je .panic_strlen_end
    inc rdx
    jmp panic_strlen
.panic_strlen_end:
    ret

panic:  # rdi = msg
    mov rax, 1      # write
    mov rsi, 1      # stdout
    xchg rdi, rsi   # everything in its rightful place...
    xor rdx, rdx
    call panic_strlen   # grab size of rsi
    syscall
    mov rdi, 1
    call exit

newline:
    mov rax, 1
    mov rdi, 1
    lea rsi, [NEWLINE]
    mov rdx, 1
    syscall
    ret

putv:
    mov rax, rdi
    shr rax, 32
    cmp rax, 1
    je .putv_int
    cmp rax, 2
    je .putv_str
    cmp rax, 3
    je .putv_fn
    cmp rax, 4
    je .putv_ufn
    cmp rax, 5
    je .putv_bool
    ret
.putv_fn:
    mov rdi, 1
    lea rsi, [rip + VAL_FN]
    mov rdx, 17
    mov rax, 1
    syscall
    call newline
    ret
.putv_ufn:
    mov rdi, 1
    lea rsi, [rip + VAL_UFN]
    mov rdx, 10
    mov rax, 1
    syscall
    call newline
    ret
.putv_bool:
    test edi, edi
    jnz .putv_true
    lea rsi, [rip + false_name]
    mov rdx, 5
    jmp .putv_true_or_false
.putv_true:
    lea rsi, [rip + true_name]
    mov rdx, 4
.putv_true_or_false:
    mov rdi, 1
    mov rax, 1
    syscall
    call newline
    ret
.putv_int:
    lea rcx, [IOBUF]
    mov r8, 1
    mov r9, 0
    xor rax, rax
    mov eax, edi
    cmp edi, 0
    jge .putv_int_nonnegative
    mov BYTE PTR [rcx], 45   # '-'
    inc rcx
    neg eax 
.putv_int_nonnegative:
    cmp rax, r8
    jl .putv_int_serialize
    mov rdx, 10
    imul r8, rdx
    inc r9
    jmp .putv_int_nonnegative
.putv_int_serialize:
    mov r10, 10
    push rax
    mov rax, r8
    cdq
    idiv r10
    mov r8, rax
    mov rax, [rsp]
    cdq
    idiv r8
    mov r10, rax
    mov r11, rax
    imul r11, r8
    pop rax
    sub rax, r11
    add r10, 48 # '0'
    mov BYTE PTR [rcx], r10b
    inc rcx 
    dec r9
    cmp r9, 0
    jg .putv_int_serialize
.putv_int_write:
    lea rdx, [IOBUF]
    sub rcx, rdx
    mov rdx, rcx
    mov rdi, 1  # stdout
    lea rsi, [IOBUF]
    mov rax, 1
    syscall
    call newline
    ret
.putv_str:
    xor rax, rax
    mov eax, edi
    add rax, r14
    mov rdx, [rax]
    lea rsi, [rax + 8]
    mov rdi, 1
    mov rax, 1
    syscall
    call newline
    ret

reserve_heap:
    mov rdi, 1048576    # create A heap
    call mmap
    mov [rip + HEAP_A], rax

    mov rdi, 1048576    # create B heap
    call mmap
    mov [rip + HEAP_B], rax
    
    mov r14, [rip + HEAP_A]   # start with A heap
    mov r15, r14
    add r15, 1048576    # heap grows down, so we start at the top
    ret

reserve_stack:
    mov rdi, 1048576
    call mmap
    lea r13, [rax + 1048576]
    mov r12, r13
    ret

reserve_cstack:
    mov rdi, 1048576
    call mmap
    mov [rip + CSTACK], rax
    ret

reserve_sstack:
    mov rdi, 1048576
    call mmap
    mov [rip + SSTACK], rax
    ret

reserve_code:   
    mov ecx, [rsp + 8]  # get argc
    cmp ecx, 2
    jne .no_args
    mov rdi, [rsp + 24] # get argv[1]
    call open           # open argv[1]
    push rax
    
    mov rdi, 1048576    # allocate space for code
    call mmap
    mov [rip + CODE], rax
    
    pop rdi             # read from source fd
    mov rsi, rax
    mov rdx, 1048576
    mov rax, 0
    syscall

    cmp rax, 1048576    # err if source was too large
    jge .too_big

    mov rbx, [rip + CODE]     # otherwise, set our instruction pointer to [CODE]
    ret

reserve_env:
    mov rdi, 1048576
    call mmap
    mov [rip + VARENV], rax
    mov QWORD PTR [rip + VCOUNT], 0
    ret

.too_big:
    lea rdi, [MSG_TOO_BIG]          # source was over 1MB
    call panic
.no_args:
    lea rdi, [MSG_USAGE]            # no args were provided
    call panic

exit:
    mov rax, 60
    syscall

set_range:
    lea rcx, [rip + OPCODE]
    mov [rcx + rsi * 8], rdi
    inc rsi    
    cmp rsi, rdx
    jl set_range
.set_range_end:
    ret

setup_opcode:
    lea rdi, [rip + halt]   # control chars 
    mov rsi, 0
    mov rdx, 8
    call set_range

    lea rdi, [rip + space]  # spaces 
    mov rsi, 8
    mov rdx, 14
    call set_range 

    lea rdi, [rip + halt]   # control chars 
    mov rsi, 14
    mov rdx, 32
    call set_range

    lea rdi, [rip + space]   # single space
    mov rsi, 32
    mov rdx, 33
    call set_range

    lea rdi, [rip + letter] # letters/symbols
    mov rsi, 33
    mov rdx, 40
    call set_range

    lea rdi, [rip + openf] # (
    mov rsi, 40
    mov rdx, 41
    call set_range

    lea rdi, [rip + closef] # )
    mov rsi, 41
    mov rdx, 42
    call set_range

    lea rdi, [rip + letter] # letters/symbols
    mov rsi, 42
    mov rdx, 46
    call set_range

    lea rdi, [rip + fence] # fence (.)
    mov rsi, 46
    mov rdx, 47
    call set_range

    lea rdi, [rip + letter] # letters/symbols
    mov rsi, 47
    mov rdx, 48
    call set_range

    lea rdi, [rip + digit]  # digits
    mov rsi, 48
    mov rdx, 58
    call set_range

    lea rdi, [rip + letter] # letters
    mov rsi, 58
    mov rdx, 91
    call set_range

    lea rdi, [rip + quote] # [
    mov rsi, 91
    mov rdx, 92
    call set_range

    lea rdi, [rip + letter] # letter (\)
    mov rsi, 92
    mov rdx, 93
    call set_range

    lea rdi, [rip + unquote] # ]
    mov rsi, 93
    mov rdx, 94
    call set_range

    lea rdi, [rip + letter] # letters
    mov rsi, 94
    mov rdx, 127
    call set_range

    ret

# type ids:
#  - char = 0
#  - int = 1
#  - string = 2
#  - native func = 3
#  - user func = 4
#  - bool = 5
#  - fence = 6

letter:
digit:
    cmp QWORD PTR [rip + QCOUNT], 0
    jg .char_end
    sub r12, 8      # circumvent push_stack to avoid invoke
    mov [r12], rdi
.char_end:
    ret 

fence:
    push QWORD PTR [rip + CSTACK]
    call space
    pop rax
    cmp rax, QWORD PTR [rip + CSTACK]
    jne .fence_end

    xor rdi, rdi
    mov rdi, 6      # fence object
    shl rdi, 32
    call push_stack

.fence_end:
    ret

openf: # open substack
    push QWORD PTR [rip + CSTACK]
    call space
    pop rax
    cmp rax, QWORD PTR [rip + CSTACK]
    jne .openf_end

    mov rax, QWORD PTR [rip + SSTACK]
    mov [rax], r13
    add QWORD PTR [rip + SSTACK], 8 # save base pointer on s stack
    mov r13, r12
.openf_end:
    ret

closef: # close substack
    push QWORD PTR [rip + CSTACK]
    call space
    pop rax
    cmp rax, QWORD PTR [rip + CSTACK]
    jne .closef_end

    sub QWORD PTR [rip + SSTACK], 8
    mov rax, QWORD PTR [rip + SSTACK]
    mov rax, [rax]
    xchg rax, r13   # restore base pointer, get base pointer in rax

    cmp rax, r12
    jle .closef_end

    # pop then push elements
    call pop_stack
    mov rdi, rax
    call push_stack 
.closef_end:
    ret

halt:
    push QWORD PTR [rip + CSTACK]
    call space
    pop rax
    cmp rax, QWORD PTR [rip + CSTACK]
    jne .halt_end
    sub QWORD PTR [rip + CSTACK], 8
    mov rbx, QWORD PTR [rip + CSTACK]
    mov rbx, [rbx]
    test rbx, rbx
    jnz .halt_end

    # when the return address is nullptr, we exit the program
    mov QWORD PTR [rip + HALTED], 1

    cmp r12, r13
    je .halt_end    # skip printing if stack is empty
    mov rdi, [r12]
    call putv
.halt_end:
    ret

quote:
    push QWORD PTR [rip + CSTACK]
    call space
    pop rax
    cmp rax, QWORD PTR [rip + CSTACK]
    jne .quote_end

    cmp QWORD PTR [rip + QCOUNT], 0
    jne .quote_inner
    mov QWORD PTR [rip + QBEGIN], rbx       # store beginning
.quote_inner:
    inc QWORD PTR [rip + QCOUNT]
.quote_end:
    ret

unquote:
    dec QWORD PTR [rip + QCOUNT]
    cmp QWORD PTR [rip + QCOUNT], 0
    jne .unquote_inner

    mov rdi, [rip + QBEGIN]     # push quoted string
    inc rdi                     # skip opening bracket
    mov rsi, rbx
    sub rsi, rdi
    call alloc_string
    mov rdi, rax
    call push_stack
.unquote_inner:
    ret

# like a memcpy, but iterates inversely through the source string
inv_memcpy:
    test rdx, rdx
    jz .inv_memcpy_end
    mov al, BYTE PTR [rsi]
    mov BYTE PTR [rdi], al
    dec rdx
    dec rsi
    inc rdi
    jmp inv_memcpy
.inv_memcpy_end:
    ret  

is_ref: # eax = type sig
    cmp eax, 2
    je .is_ref_yes
    mov rax, 1    # not a ref
    ret
.is_ref_yes:
    xor rax, rax      # is a ref type
    ret

gc_memcpy:            # we keep rsi, rdi, r14, r15, rdx, rcx alive
    test r8, r8
    jz .gc_memcpy_end
    sub r8, 8
    mov r9, QWORD PTR [rax + r8]
    mov QWORD PTR [rdi + r8], r9
    jmp gc_memcpy
.gc_memcpy_end:
    ret

invoke:
    push rdi   
    push rcx
    xor rcx, rcx
    mov ecx, edi
    lea rdi, [rcx + r14 + 8]    # compute fn ptr
    mov rcx, [rdi]
    call rcx

    pop rcx
    pop rdi
    ret

user_invoke:
    push rdi
    push rcx

    lea rcx, [rbx]  # we save the *current* pointer, possibly re-running this instruction
.user_invoke_nofence:
    mov rbx, QWORD PTR [rip + CSTACK]
    mov [rbx], rcx      # save instruction pointer
    add QWORD PTR [rip + CSTACK], 8
    xor rcx, rcx
    mov ecx, edi
    add rcx, r14
    lea rbx, [rcx + 7]  # pointer to string, minus one to account for inc

    pop rcx
    pop rdi
    ret 

pop_stack:
    mov rax, [r12]
    add r12, 8
    ret

push_stack:
    cmp r12, r13
    je .push_stack_value
    cmp DWORD PTR [r12 + 4], 6  # is the top of the stack a fence?
    je .push_stack_value
    cmp DWORD PTR [r12 + 4], 4  # is the top of the stack a user function?
    je .push_stack_top_ufn
    cmp DWORD PTR [r12 + 4], 3  # is the top of the stack a function?
    jne .push_stack_maybe_fn

    xchg [r12], rdi             # swap function into rdi
    call invoke
    ret
.push_stack_top_ufn:
    xchg [r12], rdi             # swap function into rdi
    call user_invoke
    ret
.push_stack_maybe_fn:
    push rax
    mov rax, rdi
    shr rax, 32     # get type
    cmp rax, 4      # user function call
    je .push_stack_ufn
    cmp rax, 3      # is it a native function?
    jne .push_stack_value
    pop rax

    # we have a function, so we invoke it
    call invoke     # function is already in rdi
    ret
.push_stack_ufn:
    # we have a user
    pop rax
    call user_invoke
    ret
.push_stack_value:
    pop rax
    sub r12, 8
    mov [r12], rdi
    ret 

def_var:    # rdi = name, rsi = value
    push rdi
    push rsi
    call lookup
    test rax, rax
    jnz .def_var_exists

    pop rsi
    pop rdi
    mov rax, [rip + VCOUNT]
    inc QWORD PTR [rip + VCOUNT]
    shl rax, 4
    lea rdx, [rip + VARENV]
    mov rdx, [rdx]  # get the actual varenv ptr
    add rdx, rax 
    mov [rdx], rdi
    mov [rdx + 8], rsi
    ret
.def_var_exists:
    pop rsi
    pop rdi
    mov [rdx + 8], rsi
    ret

lookup_cmp:
    mov rax, rdi
    shr rax, 32
    mov r8, rsi
    shr r8, 32
    cmp rax, r8 # type mismatch
    jne .lookup_cmp_fail

    xor r8, r8
    xor r9, r9
    mov r8d, edi
    mov r9d, esi
    add r8, r14
    add r9, r14
    mov rax, QWORD PTR [r8]
    cmp rax, QWORD PTR [r9]
    jne .lookup_cmp_fail    # size mismatch
    sub rax, 1              # sub one to point to a real char
.lookup_cmp_loop:
    mov sil, BYTE PTR [r8 + rax + 8]
    cmp sil, BYTE PTR [r9 + rax + 8]
    jne .lookup_cmp_fail
    dec rax
    cmp rax, 0
    jge .lookup_cmp_loop
    xor rax, rax
    ret
.lookup_cmp_fail:
    mov rax, 1
    ret

lookup:
    xor rcx, rcx
.lookup_loop:
    cmp rcx, [rip + VCOUNT]
    jge .lookup_fail
    
    mov rax, 16
    imul rax, rcx
    lea rsi, [rip + VARENV]
    mov rsi, [rsi]  # get actual env ptr
    add rsi, rax    # VARENV[rcx * 16]
    mov rdx, rsi
    mov rsi, [rdx]  # get key

    call lookup_cmp
    test rax, rax
    jz .lookup_success

    inc rcx 
    jmp .lookup_loop
.lookup_success:
    mov rax, [rdx + 8]
    ret
.lookup_fail:
    xor rax, rax
    ret 

# perform a pass of the copying collector
do_collection:
    push rsi
    push rdi
    push rcx
    push rdx
    # basic steps of a GC
    # 1. figure out if we are on heap A or B, and set rdi/rsi to the bounds of the other one
    # 2. iterate through the stack, word by word, checking is_ref for every word
    # 3. if is_ref returns true, realloc the object in the other heap and redirect the pointer
    # 4. change r14 and r15 over to the new heap
    cmp r14, [HEAP_A]
    je .do_collection_on_a

    mov rsi, [HEAP_A]   # we are on heap B
    mov rdi, rsi
    add rdi, 1048576
    jmp .do_collection_scan

.do_collection_on_a:
    mov rsi, [HEAP_A]   # we are on heap A
    mov rdi, rsi
    add rdi, 1048576

.do_collection_scan:
    mov rcx, r13        # bounds of the stack, for traversal
    mov rdx, r12

.do_collection_scan_loop:
    cmp rdx, rcx
    jge .do_collection_end

    mov eax, DWORD PTR [rdx + 4]    # get type tag of next word
    call is_ref
    test eax, eax
    jnz .do_collection_scan_inc

    # we found a ref, so we'll copy it
    mov eax, DWORD PTR [rdx]        # get heap offset
    add rax, r14                    # get absolute address from heap ptr
    mov r8, [rax]                   # store size
    sub rdi, r8                     # alloc on other heap

    cmp rdi, rsi                    # check if we're out of memory
    jl .collection_oom_err

    call gc_memcpy

    mov rax, rdi
    sub rax, rsi
    mov DWORD PTR [rdx], eax        # fix up reference
.do_collection_scan_inc:
    add rdx, 8
    jmp .do_collection_scan_loop

.do_collection_end:
    mov r14, rsi
    mov r15, rdi
    pop rdx
    pop rcx
    pop rdi
    pop rsi
    ret
.collection_oom_err:
    lea rdi, [MSG_OOM]
    call panic

space:
    cmp QWORD PTR [rip + QCOUNT], 0
    jg .space_end
    cmp r12, r13
    je .space_end
    # tok_to_string
    # this section goes through the preceding stacked chars and forms a string
    # resulting from them
    mov rcx, r12
.space_loop:
    cmp DWORD PTR [rcx + 4], 0    # loop until we hit a non-char or stack base
    jne .space_done
    cmp rcx, r13
    je .space_done

    add rcx, 8
    jmp .space_loop
.space_done:
    mov rsi, rcx
    cmp rcx, r12
    je .space_end   # empty string
    
    xor rax, rax    # rax = accumulator
    mov rdx, rcx    # rdx = dest
    dec rdx
    sub rcx, 8
.space_assemble_loop:
    cmp rcx, r12
    jl .space_assemble_done
    mov dil, BYTE PTR [rcx]
    mov BYTE PTR [rdx], dil
    dec rdx
    inc rax
    sub rcx, 8
    jmp .space_assemble_loop
.space_assemble_done:
    # at this point:
    #  - rsi points to the beginning of a string
    #  - rdx points to the end of a string
    #  - rax has the length of the string
    #  - rcx should be past the end of the stack
    mov r12, rsi 
    test rax, rax
    jz .space_end
    cmp BYTE PTR [rsi - 1], 48
    jl .space_alloc_string
    cmp BYTE PTR [rsi - 1], 57
    jg .space_alloc_string
    # otherwise our first char is between 0 and 9, so it's a number
    xor rdi, rdi
    dec rsi
.space_number_loop:
    test rax, rax
    jz .space_number_end
    mov ecx, 10
    imul edi, ecx
    xor rcx, rcx
    mov cl, BYTE PTR [rsi]
    sub cl, 48
    add rdi, rcx
    dec rax
    dec rsi
    jmp .space_number_loop
.space_number_end:
    mov rax, 1
    shl rax, 32
    or rdi, rax     # add int type
    jmp .space_push
.space_alloc_string:
    inc rax         # space for null terminator
    lea rcx, [rax + 15]
    and cl, 248     # align 8
    sub r15, rcx    # allocate rcx bytes on heap
    cmp r15, r14   
    mov rdx, rax    # store original size (this is preserved across collections)
    jge .space_alloc_success
    call do_collection
.space_alloc_success:
    mov QWORD PTR [r15], rdx    # save size before string
    lea rdi, [r15 + 8]          # copy string to allocated region
    mov rcx, rsi                # save unmoved rsi
    sub rsi, 1
    call inv_memcpy
    
    mov rdi, r15
    sub rdi, r14
    mov rdx, [r15]
    mov BYTE PTR [r15 + rdx + 7], 0 # null terminator

    mov rcx, 2      # string type
    shl rcx, 32
    or rcx, rdi    # heap offset
.space_lookup:
    mov rdi, rcx
    push rdi
    call lookup
    pop rdi
    test rax, rax
    jz .space_push

    mov rdi, rax    # use looked-up value
.space_push:
    call push_stack # push rdi
.space_end:
    ret

run:
    test rbx, rbx
    jz .halted
    cmp BYTE PTR [rip + HALTED], 0
    jne .halted
    xor rdi, rdi
    mov dil, [rbx]
    lea rax, QWORD PTR [rip + OPCODE]
    mov rax, [rax + rdi * 8]
    call rax
    inc rbx
    jmp run
.halted:
    ret

memcpy: # rdi = dest, rsi = src, rdx = size
    test rdx, rdx
    jz .memcpy_end
    dec rdx
    mov al, BYTE PTR [rsi]
    mov BYTE PTR [rdi], al
    inc rdi
    inc rsi
    jmp memcpy
.memcpy_end:
    ret

alloc_string:  # rdi = string ptr, rsi = size
    inc rsi    # null terminator
    lea rcx, [rsi + 15]
    and cl, 248     # align 8
    sub r15, rcx    # allocate rax bytes on heap
    cmp r15, r14   
    jge .alloc_string_success
    call do_collection
.alloc_string_success:
    mov [r15], rsi
    mov rdx, rsi
    mov rsi, rdi
    lea rdi, [r15 + 8]
    push rdx
    dec rdx
    call memcpy
    pop rdx
    mov BYTE PTR [r15 + rdx + 7], 0
    mov rax, 2
    shl rax, 32
    mov rdx, r15
    sub rdx, r14
    or rax, rdx
    ret    

alloc_fn:   # rdi = fn ptr, rsi = size (at least 8)
    lea rdx, [rsi + 15]
    and dl, 248 # align 8
    sub r15, rdx
    cmp r15, r14   
    jge .alloc_fn_success
    call do_collection
.alloc_fn_success:
    mov QWORD PTR [r15], rdx
    mov QWORD PTR [r15 + 8], rdi
    mov rdx, r15
    sub rdx, r14
    mov rax, 3
    shl rax, 32
    or rax, rdx
    ret

.global _start
_start: # entry point
    call reserve_code   # load program
    call reserve_heap   # set up heap
    call reserve_stack  # set up stack
    call reserve_env    # set up env
    call reserve_cstack # set up call stack
    call reserve_sstack # set up call stack
    call setup_opcode   # populate opcode table
    call prelude        # primitive defs

    mov rbp, rsp        # set up stack
    mov rax, QWORD PTR [rip + CSTACK]
    mov QWORD PTR [rax], 0          # outermost return addr is nullptr 
    add QWORD PTR [rip + CSTACK], 8 # increment callstack pointer by 8

    call run
    call exit

.data

inc_name: .asciz "inc"
dup_name: .asciz "dup"
add_name: .asciz "+"
sub_name: .asciz "-"
mul_name: .asciz "*"
div_name: .asciz "/"
def_name: .asciz "="
fn_name:  .asciz "fn"
true_name: .asciz "true"
false_name: .asciz "false"
and_name: .asciz "and"
or_name:  .asciz "or"
not_name: .asciz "not"
eq_name:  .asciz "=="
neq_name: .asciz "!="
lt_name:  .asciz "<"
leq_name: .asciz "<="
gt_name:  .asciz ">"
geq_name: .asciz ">="
putv_name: .asciz "putv"
if_name: .asciz "if"
in_name: .asciz "in"

.text

inc_native:
    inc DWORD PTR [r12]
    ret

dup_native:
    mov rdi, QWORD PTR [r12]
    call push_stack
    ret

add_native:
    lea rdi, [rip + partial_add_native]
    mov rsi, 16     # alloc new closure with 1 capture
    call alloc_fn
    push rax
    call pop_stack                  # get underlying int (capture)
    mov QWORD PTR [r15 + 16], rax   # store capture in closure
    pop rdi
    call push_stack                 # push our function value
    ret

partial_add_native:
    mov rax, QWORD PTR [rdi + 8]    # get capture
    add DWORD PTR [r12], eax        # add capture to stack
    ret

sub_native:
    lea rdi, [rip + partial_sub_native]
    mov rsi, 16     # alloc new closure with 1 capture
    call alloc_fn
    push rax
    call pop_stack                  # get underlying int (capture)
    mov QWORD PTR [r15 + 16], rax   # store capture in closure
    pop rdi
    call push_stack                 # push our function value
    ret

partial_sub_native:
    mov eax, DWORD PTR [rdi + 8]    # get capture
    sub eax, DWORD PTR [r12]        # subtract stack from capture
    mov DWORD PTR [r12], eax 
    ret

mul_native:
    lea rdi, [rip + partial_mul_native]
    mov rsi, 16     # alloc new closure with 1 capture
    call alloc_fn
    push rax
    call pop_stack                  # get underlying int (capture)
    mov QWORD PTR [r15 + 16], rax   # store capture in closure
    pop rdi
    call push_stack                 # push our function value
    ret

partial_mul_native:
    mov rax, QWORD PTR [rdi + 8]    # get capture
    imul eax, DWORD PTR [r12]       # multiply capture by stack value
    mov DWORD PTR [r12], eax 
    ret

div_native:
    lea rdi, [rip + partial_div_native]
    mov rsi, 16     # alloc new closure with 1 capture
    call alloc_fn
    push rax
    call pop_stack                  # get underlying int (capture)
    mov QWORD PTR [r15 + 16], rax   # store capture in closure
    pop rdi
    call push_stack                 # push our function value
    ret

partial_div_native:
    mov rax, QWORD PTR [rdi + 8]    # get capture
    cdqe
    idiv DWORD PTR [r12]            # divide capture by stack value
    mov DWORD PTR [r12], eax
    ret

def_native:
    lea rdi, [rip + partial_def_native]
    mov rsi, 16     # alloc new closure with 1 capture (name)
    call alloc_fn
    push rax
    call pop_stack                  # get name (capture)
    mov QWORD PTR [r15 + 16], rax   # store capture in closure
    pop rdi
    call push_stack                 # push our function value
    ret

partial_def_native:
    mov rdi, QWORD PTR [rdi + 8]    # get function capture
    push rdi
    call pop_stack
    mov rsi, rax
    pop rdi
    call def_var
    ret

in_native:
    lea rdi, [rip + partial_in_native]
    mov rsi, 16     # alloc new closure with 1 capture (name)
    call alloc_fn
    push rax
    call pop_stack                  # get value (capture)
    mov QWORD PTR [r15 + 16], rax   # store capture in closure
    pop rdi
    call push_stack                 # push our function value
    ret

partial_in_native:
    mov rsi, QWORD PTR [rdi + 8]    # get function capture
    push rsi
    call pop_stack
    mov rdi, rax
    pop rsi
    call def_var
    ret

fn_native:
    lea rdi, [rip + partial_fn_native]
    mov rsi, 16     # alloc new closure with 1 capture (name)
    call alloc_fn
    push rax
    call pop_stack                  # get name (capture)
    mov QWORD PTR [r15 + 16], rax   # store capture in closure
    pop rdi
    call push_stack                 # push our function value
    ret

partial_fn_native:
    mov rdi, QWORD PTR [rdi + 8]    # get function capture
    push rdi
    call pop_stack

    mov rsi, rax    # set type to 4 (user function)
    mov rax, 255
    shl rax, 32
    not rax
    and rsi, rax
    mov rax, 4
    shl rax, 32
    or rsi, rax

    pop rdi
    call def_var
    ret

and_native:
    lea rdi, [rip + partial_and_native]
    mov rsi, 16     # alloc new closure with 1 capture
    call alloc_fn
    push rax
    call pop_stack                  # get underlying int (capture)
    mov QWORD PTR [r15 + 16], rax   # store capture in closure
    pop rdi
    call push_stack                 # push our function value
    ret

partial_and_native:
    mov rax, QWORD PTR [rdi + 8]    # get capture
    and BYTE PTR [r12], al          # and byte by al
    ret

or_native:
    lea rdi, [rip + partial_or_native]
    mov rsi, 16     # alloc new closure with 1 capture
    call alloc_fn
    push rax
    call pop_stack                  # get underlying int (capture)
    mov QWORD PTR [r15 + 16], rax   # store capture in closure
    pop rdi
    call push_stack                 # push our function value
    ret

partial_or_native:
    mov rax, QWORD PTR [rdi + 8]    # get capture
    or BYTE PTR [r12], al          # and byte by al
    ret

not_native:
    not BYTE PTR [r12]
    and DWORD PTR [r12], 1
    ret

lt_native:
    lea rdi, [rip + partial_lt_native]
    mov rsi, 16     # alloc new closure with 1 capture
    call alloc_fn
    push rax
    call pop_stack                  # get underlying int (capture)
    mov QWORD PTR [r15 + 16], rax   # store capture in closure
    pop rdi
    call push_stack                 # push our function value
    ret

partial_lt_native:
    mov rax, QWORD PTR [rdi + 8]    # get capture
    cmp eax, DWORD PTR [r12]
    setl al
    xor rdi, rdi
    mov rdi, 5
    shl rdi, 32
    or dil, al
    mov [r12], rdi
    ret

leq_native:
    lea rdi, [rip + partial_leq_native]
    mov rsi, 16     # alloc new closure with 1 capture
    call alloc_fn
    push rax
    call pop_stack                  # get underlying int (capture)
    mov QWORD PTR [r15 + 16], rax   # store capture in closure
    pop rdi
    call push_stack                 # push our function value
    ret

partial_leq_native:
    mov rax, QWORD PTR [rdi + 8]    # get capture
    cmp eax, DWORD PTR [r12]
    setle al
    xor rdi, rdi
    mov rdi, 5
    shl rdi, 32
    or dil, al
    mov [r12], rdi
    ret

gt_native:
    lea rdi, [rip + partial_gt_native]
    mov rsi, 16     # alloc new closure with 1 capture
    call alloc_fn
    push rax
    call pop_stack                  # get underlying int (capture)
    mov QWORD PTR [r15 + 16], rax   # store capture in closure
    pop rdi
    call push_stack                 # push our function value
    ret

partial_gt_native:
    mov rax, QWORD PTR [rdi + 8]    # get capture
    cmp eax, DWORD PTR [r12]
    setg al
    xor rdi, rdi
    mov rdi, 5
    shl rdi, 32
    or dil, al
    mov [r12], rdi
    ret

geq_native:
    lea rdi, [rip + partial_geq_native]
    mov rsi, 16     # alloc new closure with 1 capture
    call alloc_fn
    push rax
    call pop_stack                  # get underlying int (capture)
    mov QWORD PTR [r15 + 16], rax   # store capture in closure
    pop rdi
    call push_stack                 # push our function value
    ret

partial_geq_native:
    mov rax, QWORD PTR [rdi + 8]    # get capture
    cmp eax, DWORD PTR [r12]
    setge al
    xor rdi, rdi
    mov rdi, 5
    shl rdi, 32
    or dil, al
    mov [r12], rdi
    ret

eq_native:
    lea rdi, [rip + partial_eq_native]
    mov rsi, 16     # alloc new closure with 1 capture
    call alloc_fn
    push rax
    call pop_stack                  # get underlying int (capture)
    mov QWORD PTR [r15 + 16], rax   # store capture in closure
    pop rdi
    call push_stack                 # push our function value
    ret

partial_eq_native:
    mov rax, QWORD PTR [rdi + 8]    # get capture
    cmp eax, DWORD PTR [r12]
    sete al
    xor rdi, rdi
    mov rdi, 5
    shl rdi, 32
    or dil, al
    mov [r12], rdi
    ret

neq_native:
    lea rdi, [rip + partial_neq_native]
    mov rsi, 16     # alloc new closure with 1 capture
    call alloc_fn
    push rax
    call pop_stack                  # get underlying int (capture)
    mov QWORD PTR [r15 + 16], rax   # store capture in closure
    pop rdi
    call push_stack                 # push our function value
    ret

partial_neq_native:
    mov rax, QWORD PTR [rdi + 8]    # get capture
    cmp eax, DWORD PTR [r12]
    setne al
    xor rdi, rdi
    mov rdi, 5
    shl rdi, 32
    or dil, al
    mov [r12], rdi
    ret

putv_native:
    call pop_stack
    mov rdi, rax
    call putv
    ret 

if_native:
    lea rdi, [rip + partial_if_cond_native]
    mov rsi, 24     # alloc new closure with 2 captures
    call alloc_fn
    push rax
    call pop_stack                  # get underlying cond (capture)
    mov QWORD PTR [r15 + 16], rax   # store capture in closure
    pop rdi
    call push_stack                 # push our function value
    ret

partial_if_cond_native:
    call pop_stack
    mov [rdi + 16], rax
    lea rax, [rip + partial_if_body_native]
    mov [rdi], rax

    lea rax, [rdi - 8]
    sub rax, r14
    xor rdi, rdi
    mov rdi, 3
    shl rdi, 32
    or rdi, rax
    call push_stack # push our next function value
    ret

partial_if_body_native:
    mov rdx, [rdi + 8]  # get cond
    mov rcx, [rdi + 16] # get iftrue
    call pop_stack  # get iffalse in rax

    test dl, dl
    jz .if_body    # false
    mov rax, rcx    # use iftrue body
.if_body:
    mov rcx, 255
    shl rcx, 32
    not rcx
    and rax, rcx
    mov rcx, 4
    shl rcx, 32
    or rax, rcx
    
    mov rdi, rax
    call user_invoke
    ret

def_builtin:    # rdi = name, rsi = size, rdx = addr
    push rdx
    call alloc_string
    pop rdi
    mov rsi, 8  # only fn ptr, no closures are built in
    push rax
    call alloc_fn
    mov rsi, rax
    pop rdi
    call def_var
    ret

def_const:    # rdi = name, rsi = size, rdx = addr
    push rdx
    call alloc_string
    pop rsi
    mov rdi, rax 
    call def_var
    ret

prelude:
    # inc
    lea rdi, [rip + inc_name]
    mov rsi, 3
    lea rdx, [rip + inc_native]
    call def_builtin

    # dup
    lea rdi, [rip + dup_name]
    mov rsi, 3
    lea rdx, [rip + dup_native]
    call def_builtin

    # add
    lea rdi, [rip + add_name]
    mov rsi, 1
    lea rdx, [rip + add_native]
    call def_builtin

    # sub
    lea rdi, [rip + sub_name]
    mov rsi, 1
    lea rdx, [rip + sub_native]
    call def_builtin

    # mul
    lea rdi, [rip + mul_name]
    mov rsi, 1
    lea rdx, [rip + mul_native]
    call def_builtin

    # div
    lea rdi, [rip + div_name]
    mov rsi, 1
    lea rdx, [rip + div_native]
    call def_builtin

    # defs
    lea rdi, [rip + def_name]
    mov rsi, 1
    lea rdx, [rip + def_native]
    call def_builtin

    # fn
    lea rdi, [rip + fn_name]
    mov rsi, 2
    lea rdx, [rip + fn_native]
    call def_builtin

    # and
    lea rdi, [rip + and_name]
    mov rsi, 3
    lea rdx, [rip + and_native]
    call def_builtin

    # or
    lea rdi, [rip + or_name]
    mov rsi, 2
    lea rdx, [rip + or_native]
    call def_builtin

    # not
    lea rdi, [rip + not_name]
    mov rsi, 3
    lea rdx, [rip + not_native]
    call def_builtin

    # <
    lea rdi, [rip + lt_name]
    mov rsi, 1
    lea rdx, [rip + lt_native]
    call def_builtin

    # <=
    lea rdi, [rip + leq_name]
    mov rsi, 2
    lea rdx, [rip + leq_native]
    call def_builtin

    # >
    lea rdi, [rip + gt_name]
    mov rsi, 1
    lea rdx, [rip + gt_native]
    call def_builtin

    # >=
    lea rdi, [rip + geq_name]
    mov rsi, 2
    lea rdx, [rip + geq_native]
    call def_builtin

    # ==
    lea rdi, [rip + eq_name]
    mov rsi, 2
    lea rdx, [rip + eq_native]
    call def_builtin

    # !=
    lea rdi, [rip + neq_name]
    mov rsi, 2
    lea rdx, [rip + neq_native]
    call def_builtin

    # true
    lea rdi, [rip + true_name]
    mov rsi, 4
    xor rdx, rdx
    mov rdx, 5  # bool
    shl rdx, 32
    or rdx, 1
    call def_const

    # false
    lea rdi, [rip + false_name]
    mov rsi, 5
    xor rdx, rdx
    mov rdx, 5  # bool
    shl rdx, 32
    call def_const

    # putv
    lea rdi, [rip + putv_name]
    mov rsi, 4
    lea rdx, [rip + putv_native]
    call def_builtin

    # ifs
    lea rdi, [rip + if_name]
    mov rsi, 2
    lea rdx, [rip + if_native]
    call def_builtin

    # in
    lea rdi, [rip + in_name]
    mov rsi, 2
    lea rdx, [rip + in_native]
    call def_builtin

    ret
