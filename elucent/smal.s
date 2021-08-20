# smalang 0.1

.bss

CODE: .quad 0       # pointer to source program
HEAP_A: .quad 0     # pointer to heap A
HEAP_B: .quad 0     # pointer to heap B
VARENV: .quad 0     # pointer to env
VCOUNT: .quad 0     # number of entries in env
HALTED: .quad 0     # whether or not we've halted
CALLBACK: .quad 0   # optional function pointer to call
OPCODE: .space 1024 # pointers for all ascii chars
IOBUF: .space 256   # buffer for io formatting

.data

MSG_OOM: .asciz "Out of memory!\n"
MSG_USAGE: .asciz "Usage: smal <source file>\n"
MSG_TOO_BIG: .asciz "File too big! Max source size is 1MB.\n"
MSG_TYPERR: .asciz "Type error!\n"
NEWLINE: .asciz "\n"

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
    ret
.putv_int:
    lea rcx, [IOBUF]
    cmp rdi, 0
    mov r8, 1
    mov r9, 0
    xor rax, rax
    mov eax, edi
    jge .putv_int_nonnegative
    mov BYTE PTR [rcx], 45   # '-'
    inc rcx
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
    mov rdx, 48
    call set_range

    lea rdi, [rip + digit]  # digits
    mov rsi, 48
    mov rdx, 58
    call set_range

    lea rdi, [rip + letter] # letters
    mov rsi, 58
    mov rdx, 127
    call set_range

    ret

# type ids:
#  - char = 0
#  - int = 1
#  - string = 2
#  - func = 3

letter:
    sub r12, 8
    mov [r12], rdi
    ret

digit:
    sub dil, 48      # minus '0'
    cmp r12, r13
    je .single_digit
    cmp DWORD PTR [r12 + 4], 1
    jne .single_digit
    mov ecx, DWORD PTR [r12]
    mov edx, 10
    imul ecx, edx
    add ecx, edi
    mov DWORD PTR [r12], ecx
    ret
.single_digit:
    mov rcx, 1      # int tag
    shl rcx, 32
    movzx rdi, dil
    or rcx, rdi
    sub r12, 8
    mov [r12], rcx
    ret

halt:
    call space
    mov QWORD PTR [rip + HALTED], 1
    mov rdi, [r12]
    call putv
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

pop_stack:
    add r12, 8
    ret

push_stack:
    sub r12, 8
    mov [r12], rdi
    ret 

def_var:    # rdi = name, rsi = value
    push rsi
    call lookup
    test rax, rax
    jnz .def_var_exists

    mov rax, [rip + VCOUNT]
    inc QWORD PTR [rip + VCOUNT]
    mov rcx, 16
    imul rcx, rax
    lea rdx, [rip + VARENV]
    mov rdx, [rdx]  # get the actual varenv ptr
    add rdx, rcx 
    pop rsi
    mov [rdx], rdi
    mov [rdx + 8], rsi
.def_var_exists:
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
    sub rax, 9              # sub 8 to account for size prefix, and an extra one to point to a real char
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
    lea rcx, [rax + 15]
    and cl, 248     # align 8
    sub r15, rcx    # allocate rcx bytes on heap
    cmp r15, r14   
    mov rdx, rax    # store original size (this is preserved across collections)
    jge .space_alloc_success
    call do_collection
.space_alloc_success:
    mov QWORD PTR [r15], rcx    # save size before string
    lea rdi, [r15 + 8]          # copy string to allocated region
    mov rcx, rsi                # save unmoved rsi
    sub rsi, 1
    call inv_memcpy
    
    mov r12, rcx
    mov rdi, r15
    sub rdi, r14

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

    mov rdi, rax
    mov rcx, rdi
    shr rcx, 32     # get type
    cmp ecx, 3      # is it a function?
    jne .space_push
    
    xor rcx, rcx
    mov ecx, edi
    add rcx, r14
    mov rcx, [rcx + 8]
    call rcx        # call function
    jmp .space_end
.space_push:
    call push_stack # push rdi

    cmp QWORD PTR [CALLBACK], 0
    je .space_end

    xor rax, rax
    xchg rax, QWORD PTR [CALLBACK]
    call rax
.space_end:
    ret

run:
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
    lea rcx, [rsi + 15]
    and cl, 248     # align 8
    sub r15, rcx    # allocate rax bytes on heap
    cmp r15, r14   
    jge .alloc_string_success
    call do_collection
.alloc_string_success:
    mov [r15], rcx
    mov rdx, rsi
    mov rsi, rdi
    lea rdi, [r15 + 8]
    call memcpy
    mov rax, 2
    shl rax, 32
    mov rdx, r15
    sub rdx, r14
    or rax, rdx
    ret    

alloc_fn:   # rdi = fn ptr
    sub r15, 16
    cmp r15, r14   
    jge .alloc_fn_success
    call do_collection
.alloc_fn_success:
    mov QWORD PTR [r15], 16
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
    call setup_opcode   # populate opcode table
    call prelude        # primitive defs

    mov rbp, rsp        # set up stack
    call run
    call exit

.data

inc_name: .asciz "inc"

.text

inc_native:
    inc DWORD PTR [r12]
    ret

prelude:
    # inc
    lea rdi, [rip + inc_name]
    mov rsi, 3
    call alloc_string
    mov rcx, rax
    lea rdi, [rip + inc_native]
    call alloc_fn
    mov rsi, rax
    mov rdi, rcx 
    call def_var

    ret
