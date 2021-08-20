# smalang 0.1

.bss

CODE: .quad 0       # pointer to source program
HEAP_A: .quad 0     # pointer to heap A
HEAP_B: .quad 0     # pointer to heap B
HALTED: .quad 0     # whether or not we've halted
OPCODE: .space 1024 # pointers for all ascii chars

PRIMS:

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
.too_big:
    mov rdi, 2          # source was over 1MB
    call exit
.no_args:
    mov rdi, 1          # no args provided; do nothing
    call exit

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
    mov QWORD PTR [rip + HALTED], 1
    mov rdi, [r12]
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

do_collection:
    ret

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
    #  - rdx should be at the end of the stack
    lea rcx, [rax + 15]
    and cl, 248   # align 8
    sub r15, rcx    # allocate rax bytes on heap
    cmp r15, r14   
    jge .space_alloc_success
    call do_collection
.space_alloc_success:
    mov QWORD PTR [r15], rcx    # save size before string
    lea rdi, [r15 + 8]          # copy string to allocated region
    sub rcx, 8
    mov rdx, rcx
    mov rcx, rsi                # save unmoved rsi
    call inv_memcpy
    
    lea r12, [rcx - 8]
    mov rdi, r15
    sub rdi, r14
    mov DWORD PTR [r12 + 4], 2  # string type
    mov DWORD PTR [r12], edi    # heap offset
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

.global _start
_start: # entry point
    call reserve_code   # load program
    call reserve_heap   # set up heap
    call reserve_stack  # set up stack
    call setup_opcode   # populate opcode table

    mov rbp, rsp        # set up stack
    call run
    call exit
