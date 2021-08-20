# smalang 0.1

.bss

CODE: .quad 0       # pointer to source program
HEAP_A: .quad 0     # pointer to heap A
HEAP_B: .quad 0     # pointer to heap B

.text

# reserved registers
#  - rbx = instruction pointer
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
    

reserve_heap:
    mov rdi, 1048576    # create A heap
    call mmap
    mov [HEAP_A], rax

    mov rdi, 1048576    # create B heap
    call mmap
    mov [HEAP_B], rax

    mov r15, [HEAP_A]   # start with A heap
    add r15, 1048576    # heap grows down, so we start at the top
    ret

reserve_code:   
    mov ecx, [rsp]      # get argc
    cmp ecx, 2
    jne .no_args
    mov rdx, [rsp + 12] # get argv[1]
    ret
.no_args:
    mov rdi, 1
    call exit

exit:
    mov rax, 60
    syscall

.global _start

_start: # entry point
    call reserve_heap   # set up heap
    call reserve_code   # load program

    mov rbp, rsp        # set up stacl

    mov rdi, 0  # exit 0
    call exit
