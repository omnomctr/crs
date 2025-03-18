global _start
global _exit
global write
extern main

section .data

section .text
_start:
    call main

    mov edi, eax
    ; go into _exit

_exit:
    ; edi is already in its place
    mov eax, 60 ; exit syscall
    syscall

_write: ; void write(unsigned int, const char *, size_t);
    mov rax, 1
    syscall
    ret
