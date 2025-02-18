global _start
global _exit
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