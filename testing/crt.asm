global _start
extern main

section .data

section .text
_start:
    call main

    mov edi, eax
    mov eax, 60 ; exit syscall
    syscall
