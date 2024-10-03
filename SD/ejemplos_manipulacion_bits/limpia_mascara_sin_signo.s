.data
mascara1: .byte 0x00 0x00 0xFF 0x00
mascara2: .byte 0x00 0x00 0x00 0xFF
dato: .byte 0xF0 0x0F 0xFE 0x3C
dato2_msg: .string "El dato 2 es:"
dato3_msg: .string "El dato 3 es:"
result_msg: .string "El resultado es:"
.text
main:
    #cargamos el dato
    lw a0, dato
    #cargamos las mascara
    lw a1, mascara1
    lw a2, mascara2
    jal limpiar_dato_sin_signo
    li a7, 93
    li a0, 0
    ecall

limpiar_dato_sin_signo:
    #vamos a usar s0, s1, s2
    #asi que los guardamos
    addi sp, sp, -12
    sw s0, 8(sp)
    sw s1, 4(sp)
    sw s2, 0(sp)
    #limpiamos y sumamos
    and s0, a0, a1
    srli s0, s0, 16
    and s1, a0, a2
    srli s1, s1, 24
    add s2, s0, s1
    li a7, 4
    la a0, dato2_msg
    ecall 
    mv a0, s0
    li a7, 34
    ecall
    li a7, 4
    la a0, dato3_msg
    ecall 
    mv a0, s1
    li a7, 34
    ecall
    li a7, 4
    la a0, result_msg
    ecall 
    mv a0, s2
    li a7, 34
    ecall
    mv a0, s0
    #reponemos registros preservados
    #y estado de pila
    sw s0, 8(sp)
    sw s1, 4(sp)
    sw s2, 0(sp)
    addi sp, sp, 12
    jr ra
    
