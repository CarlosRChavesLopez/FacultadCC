.data
# las constantes se definen desde el byte mas bajo
mascara: .byte 0xDD 0xCC 0xBB 0xAA
mascara2: .word 0xAABBCCDD
mask_msg: .string " La mascara 1 es:"
mask2_msg: .string " La mascara 2 es:"
.text
main:
    li a7, 4
    la a0, mask_msg
    ecall
	#cargamos la mascara 
    lw a0, mascara
    li a7, 34
    ecall
    li a7, 4
    la a0, mask2_msg
    ecall
	#cargamos la mascara 
    lw a0, mascara2
    li a7, 34
    ecall
    li a7, 93
    li a0, 0
    ecall
