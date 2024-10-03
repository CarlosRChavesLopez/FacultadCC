.data
# las constantes se definen desde el byte mas bajo
mascara: .byte 0xFF 0xFF 0xFF 0xFF
dato: .byte 0xF0 0x0F 0xFF 0x00
word_msg: .string " La palabra es:"
result_msg: .string " El resultado es:"
mask_msg: .string " El resultado es:"
.text
main:
	#cargamos la mascara de todos unos
	lw s0, mascara
	#cargamos el dato
	lw s1, dato
    li a7, 4
    la a0, word_msg
    ecall
    mv a0, s1
    li a7, 34
    ecall
    li a7, 4
    la a0, mask_msg
    ecall
    mv a0, s0
    li a7, 34
    ecall
	xor s0, s0, s1
    li a7, 4
    la a0, result_msg
    ecall
    mv a0, s0
    li a7, 34
    ecall
    li a7, 93
    li a0, 0
    ecall
