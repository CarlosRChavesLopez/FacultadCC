.data
# las constantes se definen desde el byte mas bajo
mascara: .byte 0x00 0x00 0xFF 0xFF
pixel: .byte 0x3C 0x4E 0x7A 0x35
pixel_msg: .string " El pixel es:"
mask_msg: .string " La máscara es:"
result_msg: .string " El resultado es:"
.text
main:
	#cargamos la mascara que
	#limpia verde y azul
	lw s0, mascara
	#cargamos el pixel
	lw s1, pixel
    li a7, 4
    la a0, pixel_msg
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
	and s0, s0, s1
    li a7, 4
    la a0, result_msg
    ecall
    mv a0, s0
    li a7, 34
    ecall
    li a7, 93
    li a0, 0
    ecall
