// Bitwise operators: &, |, ^, ~

#include <stdio.h>

void print_bits_ln(char b) {
    printf("0b");
    for (int i = 8; i-- > 0 ; ) {
        printf("%i", (b >> i) & 1);
    }
    printf("\n");
}

int main() {
    char a = 0xAA; // = 0b10101010;
    char b = 0xF0; // = 0b11110000;

    printf("a | b = ");
    print_bits_ln(a | b);   // should be 0xFA = 0b11111010

    printf("a & b = ");
    print_bits_ln(a & b);  // should be 0xA0 = 0b10100000

    printf("a ^ b = ");
    print_bits_ln(a ^ b);   // should be 0x5A = 0b01011010

    printf("  ~a  = ");
    print_bits_ln(~a);      // should be 0x55 = 0b01010101
}
