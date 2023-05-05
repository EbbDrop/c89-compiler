#include <stdio.h>

int main() {
    // All escape sequences from C are supported
    const char* escapes = " \" \n \r \t \f \v \b \a \\ \? \' ";
    // Octal escapes in string are also supported
    const char* octal_escapes = "\3 \23 \77 \239 \0003";
    // And hex escapes are supported as well
    const char* hex_escapes = "\x0A\x54\x38\64\x0a\x0003";

    // Warnings are given when the escapes value doesn't fit in a char (byte)
    const char* out_of_range_esacpes
        = "\xFFFF \x101 \x23798 \777";

    // Also warns when string contains embedded zero bytes
    const char* embedded_zero = "Hello \0 \x0 \x0000 world!";

    return 0;
}
