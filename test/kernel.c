
// Disable standard library includes
#define NULL ((void*)0)

// Function to write a character to VGA text mode buffer
void print_char(char c, int row, int col) {
    char* vga_buffer = (char*)0xB8000;
    int offset = (row * 80 + col) * 2;
    vga_buffer[offset] = c;
    vga_buffer[offset + 1] = 0x0F; // White on black
}

// Function to print a string to VGA text mode buffer
void print_string(const char* str, int row, int col) {
    int i = 0;
    while (str[i] != '\0') {
        print_char(str[i], row, col + i);
        i++;
    }
}

// External Lumora function to get the message
extern const char* get_hello_message();

// Kernel entry point
void _start() {
    // Clear screen (optional, but good practice)
    for (int i = 0; i < 25 * 80; i++) {
        print_char(' ', 0, i);
    }

    const char* message = get_hello_message();
    print_string(message, 0, 0); // Print at top-left corner

    // Hang indefinitely
    while (1) {}
}
