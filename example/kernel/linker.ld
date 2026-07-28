ENTRY(_start32)
SECTIONS {
    . = 0x100000;
    .boot : {
        *(.boot)
    }

    . = 0x101000;
    .text : {
        *(.text)
    }

    .rodata : {
        *(.rodata*)
    }

    /DISCARD/ : {
        *(.note*)
        *(.comment)
        *(.eh_frame*)
    }

    . = ALIGN(0x1000);
    .data : {
        *(.data)
    }

    .bss : {
        *(.bss)
        *(COMMON)
    }

    _kernel_end = .;
}
