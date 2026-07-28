extern "C" {
    fn fopen(path: &u8, mode: &u8) -> *void;
    fn fclose(stream: *void) -> i32;
    fn fread(ptr: *void, size: u64, nmemb: u64, stream: *void) -> u64;
    fn fseek(stream: *void, offset: i64, whence: i32) -> i32;
    fn ftell(stream: *void) -> i64;
    fn malloc(size: u64) -> *void;
    fn free(ptr: *void);
    fn putchar(c: i32) -> i32;
    fn getchar() -> i32;
}
