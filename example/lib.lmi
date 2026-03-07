extern "C" {
    fn printf(fmt: *u8, ...) -> i32;
    fn malloc(size: u64) -> *void;
    fn free(ptr: *void);
    fn exit(code: i32);
}
