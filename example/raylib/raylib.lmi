extern "C" {
    fn InitWindow(width: i32, height: i32, title: &u8);
    fn CloseWindow();
    fn WindowShouldClose() -> bool;
    fn BeginDrawing();
    fn EndDrawing();
    fn ClearBackground(color: i32);
    fn DrawText(text: &u8, x: i32, y: i32, fontSize: i32, color: i32);
}
