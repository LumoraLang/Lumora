extern "C" {
    fn glViewport(x: i32, y: i32, w: i32, h: i32);
    fn glClearColor(r: f32, g: f32, b: f32, a: f32);
    fn glClear(mask: i32);
    fn glBegin(mode: i32);
    fn glEnd();
    fn glColor3f(r: f32, g: f32, b: f32);
    fn glVertex2f(x: f32, y: f32);
    fn glMatrixMode(mode: i32);
    fn glLoadIdentity();
    fn glRotatef(angle: f32, x: f32, y: f32, z: f32);
    fn glOrtho(left: f64, right: f64, bottom: f64, top: f64, nearVal: f64, farVal: f64);
}

const GL_COLOR_BUFFER_BIT: i32 = 0x00004000;
const GL_TRIANGLES: i32 = 0x0004;
const GL_MODELVIEW: i32 = 0x1700;
const GL_PROJECTION: i32 = 0x1701;
