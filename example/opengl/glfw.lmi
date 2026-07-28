extern "C" {
    fn glfwSetErrorCallback(callback: *void) -> *void;
    fn glfwInit() -> i32;
    fn glfwTerminate();
    fn glfwWindowHint(hint: i32, value: i32);
    fn glfwCreateWindow(width: i32, height: i32, title: &u8, monitor: *void, share: *void) -> *void;
    fn glfwDestroyWindow(window: *void);
    fn glfwSetKeyCallback(window: *void, callback: *void) -> *void;
    fn glfwMakeContextCurrent(window: *void);
    fn glfwSwapInterval(interval: i32);
    fn glfwGetFramebufferSize(window: *void, width: *i32, height: *i32);
    fn glfwSwapBuffers(window: *void);
    fn glfwPollEvents();
    fn glfwWindowShouldClose(window: *void) -> i32;
    fn glfwSetWindowShouldClose(window: *void, value: i32);
    fn glfwGetTime() -> f64;
}

const GLFW_CONTEXT_VERSION_MAJOR: i32 = 0x22002;
const GLFW_CONTEXT_VERSION_MINOR: i32 = 0x22003;
const GLFW_OPENGL_PROFILE: i32 = 0x22008;
const GLFW_OPENGL_CORE_PROFILE: i32 = 0x32001;
const GLFW_KEY_ESCAPE: i32 = 256;
const GLFW_PRESS: i32 = 1;
const GLFW_TRUE: i32 = 1;
