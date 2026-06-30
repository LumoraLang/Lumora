extern "C" {
    fn gtk_init(argc: *i32, argv: **&u8);
    fn gtk_window_new(window_type: i32) -> *void;
    fn gtk_window_set_title(window: *void, title: &u8);
    fn gtk_window_set_default_size(window: *void, width: i32, height: i32);
    fn gtk_window_set_position(window: *void, position: i32);
    fn gtk_container_add(container: *void, widget: *void);
    fn gtk_widget_show_all(window: *void);
    fn gtk_main();
    fn gtk_main_quit();
    fn gtk_label_new(str: &u8) -> *void;
    fn gtk_button_new_with_label(label: &u8) -> *void;
    fn gtk_box_new(orientation: i32, spacing: i32) -> *void;
    fn g_signal_connect_data(instance: *void, detailed_signal: &u8, handler: *void, data: *void, destroy_data: *void, connect_flags: i32) -> u64;
}
