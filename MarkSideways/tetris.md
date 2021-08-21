# Tetris

This is not quite tetris yet. This is just a test for integrating PyGame (SDL2) into the interpreter runtime.

```
game_create_window("Game Demo", 640, 480, 30);

x = 0;
y = 0;

while !game_is_quit() {
    x += 2;
    y += 1;
    game_fill_screen(40, 40, 40);
    game_draw_rectangle(x, y, 50, 50, 255, 0, 0);
    game_end_frame();
}
```
