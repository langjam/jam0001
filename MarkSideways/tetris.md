# Tetris

This is not quite tetris yet. This is just a test for integrating PyGame (SDL2) into the interpreter runtime.

```
game_create_window("Game Demo", 640, 480, 30);

game_instance = TetrisGameInstance.init();

while !game_is_quit() {
    game_fill_screen(40, 40, 40);
    events = [];
    game_instance.updatePhase(events);
    game_instance.renderPhase();
    
    
    game_end_frame();
}
```

### Make Grid

- `width` - width of the grid
- `height` - height of the grid

```
cols = [];
for x = 1 thru width {
    col = [];
    for y = 1 thru height {
        col.add(0);
    }
    cols.add(col);
}
return cols;
```

## Tetris Game Instance

```
this.grid = makeGrid(10, 20);
this.overlay = null;
this.overlayX = 0;
this.overlayY = 0;

for x = 0 till this.grid.length {
    for y = 0 till this.grid[0].length {
        this.grid[x][y] = (x + y) % 4;
    }
}

this.fallCounter = 0;

```

### Update Phase

- `events` - list of key presses and releases since the last frame

```

if this.fallCounter <= 0 {
    this.fallCounter = 30.0;
    this.createNewOverlay();
}

```

### Flatten Overlay

TODO: this
```

```

### Create New Overlay

```
pieceId = floor(random_float() * 7);
flatOverlay = getPiece(pieceId);
this.overlay = makeGrid(4, 4);
this.overlayX = 3;
this.overlayY = 0;
for y = 0 till 4 {
    for x = 0 till 4 {
        this.overlay[x][y] = flatOverlay[y * 4 + x];
    }
}
```

### Get Piece

- `id` - The ID# of the piece, a number between 0 and 6 inclusive
```
overlay = makeGrid(4, 4);
if id == 0 {
    return [
        0, 1, 0, 0,
        0, 1, 0, 0,
        0, 1, 0, 0,
        0, 1, 0, 0,
    ];
}
```

### Render Phase

```

tile_size = 20;
grid_width = tile_size * 10;
grid_height = tile_size * 20;
grid_left = (640 - grid_width) / 2;
grid_top = (480 - grid_height) / 2;

game_draw_rectangle(grid_left, grid_top, grid_width, grid_height, 0, 0, 0);

color1 = [255, 255, 255];
color2 = [0, 128, 255];
color3 = [0, 128, 40];

colors = [null, color1, color2, color3];

px = grid_left;
for x = 0 till 10 {
    py = grid_top;
    for y = 0 till 20 {
        color = this.grid[x][y];
        if color == 0 && this.overlay != null {
            if 
                x >= this.overlayX && 
                y >= this.overlayY && 
                x < this.overlayX + 4 && 
                y < this.overlayY + 4 {

                color = this.overlay[x - this.overlayX][y - this.overlayY];
            }
        }
        if color > 0 {
            rgb = colors[color];
            game_draw_rectangle(px, py, tile_size, tile_size, rgb[0], rgb[1], rgb[2]);
        }
        py += tile_size;
    }
    px += tile_size;
}

```