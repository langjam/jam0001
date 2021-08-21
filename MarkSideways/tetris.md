# Tetris

This is not quite tetris yet.

```
game_create_window("Tetris", 640, 480, 30);

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
this.overlayUsesTranspose = false;
this.linesCleared = 0;
this.fallCounter = 0;

```

### Update Phase

- `events` - list of key presses and releases since the last frame

```

fallCounterStart = 30.0;

if this.overlay == null {
    this.fallCounter = fallCounterStart;
    this.createNewOverlay();
}

if this.fallCounter <= 0 {
    this.fallCounter = fallCounterStart;
    if !this.tryMoveOverlay(0, 1) {
        this.flattenOverlay();
        this.overlay = null;
    }
} else {
    this.fallCounter -= this.getDropRate();
}

```

### TryMoveOverlay

- `dx` - the amount to move the overlay's x coordinate
- `dy` - the amount to move the overlay's y coordinate

Returns a boolean `true` if the move was successful, `false` if it ran into something
like a wall or another piece.

```
if this.overlay == null {
    return true;
}

this.overlayX += dx;
this.overlayY += dy;
if !this.isOverlayValid() {
    this.overlayX -= dx;
    this.overlayY -= dy;
    return false;
}
return true;
```

### Is Overlay Valid?

```
if this.overlay == null {
    return true;
}

for y = 0 till 4 {
    for x = 0 till 4 {
        color = this.overlay[x][y];
        if color > 0 {
            gx = x + this.overlayX;
            gy = y + this.overlayY;
            if gx < 0 || gy < 0 || gx >= 10 || gy >= 20 {
                if this.grid[gx][gy] != 0 {
                    return false;
                }
            }
        }
    }
}

return true;
```

### Get Drop Rate

TODO: make this increase in terms of the lines cleared/level
```
return 1.0;
```

### Flatten Overlay

TODO: this
```
for y = 0 till 4 {
    for x = 0 till 4 {
        color = this.overlay[x][y];
        if color != 0 {
            this.grid[this.overlayX + x][this.overlayY + y] = color;
        }
    }
}
```

### Create New Overlay

```
pieceId = floor(random_float() * 7);
flatOverlay = this.getPiece(pieceId);
this.overlay = makeGrid(4, 4);
this.overlayX = 3;
this.overlayY = 0;
this.overlayUsesTranspose = pieceId < 2;
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
if id == 1 {
    return [
        0, 0, 0, 0,
        0, 1, 1, 0,
        0, 1, 1, 0,
        0, 0, 0, 0,
    ];
}
if id == 2 {
    return [
        0, 1, 0, 0,
        1, 1, 1, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
    ];
}
if id == 3 {
    return [
        0, 2, 0, 0,
        0, 2, 2, 0,
        0, 0, 2, 0,
        0, 0, 0, 0,
    ];
}
if id == 4 {
    return [
        0, 2, 0, 0,
        0, 2, 0, 0,
        0, 2, 2, 0,
        0, 0, 0, 0,
    ];
}
if id == 5 {
    return [
        0, 3, 0, 0,
        3, 3, 0, 0,
        3, 0, 0, 0,
        0, 0, 0, 0,
    ];
}
if id == 6 {
    return [
        0, 3, 0, 0,
        0, 3, 0, 0,
        3, 3, 0, 0,
        0, 0, 0, 0,
    ];
}
return overlay;
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