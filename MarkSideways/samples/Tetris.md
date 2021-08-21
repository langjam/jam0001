# Tetris

This is not quite tetris yet.

```
game_create_window("Tetris", 640, 480, 30);

game_instance = TetrisGameInstance.init();

while !game_is_quit() {
    game_fill_screen(40, 40, 40);
    events = game_get_events();
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
this.clearingCounterMax = 30;
this.clearingCounter = null;
this.clearingLines = null;
this.colorThemes = this.generateColorThemes();

```

### Update Phase

- `events` - list of key presses and releases since the last frame

```

if this.clearingCounter != null {
    this.performClearingLineUpdate();
} else {

    for i = 0 till events.length {
        event = events[i];
        if event == 'left:press' {
            this.tryMoveOverlay(-1, 0);
        } else if event == 'right:press' {
            this.tryMoveOverlay(1, 0);
        } else if event == 'up:press' {
            this.overlayRotate(true);
        } else if event == 'space:press' {
            this.overlayRotate(false);
        }
    }

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
            this.checkForClearLines();
        }
    } else {
        this.fallCounter -= this.getDropRate();
        if game_is_key_pressed('down') {
            this.fallCounter -= 10;
        }
    }
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
                return false;
            }
            if this.grid[gx][gy] != 0 {
                return false;
            }
        }
    }
}

return true;
```

### Get Drop Rate

TODO: make this increase in terms of the lines cleared/level
```
level = this.linesCleared / 10;
return 2.0 + level * 0.5;
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

### Overlay Rotate

- `isClockwise` - a boolean indicating if the piece should be rotated clockwise

The implementation here is a transpose of the overlay followed by a flip for clockwise, and a flip followed by a transpose for counter-clocwise. This allows easy swap-based shuffling. If the current piece is marked with `overlayUsesTranspose = true`, then only use transpose without the flip (The I-beam and the square).

```
if this.overlay == null {
    return;
}

if this.overlayUsesTranspose {
    this.overlayTranspose();
    if !this.isOverlayValid() {
        this.overlayTranspose();
    }
    return;
}

if isClockwise {
    this.overlayTranspose();
    this.overlayFlip();
} else {
    this.overlayFlip();
    this.overlayTranspose();
}

if !this.isOverlayValid() {
    if isClockwise {
        this.overlayFlip();
        this.overlayTranspose();
    } else {
        this.overlayTranspose();
        this.overlayFlip();
    }
}
```

### Overlay Transpose

```
for y = 0 till 4 {
    for x = y + 1 till 4 {
        t = this.overlay[x][y];
        this.overlay[x][y] = this.overlay[y][x];
        this.overlay[y][x] = t;
    }
}
```

### Overlay Flip

```
for y = 0 till 4 {
    t = this.overlay[0][y];
    this.overlay[0][y] = this.overlay[2][y];
    this.overlay[2][y] = t;
}
```

### Check for Clear Lines

```
    linesToKeep = [];
    linesToClear = [];
    for y = 0 till 20 {
        hasHoles = false;
        for x = 0 till 10 {
            if this.grid[x][y] == 0 {
                hasHoles = true;
                break;
            }
        }
        if hasHoles {
            linesToKeep.add(y);
        } else {
            linesToClear.add(y);
        }
    }

    if linesToClear.length == 0 {
        return;
    }

    this.clearingLines = linesToClear;
    this.clearingCounter = 0;
```

### Perform Clearing Line Update

```
progress = 1.0 * this.clearingCounter / this.clearingCounterMax;
clearThroughX = floor(progress * 10);
for i = 0 till this.clearingLines.length {
    y = this.clearingLines[i];
    for x = 0 till clearThroughX {
        this.grid[x][y] = 0;
    }
}

if this.clearingCounter == this.clearingCounterMax {
    this.linesCleared += this.clearingLines.length;
    this.removeAndCollapseLines(this.clearingLines);
    this.clearingCounter = null;
    game_set_title("Tetris / Lines: " + this.linesCleared);
    this.clearingLines = null;
} else {
    this.clearingCounter++;
}
```

### Generate Color Themes

```
WHITE = [240, 240, 240];
CERULEAN = [0, 128, 255];
GREEN = [0, 128, 50];
ORANGE = [255, 128, 0];
YELLOW = [255, 240, 0];
RED = [255, 0, 30];
PURPLE = [128, 0, 140];
MAGENTA = [255, 40, 255];
BLUE = [0, 0, 235];
LIME = [50, 255, 0];
BROWN = [128, 64, 0];
TAN = [200, 150, 100];
PINK = [255, 180, 225];
CYAN = [0, 255, 255];

themesByLevel = [
    [CERULEAN, GREEN],
    [ORANGE, YELLOW],
    [RED, PURPLE],
    [BLUE, MAGENTA],
    [GREEN, LIME],
    [BROWN, TAN],
    [PINK, YELLOW],
    [GREEN, PURPLE],
    [BLUE, CYAN],
    [ORANGE, RED],
];

themes = [];
for i = 0 till themesByLevel.length {
    pair = themesByLevel[i];
    colors = [null, WHITE, pair[0], pair[1]];
    darkColors = [null];
    lightColors = [null];
    for j = 1 till colors.length {
        color = colors[j];
        r = color[0];
        g = color[1];
        b = color[2];
        darkColors.add([
            r * 2 / 3, 
            g * 2 / 3, 
            b * 2 / 3]);
        lightColors.add([
            255 - (255 - r) * 2 / 3,
            255 - (255 - g) * 2 / 3,
            255 - (255 - b) * 2 / 3]);
    }
    themes.add([
        colors, darkColors, lightColors
    ]);
}

return themes;
```

### Remove and Collapse Lines

- `lines` - a list of the lines to remove

```
actualLine = 19;
for y = 19 thru 0 {
    keepThisLine = lines.length == 0 || lines[lines.length - 1] != y;
    if !keepThisLine {
        lines.pop();
    }
    if keepThisLine {
        for x = 0 till 10 {
            this.grid[x][actualLine] = this.grid[x][y];
        }
        actualLine--;
    }
}

while actualLine >= 0 {
    for x = 0 till 10 {
        this.grid[x][actualLine] = 0;
    }
    actualLine--;
}
```

### Render Phase

```

tile_size = 20;
grid_width = tile_size * 10;
grid_height = tile_size * 20;
grid_left = (640 - grid_width) / 2;
grid_top = (480 - grid_height) / 2;

level = this.linesCleared / 10;
colors = this.colorThemes[level % this.colorThemes.length];

fullColors = colors[0];
darkColors = colors[1];
lightColors = colors[2];

game_draw_rectangle(grid_left, grid_top, grid_width, grid_height, 0, 0, 0);


px = grid_left;
for x = 0 till 10 {
    py = grid_top;
    for y = 0 till 20 {
        colorId = this.grid[x][y];
        if colorId == 0 && this.overlay != null {
            if 
                x >= this.overlayX && 
                y >= this.overlayY && 
                x < this.overlayX + 4 && 
                y < this.overlayY + 4 {
                colorId = this.overlay[x - this.overlayX][y - this.overlayY];
            }
        }
        if colorId > 0 {
            rgb = fullColors[colorId];
            darkRgb = darkColors[colorId];
            lightRgb = lightColors[colorId];
            
            game_draw_rectangle(px, py, tile_size, tile_size, darkRgb[0], darkRgb[1], darkRgb[2]);
            game_draw_rectangle(px, py, tile_size - 2, tile_size - 2, lightRgb[0], lightRgb[1], lightRgb[2]);
            game_draw_rectangle(px + 2, py + 2, tile_size - 4, tile_size - 4, rgb[0], rgb[1], rgb[2]);
        }
        py += tile_size;
    }
    px += tile_size;
}

```