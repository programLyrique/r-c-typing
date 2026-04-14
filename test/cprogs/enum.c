enum Color
{
    RED,
    GREEN = 4,
    BLUE,
    CYAN = BLUE + 2
};

typedef enum Direction
{
    DIR_LEFT = -1,
    DIR_NONE = 0,
    DIR_RIGHT = 1
} Direction;

enum Mask
{
    MASK_READ = 1,
    MASK_WRITE = 1 << 1,
    MASK_RW = MASK_READ | MASK_WRITE
};

enum Color keep_color(enum Color color)
{
    if (color == RED)
        return GREEN;
    switch (color)
    {
    case BLUE:
        return GREEN;
    default:
        return RED;
    }
}

Direction flip_direction(Direction dir)
{
    if (dir == DIR_LEFT)
        return DIR_RIGHT;
    if (dir == DIR_RIGHT)
        return DIR_LEFT;
    return DIR_NONE;
}

int mask_value()
{
    return MASK_READ;
}