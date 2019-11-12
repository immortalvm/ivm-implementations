### Render 5.12 s video at 50 FPS, 256 x 128 resolution and 44100 Hz sample
### rate. Observe that when a pixel is not set, its color is undefined. In
### particular, this is the case for the whole bottom left half of each frame.

    IMPORT ex5_write_decimal_number/write_decimal_number

    fps = 50
    frames = 256                # 5.12 seconds

    ratio = 2
    width = frames
    height = (/u frames ratio)

    sample_rate = 44100
    base_volume = 1024
    volume_factor = (/u 8192 frames) # Max_volume: 1024 + 8192
    left_step = 50
    right_step = 75

### "Local variables"
    push* [0 0 0 base_volume (+ base_volume (* frames volume_factor)) 0 0 0]
    i = 7                       # Frame counter
    x = 6
    y = 5

    left_volume = 4
    right_volume = 3
    left = 2
    right = 1
    j = 0                       # Sample counter (within frame)

per_frame:
    new_frame* [width height sample_rate]

    ## Print frame number to terminal
    push! $i
    call! write_decimal_number
    set_sp! &1

### Image
    store8!! 0 &x
per_column:
    store8!! 0 &y
per_row:
    R = (+ 255 -$i)
    G = (+ $i -$x)
    B = (+ $i -(* $y 2))
    set_pixel* [$x $y R G B]
    store8!! (+ $y 1) &y
    jump_not_zero!! (<=u (* $y ratio) $x) per_row
    store8!! (+ $x 1) &x
    jump_not_zero!! (<=u $x $i) per_column

### Sound
    store8!! (/u sample_rate fps) &j # Count down from 44100/50.
per_sample:
    store8!! (+ $left left_step) &left
    jump_not_zero!! (<s $left $left_volume) left_ok
    store8!! -$left &left       # Does this makes sense?
left_ok:
    store8!! (+ $right right_step) &right
    jump_not_zero!! (<s $right $right_volume) right_ok
    store8!! -$right &right
right_ok:
    add_sample!! $left $right
    store8!! (+ $j -1) &j
    jump_not_zero!! $j per_sample
    store8!! (+ $left_volume volume_factor) &left_volume
    store8!! (+ $right_volume -volume_factor) &right_volume

    store8!! (+ $i 1) &i
    jump_not_zero!! (<u $i frames) per_frame

    set_sp! &8                  # Clear local variables
    exit
