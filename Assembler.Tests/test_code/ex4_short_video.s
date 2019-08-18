### Render 5.12 s video at 50 FPS, 256 x 128 resolution and 44100 Hz sample
### rate. Observe that when a pixel is not set, its color is undefined. In
### particular, this is the case for the whole bottom left half of each frame.

    stack_size = 16384
    allocate! stack_size
    add! stack_size
    set_sp

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
    push!!!!!! 0 0 0 0 0 0
    i = 5                       # Frame counter
    x = 4
    y = 3
    left = 2
    right = 1
    sample = 0

per_frame:
    new_frame!!! width height sample_rate

### Image
    store8!! 0 &x
per_column:
    store8!! 0 &y
per_row:
    set_pixel!!!!! $x $y (+ 255 -$i) (+ $i -$x) (+ $i -(* $y 2))
    store8!! (+ $y 1) &y
    jump_not_zero!! (<=u (* $y ratio) $x) per_row
    store8!! (+ $x 1) &x
    jump_not_zero!! (<=u $x $i) per_column

### Sound
    store8!! (/u sample_rate fps) &sample # 44100/50 = 882
per_sample:
    store8!! (+ $left left_step) &left
    jump_not_zero!! (<s $left (+ base_volume (* $i volume_factor))) left_ok
    store8!! -$left &left       # Does this makes sense?
left_ok:
    store8!! (+ $right right_step) &right
    jump_not_zero!! (<s $right (+ base_volume (* (+ frames -$i) volume_factor))) right_ok
    store8!! -$right &right
right_ok:
    add_sample!! $left $right
    store8!! (+ $sample -1) &sample
    jump_not_zero!! $sample per_sample

    store8!! (+ $i 1) &i
    jump_not_zero!! (<u $i frames) per_frame

    set_sp! &6                  # Clear local variables
    exit
