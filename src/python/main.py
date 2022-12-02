import numpy as np
import pygame

from utils import MovingAverage
import csg
import tape
from __engine import __engine


# Dimensions (in pixels) of the screen
WIDTH, HEIGHT = 1200, 900
# Field of view of the camera, in radians
FOV_RAD = np.deg2rad(70)
# The move speed of the camera, in world unit per second
CAM_MOVE_SPEED = 5
MAX_FPS = 120
WHITE = (255, 255, 255)

def main():
    # Create the window
    pygame.init()
    pygame.font.init()
    screen = pygame.display.set_mode((WIDTH, HEIGHT), depth = 32)
    pygame.display.set_caption("F-rep rendering on the GPU")

    # Create a font
    font = pygame.font.SysFont('comicsans', 40)

    # Create the futhark instance
    fut = __engine()
    
    cam_pos     = np.array([0.0, 0.0, 20.0])
    cam_forward = np.array([0.0, 0.0, -1.0])
    cam_right   = np.array([1.0, 0.0, 0.0])
    cam_up      = np.array([0.0, 1.0, 0.0])
        
    # Create the expression and tape
    expr = csg.X() * csg.X() + csg.Y() * csg.Y() + csg.Z() * csg.Z() - csg.const(10*10)
    tap = tape.Tape(expr)
    print(tap.to_string(detailed = True))

    run = True
    clock = pygame.time.Clock()
    ma = MovingAverage(30)
    while run:
        # delta_t is the time since last frame in seconds
        delta_t = clock.tick(MAX_FPS) / 1000.0
       
        # Process events
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                run = False
        
        # Process key presses
        keys = pygame.key.get_pressed()
        if keys[pygame.K_RIGHT]:
            cam_pos += delta_t * CAM_MOVE_SPEED * cam_right
        if keys[pygame.K_LEFT]:
            cam_pos -= delta_t * CAM_MOVE_SPEED * cam_right
        if keys[pygame.K_UP]:
            cam_pos += delta_t * CAM_MOVE_SPEED * cam_forward
        if keys[pygame.K_DOWN]:
            cam_pos -= delta_t * CAM_MOVE_SPEED * cam_forward
        if keys[pygame.K_RCTRL]:
            cam_pos -= delta_t * CAM_MOVE_SPEED * cam_up 
        if keys[pygame.K_RSHIFT]:
            cam_pos += delta_t * CAM_MOVE_SPEED * cam_up 

        # Raytrace the image
        t0 = pygame.time.get_ticks()
        raw_img = fut.main(
            WIDTH, HEIGHT, 
            *cam_pos, *cam_forward, *cam_right, *cam_up, FOV_RAD,
            np.array(tap.instructions, dtype = np.uint32), 
            np.array(tap.constant_pool, dtype = np.float32),
            tap.slot_count).get()
        t1 = pygame.time.get_ticks()
        ma.add_sample(t1 - t0)
        # For pygame, the first axis is horizontal from left to right
        # and the second axis is vertical from top to bottom
        img = np.flip(raw_img, axis = 1)    

        # Draw the image
        # Blitting to the whole screen seems to very slow : it's quite annoying
        pygame.surfarray.blit_array(screen, img)
        fps_text = font.render("%.1f FPS" % clock.get_fps(), 1, WHITE)
        screen.blit(fps_text, (10, 10))
        ma_text = font.render("Raytracing : %.1f ms" % ma.get_average(), 1, WHITE)
        screen.blit(ma_text, (10, 30))
        pygame.display.update()

    pygame.quit()

def main2():
    X = csg.X()
    Y = csg.Y()
    Z = csg.const(4.0)
    expr = X * X + Y * csg.const(-1) + Y / csg.sin(Z)
    graph = csg.merge_axes(expr).to_dot_graph(True)

    filepath = "csg.dot"
    with open(filepath, 'w') as f:
        f.write(graph.source)

    tap = tape.Tape(expr)
    print(tap.to_string(detailed = True))


if __name__ == "__main__":
    main()
