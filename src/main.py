import numpy as np
import pygame
from dda import dda

# Dimensions (in pixels) of the screen
WIDTH, HEIGHT = 1200, 900
# Field of view of the camera, in radians
FOV_RAD = np.deg2rad(70)
# The move speed of the camera, in world unit per second
CAM_MOVE_SPEED = 5
MAX_FPS = 120
WHITE = (255, 255, 255)

# A simple class for storing the average of the last [n] measurements
class MovingAverage:
    def __init__(self, max_samples):
        assert(max_samples > 0)
        self.max_samples = max_samples
        self.samples = []
        self.i = 0

    def add_sample(self, x):
        if len(self.samples) < self.max_samples:
            self.samples.append(x)
        else:
            self.samples[self.i] = x
            self.i = (self.i + 1) % self.max_samples

    def get_average(self):
        assert(len(self.samples) > 0)
        return sum(self.samples, 0) / float(len(self.samples))


def main():
    # Create the window
    pygame.init()
    pygame.font.init()
    screen = pygame.display.set_mode((WIDTH, HEIGHT), depth = 32)
    pygame.display.set_caption("F-rep rendering on the GPU")

    # Create a font
    font = pygame.font.SysFont('comicsans', 40)

    # Create the futhark instance
    fut = dda()
    
    cam_pos     = np.array([0.0, 0.0, 20.0])
    cam_forward = np.array([0.0, 0.0, -1.0])
    cam_right   = np.array([1.0, 0.0, 0.0])
    cam_up      = np.array([0.0, 1.0, 0.0])
        
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
        raw_img = fut.main(WIDTH, HEIGHT, *cam_pos, *cam_forward, *cam_right, *cam_up, FOV_RAD).get()
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

if __name__ == "__main__":
    main()
