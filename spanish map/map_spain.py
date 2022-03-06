#%%
from PIL import Image
import numpy as np
import os
from tqdm import tqdm
import matplotlib.pyplot as plt

os.chdir(os.path.dirname(os.path.realpath(__file__)))
#%%

def set_black(r_min, r_max, g_min, g_max, b_min, b_max, img_array, contrast = 0):
    r_min, r_max, g_min, g_max, b_min, b_max = np.array([r_min, r_max, g_min, g_max, b_min, b_max]) + contrast
    h, w, _ = img_array.shape
    for i in tqdm(range(h)):
        for j in range(w):
            if (r_min <= img_array[i,j,0] <= r_max) and (g_min <= img_array[i, j, 1] <= g_max) and (b_min <= img_array[i, j, 2] <= b_max):
                img_array[i,j,:] = 0
    return img_array

def set_bnw(img_array):
    h, w, _ = img_array.shape
    for i in range(h):
        for j in range(w):
            if not img_array[i,j,:].all() ==0:
                img_array[i,j,:] = 255
    return img_array

roads = {
    "pink":(150, 255, 50, 125, 50, 135),
    # "dark red" : (150, 200, 85, 115, 85, 110),
    "yellow" : (200, 255, 180, 220, 70, 130),
    "blue" : (90, 140, 110, 160, 135, 185),
    "green" : (100, 145, 160, 205, 120, 175),
    "grey" : (90, 145, 80, 130, 70, 120)

}


#%%

map = Image.open("spanish_map.jpeg")
map_array = np.array(map)

# %%
bins = np.array([0., 250.])
digitized = 1 - (np.digitize(map_array, bins =bins ) - 1)

# %%
digitized = np.multiply.reduce(digitized, axis=2)
fig, ax = plt.subplots(figsize=(16,16))
ax.imshow(digitized, cmap="Greys")
# %%
