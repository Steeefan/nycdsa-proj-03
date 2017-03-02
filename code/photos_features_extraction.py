# loading required libraries
import numpy as np
import math as math
from PIL import Image, ImageStat
import glob
import os.path
import time

def brightness(im):
    stat = ImageStat.Stat(im)
    gs = (math.sqrt(0.241*(r**2) + 0.691*(g**2) + 0.068*(b**2)) for r, g, b in im.getdata())
    return sum(gs)/stat.count[0]

# initialize file to save photos stats into
os.chdir(r'J:\Downloads\kaggle\images')
fileName = 'D:/photo_features-' + time.strftime("%Y%m%d-%H%M%S") + '.csv'
with open(fileName, 'a') as file:
        file.write('listing_id,photo_id,width,height,r,g,b,brightness\n')

# script for batch processing:
# this codes assumes the root to be inside a directory where each folder corresponds to a listing id

# get a list of names for each listing_id
listing_ids = glob.glob('[0-9]*')  # filters down a list with only directories which are numbers-Listing id

i = 0
for listing in listing_ids[0:]:
    try:
        listing_id = listing
        # extract only images with jpg extension
        photos = glob.glob('./%i/*.jpg'%(int(listing)))
        for photo in photos:
            try:
                if i % 100 == 0:
                    print time.strftime("%Y-%m-%d %H:%M:%S"), str(i), photo

                im = Image.open(photo)
                # get images dimensions
                width, height = im.size
                # get rgb value
                rgb = np.array(im).mean(axis=(0,1))
                # get brightness
                bri = brightness(im)
                # save information to csv. 'a' stands for append
                with open(fileName, 'a') as file:
                    file.write('%s,%s,%f,%f,%f,%f,%f,%f\n'%(listing_id,photo,width, height, rgb[0], rgb[1], rgb[2], bri))

                i += 1
            except IOError, e:
                # Report error, and then skip to the next argument
                print "Problem opening", photo, ":", e
                continue

            except:
                print 'Some other error on', photok
                continue
    except IOError, e:
        print "Problem opening", listing_id, ":", e
        continue