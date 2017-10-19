import numpy as np
import cv2
import pandas as pd

df = pd.read_csv("..\data\eye-tracking_data\p01_ET_samples.txt", "\t")
for filename in df["Stimulus"].unique():
    if filename == "richtext.jpg" or filename == "richtext7.jpg":
        continue
    singledf = df.loc[(df["Stimulus"] == filename)]# & (df["L Event Info"] == "Fixation") & (df["R Event Info"] == "Fixation")]
    if len(singledf) == 0:
        continue

    lxcolumn = singledf["L POR X [px]"]
    lycolumn = singledf["L POR Y [px]"]

    minx = lxcolumn.min()
    maxx = lxcolumn.max()
    miny = lycolumn.min()
    maxy = lycolumn.max()

    size = (int(maxy - miny), int(maxx - minx), 3)
    if size[0] == 0 or size[1] == 0:
        continue

    xoffset = -minx
    yoffset = -miny

    img = np.full(size, 255, np.uint8)

    lprev = (int(lxcolumn.iloc[0] + xoffset), int(lycolumn.iloc[0] + yoffset))
    for i in range(1, lxcolumn.size):
        lloc = (int(lxcolumn.iloc[i] + xoffset), int(lycolumn.iloc[i] + yoffset))

        #cv2.line(img, lprev, lloc, (0, i % (lxcolumn.size / 1) * 255 / (lxcolumn.size / 1), 255), 2)
        if singledf["L Event Info"].iloc[i] == "Fixation":
            cv2.line(img, lprev, lloc, (0, 0, 255), 2)
        elif singledf["L Event Info"].iloc[i] == "Blink":
            cv2.line(img, lprev, lloc, (0, 255, 0), 2)
        elif singledf["L Event Info"].iloc[i] == "Saccade":
            cv2.line(img, lprev, lloc, (255, 0, 0), 2)
        lprev = lloc

    print filename

    cv2.line(img, (int(xoffset), 0), (int(xoffset), size[0]), (0, 0, 0), 2)
    cv2.line(img, (0, int(yoffset)), (size[1], int(yoffset)), (0, 0, 0), 2)

    cv2.namedWindow("img", cv2.WINDOW_NORMAL)
    cv2.resizeWindow("img", size[0], size[1])
    cv2.imshow("img", img)
    cv2.waitKey()
    cv2.destroyAllWindows()
