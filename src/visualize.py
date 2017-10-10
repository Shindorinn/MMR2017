import numpy as np
import cv2
import pandas as pd

df = pd.read_csv("p01_ET_samples.txt", "\t")
for filename in df["Stimulus"].unique():
    if filename == "richtext.jpg" or filename == "richtext7.jpg":
        continue
    singledf = df.loc[(df["Stimulus"] == filename) & (df["L Event Info"] == "Fixation") & (df["R Event Info"] == "Fixation")]
    if len(singledf) == 0:
        continue

    lxcolumn = singledf["L POR X [px]"]
    lycolumn = singledf["L POR Y [px]"]
    rxcolumn = singledf["R POR X [px]"]
    rycolumn = singledf["R POR Y [px]"]

    minx = min(lxcolumn.min(), rxcolumn.min())
    maxx = max(lxcolumn.max(), rxcolumn.max())
    miny = min(lycolumn.min(), rycolumn.min())
    maxy = max(lycolumn.max(), rycolumn.max())

    size = (int(maxy - miny), int(maxx - minx), 3)
    if size[0] == 0 or size[1] == 0:
        continue

    xoffset = -minx
    yoffset = -miny

    img = np.full(size, 255, np.uint8)

    lprev = (int(lxcolumn.iloc[0] + xoffset), int(lycolumn.iloc[0] + yoffset))
    rprev = (int(rxcolumn.iloc[0] + xoffset), int(rycolumn.iloc[0] + yoffset))
    for i in range(1, lxcolumn.size):
        print lxcolumn.iloc[i], rxcolumn.iloc[i], lycolumn.iloc[i], rycolumn.iloc[i]

        lloc = (int(lxcolumn.iloc[i] + xoffset), int(lycolumn.iloc[i] + yoffset))
        rloc = (int(rxcolumn.iloc[i] + xoffset), int(rycolumn.iloc[i] + yoffset))

        cv2.line(img, lprev, lloc, (255, i * 255 / lxcolumn.size, 0), 2)
        cv2.line(img, rprev, rloc, (0, i * 255 / lxcolumn.size, 255), 2)
        lprev = lloc
        rprev = rloc

    print filename

    cv2.namedWindow("img", cv2.WINDOW_NORMAL)
    cv2.resizeWindow("img", size[0], size[1])
    cv2.imshow("img", img)
    cv2.waitKey()
    cv2.destroyAllWindows()
