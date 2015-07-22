import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib as plt

#cran_all = pd.read_csv("/Users/nancyorgan/Documents/ExploR/cran_all.txt")
heavy_counts = pd.read_csv("/Users/nancyorgan/Desktop/heavy_counts.txt")
heavy_counts = heavy_counts[heavy_counts.max(columns=1).order(ascending=False).index]

#heavy_counts = heavy_counts.sort()

#print(cran_all.head())
violin_base = sns.violinplot(data = heavy_counts, inner = None, cut=0)
violin_base.set_title("Daily Downloads for the Top 10 \n Downloaded R Packages of 2012-2015")
violin_base.set_xlabel("Package")
violin_base.set_ylabel("Daily Downloads")
violin_base.set_yticks(np.arange(0, 26000, 2000))
for label in violin_base.xaxis.get_ticklabels():
    label.set_rotation(45)

violin_base

sns.plt.show()

test = pd.DataFrame({"c":[1,2,3],
                     "a":[4,5,6],
                     "t":[7,8,9]})
