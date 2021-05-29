import xml.etree.ElementTree as ET
import pandas as pd


def xml2csv(fname, delcols=[]):
    tree = ET.parse(fname)
    root = tree.getroot()
    d = pd.DataFrame([e.attrib for e in root])
    for name in delcols: del d[name]
    d.to_csv(fname[:-4] + ".csv", index=False)


frames = ["Badges", "Comments", "PostHistory", "PostLinks", "Posts", "Tags", "Users", "Votes"]
religions = ["buddhism", "christianity", "hinduism", "islam", "judaism"]

for religion in religions:
    for frame in frames:
        xml2csv(religion + "/" + frame + ".xml")
