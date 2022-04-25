## read docx files in a specific folder, append them and turn into data frame
import os
import pandas as pd
from docx import Document

document_pathList = []

for path, subdirs, files in os.walk(os.getcwd()): 
    for name in files:
        # For each file we find, we need to ensure it is a .docx file before adding
        #  it to our list
        if os.path.splitext(os.path.join(path, name))[1] == ".docx":
            document_pathList.append(os.path.join(path, name))
            
data = {'path':[],'text':[]}
for doc_path in document_pathList:
    try:
        s =""
        fullText=[]
        document = Document(doc_path)
        for p in document.paragraphs:
            if p.text.strip("\n")!='':
                s+=p.text.strip("\n")+" "
                fullText.append(p.text.strip("\n"))
        data['path'].append(doc_path)
        data['text'].append(s)
    except:
        continue

df = pd.DataFrame(data)


## search specific keywords in these docx files and count each of them in all docx files
keyw= ['esg','environment','employee','security','health','emission','sustainability','sustainable']

from collections import *
count_list=[]
for key in keyw:
    c=0
    for i in range(0,len(df)):
        words = df.text.iloc[i].split()
        wordCount = Counter(words)
        c += wordCount[key]
    count_list.append(c)
    
ans = pd.DataFrame(dict(keyw=keyw,count=count_list))