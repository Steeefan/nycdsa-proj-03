import os
import json
import pandas as pd

os.chdir(r'E:\Projects\03 Machine Learning\data')

with open('train.json') as data:
    jsonStr = data.read()

# print(jsonStr[0:1000])
jsonStr = jsonStr.replace('\n', '')
jsonObj = json.loads(jsonStr)

aptDF = pd.DataFrame.from_dict(jsonObj)
aptDF['aptID'] = aptDF.index

featRows = []
photoRows = []

# for r in featDFsrc:
#     print(r)
#     for f in r:
#         print(f)
#         featDF.loc[len(featDF)] = r

_ = aptDF.apply(lambda row: [featRows.append([row['aptID'], f]) for f in row.features], axis=1)
_ = aptDF.apply(lambda row: [photoRows.append([row['aptID'], f]) for f in row.photos], axis=1)

featDF = pd.DataFrame(featRows, columns=['aptID', 'feature'])
photoDF = pd.DataFrame(photoRows, columns=['aptID', 'feature'])

featDF.to_csv('features.csv', index=False)
photoDF.to_csv('photos.csv', index=False)