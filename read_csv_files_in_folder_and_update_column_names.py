### read all csv files in folder and create dataframe with updating columnnames if changed

path = r'\\csv_files' # use your path
all_files = glob.glob(path + "\\*.csv")


parse_dates = ['start_time', 'end_time']

li = []
cols = {}
df = {}
for filename in all_files:
    df[filename[-16:]] = pd.read_csv(filename,sep=";", header=0, index_col=None, parse_dates=parse_dates)
    cols[filename[-16:]] = df[filename[-16:]].columns.values.tolist()
    s_data = sorted(cols)
    val_list = []
    for i in range(len(s_data)-1):
        if cols[s_data[i+1]] != cols[s_data[i]]:
            for colx in cols[s_data[i+1]]:
                if colx not in cols[s_data[i]]:
                    for item in cols[s_data[i]]:
                        val = re.search(colx,item)
                        if val is not None:
                            val_list.append(" ".join([val.group()]))
                            for m in range(len(val_list)):
                                if item.startswith(val_list[m]):
                                    cols[s_data[i]][cols[s_data[i]].index(item)] = val_list[m]
                                    df[s_data[i]].columns = cols[s_data[i]]
                                    
    li.append(df[filename[-16:]])
    
common_cols = list(reduce(set.intersection, (set(val) for val in cols.values())))
frame = pd.concat([df[common_cols] for df in li], axis=0, ignore_index=True)

for val in cols.values():
    frame = frame.reindex(columns=val[0:14])

for df in li:
    for col in df.columns:
        if col not in common_cols:
            final_df = pd.merge(frame, df[['id',col]], how='left', on=['id', 'id'])