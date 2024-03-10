import pandas as pd

data = pd.read_csv('pizza.gdt', sep='\s+')
data.to_csv('/path/to/output.csv', index=False)

