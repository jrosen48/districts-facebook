import PyCrowdTangle as pct        
import pandas as pd

data = pct.ct_get_posts(search_term = "common core", api_token="NgdKtOSYfEODUcMjaq8s5p2zzr56kACOgwiJ3prL") 
df = pd.DataFrame(data['result']['posts'])
df.head()
