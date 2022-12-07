import requests
import os
from secret import session_cookie

cwd = os.getcwd().split('/')[-1]
assert cwd.startswith('day')
day = int(cwd[3:])

url = f'https://adventofcode.com/2022/day/{day}/input'
r = requests.get(url, cookies={'session': session_cookie})

assert r.status_code == 200

with open('puzzle.in', 'w') as f:
    f.write(r.text)
