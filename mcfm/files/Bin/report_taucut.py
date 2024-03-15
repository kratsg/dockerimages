from pathlib import Path
import re
from math import sqrt

logfiles = Path().glob("slurm-*.out")

number_pattern = "[+-]?((\d+(\.\d*)?)|(\.\d+))([eE][+-]?\d+)?"

pattern_running = re.compile('Running MCFM as (?P<config>.*)')
pattern_xsec = re.compile(f'Value of integral is\s+(?P<value>{number_pattern})\s+±\s+(?P<uncert>{number_pattern})\s+fb')
pattern_taucut = re.compile(f'\s+sigma\(tcut=(?P<low>{number_pattern})\)\s+-\s+sigma\(tcut=(?P<high>{number_pattern})\)\s+=\s+(?P<diff>{number_pattern})\s+±\s+(?P<uncert>{number_pattern})\s+fb')

for logfile in sorted(logfiles):
    contents = logfile.read_text()

    last_block_slice = slice(contents.rfind('Snapshot written to'), None)

    print(f"{logfile}: {pattern_running.search(contents).groupdict()['config']}")

    xsec = {k:float(v) for k,v in pattern_xsec.search(contents[last_block_slice]).groupdict().items()}

    print(f"  - xsec: {xsec['value']:0.4f} ± {xsec['uncert']:0.4f} fb")

    for item in [m.groupdict() for m in pattern_taucut.finditer(contents[last_block_slice])]:
        taucut = {k:float(v) for k,v in item.items()}

        relative_diff = 100 * abs(taucut['diff'])/xsec['value']
        relative_uncert = relative_diff * sqrt((taucut['uncert']/taucut['diff'])**2 + (xsec['uncert']/xsec['value'])**2)

        print(f"  - taucut: Δxsec({taucut['low']:0.4f}-{taucut['high']:0.4f}) = {taucut['diff']:0.4f} ± {taucut['uncert']:0.4f} fb ({relative_diff:0.4f} ± {relative_uncert:0.4f} %)")
