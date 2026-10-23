"""Builds the whole C. elegans nervous system for the browser's worm mode.

Data, all redistributed by OpenWorm. Local copies live in web/tools/source-data/worm/ (gitignored):
  Wiring: CElegansNeuronTables.xls, sheet Connectome (Varshney et al., PLoS Comput Biol 2011, CC BY),
    https://github.com/openworm/ConnectomeToolbox/blob/main/cect/data/CElegansNeuronTables.xls
  Transmitters: elife-95402-supp2-v1.xlsx (Wang et al., eLife 2024, CC BY), column "Neurotransmitter(s)",
    https://github.com/openworm/ConnectomeToolbox/blob/main/cect/data/elife-95402-supp2-v1.xlsx
  Soma positions: CElegansNeuroML generatedNeuroML2/*.cell.nml (WormBase Virtual Worm, public domain),
    https://github.com/openworm/CElegansNeuroML/tree/master/CElegans/generatedNeuroML2

Run:
  uv run --no-project --with pandas --with xlrd --with openpyxl python -I web/tools/extract_worm.py \
    web/tools/source-data/worm/CElegansNeuronTables.xls web/tools/source-data/worm/elife-95402-supp2-v1.xlsx \
    web/tools/source-data/worm/generatedNeuroML2 web/data/worm-circuit.json

Unlike the fruit fly, the worm is small enough to keep every neuron and every connection.
"""

import glob
import json
import math
import os
import re
import sys

import pandas as pd

xls_path, atlas_path, nml_dir, out_path = sys.argv[1:5]

# Gap junctions pass a share of each partner's activity. Their strength relative to chemical synapses is
# unknown, so it is a guess, kept below 1 so coupling cannot run away.
GAP_GAIN = 0.5

# Head steering. The worm crawls on its side and steers by bending its head dorsally or ventrally,
# driven by the SMD and RMD head motor neurons (klinotaxis). Dorsal bends map to the snake's left.
LEFT = ["SMDDL", "SMDDR", "RMDDL", "RMDDR"]
RIGHT = ["SMDVL", "SMDVR", "RMDVL", "RMDVR"]

# The snake's senses feed head sensory neurons: amphid chemosensory neurons for the fruit,
# head mechanosensory neurons for walls and body. Tail and body sensors would sense nothing useful.
INPUTS = ["ASEL", "ASER", "AWCL", "AWCR", "AWAL", "AWAR", "AWBL", "AWBR", "ASHL", "ASHR", "ADLL", "ADLR",
          "ASIL", "ASIR", "ASKL", "ASKR", "ADFL", "ADFR", "ASJL", "ASJR",
          "FLPL", "FLPR", "OLQDL", "OLQDR", "OLQVL", "OLQVR", "CEPDL", "CEPDR", "CEPVL", "CEPVR",
          "IL1DL", "IL1DR", "IL1VL", "IL1VR", "IL1L", "IL1R"]

STEPS = 5

table = pd.read_excel(xls_path, sheet_name="Connectome")


def sign(transmitters):
    """One sign per sending neuron (Dale's law). The wiring sheet labels each neuron with a single, often
    peptide or monoamine transmitter (ASE as FMRFamide), which would silence glutamatergic and cholinergic
    neurons, so signs come from the newer atlas instead. OpenWorm's c302 model counts GABA as the only
    inhibitory transmitter. Glutamate can act either way in the worm, depending on receptors the wiring data
    lacks, so it counts excitatory, as in c302. Neurons with only monoamines act slowly and diffusely, and are
    dropped as senders, as in the fruit fly mode."""
    text = str(transmitters)
    if "GABA" in text:
        return -1
    if "ACh" in text or "Glu" in text:
        return 1
    return None


atlas = pd.read_excel(atlas_path, sheet_name=0, header=3)
atlas = atlas[atlas["Neuron"].notna()]
sign_of = {n: sign(t) for n, t in zip(atlas["Neuron"], atlas["Neurotransmitter(s)"]) if sign(t) is not None}

position = {}
for f in glob.glob(f"{nml_dir}/*.cell.nml"):
    m = re.search(r'<segment id="0"[^>]*>\s*<proximal x="([-\d.e]+)" y="([-\d.e]+)" z="([-\d.e]+)"', open(f).read())
    if m:
        position[os.path.basename(f).split(".")[0]] = tuple(map(float, m.groups()))

neurons = sorted(set(table["Origin"]) | set(table["Target"]))
missing = [n for n in neurons if n not in position]
assert not missing, f"no soma position for {missing}"
for name in INPUTS + LEFT + RIGHT:
    assert name in neurons, f"{name} is not in the wiring data"

# Inputs first, then interneurons, then steering, matching the fruit fly file's layout.
order = INPUTS + [n for n in neurons if n not in INPUTS + LEFT + RIGHT] + LEFT + RIGHT
index = {n: i for i, n in enumerate(order)}

chemical = table[(table["Type"] == "Send") & table["Origin"].isin(sign_of)]
chemical = chemical.groupby(["Origin", "Target"], as_index=False)["Number of Connections"].sum()
total_in = chemical.groupby("Target")["Number of Connections"].sum()
edges = []
for pre, post, count in chemical.itertuples(index=False):
    edges += [index[pre], index[post], round(sign_of[pre] * count / total_in[post], 4)]

# The sheet lists almost every gap junction once per direction, so each row becomes one directed coupling.
# A few pharyngeal entries are one way only. Self-loops add nothing and would dilute the neuron's other couplings.
gaps_table = table[(table["Type"] == "GapJunction") & (table["Origin"] != table["Target"])]
gap_total = gaps_table.groupby("Target")["Number of Connections"].sum()
gaps = []
for pre, post, count in zip(gaps_table["Origin"], gaps_table["Target"], gaps_table["Number of Connections"]):
    gaps += [index[pre], index[post], round(GAP_GAIN * count / gap_total[post], 4)]

senders = set(chemical["Origin"]) | set(gaps_table["Origin"])
silent = [n for n in INPUTS if n not in senders]
assert not silent, f"input neurons that reach nothing: {silent}"
unsigned = [n for n in neurons if n not in sign_of]

# Side view: body axis across, dorsal-ventral axis up. The Virtual Worm is curved, so each neuron's height is
# measured from the local body centerline, which straightens the worm. Most neurons crowd into the head's nerve
# ring, a few percent of the body length, so the body axis is log-stretched: the head gets about half the width.
# The thin body is drawn 4 times thicker than life.
HEAD_STRETCH = 30
THICKNESS = 4
ys = [position[n][1] for n in neurons]
zs = [position[n][2] for n in neurons]
y0, length = min(ys), max(ys) - min(ys)


def centerline(y):
    near = [z for z, yy in zip(zs, ys) if abs(yy - y) < 0.06 * length]
    return sum(near) / len(near) if near else 0.0


along = lambda t: math.log1p(HEAD_STRETCH * t) / math.log1p(HEAD_STRETCH)
norm = lambda y, z: [round(along((y - y0) / length), 4), round(0.5 - THICKNESS * (z - centerline(y)) / length, 4)]

# A faint body outline that tapers at both ends of the drawing, not of the real body, so the stretched head stays round.
cloud = []
for k in range(240):
    u = k / 239
    half = THICKNESS * (0.03 * math.sin(math.pi * u) ** 0.5 + 0.003)
    cloud += [round(u, 4), round(0.5 - half, 4), round(u, 4), round(0.5 + half, 4)]


def role(name):
    if name in INPUTS:
        return "input"
    return "output" if name in LEFT + RIGHT else "inter"


result = {
    "source": "C. elegans connectome: Varshney et al. 2011 (CC BY), transmitters from Wang et al. 2024 (CC BY), WormBase Virtual Worm positions, via OpenWorm (MIT).",
    "neurons": [{"type": n, "side": "", "role": role(n), "pos": norm(position[n][1], position[n][2])} for n in order],
    "inputs": [index[n] for n in INPUTS],
    "left": [[index[n], 1.0] for n in LEFT],
    "right": [[index[n], 1.0] for n in RIGHT],
    "edges": edges,
    "gaps": gaps,
    "steps": STEPS,
    "cloud": cloud,
}

with open(out_path, "w") as f:
    json.dump(result, f, separators=(",", ":"))

print(f"neurons {len(order)} chemical {len(edges) // 3} gap {len(gaps) // 3} inputs {len(INPUTS)} "
      f"inhibitory senders {sum(1 for n in neurons if sign_of.get(n) == -1)} unsigned {len(unsigned)} -> {out_path}")
