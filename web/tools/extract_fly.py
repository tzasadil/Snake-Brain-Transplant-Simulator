"""Extracts the steering subcircuit of the male fruit fly connectome for the browser's fruit fly mode.

Data: Male CNS v1.0 (FlyEM at HHMI Janelia, Cambridge, MRC LMB, Google Research), CC-BY 4.0.
Download the three flat-connectome files from https://male-cns.janelia.org/download/ into one directory:
  body-annotations-male-cns-v1.0-minconf-0.5.feather
  body-neurotransmitters-male-cns-v1.0.feather
  connectome-weights-male-cns-v1.0-minconf-0.5.feather   (1.1 GB, 152M rows)

Run:
  uv run --no-project --with pyarrow --with pandas python -I web/tools/extract_fly.py <data dir> web/public/fly-circuit.json

The full brain (166k neurons) is far too large to download and evolve in a browser, so this keeps only
the strongest paths from visual projection neurons to the steering descending neurons DNa01 and DNa02.
"""

import json
import sys

import pyarrow.dataset as ds
import pyarrow.feather as feather

data_dir, out_path = sys.argv[1], sys.argv[2]

# Layer sizes, picked so the circuit stays small enough to evolve 200 games per generation in a browser.
LAYER1 = 40
LAYER2 = 120
INPUTS_PER_SIDE = 16
# Weaker connections are dropped: below a few synapses, reconstruction noise dominates.
MIN_SYNAPSES = 3
CLOUD_SAMPLES = 3000

# Steering readout. DNa02 tracks brief turns, DNa01 slow sustained ones (Rayshubskiy et al., eLife 2025),
# and a snake turns once per move, so DNa02 counts fully and DNa01 half.
OUTPUTS = {"DNa02": 1.0, "DNa01": 0.5}

# Signs as in Shiu et al. (Nature 2024). Glutamate is inhibitory in the fly central brain (GluCl channels).
# Neuromodulators act slowly and diffusely, so their synapses are dropped.
SIGN = {"acetylcholine": 1, "gaba": -1, "glutamate": -1}

annotations = feather.read_table(
    f"{data_dir}/body-annotations-male-cns-v1.0-minconf-0.5.feather",
    columns=["bodyId", "type", "instance", "somaSide", "superclass", "somaLocation", "tosomaLocation"],
).to_pandas()
annotations = annotations[annotations["type"].notna() & annotations["superclass"].notna()]
by_id = annotations.set_index("bodyId")

transmitters = feather.read_table(
    f"{data_dir}/body-neurotransmitters-male-cns-v1.0.feather", columns=["body", "consensus_nt"]
).to_pandas()
sign_of = {
    body: SIGN[nt]
    for body, nt in zip(transmitters["body"], transmitters["consensus_nt"])
    if nt in SIGN
}

weights = ds.dataset(f"{data_dir}/connectome-weights-male-cns-v1.0-minconf-0.5.feather", format="feather")


def edges_into(posts):
    """All annotated, signed connections onto `posts`, streamed so the 152M-row table never sits in memory."""
    table = weights.to_table(
        filter=(ds.field("weight") >= MIN_SYNAPSES) & ds.field("body_post").isin(list(posts))
    ).to_pandas()
    table = table[table["body_pre"].isin(by_id.index) & table["body_pre"].map(lambda b: b in sign_of)]
    return table


def top_partners(posts, count, exclude):
    table = edges_into(posts)
    table = table[~table["body_pre"].isin(exclude)]
    totals = table.groupby("body_pre")["weight"].sum().sort_values(ascending=False)
    return list(totals.index[:count])


outputs = annotations[annotations["type"].isin(OUTPUTS.keys())]
assert len(outputs) == 4, f"expected DNa01 and DNa02 on both sides, got {len(outputs)}"
output_ids = list(outputs["bodyId"])

layer1 = top_partners(output_ids, LAYER1, set(output_ids))
layer2 = top_partners(output_ids + layer1, LAYER2, set(output_ids + layer1))
core = output_ids + layer1 + layer2

inputs = []
for side in ["L", "R"]:
    vpns = edges_into(core)
    vpns = vpns[
        (vpns["body_pre"].map(by_id["superclass"]) == "visual_projection")
        & (vpns["body_pre"].map(by_id["somaSide"]) == side)
        & ~vpns["body_pre"].isin(core)
    ]
    totals = vpns.groupby("body_pre")["weight"].sum().sort_values(ascending=False)
    inputs += list(totals.index[:INPUTS_PER_SIDE])

circuit = inputs + [b for b in core if b not in inputs]
index = {body: i for i, body in enumerate(circuit)}

edges = edges_into(circuit)
edges = edges[edges["body_pre"].isin(index)]
# Normalize each neuron's input within the subcircuit, so activity stays bounded whatever its size.
total_in = edges.groupby("body_post")["weight"].sum()
flat_edges = []
for pre, post, w in zip(edges["body_pre"], edges["body_post"], edges["weight"]):
    flat_edges += [index[pre], index[post], round(sign_of[pre] * w / total_in[post], 4)]


def position(body):
    row = by_id.loc[body]
    for col in ["somaLocation", "tosomaLocation"]:
        loc = row[col]
        if loc is not None and not (isinstance(loc, float)):
            return float(loc[0]), float(loc[1])
    return None


positions = {b: position(b) for b in circuit}

brain = annotations[
    ~annotations["superclass"].str.startswith("vnc")
    & annotations["somaLocation"].notna()
    & ~annotations["superclass"].isin(["ascending_neuron", "sensory_ascending", "efferent_ascending"])
]
cloud = brain.sample(n=CLOUD_SAMPLES, random_state=1)["somaLocation"].map(lambda l: (float(l[0]), float(l[1])))
xs = [p[0] for p in cloud] + [p[0] for p in positions.values() if p]
ys = [p[1] for p in cloud] + [p[1] for p in positions.values() if p]
x0, y0 = min(xs), min(ys)
scale = max(max(xs) - x0, max(ys) - y0)

# The dataset's x axis grows toward the fly's left. Flipping it shows the brain from behind,
# so the fly's left side is on the viewer's left, matching the snake's left turn.
norm = lambda p: [round(1 - (p[0] - x0) / scale, 4), round((p[1] - y0) / scale, 4)]

# Neurons without a soma position sit at the mean of their partners' positions.
partners = {}
for pre, post in zip(edges["body_pre"], edges["body_post"]):
    partners.setdefault(pre, []).append(post)
    partners.setdefault(post, []).append(pre)
for body, pos in list(positions.items()):
    if pos is None:
        known = [positions[p] for p in partners.get(body, []) if positions.get(p)]
        positions[body] = (sum(p[0] for p in known) / len(known), sum(p[1] for p in known) / len(known)) if known else (x0 + scale / 2, y0 + scale / 2)


def side(body):
    s = by_id.loc[body, "somaSide"]
    return s if isinstance(s, str) else ""


def role(body):
    if body in inputs:
        return "input"
    return "output" if body in output_ids else "inter"


result = {
    "source": "Male CNS v1.0 connectome, FlyEM (HHMI Janelia), University of Cambridge, MRC LMB, Google Research. CC-BY 4.0. https://male-cns.janelia.org",
    "neurons": [
        {"type": by_id.loc[b, "type"], "side": side(b), "role": role(b), "pos": norm(positions[b])}
        for b in circuit
    ],
    "inputs": [index[b] for b in inputs],
    "left": [[index[b], OUTPUTS[by_id.loc[b, "type"]]] for b in output_ids if by_id.loc[b, "somaSide"] == "L"],
    "right": [[index[b], OUTPUTS[by_id.loc[b, "type"]]] for b in output_ids if by_id.loc[b, "somaSide"] == "R"],
    "edges": flat_edges,
    "cloud": [v for p in cloud for v in norm(p)],
}

with open(out_path, "w") as f:
    json.dump(result, f, separators=(",", ":"))

print(f"neurons {len(circuit)} edges {len(flat_edges) // 3} inputs {len(inputs)} -> {out_path}")
