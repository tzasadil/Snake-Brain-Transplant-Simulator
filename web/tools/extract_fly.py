"""Extracts the steering subcircuit of the male fruit fly connectome for the browser's fruit fly mode.

Data: Male CNS v1.0 (FlyEM at HHMI Janelia, Cambridge, MRC LMB, Google Research), CC-BY 4.0.
Download the three flat-connectome files from https://male-cns.janelia.org/download/ into one directory.
Local copies live in web/tools/source-data/fly-male-cns-v1.0/ (gitignored, about 1.1 GB):
  body-annotations-male-cns-v1.0-minconf-0.5.feather
  body-neurotransmitters-male-cns-v1.0.feather
  connectome-weights-male-cns-v1.0-minconf-0.5.feather   (1.1 GB, 152M rows)

Run:
  uv run --no-project --with pyarrow --with pandas --with scipy python -I web/tools/extract_fly.py \
    web/tools/source-data/fly-male-cns-v1.0 web/data/fly-circuit.json

The full brain (166k neurons) is far too large to download and evolve in a browser, so this keeps the
neurons that carry the most signal from vision to steering. Signal flows forward from the visual projection
neurons and backward from the steering neurons DNa01 and DNa02, hop by hop. A neuron scores by how much of
both reaches it. Picking by input to the steering neurons alone (the first version) kept neurons beside the
visual path rather than on it: every visual input then reached steering in one hop.
"""

import json
import sys

import numpy as np
import pyarrow.dataset as ds
import pyarrow.feather as feather
import scipy.sparse as sp
import scipy.sparse.csgraph as csgraph

data_dir, out_path = sys.argv[1], sys.argv[2]

# Sizes keep the circuit small enough to evolve 200 games per generation in a browser.
MAX_NEURONS = 250
MAX_PATHS = 200
INPUTS_PER_SIDE = 16
# Each pick divides its connections' strength by this, so later picks favor fresh routes.
PATH_PENALTY = 4
# Each neuron keeps only its strongest inputs, which bounds the work per update step.
MAX_INPUTS_PER_NEURON = 16
# Paths longer than this barely carry signal after normalization, and each hop costs an update step.
MAX_HOPS = 5
# Weaker connections are dropped: below a few synapses, reconstruction noise dominates.
MIN_SYNAPSES = 5
CLOUD_SAMPLES = 3000

# Steering readout. DNa02 tracks brief turns, DNa01 slow sustained ones (Rayshubskiy et al., eLife 2025),
# and a snake turns once per move, so DNa02 counts fully and DNa01 half.
OUTPUTS = {"DNa02": 1.0, "DNa01": 0.5}

# Signs as in Shiu et al. (Nature 2024). Glutamate is inhibitory in the fly central brain (GluCl channels).
# Neuromodulators act slowly and diffusely, so their synapses are dropped.
SIGN = {"acetylcholine": 1, "gaba": -1, "glutamate": -1}

# Central brain classes that can sit on a vision-to-steering path. Optic lobe internals are upstream of the
# visual projection neurons, and nerve cord classes are downstream of the descending neurons.
PATH_CLASSES = {"visual_projection", "cb_intrinsic", "descending_neuron", "visual_centrifugal"}

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

eligible = annotations[annotations["superclass"].isin(PATH_CLASSES) & annotations["bodyId"].map(lambda b: b in sign_of)]
ids = eligible["bodyId"].to_numpy()
pos_of = {b: i for i, b in enumerate(ids)}

weights = ds.dataset(f"{data_dir}/connectome-weights-male-cns-v1.0-minconf-0.5.feather", format="feather")
edges = weights.to_table(
    filter=(ds.field("weight") >= MIN_SYNAPSES)
    & ds.field("body_pre").isin(ids)
    & ds.field("body_post").isin(ids)
).to_pandas()

pre = edges["body_pre"].map(pos_of).to_numpy()
post = edges["body_post"].map(pos_of).to_numpy()
w = edges["weight"].to_numpy(dtype=float)
n = len(ids)
A = sp.csr_matrix((w, (pre, post)), shape=(n, n))
out_total = np.asarray(A.sum(axis=1)).ravel() + 1e-12
in_total = np.asarray(A.sum(axis=0)).ravel() + 1e-12
# Forward: the share of a neuron's output each partner receives. Backward: the share of a neuron's input each partner sends.
forward = sp.diags(1 / out_total) @ A
backward = A @ sp.diags(1 / in_total)

superclass = eligible["superclass"].to_numpy()
somaside = eligible["somaSide"].to_numpy()
types = eligible["type"].to_numpy()
is_output = np.isin(types, list(OUTPUTS))
output_ids = list(ids[is_output])
assert len(output_ids) == 4, f"expected DNa01 and DNa02 on both sides, got {len(output_ids)}"
is_vpn = superclass == "visual_projection"
# Visual neurons wired straight onto steering are excluded as inputs, so signals must cross the brain.
direct = np.asarray((A[:, is_output] > 0).sum(axis=1)).ravel() > 0

# Paths, not scores: an earlier version scored neurons by signal flow through the whole brain and kept the top
# ones. Many of the paths they sat on ran through neurons it dropped, so inside the small circuit they got
# little signal. Here whole paths are kept. A connection's strength is its share of the receiving neuron's
# input. A path's strength is the product along it, so the strongest path is the shortest under -log(share).
# After each pick, the path's connections are weakened, so the next search finds a different route.
share = backward.tocoo()
cost = sp.csr_matrix((-np.log(share.data), (share.col, share.row)), shape=(n, n))  # reversed: post -> pre
cost.data = np.maximum(cost.data, 1e-9)  # a sole input has share 1 and cost 0, which the search drops
targets = np.flatnonzero(is_output)

nodes, inputs, path_edges, path_lengths = set(output_ids), {"L": [], "R": []}, set(), []
for _ in range(MAX_PATHS):
    dist, pred = csgraph.dijkstra(cost, indices=targets, min_only=True, return_predecessors=True)[:2]
    for side in ["L", "R"]:
        chosen = inputs[side]
        if len(chosen) < INPUTS_PER_SIDE:
            candidates = np.flatnonzero(is_vpn & (somaside == side) & ~direct)
        else:
            candidates = np.array([pos_of[b] for b in chosen])
        candidates = candidates[np.isfinite(dist[candidates])]
        start = candidates[np.argmin(dist[candidates])]
        path = [start]
        while path[-1] not in targets:
            path.append(pred[path[-1]])
        if len(path) - 1 > MAX_HOPS:
            continue
        if ids[start] not in chosen:
            chosen.append(ids[start])
        path_lengths.append(len(path) - 1)
        for a, b in zip(path, path[1:]):
            nodes.update([ids[a], ids[b]])
            path_edges.add((ids[a], ids[b]))
            # Weaken the used connection (reversed matrix: row b, column a).
            row = slice(cost.indptr[b], cost.indptr[b + 1])
            hit = np.flatnonzero(cost.indices[row] == a)
            cost.data[cost.indptr[b] + hit] += np.log(PATH_PENALTY)
    if len(nodes) >= MAX_NEURONS:
        break

inputs = inputs["L"] + inputs["R"]
selected = [b for b in nodes if b not in inputs and b not in output_ids]

inters = sorted(selected)
circuit = inputs + inters + output_ids
index = {body: i for i, body in enumerate(circuit)}

edges = edges[edges["body_pre"].isin(index) & edges["body_post"].isin(index)]
is_path_edge = [(a, b) in path_edges for a, b in zip(edges["body_pre"], edges["body_post"])]
strongest = edges.sort_values("weight", ascending=False).groupby("body_post").head(MAX_INPUTS_PER_NEURON)
# Pruning keeps each neuron's strongest inputs, and always the connections of the chosen paths.
edges = edges[is_path_edge | edges.index.isin(strongest.index)]
# Normalize each neuron's input within the subcircuit, so activity stays bounded whatever its size.
total_in = edges.groupby("body_post")["weight"].sum()
flat_edges = []
for a, b, x in zip(edges["body_pre"], edges["body_post"], edges["weight"]):
    flat_edges += [index[a], index[b], round(sign_of[a] * x / total_in[b], 4)]

# Hop distance from the inputs. Every steering neuron must be reachable. The browser runs MAX_HOPS update steps,
# so paths up to MAX_HOPS long all deliver signal, not only the shortest.
hops = {index[b]: 0 for b in inputs}
frontier = list(hops)
succ = {}
for a, b in zip(edges["body_pre"], edges["body_post"]):
    succ.setdefault(index[a], []).append(index[b])
while frontier:
    nxt = []
    for u in frontier:
        for v in succ.get(u, []):
            if v not in hops:
                hops[v] = hops[u] + 1
                nxt.append(v)
    frontier = nxt
output_hops = [hops.get(index[b]) for b in output_ids]
assert all(h is not None for h in output_hops), f"a steering neuron is unreachable: {output_hops}"
steps = MAX_HOPS

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
    "steps": steps,
    "cloud": [v for p in cloud for v in norm(p)],
}

with open(out_path, "w") as f:
    json.dump(result, f, separators=(",", ":"))

print(f"neurons {len(circuit)} edges {len(flat_edges) // 3} inputs {len(inputs)} paths {len(path_lengths)} "
      f"path hops {np.bincount(path_lengths).tolist()} steps {steps} -> {out_path}")
