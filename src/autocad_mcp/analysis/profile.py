"""Describe every layer by its geometry, not by its name.

Layer names from outside firms cannot be trusted, but the grouping still
can: whoever drew it did put all the walls on one layer, whatever they
called it. So measure each layer and let a person confirm the roles once.

On the drawing this was built against, the walls were spread over WALL,
WALL2, WALL3, WALL7, 1-wall牆, ST, A-梁線 and 玻璃欄杆 — three of which no
name-matching rule would ever have found.
"""

from __future__ import annotations

import math
import statistics

from autocad_mcp.analysis import dxfscan

# Above this many segments a layer is sampled for the pair statistic, which
# is O(n^2) inside each angle bin. The sample is reported, never silent.
_PAIR_SAMPLE = 2500

# Metric, converted to drawing units at call time — see walls.py for why a
# constant in raw units is a bug waiting for a millimetre drawing.
_MAX_GAP_M = 0.60
_PAIR_MIN_LENGTH_M = 0.20


def profile_layers(path: str, skip: tuple = dxfscan.DEFAULT_SKIP,
                   max_gap: float | None = None) -> dict:
    """Measure each layer. `max_gap` is the widest wall to look for, in
    drawing units; omitted, it scales from the drawing's own units."""
    _, ents, units = dxfscan.load(path)
    layers = dxfscan.by_layer(ents, skip=skip)
    if not layers:
        return {"error": "no geometry found after skip filter", "units": units}

    every = [s for segs in layers.values() for s in segs]
    unit_m = units.get("metres_per_unit") or 1.0
    if max_gap is None:
        max_gap = _MAX_GAP_M / unit_m
    min_pair_length = _PAIR_MIN_LENGTH_M / unit_m
    extent = dxfscan.main_extent(every, bin_size=5.0 / unit_m)
    box = extent["bbox"]

    rows = []
    for name, segs in layers.items():
        if len(segs) < 8:
            continue
        rows.append(_measure(name, segs, box, max_gap, unit_m, min_pair_length))
    rows.sort(key=lambda r: -r["total_length_m"])

    return {
        "units": units,
        "entities_flattened": len(ents),
        "extent_m": None if not box else {
            "width": round((box[2] - box[0]) * unit_m, 1),
            "height": round((box[3] - box[1]) * unit_m, 1),
        },
        "stray_segments_outside_extent": extent["dropped"],
        "layers_measured": len(rows),
        "layers": rows,
        "hint": "role_guess is a hint from geometry alone — confirm before use. "
                "Boundary layers are usually high orthogonality with a high "
                "parallel-pair rate; long median length means envelope or beam, "
                "short means partition.",
    }


def _measure(name: str, segs: list, box, max_gap: float, unit_m: float,
             min_pair_length: float) -> dict:
    lengths = [math.dist(s[0], s[1]) for s in segs]
    angles = [dxfscan.angle_deg(s) for s in segs]
    ortho = sum(1 for a in angles if min(a % 90, 90 - a % 90) <= 1.0) / len(angles)

    xs = [p[0] for s in segs for p in s]
    ys = [p[1] for s in segs for p in s]
    inside = 1.0
    if box:
        inside = sum(1 for s in segs if all(
            box[0] <= p[0] <= box[2] and box[1] <= p[1] <= box[3] for p in s)) / len(segs)

    pair_rate, sampled = _pair_rate(segs, angles, lengths, max_gap, min_pair_length)
    median_len = statistics.median(lengths)

    row = {
        "layer": name,
        "segments": len(segs),
        "total_length_m": round(sum(lengths) * unit_m, 1),
        "median_length_m": round(median_len * unit_m, 2),
        "bbox_m": [round((max(xs) - min(xs)) * unit_m, 1),
                   round((max(ys) - min(ys)) * unit_m, 1)],
        "orthogonal": round(ortho, 3),
        "parallel_pair_rate": round(pair_rate, 3),
        "inside_extent": round(inside, 3),
    }
    if sampled:
        row["pair_rate_sampled_from"] = _PAIR_SAMPLE
    row["role_guess"] = _guess(row, median_len * unit_m, ortho, pair_rate)
    return row


def _pair_rate(segs: list, angles: list, lengths: list, max_gap: float,
               min_length: float) -> tuple:
    """Share of segments that face a parallel neighbour within max_gap.

    Two lines drawn a consistent small distance apart, overlapping along
    their length, is what a wall looks like from below. Furniture and
    hatching do not produce it.
    """
    items = [(s, a, ln) for s, a, ln in zip(segs, angles, lengths) if ln > min_length]
    sampled = len(items) > _PAIR_SAMPLE
    if sampled:
        step = len(items) / _PAIR_SAMPLE
        items = [items[int(i * step)] for i in range(_PAIR_SAMPLE)]
    if not items:
        return 0.0, sampled

    bins: dict[int, list] = {}
    for it in items:
        bins.setdefault(round(it[1]), []).append(it)

    paired = 0
    for group in bins.values():
        for i, (s1, a1, l1) in enumerate(group):
            ux, uy = math.cos(math.radians(a1)), math.sin(math.radians(a1))
            nx, ny = -uy, ux
            d1 = s1[0][0] * nx + s1[0][1] * ny
            t1 = sorted([s1[0][0] * ux + s1[0][1] * uy, s1[1][0] * ux + s1[1][1] * uy])
            for s2, _a2, l2 in group[i + 1:]:
                gap = abs(d1 - (s2[0][0] * nx + s2[0][1] * ny))
                if gap < 1.0 or gap > max_gap:
                    continue
                t2 = sorted([s2[0][0] * ux + s2[0][1] * uy, s2[1][0] * ux + s2[1][1] * uy])
                if min(t1[1], t2[1]) - max(t1[0], t2[0]) < 0.5 * min(l1, l2):
                    continue
                paired += 1
                break
    return paired / len(items), sampled


def _guess(row: dict, median_m: float, ortho: float, pair: float) -> str:
    """Facing pairs plus segment length are what separate walls from the rest.

    Orthogonality deliberately does not gate the wall test: a structural
    layer measured here paired at 0.80 while sitting at 0.69 orthogonal
    because part of it runs on a diagonal, and gating on right angles threw
    it away. Median length does the opposite job — label boxes and opening
    marks pair up just as happily as walls do, but their segments are a
    fraction of a metre, so a length floor is what keeps them out.
    """
    if row["inside_extent"] < 0.3:
        return "圖框/範圍外"
    if pair >= 0.35 and median_m >= 0.35:
        return "牆-外牆或結構" if median_m >= 1.0 else "牆-隔間"
    if pair < 0.15 and median_m >= 1.5 and ortho >= 0.9:
        return "單線-軸線或梁線"
    if median_m < 0.15 and row["segments"] > 300:
        return "家具/設備細節"
    if pair >= 0.35:
        return "短線成對-標籤框或開口記號"
    if ortho < 0.5:
        return "曲線/文字外框/雜項"
    return "未分類"
