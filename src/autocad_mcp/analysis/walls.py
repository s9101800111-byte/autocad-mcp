"""Find walls as pairs of parallel lines, and report the thicknesses used.

Nothing here is told what a wall is. The thicknesses fall out of the gap
histogram: on the drawing this was built against, 15 cm and 10 cm stood out
at 265 and 205 pairs against a long tail of noise — RC and stud partition,
discovered rather than assumed.

Each matched pair also yields a centreline, which is what a Revit wall wants.
"""

from __future__ import annotations

import math

from autocad_mcp.analysis import dxfscan

# Metric defaults, converted to drawing units at call time.
_MAX_GAP_M = 0.60      # thicker than this is a room, not a wall
_MIN_GAP_M = 0.01      # thinner is the same line drawn twice
_MIN_LENGTH_M = 0.20   # shorter is a jamb, a label box, an opening mark


def detect_walls(path: str, layers: list | None = None, max_gap: float | None = None,
                 min_gap: float | None = None, min_length: float | None = None,
                 angle_tol_deg: float = 1.0, min_overlap: float = 0.5) -> dict:
    """Pair up facing lines on `layers` (all non-skipped layers if omitted).

    `max_gap` / `min_gap` / `min_length` are in DRAWING UNITS when given.
    Left out, they come from the metric defaults below scaled by the
    drawing's own units — a fixed 20 would mean 20 cm on a centimetre
    drawing but 2 cm on a millimetre one, which is how a default silently
    becomes a bug. Everything a pair could not be found for is counted and
    reported: a thin result should be explainable, not trusted blindly.
    """
    _, ents, units = dxfscan.load(path)
    by_layer = dxfscan.by_layer(ents)
    if layers:
        wanted = set(layers)
        by_layer = {k: v for k, v in by_layer.items() if k in wanted}
        missing = sorted(wanted - set(by_layer))
    else:
        missing = []

    segs = [s for v in by_layer.values() for s in v]
    if not segs:
        return {"error": "no segments on the requested layers",
                "layers_requested": layers, "layers_not_found": missing}

    unit_m = units.get("metres_per_unit") or 1.0
    if max_gap is None:
        max_gap = _MAX_GAP_M / unit_m
    if min_gap is None:
        min_gap = _MIN_GAP_M / unit_m
    if min_length is None:
        min_length = _MIN_LENGTH_M / unit_m

    extent = dxfscan.main_extent(segs, bin_size=5.0 / unit_m)
    segs = extent.get("segments") or segs

    long_enough = [s for s in segs if math.dist(s[0], s[1]) >= min_length]
    pairs, unpaired = _pair(long_enough, max_gap, min_gap, angle_tol_deg, min_overlap)

    hist: dict[float, int] = {}
    for p in pairs:
        key = round(p["thickness"], 1)
        hist[key] = hist.get(key, 0) + 1
    peaks = sorted(hist.items(), key=lambda kv: -kv[1])

    return {
        "units": units,
        "layers_used": sorted(by_layer),
        "layers_not_found": missing,
        "segments_considered": len(long_enough),
        "segments_too_short": len(segs) - len(long_enough),
        "stray_segments_outside_extent": extent["dropped"],
        "pairs_found": len(pairs),
        "unpaired": unpaired,
        "thickness_peaks": [
            {"thickness_m": round(t * unit_m, 3), "pairs": n}
            for t, n in peaks[:12]
        ],
        "centerlines": [
            {"start": [round(v, 3) for v in p["start"]],
             "end": [round(v, 3) for v in p["end"]],
             "thickness_m": round(p["thickness"] * unit_m, 3),
             "length_m": round(p["length"] * unit_m, 3),
             "angle": round(p["angle"], 2)}
            for p in pairs
        ],
    }


def _pair(segs: list, max_gap: float, min_gap: float,
          angle_tol: float, min_overlap: float) -> tuple:
    """Match each segment to its nearest facing parallel partner.

    Segments are bucketed by angle first so this stays O(n^2) per bucket
    rather than over the whole drawing. Each segment is consumed once, so a
    three-line wall section yields one pair and one leftover rather than two
    overlapping walls.
    """
    bins: dict[int, list] = {}
    for s in segs:
        a = dxfscan.angle_deg(s)
        bins.setdefault(int(round(a / max(angle_tol, 0.5))), []).append((s, a))

    used = set()
    pairs = []
    for group in bins.values():
        for i, (s1, a1) in enumerate(group):
            if i in used:
                continue
            ux, uy = math.cos(math.radians(a1)), math.sin(math.radians(a1))
            nx, ny = -uy, ux
            d1 = s1[0][0] * nx + s1[0][1] * ny
            t1 = sorted([s1[0][0] * ux + s1[0][1] * uy, s1[1][0] * ux + s1[1][1] * uy])
            l1 = t1[1] - t1[0]

            best = None
            for j in range(i + 1, len(group)):
                if j in used:
                    continue
                s2, a2 = group[j]
                if abs(a1 - a2) > angle_tol and abs(abs(a1 - a2) - 180) > angle_tol:
                    continue
                gap = abs(d1 - (s2[0][0] * nx + s2[0][1] * ny))
                if gap < min_gap or gap > max_gap:
                    continue
                t2 = sorted([s2[0][0] * ux + s2[0][1] * uy,
                             s2[1][0] * ux + s2[1][1] * uy])
                overlap = min(t1[1], t2[1]) - max(t1[0], t2[0])
                if overlap < min_overlap * min(l1, t2[1] - t2[0]):
                    continue
                if best is None or gap < best[0]:
                    best = (gap, j, t2, overlap)
            if best is None:
                continue

            gap, j, t2, overlap = best
            used.add(i)
            used.add(j)
            d2 = group[j][0][0][0] * nx + group[j][0][0][1] * ny
            dm = (d1 + d2) / 2.0
            lo, hi = max(t1[0], t2[0]), min(t1[1], t2[1])
            pairs.append({
                "start": (dm * nx + lo * ux, dm * ny + lo * uy),
                "end": (dm * nx + hi * ux, dm * ny + hi * uy),
                "thickness": gap,
                "length": hi - lo,
                "angle": a1,
            })

    unpaired = sum(len(g) for g in bins.values()) - 2 * len(pairs)
    return pairs, unpaired
