"""Read a DXF into flat line segments, per layer.

Two things here are not optional on real drawings:

  * Block recursion. An XREF that has been bound leaves the geometry inside
    block definitions, so modelspace can look almost empty while the walls
    sit two levels down. One permit drawing tested here went from 1,175
    modelspace entities to 64,246 after recursion.

  * Stray geometry. A mirrored or misplaced insert can sit hundreds of
    metres away and wreck any bounding box taken from min/max. Percentiles
    are not enough when the stray holds more than a few percent of the
    points, so the extent comes from the largest contiguous run of a coarse
    histogram instead.
"""

from __future__ import annotations

import math

# Metres per drawing unit, for the units that show up in practice.
_METRES = {
    1: 0.0254, 2: 0.3048, 3: 1609.344, 4: 0.001, 5: 0.01, 6: 1.0, 7: 1000.0,
    10: 0.9144, 14: 0.1, 15: 10.0, 16: 100.0,
}
_UNIT_NAMES = {
    0: "unitless", 1: "inches", 2: "feet", 4: "millimeters", 5: "centimeters",
    6: "meters", 14: "decimeters",
}

# Layers that never bound a space: annotation, sheet furniture, helpers.
# Matched case-insensitively as substrings, so "DIM結構體" and "A-DIM-01"
# both drop out. Callers can override.
DEFAULT_SKIP = ("dim", "defpoints", "圖框", "text", "文字", "invisible", "grid", "軸線")

_ARC_STEP = 0.25  # radians per chord when flattening arcs


def load(path: str, max_depth: int = 6):
    """Open a DXF and return (doc, flattened entities, units dict).

    Flattening applies each INSERT's transform, so coordinates come back in
    world space regardless of nesting depth.
    """
    import ezdxf

    doc = ezdxf.readfile(path)
    ents = list(_flatten(doc.modelspace(), 0, max_depth))
    code = doc.header.get("$INSUNITS", 0)
    units = {
        "insunits": code,
        "name": _UNIT_NAMES.get(code, f"code {code}"),
        "metres_per_unit": _METRES.get(code),
    }
    return doc, ents, units


def _flatten(entities, depth: int, max_depth: int):
    for e in entities:
        if e.dxftype() == "INSERT" and depth < max_depth:
            try:
                yield from _flatten(e.virtual_entities(), depth + 1, max_depth)
            except Exception:
                # A dynamic block whose representation data cannot be copied
                # still counts as visited; skipping it beats aborting the read.
                continue
        else:
            yield e


def segments(entity) -> list:
    """Every straight piece of one entity, as ((x1,y1),(x2,y2)) tuples.

    Covers the types that bound space. POLYLINE (the pre-1997 heavy kind) is
    included deliberately: older drawings from outside firms use it heavily
    and leaving it out loses geometry silently.
    """
    t = entity.dxftype()
    if t == "LINE":
        return [((entity.dxf.start.x, entity.dxf.start.y),
                 (entity.dxf.end.x, entity.dxf.end.y))]
    if t == "LWPOLYLINE":
        pts = [(p[0], p[1]) for p in entity.get_points("xy")]
        return _chain(pts, entity.closed)
    if t == "POLYLINE":
        try:
            pts = [(v.dxf.location.x, v.dxf.location.y) for v in entity.vertices]
        except Exception:
            return []
        return _chain(pts, entity.is_closed)
    if t == "ARC":
        c, r = entity.dxf.center, entity.dxf.radius
        a0 = math.radians(entity.dxf.start_angle)
        a1 = math.radians(entity.dxf.end_angle)
        if a1 < a0:
            a1 += 2 * math.pi
        n = max(4, int((a1 - a0) / _ARC_STEP))
        pts = [(c.x + r * math.cos(a0 + (a1 - a0) * i / n),
                c.y + r * math.sin(a0 + (a1 - a0) * i / n)) for i in range(n + 1)]
        return _chain(pts, False)
    if t == "CIRCLE":
        c, r = entity.dxf.center, entity.dxf.radius
        pts = [(c.x + r * math.cos(2 * math.pi * i / 32),
                c.y + r * math.sin(2 * math.pi * i / 32)) for i in range(33)]
        return _chain(pts, False)
    return []


def _chain(pts: list, closed: bool) -> list:
    if len(pts) < 2:
        return []
    idx = range(len(pts)) if closed else range(len(pts) - 1)
    return [(pts[i], pts[(i + 1) % len(pts)]) for i in idx]


def by_layer(entities, skip: tuple = DEFAULT_SKIP, min_length: float = 0.3) -> dict:
    """Group flattened entities into segments keyed by layer name."""
    skip_lc = tuple(s.lower() for s in skip)
    out: dict[str, list] = {}
    for e in entities:
        layer = e.dxf.get("layer", "0")
        low = layer.lower()
        if any(s in low for s in skip_lc):
            continue
        segs = [s for s in segments(e) if _length(s) >= min_length]
        if segs:
            out.setdefault(layer, []).extend(segs)
    return out


def _length(seg) -> float:
    return math.dist(seg[0], seg[1])


def main_extent(segs: list, bin_size: float) -> dict:
    """Bounding box of the densest contiguous run, in drawing units.

    `bin_size` should be a few metres in drawing units — coarse enough that a
    real floor plan lands in one unbroken run of bins, fine enough that a
    stray insert lands in its own. Returns the box plus how much was dropped,
    so a caller can tell "clean drawing" from "we threw away half of it".
    """
    if not segs:
        return {"bbox": None, "kept": 0, "dropped": 0}
    mids_x = [(s[0][0] + s[1][0]) / 2 for s in segs]
    mids_y = [(s[0][1] + s[1][1]) / 2 for s in segs]
    x0, x1 = _densest_run(mids_x, bin_size)
    y0, y1 = _densest_run(mids_y, bin_size)
    kept = [s for s in segs
            if all(x0 <= p[0] <= x1 and y0 <= p[1] <= y1 for p in s)]
    if not kept:
        return {"bbox": None, "kept": 0, "dropped": len(segs)}
    xs = [p[0] for s in kept for p in s]
    ys = [p[1] for s in kept for p in s]
    return {
        "bbox": (min(xs), min(ys), max(xs), max(ys)),
        "kept": len(kept),
        "dropped": len(segs) - len(kept),
        "segments": kept,
    }


def _densest_run(vals: list, bin_size: float) -> tuple:
    lo = min(vals)
    counts: dict[int, int] = {}
    for v in vals:
        b = int((v - lo) / bin_size)
        counts[b] = counts.get(b, 0) + 1
    mode = max(counts, key=lambda k: counts[k])
    a = b = mode
    while (a - 1) in counts:
        a -= 1
    while (b + 1) in counts:
        b += 1
    return lo + a * bin_size, lo + (b + 1) * bin_size


def angle_deg(seg) -> float:
    """Segment direction folded into [0,180) — a wall has no near/far side."""
    return math.degrees(math.atan2(seg[1][1] - seg[0][1],
                                   seg[1][0] - seg[0][0])) % 180.0
