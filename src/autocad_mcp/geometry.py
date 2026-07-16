"""Normalise raw CAD geometry into the shapes a BIM model is built from.

Drawings describe a column as a rectangle of four corners and a beam as two
parallel lines. A model wants a centre point with a size and a rotation, or a
centreline with a width. Doing that conversion here — before anything is
modelled — means the counts and sizes can be checked against the drawing
without opening Revit.

Lives Python-side on purpose: AutoLISP has no arrays and its string building is
O(n^2), which makes it the wrong place for geometry.
"""

from __future__ import annotations

import math

# Tolerances are in drawing units / degrees. The caller knows the units, so
# anything length-based has to be passed in rather than guessed at here.
DEFAULT_ANGLE_TOL_DEG = 1.0


def _sub(a, b):
    return (a[0] - b[0], a[1] - b[1])


def _add(a, b):
    return (a[0] + b[0], a[1] + b[1])


def _scale(a, k):
    return (a[0] * k, a[1] * k)


def _dot(a, b):
    return a[0] * b[0] + a[1] * b[1]


def _norm(a):
    return math.hypot(a[0], a[1])


def _dedupe_closing_vertex(verts, tol=1e-9):
    """A closed rectangle is often stored with its first point repeated."""
    if len(verts) >= 2 and _norm(_sub(verts[0], verts[-1])) <= tol:
        return verts[:-1]
    return verts


def rectangle_from_verts(
    verts,
    angle_tol_deg: float = DEFAULT_ANGLE_TOL_DEG,
    rel_len_tol: float = 0.01,
):
    """Four corners -> {center, width, depth, rotation}, or None if not a rectangle.

    width is always the longer side and rotation is that side's angle, folded to
    [0, 180). Without that convention the same physical column would come back as
    (30x60 at 0 deg) or (60x30 at 90 deg) depending on which corner it was drawn
    from, and the two would not compare equal.
    """
    pts = _dedupe_closing_vertex([tuple(v) for v in verts])
    if len(pts) != 4:
        return None

    edges = [_sub(pts[(i + 1) % 4], pts[i]) for i in range(4)]
    lens = [_norm(e) for e in edges]
    if min(lens) <= 0:
        return None

    # opposite sides equal
    for i in (0, 1):
        longer = max(lens[i], lens[i + 2])
        if abs(lens[i] - lens[i + 2]) > rel_len_tol * longer:
            return None

    # corners square
    cos_tol = math.cos(math.radians(90 - angle_tol_deg))
    for i in range(4):
        a, b = edges[i], edges[(i + 1) % 4]
        if abs(_dot(a, b)) / (lens[i] * lens[(i + 1) % 4]) > cos_tol:
            return None

    center = (sum(p[0] for p in pts) / 4.0, sum(p[1] for p in pts) / 4.0)

    long_i = 0 if lens[0] >= lens[1] else 1
    width, depth = lens[long_i], lens[1 - long_i]
    rot = math.degrees(math.atan2(edges[long_i][1], edges[long_i][0])) % 180.0

    return {
        "center": [round(center[0], 6), round(center[1], 6)],
        "width": round(width, 6),
        "depth": round(depth, 6),
        "rotation": round(rot, 6),
    }


def rectangles_from_entities(entities, angle_tol_deg=DEFAULT_ANGLE_TOL_DEG, rel_len_tol=0.01):
    """Pull rectangles out of closed 4-vertex LWPOLYLINEs.

    Returns (rectangles, skipped). Nothing is dropped quietly — every entity that
    could not be read as a rectangle comes back in `skipped` with a reason, so a
    count that looks short can be explained.
    """
    rects, skipped = [], []
    for e in entities:
        if e.get("type") != "LWPOLYLINE":
            skipped.append({"handle": e.get("handle"), "type": e.get("type"),
                            "reason": "not an LWPOLYLINE"})
            continue
        if not e.get("closed"):
            skipped.append({"handle": e.get("handle"), "type": e.get("type"),
                            "reason": "polyline is not closed"})
            continue
        verts = e.get("verts") or []
        rect = rectangle_from_verts(verts, angle_tol_deg, rel_len_tol)
        if rect is None:
            n = len(_dedupe_closing_vertex([tuple(v) for v in verts]))
            skipped.append({
                "handle": e.get("handle"), "type": e.get("type"),
                "reason": f"not a rectangle ({n} corners, or sides not equal/square)",
            })
            continue
        rect["handle"] = e.get("handle")
        rect["layer"] = e.get("layer")
        rects.append(rect)
    return rects, skipped


def _line_dir(line):
    d = _sub(line["end"], line["start"])
    n = _norm(d)
    if n <= 0:
        return None, 0.0
    return _scale(d, 1.0 / n), n


def _angle_mod_180(u):
    return math.degrees(math.atan2(u[1], u[0])) % 180.0


def _angles_close(a, b, tol):
    d = abs(a - b) % 180.0
    return min(d, 180.0 - d) <= tol


def pair_double_lines(
    entities,
    max_width: float,
    min_width: float = 0.0,
    angle_tol_deg: float = DEFAULT_ANGLE_TOL_DEG,
    min_overlap: float = 0.0,
):
    """Pair parallel LINEs facing each other into centrelines.

    max_width is required and has no default: it is the only thing separating a
    beam's two faces from an unrelated parallel line somewhere else on the layer,
    and its value depends on the drawing's units, which this code cannot know.

    Pairing is greedy, nearest partner first: the two faces of one wall are
    closer to each other than either is to the next wall along. Each line is
    consumed once. Exactly-duplicated lines are folded before pairing.

    Returns (centerlines, unpaired, duplicates). This is a heuristic on geometry
    alone — it cannot know which face belongs to which wall, so check the width
    tally: real walls land on a few repeated thicknesses, and a smear of values
    means max_width is letting neighbouring walls pair with each other.
    """
    if max_width is None or max_width <= 0:
        raise ValueError("max_width must be a positive number in drawing units")

    lines = []
    unpaired = []
    duplicates = []
    seen = {}
    for e in entities:
        if e.get("type") != "LINE":
            unpaired.append({"handle": e.get("handle"), "type": e.get("type"),
                             "reason": "not a LINE"})
            continue
        u, length = _line_dir(e)
        if u is None:
            unpaired.append({"handle": e.get("handle"), "type": "LINE",
                             "reason": "zero-length line"})
            continue

        # Drawings routinely carry lines stacked exactly on top of each other.
        # Left in, each copy pairs off separately and the same wall is reported
        # twice, so fold them here and say how many were folded.
        a, b = tuple(round(c, 6) for c in e["start"]), tuple(round(c, 6) for c in e["end"])
        key = (a, b) if a <= b else (b, a)
        if key in seen:
            duplicates.append({"handle": e.get("handle"), "type": "LINE",
                               "reason": f"duplicate of {seen[key]}"})
            continue
        seen[key] = e.get("handle")

        lines.append({
            "handle": e.get("handle"), "layer": e.get("layer"),
            "p0": tuple(e["start"]), "p1": tuple(e["end"]),
            "u": u, "len": length, "angle": _angle_mod_180(u),
        })

    # candidate pairs, scored by how much of their length actually faces
    candidates = []
    for i in range(len(lines)):
        a = lines[i]
        for j in range(i + 1, len(lines)):
            b = lines[j]
            if not _angles_close(a["angle"], b["angle"], angle_tol_deg):
                continue
            u = a["u"]
            n = (-u[1], u[0])
            signed = _dot(_sub(b["p0"], a["p0"]), n)
            width = abs(signed)
            if width < min_width or width > max_width:
                continue
            if width <= 1e-9:
                continue  # collinear, not two faces of something

            a0, a1 = 0.0, a["len"]
            b0 = _dot(_sub(b["p0"], a["p0"]), u)
            b1 = _dot(_sub(b["p1"], a["p0"]), u)
            lo = max(min(a0, a1), min(b0, b1))
            hi = min(max(a0, a1), max(b0, b1))
            overlap = hi - lo
            if overlap <= min_overlap:
                continue
            candidates.append((width, -overlap, i, j, signed, lo, hi))

    # Nearest partner first, longest run breaking ties. Sorting by overlap alone
    # mis-pairs in real plans: a wall face sees its own opposite face AND a
    # neighbouring wall's face, and the neighbour often shares a longer run — so
    # the widths come back smeared across every gap that fit under max_width
    # instead of landing on the handful of real thicknesses.
    candidates.sort(key=lambda c: (c[0], c[1]))

    used = set()
    centerlines = []
    for width, neg_overlap, i, j, signed, lo, hi in candidates:
        if i in used or j in used:
            continue
        used.add(i)
        used.add(j)
        a = lines[i]
        u = a["u"]
        n = (-u[1], u[0])
        origin = _add(a["p0"], _scale(n, signed / 2.0))
        start = _add(origin, _scale(u, lo))
        end = _add(origin, _scale(u, hi))
        centerlines.append({
            "start": [round(start[0], 6), round(start[1], 6)],
            "end": [round(end[0], 6), round(end[1], 6)],
            "width": round(abs(signed), 6),
            "length": round(-neg_overlap, 6),
            "angle": round(a["angle"], 6),
            "layer": a["layer"],
            "handles": [a["handle"], lines[j]["handle"]],
        })

    for idx, ln in enumerate(lines):
        if idx not in used:
            unpaired.append({
                "handle": ln["handle"], "type": "LINE",
                "reason": f"no parallel partner within width {min_width}..{max_width}",
                "angle": round(ln["angle"], 6), "length": round(ln["len"], 6),
            })

    return centerlines, unpaired, duplicates
