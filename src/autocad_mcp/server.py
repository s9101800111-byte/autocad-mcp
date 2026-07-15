"""AutoCAD MCP Server v3.1 — 8 consolidated tools with operation dispatch.

Tools: drawing, entity, layer, block, annotation, pid, view, system
"""

from __future__ import annotations

import structlog
from mcp.server.fastmcp import FastMCP

from autocad_mcp.backends.base import CommandResult
from autocad_mcp.client import (
    _error,
    _json,
    _safe,
    add_screenshot_if_available,
    get_backend,
)

# FastMCP validates return types via Pydantic. Tools that may return
# ImageContent (screenshot) alongside TextContent need a union return type.
ToolResult = str | list

# Default cap for entity reads. The LISP side builds its JSON by strcat, which
# is O(n^2), so an uncapped read of a large drawing exceeds the IPC timeout.
# Truncation is always reported (never silent) — see the entity docstring.
DEFAULT_READ_LIMIT = 200

# INSUNITS codes — nobody remembers that 4 means millimetres.
_INSUNITS_NAMES = {
    0: "unitless", 1: "inches", 2: "feet", 3: "miles", 4: "millimeters",
    5: "centimeters", 6: "meters", 7: "kilometers", 8: "microinches", 9: "mils",
    10: "yards", 11: "angstroms", 12: "nanometers", 13: "microns", 14: "decimeters",
    15: "decameters", 16: "hectometers", 17: "gigameters", 18: "astronomical units",
    19: "light years", 20: "parsecs", 21: "us survey feet", 22: "us survey inch",
    23: "us survey yard", 24: "us survey mile",
}


# Metres per drawing unit, for the units that show up in practice.
_INSUNITS_METRES = {
    1: 0.0254, 2: 0.3048, 3: 1609.344, 4: 0.001, 5: 0.01, 6: 1.0, 7: 1000.0,
    10: 0.9144, 14: 0.1, 15: 10.0, 16: 100.0,
}


def _extent_span(p: dict) -> dict | None:
    """Drawing extents as a span, plus the real-world size the declared units imply.

    A declared unit is only ever wrong in context: seeing "spans 3492 m" next to
    "centimeters" is what makes a mis-set INSUNITS obvious to a reader. Left as a
    plain report rather than a warning — plenty of site drawings really are that
    big, and a guess here would cry wolf.
    """
    lo, hi = p.get("extmin"), p.get("extmax")
    if not lo or not hi:
        return None
    span = {"x": round(hi[0] - lo[0], 6), "y": round(hi[1] - lo[1], 6)}
    factor = _INSUNITS_METRES.get(p.get("insunits"))
    if factor:
        span["meters"] = [round(span["x"] * factor, 3), round(span["y"] * factor, 3)]
    return span


def _units_warnings(p: dict) -> list[str]:
    """Flag the settings that silently skew a linked/inserted drawing.

    This is the point of reading these values at all: an INSUNITS of 0 or a
    shifted base/UCS does not fail loudly, it just puts the geometry in the
    wrong place or at the wrong scale after the link.
    """
    warnings = []

    if p.get("insunits") == 0:
        warnings.append(
            "INSUNITS is 0 (unitless): AutoCAD cannot rescale this drawing on "
            "insert/link, so it lands at whatever the target's units imply. Set "
            "INSUNITS before linking."
        )

    def moved(v):
        return v is not None and any(abs(float(c)) > 1e-9 for c in v)

    if moved(p.get("insbase")):
        warnings.append(
            f"INSBASE is {p['insbase']}, not the origin: an insert lands offset "
            "by this vector."
        )
    if moved(p.get("ucsorg")):
        warnings.append(
            f"UCS origin is {p['ucsorg']}, not the WCS origin. Coordinates this "
            "server reads and writes are WCS, so they will not match what you "
            "pick on screen."
        )

    x, y = p.get("ucsxdir"), p.get("ucsydir")
    rotated = (x is not None and (abs(x[0] - 1) > 1e-9 or abs(x[1]) > 1e-9)) or (
        y is not None and (abs(y[0]) > 1e-9 or abs(y[1] - 1) > 1e-9)
    )
    if rotated:
        warnings.append(
            f"UCS is rotated (X axis {x}, Y axis {y}); the same caveat as a "
            "shifted origin applies."
        )

    return warnings

log = structlog.get_logger()

mcp = FastMCP("autocad-mcp")


# ==========================================================================
# 1. drawing — File/drawing management
# ==========================================================================


@mcp.tool(annotations={"title": "AutoCAD Drawing Operations", "readOnlyHint": False})
@_safe("drawing")
async def drawing(
    operation: str,
    data: dict | None = None,
    include_screenshot: bool = False,
) -> ToolResult:
    """Drawing file management.

    Operations:
      create     — Create a new empty drawing. data: {name?}
      open       — Open an existing drawing. data: {path}
      info       — Get drawing extents, entity count, layers, blocks.
      save       — Save current drawing. data: {path?} (saves to path if given, else QSAVE)
      save_as_dxf — Export as DXF. data: {path}
      plot_pdf   — Plot to PDF. data: {path}
      purge      — Purge unused objects.
      get_variables — Get system variables. data: {names: [...]}
      get_units_and_base — What decides whether this drawing lands correctly
        when linked or inserted: units, insertion base, active UCS, extents.
        → {insunits, insunits_name, measurement, insbase, ucsname, ucsorg,
           ucsxdir, ucsydir, extmin, extmax, extent_span, limmin, limmax,
           lunits, luprec, aunits, auprec, ctab, tilemode, dwgname,
           warnings: [...]}
        warnings flags what silently skews a link: unitless INSUNITS, an
        insertion base off the origin, a shifted or rotated UCS. Worth
        checking before link_cads_by_floor — none of these fail loudly.
        extent_span gives the drawing size in the declared units and in
        metres; a mis-set INSUNITS shows up there as an implausible size
        rather than as an error. limmin/limmax are usually a stale default
        and say nothing about the units — don't read them as evidence.
      set_insertion_base — data: {x, y, z?}  Sets INSBASE.
      undo       — Undo last operation.
      redo       — Redo last undone operation.
      wblock_by_regions — Batch WBLOCK by closed polylines on a layer.
        data: {boundary_layer, output_dir, name_template?, index_start?,
               index_pad?, text_layer?}
        name_template placeholders: {index} {text} {handle} {layer}
        Base point = (0,0,0). Boundary polyline is included in output.
    """
    data = data or {}
    backend = await get_backend()

    if operation == "create":
        result = await backend.drawing_create(data.get("name"))
    elif operation == "info":
        result = await backend.drawing_info()
    elif operation == "save":
        result = await backend.drawing_save(data.get("path"))
    elif operation == "save_as_dxf":
        result = await backend.drawing_save_as_dxf(data["path"])
    elif operation == "plot_pdf":
        result = await backend.drawing_plot_pdf(data["path"])
    elif operation == "purge":
        result = await backend.drawing_purge()
    elif operation == "get_variables":
        result = await backend.drawing_get_variables(data.get("names"))
    elif operation == "get_units_and_base":
        result = await backend.drawing_get_units_and_base()
        if result.ok and isinstance(result.payload, dict):
            result.payload["insunits_name"] = _INSUNITS_NAMES.get(
                result.payload.get("insunits"), "unknown"
            )
            result.payload["extent_span"] = _extent_span(result.payload)
            result.payload["warnings"] = _units_warnings(result.payload)
    elif operation == "set_insertion_base":
        result = await backend.drawing_set_insertion_base(
            data["x"], data["y"], data.get("z", 0.0)
        )
    elif operation == "open":
        result = await backend.drawing_open(data["path"])
    elif operation == "undo":
        result = await backend.undo()
    elif operation == "redo":
        result = await backend.redo()
    elif operation == "wblock_by_regions":
        result = await backend.drawing_wblock_by_regions(
            data["boundary_layer"],
            data["output_dir"],
            data.get("name_template", "region_{index}"),
            int(data.get("index_start", 1)),
            int(data.get("index_pad", 2)),
            data.get("text_layer"),
        )
    else:
        return _json({"error": f"Unknown drawing operation: {operation}"})

    return await add_screenshot_if_available(result, include_screenshot)


# ==========================================================================
# 2. entity — Entity CRUD + modification
# ==========================================================================


@mcp.tool(annotations={"title": "AutoCAD Entity Operations", "readOnlyHint": False})
@_safe("entity")
async def entity(
    operation: str,
    x1: float | None = None,
    y1: float | None = None,
    x2: float | None = None,
    y2: float | None = None,
    points: list[list[float]] | None = None,
    layer: str | None = None,
    entity_id: str | None = None,
    data: dict | None = None,
    include_screenshot: bool = False,
) -> ToolResult:
    """Entity creation, querying, and modification.

    Create operations:
      create_line       — x1, y1, x2, y2, layer?
      create_circle     — data: {cx, cy, radius}, layer?
      create_polyline   — points: [[x,y],...], data: {closed?}, layer?
      create_rectangle  — x1, y1, x2, y2, layer?
      create_arc        — data: {cx, cy, radius, start_angle, end_angle}, layer?
      create_ellipse    — data: {cx, cy, major_x, major_y, ratio}, layer?
      create_mtext      — data: {x, y, width, text, height?}, layer?
      create_hatch      — entity_id, data: {pattern?}

    Read operations:
      list              — layer? → list entities
      count             — layer? → count entities
      get               — entity_id → entity details
      get_selection     — Read the user's current pick/grip selection in AutoCAD.
                          data: {limit?}  (File IPC only — needs interactive AutoCAD.)
      query             — Filter entities without round-tripping each handle.
                          data: {layer?, etype?, text?, window?: [x1,y1,x2,y2], mode?, limit?}
                            layer  — name/wildcard/comma-OR, e.g. "S-*,A-WALL"
                            etype  — DXF type/comma-OR, e.g. "LINE,LWPOLYLINE"
                            text   — group-1 wildcard for TEXT/MTEXT/ATTRIB, e.g. "*3F*"
                            window — spatial box; mode "crossing" (default) or "inside"
                            limit  — max entities returned (default 200; 0 = no cap)
      find_text         — Search drawing text, including block attributes.
                          data: {pattern, layer?, window?, mode?, limit?,
                                 ignore_case?, include_attribs?}
                            pattern — AutoCAD wildcard, NOT regex: * ? # @ ~ [] ,
                                      e.g. "*3F*", "#F", "A-##"
                          Unlike query's text filter, this matches MTEXT with its
                          formatting codes stripped, and reaches ATTRIB text inside
                          block inserts (ssget cannot select those directly).
                          ATTRIB hits also carry {tag, block}. When filtering by
                          layer/window, an ATTRIB is matched via its INSERT.

    These reads return:
      {count, returned, truncated, entities: [{type, handle, layer, point?, text?}]}
      count is the full match total even when truncated, so a capped result
      still tells you how many exist. Raise limit (or 0) to fetch more, but an
      uncapped read of thousands of entities will blow the IPC timeout —
      narrow it with layer/etype/window instead.

    Modify operations:
      copy    — entity_id, data: {dx, dy}
      move    — entity_id, data: {dx, dy}
      rotate  — entity_id, data: {cx, cy, angle}
      scale   — entity_id, data: {cx, cy, factor}
      mirror  — entity_id, x1, y1, x2, y2
      offset  — entity_id, data: {distance}
      array   — entity_id, data: {rows, cols, row_dist, col_dist}
      fillet  — data: {id1, id2, radius}
      chamfer — data: {id1, id2, dist1, dist2}
      erase   — entity_id

    Batch:
      batch — Run several entity operations in one tool call.
              data: {ops: [{op, ...params}, ...], stop_on_error?}
              Each op is a flat dict — write every param at the op's top level,
              whether the op normally takes it as an argument or inside data:
                {"op": "create_line", "x1": 0, "y1": 0, "x2": 10, "y2": 0,
                 "layer": "A-WALL"}
                {"op": "query", "layer": "A-GRID", "etype": "LINE", "limit": 50}
              stop_on_error defaults true — set false to run the rest anyway.
              → {count, executed, ok_count, failed_count, stopped_early,
                 results: [{index, op, ok, payload?|error?}]}
              This saves tool-call round-trips, not IPC dispatches: ops still
              execute sequentially, one AutoCAD dispatch each.
    """
    data = data or {}
    backend = await get_backend()

    if operation == "batch":
        result = await _entity_batch(
            backend, data.get("ops") or [], bool(data.get("stop_on_error", True))
        )
    else:
        result = await _entity_dispatch(
            backend, operation, x1, y1, x2, y2, points, layer, entity_id, data
        )
    return await add_screenshot_if_available(result, include_screenshot)


async def _entity_dispatch(
    backend,
    operation: str,
    x1: float | None = None,
    y1: float | None = None,
    x2: float | None = None,
    y2: float | None = None,
    points: list[list[float]] | None = None,
    layer: str | None = None,
    entity_id: str | None = None,
    data: dict | None = None,
) -> CommandResult:
    """Route one entity operation to the backend.

    Shared by the entity tool and entity.batch. Returns CommandResult rather
    than serialized JSON so batch can collect per-op outcomes.
    """
    data = data or {}

    # --- Create ---
    if operation == "create_line":
        result = await backend.create_line(x1, y1, x2, y2, layer)
    elif operation == "create_circle":
        result = await backend.create_circle(data["cx"], data["cy"], data["radius"], layer)
    elif operation == "create_polyline":
        result = await backend.create_polyline(points or [], data.get("closed", False), layer)
    elif operation == "create_rectangle":
        result = await backend.create_rectangle(x1, y1, x2, y2, layer)
    elif operation == "create_arc":
        result = await backend.create_arc(data["cx"], data["cy"], data["radius"], data["start_angle"], data["end_angle"], layer)
    elif operation == "create_ellipse":
        result = await backend.create_ellipse(data["cx"], data["cy"], data["major_x"], data["major_y"], data["ratio"], layer)
    elif operation == "create_mtext":
        result = await backend.create_mtext(data["x"], data["y"], data["width"], data["text"], data.get("height", 2.5), layer)
    elif operation == "create_hatch":
        result = await backend.create_hatch(entity_id, data.get("pattern", "ANSI31"))
    # --- Read ---
    elif operation == "list":
        result = await backend.entity_list(layer)
    elif operation == "count":
        result = await backend.entity_count(layer)
    elif operation == "get":
        result = await backend.entity_get(entity_id)
    elif operation == "get_selection":
        result = await backend.entity_get_selection(int(data.get("limit", DEFAULT_READ_LIMIT)))
    elif operation == "query":
        result = await backend.entity_query(
            data.get("layer"), data.get("etype"), data.get("text"),
            data.get("window"), data.get("mode", "crossing"),
            int(data.get("limit", DEFAULT_READ_LIMIT)),
        )
    elif operation == "find_text":
        result = await backend.entity_find_text(
            data["pattern"], data.get("layer"),
            data.get("window"), data.get("mode", "crossing"),
            int(data.get("limit", DEFAULT_READ_LIMIT)),
            bool(data.get("ignore_case", True)),
            bool(data.get("include_attribs", True)),
        )
    # --- Modify ---
    elif operation == "copy":
        result = await backend.entity_copy(entity_id, data["dx"], data["dy"])
    elif operation == "move":
        result = await backend.entity_move(entity_id, data["dx"], data["dy"])
    elif operation == "rotate":
        result = await backend.entity_rotate(entity_id, data["cx"], data["cy"], data["angle"])
    elif operation == "scale":
        result = await backend.entity_scale(entity_id, data["cx"], data["cy"], data["factor"])
    elif operation == "mirror":
        result = await backend.entity_mirror(entity_id, x1, y1, x2, y2)
    elif operation == "offset":
        result = await backend.entity_offset(entity_id, data["distance"])
    elif operation == "array":
        result = await backend.entity_array(entity_id, data["rows"], data["cols"], data["row_dist"], data["col_dist"])
    elif operation == "fillet":
        result = await backend.entity_fillet(data["id1"], data["id2"], data["radius"])
    elif operation == "chamfer":
        result = await backend.entity_chamfer(data["id1"], data["id2"], data["dist1"], data["dist2"])
    elif operation == "erase":
        result = await backend.entity_erase(entity_id)
    else:
        return CommandResult(ok=False, error=f"Unknown entity operation: {operation}")

    return result


# Params the entity tool takes at top level. A batch op is a flat dict, so these
# are lifted out — but they are ALSO left in the op's `data`, because the two
# calling conventions overlap: count/list read `layer` as a top-level arg while
# query/find_text read it out of `data`. Passing both lets each op read from
# where it expects; the other copy is simply ignored.
_BATCH_TOP_LEVEL = {"x1", "y1", "x2", "y2", "points", "layer", "entity_id"}


async def _entity_batch(backend, ops: list, stop_on_error: bool = True) -> CommandResult:
    """Run a list of entity operations sequentially, reporting each outcome."""
    if not isinstance(ops, list):
        return CommandResult(ok=False, error="data.ops must be a list of operations")

    results = []
    stopped_early = False
    for i, o in enumerate(ops):
        if not isinstance(o, dict) or not o.get("op"):
            results.append({"index": i, "ok": False, "error": "each op needs an 'op' key"})
            if stop_on_error:
                stopped_early = True
                break
            continue

        name = o["op"]
        if name == "batch":
            results.append({"index": i, "op": name, "ok": False, "error": "batch cannot nest"})
            if stop_on_error:
                stopped_early = True
                break
            continue

        top = {k: v for k, v in o.items() if k in _BATCH_TOP_LEVEL}
        op_data = {k: v for k, v in o.items() if k != "op"}
        try:
            r = await _entity_dispatch(backend, name, data=op_data, **top)
        except Exception as e:  # a bad op must not abort the whole batch report
            r = CommandResult(ok=False, error=f"{type(e).__name__}: {e}")

        entry = {"index": i, "op": name, "ok": r.ok}
        if r.ok:
            entry["payload"] = r.payload
        else:
            entry["error"] = r.error
        results.append(entry)

        if not r.ok and stop_on_error:
            stopped_early = True
            break

    ok_count = sum(1 for r in results if r["ok"])
    return CommandResult(ok=True, payload={
        "count": len(ops),
        "executed": len(results),
        "ok_count": ok_count,
        "failed_count": len(results) - ok_count,
        "stopped_early": stopped_early,
        "results": results,
    })


# ==========================================================================
# 3. layer — Layer management
# ==========================================================================


@mcp.tool(annotations={"title": "AutoCAD Layer Operations", "readOnlyHint": False})
@_safe("layer")
async def layer(
    operation: str,
    data: dict | None = None,
    include_screenshot: bool = False,
) -> ToolResult:
    """Layer creation and management.

    Operations:
      list            — List all layers with properties.
      create          — data: {name, color?, linetype?}
      set_current     — data: {name}
      set_properties  — data: {name, color?, linetype?, lineweight?}
      freeze          — data: {name}
      thaw            — data: {name}
      lock            — data: {name}
      unlock          — data: {name}
      translate       — Rename/merge layers from a mapping table.
        data: {map: {"OLD": "NEW", ...}, dry_run?, purge?}
          map     — source may use wildcards ("VENDOR_*": "A-WALL"); several
                    sources mapping to one target merges them.
          dry_run — count what would move without touching the drawing.
                    Run this first: merging discards the original layer
                    assignment and there is no per-entity undo of the map.
          purge   — drop each emptied source layer (default true).
        → {dry_run, total_entities, results: [{from, to, entities,
           target_created, source_purged}]}
        Only entities in the drawing move. Geometry inside block definitions
        keeps its own layer, which is also why a source layer may survive
        purge — a block definition still references it.
    """
    data = data or {}
    backend = await get_backend()

    if operation == "list":
        result = await backend.layer_list()
    elif operation == "create":
        result = await backend.layer_create(data["name"], data.get("color", "white"), data.get("linetype", "CONTINUOUS"))
    elif operation == "set_current":
        result = await backend.layer_set_current(data["name"])
    elif operation == "set_properties":
        result = await backend.layer_set_properties(data["name"], data.get("color"), data.get("linetype"), data.get("lineweight"))
    elif operation == "freeze":
        result = await backend.layer_freeze(data["name"])
    elif operation == "thaw":
        result = await backend.layer_thaw(data["name"])
    elif operation == "lock":
        result = await backend.layer_lock(data["name"])
    elif operation == "unlock":
        result = await backend.layer_unlock(data["name"])
    elif operation == "translate":
        mapping = data.get("map")
        if not isinstance(mapping, dict) or not mapping:
            return _json({"error": "layer.translate needs data.map = {\"OLD\": \"NEW\", ...}"})
        result = await backend.layer_translate(
            mapping, bool(data.get("dry_run", False)), bool(data.get("purge", True))
        )
    else:
        return _json({"error": f"Unknown layer operation: {operation}"})

    return await add_screenshot_if_available(result, include_screenshot)


# ==========================================================================
# 4. block — Block operations
# ==========================================================================


@mcp.tool(annotations={"title": "AutoCAD Block Operations", "readOnlyHint": False})
@_safe("block")
async def block(
    operation: str,
    data: dict | None = None,
    include_screenshot: bool = False,
) -> ToolResult:
    """Block definition, insertion, and attribute management.

    Operations:
      list                 — List all block definitions.
      insert               — data: {name, x, y, scale?, rotation?, block_id?}
      insert_with_attributes — data: {name, x, y, scale?, rotation?, attributes: {tag: value}}
      get_attributes       — data: {entity_id}
      update_attribute     — data: {entity_id, tag, value}
      define               — data: {name, entities: [{type, ...}]}
      extract_attributes_csv — Every attributed insert in one table.
        data: {block?, layer?, window?, mode?, limit?, path?, overwrite?}
          block  — block name wildcard/comma-OR, e.g. "A-圖框", "W-*"
          path   — write a CSV here; without it the rows are just returned
          limit  — 0 (default) means no cap: an export should be complete.
                   A huge uncapped set can blow the IPC timeout — narrow it
                   with block/layer instead. Truncation is always reported.
        → {count, returned, truncated, blocks: [{block, handle, layer,
           point, attributes: {tag: value}}], csv?: {path, rows, columns}}
        CSV columns are block, handle, layer, x, y + the union of every tag
        seen, so mixed block types give a sparse table. Written UTF-8 with a
        BOM so Excel reads CJK tags correctly.
    """
    data = data or {}
    backend = await get_backend()

    if operation == "list":
        result = await backend.block_list()
    elif operation == "insert":
        result = await backend.block_insert(
            data["name"], data["x"], data["y"],
            data.get("scale", 1.0), data.get("rotation", 0.0), data.get("block_id"),
        )
    elif operation == "insert_with_attributes":
        result = await backend.block_insert_with_attributes(
            data["name"], data["x"], data["y"],
            data.get("scale", 1.0), data.get("rotation", 0.0), data.get("attributes"),
        )
    elif operation == "get_attributes":
        result = await backend.block_get_attributes(data["entity_id"])
    elif operation == "update_attribute":
        result = await backend.block_update_attribute(data["entity_id"], data["tag"], data["value"])
    elif operation == "define":
        result = await backend.block_define(data["name"], data.get("entities", []))
    elif operation == "extract_attributes_csv":
        result = await backend.block_extract_attributes(
            data.get("block"), data.get("layer"), data.get("window"),
            data.get("mode", "crossing"), int(data.get("limit", 0)),
        )
        path = data.get("path")
        if result.ok and path:
            try:
                result.payload["csv"] = _write_attributes_csv(
                    path, result.payload.get("blocks", []), bool(data.get("overwrite", False))
                )
            except Exception as e:
                result = CommandResult(ok=False, error=f"CSV write failed: {e}")
    else:
        return _json({"error": f"Unknown block operation: {operation}"})

    return await add_screenshot_if_available(result, include_screenshot)


def _write_attributes_csv(path: str, blocks: list, overwrite: bool = False) -> dict:
    """Write extracted block attributes to CSV. Returns {path, rows, columns}.

    Columns are the fixed fields plus the union of every tag seen, so mixed
    block types produce a sparse table rather than losing columns.
    """
    import csv
    from pathlib import Path

    p = Path(path)
    if p.exists() and not overwrite:
        raise FileExistsError(f"{p} exists — pass overwrite: true to replace it")
    p.parent.mkdir(parents=True, exist_ok=True)

    tags = sorted({t for b in blocks for t in (b.get("attributes") or {})})
    columns = ["block", "handle", "layer", "x", "y"] + tags

    # utf-8-sig: Excel misreads plain UTF-8 CSV with CJK tags.
    with p.open("w", newline="", encoding="utf-8-sig") as f:
        w = csv.writer(f)
        w.writerow(columns)
        for b in blocks:
            pt = b.get("point") or ["", ""]
            attrs = b.get("attributes") or {}
            w.writerow(
                [b.get("block", ""), b.get("handle", ""), b.get("layer", ""), pt[0], pt[1]]
                + [attrs.get(t, "") for t in tags]
            )
    return {"path": str(p), "rows": len(blocks), "columns": columns}


# ==========================================================================
# 5. annotation — Text, dimensions, leaders
# ==========================================================================


@mcp.tool(annotations={"title": "AutoCAD Annotation Operations", "readOnlyHint": False})
@_safe("annotation")
async def annotation(
    operation: str,
    data: dict | None = None,
    include_screenshot: bool = False,
) -> ToolResult:
    """Annotation: text, dimensions, and leaders.

    Operations:
      create_text             — data: {x, y, text, height?, rotation?, layer?}
      create_dimension_linear — data: {x1, y1, x2, y2, dim_x, dim_y}
      create_dimension_aligned — data: {x1, y1, x2, y2, offset}
      create_dimension_angular — data: {cx, cy, x1, y1, x2, y2}
      create_dimension_radius — data: {cx, cy, radius, angle}
      create_leader           — data: {points: [[x,y],...], text}
      find_replace            — Substring replace across drawing text.
        data: {find, replace?, layer?, window?, mode?, limit?,
               ignore_case?, include_attribs?, dry_run?}
          find    — a literal substring, not a wildcard or regex.
          dry_run — preview before/after without writing. Do this first.
        → {dry_run, count, returned, truncated,
           entities: [{handle, type, layer, before, after}]}
        Covers TEXT/MTEXT/ATTDEF and ATTRIB text inside blocks. before/after
        are the RAW stored text: for MTEXT that includes formatting codes,
        because the replace runs over the raw string to keep formatting
        intact. The flip side is that a find string which also occurs in a
        format code (a font name, say) would corrupt it — dry_run shows
        exactly what would change, so check it there.
    """
    data = data or {}
    backend = await get_backend()

    if operation == "create_text":
        result = await backend.create_text(
            data["x"], data["y"], data["text"],
            data.get("height", 2.5), data.get("rotation", 0.0), data.get("layer"),
        )
    elif operation == "create_dimension_linear":
        result = await backend.create_dimension_linear(
            data["x1"], data["y1"], data["x2"], data["y2"], data["dim_x"], data["dim_y"],
        )
    elif operation == "create_dimension_aligned":
        result = await backend.create_dimension_aligned(
            data["x1"], data["y1"], data["x2"], data["y2"], data["offset"],
        )
    elif operation == "create_dimension_angular":
        result = await backend.create_dimension_angular(
            data["cx"], data["cy"], data["x1"], data["y1"], data["x2"], data["y2"],
        )
    elif operation == "create_dimension_radius":
        result = await backend.create_dimension_radius(
            data["cx"], data["cy"], data["radius"], data["angle"],
        )
    elif operation == "create_leader":
        result = await backend.create_leader(data["points"], data["text"])
    elif operation == "find_replace":
        result = await backend.annotation_find_replace(
            data["find"], data.get("replace", ""), data.get("layer"),
            data.get("window"), data.get("mode", "crossing"),
            int(data.get("limit", DEFAULT_READ_LIMIT)),
            bool(data.get("ignore_case", True)),
            bool(data.get("include_attribs", True)),
            bool(data.get("dry_run", False)),
        )
    else:
        return _json({"error": f"Unknown annotation operation: {operation}"})

    return await add_screenshot_if_available(result, include_screenshot)


# ==========================================================================
# 6. pid — P&ID operations (CTO library)
# ==========================================================================


@mcp.tool(annotations={"title": "P&ID Operations (CTO Library)", "readOnlyHint": False})
@_safe("pid")
async def pid(
    operation: str,
    data: dict | None = None,
    include_screenshot: bool = False,
) -> ToolResult:
    """P&ID drawing with CTO symbol library.

    Operations:
      setup_layers     — Create standard P&ID layers.
      insert_symbol    — data: {category, symbol, x, y, scale?, rotation?}
      list_symbols     — data: {category}
      draw_process_line — data: {x1, y1, x2, y2}
      connect_equipment — data: {x1, y1, x2, y2}
      add_flow_arrow   — data: {x, y, rotation?}
      add_equipment_tag — data: {x, y, tag, description?}
      add_line_number  — data: {x, y, line_num, spec}
      insert_valve     — data: {x, y, valve_type, rotation?, attributes?}
      insert_instrument — data: {x, y, instrument_type, rotation?, tag_id?, range_value?}
      insert_pump      — data: {x, y, pump_type, rotation?, attributes?}
      insert_tank      — data: {x, y, tank_type, scale?, attributes?}
    """
    data = data or {}
    backend = await get_backend()

    if operation == "setup_layers":
        result = await backend.pid_setup_layers()
    elif operation == "insert_symbol":
        result = await backend.pid_insert_symbol(
            data["category"], data["symbol"], data["x"], data["y"],
            data.get("scale", 1.0), data.get("rotation", 0.0),
        )
    elif operation == "list_symbols":
        result = await backend.pid_list_symbols(data["category"])
    elif operation == "draw_process_line":
        result = await backend.pid_draw_process_line(data["x1"], data["y1"], data["x2"], data["y2"])
    elif operation == "connect_equipment":
        result = await backend.pid_connect_equipment(data["x1"], data["y1"], data["x2"], data["y2"])
    elif operation == "add_flow_arrow":
        result = await backend.pid_add_flow_arrow(data["x"], data["y"], data.get("rotation", 0.0))
    elif operation == "add_equipment_tag":
        result = await backend.pid_add_equipment_tag(data["x"], data["y"], data["tag"], data.get("description", ""))
    elif operation == "add_line_number":
        result = await backend.pid_add_line_number(data["x"], data["y"], data["line_num"], data["spec"])
    elif operation == "insert_valve":
        result = await backend.pid_insert_valve(
            data["x"], data["y"], data["valve_type"],
            data.get("rotation", 0.0), data.get("attributes"),
        )
    elif operation == "insert_instrument":
        result = await backend.pid_insert_instrument(
            data["x"], data["y"], data["instrument_type"],
            data.get("rotation", 0.0), data.get("tag_id", ""), data.get("range_value", ""),
        )
    elif operation == "insert_pump":
        result = await backend.pid_insert_pump(
            data["x"], data["y"], data["pump_type"],
            data.get("rotation", 0.0), data.get("attributes"),
        )
    elif operation == "insert_tank":
        result = await backend.pid_insert_tank(
            data["x"], data["y"], data["tank_type"],
            data.get("scale", 1.0), data.get("attributes"),
        )
    else:
        return _json({"error": f"Unknown pid operation: {operation}"})

    return await add_screenshot_if_available(result, include_screenshot)


# ==========================================================================
# 7. view — Viewport and screenshot
# ==========================================================================


@mcp.tool(annotations={"title": "AutoCAD View Operations", "readOnlyHint": True})
@_safe("view")
async def view(
    operation: str,
    x1: float | None = None,
    y1: float | None = None,
    x2: float | None = None,
    y2: float | None = None,
) -> ToolResult:
    """Viewport control and screenshot capture.

    Operations:
      zoom_extents   — Zoom to show all entities.
      zoom_window    — Zoom to window: x1, y1, x2, y2
      get_screenshot — Capture current view as PNG image.
    """
    backend = await get_backend()

    if operation == "zoom_extents":
        result = await backend.zoom_extents()
        return _json(result.to_dict())
    elif operation == "zoom_window":
        result = await backend.zoom_window(x1, y1, x2, y2)
        return _json(result.to_dict())
    elif operation == "get_screenshot":
        result = await backend.get_screenshot()
        if result.ok and result.payload:
            from mcp.types import ImageContent, TextContent

            return [
                TextContent(type="text", text=_json({"ok": True, "screenshot": "attached"})),
                ImageContent(type="image", data=result.payload, mimeType="image/png"),
            ]
        return _json(result.to_dict())
    else:
        return _json({"error": f"Unknown view operation: {operation}"})


# ==========================================================================
# 8. system — Server management
# ==========================================================================


@mcp.tool(annotations={"title": "AutoCAD MCP System", "readOnlyHint": True})
@_safe("system")
async def system(
    operation: str,
    data: dict | None = None,
    include_screenshot: bool = False,
) -> ToolResult:
    """Server status and management.

    Operations:
      status        — Backend info, capabilities, health check.
      health        — Quick health check (ping backend).
      get_backend   — Return current backend name and capabilities.
      runtime       — Return process/runtime details for spawn diagnostics.
      init          — Re-initialize the backend.
      execute_lisp  — Execute arbitrary AutoLISP code (File IPC only). data: {code}
    """
    data = data or {}

    if operation == "status" or operation == "get_backend":
        backend = await get_backend()
        result = await backend.status()
        return await add_screenshot_if_available(result, include_screenshot)
    elif operation == "health":
        try:
            backend = await get_backend()
            result = await backend.status()
            return _json({"ok": result.ok, "backend": backend.name})
        except Exception as e:
            return _json({"ok": False, "error": str(e)})
    elif operation == "runtime":
        import os
        import sys

        return _json(
            {
                "ok": True,
                "platform": sys.platform,
                "python": sys.executable,
                "cwd": os.getcwd(),
                "backend_env": os.environ.get("AUTOCAD_MCP_BACKEND", "auto"),
                "wsl_interop": bool(os.environ.get("WSL_INTEROP")),
            }
        )
    elif operation == "init":
        # Force re-initialization
        from autocad_mcp import client
        client._backend = None
        backend = await get_backend()
        result = await backend.status()
        return _json(result.to_dict())
    elif operation == "execute_lisp":
        backend = await get_backend()
        if not data.get("code"):
            return _json({"error": "data.code is required"})
        result = await backend.execute_lisp(data["code"])
        return await add_screenshot_if_available(result, include_screenshot)
    else:
        return _json({"error": f"Unknown system operation: {operation}"})


# ==========================================================================
# Main entry point
# ==========================================================================


def main():
    """Run the MCP server on stdio transport."""
    import logging
    import sys

    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
        handlers=[logging.StreamHandler(sys.stderr)],
    )

    structlog.configure(
        wrapper_class=structlog.make_filtering_bound_logger(logging.INFO),
        logger_factory=structlog.PrintLoggerFactory(file=sys.stderr),
        cache_logger_on_first_use=True,
        processors=[
            structlog.processors.add_log_level,
            structlog.processors.TimeStamper(fmt="iso"),
            structlog.dev.ConsoleRenderer(),
        ],
    )

    log.info("autocad_mcp_starting", version="3.1.0")
    mcp.run(transport="stdio")
