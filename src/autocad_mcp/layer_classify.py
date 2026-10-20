"""Layer role classification with TypeSafe Jev (System One, choice questions).

Only layer names and entity counts are sent — never drawing text. The API key
is read from TYPESAFE_API_KEY (process env, then HKCU\\Environment, because
Claude Desktop does not pass user environment variables to MCP servers) and
never appears in a result, error or log line.
"""

from __future__ import annotations

import asyncio
import json
import os
import sys
import time
import urllib.error
import urllib.request
from pathlib import Path

ENDPOINT = "https://api.typesafe.ai/v1/systemone"
MODEL = "jev-latest"
USD_PER_INPUT_TOKEN = 0.042 / 1_000_000  # output tokens are free

AUTO_MIN = 0.8
CONFIRM_MIN = 0.5

# A request must fit 64k tokens (state + all questions). Each question carries
# the full criteria, so chunk and send the chunks concurrently.
QUESTIONS_PER_REQUEST = 60
CONCURRENCY = 4

ROLES: dict[str, tuple[str, str]] = {
    "column":    ("柱", "柱的輪廓線：RC 柱、鋼柱斷面的框線（不含柱的填充圖案）"),
    "beam":      ("梁", "梁線：平面上的梁邊線、虛線梁，以及梁編號"),
    "wall":      ("牆", "牆線：RC 牆、磚牆、輕隔間的雙線或牆體輪廓"),
    "grid":      ("軸線", "結構軸線：貫穿柱心的長直線，常用中心線線型"),
    "grid_tag":  ("軸號", "軸線端部的圓圈與軸號（A、B、1、2…）"),
    "opening":   ("門窗", "門窗：門扇、開門弧、窗框、窗扇、門窗開口線、門窗編號符號"),
    "frame":     ("圖框", "圖紙外框、標題欄格線"),
    "title":     ("標題/圖名", "圖名、圖號、樓層名稱、比例等圖紙標題文字"),
    "dimension": ("尺寸標註", "尺寸標註線與標註文字"),
    "text":      ("文字", "一般文字：房間名稱、說明、註記"),
    "hatch":     ("填充", "填充圖案：材質、鋪面、剖面填充"),
    "other":     ("其他", "以上皆非：家具、設備、衛浴、機電、景觀植栽、樓梯、電梯、"
                          "面積計算線、管道間、不出圖的輔助線等"),
}

_TEXT_TYPES = {"TEXT", "MTEXT"}


def normalize(name: str) -> str:
    return " ".join(name.split()).upper()


def cache_path() -> Path:
    override = os.environ.get("AUTOCAD_MCP_LAYER_CACHE")
    if override:
        return Path(override)
    base = os.environ.get("LOCALAPPDATA") or str(Path.home())
    return Path(base) / "autocad-mcp" / "layer_roles.json"


def _load_cache(path: Path) -> dict:
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return {}


def _save_cache(path: Path, cache: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_suffix(".tmp")
    tmp.write_text(json.dumps(cache, ensure_ascii=False, indent=1), encoding="utf-8")
    os.replace(tmp, path)


def _api_key() -> str | None:
    key = os.environ.get("TYPESAFE_API_KEY")
    if key:
        return key
    if sys.platform == "win32":
        try:
            import winreg
            with winreg.OpenKey(winreg.HKEY_CURRENT_USER, "Environment") as k:
                return winreg.QueryValueEx(k, "TYPESAFE_API_KEY")[0] or None
        except OSError:
            return None
    return None


def tier(confidence: float) -> str:
    if confidence >= AUTO_MIN:
        return "auto"
    if confidence >= CONFIRM_MIN:
        return "needs_confirm"
    return "unknown"


def _rule(name: str, stats: dict[str, int] | None) -> str | None:
    """Cases a rule decides outright — Jev is not asked about these."""
    if normalize(name) == "DEFPOINTS":
        return "other"
    if stats:
        types = set(stats)
        if types == {"DIMENSION"}:
            return "dimension"
        if types == {"HATCH"}:
            return "hatch"
    return None


def _describe(stats: dict[str, int] | None) -> str:
    if stats is None:
        return "（沒有物件統計）"
    if not stats:
        return "（此圖層沒有物件）"
    parts = sorted(stats.items(), key=lambda kv: -kv[1])
    return "物件統計：" + "、".join(f"{t} {n}" for t, n in parts) + f"（共 {sum(stats.values())} 個）"


def _state(all_names: list[str]) -> str:
    return ("這是一張台灣建築圖（AutoCAD DWG）的圖層清單，要把每個圖層歸類成它所畫的建築元素。"
            "圖層名稱常混用中英文縮寫與事務所自訂代號，例如 COL=柱、BEAM=梁、WALL=牆、"
            "AXIS/GRID=軸線、OPEN/DOOR/WIN=門窗、DIM=標註、HAT/HATCH=填充、-invisible/不出圖=輔助線；"
            "前綴 L- 多為景觀、S- 多為結構、A- 多為建築。\n"
            "全部圖層：" + "、".join(all_names))


def _question(name: str, stats: dict[str, int] | None) -> dict:
    return {
        "type": "choice",
        "instructions": f"圖層「{name}」畫的是哪一類建築元素？{_describe(stats)}",
        "criteria": {k: f"{zh}：{desc}" for k, (zh, desc) in ROLES.items()},
    }


def _post(key: str, body: dict) -> tuple[int, dict]:
    req = urllib.request.Request(
        ENDPOINT,
        data=json.dumps(body, ensure_ascii=False).encode("utf-8"),
        headers={"Authorization": f"Bearer {key}", "Content-Type": "application/json"},
        method="POST",
    )
    try:
        with urllib.request.urlopen(req, timeout=60) as r:
            return r.status, json.loads(r.read().decode("utf-8"))
    except urllib.error.HTTPError as e:
        try:
            detail = json.loads(e.read().decode("utf-8"))
        except ValueError:
            detail = {}
        return e.code, detail
    except (urllib.error.URLError, TimeoutError) as e:
        return 0, {"error": str(getattr(e, "reason", e))}


async def _ask_chunk(key: str, state: str, chunk: list[tuple[str, dict | None]], sem) -> tuple[dict, dict]:
    questions = {f"L{i}": _question(name, stats) for i, (name, stats) in enumerate(chunk)}
    body = {"model": MODEL, "state": state, "questions": questions}
    async with sem:
        for attempt in range(1, 5):
            status, data = await asyncio.to_thread(_post, key, body)
            if status == 200:
                answers = data.get("answers", {})
                return ({chunk[int(qid[1:])][0]: a for qid, a in answers.items()},
                        data.get("usage", {}))
            if status in (401, 403):
                raise RuntimeError(f"TypeSafe 拒絕金鑰（HTTP {status}），請確認 TYPESAFE_API_KEY 是否有效。")
            if status not in (0, 429, 529) and status < 500:
                raise RuntimeError(f"TypeSafe 回 HTTP {status}：{json.dumps(data, ensure_ascii=False)[:300]}")
            if attempt == 4:
                raise RuntimeError(f"TypeSafe 呼叫失敗（HTTP {status}），已重試 4 次。")
            await asyncio.sleep(2 * attempt)
    raise AssertionError("unreachable")


async def classify(layers: list[tuple[str, dict[str, int] | None]], refresh: bool = False) -> dict:
    """layers: [(name, {entity_type: count} | None)] → per-layer role, confidence, tier, source."""
    t0 = time.monotonic()
    path = cache_path()
    cache = _load_cache(path)
    rows: dict[str, dict] = {}
    to_ask: list[tuple[str, dict | None]] = []

    for name, stats in layers:
        role = _rule(name, stats)
        if role:
            rows[name] = {"role": role, "confidence": 1.0, "source": "rule"}
            continue
        hit = None if refresh else cache.get(normalize(name))
        if hit:
            rows[name] = {**hit, "source": "cache"}
        else:
            to_ask.append((name, stats))

    usage = {"requests": 0, "input_tokens": 0}
    if to_ask:
        key = _api_key()
        if not key:
            raise RuntimeError("找不到 TYPESAFE_API_KEY（已找過行程環境變數與 HKCU\\Environment）。"
                               "用 setx 設定後完全關閉再重開 Claude。")
        state = _state([n for n, _ in layers])
        sem = asyncio.Semaphore(CONCURRENCY)
        chunks = [to_ask[i:i + QUESTIONS_PER_REQUEST] for i in range(0, len(to_ask), QUESTIONS_PER_REQUEST)]
        results = await asyncio.gather(*(_ask_chunk(key, state, c, sem) for c in chunks))
        for answers, u in results:
            usage["requests"] += 1
            usage["input_tokens"] += u.get("input_tokens", 0)
            for name, a in answers.items():
                probs = a.get("probabilities") or {}
                ranked = sorted(probs.items(), key=lambda kv: -kv[1])
                row = {
                    "role": a.get("choice"),
                    "confidence": round(float(a.get("confidence") or 0.0), 3),
                    "alternatives": [{"role": r, "p": round(p, 3)} for r, p in ranked[1:3]],
                }
                cache[normalize(name)] = {**row, "model": MODEL}
                rows[name] = {**row, "source": "jev"}
        _save_cache(path, cache)

    out, by_role, counts = [], {}, {"auto": 0, "needs_confirm": 0, "unknown": 0}
    for name, _ in layers:
        r = rows[name]
        t = tier(r["confidence"])
        counts[t] += 1
        item = {"layer": name, "role": r["role"], "role_zh": ROLES.get(r["role"], ("?",))[0],
                "confidence": r["confidence"], "tier": t, "source": r["source"]}
        if t != "auto" and r.get("alternatives"):
            item["alternatives"] = r["alternatives"]
        out.append(item)
        if t == "auto":
            by_role.setdefault(r["role"], []).append(name)

    usage["cost_usd"] = round(usage["input_tokens"] * USD_PER_INPUT_TOKEN, 6)
    usage["elapsed_ms"] = int((time.monotonic() - t0) * 1000)
    return {
        "roles": {k: zh for k, (zh, _) in ROLES.items()},
        "thresholds": {"auto": AUTO_MIN, "needs_confirm": CONFIRM_MIN},
        "counts": counts,
        "by_role": by_role,
        "layers": out,
        "usage": usage,
        "cache_path": str(path),
    }
