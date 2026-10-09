"""Relational (Power BI-style) diagram of the Aurora Market database -> SVG."""
W, H = 1820, 960
BOX_W, HEAD, ROW, PAD = 270, 46, 23, 10

# colours (print-friendly, light background)
INK, INK2, MUTED = "#1f2328", "#52514e", "#8a8984"
LINE = "#2a78d6"
HUB_HEAD, DIM_HEAD = "#1d4f91", "#5b6b7f"
PK_BG, FK_BG = "#fdf3d8", "#e8f1fc"
BORDER = "#c9ccd1"

tables = {
    # name: (x, y, rows, hub?, [(col, type, role)])  role: PK / FK / PK,FK / ""
    "customers": (80, 60, "82,570", False, [
        ("customer_record_id", "text", "PK"), ("customer_id", "text", "ID"),
        ("postal_area", "text", ""), ("city", "text", ""), ("region", "text", "")]),
    "payments": (80, 345, "86,345", False, [
        ("order_id", "text", "PK,FK"), ("payment_sequence", "int", "PK"),
        ("payment_method", "text", ""), ("installments", "int", ""), ("payment_amount", "num", "")]),
    "customer_feedback": (80, 630, "82,380", False, [
        ("feedback_id", "text", "PK"), ("order_id", "text", "PK,FK"), ("rating", "int", ""),
        ("has_title", "bool", ""), ("has_comment", "bool", ""), ("comment_length", "int", ""),
        ("feedback_created_timestamp", "datetime", ""), ("feedback_answered_timestamp", "datetime", "")]),
    "orders": (590, 300, "82,570", True, [
        ("order_id", "text", "PK"), ("customer_record_id", "text", "FK"), ("order_status", "text", ""),
        ("purchase_timestamp", "datetime", ""), ("approval_timestamp", "datetime", ""),
        ("carrier_timestamp", "datetime", ""), ("delivery_timestamp", "datetime", ""),
        ("estimated_delivery_timestamp", "datetime", "")]),
    "order_items": (1070, 300, "93,519", True, [
        ("order_id", "text", "PK,FK"), ("item_number", "int", "PK"), ("product_id", "text", "FK"),
        ("seller_id", "text", "FK"), ("shipping_deadline", "datetime", ""),
        ("item_price", "num", ""), ("freight_cost", "num", "")]),
    "products": (1500, 40, "29,389", False, [
        ("product_id", "text", "PK"), ("product_category", "text", ""), ("name_length", "int", ""),
        ("description_length", "int", ""), ("photo_count", "int", ""), ("weight_g", "num", ""),
        ("length_cm", "num", ""), ("height_cm", "num", ""), ("width_cm", "num", "")]),
    "sellers": (1500, 640, "2,983", False, [
        ("seller_id", "text", "PK"), ("postal_area", "text", ""), ("city", "text", ""), ("region", "text", "")]),
}

def box_h(t): return HEAD + len(tables[t][4]) * ROW + PAD
def row_y(t, col):
    x, y, _, _, cols = tables[t]
    i = [c[0] for c in cols].index(col)
    return y + HEAD + 5 + i * ROW + ROW / 2

def esc(s): return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")

out = [f'<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}" viewBox="0 0 {W} {H}" '
       f'font-family="Segoe UI, Helvetica, Arial, sans-serif">',
       f'<rect width="{W}" height="{H}" fill="#ffffff"/>']

# ---- relationships: (from_table, from_col, from_card, to_table, to_col, to_card, label_dy)
rels = [
    ("customers", "customer_record_id", "1", "orders", "customer_record_id", "1", -16),
    ("payments", "order_id", "*", "orders", "order_id", "1", 0),
    ("customer_feedback", "order_id", "*", "orders", "order_id", "1", 0),
    ("orders", "order_id", "1", "order_items", "order_id", "*", -34),
    ("order_items", "product_id", "*", "products", "product_id", "1", 0),
    ("order_items", "seller_id", "*", "sellers", "seller_id", "1", 0),
]
lines, labels, drawn = [], [], set()
for ft, fc, fcard, tt, tc, tcard, ldy in rels:
    fx = tables[ft][0] + BOX_W; fy = row_y(ft, fc)
    tx = tables[tt][0];          ty = row_y(tt, tc)
    dx = (tx - fx) * 0.5
    lines.append(f'<path d="M{fx},{fy} C{fx+dx},{fy} {tx-dx},{ty} {tx},{ty}" fill="none" '
                 f'stroke="{LINE}" stroke-width="2.2"/>')
    for (px, py, card, side) in [(fx, fy, fcard, 1), (tx, ty, tcard, -1)]:
        lines.append(f'<circle cx="{px}" cy="{py}" r="4" fill="{LINE}"/>')
        key = (round(px), round(py))
        if key in drawn: continue
        drawn.add(key)
        below = (ft == "customers" and side == -1)
        lines.append(f'<text x="{px + side*12}" y="{py + (19 if below else -7)}" font-size="15" font-weight="700" fill="{LINE}" '
                     f'text-anchor="{"start" if side == 1 else "end"}">{card}</text>')
    # label pill at curve midpoint
    mx, my = (fx + tx) / 2, (fy + ty) / 2 + ldy
    txt = f"{fc}  ·  {fcard} : {tcard}" if ft != "orders" or True else fc
    txt = f"{fc}   {('1 : 1' if fcard == tcard == '1' else '1 : many')}"
    wpx = len(txt) * 7.6 + 22
    labels.append(f'<rect x="{mx - wpx/2}" y="{my - 13}" width="{wpx}" height="26" rx="13" '
                  f'fill="#ffffff" stroke="{LINE}" stroke-width="1.4"/>')
    labels.append(f'<text x="{mx}" y="{my + 5}" font-size="13" text-anchor="middle" fill="{INK}">'
                  f'<tspan font-weight="700">{esc(fc)}</tspan><tspan fill="{INK2}">   '
                  f'{"1 : 1" if fcard == tcard == "1" else "1 : many"}</tspan></text>')
out += lines

# ---- table boxes
for name, (x, y, nrows, hub, cols) in tables.items():
    h = box_h(name)
    head = HUB_HEAD if hub else DIM_HEAD
    out.append(f'<rect x="{x+3}" y="{y+4}" width="{BOX_W}" height="{h}" rx="8" fill="#000" opacity="0.06"/>')
    out.append(f'<rect x="{x}" y="{y}" width="{BOX_W}" height="{h}" rx="8" fill="#ffffff" stroke="{BORDER}" stroke-width="1.2"/>')
    out.append(f'<path d="M{x},{y+8} a8,8 0 0 1 8,-8 h{BOX_W-16} a8,8 0 0 1 8,8 v{HEAD-8} h-{BOX_W} z" fill="{head}"/>')
    out.append(f'<text x="{x+14}" y="{y+21}" font-size="16" font-weight="700" fill="#ffffff">{name}</text>')
    out.append(f'<text x="{x+14}" y="{y+38}" font-size="11.5" fill="#dfe6ef">{nrows} rows</text>')
    for i, (c, typ, role) in enumerate(cols):
        ry = y + HEAD + 5 + i * ROW
        if role:
            bg = PK_BG if "PK" in role else (FK_BG if "FK" in role else "#f3f4f6")
            out.append(f'<rect x="{x+1}" y="{ry}" width="{BOX_W-2}" height="{ROW}" fill="{bg}"/>')
        badge = {"PK": "PK", "FK": "FK", "PK,FK": "PK FK", "ID": "ID"}.get(role, "")
        weight = "700" if role else "400"
        out.append(f'<text x="{x+14}" y="{ry+16}" font-size="13" font-weight="{weight}" fill="{INK}">{esc(c)}</text>')
        if badge:
            out.append(f'<text x="{x+BOX_W-12}" y="{ry+16}" font-size="10.5" font-weight="700" '
                       f'text-anchor="end" fill="#8a6d1d">{badge}</text>')
        else:
            out.append(f'<text x="{x+BOX_W-12}" y="{ry+16}" font-size="11" text-anchor="end" fill="{MUTED}">{typ}</text>')
out += labels

# ---- title + legend
out.append(f'<text x="80" y="{H-38}" font-size="13" fill="{INK2}">'
           f'<tspan font-weight="700" fill="{INK}">Key:</tspan>  PK = primary key (unique row identifier) · '
           f'FK = foreign key (links to another table) · ID = non-unique person identifier · '
           f'lines join the exact columns used to relate tables; "1 : many" = one row on the 1-side matches many on the other.</text>')
out.append(f'<text x="80" y="{H-16}" font-size="13" fill="{INK2}">'
           f'Hub tables (dark headers): orders and order_items. '
           f'Note: feedback_id can also span several orders, and order_items has 60 exact duplicate rows, so its intended key is not yet unique.</text>')
out.append("</svg>")
open("/home/claude/fsar/report_assets/aurora_schema.svg", "w").write("\n".join(out))
print("ok")
