import pandas as pd
import numpy as np
import geopandas as gpd
import json

# descriptive stats with dataset not coef complete 
import pandas as pd 
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import numpy as np

df = pd.read_parquet ("/home/onyxia/work/Policy in Action/Data/PiA_panel_data_NA.parquet")
#df.info()
df20 = df[df['year'] == 2020]
df23 = df[df['year'] == 2023]

COLORS = {2020: "steelblue", 2023: "tomato"}

# ── Data ──────────────────────────────────────────────────────────────────────
df = pd.read_parquet("/home/onyxia/work/Policy in Action/Data/PiA_panel_data_NA.parquet")

# move REGL to df20, remove from df23
df20 = df[df['year'] == 2020][["farm_id", "BENEF_PAC", "SIEGE_LIB_DEP"]].copy()
df23 = df[df['year'] == 2023][["farm_id", "BENEF_PAC", "EXIT","REGL",
                                "AGE", "SAU_TOT", "PBSTOT_COEF17", "UGBTA.TOT",
                                "CERTIFICATION_BINARY", "BIO_FIL", "EXTERNALISFIL",
                                "STATUT", "OTEFDA_COEF17", "RDEV", "LABOUR_TYPE"]].copy()

merged = df20.merge(df23, on="farm_id", suffixes=("_20", "_23"))

# ── Group assignment ──────────────────────────────────────────────────────────
def assign_group(row):
    if row["EXIT"] == 1: return "exited"
    b20, b23 = row["BENEF_PAC_20"], row["BENEF_PAC_23"]
    if pd.isna(b20) or pd.isna(b23): return "other"
    if b20==1 and b23==1: return "always_cap"
    if b20==1 and b23==0: return "lost_cap"
    if b20==0 and b23==1: return "gained_cap"
    if b20==0 and b23==0: return "never_cap"
    return "other"

merged["group"] = merged.apply(assign_group, axis=1)


import json
import numpy as np

# Build enhanced dept stats with full profiles
def dept_stats_full(grp):
    total  = len(grp)
    lost   = grp[grp["group"] == "lost_cap"]
    always = grp[grp["group"] == "always_cap"]
    exited = grp[grp["group"] == "exited"]
    never  = grp[grp["group"] == "never_cap"]

    def sm(s):   return round(float(s.mean()), 1)    if s.notna().any() else None
    def pct(s):  return round(float(s.mean()*100),1) if s.notna().any() else None
    def mode(s): return s.mode()[0] if s.notna().any() and len(s.mode()) > 0 else None

    # "stayed" = always_cap + never_cap + gained_cap (anyone who didn't exit)
    stayed = grp[grp["group"].isin(["always_cap", "never_cap", "gained_cap", "lost_cap"])]

    return pd.Series({
        "total_farms"    : total,
        "pct_always_cap" : round(len(always)/total*100, 1),
        "pct_lost"       : round(len(lost)/total*100, 1),
        "pct_gained"     : round((grp["group"]=="gained_cap").sum()/total*100, 1),
        "pct_never"      : round(len(never)/total*100, 1),
        "pct_exited"     : round(len(exited)/total*100, 1),
        "n_lost"         : len(lost),
        "n_exited"       : len(exited),

        # ── Numerical: lost vs kept ───────────────────────────────
        "lost_age"       : sm(lost["AGE"]),
        "kept_age"       : sm(always["AGE"]),
        "lost_sau"       : sm(lost["SAU_TOT"]),
        "kept_sau"       : sm(always["SAU_TOT"]),
        "lost_output"    : sm(lost["PBSTOT_COEF17"]),
        "kept_output"    : sm(always["PBSTOT_COEF17"]),
        "lost_ugb"       : sm(lost["UGBTA.TOT"]),
        "kept_ugb"       : sm(always["UGBTA.TOT"]),

        # ── Binary: lost vs kept ──────────────────────────────────
        "lost_regl"      : pct(lost["REGL"]),
        "kept_regl"      : pct(always["REGL"]),
        "lost_cert"      : pct(lost["CERTIFICATION_BINARY"]),
        "kept_cert"      : pct(always["CERTIFICATION_BINARY"]),
        "lost_bio"       : pct(lost["BIO_FIL"]),
        "kept_bio"       : pct(always["BIO_FIL"]),
        "lost_ext"       : pct(lost["EXTERNALISFIL"]),
        "kept_ext"       : pct(always["EXTERNALISFIL"]),
        "lost_rdev" : pct(lost["RDEV"]),
        "kept_rdev" : pct(always["RDEV"]),
        "exit_rdev" : pct(exited["RDEV"]),
        "stay_rdev" : pct(stayed["RDEV"]),
        

        # ── Categorical: lost vs kept ─────────────────────────────
        "lost_statut"    : mode(lost["STATUT"]),
        "kept_statut"    : mode(always["STATUT"]),
        "lost_otef"      : mode(lost["OTEFDA_COEF17"]),
        "kept_otef"      : mode(always["OTEFDA_COEF17"]),
        "lost_labour"    : mode(lost["LABOUR_TYPE"]),
        "kept_labour"    : mode(always["LABOUR_TYPE"]),

        # ── Numerical: exited vs stayed ───────────────────────────
        "exit_age"       : sm(exited["AGE"]),
        "stay_age"       : sm(stayed["AGE"]),
        "exit_sau"       : sm(exited["SAU_TOT"]),
        "stay_sau"       : sm(stayed["SAU_TOT"]),
        "exit_output"    : sm(exited["PBSTOT_COEF17"]),
        "stay_output"    : sm(stayed["PBSTOT_COEF17"]),
        "exit_ugb"       : sm(exited["UGBTA.TOT"]),
        "stay_ugb"       : sm(stayed["UGBTA.TOT"]),

        # ── Binary: exited vs stayed ──────────────────────────────
        "exit_regl"      : pct(exited["REGL"]),
        "stay_regl"      : pct(stayed["REGL"]),
        "exit_cert"      : pct(exited["CERTIFICATION_BINARY"]),
        "stay_cert"      : pct(stayed["CERTIFICATION_BINARY"]),
        "exit_bio"       : pct(exited["BIO_FIL"]),
        "stay_bio"       : pct(stayed["BIO_FIL"]),
        "exit_ext"       : pct(exited["EXTERNALISFIL"]),
        "stay_ext"       : pct(stayed["EXTERNALISFIL"]),
        "exit_rdev" : pct(exited["RDEV"]),
        "stay_rdev" : pct(stayed["RDEV"]),

        # ── Categorical: exited vs stayed ─────────────────────────
        "exit_statut"    : mode(exited["STATUT"]),
        "stay_statut"    : mode(stayed["STATUT"]),
        "exit_otef"      : mode(exited["OTEFDA_COEF17"]),
        "stay_otef"      : mode(stayed["OTEFDA_COEF17"]),
        "exit_labour"    : mode(exited["LABOUR_TYPE"]),
        "stay_labour"    : mode(stayed["LABOUR_TYPE"]),
    })

stats_full = merged.groupby("SIEGE_LIB_DEP").apply(dept_stats_full).reset_index()
stats_full["SIEGE_LIB_DEP"] = stats_full["SIEGE_LIB_DEP"].replace(dept_name_map)

# Build lookup dict for JS: {dept_name: {stats}}
dept_lookup = {}
for _, row in stats_full.iterrows():
    dept_lookup[row["SIEGE_LIB_DEP"]] = {
        k: (None if (isinstance(v, float) and np.isnan(v)) else v)
        for k, v in row.items() if k != "SIEGE_LIB_DEP"
    }

dept_lookup_json = json.dumps(dept_lookup)
dept_names_json  = json.dumps(sorted(dept_lookup.keys()))

html = f"""<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8"/>
  <title>CAP Farm Profile Explorer</title>
  <style>
    * {{ box-sizing: border-box; margin: 0; padding: 0; }}
    body {{ font-family: Georgia, serif; background: #f5f5f0; color: #2c2c2c; min-height: 100vh; padding: 30px 20px; }}
    h1 {{ text-align: center; font-size: 20px; margin-bottom: 6px; color: #1a1a1a; }}
    .subtitle {{ text-align: center; font-size: 13px; color: #888; margin-bottom: 24px; }}
    #selector-wrap {{ display: flex; justify-content: center; margin-bottom: 28px; }}
    select {{ font-size: 15px; font-family: Georgia, serif; padding: 8px 16px; border: 1px solid #ccc;
              border-radius: 6px; background: white; cursor: pointer; min-width: 260px; box-shadow: 0 1px 4px rgba(0,0,0,0.08); }}
    #dashboard {{ display: none; max-width: 900px; margin: 0 auto; }}
    #overview {{ background: white; border-radius: 10px; padding: 16px 20px; margin-bottom: 22px; box-shadow: 0 1px 6px rgba(0,0,0,0.07); }}
    #overview h2 {{ font-size: 15px; margin-bottom: 12px; color: #555; }}
    .overview-grid {{ display: flex; gap: 10px; flex-wrap: wrap; }}
    .ov-card {{ flex: 1; min-width: 120px; background: #f9f9f7; border-radius: 8px; padding: 10px 14px; text-align: center; border-top: 3px solid #ccc; }}
    .ov-val {{ font-size: 22px; font-weight: bold; }}
    .ov-lbl {{ font-size: 11px; color: #888; margin-top: 2px; }}
    .section {{ background: white; border-radius: 10px; padding: 20px 24px; margin-bottom: 20px; box-shadow: 0 1px 6px rgba(0,0,0,0.07); }}
    .section h2 {{ font-size: 16px; margin-bottom: 4px; padding-bottom: 8px; border-bottom: 2px solid #eee; }}
    .section-sub {{ font-size: 12px; color: #999; margin-bottom: 16px; }}
    table {{ width: 100%; border-collapse: collapse; font-size: 13px; }}
    th {{ text-align: left; padding: 8px 10px; background: #f5f5f0; font-weight: bold; font-size: 12px; color: #555; border-bottom: 1px solid #e0e0e0; }}
    td {{ padding: 9px 10px; border-bottom: 1px solid #f0f0ec; }}
    tr:last-child td {{ border-bottom: none; }}
    .val-a {{ font-weight: bold; }}
    .diff-pos {{ color: #c0392b; font-size: 11px; }}
    .diff-neg {{ color: #27ae60; font-size: 11px; }}
    .diff-neu {{ color: #aaa; font-size: 11px; }}
    #source {{ text-align: center; font-size: 11px; color: #aaa; margin-top: 30px; }}
  </style>
</head>
<body>

<h1>CAP Farm Profile Explorer</h1>
<p class="subtitle">Select a department to explore profiles of farms that lost CAP or exited</p>

<div id="selector-wrap">
  <select id="dept-select" onchange="updateDashboard()">
    <option value="">— Select a department —</option>
  </select>
</div>

<div id="dashboard">

  <!-- Overview -->
  <div id="overview">
    <h2>Overview — <span id="dept-title"></span></h2>
    <div class="overview-grid">
      <div class="ov-card" style="border-color:#6baed6"><div class="ov-val" id="ov-total"></div><div class="ov-lbl">Total farms</div></div>
      <div class="ov-card" style="border-color:#78c679"><div class="ov-val" id="ov-always"></div><div class="ov-lbl">Always had CAP</div></div>
      <div class="ov-card" style="border-color:#fd8d3c"><div class="ov-val" id="ov-lost"></div><div class="ov-lbl">Lost CAP</div></div>
      <div class="ov-card" style="border-color:#c994c7"><div class="ov-val" id="ov-never"></div><div class="ov-lbl">Never had CAP</div></div>
      <div class="ov-card" style="border-color:#969696"><div class="ov-val" id="ov-exited"></div><div class="ov-lbl">Exited 2023</div></div>
    </div>
  </div>

  <!-- Section 1: Lost CAP -->
  <div class="section">
    <h2 style="color:#c0392b">Farms that Lost CAP</h2>
    <p class="section-sub">Comparing farms that lost CAP (2020→2023) vs farms that kept it</p>
    <table>
      <thead>
        <tr>
          <th>Indicator</th>
          <th>Lost CAP (<span id="n-lost"></span> farms)</th>
          <th>Kept CAP</th>
          <th>Difference</th>
        </tr>
      </thead>
      <tbody>
        <tr style="background:#f9f9f7"><td colspan="4" style="font-size:11px;color:#999;padding:6px 10px;">NUMERICAL — averages</td></tr>
        <tr><td>Age (years)</td>        <td class="val-a" id="l-age"></td>    <td id="k-age"></td>    <td id="d-age"></td></tr>
        <tr><td>Farm area (ha)</td>     <td class="val-a" id="l-sau"></td>    <td id="k-sau"></td>    <td id="d-sau"></td></tr>
        <tr><td>Gross output (€)</td>   <td class="val-a" id="l-output"></td> <td id="k-output"></td> <td id="d-output"></td></tr>
        <tr><td>Livestock units</td>    <td class="val-a" id="l-ugb"></td>    <td id="k-ugb"></td>    <td id="d-ugb"></td></tr>
        <tr style="background:#f9f9f7"><td colspan="4" style="font-size:11px;color:#999;padding:6px 10px;">BINARY — % yes</td></tr>
        <tr><td>Disadvantaged location</td> <td class="val-a" id="l-regl"></td> <td id="k-regl"></td> <td id="d-regl"></td></tr>
        <tr><td>Certification</td>        <td class="val-a" id="l-cert"></td> <td id="k-cert"></td> <td id="d-cert"></td></tr>
        <tr><td>Organic farming</td>      <td class="val-a" id="l-bio"></td>  <td id="k-bio"></td>  <td id="d-bio"></td></tr>
        <tr><td>Externalisation</td>      <td class="val-a" id="l-ext"></td>  <td id="k-ext"></td>  <td id="d-ext"></td></tr>
        <tr><td>Rural development</td>    <td class="val-a" id="l-rdev"></td> <td id="k-rdev"></td> <td id="d-rdev"></td></tr>
        <tr style="background:#f9f9f7"><td colspan="4" style="font-size:11px;color:#999;padding:6px 10px;">CATEGORICAL — most common value</td></tr>
        <tr><td>Legal status</td>  <td class="val-a" id="l-statut"></td> <td id="k-statut"></td> <td>—</td></tr>
        <tr><td>Farm type</td>     <td class="val-a" id="l-otef"></td>   <td id="k-otef"></td>   <td>—</td></tr>
        <tr><td>Labour type</td>   <td class="val-a" id="l-labour"></td> <td id="k-labour"></td> <td>—</td></tr>
      </tbody>
    </table>
  </div>

  <!-- Section 2: Exited -->
  <div class="section">
    <h2 style="color:#555">Farms that Exited in 2023</h2>
    <p class="section-sub">Comparing farms that exited vs farms that stayed</p>
    <table>
      <thead>
        <tr>
          <th>Indicator</th>
          <th>Exited (<span id="n-exited"></span> farms)</th>
          <th>Stayed</th>
          <th>Difference</th>
        </tr>
      </thead>
      <tbody>
        <tr style="background:#f9f9f7"><td colspan="4" style="font-size:11px;color:#999;padding:6px 10px;">NUMERICAL — averages</td></tr>
        <tr><td>Age (years)</td>        <td class="val-a" id="e-age"></td>    <td id="s-age"></td>    <td id="de-age"></td></tr>
        <tr><td>Farm area (ha)</td>     <td class="val-a" id="e-sau"></td>    <td id="s-sau"></td>    <td id="de-sau"></td></tr>
        <tr><td>Gross output (€)</td>   <td class="val-a" id="e-output"></td> <td id="s-output"></td> <td id="de-output"></td></tr>
        <tr><td>Livestock units</td>    <td class="val-a" id="e-ugb"></td>    <td id="s-ugb"></td>    <td id="de-ugb"></td></tr>
        <tr style="background:#f9f9f7"><td colspan="4" style="font-size:11px;color:#999;padding:6px 10px;">BINARY — % yes</td></tr>
        <tr><td>Disadvantaged location</td> <td class="val-a" id="e-regl"></td> <td id="s-regl"></td> <td id="de-regl"></td></tr>
        <tr><td>Certification</td>        <td class="val-a" id="e-cert"></td> <td id="s-cert"></td> <td id="de-cert"></td></tr>
        <tr><td>Organic farming</td>      <td class="val-a" id="e-bio"></td>  <td id="s-bio"></td>  <td id="de-bio"></td></tr>
        <tr><td>Externalisation</td>      <td class="val-a" id="e-ext"></td>  <td id="s-ext"></td>  <td id="de-ext"></td></tr>
        <tr><td>Rural development</td> <td class="val-a" id="e-rdev"></td> <td id="s-rdev"></td> <td id="de-rdev"></td></tr>
        <tr style="background:#f9f9f7"><td colspan="4" style="font-size:11px;color:#999;padding:6px 10px;">CATEGORICAL — most common value</td></tr>
        <tr><td>Legal status</td>  <td class="val-a" id="e-statut"></td> <td id="s-statut"></td> <td>—</td></tr>
        <tr><td>Farm type</td>     <td class="val-a" id="e-otef"></td>   <td id="s-otef"></td>   <td>—</td></tr>
        <tr><td>Labour type</td>   <td class="val-a" id="e-labour"></td> <td id="s-labour"></td> <td>—</td></tr>
      </tbody>
    </table>
  </div>

</div>

<p id="source">Source: Complete Dataset (incl. coefficients NAs)</p>

<script>
var data  = {dept_lookup_json};
var depts = {dept_names_json};

var sel = document.getElementById('dept-select');
depts.forEach(function(d) {{
  var opt = document.createElement('option');
  opt.value = d; opt.text = d;
  sel.appendChild(opt);
}});

var statutLabels = {{
  "01": "Individual farmer", "02": "GAEC", "03": "EARL",
  "08": "Non-corporate group", "09": "Other legal entities"
}};
function fmtStatut(v) {{
  if (v === null || v === undefined) return 'N/A';
  return statutLabels[String(v).padStart(2,'0')] || v;
}}

var otefLabels = {{
  "1516": "Grandes cultures",
  "2829": "Maraîchage ou horticulture",
  "3500": "Viticulture",
  "3900": "Cultures fruitières ou autres cultures permanentes",
  "4500": "Bovines spécialisées — orientation lait",
  "4600": "Bovines spécialisées — orientation élevage et viande",
  "4700": "Bovines — lait, élevage et viande combinés",
  "4800": "Ovins et/ou caprins, et/ou autres herbivores",
  "5074": "Porcins et/ou volailles",
  "6184": "Polyculture et/ou polyélevage",
  "9000": "Exploitations non classées"
}};

function fmtOtef(v) {{
  if (v === null || v === undefined) return 'N/A';
  return otefLabels[String(v)] || v;
}}

function fmt(v, suffix) {{
  return (v === null || v === undefined) ? 'N/A' : v + (suffix||'');
}}

function diffCell(a, b, id, higherIsBad) {{
  var el = document.getElementById(id);
  if (a === null || b === null) {{ el.innerHTML = '<span class="diff-neu">—</span>'; return; }}
  var d = Math.round((a - b) * 10) / 10;
  var sign = d > 0 ? '+' : '';
  var cls = d === 0 ? 'diff-neu' : (d > 0 === higherIsBad ? 'diff-pos' : 'diff-neg');
  el.innerHTML = '<span class="' + cls + '">' + sign + d + '</span>';
}}

function updateDashboard() {{
  var dept = document.getElementById('dept-select').value;
  if (!dept) return;
  var d = data[dept];
  if (!d) return;

  document.getElementById('dashboard').style.display = 'block';
  document.getElementById('dept-title').innerText = dept;

  // Overview
  document.getElementById('ov-total').innerText  = d.total_farms;
  document.getElementById('ov-always').innerText = fmt(d.pct_always_cap, '%');
  document.getElementById('ov-lost').innerText   = fmt(d.pct_lost, '%');
  document.getElementById('ov-never').innerText  = fmt(d.pct_never, '%');
  document.getElementById('ov-exited').innerText = fmt(d.pct_exited, '%');

  // Lost CAP — numerical
  document.getElementById('n-lost').innerText    = d.n_lost || 0;
  document.getElementById('l-age').innerText     = fmt(d.lost_age,    ' yrs');
  document.getElementById('k-age').innerText     = fmt(d.kept_age,    ' yrs');
  document.getElementById('l-sau').innerText     = fmt(d.lost_sau,    ' ha');
  document.getElementById('k-sau').innerText     = fmt(d.kept_sau,    ' ha');
  document.getElementById('l-output').innerText  = fmt(d.lost_output, ' €');
  document.getElementById('k-output').innerText  = fmt(d.kept_output, ' €');
  document.getElementById('l-ugb').innerText     = fmt(d.lost_ugb,    ' UGB');
  document.getElementById('k-ugb').innerText     = fmt(d.kept_ugb,    ' UGB');
  diffCell(d.lost_age,    d.kept_age,    'd-age',    true);
  diffCell(d.lost_sau,    d.kept_sau,    'd-sau',    false);
  diffCell(d.lost_output, d.kept_output, 'd-output', false);
  diffCell(d.lost_ugb,    d.kept_ugb,    'd-ugb',    false);

  // Lost CAP — binary
  document.getElementById('l-regl').innerText   = fmt(d.lost_regl, '%');
  document.getElementById('k-regl').innerText   = fmt(d.kept_regl, '%');
  document.getElementById('l-cert').innerText   = fmt(d.lost_cert, '%');
  document.getElementById('k-cert').innerText   = fmt(d.kept_cert, '%');
  document.getElementById('l-bio').innerText    = fmt(d.lost_bio,  '%');
  document.getElementById('k-bio').innerText    = fmt(d.kept_bio,  '%');
  document.getElementById('l-ext').innerText    = fmt(d.lost_ext,  '%');
  document.getElementById('k-ext').innerText    = fmt(d.kept_ext,  '%');
  document.getElementById('l-rdev').innerText   = fmt(d.lost_rdev, '%');
  document.getElementById('k-rdev').innerText   = fmt(d.kept_rdev, '%');
  diffCell(d.lost_regl, d.kept_regl, 'd-regl', false);
  diffCell(d.lost_cert, d.kept_cert, 'd-cert', false);
  diffCell(d.lost_bio,  d.kept_bio,  'd-bio',  false);
  diffCell(d.lost_ext,  d.kept_ext,  'd-ext',  false);
  diffCell(d.lost_rdev, d.kept_rdev, 'd-rdev', false);

  // Lost CAP — categorical
  document.getElementById('l-statut').innerText = fmtStatut(d.lost_statut);
  document.getElementById('k-statut').innerText = fmtStatut(d.kept_statut);
  document.getElementById('l-otef').innerText   = fmtOtef(d.lost_otef);
  document.getElementById('k-otef').innerText   = fmtOtef(d.kept_otef);
  document.getElementById('l-labour').innerText = fmt(d.lost_labour);
  document.getElementById('k-labour').innerText = fmt(d.kept_labour);

  // Exited — numerical
  document.getElementById('n-exited').innerText  = d.n_exited || 0;
  document.getElementById('e-age').innerText     = fmt(d.exit_age,    ' yrs');
  document.getElementById('s-age').innerText     = fmt(d.stay_age,    ' yrs');
  document.getElementById('e-sau').innerText     = fmt(d.exit_sau,    ' ha');
  document.getElementById('s-sau').innerText     = fmt(d.stay_sau,    ' ha');
  document.getElementById('e-output').innerText  = fmt(d.exit_output, ' €');
  document.getElementById('s-output').innerText  = fmt(d.stay_output, ' €');
  document.getElementById('e-ugb').innerText     = fmt(d.exit_ugb,    ' UGB');
  document.getElementById('s-ugb').innerText     = fmt(d.stay_ugb,    ' UGB');
  diffCell(d.exit_age,    d.stay_age,    'de-age',    true);
  diffCell(d.exit_sau,    d.stay_sau,    'de-sau',    false);
  diffCell(d.exit_output, d.stay_output, 'de-output', false);
  diffCell(d.exit_ugb,    d.stay_ugb,    'de-ugb',    false);

  //Exited — binary
  document.getElementById('e-regl').innerText = fmt(d.exit_regl, '%');
  document.getElementById('s-regl').innerText = fmt(d.stay_regl, '%');
  document.getElementById('e-cert').innerText = fmt(d.exit_cert, '%');
  document.getElementById('s-cert').innerText = fmt(d.stay_cert, '%');
  document.getElementById('e-bio').innerText  = fmt(d.exit_bio,  '%');
  document.getElementById('s-bio').innerText  = fmt(d.stay_bio,  '%');
  document.getElementById('e-ext').innerText  = fmt(d.exit_ext,  '%');
  document.getElementById('s-ext').innerText  = fmt(d.stay_ext,  '%');
  document.getElementById('e-rdev').innerText = fmt(d.exit_rdev, '%');
  document.getElementById('s-rdev').innerText = fmt(d.stay_rdev, '%');
  diffCell(d.exit_regl, d.stay_regl, 'de-regl', false);
  diffCell(d.exit_cert, d.stay_cert, 'de-cert', false);
  diffCell(d.exit_bio,  d.stay_bio,  'de-bio',  false);
  diffCell(d.exit_ext,  d.stay_ext,  'de-ext',  false);
  diffCell(d.exit_rdev, d.stay_rdev, 'de-rdev', false);

  // Exited — categorical
  document.getElementById('e-statut').innerText = fmtStatut(d.exit_statut);
  document.getElementById('s-statut').innerText = fmtStatut(d.stay_statut);
  document.getElementById('e-otef').innerText   = fmtOtef(d.exit_otef);
  document.getElementById('s-otef').innerText   = fmtOtef(d.stay_otef);
  document.getElementById('e-labour').innerText = fmt(d.exit_labour);
  document.getElementById('s-labour').innerText = fmt(d.stay_labour);
}}
</script>
</body>
</html>"""

with open("Full_cap_profile_explorer.html", "w", encoding="utf-8") as f:
    f.write(html)
