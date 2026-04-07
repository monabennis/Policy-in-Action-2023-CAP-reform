import pandas as pd
import numpy as np
import geopandas as gpd
import json

# ── Data ──────────────────────────────────────────────────────────────────────
df = pd.read_parquet("/home/onyxia/work/Policy in Action/Data/PiA_panel_data_NA.parquet")

df20 = df[df['year'] == 2020][["farm_id", "BENEF_PAC", "SIEGE_LIB_DEP"]].copy()
df23 = df[df['year'] == 2023][["farm_id", "BENEF_PAC", "EXIT",
                                "AGE", "PBSTOT_COEF17", "SAU_TOT", "UGBTA.TOT"]].copy()

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

# ── Department-level stats ────────────────────────────────────────────────────
NUM_COLS = {"AGE": "age", "SAU_TOT": "sau", "PBSTOT_COEF17": "output", "UGBTA.TOT": "ugb"}

def dept_stats(grp):
    total = len(grp)
    lost  = grp[grp["group"] == "lost_cap"]
    kept  = grp[grp["group"] == "always_cap"]
    out = {
        "total_farms"   : total,
        "pct_always_cap": round((grp["group"]=="always_cap").sum()/total*100, 1),
        "pct_lost"      : round(len(lost)/total*100, 1),
        "pct_gained"    : round((grp["group"]=="gained_cap").sum()/total*100, 1),
        "pct_never"     : round((grp["group"]=="never_cap").sum()/total*100, 1),
        "pct_exited"    : round((grp["group"]=="exited").sum()/total*100, 1),
    }
    for col, label in NUM_COLS.items():
        out[f"{label}_lost"] = round(lost[col].mean(), 1) if lost[col].notna().any() else np.nan
        out[f"{label}_kept"] = round(kept[col].mean(), 1) if kept[col].notna().any() else np.nan
    return pd.Series(out)

stats_dep = merged.groupby("SIEGE_LIB_DEP").apply(dept_stats).reset_index()

# ── Department name mapping ───────────────────────────────────────────────────
dept_name_map = {
    'AIN':'Ain','AISNE':'Aisne','ALLIER':'Allier','ALPES DE HAUTE PROVENCE':'Alpes-de-Haute-Provence',
    'ALPES MARITIMES':'Alpes-Maritimes','ARDECHE':'Ardèche','ARDENNES':'Ardennes','ARIEGE':'Ariège',
    'AUBE':'Aube','AUDE':'Aude','AVEYRON':'Aveyron','BAS RHIN':'Bas-Rhin',
    'BOUCHES DU RHONE':'Bouches-du-Rhône','CALVADOS':'Calvados','CANTAL':'Cantal','CHARENTE':'Charente',
    'CHARENTE MARITIME':'Charente-Maritime','CHER':'Cher','CORREZE':'Corrèze','CORSE DU SUD':'Corse-du-Sud',
    'COTE D OR':"Côte-d'Or",'COTES D ARMOR':"Côtes-d'Armor",'CREUSE':'Creuse','DEUX SEVRES':'Deux-Sèvres',
    'DORDOGNE':'Dordogne','DOUBS':'Doubs','DROME':'Drôme','ESSONNE':'Essonne','EURE':'Eure',
    'EURE ET LOIR':'Eure-et-Loir','FINISTERE':'Finistère','GARD':'Gard','GERS':'Gers','GIRONDE':'Gironde',
    'HAUT RHIN':'Haut-Rhin','HAUTE CORSE':'Haute-Corse','HAUTE GARONNE':'Haute-Garonne',
    'HAUTE LOIRE':'Haute-Loire','HAUTE MARNE':'Haute-Marne','HAUTE SAONE':'Haute-Saône',
    'HAUTE SAVOIE':'Haute-Savoie','HAUTE VIENNE':'Haute-Vienne','HAUTES ALPES':'Hautes-Alpes',
    'HAUTES PYRENEES':'Hautes-Pyrénées','HAUTS DE SEINE':'Hauts-de-Seine','HERAULT':'Hérault',
    'ILLE ET VILAINE':'Ille-et-Vilaine','INDRE':'Indre','INDRE ET LOIRE':'Indre-et-Loire','ISERE':'Isère',
    'JURA':'Jura','LANDES':'Landes','LOIR ET CHER':'Loir-et-Cher','LOIRE':'Loire',
    'LOIRE ATLANTIQUE':'Loire-Atlantique','LOIRET':'Loiret','LOT':'Lot','LOT ET GARONNE':'Lot-et-Garonne',
    'LOZERE':'Lozère','MAINE ET LOIRE':'Maine-et-Loire','MANCHE':'Manche','MARNE':'Marne',
    'MAYENNE':'Mayenne','MEURTHE ET MOSELLE':'Meurthe-et-Moselle','MEUSE':'Meuse','MORBIHAN':'Morbihan',
    'MOSELLE':'Moselle','NIEVRE':'Nièvre','NORD':'Nord','OISE':'Oise','ORNE':'Orne','PARIS':'Paris',
    'PAS DE CALAIS':'Pas-de-Calais','PUY DE DOME':'Puy-de-Dôme','PYRENEES ATLANTIQUES':'Pyrénées-Atlantiques',
    'PYRENEES ORIENTALES':'Pyrénées-Orientales','RHONE':'Rhône','SAONE ET LOIRE':'Saône-et-Loire',
    'SARTHE':'Sarthe','SAVOIE':'Savoie','SEINE ET MARNE':'Seine-et-Marne','SEINE MARITIME':'Seine-Maritime',
    'SEINE SAINT DENIS':'Seine-Saint-Denis','SOMME':'Somme','TARN':'Tarn','TARN ET GARONNE':'Tarn-et-Garonne',
    'TERRITOIRE DE BELFORT':'Territoire de Belfort','VAL D OISE':"Val-d'Oise",'VAL DE MARNE':'Val-de-Marne',
    'VAR':'Var','VAUCLUSE':'Vaucluse','VENDEE':'Vendée','VIENNE':'Vienne','VOSGES':'Vosges',
    'YONNE':'Yonne','YVELINES':'Yvelines',
}
stats_dep["SIEGE_LIB_DEP"] = stats_dep["SIEGE_LIB_DEP"].replace(dept_name_map)

# ── Geo merge ─────────────────────────────────────────────────────────────────
deps_geo = gpd.read_file(
    "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements.geojson"
)
deps_map = deps_geo.merge(stats_dep, left_on="nom", right_on="SIEGE_LIB_DEP", how="left")
deps_map = deps_map[deps_map["total_farms"].notna()]
deps_map[deps_map.select_dtypes("float").columns] = deps_map.select_dtypes("float").round(1)

geojson_data = json.loads(deps_map.to_json())

# ── HTML map ──────────────────────────────────────────────────────────────────
METRICS = {
    "Always had CAP": ("pct_always_cap", ["#ffffcc","#78c679","#006837"]),
    "Lost CAP"      : ("pct_lost",       ["#fff5eb","#fd8d3c","#7f0000"]),
    "Gained CAP"    : ("pct_gained",     ["#eff3ff","#6baed6","#08306b"]),
    "Never had CAP" : ("pct_never",      ["#f7f4f9","#c994c7","#67001f"]),
    "Exited 2023"   : ("pct_exited",     ["#ffffff","#969696","#252525"]),
}

col_map_json     = json.dumps({k: v[0] for k, v in METRICS.items()})
palettes_json    = json.dumps({k: v[1] for k, v in METRICS.items()})
metric_opts_html = "".join(f'<option value="{k}">{k}</option>' for k in METRICS)

html = f"""<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8"/>
  <title>CAP Transition Map</title>
  <link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"/>
  <script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
  <style>
    body {{ margin:0; font-family:Georgia,serif; }}
    #map {{ height:100vh; }}
    #controls {{
      position:fixed; top:15px; left:50%; transform:translateX(-50%);
      z-index:9999; background:white; padding:10px 20px; border-radius:8px;
      box-shadow:0 2px 10px rgba(0,0,0,.2); display:flex; align-items:center; gap:12px; font-size:14px;
    }}
    select {{ font-size:13px; font-family:Georgia,serif; border:1px solid #ccc; border-radius:4px; padding:4px 10px; cursor:pointer; }}
    #legend {{
      position:fixed; bottom:30px; right:15px; z-index:9999;
      background:white; padding:10px 14px; border-radius:6px;
      box-shadow:0 2px 8px rgba(0,0,0,.15); font-size:12px; min-width:160px;
    }}
    .legend-bar {{ height:12px; width:140px; margin:6px 0 2px; border-radius:3px; }}
    .legend-labels {{ display:flex; justify-content:space-between; font-size:11px; color:#555; }}
    #source {{ position:fixed; bottom:10px; left:15px; z-index:9999; font-size:11px; color:#888; }}
  </style>
</head>
<body>
<div id="controls">
  <strong>Select Metric:</strong>
  <select id="metric-select" onchange="updateLayer()">{metric_opts_html}</select>
</div>
<div id="legend">
  <div id="legend-title" style="font-weight:bold;margin-bottom:4px;"></div>
  <div class="legend-bar" id="legend-bar"></div>
  <div class="legend-labels"><span id="leg-min"></span><span id="leg-max"></span></div>
</div>
<div id="source">Source: Complete Dataset (incl. coefficients NAs)</div>
<div id="map"></div>

<script>
var geojson  = {json.dumps(geojson_data)};
var colMap   = {col_map_json};
var palettes = {palettes_json};
var map = L.map('map').setView([46.5, 2.5], 6);
L.tileLayer('https://{{s}}.basemaps.cartocdn.com/light_all/{{z}}/{{x}}/{{y}}{{r}}.png',
  {{attribution:'© OpenStreetMap contributors © CARTO'}}).addTo(map);

var currentLayer = null;

function lerp(c1, c2, t) {{
  var h = c => [parseInt(c.slice(1,3),16), parseInt(c.slice(3,5),16), parseInt(c.slice(5,7),16)];
  var [r1,g1,b1]=h(c1), [r2,g2,b2]=h(c2);
  return `rgb(${{Math.round(r1+(r2-r1)*t)}},${{Math.round(g1+(g2-g1)*t)}},${{Math.round(b1+(b2-b1)*t)}})`;
}}

function getColor(val, min, max, pal) {{
  if (val==null) return '#e0e0e0';
  var t = (val-min)/(max-min);
  return t < 0.5 ? lerp(pal[0],pal[1],t*2) : lerp(pal[1],pal[2],(t-0.5)*2);
}}

function fmt(v, unit='') {{ return (v!=null && v!='') ? v+unit : 'N/A'; }}

function updateLayer() {{
  var metric = document.getElementById('metric-select').value;
  var col = colMap[metric], pal = palettes[metric];
  var vals = geojson.features.map(f=>f.properties[col]).filter(v=>v!=null);
  var min = Math.min(...vals), max = Math.max(...vals);

  if (currentLayer) map.removeLayer(currentLayer);
  currentLayer = L.geoJSON(geojson, {{
    style: f => ({{
      fillColor: getColor(f.properties[col], min, max, pal),
      fillOpacity: 0.75, color:'#555', weight:0.5
    }}),
    onEachFeature: function(feature, layer) {{
      var p = feature.properties;
      layer.bindTooltip(
        `<div style="font-family:Georgia,serif;font-size:12px;min-width:210px;">
        <b>${{p.nom||''}}</b><br>
        Total farms: ${{p.total_farms}} &nbsp;|&nbsp; <b>${{metric}}: ${{fmt(p[col],'%')}}</b>
        <hr style="margin:4px 0">
        <table style="font-size:11px;border-collapse:collapse;width:100%">
          <tr><th></th><th style="color:#c0392b;text-align:right">Lost CAP</th><th style="color:#27ae60;text-align:right">Kept CAP</th></tr>
          <tr><td>Age (yrs)</td>   <td style="text-align:right">${{fmt(p.age_lost)}}</td>   <td style="text-align:right">${{fmt(p.age_kept)}}</td></tr>
          <tr><td>Area (ha)</td>   <td style="text-align:right">${{fmt(p.sau_lost)}}</td>   <td style="text-align:right">${{fmt(p.sau_kept)}}</td></tr>
          <tr><td>Output (€)</td>  <td style="text-align:right">${{fmt(p.output_lost)}}</td><td style="text-align:right">${{fmt(p.output_kept)}}</td></tr>
          <tr><td>Livestock (UGB)</td><td style="text-align:right">${{fmt(p.ugb_lost)}}</td><td style="text-align:right">${{fmt(p.ugb_kept)}}</td></tr>
        </table></div>`,
        {{sticky:true}}
      );
    }}
  }}).addTo(map);

  document.getElementById('legend-title').innerText = metric;
  document.getElementById('legend-bar').style.background =
    `linear-gradient(to right,${{pal[0]}},${{pal[1]}},${{pal[2]}})`;
  document.getElementById('leg-min').innerText = min.toFixed(1)+'%';
  document.getElementById('leg-max').innerText = max.toFixed(1)+'%';
}}

updateLayer();
</script>
</body>
</html>"""

with open("Complete_case_cap_map.html", "w", encoding="utf-8") as f:
    f.write(html)
