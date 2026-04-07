

import pandas as pd
import numpy as np

ra_esea = pd.read_parquet("/home/onyxia/work/Policy in Action/Data/RA2020_ESEA2023_PAC.parquet")


# Helper: binary float (0/1) keeping NaN
def to_binary(condition, source):
    result = condition.astype(float)
    result[source.isna()] = np.nan
    return result

# ============================================================================
# LOCATION & TERRITORIAL CHARACTERISTICS
# ============================================================================

ra_esea['REGL'] = to_binary(ra_esea['REGL_1305_2013'] != 'NNT_ANC', ra_esea['REGL_1305_2013'])

# ============================================================================
# FARM SIZE & RESOURCES
# ============================================================================

# --- Agricultural Land ---
ra_esea['SAU_TOT_CAT'] = pd.cut(
    ra_esea['SAU_TOT'],
    bins=[-0.001, 0, 5, 20, 50, 100, 200, 2700],
    labels=['No land (0ha)', 'Micro (<5ha)', 'Very Small (5-20ha)',
            'Small (20-50ha)', 'Medium (50-100ha)',
            'Large (100-200ha)', 'Very Large (>200ha)'],
    include_lowest=True
)
ra_esea.loc[ra_esea['SAU_TOT'].isna(), 'SAU_TOT_CAT'] = np.nan

# --- Livestock ---
ra_esea['UGBTA.TOT_CAT'] = pd.cut(
    ra_esea['UGBTA.TOT'],
    bins=[-0.001, 0, 10, 50, 100, 200, 500, 14001],
    labels=['No livestock (0)', 'Very Small (<10 LSU)', 'Small (10-50 LSU)',
            'Medium (50-100 LSU)', 'Large (100-200 LSU)',
            'Very Large (200-500 LSU)', 'Intensive (>500 LSU)'],
    include_lowest=True
)
ra_esea.loc[ra_esea['UGBTA.TOT'].isna(), 'UGBTA.TOT_CAT'] = np.nan

ra_esea['PBSTOT_CAT'] = pd.cut(
    ra_esea['PBSTOT_COEF17'],
    bins=[-0.001, 0, 25000, 100000, 250000, 500000, 1000000, 101200000],
    labels=['No production (0)', 'Micro (<25k€)', 'Very Small (25-100k€)',
            'Small (100-250k€)', 'Medium (250-500k€)',
            'Large (500k-1M€)', 'Very Large (>1M€)'],
    include_lowest=True
)
ra_esea.loc[ra_esea['PBSTOT_COEF17'].isna(), 'PBSTOT_CAT'] = np.nan

# ============================================================================
# FARM STRUCTURE & CHARACTERISTICS
# ============================================================================

ra_esea['IS_MICRO'] = to_binary(ra_esea['DIMECO_COEF2017'] == "1-micros", ra_esea['DIMECO_COEF2017'])

ra_esea['CAPITAL_TOTAL_BINARY'] = pd.Series(np.nan, index=ra_esea.index, dtype=object)
ra_esea.loc[ra_esea['CAPITAL_TOTAL'] == 100.0, 'CAPITAL_TOTAL_BINARY'] = 'Distributed'
ra_esea.loc[ra_esea['CAPITAL_TOTAL'].notna() & (ra_esea['CAPITAL_TOTAL'] != 100.0),  'CAPITAL_TOTAL_BINARY'] = 'Not distributed'

# ============================================================================
# MANAGEMENT & ORGANIZATION
# ============================================================================

ra_esea['CORPO'] = to_binary(ra_esea['SOC_GESPRESTFIL'] == 1, ra_esea['SOC_GESPRESTFIL'])

# ============================================================================
# LABOR & WORKFORCE
# ============================================================================

def labour_type(row):
    cols = ['MOF_SAL_TOT', 'MOF_NSAL_TOT', 'MONF_TOT', 'NB_SAISON']
    if all(pd.isna(row[c]) for c in cols):
        return np.nan

    # Family = salaried OR non-salaried family workers
    has_family    = (
        (row['MOF_SAL_TOT']  > 0 if pd.notna(row['MOF_SAL_TOT'])  else False) or
        (row['MOF_NSAL_TOT'] > 0 if pd.notna(row['MOF_NSAL_TOT']) else False)
    )
    has_nonfamily = row['MONF_TOT']  > 0 if pd.notna(row['MONF_TOT'])  else False
    has_seasonal  = row['NB_SAISON'] > 0 if pd.notna(row['NB_SAISON']) else False

    if has_family and not has_nonfamily and not has_seasonal:
        return 'Family only'
    elif has_family and has_seasonal and not has_nonfamily:
        return 'Family + Seasonal'
    elif has_family and has_nonfamily:
        return 'Family + Hired'
    elif not has_family and has_nonfamily:
        return 'Hired only'
    elif not has_family and has_seasonal:
        return 'Seasonal only'
    else:
        return 'No workforce recorded'

ra_esea['LABOUR_TYPE'] = ra_esea.apply(labour_type, axis=1)

# Does the farm have any sesonal workers ?
ra_esea['SEASON'] = to_binary(ra_esea['NB_SAISON'] > 0, ra_esea['NB_SAISON'])

# ============================================================================
# ORGANIC FARMING
# ============================================================================

ra_esea['RECENT_BIO'] = to_binary(
    (ra_esea['BIO_ANNEECONV'] >= 2018) & (ra_esea['BIO_ANNEECONV'] <= 2020),
    ra_esea['BIO_ANNEECONV']
)
# ============================================================================
# CERTIFICATIONS LABELS
# ============================================================================

cert_vars = ['LABEL', 'AOC', 'IGP', 'STG', 'AUTREDEM_FIL',
                 'BIODYNAMIE', 'NATUREPROGRES', 'HVE', 'GIEE', 'DEPHY', 'AUTRE']

def get_certifications(row):
    if all(pd.isna(row[c]) for c in cert_vars):
        return np.nan
    active = [c for c in cert_vars if row[c] == 1]
    return ', '.join(active) if active else 'None'

ra_esea['CERTIFICATION'] = ra_esea.apply(get_certifications, axis=1)
ra_esea['CERTIFICATION_BINARY'] = ra_esea['CERTIFICATION'].map(
    lambda x: np.nan if pd.isna(x) else (1 if x != 'None' else 0)
)

# ============================================================================
# SHORT SUPPLY CHAINS
# ============================================================================

products = ['CIRCOU_COPFIL', 'CIRCOU_TUBFIL_DOM', 'CIRCOU_LEGFIL', 'CIRCOU_VINFIL',
            'CIRCOU_HOLIFIL', 'CIRCOU_FRUITFIL', 'CIRCOU_AUTRFIL', 'CIRCOU_LAITFIL',
            'CIRCOU_OVOLFIL', 'CIRCOU_ANIMFIL', 'CIRCOU_MIELFIL']

def get_circou_type(row):
    if all(pd.isna(row[p]) for p in products):
        return np.nan
    active = [p for p in products if row[p] == 1]
    return ', '.join(active) if active else 'None'

ra_esea['CIRCOU_TYPE'] = ra_esea.apply(get_circou_type, axis=1)

# ============================================================================
# RURAL DEVELOPMENT PROGRAMS
# ============================================================================

rdev_vars = ['RDEV_15_02', 'RDEV_16_03', 'RDEV_17_04', 'RDEV_18_05', 'RDEV_19A1_061',
             'RDEV_19A3_063', 'RDEV_21_08', 'RDEV_28_10', 'RDEV_29_11', 'RDEV_31_13', 'RDEV_36_17']
rdev_sum = ra_esea[rdev_vars].sum(axis=1, min_count=1)
ra_esea['RDEV'] = to_binary(rdev_sum > 0, rdev_sum)


def get_rdev(row):
    if all(pd.isna(row[v]) for v in rdev_vars):
        return np.nan
    active = [v for v in rdev_vars if row[v] == 1]
    return ', '.join(active) if active else 'None'

ra_esea['RDEV_LIST'] = ra_esea.apply(get_rdev, axis=1)

# ============================================================================
# FARMER CHARACTERISTICS
# ============================================================================

# 2020
ra_esea['OVER_67_2020']   = to_binary(ra_esea['AGE'] > 67, ra_esea['AGE'])

# 2023 (+3)
ra_esea['AGE_2023']       = ra_esea['AGE'] + 3
ra_esea['OVER_67_2023']   = to_binary(ra_esea['AGE_2023'] > 67, ra_esea['AGE_2023'])

# ============================================================================
# CAP & ACTIVE STATUS
# ============================================================================

ra_esea['EXIT'] = ra_esea['ETAT_ESEA2023'].map({'ACTIF': 0, 'CESSE': 1})  # unmapped → NaN automatically
ra_esea['BENEF_PAC_2020'] = pd.to_numeric(ra_esea['BENEF_PAC_2020'], errors='coerce')
ra_esea['BENEF_PAC_2023'] = pd.to_numeric(ra_esea['BENEF_PAC_2023'], errors='coerce')

# ============================================================================
# VARIABLE CATEGORIES & EXPORT
# ============================================================================

variable_categories = {
    'Location':               ['REGL', 'SIEGENAT', 'SIEGE_LIB_DEP', 'SIEGE_LIB_REG', 'REGL_1305_2013'],
    'Farm Size':              ['SAU_TOT', 'SAU_TOT_CAT'],
    'Livestock':              ['UGBTA.TOT', 'UGBTA.TOT_CAT'],
    'Farm Structure':         ['CAPITAL_TOTAL', 'CAPITAL_TOTAL_BINARY', 'STATUT',
                               'DIMECO_COEF2017', 'IS_MICRO', 'OTEFDA_COEF17', 'PBSTOT_COEF17', 'PBSTOT_CAT'],
    'Management':             ['CORPO', 'EXTERNALISFIL', 'AUTRENTITE_FIL'],
    'Labor':                  ['LABOUR_TYPE', 'MOF_SAL_TOT', 'MOF_NSAL_TOT', 'MONF_TOT', 'NB_SAISON', 'SEASON'],
    'Organic':                ['BIO_INTEGRAL', 'BIO_FIL', 'RECENT_BIO', 'BIO_ANNEECONV'],
    'Certifications':         ['CERTIFICATION', 'CERTIFICATION_BINARY'],
    'Supply Chains':          ['TRANSFOFIL', 'CIRCOUFIL', 'CIRCOU_TYPE'],
    'Rural Development':      ['RDEV', 'RDEV_LIST'],
    'Farmer Characteristics': ['CHEFREFIL', 'REFACTIVEXPL', 'REFSEX', 'AGE',
                               'OVER_67_2020', 'OVER_67_2023'],
    'Tax & Succession':       ['IMPODET', 'DEVENIR'],
    'CAP & Survey':           ['BENEF_PAC_2020', 'BENEF_PAC_2023', 'EXIT', 'ETAT_ESEA2023', 'COEF2023', 'IMPUT_NR_TOT']
}

all_vars = list({v for vlist in variable_categories.values() for v in vlist if v in ra_esea.columns})
df = ra_esea[all_vars].copy()

print("=" * 60)
print("✓ All variables created successfully")
print(f"Full dataset : {len(ra_esea):,} rows")
print(f"Selected dataset: {len(df):,} rows × {len(df.columns)} columns")
print(f"\nNA counts (non-zero only):\n{df.isna().sum()[df.isna().sum() > 0]}")
print("=" * 60)

df.to_parquet("/home/onyxia/work/Policy in Action/Data/PiA_data.parquet",
              engine='pyarrow', compression='snappy', index=False)
