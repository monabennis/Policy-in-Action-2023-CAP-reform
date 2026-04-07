# Panel data 

import pandas as pd
import numpy as np

df = pd.read_parquet("/home/onyxia/work/Policy in Action/Data/PiA_data.parquet")
df['farm_id'] = range(len(df))

# ============================================================================
# HELPER (makes sure binary and nans are kept)
# ============================================================================

def to_binary(condition, source):
    result = condition.astype(float)
    result[source.isna()] = np.nan
    return result

# ============================================================================
# STEP 1: BUILD PANEL FROM A GIVEN DATAFRAME
# ============================================================================

def build_panel(df):
    df = df.copy()

    # --- 2023 numeric labor ---
    df['NB_SAISON_2023']    = df['NB_SAISON']    * df['COEF2023']
    df['MOF_SAL_TOT_2023']  = df['MOF_SAL_TOT']  * df['COEF2023']
    df['MOF_NSAL_TOT_2023'] = df['MOF_NSAL_TOT'] * df['COEF2023']
    df['MONF_TOT_2023']     = df['MONF_TOT']      * df['COEF2023']

    # --- Labour type 2023 ---
    def labour_type_2023(row):
        cols = ['MOF_SAL_TOT_2023', 'MOF_NSAL_TOT_2023', 'MONF_TOT_2023', 'NB_SAISON_2023']
        if all(pd.isna(row[c]) for c in cols):
            return np.nan
        has_family    = (
            (row['MOF_SAL_TOT_2023']  > 0 if pd.notna(row['MOF_SAL_TOT_2023'])  else False) or
            (row['MOF_NSAL_TOT_2023'] > 0 if pd.notna(row['MOF_NSAL_TOT_2023']) else False)
        )
        has_nonfamily = row['MONF_TOT_2023']  > 0 if pd.notna(row['MONF_TOT_2023'])  else False
        has_seasonal  = row['NB_SAISON_2023'] > 0 if pd.notna(row['NB_SAISON_2023']) else False

        if has_family and not has_nonfamily and not has_seasonal:   return 'Family only'
        elif has_family and has_seasonal and not has_nonfamily:     return 'Family + Seasonal'
        elif has_family and has_nonfamily:                          return 'Family + Hired'
        elif not has_family and has_nonfamily:                      return 'Hired only'
        elif not has_family and has_seasonal:                       return 'Seasonal only'
        else:                                                        return 'No workforce recorded'

    df['LABOUR_TYPE_2023'] = df.apply(labour_type_2023, axis=1)
    df['SEASON_2023']      = to_binary(df['NB_SAISON_2023'] > 0, df['NB_SAISON_2023'])

    # --- Livestock 2023 ---
    df['UGBTA.TOT_2023'] = df['UGBTA.TOT'] * df['COEF2023']
    df['UGBTA.TOT_CAT_2023'] = pd.cut(
        df['UGBTA.TOT_2023'],
        bins=[-0.001, 0, 10, 50, 100, 200, 500, 14001],
        labels=['No livestock (0)', 'Very Small (<10 LSU)', 'Small (10-50 LSU)',
                'Medium (50-100 LSU)', 'Large (100-200 LSU)',
                'Very Large (200-500 LSU)', 'Intensive (>500 LSU)'],
        include_lowest=True
    )
    df.loc[df['UGBTA.TOT_2023'].isna(), 'UGBTA.TOT_CAT_2023'] = np.nan

    # --- PBS 2023 ---
    pbs_bins   = [-0.001, 0, 25000, 100000, 250000, 500000, 1000000, 101200000]
    pbs_labels = ['No production (0)', 'Micro (<25k€)', 'Very Small (25-100k€)',
                  'Small (100-250k€)', 'Medium (250-500k€)', 'Large (500k-1M€)', 'Very Large (>1M€)']
    df['PBSTOT_COEF17_2023'] = df['PBSTOT_COEF17'] * df['COEF2023']
    df['PBSTOT_CAT_2023']    = pd.cut(df['PBSTOT_COEF17_2023'], bins=pbs_bins, labels=pbs_labels, include_lowest=True)
    df.loc[df['PBSTOT_COEF17_2023'].isna(), 'PBSTOT_CAT_2023'] = np.nan

    # --- Variable mapping ---
    pairs = {
        'OVER_67_2020':   'OVER_67_2023',
        'BENEF_PAC_2020': 'BENEF_PAC_2023',
        'UGBTA.TOT':      'UGBTA.TOT_2023',
        'UGBTA.TOT_CAT':  'UGBTA.TOT_CAT_2023',
        'PBSTOT_COEF17':  'PBSTOT_COEF17_2023',
        'PBSTOT_CAT':     'PBSTOT_CAT_2023',
        'NB_SAISON':      'NB_SAISON_2023',
        'SEASON':         'SEASON_2023',
        'MOF_SAL_TOT':    'MOF_SAL_TOT_2023',
        'MOF_NSAL_TOT':   'MOF_NSAL_TOT_2023',
        'MONF_TOT':       'MONF_TOT_2023',
        'LABOUR_TYPE':    'LABOUR_TYPE_2023',
    }
    exclude_from_copy = (
        {'farm_id', 'COEF2023', 'IMPUT_NR_TOT', 'ETAT_ESEA2023', 'EXIT'} |
        set(pairs.keys()) | set(pairs.values())
    )
    vars_to_copy = [col for col in df.columns if col not in exclude_from_copy]

    # --- Build 2020 and 2023 rows ---
    df_2020 = df[['farm_id'] + vars_to_copy + list(pairs.keys())].copy()
    df_2020['year'] = 2020

    df_2023 = df[['farm_id'] + vars_to_copy].copy()
    for col_2020, col_2023 in pairs.items():
        df_2023[col_2020] = df[col_2023]
    df_2023['year']          = 2023
    df_2023['EXIT']          = df['EXIT']
    df_2023['ETAT_ESEA2023'] = df['ETAT_ESEA2023']
    df_2023['COEF2023']      = df['COEF2023']
    df_2023['IMPUT_NR_TOT']  = df['IMPUT_NR_TOT']

    # --- Stack ---
    panel = (pd.concat([df_2020, df_2023], axis=0, ignore_index=True)
               .sort_values(['farm_id', 'year'])
               .reset_index(drop=True))
    panel = panel.rename(columns={'OVER_67_2020': 'OVER_67', 'BENEF_PAC_2020': 'BENEF_PAC'})

    return panel

# ============================================================================
# STEP 2: CREATE 2 DATASETS
# ============================================================================

# Complete case: only farms where COEF2023 is present
df_complete = df.loc[df['COEF2023'].notna()].copy()
panel_complete = build_panel(df_complete)

# Full dataset: all farms (COEF2023 NA → all 2023-computed columns will be NA)
panel_full = build_panel(df)

# ============================================================================
# STEP 3:  SAVE
# ============================================================================

n_total    = len(df)
n_complete = df['COEF2023'].notna().sum()
n_missing  = df['COEF2023'].isna().sum()

panel_complete.to_parquet(
    "/home/onyxia/work/Policy in Action/Data/PiA_panel_complete_case.parquet",
    engine='pyarrow', compression='snappy', index=False
)
panel_full.to_parquet(
    "/home/onyxia/work/Policy in Action/Data/PiA_panel_data_NA.parquet",
    engine='pyarrow', compression='snappy', index=False
)
