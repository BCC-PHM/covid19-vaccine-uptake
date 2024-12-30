# -*- coding: utf-8 -*-
"""
Covid-19 Vaccine uptake by ethnicity and IMD
"""
import config
import seaborn as sns
import pandas as pd
import EquiPy.Matrix as Mat

import matplotlib.pyplot as plt
import numpy as np

custom_params = {"axes.spines.right": False, "axes.spines.top": False}
sns.set_theme(style="ticks", rc=custom_params)

plot_colors = {
    "Female" : "Greens",
    "Male" : "Purples"
        }

#%% Covid-19 vaccine rates by broad ethnicity and IMD

data = pd.read_csv(
    config.vaccine_data_path + "\\covid_vac_rates_broad-oct23_to_sept24.csv"
    )

data["Ethnic Group"] = data["BroadEthnicity"]
data["IMD Quintile"] = data["IMD_quintile"]

for age_grp in np.unique(data["Age_Group"]):
    for sex in ["Female", "Male"]:

        data_age = data[data["Age_Group"] == age_grp]
        
        data_sex = data_age[data_age["Sex"] == sex]
        
        N_table = data_sex[["Ethnic Group", "IMD Quintile", "N_GP"]].pivot_table(
            values = "N_GP", 
            index = "IMD Quintile",
            columns = "Ethnic Group"
            )
        
        n_table = data_sex[["Ethnic Group", "IMD Quintile", "n"]].pivot_table(
            values = "n", 
            index = "IMD Quintile",
            columns = "Ethnic Group"
            )
        
        perc_table = 100 * n_table / N_table
        
        fig = Mat.inequality_map(N_table, 
                            perc_pivot = perc_table,
                            title = "Covid-19 Vaccine\nUptake %",
                            #ttest = True,
                            letter="",
                            CI_method = "Byar",
                            palette = plot_colors[sex],
                            bar_rel_size = [0.25, 0.25]
                            )
        
        fig.axes[0].set_xlabel("")
        
        save_name = "..\\output\\eth-IMD-matricies\\" + \
            "CovidMat-{}-{}.png".format(age_grp, sex)

        fig.savefig(save_name, bbox_inches = "tight",
                    dpi = 300)
plt.close("all")

#%% Number unvaccinated for covid-19 between October 2023 – September 2024) by ethnicity and IMD

data2 = pd.read_csv(
    config.vaccine_data_path + "\\covid_unvaccinated_counts-oct23_to_sept24.csv"
    )

eth_order = ['Any other Asian background', 'Bangladeshi', 'Indian', 
             'Pakistani', 'African', 'Any other Black background', 
             'Caribbean', 'Any other mixed background', 'White and Asian',
             'White and Black African', 'White and Black Caribbean',
             'Any other ethnic group', 'Chinese',
             'Any other white background', 'Irish', 'White British']

data2["IMD Quintile"] = data2["IMD_quintile"]

for age_grp in np.unique(data2["Age_Group"]):
    for sex in ["Female", "Male"]:

        data_age = data2[data2["Age_Group"] == age_grp]
        
        data_sex = data_age[data_age["Sex"] == sex]
        
        # Calcuate counts pivot
        N_table = data_sex[["Ethnicity", "IMD Quintile", "number_unvaxed"]].pivot_table(
            values = "number_unvaxed", 
            index = "IMD Quintile",
            columns = "Ethnicity"
            ).fillna(0).astype(int).loc[:, eth_order]
        
        # Plot inequality matrix
        fig = Mat.inequality_map(N_table, 
                            title = "# Unvaccinated",
                            #ttest = True,
                            letter="",
                            palette = plot_colors[sex],
                            width = 15,
                            height = 7,
                            supp_thresh = 5,
                            supp_label = "<5",
                            bar_rel_size = [0.2, 0.12]
                            )

        fig.axes[0].set_xticklabels(
            fig.axes[0].get_xticklabels(), 
            rotation = 45,
            ha = "right",
            rotation_mode='anchor'
            )
        fig.axes[0].set_xlabel("")
        
        save_name = "..\\output\\unvaccinated\\eth-IMD-matricies\\" + \
            "Unvaxed-{}-{}.png".format(age_grp, sex)

        fig.savefig(save_name, bbox_inches = "tight",
                    dpi = 300)
plt.close("all")