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

#%% Load data
data = pd.read_csv(
    config.vaccine_data_path + "\\covid_vac_rates-oct23_to_sept24.csv"
    )

data["Ethnic Group"] = data["BroadEthnicity"]
data["IMD Quintile"] = data["IMD_quintile"]

#%% Create pivot tables
plot_colors = {
    "Female" : "Greens",
    "Male" : "Purples"
        }

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
                            title = "Covid Vaccine\nUptake %",
                            #ttest = True,
                            letter="",
                            CI_method = "Byar",
                            palette = plot_colors[sex],
                            )
        save_name = "..\\output\\eth-IMD-matricies\\" + "CovidMat-{}-{}.pdf".format(age_grp, sex)

        fig.savefig(save_name, bbox_inches = "tight",
                    dpi = 300)
plt.close("all")