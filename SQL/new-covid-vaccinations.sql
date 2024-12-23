SELECT [action]
      ,[pillar]
      ,[site_ods_code]
      ,[string_dose_number]
      ,[cleaned_cohort]
      ,[ethnicity_description]
      ,[practice_code]
      ,[age]
      ,[AgeBand]
      ,[gender]
      ,[LSOA]
	  ,TimePeriod = 'Oct 23 - Sept 24'
  FROM [EAT_Reporting_BSOL].[COVID].[vw_Immunisations]
  WHERE 
	[vaccination_date] < '2024-10-01' AND
	[vaccination_date] >= '2023-10-01'

