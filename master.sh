#This is a master file running all the scripts in the correct order for data construction and data analysis
cd /main_project/build_all_data
sh wildfire_reports/make.sh
sh big_news/make.sh
sh cities/make.sh
sh home_value/make.sh
sh land_cover/make.sh
sh merra2/make.sh
sh newspaper_coverage/make.sh
sh population/make.sh
sh terrain/make.sh
sh wf_jurisdictions/make.sh
sh merge_datasets/make.sh

cd /main_project/data_analysis
sh make.sh
