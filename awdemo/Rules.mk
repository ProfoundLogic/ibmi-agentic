# AdelWiggins (AWDEMO) demo dataset
#
# Schema is a clone of library BOMSAMPLE, so programs written against
# BOMSAMPLE run unchanged here.  Objects are created into the current
# build library -- set IBMI_BUILD_LIBRARY=AWDEMO to deploy for real.
#
# Data is NOT loaded by codermake.  After building, run:
#     awdemo/tools/load-awdemo-data.sh <LIBRARY>

# Master tables (no foreign keys)
item.file:      sql/item.table.sql
warehouse.file: sql/warehouse.table.sql
supplier.file:  sql/supplier.table.sql

# Dependent tables -- order-only so a master rebuild does not force these
bomheader.file: sql/bomheader.table.sql | item.file
bomdetail.file: sql/bomdetail.table.sql | bomheader.file item.file
inventory.file: sql/inventory.table.sql | item.file warehouse.file
supplitem.file: sql/supplitem.table.sql | supplier.file item.file
mfgorders.file: sql/mfgorders.table.sql | item.file

# Indexes -- system names pinned to match BOMSAMPLE
ixitemtype.file: sql/ixitemtype.index.sql item.file
ixbomhpar.file:  sql/ixbomhpar.index.sql  bomheader.file
ixbomdcmp.file:  sql/ixbomdcmp.index.sql  bomdetail.file
ixbomdbom.file:  sql/ixbomdbom.index.sql  bomdetail.file
ixsuppname.file: sql/ixsuppname.index.sql supplier.file
ixsuppitem.file: sql/ixsuppitem.index.sql supplitem.file
ixmfoitem.file:  sql/ixmfoitem.index.sql  mfgorders.file
ixmfodate.file:  sql/ixmfodate.index.sql  mfgorders.file
ixmfostat.file:  sql/ixmfostat.index.sql  mfgorders.file
