library(bigrquery)

query_ = "
with creds as (
       select first_paid_order, 
              document,
       from dataplatform-prd.ton_analytics.dim_client
       where 
       first_paid_order is not null 
       AND sales_channel = 'Inv. Autocred'
       order by 1 asc
),
address as (
       select document
              , address.city
              , address.state
              --, address.neighborhood
       from dataplatform-prd.planning_ops_ton.vw_smuser_users
)
select t1.first_paid_order as date
       , t2.city
       , t2.state
       --, t2.neighborhood
       , count(distinct t1.document) as creds
from creds t1
left join address t2 using (document)
group by 1, 2, 3
order by 1;
"

bq_auth()

billing_ = "analytics-254117"

# querying:
tb = bq_project_query(billing_, query_)

# download data:
bar = bq_table_download(tb)
setDT(bar)

# saving:
fwrite(bar, "../geoLift_martech/data/creds_autocred.csv")
