import os

from airflow import DAG
from airflow.contrib.hooks.fs_hook import FSHook
from airflow.contrib.sensors.file_sensor import FileSensor
from airflow.hooks.mysql_hook import MySqlHook
from airflow.operators.python_operator import PythonOperator
from airflow.utils.dates import days_ago
from structlog import get_logger
import pandas as pd

logger = get_logger()



DATE_COLUMNS = ["ORDERDATE"]

FILE_CONNECTION_NAME = 'monitor_file'
CONNECTION_DB_NAME = 'mysql_db' 

def etl_process(**kwargs):
    logger.info(kwargs["execution_date"])
    file_path = FSHook(FILE_CONNECTION_NAME).get_path()
    filename = 'time_series_covid19_deaths_global.csv'
    mysql_connection = MySqlHook(mysql_conn_id=CONNECTION_DB_NAME).get_sqlalchemy_engine()
    full_path = f'{file_path}/{filename}'
    logger.info(full_path)
    df = pd.read_csv(full_path) 
    df = pd.melt(df, id_vars =['Lat','Long','Province/State','Country/Region'],var_name="RegDate", value_name="Count")
    df = df[df["Count"] > 0]
    df =df.rename(columns={'Province/State': 'State', 'Country/Region': 'Country'})
    df['RegDate'] = pd.to_datetime(df['RegDate'])
    df['Type'] = 'D'
    with mysql_connection.begin() as connection:
        connection.execute("DELETE FROM Covid.Cases WHERE Type='D'")
        df.to_sql('Cases', con=connection, schema='Covid', if_exists='append', index=False)

    
    os.remove(full_path)



    logger.info(f"Rows inserted confirmed {len(df.index)}")





dag = DAG('InsertDataDagDead', description='Dag to Ingest covid data',
          default_args={
              'owner': 'Jose.Benavides',
              'depends_on_past': False,
              'max_active_runs': 1,
              'start_date': days_ago(5)
              
          },
          schedule_interval='0 1 * * *',
          catchup=False)

sensor = FileSensor(task_id="file_sensor_task3",
                    dag=dag,
                    filepath='time_series_covid19_deaths_global.csv',
                    fs_conn_id=FILE_CONNECTION_NAME,
                    poke_interval=10,
                    timeout=600)

etl = PythonOperator(task_id="sales_etl3",
                     provide_context=True,
                     python_callable=etl_process,
                     dag=dag
                     )

sensor >> etl
