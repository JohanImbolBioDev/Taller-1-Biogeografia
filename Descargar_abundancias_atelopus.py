
# # Descargar abundacias de especies o generos

import subprocess
import sys

librerias = ["pandas", "openpyxl", "pygbif"]

for lib in librerias:
    try:
        __import__(lib)
    except ImportError:
        subprocess.check_call([sys.executable, "-m", "pip", "install", lib])

# %%
from pygbif import species

resultado = species.name_backbone(scientificName="Atelopus", # Cambia Atelopus por tu busqueda
                                  taxonRank="GENUS",   # Cambia el nivel taxonomico de la busqueda que quieres hacer (revisar pygbif )
                                  family="Bufonidae")  # opcional para una busqueda más rapida y especifica


# %%
from pygbif import occurrences as occ
import pandas as pd
import time


def descargar_registros_especie(
    taxon_key,
    nombre_especie
):

    registros = []

    offset = 0
    limit = 300


    while True:

        respuesta = occ.search(

            taxonKey=int(taxon_key),

            country="CO",

            hasCoordinate=True,

            limit=limit,

            offset=offset
        )


        resultados = respuesta[
            "results"
        ]


        if len(resultados) == 0:

            break


        for registro in resultados:

            registro[
                "especie_consulta"
            ] = nombre_especie


            registros.append(
                registro
            )


        print(
            f"{nombre_especie}: "
            f"{len(registros)} registros"
        )


        if len(resultados) < limit:

            break


        offset += limit

        time.sleep(0.2)


    return registros

# %%
registros_totales = descargar_registros_especie(resultado['usage']['key'], "Atelopus")	

# %%
df_registros = pd.DataFrame(
    registros_totales
)

# %%
df_registros.head()

# %%
df_registros.to_csv(
    "Datos/registros_gbif_raw_atelopus.csv",
    index=False
)

# %% [markdown]
# Los datos descargados contienen muchas columnas que no necesitamos. Por lo que seleccionamos solo las que bos sirven

# %%
columnas = [

    "key",

    "scientificName",

    "species",

    "decimalLatitude",

    "decimalLongitude",

    "stateProvince",

    "municipality",

    "basisOfRecord",

    "year",

    "datasetKey",

    "datasetName",

    "institutionCode",

    "collectionCode",

    "coordinateUncertaintyInMeters",

    "hasGeospatialIssues",

    "issues",

    "especie_consulta"
]

# %%
columnas_existentes = [

    columna

    for columna in columnas

    if columna in df_registros.columns
]


df_registros = df_registros[
    columnas_existentes
]

# %% [markdown]
# ## Guardar los datos obtenidos

# %%
df_registros.to_csv(

    "Datos/registros_atelopus_colombia.csv",

    index=False
)


