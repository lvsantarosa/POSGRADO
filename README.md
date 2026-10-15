<div align="center">

# 🌎💧 Material de Posgrado — Introducción al SIG, Hidrología y Geotecnologías

**Laboratorio de Recursos Hídricos y Geotecnologías (LRHG)**
Facultad de Ciencias Agronómicas y de los Alimentos · Escuela de Agronomía
Pontificia Universidad Católica de Valparaíso (PUCV)

[![R](https://img.shields.io/badge/R-terra%20%7C%20tidyterra-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)
[![Python](https://img.shields.io/badge/Python-geemap%20%7C%20geopandas-3776AB?logo=python&logoColor=white)](https://www.python.org/)
[![Google Earth Engine](https://img.shields.io/badge/Google%20Earth%20Engine-API-4285F4?logo=googleearth&logoColor=white)](https://earthengine.google.com/)
[![Colab](https://img.shields.io/badge/Google%20Colab-abrir%20notebooks-F9AB00?logo=googlecolab&logoColor=white)](https://colab.research.google.com/)
[![QGIS](https://img.shields.io/badge/QGIS-3.x-589632?logo=qgis&logoColor=white)](https://qgis.org/)

</div>

---

## 📖 Sobre el repositorio

Este repositorio reúne **materiales, scripts, datos de apoyo y orientaciones** utilizados en las asignaturas de posgrado impartidas en el ámbito de la Facultad de Ciencias Agronómicas y de los Alimentos y de la Escuela de Agronomía de la **PUCV**.

Su contenido se articula en torno a los temas centrales de investigación del **Laboratorio de Recursos Hídricos y Geotecnologías**:

| | Línea temática |
|:--:|:--|
| 💧 | **Hidrología** y balance hídrico de cuencas |
| 🛰️ | **Geotecnologías** y teledetección aplicada |
| 🏞️ | **Gestión de cuencas hidrográficas** |
| 🗺️ | **Análisis espacial aplicado** a la agricultura |

El propósito principal es ofrecer una documentación **organizada, reproducible y transparente** que facilite a estudiantes e investigadores la ejecución de análisis, la replicación de ejercicios y la consulta de contenidos teóricos y prácticos.

---

## 🗂️ Estructura del repositorio

```
POSGRADO/
├── R/                    # Asignatura: Introducción a los SIG (INTROSIG)
│   ├── INTROSIG_TALLER1_R_VECTOR_DATA.R
│   ├── INTROSIG_TALLER2_R_RASTER_DATA.R
│   ├── INTROSIG_TALLER3_R_EVALUACION_QGIS.R
│   ├── INTROSIG_TALLER4_R_COQUIMBO_DERECHOS.R
│   └── DATOS/
│       ├── datospublicos.md     # Fuentes oficiales de descarga
│       ├── 01_FRUTALES.rar
│       └── Derechos.rar
└── Python/               # Asignatura: Geohidrología (GEOHIDRO) — Google Earth Engine
    ├── GEOHIDRO_TALLER3.ipynb
    ├── GEOHIDRO_TALLER4.ipynb
    └── GEOHIDRO_TALLER5.ipynb
```

---

## 📘 Contenido de los talleres

### 🅡 Introducción a los SIG — scripts en R

| Taller | Script | Contenido |
|:--:|:--|:--|
| 1 | [`INTROSIG_TALLER1_R_VECTOR_DATA.R`](R/INTROSIG_TALLER1_R_VECTOR_DATA.R) | Datos **vectoriales**: carga de coberturas frutícolas y provincias de Valparaíso, geoprocesos de intersección y estadística descriptiva de superficie cultivada. |
| 2 | [`INTROSIG_TALLER2_R_RASTER_DATA.R`](R/INTROSIG_TALLER2_R_RASTER_DATA.R) | Integración **vector–ráster** con Landsat 8 (Nivel 2): recorte a la Provincia de Quillota, conversión a reflectancia de superficie, índices **NDVI, NDWI y NDMI** y estadísticas zonales por especie frutal. |
| 3 | [`INTROSIG_TALLER3_R_EVALUACION_QGIS.R`](R/INTROSIG_TALLER3_R_EVALUACION_QGIS.R) | **Evaluación práctica**: densidad de riego y superficie frutícola provincial, NDVI estacional y estadísticas zonales. |
| 4 | [`INTROSIG_TALLER4_R_COQUIMBO_DERECHOS.R`](R/INTROSIG_TALLER4_R_COQUIMBO_DERECHOS.R) | **Vulnerabilidad hídrica y estrés vegetativo (Coquimbo)**: dependencia de aguas subterráneas de la matriz frutícola a partir de los derechos de aprovechamiento de la DGA y mosaicos Landsat 8 de sequía estival. |

**Paquetes R utilizados:** `terra`, `tidyterra`, `dplyr`, `tidyr`, `ggplot2`, `readxl`, `janitor`

```r
install.packages(c("terra", "tidyterra", "dplyr", "tidyr", "ggplot2", "readxl", "janitor"))
```

### 🅟 Geohidrología — notebooks en Python / Google Earth Engine

| Taller | Notebook | Contenido |
|:--:|:--|:--|
| 3 | [`GEOHIDRO_TALLER3.ipynb`](Python/GEOHIDRO_TALLER3.ipynb) | Análisis espacial y temporal de **precipitación (CHIRPS)**: extracción de series por subcuenca, detección de quiebres y generación de mapas. |
| 4 | [`GEOHIDRO_TALLER4.ipynb`](Python/GEOHIDRO_TALLER4.ipynb) | **Balance hídrico** mensual (2003–2026) con CHIRPS (P) y MODIS MOD16A2GF (ET), series temporales y cartografía. |
| 5 | [`GEOHIDRO_TALLER5.ipynb`](Python/GEOHIDRO_TALLER5.ipynb) | **Análisis comparativo entre cuencas** con panel de control configurable: CHIRPS, MODIS, GLDAS y ERA5-Land, balances anuales y boxplots. |

**Librerías Python utilizadas:** `earthengine-api`, `geemap`, `geopandas`, `pandas`, `numpy`, `matplotlib`, `seaborn`

> 💡 Los notebooks están pensados para ejecutarse en **Google Colab** con Google Drive montado y una cuenta habilitada en Google Earth Engine.

---

## 🚀 Cómo utilizar este material

1. **Clonar el repositorio**
   ```bash
   git clone https://github.com/lvsantarosa/POSGRADO.git
   cd POSGRADO
   ```
2. **Descargar los datos** desde sus fuentes oficiales siguiendo [`R/DATOS/datospublicos.md`](R/DATOS/datospublicos.md).
3. **Ajustar las rutas** al inicio de cada script o notebook (`setwd()`, `vect()`, `ruta_zip`): las rutas incluidas corresponden al equipo del docente y deben adaptarse a su propia estructura de carpetas.
4. **Ejecutar paso a paso**, siguiendo los bloques numerados y comentados de cada archivo.

### ✅ Requisitos previos

- [R](https://www.r-project.org/) ≥ 4.2 y [RStudio](https://posit.co/download/rstudio-desktop/)
- [QGIS](https://qgis.org/) 3.x (talleres de evaluación)
- Cuenta de [Google Earth Engine](https://earthengine.google.com/signup/) y acceso a [Google Colab](https://colab.research.google.com/)

---

## 📂 Datos

Los conjuntos de datos **no se distribuyen completos** en este repositorio: deben descargarse desde sus fuentes oficiales (ODEPA/ICET, BCN, DGA). El detalle de cada fuente, su descripción y la carpeta de destino están documentados en **[`R/DATOS/datospublicos.md`](R/DATOS/datospublicos.md)**.

Para datos satelitales preparados para Chile, véase también el repositorio complementario [**gee-global-data-chile**](https://github.com/lvsantarosa/gee-global-data-chile), junto con los demás repositorios listados en [Repositorios complementarios](#-repositorios-complementarios).

---

## 🔗 Repositorios complementarios

Otros repositorios del autor con material de apoyo, scripts y tutoriales que amplían los contenidos de estos talleres:

| Repositorio | Lenguaje | Contenido |
|:--|:--:|:--|
| [**gee-global-data-chile**](https://github.com/lvsantarosa/gee-global-data-chile) | GEE | Datos satelitales globales preparados y recortados para **Chile**, listos para su uso en Google Earth Engine. |
| [**NASA-data-download**](https://github.com/lvsantarosa/NASA-data-download) | Python | Descarga **masiva de datos NASA** (GES DISC) con `wget` a partir de una lista de enlaces, ejecutable en Google Colab con Google Drive montado. Incluye el notebook de ejemplo para el conjunto **GRACE**. |
| [**FLDAS_GLDAS_Processing**](https://github.com/lvsantarosa/FLDAS_GLDAS_Processing) | R | Procesamiento de las series hidrológicas **FLDAS**, **GLDAS** y **GRACE**: lectura de los archivos descargados, extracción de variables del balance hídrico y almacenamiento de agua subterránea. |
| [**Geostatistical-analysis-in-R**](https://github.com/lvsantarosa/Geostatistical-analysis-in-R) | R | **Análisis geoestadístico** con `gstat`: organización de los datos de monitoreo del Servicio Geológico de Brasil (RIMAS) con el *tidyverse*, variografía y predicción espacial del nivel del agua subterránea. |

> 💡 Los tres últimos repositorios fueron desarrollados en el contexto de la asignatura *Analysis of hydrological and climatic data obtained by Orbital Sensors* (Posgrado en Geociencias y Medio Ambiente, UNESP) y se mantienen aquí como referencia metodológica complementaria.

---

## 🤖 Nota sobre el uso de inteligencia artificial

Una parte significativa de los materiales ha sido **perfeccionada mediante herramientas de inteligencia artificial**, con el fin de mejorar la claridad de los comentarios, optimizar la estructura de los documentos y facilitar el estudio y uso de estos recursos. La concepción metodológica, la selección de datos y la validación de los resultados son responsabilidad del autor.

---

## 📑 Cómo citar

> Santarosa, L. V. (2026). *Material de apoyo y scripts para cursos de posgrado — Laboratorio de Recursos Hídricos y Geotecnologías*. Pontificia Universidad Católica de Valparaíso. https://github.com/lvsantarosa/POSGRADO

---

## 📬 Contacto

| | |
|:--|:--|
| 📷 **Instagram del laboratorio** | [@lrhg.pucv](https://www.instagram.com/lrhg.pucv) |
| 💼 **LinkedIn** | [Lucas Vituri Santarosa](https://www.linkedin.com/in/lucas-vituri-santarosa-321b74a6/) |
| 🏛️ **Institución** | [Pontificia Universidad Católica de Valparaíso](https://www.pucv.cl/) |

<div align="center">

---

*Prof. Lucas Vituri Santarosa · Laboratorio de Recursos Hídricos y Geotecnologías · PUCV*

</div>
