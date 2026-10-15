# 📂 Fuentes de datos utilizadas

Los conjuntos de datos empleados en los talleres **no se distribuyen completos** en este repositorio. Deben descargarse desde sus fuentes oficiales y ubicarse en las carpetas correspondientes para permitir la ejecución de los scripts.

> ⚠️ Recuerde ajustar las rutas (`setwd()`, `vect()`, `ruta_zip`) al inicio de cada script según su propia estructura de carpetas.

---

## 🗃️ Conjuntos de datos

### 🥭 Frutales — ICET / ODEPA

| | |
|:--|:--|
| **Fuente oficial** | <https://icet.odepa.gob.cl/> |
| **Descripción** | Estadísticas y cartografía de superficie frutícola nacional. |
| **Carpeta sugerida** | `data/frutales/` |
| **Usado en** | Talleres 1, 2, 3 y 4 (R) |

### 🗺️ Límites político-administrativos — BCN

| | |
|:--|:--|
| **Fuente oficial** | <https://www.bcn.cl/siit/mapas_vectoriales/index_html> |
| **Descripción** | Límites comunales, provinciales y regionales de Chile en formato vectorial. |
| **Carpeta sugerida** | `data/limites_politicos/` |
| **Usado en** | Talleres 1, 2, 3 y 4 (R) |

### 💧 Derechos de aprovechamiento de aguas — DGA

| | |
|:--|:--|
| **Fuente oficial** | <https://dga.mop.gob.cl/derechos-de-agua/derechos-registrados/> |
| **Descripción** | Registro público de derechos de aguas consuntivos y no consuntivos. |
| **Carpeta sugerida** | `data/derechos_agua/` |
| **Usado en** | Taller 4 (R) — Coquimbo |

### 🏞️ Cuencas y subcuencas — Mapoteca Digital DGA

| | |
|:--|:--|
| **Fuente oficial** | <https://dga.mop.gob.cl/mapoteca-digital/> |
| **Descripción** | Cartografía oficial de cuencas, subcuencas y subsubcuencas (BNA). |
| **Carpeta sugerida** | Google Drive, como archivo `.zip` (ver notebooks) |
| **Usado en** | Talleres 3, 4 y 5 (Python / GEE) |

---

## 🛰️ Datos satelitales

Las imágenes y productos satelitales (Landsat 8, CHIRPS, MODIS, GLDAS, ERA5-Land) se acceden directamente desde **Google Earth Engine** en los notebooks de Python.

Para aplicaciones ya preparadas para Chile, consulte el repositorio complementario:

👉 [**lvsantarosa/gee-global-data-chile**](https://github.com/lvsantarosa/gee-global-data-chile)
