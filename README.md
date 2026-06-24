# 🏠 Preu de l'Habitatge a Barcelona (2014–2025)

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/R_Shiny-316CE6?style=for-the-badge&logo=RStudio&logoColor=white)

Aquest projecte presenta el disseny, la implementació i l'anàlisi d'una aplicació interactiva de visualització de dades sobre el mercat immobiliari de Barcelona per al període 2014-2025. L'aplicació ha estat desenvolupada íntegrament en R per analitzar l'evolució dels preus, les desigualtats territorials i l'accessibilitat a l'habitatge.

Projecte realitzat per a l'assignatura de **Visualització de Dades** del **Grau en Enginyeria de Dades** de la **Universitat Autònoma de Barcelona (UAB)** (Curs 2025/26).

## 👥 Equip Desenvolupador (Grup 01)
- Marc Arroyo
- Santi Prats
- Oriol Ramos
- Pere Maeso
- David Miquel
- Lluc Verdaguer

## 🎯 Objectius i Hipòtesis
L'aplicació permet explorar de forma interactiva 73 barris i 10 districtes de la ciutat integrant múltiples fonts de dades públiques. S'articula al voltant de tres hipòtesis principals (confirmades amb les dades):

1. **Creixement per sobre de la inflació:** El preu nominal de l'habitatge ha crescut un +82,2% des del 2014 (+37% en termes reals), superant àmpliament l'increment de l'IPC (+26,3%).
2. **Desigualtat territorial:** Existeix una forta bretxa entre districtes, amb una ràtio de 2,1x entre el districte més car (Sarrià-Sant Gervasi) i el més econòmic (Nou Barris), la qual cosa pot suposar una diferència de fins a 240.000€ per un pis de 70m².
3. **Nou vs. Segona mà:** L'habitatge nou s'ha encarit més ràpidament, acumulant una diferència de 16,2 punts percentuals respecte al de segona mà des del 2015.

## 📊 Datasets Utilitzats
- **Compravendes (Registre de la Propietat):** Dades del Departament d'Estadística de l'Ajuntament de Barcelona sobre transaccions (2014-2025) consolidades i transformades a format *long* (55.272 registres).
- **Índex de Preus al Consum (INE):** Dades de la província de Barcelona (base 100 = 2014) per deflactar i obtenir els preus reals.
- **Renda Disponible (Ajuntament de Barcelona):** Renda familiar disponible per persona per analitzar l'accessibilitat i calcular l'índex d'esforç (mesos de renda per m²).

## ⚙️ Tecnologies i Arquitectura
L'aplicació segueix una arquitectura modular:
- **Interfície i Interactivitat:** `Shiny`, `shinydashboard`, `shinyWidgets`.
- **Visualització Estàtica:** `ggplot2` (Line charts, Jitter plots, Bubble charts amb facetes i paletes de colors qualitatives persistents per districte).
- **Visualització Geogràfica:** `leaflet` (Mapes interactius amb marcadors dinàmics sobre base CartoDB.Positron).
- **Estructura Modular:** Codi dividit en fitxers independents per facilitar el manteniment (`mod_evolucio.R`, `mod_mapa.R`, `mod_renda.R`, `mod_hipotesis.R` i `utils.R`).

## 🚀 Com executar el projecte en local
Per desplegar l'aplicació interactiva al teu entorn local:

1. **Clona o descarrega** aquest repositori en format `.zip` i descomprimeix-lo.
2. Obre l'RStudio.
3. Executa primer l'script `install_packages.R` per assegurar-te que tens totes les dependències necessàries instal·lades.
4. Obre l'script principal de l'aplicació: `app.R`.
5. Fes clic al botó **"Run App"** a la part superior dreta de l'editor per iniciar l'aplicació.

## 📝 Notes de Preprocessament
Durant la fase de tractament de dades, els valors de confidencialitat estadística ("..") en barris de poca activitat s'han codificat com a `NA` per evitar biaixos en l'anàlisi territorial. Així mateix, s'ha aplicat un tractament d'estandardització a la toponímia per solucionar discrepàncies d'accents en els noms dels districtes.
README.md
Mostrando README.md.
