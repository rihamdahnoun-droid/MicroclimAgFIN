<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">

</head>

<body>

<h1> microclimAg</h1>

<p><strong>Microclimate Analysis in Agricultural Landscapes (R Package)</strong></p>

<p>
Télédétection thermique • NDVI • analyse spatiale • modélisation microclimatique
</p>

<hr>

<h2> Objectif scientifique</h2>

<p>
microclimAg analyse les variations microclimatiques dans les paysages agricoles à partir de données :
</p>

<ul>
<li>LST (MODIS / Landsat)</li>
<li>NDVI</li>
<li>Données météo</li>
<li>Données spatiales</li>
</ul>

<h3>Objectifs principaux</h3>

<ul>
<li>Détection des îlots de chaleur ruraux</li>
<li>Analyse du stress thermique des cultures</li>
<li>Relation végétation–température</li>
<li>Cartographie microclimatique</li>
<li>Modélisation spatiale simple</li>
</ul>

<hr>

<h2> Pipeline global</h2>

<pre>
download_lst_data()
clean_lst_data()
calculate_lst_anomaly()
cluster_thermal_zones()
extract_microclimate_features()
generate_report()
</pre>

<hr>

<h2> Traitement LST</h2>

<h3>clean_lst_data()</h3>
<p>Nettoyage des données LST :</p>
<ul>
<li>Suppression des outliers</li>
<li>Interpolation des NA</li>
<li>Harmonisation de la résolution</li>
</ul>

<pre>
lst_clean <- clean_lst_data(lst_raw)
</pre>

<h3>calculate_lst_anomaly()</h3>

<p>Calcul des anomalies thermiques :</p>
<ul>
<li>Mean difference</li>
<li>Z-score standardisation</li>
</ul>

<pre>
anom <- calculate_lst_anomaly(lst_clean, method = "zscore")
</pre>

<hr>

<h2> NDVI</h2>

<p><strong>Formule :</strong></p>

<pre>
NDVI = (NIR - Red) / (NIR + Red)
</pre>

<pre>
ndvi <- calculate_ndvi(nir, red)
</pre>

<hr>

<h2> Analyse spatiale</h2>

<h3>cluster_thermal_zones()</h3>
<p>K-means clustering : zones froides / chaudes</p>

<h3>analyze_spatial_correlation()</h3>
<p>Moran’s I : autocorrélation spatiale</p>

<hr>

<h2>Modélisation</h2>

<p>Variables :</p>
<ul>
<li>NDVI</li>
<li>Altitude</li>
<li>Occupation du sol</li>
</ul>

<pre>
model_temperature_relationships(lst, ndvi)
</pre>

<hr>

<h2> Extraction de features</h2>

<pre>
extract_microclimate_features(lst, ndvi)
</pre>

<hr>

<h2> Visualisation</h2>

<pre>
plot_microclimate_map(lst)
</pre>

<hr>

<h2>📄 Reporting</h2>

<pre>
generate_report(results)
</pre>

<hr>

<h2> Inputs</h2>

<ul>
<li>LST (MODIS / Landsat)</li>
<li>NDVI</li>
<li>Météo</li>
<li>Land cover</li>
<li>Stations météo</li>
</ul>

<h2>Outputs</h2>

<ul>
<li>Raster : LST, NDVI, anomalies</li>
<li>Tables : statistiques microclimatiques</li>
<li>Maps : heatmaps, clusters</li>
<li>Reports : HTML / PDF</li>
</ul>

<hr>

<h2> Author</h2>

<p>
<strong>Riham Dahnoun</strong><br>
IAV Hassan II<br>
 riham.dahnoun@iav.ac.ma
</p>

<hr>

<p><em>microclimAg — Microclimate analysis framework for agricultural landscapes</em></p>

</body>
</html>
