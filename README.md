# Streaming Data Management and Time Series Analysis Project 
*Progetto del corso Streaming Data Management and Time Series Analysis (Laurea magistrale Data Science)*

<p align="center">
  <img width="350" height="200" src="https://editor.analyticsvidhya.com/uploads/3951420200902_blog_-forecasting-with-time-series-models-using-python_pt2_website.png">
</p>

_**Abstract**_ <br />
*Rilevazioni CO2, previsioni future*

Viene fornita una time series univariata, relativa a misurazioni orarie di ossido di carbonio (CO). I dati sono organizzati nelle seguenti 3 colonne:

* Date - stringa codificante la data della misurazione, in formato yyyy-mm-dd
* Hour - intero indicante l'ora della misurazione. I valori vanno da 0 a 23, con 0 che rappresenta l'intervallo 00:00 - 00:59, 1 che rappresenta l'intervallo 01:00 - 01:59, ... e 23 che rappresenta l'intervallo 23:00 - 23:59
* CO - valore di CO rilevato
I dati coprono il periodo da 2004-03-10 (hour=18) a 2005-02-28 (hour=23), e sono storicizzati nel file CSV in fondo.

## Obiettivo del progetto

* Predire i valori orari di CO per il periodo dal 2005-03-01 (hour=0) al 2005-03-31 (hour=23)
* Utilizzare (almeno) tre diversi algoritmi: uno della famiglia ARIMA, uno della famiglia UCM, e uno della famiglia Machine Learning
* Caricare il file delle previsioni nella sezione "Assignment": è possibile effettuare più caricamenti fino ad una settimana prima dell'appello. Le previsioni saranno verificate ed un ranking verrà fornito con una cadenza giornaliera (nel caso di nuovi caricamenti). La misura di performance su cui sarà basato il ranking è il MAPE.
* Il formato delle previsioni è il seguente e deve essere categoricamente rispettato (il controllo delle previsioni avverrà in modo automatizzato). Le previsioni dovranno essere organizzate nelle seguenti 5 colonne:
  * Date - come sopra
  * Hour - come sopra
  * ARIMA - previsione dei livelli di CO secondo l'algoritmo della famiglia ARIMA
  * UCM  - previsione dei livelli di CO secondo l'algoritmo della famiglia UCM
  * ML  - previsione dei livelli di CO secondo l'algoritmo della famiglia Machine Learning

***

_**Marzorati Stefano**_ 
