## Introduzione
Questo progetto si concentra sull'analisi delle politiche agricole globali utilizzando un dataset fornito dall'OECD. L'obiettivo principale è applicare analisi statistiche sui vari indicatori del dataset dal 2005 al 2020.

## Dataset
Il dataset utilizzato è il **"Monitoring and Evaluation: Single Commodity Indicators"** dell'OECD. Copre 36 nazioni membri dell'OECD, 5 nazioni dell'Unione Europea e non membri dell'OECD e 11 nazioni emergenti. Le materie prime analizzate includono grano, mais, soia, zucchero, riso, avena, carne bovina e di vitello e caffè.

## Domande di Ricerca
Le principali domande di ricerca affrontate nel progetto sono:
1. Come è cambiata la produzione totale nel tempo per ciascun paese?
2. Esistono paesi in cui la produzione è aumentata costantemente nel tempo?
3. Esiste una relazione tra il livello di produzione e il livello di consumo?
4. Esiste una relazione tra il livello di produzione e il prezzo di vendita?
5. Esiste una relazione tra il livello di consumo e il prezzo di vendita?
6. Il guadagno annuale dei produttori (producer price) è influenzato dal Market Price Differential?
7. Queste relazioni sono valide per ogni alimento?
8. In che modo le politiche che influenzano il mercato (MPD) interagiscono con il consumo per determinare i prezzi di mercato?
9. È possibile raggruppare i paesi per consumo, produzione e incassi?
10. Di quanto cambieranno i valori degli indicatori nei prossimi anni?
11. Esiste una differenza significativa nei prezzi tra i paesi che applicano politiche agricole di supporto (MPD) rispetto a quelli che non le applicano?

## ⚙️ Struttura del Progetto
Il progetto è organizzato nel seguente modo:
* **Data:** contiene i risultati e i dati del progetto
  * **Data/Clustering:** contiene i risultati del clustering gerarchico e non
  * **Data/Correlation:** contiene i risultati della correlazione tra gli indicatori e i risultati della regressione
  * **Data/DataExploration:** contiene i risultati dell’analisi esplorativa dei dati, incluse analisi temporali e geografiche
  * **Data/Estimations:** contiene i risultati della stima di incremento e intervallare
  * **Data/SubDatasets:** contiene la divisione del dataset finale per indicatore
  * **Data/Ipotesi:** contiene i risultati della verifica delle ipotesi
* **Scripts:** contiene tutti gli script del progetto
  * **Scripts/data_preprocessing:** contiene gli script per il preprocessing dei dati
  * **Scripts/data_exploration_functions:** contiene gli script per l’analisi esplorativa dei dati

## 💻 Project Environment
Per poter replicare il progetto, installa le librerie necessarie indicate nel renv.lock file tramite il seguente comando:
`renv::restore()`
## 📧 Contatti
- **Rocco Iuliano:** r.iuliano13@studenti.unisa.it
- **Simone Della Porta:** s.dellaporta6@studenti.unisa.it
