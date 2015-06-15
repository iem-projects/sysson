In Version 1.3.0:

# Globale Tools

## Convert Spreadsheet to NetCDF

Im 'File' Menu zu finden. Nach Auswahl des .xls oder .xlsx Excel Files, wird eine Tabelle mit einer einfachen Darstellung des gewaehlten Spreadsheets geoeffnet. Hier muss man nacheinander Variablen definieren, indem man ein Zellauswahl in der Tabelle machen und anschliessend auf '+' klickt. Man muss der Variablen dann einen Namen, optional Einheit und Beschreibung geben. Bei Dimensionen (e.g. Zeitachse) laesst man die 'Dim' Felder rechts frei. Bei abhaengigen Variablen gibt man den Namen der Dimension bzw. bei 2D die Namen beider Dimensionen rechts ein. Diese muessen _vorher_ in der untenstehenden Tabelle definiert worden sein. Nach Bestaetigung mit 'Ok' waehlt man den Namen des zu erzeugenden NetCDF Files aus. Dieses File muss man ggf. dann wieder in den Workspace importieren.

# DataSource Tools

Im DataSource Fenster kann man nach Auswahl einer Variablen auf mehrere Aktionen im 'Actions' Menu zugreifen.

## Histogram Plot

Erzeugt Plots der Verteilung der Werte in der gewaehlten Variablen. Man muss etwas warten, bis die Berechnung abgeschlossen ist. Es werden zwei Plots gezeigt, links ein Histogram (X-Achse Variablenwerte, Y-Achse relative Haeufigkeit), rechts ein akkumulatives Histogram (Y-Achse wird gegenueber linkem Plot integriert bis rechts 100% erreicht sind).

## 1D Plot

Einfacher 1-dimensionaler Plot. Erfordert, dass die Variable selbst 1-dimensional ist.

## Concatenate Matrices

Dient zum Zusammenfuegen von einer aufgeteilten Variable, z.B. historische und projezierte Zeitreihe. Die Variable, die beim Aufruf des Menus ausgewaehlt war, wird als erste in der Zeitreihe betrachtet. In dem aufgehenden Fenster muss man nun noch die zweite zugehoerige Variable per Drag-and-Drop dort definieren. Beim Klick auf 'Ok' wird das zu erzeugende NetCDF File ausgewaehlt. Beide Variablen muessen identische Dimensionen und Form habe, wobei die zusammenzuklebende Dimension in der Groesse unterschiedlich sein darf.

## Calculate Anomalies

Geht momentan davon aus, dass die gewaehlte Variable eine Zeitreihe in Monatsaufloesung ist. Arbeitet in zwei Schritten. Zunaechst werden die Mittel jedes Datenpunktes pro Monat berechnet. Dabei wird nicht ueber den gesamten Datensatz gemittelt, sondern ueber ein gleitendes Fenster von einstellbarer Laenge in Jahren. Anschliessend wird die Variable in ein neues NetCDF File geschrieben und von jedem Datenpunkt der zugehoerige Normalwert subtrahiert.

