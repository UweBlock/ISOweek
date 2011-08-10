ISOweek <- function(Datum) {
# Berechnet Kalenderwoche nach ISO-Standard
# Gibt Zeichenkette im ISO-Format JJJJ->W<WW, z.B. 2008-W06 zurück
# Uwe Block, 2008-12-10
# Quelle: http://www.salesianer.de/util/kalwoch.html

donnerstag <- function(Datum) {
	# gibt als Datumsobjekt den Donnerstag der Woche 
	# zurück, in der das übergebene "Datum" liegt.
	# 'eptage(Datum) - wochentag(Datum)' ist der Montag,
	# drei Tage später der Donnerstag
	Datum - wochentag(Datum) + 3
}
wochentag <- function(Datum) {
	# gibt den  zugehörigen Wochentag als Zahl zurück
	# wobei 0=Montag, 1=Dienstag, ... 6=Sonntag
	# format %w returns
	# Weekday as decimal number (0–6, Sunday is 0).
	(as.numeric(format(Datum, "%w"))+6) %% 7
}
jahr <- function(Datum) {
	# gibt die zu einem Datumsobjekt gehörende 
	# Jahreszahl zurück
	as.numeric(format(Datum, "%Y"))
}

	wochendonnerstag = donnerstag(Datum);
	kalenderwochenjahr = jahr(wochendonnerstag);
	ersterdonnerstag = donnerstag(as.Date(ifelse(is.na(kalenderwochenjahr),NA,
		paste(kalenderwochenjahr,"01","04",sep="-") )));
	kalenderwoche = as.numeric(wochendonnerstag-ersterdonnerstag) %/% 7 + 1;
	ifelse(is.na(kalenderwoche), NA_character_, 
		sprintf("%04.0f-W%02.0f", kalenderwochenjahr, kalenderwoche))
}

ISOweekday <- function(Datum) {
# Berechnet Tag der Woche nach ISO-Standard
# Gibt Zahl zwischen 1 und 7 zurück, Montag ist 1
# format %w returns
# Weekday as decimal number (0–6, Sunday is 0).
	(as.numeric(format(Datum, "%w"))+6) %% 7 + 1
}