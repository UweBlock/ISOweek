ISOweekday <-
function(Datum) {
# Berechnet Tag der Woche nach ISO-Standard
# Gibt Zahl zwischen 1 und 7 zurück, Montag ist 1
# format %w returns
# Weekday as decimal number (0–6, Sunday is 0).
	(as.numeric(format(Datum, "%w"))+6) %% 7 + 1
}

