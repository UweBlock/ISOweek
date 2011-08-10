\name{ISOweek}
\alias{ISOweek}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ISOweek(Datum)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Datum}{
%%     ~~Describe \code{Datum} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(Datum) {
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
	# format \%w returns
	# Weekday as decimal number (0–6, Sunday is 0).
	(as.numeric(format(Datum, "\%w"))+6) \%\% 7
  }
jahr <- function(Datum) {
	# gibt die zu einem Datumsobjekt gehörende 
	# Jahreszahl zurück
	as.numeric(format(Datum, "\%Y"))
  }

	wochendonnerstag = donnerstag(Datum);
	kalenderwochenjahr = jahr(wochendonnerstag);
	ersterdonnerstag = donnerstag(as.Date(ifelse(is.na(kalenderwochenjahr),NA,
		paste(kalenderwochenjahr,"01","04",sep="-") )));
	kalenderwoche = as.numeric(wochendonnerstag-ersterdonnerstag) \%/\% 7 + 1;
	ifelse(is.na(kalenderwoche), NA_character_, 
		sprintf("\%04.0f-W\%02.0f", kalenderwochenjahr, kalenderwoche))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
