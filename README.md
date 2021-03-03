# HL7 version 2.x Parser for R - hl7r

## Motivation
Tasked with the aggregation of local healthcare institution data, I was receiving a large number of HL7 feeds. The majority of this aggregation process was already conducted in R. No implementation in R for parsing HL7 data existed. Existing solutions in Python and JavaScript failed to parse my data. The desire to refrain from invoking an additional language, in addition to their failure to parse my data led to `hl7r`

## About
HL7 is a specification for data, typically pertaining to electronic health information. HL7 versions prior to `2.1` existed in an XML format, but versions since `2.1` are instead character delimited. This package supports only the later. Example HL7 data may look something like this [as reproduced from here](https://help.interfaceware.com/getting-sample-hl7-data.html)

```hl7
MSH|^~`&|ECG REPORTING|ROCHESTER|ERIS|ROCHESTER|20110621050440||ORU^R01|20110621050440|P|2.1
PID|||999999999||TEST^PATIENT||18450101|F
OBR|||211088491|0^ADULT^ROCHECG|||20110620170631|||||||||M999999^^^^^^^RACFID||||||20110621060232||EC|F|||||||M999999^LASTNAME MD^FIRSTNAME^^^^^RACFID
OBX||ST|93000.2^VENTRICULAR RATE EKG/MIN^CPT4|1|52|/SEC
OBX||ST|93000.4^PR INTERVAL(MSEC)^CPT4|2|208|MSEC
OBX||ST|93000.5^QRS - INTERVAL(MSEC)^CPT4|3|88|MSEC
OBX||ST|93000.6^QT - INTERVAL(MSEC)^CPT4|4|466|MSEC
OBX||ST|93000&PTL^PHYSICAL TEST LOCATION^CPT4|5|STMA
OBX||ST|93000&PTR^PHYSICAL TEST ROOM^CPT4|6|04254
OBX||CE|93000.17^^CPT4|7|21&101^Sinus bradycardia`T`with 1st degree A-V block^MEIECG
OBX||CE|93000.17^^CPT4|8|1687^Otherwise normal ECG^MEIECG
OBX||CE|93000&CMP^^CPT4|9|1301^When compared with ECG of^MEIECG
OBX||TS|93000&CMD^EKG COMPARISON DATE^CPT4|10|201106171659
OBX||CE|93000&CMP^^CPT4|11|1305^No significant change was found^MEIECG
OBX||TX|93000.48^EKG COMMENT^CPT4|12|9917^LASTNAME MD^FIRSTNAME
```

The first 3 characters represent the segment, and segments are delimited by new lines. A message begins at an `MSH` segment and terminates at the next `MSH` segment or the end of a file. Segments are often repeated in a message, and this parser assumes to increment them in order. Fields are separated by the specified field separator (The fourth character of an `MSH` segment, most often `|`). Fields are identified by their position in a segment. The 5th character of an `MSH` segment is the component separator.

HL7 is not a tabular format, but rather a nested format. The list data structure native to R is used to structure the parsed data. This implementation assumes you will be working with batched data as opposed to single messages, however both will parse. The parsed structure looks like this:

```r
list( # List of Messages (Indexed by Position)
	list( # List of Segments in Message (Indexed by Key)
		MSH.1 = list( # List of Fields (Indexed by Position)
				'|',
				'^~`&',
			list( # List of Repetitions (Indexed by Position)
				'ECG REPORTING'
			),
			...
		),
		PID.1 = list( # Fields
			list( # Repetitions
				'',
			),
			list(
				'',
			),
			list(
				'999999999',
			),
			list(
				'',
			),
			list(			
				list( # Fields with a Component Seperator i.e. ^ (Indexed by Position)
					'TEST',
					'PATIENT',
					list( # Fields with a Subcomponent Seperator i.e & (Indexed by Position)
						...
					),
					...
				),
				...
			),
			...
		), 
		...
	),
	...
)
```

Hence in the example (assuming this has been parsed into `messages`):

```r
> messages[[1]][['OBX.1']][[3]][[1]]
[[1]]
[1] "93000.2"
[[2]]
[1] "VENTRICULAR RATE EKG/MIN"
[[3]]
[1] "CPT4"

> messages[[1]][['OBX.2']][[3]][[1]]
[[1]]
[1] "93000.4"
[[2]]
[1] "PR INTERVAL(MSEC)"
[[3]]
[1] "CPT4"
```

In other words, indexing works like:
```
message > segment > segment repetition > field > field repetition > component > subcomponent
```
However, a tree may not always have this depth.

## Installation
This package is not yet published to CRAN, so to install you must build from the source available here. This package requires no external dependencies to base-R, but requires `dplyr` if you intend to use the `hl7_df()` function.

```r
devtools::install_github('bransonf/hl7r') # OR
remotes::install_github('bransonf/hl7r')
```

## Usage
This package exports two functions. The first reads `.hl7` data and accepts either a character vector or file path directing to a valid HL7 file.

```r
library(hl7r)

# With a Vector
hl7_data <- readLines('hospital.hl7')
parsehl7(hl7_data)

# From File
parsehl7(file = 'hospital.hl7')

```

There is also a function that traverses the tree structure to produce a normalized `data.frame`. This function depends on `dplyr`
```r
library(hl7r)

# Parse from File
hl7_tree <- parsehl7(file = 'hospital.hl7')

# Coerce to Dataframe
hl7_dataframe <- hl7df(hl7_tree)

```

## Issues
This is not a comprehensive implementation according to the HL7 specification. Some considerations were made to warn the user of non-standard data, although not all will be accounted for. In order to maintain compatibility with past, current and future versions, repeatability of fields is not explicitly handled. All fields are assumed to be repeatable, except for `MSH.1.1` and `MSH.1.2`. Due to the difficulty in obtaining a large test suite of data, this has only been tested with my limited availability of data.

As of 3/3/2021, the first repetition of a *segment* is now suffixed with `.1` in order to maintain a 5 number indexing pattern.

If you receive an error or unexpected output, please [file an issue.](https://github.com/bransonf/hl7r/issues/new)

## License
This work is released under the MIT License.


