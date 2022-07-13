test_that("parsehl7 works", {
  # setup
  sample_txt <- c(
    "MSH|^~`&|ECG REPORTING|ROCHESTER|ERIS|ROCHESTER|20110621050440||ORU^R01|20110621050440|P|2.1",
    "PID|||999999999||TEST^PATIENT||18450101|F",
    "OBR|||211088491|0^ADULT^ROCHECG|||20110620170631|||||||||M999999^^^^^^^RACFID||||||20110621060232||EC|F|||||||M999999^LASTNAME MD^FIRSTNAME^^^^^RACFID",
    "OBX||ST|93000.2^VENTRICULAR RATE EKG/MIN^CPT4|1|52|/SEC",
    "OBX||ST|93000.4^PR INTERVAL(MSEC)^CPT4|2|208|MSEC",
    "OBX||ST|93000.5^QRS - INTERVAL(MSEC)^CPT4|3|88|MSEC",
    "OBX||ST|93000.6^QT - INTERVAL(MSEC)^CPT4|4|466|MSEC",
    "OBX||ST|93000&PTL^PHYSICAL TEST LOCATION^CPT4|5|STMA",
    "OBX||ST|93000&PTR^PHYSICAL TEST ROOM^CPT4|6|04254",
    "OBX||CE|93000.17^^CPT4|7|21&101^Sinus bradycardia`T`with 1st degree A-V block^MEIECG",
    "OBX||CE|93000.17^^CPT4|8|1687^Otherwise normal ECG^MEIECG",
    "OBX||CE|93000&CMP^^CPT4|9|1301^When compared with ECG of^MEIECG",
    "OBX||TS|93000&CMD^EKG COMPARISON DATE^CPT4|10|201106171659",
    "OBX||CE|93000&CMP^^CPT4|11|1305^No significant change was found^MEIECG",
    "OBX||TX|93000.48^EKG COMMENT^CPT4|12|9917^LASTNAME MD^FIRSTNAME"
  )

  sample_names <- c(
    "MSH.1", "PID.1", "OBR.1", "OBX.1", "OBX.2", "OBX.3", "OBX.4",
    "OBX.5", "OBX.6", "OBX.7", "OBX.8", "OBX.9", "OBX.10", "OBX.11",
    "OBX.12"
  )

  sample_file <- withr::local_file("sample_hl7")
  writeLines(sample_txt, sample_file)

  # eval
  res_txt <- parsehl7(sample_txt)
  res_file <- parsehl7(file = sample_file)

  # test
  expect_type(res_txt, "list")
  expect_length(res_txt, 1L)

  expect_type(res_txt[[1L]], "list")
  expect_length(res_txt[[1L]], 15L)
  expect_named(res_txt[[1L]], sample_names)

  expect_type(res_txt[[1L]][["MSH.1"]], "list")
  expect_length(res_txt[[1L]][["MSH.1"]], 11L)
  expect_type(res_txt[[1L]][["MSH.1"]][[1L]], "list")
  expect_length(res_txt[[1L]][["MSH.1"]][[1L]], 2L)
  expect_type(res_txt[[1L]][["MSH.1"]][[1L]][[2L]], "character")
  expect_equal(res_txt[[1L]][["MSH.1"]][[1L]][[2L]], "`&")

  expect_type(res_txt[[1L]][["PID.1"]], "list")
  expect_length(res_txt[[1L]][["PID.1"]], 8L)

  expect_type(res_txt[[1L]][["OBR.1"]], "list")
  expect_length(res_txt[[1L]][["OBR.1"]], 32L)

  expect_type(res_txt[[1L]][["OBX.1"]], "list")
  expect_length(res_txt[[1L]][["OBX.1"]], 6L)


  expect_equal(res_txt, res_file)
})



test_that("parsehl7 manage two messages", {
  # setup
  sample_txt <- c(
    "MSH|^~`&|ECG REPORTING|ROCHESTER|ERIS|ROCHESTER|20110621050440||ORU^R01|20110621050440|P|2.1",
    "PID|||999999999||TEST^PATIENT||18450101|F",
    "OBR|||211088491|0^ADULT^ROCHECG|||20110620170631|||||||||M999999^^^^^^^RACFID||||||20110621060232||EC|F|||||||M999999^LASTNAME MD^FIRSTNAME^^^^^RACFID",
    "OBX||ST|93000.2^VENTRICULAR RATE EKG/MIN^CPT4|1|52|/SEC",
    "MSH|^~`&|ECG REPORTING|ROCHESTER|ERIS|ROCHESTER|20110621050440||ORU^R01|20110621050440|P|2.1",
    "PID|||999999999||TEST^PATIENT||18450101|F",
    "OBR|||211088491|0^ADULT^ROCHECG|||20110620170631|||||||||M999999^^^^^^^RACFID||||||20110621060232||EC|F|||||||M999999^LASTNAME MD^FIRSTNAME^^^^^RACFID",
    "OBX||ST|93000.2^VENTRICULAR RATE EKG/MIN^CPT4|1|52|/SEC"
  )

  # eval
  res_txt <- parsehl7(sample_txt)

  # test
  expect_length(res_txt, 2L)
})


test_that("parsehl7 manage two separated messages", {
  # setup
  sample_txt <- c(
    "MSH|^~`&|ECG REPORTING|ROCHESTER|ERIS|ROCHESTER|20110621050440||ORU^R01|20110621050440|P|2.1",
    "PID|||999999999||TEST^PATIENT||18450101|F",
    "OBR|||211088491|0^ADULT^ROCHECG|||20110620170631|||||||||M999999^^^^^^^RACFID||||||20110621060232||EC|F|||||||M999999^LASTNAME MD^FIRSTNAME^^^^^RACFID",
    "OBX||ST|93000.2^VENTRICULAR RATE EKG/MIN^CPT4|1|52|/SEC",
    "MSH|^~`&|ECG REPORTING|ROCHESTER|ERIS|ROCHESTER|20110621050440||ORU^R01|20110621050440|P|2.1",
    "PID|||999999999||TEST^PATIENT||18450101|F",
    "OBR|||211088491|0^ADULT^ROCHECG|||20110620170631|||||||||M999999^^^^^^^RACFID||||||20110621060232||EC|F|||||||M999999^LASTNAME MD^FIRSTNAME^^^^^RACFID",
    "OBX||ST|93000.2^VENTRICULAR RATE EKG/MIN^CPT4|1|52|/SEC"
  )

  sample_txt_spaced <- c(
    "MSH|^~`&|ECG REPORTING|ROCHESTER|ERIS|ROCHESTER|20110621050440||ORU^R01|20110621050440|P|2.1",
    "PID|||999999999||TEST^PATIENT||18450101|F",
    "OBR|||211088491|0^ADULT^ROCHECG|||20110620170631|||||||||M999999^^^^^^^RACFID||||||20110621060232||EC|F|||||||M999999^LASTNAME MD^FIRSTNAME^^^^^RACFID",
    "OBX||ST|93000.2^VENTRICULAR RATE EKG/MIN^CPT4|1|52|/SEC",
    "",
    "MSH|^~`&|ECG REPORTING|ROCHESTER|ERIS|ROCHESTER|20110621050440||ORU^R01|20110621050440|P|2.1",
    "PID|||999999999||TEST^PATIENT||18450101|F",
    "OBR|||211088491|0^ADULT^ROCHECG|||20110620170631|||||||||M999999^^^^^^^RACFID||||||20110621060232||EC|F|||||||M999999^LASTNAME MD^FIRSTNAME^^^^^RACFID",
    "OBX||ST|93000.2^VENTRICULAR RATE EKG/MIN^CPT4|1|52|/SEC"
  )

  # eval
  res_txt <- parsehl7(sample_txt)
  res_txt_spaced <- parsehl7(sample_txt_spaced)

  # test
  expect_length(res_txt_spaced, 2L)
  expect_equal(res_txt_spaced, res_txt)
})
