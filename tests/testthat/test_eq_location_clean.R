context("eq_location_clean()")

test_that("LOCATION_NAME_cleaned is correct", {
  expect_true(is.na(eq_location_clean("GREECE, TURKEY")))
  expect_match(eq_location_clean("ALASKA:  SLANA, MENTASTA LAKE, FAIRBANKS"),
               "Slana, Mentasta Lake, Fairbanks")
  expect_match(eq_location_clean("ALASKA:  ALEUTIAN ISLANDS:  RAT ISLANDS"),
               "Rat Islands")
  expect_match(eq_location_clean("ALASKA:  SOUTHEASTERN"), "Southeastern")
  expect_match(eq_location_clean("CHINA:  QINGHAI PROVINCE:  XIDATAN"), "Xidatan")
  expect_match(eq_location_clean("CHINA:  XINJIANG PROVINCE"), "Xinjiang Province")
  expect_true(is.na(eq_location_clean("PUERTO RICO")))
  expect_match(eq_location_clean("ISRAEL:  PALESTINE; SYRIA:  HALAB; TURKEY:  ANTAKYA"),
               "Palestine")
  expect_true(is.na(eq_location_clean("AFGHANISTAN; INDIA:  HINDU-KUSH; KAZAHKSTAN:  BALKHA")))
  expect_match(eq_location_clean("TURKEY:  ANTAKYA; LEBANON:  HIMS; IRAQ:  MOSUL"),
               "Antakya")
  expect_true(is.na(eq_location_clean("TURKEY; ARMENIA:  DVINA")))
  expect_match(eq_location_clean("SAMOA: APIA; AMERICAN SAMOA: PAGO PAGO"), "Apia")
})

