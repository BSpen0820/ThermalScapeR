test_that(".get_total_ram returns a plausible byte count on this machine", {
  ram <- .get_total_ram()
  expect_type(ram, "double")
  expect_length(ram, 1L)
  expect_true(is.finite(ram))
  expect_gt(ram, 5e8)
  expect_lt(ram, 1e14)
})

test_that(".parse_meminfo_bytes reads MemTotal from /proc/meminfo text", {
  lines <- c("MemTotal:       16384000 kB", "MemFree:         1234567 kB",
             "MemAvailable:    8000000 kB")
  expect_identical(.parse_meminfo_bytes(lines), 16384000 * 1024)
  expect_identical(.parse_meminfo_bytes(rev(lines)), 16384000 * 1024)
  expect_error(.parse_meminfo_bytes(c("MemFree: 1 kB")), "MemTotal")
})

test_that(".parse_single_number handles sysctl / PowerShell style output", {
  expect_identical(.parse_single_number("17179869184"), 17179869184)
  expect_identical(.parse_single_number(c("", "  16476968 ", "")), 16476968)
  expect_true(is.na(.parse_single_number(character(0))))
  expect_true(is.na(.parse_single_number("not a number")))
})

test_that(".get_total_ram errors clearly on an unsupported platform", {
  skip_if(file.exists("/proc/meminfo"), "/proc/meminfo exists; generic fallback would succeed")
  expect_error(.get_total_ram("Plan9"), "Unable to determine total RAM")
})
