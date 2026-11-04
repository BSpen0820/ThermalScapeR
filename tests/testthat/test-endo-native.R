test_that(".endo_is_native runs a .exe natively only on Windows and a native build everywhere", {
  expect_true(.endo_is_native("C:/x/Endo2022a.exe", "Windows"))
  expect_false(.endo_is_native("/opt/x/Endo2022a.exe", "Linux"))
  expect_false(.endo_is_native("/opt/x/ENDO.EXE", "Darwin"))
  expect_true(.endo_is_native("/mnt/ceph/user/endo", "Linux"))
  expect_true(.endo_is_native("/opt/endo_build/endo.bin", "Linux"))
})
