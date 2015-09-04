##############################
#hmRLiv:v1_02_01/130612 hmIto
#	adaptive_dynamics.Rを追加
#	arguments.Rを追加
#hmRLiv:v1_02_00/120730 hmIto
#	version管理に対応
#	関数形の整理
#hmRLiv:v1_01/111122
#	vectormapにmapply適用
#	logicalアダプター作成
#	limited_image,limited_linesを追加
#hmRLiv:v1_00/110727
#	バージョン管理開始
##############################
hmRLib.Version="hmRLib_v1_02"

source(paste(hmRLib.Version,"functional.R",sep="/"),chdir=TRUE,encoding="UTF-8")
source(paste(hmRLib.Version,"bind.R",sep="/"))
source(paste(hmRLib.Version,"virtual_differential.R",sep="/"),chdir=TRUE,encoding="UTF-8")
source(paste(hmRLib.Version,"biology.R",sep="/"),chdir=TRUE,encoding="UTF-8")
source(paste(hmRLib.Version,"graphics.R",sep="/"),chdir=TRUE,encoding="UTF-8")
source(paste(hmRLib.Version,"numeric.R",sep="/"),chdir=TRUE,encoding="UTF-8")
source(paste(hmRLib.Version,"utility.R",sep="/"),chdir=TRUE,encoding="UTF-8")
source(paste(hmRLib.Version,"arguments.R",sep="/"),chdir=TRUE,encoding="UTF-8")
source(paste(hmRLib.Version,"adaptive_dynamics.R",sep="/"),chdir=TRUE,encoding="UTF-8")
