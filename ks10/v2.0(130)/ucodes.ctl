$STEP KSU
$MOUNT BLKK:
$ENDHDR
.PATH [7,3,KSU]
.PATH DEC:=BLKK:[7,3,SMFILE],DEC:
!.CTL FILE TO BUILD THE VARIOUS KS-10 MICROCODES.
!
!
!CHECK FOR THE INPUT FILES
!
.ERROR %
.DIRECT/CHECKS KS10.CMD,T10KI.CMD,T10KL.CMD,MICRO.EXE,DEC:SMFILE.EXE
.DIRECT/CHECKS T10KI.MIC,T10KL.MIC,KS10.MIC,SIMPLE.MIC,FLT.MIC,-
*	EXTEND.MIC,INOUT.MIC,PAGEF.MIC
.ERROR
.SET DEFAULT PROTECTION 477
!
!BUILD THE DEFAULT (TOPS-20, TOPS-10 7.01, AND DIAGNOSTIC UCODE)
!
.RUN MICRO
*@KS10.CMD
=^Z
.DO SMFILE.MIC KS10
!
!BUILD THE SPECIAL TOPS-10 V7.02 UBABLT MICROCODE
!
.RUN MICRO
*@T10KI.CMD
=^Z
.DO SMFILE.MIC T10KI
!
!BUILD THE SPECIAL TOPS-10 V7.03 UBABLT/INHCST MICROCODE
!
.RUN MICRO
*@T10KL.CMD
=^Z
.DO SMFILE.MIC T10KL
!
!BUILD THE DOC FILE
!
.R RUNOFF
*KS10.DOC=KS10.RND
=^Z
!
!CHECK THE RESULTS
!
.DIRECT/CHECKS KS10.ULD,KS10.MCR,T10KI.ULD,T10KI.MCR,T10KL.ULD,T10KL.MCR