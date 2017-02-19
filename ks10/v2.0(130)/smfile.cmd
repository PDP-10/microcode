write setup dskb:
write reset
read t10ki.uld[10,1141,ksu,new]
serial 4149
write cram
write boot bts256.exe[6,2020]
write done
exit
