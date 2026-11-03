      // @MBRTEXT: Program status data structure copy member
      ********************************************************************* ****
      * Application: General
      *     Purpose: Program status data structure
      *
      * Log:
      * 08/01/01  Billy Andrews  Created
      *
      ********************************************************************* ****

     d PgmStsDS       sds
     d  $ProcName        *PROC
     d  $PgmStatus       *STATUS
     d  $PrvStatus            16     20s 0
     d  $LineNum              21     28
     d  $Routine         *ROUTINE
     d  $Parms           *PARMS
     d  $ExcpType             40     42
     d  $ExcpNum              43     46
     d  $PgmLib               81     90
     d  $Excpdata             91    170
     d  $ExcpID              171    174
     d  $Date                191    198
     d  $Year                199    200s 0
     d  $LastFile            201    208
     d  $FileInfo            209    243
     d  $JobName             244    253
     d  $User                254    263
     d  $JobNum              264    269s 0
     d  $JobDate             270    275s 0
     d  $RunDate             276    281s 0
     d  $RunTime             282    287s 0
     d  $CrtDate             288    293
     d  $CrtTime             294    299
     d  $CplLevel            300    303
     d  $SrcFile             304    313
     d  $SrcLib              314    323
     d  $SrcMbr              324    333
     d  $ProcPgm             334    343
     d  $ProcMod             344    353
