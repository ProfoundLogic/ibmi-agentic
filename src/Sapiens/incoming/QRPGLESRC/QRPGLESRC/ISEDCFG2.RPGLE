     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *****************************************************************
      *****************************************************************
     H/TITLE ISEDCFG2 - EDIT CONFIGURATION INFORMATION (INSTALLS)
     H*                 PRINT SERVERS
      *****************************************************************
      ** REVISIONS:
      *
      * 10/08/03 C:128 Project:
      *                Changed to configure all Print Server
      *                workstation ID's to be 'VW' and to make the
      *                print server name the unique ID in IMPSP file.
      *
      * 04/01/03 C:128 Project:
      *                Added special chatacters to run time array to
      *                allow a greater number of work staion ID's.
      *
      *****************************************************************
      * FILE SPECIFICATIONS  * * *
      *****************************************************************
      * INDICATOR USAGE
      * ALL/DIFFERENT SCREENS AND FUNCTIONS:
      *  60      GENERAL CHAIN: ALL
      *****************************************************************
      *
     FIMPSL6    UF A E           K DISK
      *** Begin Change 10/08/03 ***************************************
     F******IMPSL2  IF  E           K        DISK
     F******      PPS01                             KRENAMEPPS02
      *** End   Change 10/08/03 ***************************************
      ***  ***  *** ***
     F*
      *
      *****************************************************************
      * EXCEPTION SPECIFICATIONS * * *
      *** Begin change 04/01/03 ***************************************

     D******              V1     41  41  1
     D******              V2     41  41  1
      *** Begin Change 10/08/03 ***************************************
     D******             V1     54  54  1
     D******              V2     54  54  1
      *** End   Change 10/08/03 ***************************************
      *** End   change 04/01/03 ***************************************
      *****************************************************************
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *** Begin Change 10/08/03 ***************************************

     I******ISWSID     UDS
     I******                                  1   2 X#WSID
      *** End   Change 10/08/03 ***************************************

     C*
      *
     C     WSKY1         KLIST
     C                   KFLD                    QQSESS           10
      *** Begin Change 10/08/03 ***************************************
     C******     WSKY2     KLIST
     C******               KFLD           PSPSID
     C*
     C******     *NAMVAR   DEFN           ISWSID
      *** End   Change 10/08/03 ***************************************
      *
      *****************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    XXSYSN           60            SYS NAME
     C                   PARM                    XXJBNM            8            JOB NAME/LU
     C                   PARM                    XXDILB           10            DART/IMAGE LIB
     C                   PARM                    XXSNDR            1            SHRD NET DRIVE
     C                   PARM                    XXDESC           40            DESCRIPTION
     C                   PARM                    XXRTN             1            RTN VALUE(S)
      *
      *** Begin Change 10/08/03 ***************************************
     C******               UNLCKISWSID
      *** Begin Change 10/08/03 ***************************************
     C                   MOVE      XXRTN         QQRTN             1
     C                   MOVEL     *BLANKS       XXRTN
     C*******************  MOVE XX#SES    NBRSES  10
     C*
     C*
      *
     C**** DO EDITS
     C                   EXSR      EDIT1
     C*
     C     ENDPGM        TAG
     C**** KEEP PGM ACTIVE BY USING RETURN... HELP SPEED OF PROCESS.
     C******************** RETRN *****************************************
     C     ENDIT         TAG
     C                   MOVE      '1'           *INLR
     C*
      *------------------------*
     C     EDIT1         BEGSR
      *------------------------*
      *** Begin Change 10/08/03 ***************************************
     C*
     C******               Z-ADD*ZERO     CNT
     C*
      *** End   Change 10/08/03 ***************************************
     C*
     C                   MOVE      'A'           SFX               1
     C                   MOVEL     XXJBNM        SES6A             8
     C                   MOVEL     XXJBNM        SES10            10
     C                   MOVEL(P)  XXJBNM        QQSESS
     C*
     C     WSKY1         CHAIN     PPS01                              60
     C     *IN60         IFEQ      *OFF
     C     QQRTN         IFNE      'Y'
     C*** FOUND *** DO UPDAT ONLY AFTER USER ASKS TO!!!!
     C                   MOVE      'F'           XXRTN
     C*
     C                   GOTO      ENDIT
     C                   ELSE
     C******************** DO UPPDATE
     C                   MOVEL     QQSESS        PSJBNM
     C                   MOVEL     SES10         PSLUNM
     C*
     C*
     C                   MOVEL     SES10         PSPQUE
     C                   MOVEL     XXDILB        PSPQLB
     C                   MOVEL     XXSNDR        PSIDRV
     C                   MOVEL     XXDESC        PSDESC
     C*
     C                   MOVEL     'A'           PSSTAT
     C                   MOVEL     Q1PGM         PSIPGM
     C                   MOVEL     Q1PGM         PSLPGM
     C*
     C                   UPDATE    PPS01
     C                   MOVEL     'U'           XXRTN
     C*
     C                   ENDIF
     C                   ELSE
     C*** NEW *** CREATE THE RECORD!!!!
     C                   MOVEL     QQSESS        PSJBNM
     C                   MOVEL     SES10         PSLUNM
      *** Begin Change 10/08/03 ***************************************
     C*** GET A 2BYTE VAL FOR THE WS CODE....
     C*
     C******     TRYAGN    TAG
     C******     *LOCK     IN   ISWSID
     C*
     C******               MOVELX#WSID    PSPSID
     C                   MOVEL     'VW'          PSPSID
     C*
     C**** INCREMENT THE WSID DTAARA FOR NEXT NUMBER...
     C*
     C******               EXSR INCRM
     C****
     C******               OUT  ISWSID
     C*** CHECK IF THE VALUE IS ALREADY USED... IF SO LOOP TO GET NEXT ONE
     C******     WSKY2     CHAINPPS02                63
     C******     *IN63     IFEQ *OFF
     C*** ERROR ALREADY EXIST, GET NEXT ID...
     C******     CNT       IFGE 10
     C******               MOVE '9'       XXRTN
     C******               GOTO ENDIT
     C******               ENDIF
     C******               ADD  1         CNT     30
     C******               GOTO TRYAGN
     C******               ENDIF
      *** End   Change 10/08/03 ***************************************
     C                   MOVEL     QQSESS        PSJBNM
     C                   MOVEL     SES6A         PSLUNM
     C*
     C*
     C                   MOVEL     SES10         PSPQUE
     C                   MOVEL     XXDILB        PSPQLB
     C                   MOVEL     XXSNDR        PSIDRV
     C*
     C                   MOVEL     'A'           PSSTAT
     C                   MOVEL     Q1PGM         PSIPGM
     C                   MOVEL     Q1PGM         PSLPGM
     C                   MOVEL     XXDESC        PSDESC
     C*
     C                   WRITE     PPS01
     C                   MOVEL     'W'           XXRTN
     C*
     C                   ENDIF
     C*
     C                   CALL      'ISCRTDQ4'
     C                   PARM                    PSPQUE
     C                   PARM                    PSPQLB
     C                   PARM      PSDESC        XXDSC            20
     C                   PARM      *BLANKS       XXRC              1
     C*
      *
     C******               UNLCKISWSID
     C*
     C*
     C                   ENDSR
     C*
      *** Begin Change 10/08/03 ***************************************
      *------------------------*
     C******     INCRM     BEGSR
      *------------------------*
     C*
     C******               MOVEA'00'      *IN,65
     C******               Z-ADD1         X       20
     C******               Z-ADD1         Y       20
     C******               MOVELX#WSID    A       1
     C******               MOVE X#WSID    B       1
     C******     A         LOKUPV1,Y                     65
     C******     B         LOKUPV2,X                     66
     C******     *IN66     IFEQ *ON
      *** Begin change 04/01/03 ***************************************
     C******     X         IFLT 41
     C******     X         IFLT 54
      *** End   change 04/01/03 ***************************************
     C******               ADD  1         X
     C******               MOVELV2,X      B
     C******     A         CAT  B         X#WSID
     C******               GOTO DONE1
     C******               ELSE
     C*** MUST CHANGE A, AND PUT B AT 1ST VALUE
     C******               Z-ADD1         X
     C******     *IN65     IFEQ *ON
      *** Begin change 04/01/03 ***************************************
     C******     Y         IFLT 41
     C******     Y         IFLT 54
      *** End   change 04/01/03 ***************************************
     C******               ADD  1         Y
     C******               MOVELV1,Y      A
     C******               MOVELV2,X      B
     C******     A         CAT  B         X#WSID
     C******               GOTO DONE1
     C*
     C******               ELSE
     C***    *** ERRROR
     C******               ENDIF
     C******               ELSE
     C***    *** ERROR
     C******               ENDIF
     C***
     C******               ENDIF
     C******               ELSE
     C***    *** ERROR
     C******               ENDIF
     C*
     C******     DONE1     ENDSR
********    VALUES
*******ABCDEFGHIJKLMNOPQRSTUVWXYZ@#$0123456789& +=[]{}_^~!()%
********    VALUES
*******0123456789@#$&ABCDEFGHIJKLMNOPQRSTUVWXYZ %()!~^_{}[]=+
      *** End   Change 10/08/03 ***************************************
